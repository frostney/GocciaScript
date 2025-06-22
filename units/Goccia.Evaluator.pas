unit Goccia.Evaluator;

{$I Goccia.inc}

interface

uses
  Goccia.Interfaces, Goccia.Values.Base, Goccia.Token, Goccia.Scope, Goccia.Error, Goccia.Logger, Goccia.Modules, Goccia.Values.NativeFunction, Goccia.Values.UndefinedValue, Goccia.Values.ObjectValue, Goccia.Values.FunctionValue, Goccia.Values.ClassValue, Goccia.Values.ArrayValue, Goccia.Values.BooleanValue, Goccia.Values.NumberValue, Goccia.Values.StringValue, Goccia.Values.StringObjectValue, Goccia.Values.Error, Goccia.Values.NullValue, Goccia.AST.Node, Goccia.AST.Expressions, Goccia.AST.Statements, Goccia.Utils, Goccia.Lexer, Goccia.Parser,
  Goccia.Evaluator.Arithmetic, Goccia.Evaluator.Bitwise, Goccia.Evaluator.Comparison, Goccia.Evaluator.TypeOperations, Goccia.Evaluator.Assignment,
  Generics.Collections, SysUtils, Math, Classes;

type
  TGocciaEvaluationContext = record
    Scope: TGocciaScope;
    OnError: TGocciaThrowError;
    LoadModule: TLoadModuleCallback;
  end;

function Evaluate(Node: TGocciaASTNode; Context: TGocciaEvaluationContext): TGocciaValue;
function EvaluateExpression(Expression: TGocciaExpression; Context: TGocciaEvaluationContext): TGocciaValue;
function EvaluateStatement(Statement: TGocciaStatement; Context: TGocciaEvaluationContext): TGocciaValue;
function EvaluateBinary(BinaryExpression: TGocciaBinaryExpression; Context: TGocciaEvaluationContext): TGocciaValue;
function EvaluateUnary(UnaryExpression: TGocciaUnaryExpression; Context: TGocciaEvaluationContext): TGocciaValue;
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
function EvaluateClassMethod(ClassMethod: TGocciaClassMethod; Context: TGocciaEvaluationContext; SuperClass: TGocciaValue = nil): TGocciaValue;
function EvaluateClass(ClassDeclaration: TGocciaClassDeclaration; Context: TGocciaEvaluationContext): TGocciaValue;
function EvaluateClassExpression(ClassExpression: TGocciaClassExpression; Context: TGocciaEvaluationContext): TGocciaValue;
function EvaluateClassDefinition(ClassDef: TGocciaClassDefinition; Context: TGocciaEvaluationContext; Line, Column: Integer): TGocciaClassValue;
function EvaluateNewExpression(NewExpression: TGocciaNewExpression; Context: TGocciaEvaluationContext): TGocciaValue;
function EvaluatePrivateMember(PrivateMemberExpression: TGocciaPrivateMemberExpression; Context: TGocciaEvaluationContext): TGocciaValue;
function EvaluatePrivatePropertyAssignment(PrivatePropertyAssignmentExpression: TGocciaPrivatePropertyAssignmentExpression; Context: TGocciaEvaluationContext): TGocciaValue;
function EvaluatePrivatePropertyCompoundAssignment(PrivatePropertyCompoundAssignmentExpression: TGocciaPrivatePropertyCompoundAssignmentExpression; Context: TGocciaEvaluationContext): TGocciaValue;
function EvaluateDestructuringAssignment(DestructuringAssignmentExpression: TGocciaDestructuringAssignmentExpression; Context: TGocciaEvaluationContext): TGocciaValue;
function EvaluateDestructuringDeclaration(DestructuringDeclaration: TGocciaDestructuringDeclaration; Context: TGocciaEvaluationContext): TGocciaValue;
function EvaluateTemplateLiteral(TemplateLiteralExpression: TGocciaTemplateLiteralExpression; Context: TGocciaEvaluationContext): TGocciaValue;
function EvaluateTemplateExpression(const ExpressionText: string; Context: TGocciaEvaluationContext; Line, Column: Integer): TGocciaValue;

// Destructuring pattern assignment procedures
procedure AssignPattern(Pattern: TGocciaDestructuringPattern; Value: TGocciaValue; Context: TGocciaEvaluationContext);
procedure AssignIdentifierPattern(Pattern: TGocciaIdentifierDestructuringPattern; Value: TGocciaValue; Context: TGocciaEvaluationContext);
procedure AssignArrayPattern(Pattern: TGocciaArrayDestructuringPattern; Value: TGocciaValue; Context: TGocciaEvaluationContext);
procedure AssignObjectPattern(Pattern: TGocciaObjectDestructuringPattern; Value: TGocciaValue; Context: TGocciaEvaluationContext);
procedure AssignAssignmentPattern(Pattern: TGocciaAssignmentDestructuringPattern; Value: TGocciaValue; Context: TGocciaEvaluationContext);
procedure AssignRestPattern(Pattern: TGocciaRestDestructuringPattern; Value: TGocciaValue; Context: TGocciaEvaluationContext);
procedure InitializeInstanceProperties(Instance: TGocciaInstanceValue; ClassValue: TGocciaClassValue; Context: TGocciaEvaluationContext);
procedure InitializePrivateInstanceProperties(Instance: TGocciaInstanceValue; ClassValue: TGocciaClassValue; Context: TGocciaEvaluationContext);

function IsObjectInstanceOfClass(Obj: TGocciaObjectValue; ClassValue: TGocciaClassValue): Boolean;

// Helper function to safely call OnError with proper error handling
procedure SafeOnError(Context: TGocciaEvaluationContext; const Message: string; Line, Column: Integer);

var
  // Global evaluation context for error handling inheritance
  GlobalEvaluationContext: TGocciaEvaluationContext;

implementation

uses
  Goccia.Values.ObjectPropertyDescriptor;

// Helper function to safely call OnError with proper error handling
procedure SafeOnError(Context: TGocciaEvaluationContext; const Message: string; Line, Column: Integer);
begin
  if Assigned(Context.OnError) then
    Context.OnError(Message, Line, Column);
end;



function Evaluate(Node: TGocciaASTNode; Context: TGocciaEvaluationContext): TGocciaValue;
begin
  // Set global context for function calls to inherit error handling
  if Assigned(Context.OnError) then
  begin
    GlobalEvaluationContext.OnError := Context.OnError;
    GlobalEvaluationContext.LoadModule := Context.LoadModule;
  end;

  Logger.Debug('Evaluate: Node class is %s', [Node.ClassName]);
  Logger.Debug('Evaluate: Node line: %d', [Node.Line]);
  Logger.Debug('Evaluate: Node column: %d', [Node.Column]);

  if Node is TGocciaExpression then
  begin
    Logger.Debug('Evaluate: Processing as Expression');
    Result := EvaluateExpression(TGocciaExpression(Node), Context);
    Logger.Debug('Evaluate: Expression result type: %s', [Result.ClassName]);
  end
  else if Node is TGocciaStatement then
  begin
    Logger.Debug('Evaluate: Processing as Statement');
    Result := EvaluateStatement(TGocciaStatement(Node), Context);
    Logger.Debug('Evaluate: Statement result type: %s', [Result.ClassName]);
  end
  else
  begin
    Logger.Debug('Evaluate: Unknown node type, returning undefined');
    Result := TGocciaUndefinedValue.Create;
  end;
  Logger.Debug('Evaluate: Final result type: %s', [Result.ClassName]);
end;

function EvaluateExpression(Expression: TGocciaExpression; Context: TGocciaEvaluationContext): TGocciaValue;
var
  LeftExpr: TGocciaExpression;
  Value: TGocciaValue;
  Obj: TGocciaValue;
  PropName: string;
  I: Integer;
  Callee: TGocciaValue;
  Arguments: TObjectList<TGocciaValue>;
  OldValue: TGocciaValue; // For increment/decrement operations
begin
  if Expression is TGocciaLiteralExpression then
    Result := TGocciaLiteralExpression(Expression).Value
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
    PropName := EvaluateExpression(TGocciaComputedPropertyAssignmentExpression(Expression).PropertyExpression, Context).ToString;
    Value := EvaluateExpression(TGocciaComputedPropertyAssignmentExpression(Expression).Value, Context);
    AssignComputedProperty(Obj, PropName, Value, Context.OnError, Expression.Line, Expression.Column);
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
    if (Obj is TGocciaInstanceValue) then
      Result := TGocciaInstanceValue(Obj).GetProperty(PropName)
    else if (Obj is TGocciaObjectValue) then
      Result := TGocciaObjectValue(Obj).GetProperty(PropName)
    else if (Obj is TGocciaClassValue) then
      Result := TGocciaClassValue(Obj).GetProperty(PropName)
    else if (Obj is TGocciaArrayValue) then
      Result := TGocciaArrayValue(Obj).GetProperty(PropName)
    else
      Result := TGocciaUndefinedValue.Create;
  end
  else if Expression is TGocciaComputedPropertyCompoundAssignmentExpression then
  begin
    // Computed property compound assignment (e.g., obj[expr] += value)
    Obj := EvaluateExpression(TGocciaComputedPropertyCompoundAssignmentExpression(Expression).ObjectExpr, Context);
    PropName := EvaluateExpression(TGocciaComputedPropertyCompoundAssignmentExpression(Expression).PropertyExpression, Context).ToString;
    Value := EvaluateExpression(TGocciaComputedPropertyCompoundAssignmentExpression(Expression).Value, Context);
    PerformPropertyCompoundAssignment(Obj, PropName, Value, TGocciaComputedPropertyCompoundAssignmentExpression(Expression).Operator, Context.OnError, Expression.Line, Expression.Column);

    // Get the final result
    if (Obj is TGocciaArrayValue) then
      Result := TGocciaArrayValue(Obj).GetProperty(PropName)
    else if (Obj is TGocciaInstanceValue) then
      Result := TGocciaInstanceValue(Obj).GetProperty(PropName)
    else if (Obj is TGocciaObjectValue) then
      Result := TGocciaObjectValue(Obj).GetProperty(PropName)
    else if (Obj is TGocciaClassValue) then
      Result := TGocciaClassValue(Obj).GetProperty(PropName)
    else
      Result := TGocciaUndefinedValue.Create;
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
      if (Obj is TGocciaInstanceValue) then
        OldValue := TGocciaInstanceValue(Obj).GetProperty(PropName)
      else if (Obj is TGocciaObjectValue) then
        OldValue := TGocciaObjectValue(Obj).GetProperty(PropName)
      else if (Obj is TGocciaClassValue) then
        OldValue := TGocciaClassValue(Obj).GetProperty(PropName)
      else
      begin
        Context.OnError('Cannot access property on non-object', Expression.Line, Expression.Column);
        Result := TGocciaUndefinedValue.Create;
        Exit;
      end;

      // Calculate new value
      Value := PerformIncrement(OldValue, TGocciaIncrementExpression(Expression).Operator = gttIncrement);

      // Set the new value (create property if it doesn't exist, like JavaScript)
      if (Obj is TGocciaInstanceValue) then
        TGocciaInstanceValue(Obj).DefineProperty(PropName,
          TGocciaPropertyDescriptorData.Create(Value, [pfEnumerable, pfConfigurable, pfWritable]))
      else if (Obj is TGocciaObjectValue) then
        TGocciaObjectValue(Obj).DefineProperty(PropName,
          TGocciaPropertyDescriptorData.Create(Value, [pfEnumerable, pfConfigurable, pfWritable]))
      else if (Obj is TGocciaClassValue) then
        TGocciaClassValue(Obj).SetProperty(PropName, Value);

      // Return value depends on prefix/postfix
      if TGocciaIncrementExpression(Expression).IsPrefix then
        Result := Value  // Prefix: return new value (++obj.prop)
      else
        Result := OldValue;  // Postfix: return old value (obj.prop++)
    end
    else
    begin
      Context.OnError('Invalid target for increment/decrement', Expression.Line, Expression.Column);
      Result := TGocciaUndefinedValue.Create;
    end;
  end
  else if Expression is TGocciaCallExpression then
  begin
    Logger.Debug('EvaluateExpression: TGocciaCallExpression - before EvaluateCall');
    Result := EvaluateCall(TGocciaCallExpression(Expression), Context);
    Logger.Debug('EvaluateExpression: TGocciaCallExpression - after EvaluateCall, result type: %s', [Result.ClassName]);
    Logger.Debug('EvaluateExpression: TGocciaCallExpression - result ToString: %s', [Result.ToString]);
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
    if EvaluateExpression(TGocciaConditionalExpression(Expression).Condition, Context).ToBoolean then
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
    Logger.Debug('EvaluateExpression: Handling TGocciaThisExpression');
    Result := Context.Scope.ThisValue;
  end
  else if Expression is TGocciaClassExpression then
  begin
    Logger.Debug('EvaluateExpression: Handling TGocciaClassExpression');
    Result := EvaluateClassExpression(TGocciaClassExpression(Expression), Context);
  end
  else if Expression is TGocciaSuperExpression then
  begin
    Logger.Debug('EvaluateExpression: Handling TGocciaSuperExpression');
    // Return the superclass - context should provide access to it
    if Context.Scope.ThisValue is TGocciaInstanceValue then
    begin
      if Assigned(TGocciaInstanceValue(Context.Scope.ThisValue).ClassValue.SuperClass) then
        Result := TGocciaInstanceValue(Context.Scope.ThisValue).ClassValue.SuperClass
      else
        Context.OnError('No superclass found', Expression.Line, Expression.Column);
    end
    else
      Context.OnError('super can only be used in a class method', Expression.Line, Expression.Column);
  end
  else if Expression is TGocciaPrivateMemberExpression then
  begin
    Logger.Debug('EvaluateExpression: Handling TGocciaPrivateMemberExpression');
    Result := EvaluatePrivateMember(TGocciaPrivateMemberExpression(Expression), Context);
  end
  else if Expression is TGocciaPrivatePropertyAssignmentExpression then
  begin
    Logger.Debug('EvaluateExpression: Handling TGocciaPrivatePropertyAssignmentExpression');
    Result := EvaluatePrivatePropertyAssignment(TGocciaPrivatePropertyAssignmentExpression(Expression), Context);
  end
  else if Expression is TGocciaPrivatePropertyCompoundAssignmentExpression then
  begin
    Logger.Debug('EvaluateExpression: Handling TGocciaPrivatePropertyCompoundAssignmentExpression');
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
    Result := TGocciaUndefinedValue.Create;
  end
  else if Expression is TGocciaDestructuringAssignmentExpression then
  begin
    Result := EvaluateDestructuringAssignment(TGocciaDestructuringAssignmentExpression(Expression), Context);
  end
  else
    Result := TGocciaUndefinedValue.Create;
  Logger.Debug('EvaluateExpression: Returning result type: %s .ToString: %s', [Result.ClassName, Result.ToString]);
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
  Logger.Debug('EvaluateStatement: Statement class is %s', [Statement.ClassName]);
  Logger.Debug('EvaluateStatement: Statement line: %d', [Statement.Line]);
  Logger.Debug('EvaluateStatement: Statement column: %d', [Statement.Column]);

  Result := TGocciaUndefinedValue.Create;

  if Statement is TGocciaExpressionStatement then
  begin
    Logger.Debug('EvaluateStatement: Processing ExpressionStatement');
    Result := EvaluateExpression(TGocciaExpressionStatement(Statement).Expression, Context);
    Logger.Debug('EvaluateStatement: ExpressionStatement result type: %s', [Result.ClassName]);
  end
      else if Statement is TGocciaVariableDeclaration then
  begin
    Logger.Debug('EvaluateStatement: Processing VariableDeclaration');
    Decl := TGocciaVariableDeclaration(Statement);
    Logger.Debug('EvaluateStatement: Processing %d variable(s)', [Length(Decl.Variables)]);

    // Process each variable in the declaration
    for I := 0 to Length(Decl.Variables) - 1 do
    begin
      Logger.Debug('EvaluateStatement: Variable name: %s', [Decl.Variables[I].Name]);
      Value := EvaluateExpression(Decl.Variables[I].Initializer, Context);
      Logger.Debug('EvaluateStatement: Initializer result type: %s', [Value.ClassName]);

      // Use the new Define/Assign pattern
      if Decl.IsConst then
        Context.Scope.DefineFromToken(Decl.Variables[I].Name, Value, gttConst)
      else
        Context.Scope.DefineFromToken(Decl.Variables[I].Name, Value, gttLet);

      Logger.Debug('EvaluateStatement: Variable %s defined in scope', [Decl.Variables[I].Name]);
    end;
  end
  else if Statement is TGocciaDestructuringDeclaration then
  begin
    Logger.Debug('EvaluateStatement: Processing DestructuringDeclaration');
    Result := EvaluateDestructuringDeclaration(TGocciaDestructuringDeclaration(Statement), Context);
  end
  else if Statement is TGocciaBlockStatement then
  begin
    Logger.Debug('EvaluateStatement: Processing BlockStatement');
    Result := EvaluateBlock(TGocciaBlockStatement(Statement), Context);
    Logger.Debug('EvaluateStatement: BlockStatement result type: %s', [Result.ClassName]);
  end
  else if Statement is TGocciaIfStatement then
  begin
    Logger.Debug('EvaluateStatement: Processing IfStatement');
    Result := EvaluateIf(TGocciaIfStatement(Statement), Context);
    Logger.Debug('EvaluateStatement: IfStatement result type: %s', [Result.ClassName]);
  end
  else if Statement is TGocciaForStatement then
  begin
    Logger.Debug('EvaluateStatement: Processing ForStatement (parsing only - no execution)');
    // For now, just return undefined since we only want parsing support
    Result := TGocciaUndefinedValue.Create;
    Logger.Debug('EvaluateStatement: ForStatement result type: %s', [Result.ClassName]);
  end
  else if Statement is TGocciaWhileStatement then
  begin
    Logger.Debug('EvaluateStatement: Processing WhileStatement (parsing only - no execution)');
    // For now, just return undefined since we only want parsing support
    Result := TGocciaUndefinedValue.Create;
    Logger.Debug('EvaluateStatement: WhileStatement result type: %s', [Result.ClassName]);
  end
  else if Statement is TGocciaDoWhileStatement then
  begin
    Logger.Debug('EvaluateStatement: Processing DoWhileStatement (parsing only - no execution)');
    // For now, just return undefined since we only want parsing support
    Result := TGocciaUndefinedValue.Create;
    Logger.Debug('EvaluateStatement: DoWhileStatement result type: %s', [Result.ClassName]);
  end
  else if Statement is TGocciaSwitchStatement then
  begin
    Logger.Debug('EvaluateStatement: Processing SwitchStatement (parsing only - no execution)');
    // For now, just return undefined since we only want parsing support
    Result := TGocciaUndefinedValue.Create;
    Logger.Debug('EvaluateStatement: SwitchStatement result type: %s', [Result.ClassName]);
  end
  else if Statement is TGocciaBreakStatement then
  begin
    Logger.Debug('EvaluateStatement: Processing BreakStatement (parsing only - no execution)');
    // For now, just return undefined since we only want parsing support
    Result := TGocciaUndefinedValue.Create;
    Logger.Debug('EvaluateStatement: BreakStatement result type: %s', [Result.ClassName]);
  end
  else if Statement is TGocciaReturnStatement then
  begin
    Logger.Debug('EvaluateStatement: Handling TGocciaReturnStatement');
    if Assigned(TGocciaReturnStatement(Statement).Value) then
    begin
      Logger.Debug('EvaluateStatement: Evaluating return value');
      Value := EvaluateExpression(TGocciaReturnStatement(Statement).Value, Context);
      Logger.Debug('EvaluateStatement: Return value type: %s', [Value.ClassName]);
      Logger.Debug('EvaluateStatement: Return value ToString: %s', [Value.ToString]);
      Logger.Debug('EvaluateStatement: Return value address: %p', [Pointer(Value)]);
      if Value = nil then
      begin
        Logger.Debug('EvaluateStatement: Return value is nil, creating TGocciaUndefinedValue');
        Value := TGocciaUndefinedValue.Create;
      end;
    end
    else
    begin
      Logger.Debug('EvaluateStatement: No return value, using undefined');
      Value := TGocciaUndefinedValue.Create;
    end;
    Logger.Debug('EvaluateStatement: Raising TGocciaReturnValue with value type: %s', [Value.ClassName]);
    raise TGocciaReturnValue.Create(Value);
  end
  else if Statement is TGocciaThrowStatement then
  begin
    Logger.Debug('EvaluateStatement: Processing ThrowStatement');
    Value := EvaluateExpression(TGocciaThrowStatement(Statement).Value, Context);
    Logger.Debug('EvaluateStatement: Throw value type: %s', [Value.ClassName]);
    Logger.Debug('EvaluateStatement: Throw value ToString: %s', [Value.ToString]);
    raise TGocciaThrowValue.Create(Value);
  end
  else if Statement is TGocciaTryStatement then
  begin
    Logger.Debug('EvaluateStatement: Processing TryStatement');
    Result := EvaluateTry(TGocciaTryStatement(Statement), Context);
    Logger.Debug('EvaluateStatement: TryStatement result type: %s', [Result.ClassName]);
  end
  else if Statement is TGocciaClassDeclaration then
  begin
    Logger.Debug('EvaluateStatement: Processing ClassDeclaration');
    Result := EvaluateClass(TGocciaClassDeclaration(Statement), Context);
    Logger.Debug('EvaluateStatement: ClassDeclaration result type: %s', [Result.ClassName]);
  end
  else if Statement is TGocciaImportDeclaration then
  begin
    Logger.Debug('EvaluateStatement: Processing ImportDeclaration');
    ImportDecl := TGocciaImportDeclaration(Statement);
    Logger.Debug('EvaluateStatement: Importing module: %s', [ImportDecl.ModulePath]);
    Module := Context.LoadModule(ImportDecl.ModulePath);

    for ImportPair in ImportDecl.Imports do
    begin
      Logger.Debug('EvaluateStatement: Importing %s as %s', [ImportPair.Key, ImportPair.Value]);
      if Module.ExportsTable.TryGetValue(ImportPair.Value, Value) then
      begin
        Logger.Debug('EvaluateStatement: Found export, type: %s', [Value.ClassName]);
        Context.Scope.DefineLexicalBinding(ImportPair.Key, Value, dtLet);
      end
      else
      begin
        Logger.Debug('EvaluateStatement: Export not found');
        Context.OnError(Format('Module "%s" has no export named "%s"',
          [ImportDecl.ModulePath, ImportPair.Value]), Statement.Line, Statement.Column);
      end;
    end;
  end;

  Logger.Debug('EvaluateStatement: Final result type: %s', [Result.ClassName]);
end;

function EvaluateBinary(BinaryExpression: TGocciaBinaryExpression; Context: TGocciaEvaluationContext): TGocciaValue;
var
  Left, Right: TGocciaValue;
begin
  // Handle short-circuiting logical operators first
  if BinaryExpression.Operator = gttAnd then
  begin
    Left := EvaluateExpression(BinaryExpression.Left, Context);
    if not Left.ToBoolean then
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
    if Left.ToBoolean then
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
    if (Left is TGocciaNullValue) or (Left is TGocciaUndefinedValue) then
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
      Result := TGocciaBooleanValue.Create(IsStrictEqual(Left, Right));
    gttNotEqual:
      Result := TGocciaBooleanValue.Create(IsNotStrictEqual(Left, Right));
    gttLess:
      Result := TGocciaBooleanValue.Create(LessThan(Left, Right));
    gttGreater:
      Result := TGocciaBooleanValue.Create(GreaterThan(Left, Right));
    gttLessEqual:
      Result := TGocciaBooleanValue.Create(LessThanOrEqual(Left, Right));
    gttGreaterEqual:
      Result := TGocciaBooleanValue.Create(GreaterThanOrEqual(Left, Right));
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
    Result := TGocciaUndefinedValue.Create;
  end;
end;

function EvaluateUnary(UnaryExpression: TGocciaUnaryExpression; Context: TGocciaEvaluationContext): TGocciaValue;
var
  Operand: TGocciaValue;
begin
  Operand := EvaluateExpression(UnaryExpression.Operand, Context);

  case UnaryExpression.Operator of
    gttNot:
      Result := TGocciaBooleanValue.Create(not Operand.ToBoolean);
    gttMinus:
      Result := TGocciaNumberValue.Create(-Operand.ToNumber);
    gttPlus:
      Result := TGocciaNumberValue.Create(Operand.ToNumber);
    gttTypeof:
      Result := EvaluateTypeof(Operand);
    gttBitwiseNot:
      Result := EvaluateBitwiseNot(Operand);
  else
    Result := TGocciaUndefinedValue.Create;
  end;
end;

function EvaluateCall(CallExpression: TGocciaCallExpression; Context: TGocciaEvaluationContext): TGocciaValue;
var
  Callee: TGocciaValue;
  Arguments: TObjectList<TGocciaValue>;
  ThisValue: TGocciaValue;
  ArgumentExpr: TGocciaExpression;
  SuperClass: TGocciaClassValue;
  MemberExpr: TGocciaMemberExpression;
  SpreadValue: TGocciaValue;
  SpreadArray: TGocciaArrayValue;
  I: Integer;
begin
  Logger.Debug('EvaluateCall: Start');
  Logger.Debug('  CallExpression.Callee: %s', [CallExpression.Callee.ToString]);


        // Handle super() calls specially
  if CallExpression.Callee is TGocciaSuperExpression then
  begin
    Logger.Debug('EvaluateCall: Calling super constructor');
    SuperClass := TGocciaClassValue(EvaluateExpression(CallExpression.Callee, Context));
    if not (SuperClass is TGocciaClassValue) then
    begin
      Context.OnError('super() can only be called within a method with a superclass',
        CallExpression.Line, CallExpression.Column);
      Result := TGocciaUndefinedValue.Create;
      Exit;
    end;

    Arguments := TObjectList<TGocciaValue>.Create(False);
    try
      for ArgumentExpr in CallExpression.Arguments do
        Arguments.Add(EvaluateExpression(ArgumentExpr, Context));

      // Call the superclass constructor with the current instance as 'this'
      if Assigned(SuperClass.ConstructorMethod) then
      begin
        Logger.Debug('EvaluateCall: Calling superclass constructor method');
        Result := SuperClass.ConstructorMethod.Call(Arguments, Context.Scope.ThisValue);
      end
      else
      begin
        Logger.Debug('EvaluateCall: No explicit constructor in superclass');
        Result := TGocciaUndefinedValue.Create;
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
  else
  begin
    // Regular function calls
    Callee := EvaluateExpression(CallExpression.Callee, Context);
    ThisValue := TGocciaUndefinedValue.Create;
  end;

  Arguments := TObjectList<TGocciaValue>.Create(False);
  try
    for ArgumentExpr in CallExpression.Arguments do
    begin
      if ArgumentExpr is TGocciaSpreadExpression then
      begin
        // Spread expression: expand arguments
        SpreadValue := EvaluateExpression(TGocciaSpreadExpression(ArgumentExpr).Argument, Context);

        if SpreadValue is TGocciaArrayValue then
        begin
          // Spread array elements as arguments
          SpreadArray := TGocciaArrayValue(SpreadValue);
          for I := 0 to SpreadArray.Elements.Count - 1 do
          begin
            // Convert holes (nil) to undefined when spreading
            if SpreadArray.Elements[I] = nil then
              Arguments.Add(TGocciaUndefinedValue.Create)
            else
              Arguments.Add(SpreadArray.Elements[I]);
          end;
        end
        else if SpreadValue is TGocciaStringValue then
        begin
          // Spread string characters as arguments
          for I := 1 to Length(SpreadValue.ToString) do
            Arguments.Add(TGocciaStringValue.Create(SpreadValue.ToString[I]));
        end
        else if not ((SpreadValue is TGocciaNullValue) or (SpreadValue is TGocciaUndefinedValue)) then
        begin
          Context.OnError('Spread syntax requires an iterable', ArgumentExpr.Line, ArgumentExpr.Column);
          // If OnError doesn't throw, continue execution (but the spread will be ignored)
        end;
        // null and undefined are ignored in spread context
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
    else if Callee is IGocciaCallable then
      Result := (Callee as IGocciaCallable).Call(Arguments, ThisValue)
    else
    begin
      SafeOnError(Context, Format('%s is not a function', [Callee.TypeName]), CallExpression.Line, CallExpression.Column);
      // If OnError doesn't throw, return undefined
      Result := TGocciaUndefinedValue.Create;
    end;

  finally
    Arguments.Free;
  end;
end;

function EvaluateMember(MemberExpression: TGocciaMemberExpression; Context: TGocciaEvaluationContext): TGocciaValue;
var
  Obj: TGocciaValue;
  PropertyName: string;
  PropertyValue: TGocciaValue;
  SuperClass: TGocciaClassValue;
  BoxedValue: TGocciaObjectValue;
begin
  Logger.Debug('EvaluateMember: Start');
  Logger.Debug('  MemberExpression.ObjectExpr: %s', [MemberExpression.ObjectExpr.ToString]);


  // Handle super.method() specially
  if MemberExpression.ObjectExpr is TGocciaSuperExpression then
  begin
    Logger.Debug('EvaluateMember: Accessing super property');
    SuperClass := TGocciaClassValue(EvaluateExpression(MemberExpression.ObjectExpr, Context));
    if not (SuperClass is TGocciaClassValue) then
    begin
      Context.OnError('super can only be used within a method with a superclass',
        MemberExpression.Line, MemberExpression.Column);
      Result := TGocciaUndefinedValue.Create;
      Exit;
    end;

    // Get the property name
    if MemberExpression.Computed and Assigned(MemberExpression.PropertyExpression) then
    begin
      PropertyValue := EvaluateExpression(MemberExpression.PropertyExpression, Context);
      PropertyName := PropertyValue.ToString;
    end
    else
    begin
      PropertyName := MemberExpression.PropertyName;
    end;

    Logger.Debug('EvaluateMember: Looking for super method: %s', [PropertyName]);

    // Get the method from the superclass
    Result := SuperClass.GetMethod(PropertyName);
    if not Assigned(Result) then
      Result := TGocciaUndefinedValue.Create
    else
    begin
      // For super.method() calls, we need to create a bound method that uses the current 'this'
      // This is handled in the call evaluation where we have access to the current 'this'
      Logger.Debug('EvaluateMember: Super method found: %s', [Result.ToString]);
    end;

    Exit;
  end;

  Obj := EvaluateExpression(MemberExpression.ObjectExpr, Context);
  Logger.Debug('EvaluateMember: Obj: %s', [Obj.ToString]);

  // Determine the property name
  if MemberExpression.Computed and Assigned(MemberExpression.PropertyExpression) then
  begin
    // Computed access: evaluate the property expression to get the property name
    PropertyValue := EvaluateExpression(MemberExpression.PropertyExpression, Context);
    PropertyName := PropertyValue.ToString;
    Logger.Debug('EvaluateMember: Computed property name: %s', [PropertyName]);
  end
  else
  begin
    // Static access: use the property name directly
    PropertyName := MemberExpression.PropertyName;
    Logger.Debug('EvaluateMember: Static property name: %s', [PropertyName]);
  end;

  if Obj is TGocciaArrayValue then
  begin
    Logger.Debug('EvaluateMember: Obj is TGocciaArrayValue');
    Result := TGocciaArrayValue(Obj).GetProperty(PropertyName);
    Logger.Debug('EvaluateMember: Result: %s', [Result.ToString]);
  end
  else if Obj is TGocciaClassValue then
  begin
    Logger.Debug('EvaluateMember: Obj is TGocciaClassValue');
    Result := TGocciaClassValue(Obj).GetProperty(PropertyName);
    Logger.Debug('EvaluateMember: Result: %s', [Result.ToString]);
  end
  else if Obj is TGocciaObjectValue then
  begin
    Logger.Debug('EvaluateMember: Obj is TGocciaObjectValue');
    Result := TGocciaObjectValue(Obj).GetProperty(PropertyName);
    Logger.Debug('EvaluateMember: Result: %s', [Result.ToString]);
  end
  else
  begin
    // Handle primitive boxing for property access
    Logger.Debug('EvaluateMember: Attempting primitive boxing for: %s', [Obj.ClassName]);
    BoxedValue := BoxPrimitive(Obj);
    if Assigned(BoxedValue) then
    begin
      Logger.Debug('EvaluateMember: Primitive boxed successfully');
      Result := BoxedValue.GetProperty(PropertyName);
      Logger.Debug('EvaluateMember: Boxed result: %s', [Result.ToString]);
    end
    else if (Obj is TGocciaNullValue) or (Obj is TGocciaUndefinedValue) then
    begin
      // null/undefined property access should throw
      Context.OnError('Cannot read property ''' + PropertyName + ''' of ' + Obj.ToString,
        MemberExpression.Line, MemberExpression.Column);
      Result := TGocciaUndefinedValue.Create;
    end
    else
    begin
      Logger.Debug('EvaluateMember: Obj is not a supported object type');
      Result := TGocciaUndefinedValue.Create;
    end;
  end;
  Logger.Debug('EvaluateMember: Returning result type: %s', [Result.ClassName]);
  Logger.Debug('EvaluateMember: Result ToString: %s', [Result.ToString]);
end;

function EvaluateMember(MemberExpression: TGocciaMemberExpression; Context: TGocciaEvaluationContext; out ObjectValue: TGocciaValue): TGocciaValue;
var
  PropertyName: string;
  PropertyValue: TGocciaValue;
  SuperClass: TGocciaClassValue;
  BoxedValue: TGocciaObjectValue;
begin
  Logger.Debug('EvaluateMember: Start (dual overload)');
  Logger.Debug('  MemberExpression.ObjectExpr: %s', [MemberExpression.ObjectExpr.ToString]);

  // Handle super.method() specially
  if MemberExpression.ObjectExpr is TGocciaSuperExpression then
  begin
    Logger.Debug('EvaluateMember: Accessing super property');
    SuperClass := TGocciaClassValue(EvaluateExpression(MemberExpression.ObjectExpr, Context));
    if not (SuperClass is TGocciaClassValue) then
    begin
      Context.OnError('super can only be used within a method with a superclass',
        MemberExpression.Line, MemberExpression.Column);
      Result := TGocciaUndefinedValue.Create;
      ObjectValue := TGocciaUndefinedValue.Create;
      Exit;
    end;

    ObjectValue := SuperClass; // For super calls, the object is the superclass

    // Get the property name
    if MemberExpression.Computed and Assigned(MemberExpression.PropertyExpression) then
    begin
      PropertyValue := EvaluateExpression(MemberExpression.PropertyExpression, Context);
      PropertyName := PropertyValue.ToString;
    end
    else
    begin
      PropertyName := MemberExpression.PropertyName;
    end;

    Logger.Debug('EvaluateMember: Looking for super method: %s', [PropertyName]);

    // Get the method from the superclass
    Result := SuperClass.GetMethod(PropertyName);
    if not Assigned(Result) then
      Result := TGocciaUndefinedValue.Create
    else
    begin
      Logger.Debug('EvaluateMember: Super method found: %s', [Result.ToString]);
    end;

    Exit;
  end;

  // Evaluate object expression once and store it
  ObjectValue := EvaluateExpression(MemberExpression.ObjectExpr, Context);
  Logger.Debug('EvaluateMember: Obj: %s', [ObjectValue.ToString]);

  // Determine the property name
  if MemberExpression.Computed and Assigned(MemberExpression.PropertyExpression) then
  begin
    // Computed access: evaluate the property expression to get the property name
    PropertyValue := EvaluateExpression(MemberExpression.PropertyExpression, Context);
    PropertyName := PropertyValue.ToString;
    Logger.Debug('EvaluateMember: Computed property name: %s', [PropertyName]);
  end
  else
  begin
    // Static access: use the property name directly
    PropertyName := MemberExpression.PropertyName;
    Logger.Debug('EvaluateMember: Static property name: %s', [PropertyName]);
  end;

  if ObjectValue is TGocciaArrayValue then
  begin
    Logger.Debug('EvaluateMember: Obj is TGocciaArrayValue');
    Result := TGocciaArrayValue(ObjectValue).GetProperty(PropertyName);
    Logger.Debug('EvaluateMember: Result: %s', [Result.ToString]);
  end
  else if ObjectValue is TGocciaClassValue then
  begin
    Logger.Debug('EvaluateMember: Obj is TGocciaClassValue');
    Result := TGocciaClassValue(ObjectValue).GetProperty(PropertyName);
    Logger.Debug('EvaluateMember: Result: %s', [Result.ToString]);
  end
  else if ObjectValue is TGocciaInstanceValue then
  begin
    Logger.Debug('EvaluateMember: Obj is TGocciaInstanceValue');
    Result := TGocciaInstanceValue(ObjectValue).GetProperty(PropertyName);
    Logger.Debug('EvaluateMember: Result: %s', [Result.ToString]);
  end
  else if ObjectValue is TGocciaObjectValue then
  begin
    Logger.Debug('EvaluateMember: Obj is TGocciaObjectValue');
    Result := TGocciaObjectValue(ObjectValue).GetProperty(PropertyName);
    Logger.Debug('EvaluateMember: Result: %s', [Result.ToString]);
  end
  else
  begin
    // Handle primitive boxing for property access
    Logger.Debug('EvaluateMember: Attempting primitive boxing for: %s', [ObjectValue.ClassName]);
    BoxedValue := BoxPrimitive(ObjectValue);
    if Assigned(BoxedValue) then
    begin
      Logger.Debug('EvaluateMember: Primitive boxed successfully');
      Result := BoxedValue.GetProperty(PropertyName);
      Logger.Debug('EvaluateMember: Boxed result: %s', [Result.ToString]);
      // Note: ObjectValue remains the original primitive for 'this' binding
    end
    else if (ObjectValue is TGocciaNullValue) or (ObjectValue is TGocciaUndefinedValue) then
    begin
      // null/undefined property access should throw
      Context.OnError('Cannot read property ''' + PropertyName + ''' of ' + ObjectValue.ToString,
        MemberExpression.Line, MemberExpression.Column);
      Result := TGocciaUndefinedValue.Create;
    end
    else
    begin
      Logger.Debug('EvaluateMember: Obj is not a supported object type');
      Result := TGocciaUndefinedValue.Create;
    end;
  end;
  Logger.Debug('EvaluateMember: Returning result type: %s', [Result.ClassName]);
  Logger.Debug('EvaluateMember: Result ToString: %s', [Result.ToString]);
end;

function EvaluateArray(ArrayExpression: TGocciaArrayExpression; Context: TGocciaEvaluationContext): TGocciaValue;
var
  Arr: TGocciaArrayValue;
  I, J: Integer;
  ElementValue: TGocciaValue;
  SpreadValue: TGocciaValue;
  SpreadArray: TGocciaArrayValue;
  SpreadObj: TGocciaObjectValue;
  PropName: string;
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
      // Spread expression: expand the array/iterable
      SpreadValue := EvaluateExpression(TGocciaSpreadExpression(ArrayExpression.Elements[I]).Argument, Context);

      if SpreadValue is TGocciaArrayValue then
      begin
        // Spread array elements
        SpreadArray := TGocciaArrayValue(SpreadValue);
        for J := 0 to SpreadArray.Elements.Count - 1 do
        begin
          // Convert holes (nil) to undefined when spreading
          if SpreadArray.Elements[J] = nil then
            Arr.Elements.Add(TGocciaUndefinedValue.Create)
          else
            Arr.Elements.Add(SpreadArray.Elements[J]);
        end;
      end
      else if SpreadValue is TGocciaStringValue then
      begin
        // Spread string characters
        for J := 1 to Length(SpreadValue.ToString) do
          Arr.Elements.Add(TGocciaStringValue.Create(SpreadValue.ToString[J]));
      end
      else if (SpreadValue is TGocciaObjectValue) then
      begin
        // For objects, we would need iterator support - for now just ignore
        // This is complex and would need proper iterator protocol implementation
      end
      else if not ((SpreadValue is TGocciaNullValue) or (SpreadValue is TGocciaUndefinedValue)) then
      begin
        Context.OnError('Spread syntax requires an iterable', ArrayExpression.Elements[I].Line, ArrayExpression.Elements[I].Column);
      end;
      // null and undefined are ignored in spread context
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
  Pair: TPair<string, TGocciaExpression>;
  ComputedPair: TPair<TGocciaExpression, TGocciaExpression>;
  ComputedKey: string;
  SpreadValue: TGocciaValue;
  SpreadObj: TGocciaObjectValue;
  SpreadArray: TGocciaArrayValue;
  Key: string;
  I: Integer;
  GetterPair: TPair<string, TGocciaGetterExpression>;
  SetterPair: TPair<string, TGocciaSetterExpression>;
  GetterFunction: TGocciaValue;
  SetterFunction: TGocciaValue;
  PropertyName: string;
  PropertyExpression: TGocciaExpression;
begin
  Obj := TGocciaObjectValue.Create;

  // Handle static properties in insertion order: {key: value, "key2": value2}
  if Assigned(ObjectExpression.Properties) and Assigned(ObjectExpression.PropertyInsertionOrder) then
  begin
    for I := 0 to ObjectExpression.PropertyInsertionOrder.Count - 1 do
    begin
      PropertyName := ObjectExpression.PropertyInsertionOrder[I];
      if ObjectExpression.Properties.TryGetValue(PropertyName, PropertyExpression) then
        Obj.DefineProperty(PropertyName, TGocciaPropertyDescriptorData.Create(EvaluateExpression(PropertyExpression, Context), [pfEnumerable, pfConfigurable, pfWritable]));
    end;
  end;

  // TODO: We shouldn't treat getters and setters separately, but as a single property descriptor

  // Handle getters: {get propertyName() { ... }}
  if Assigned(ObjectExpression.Getters) then
  begin
    for GetterPair in ObjectExpression.Getters do
    begin
      GetterFunction := EvaluateGetter(GetterPair.Value, Context);
      Obj.DefineProperty(GetterPair.Key, TGocciaPropertyDescriptorAccessor.Create(GetterFunction, nil, [pfEnumerable, pfConfigurable, pfWritable]));
    end;
  end;

  // Handle setters: {set propertyName(value) { ... }}
  if Assigned(ObjectExpression.Setters) then
  begin
    for SetterPair in ObjectExpression.Setters do
    begin
      SetterFunction := EvaluateSetter(SetterPair.Value, Context);
      Obj.DefineProperty(SetterPair.Key, TGocciaPropertyDescriptorAccessor.Create(nil, SetterFunction, [pfEnumerable, pfConfigurable, pfWritable]));
    end;
  end;

  // Handle computed properties and spread expressions: {[expr]: value, ...obj}
  if Assigned(ObjectExpression.ComputedProperties) then
  begin
    for ComputedPair in ObjectExpression.ComputedProperties do
    begin
      if ComputedPair.Key is TGocciaSpreadExpression then
      begin
        // Spread expression: copy all enumerable properties
        SpreadValue := EvaluateExpression(TGocciaSpreadExpression(ComputedPair.Key).Argument, Context);

        if SpreadValue is TGocciaObjectValue then
        begin
          // Spread object properties
          SpreadObj := TGocciaObjectValue(SpreadValue);
          for Key in SpreadObj.GetEnumerablePropertyNames do
            Obj.AssignProperty(Key, SpreadObj.GetProperty(Key));
        end
        else if SpreadValue is TGocciaArrayValue then
        begin
          // Spread array as indexed properties
          SpreadArray := TGocciaArrayValue(SpreadValue);
          for I := 0 to SpreadArray.Elements.Count - 1 do
          begin
            if SpreadArray.Elements[I] <> nil then
              Obj.AssignProperty(IntToStr(I), SpreadArray.Elements[I]);
          end;
        end
        else if SpreadValue is TGocciaStringValue then
        begin
          // Spread string as indexed properties
          for I := 1 to Length(SpreadValue.ToString) do
            Obj.AssignProperty(IntToStr(I - 1), TGocciaStringValue.Create(SpreadValue.ToString[I]));
        end
        else if not ((SpreadValue is TGocciaNullValue) or (SpreadValue is TGocciaUndefinedValue)) then
        begin
          // Other primitives (numbers, booleans) don't have enumerable properties
          // So they result in empty object spread, which is valid
        end;
        // null and undefined are ignored in spread context
      end
      else
      begin
        // Regular computed property: {[expr]: value}
        ComputedKey := EvaluateExpression(ComputedPair.Key, Context).ToString;
        // Set the property (computed properties can overwrite static ones)
        Obj.AssignProperty(ComputedKey, EvaluateExpression(ComputedPair.Value, Context));
      end;
    end;
  end;

  Result := Obj;
end;

function EvaluateGetter(GetterExpression: TGocciaGetterExpression; Context: TGocciaEvaluationContext): TGocciaValue;
var
  Statements: TObjectList<TGocciaASTNode>;
  I: Integer;
  OriginalNodes: TObjectList<TGocciaASTNode>;
  EmptyParameters: TGocciaParameterArray;
begin
  // Getter has no parameters
  SetLength(EmptyParameters, 0);

  // Create a copy of the statements to avoid ownership issues
  OriginalNodes := TGocciaBlockStatement(GetterExpression.Body).Nodes;
  Statements := TObjectList<TGocciaASTNode>.Create(False); // Don't own the objects
  for I := 0 to OriginalNodes.Count - 1 do
    Statements.Add(OriginalNodes[I]);

  // Create function with closure scope
  Result := TGocciaFunctionValue.Create(EmptyParameters, Statements, Context.Scope.CreateChild);
end;

function EvaluateSetter(SetterExpression: TGocciaSetterExpression; Context: TGocciaEvaluationContext): TGocciaValue;
var
  Statements: TObjectList<TGocciaASTNode>;
  I: Integer;
  OriginalNodes: TObjectList<TGocciaASTNode>;
  Parameters: TGocciaParameterArray;
begin
  // Setter has one parameter
  SetLength(Parameters, 1);
  Parameters[0].Name := SetterExpression.Parameter;
  Parameters[0].DefaultValue := nil; // No default value for setter parameter

  // Create a copy of the statements to avoid ownership issues
  OriginalNodes := TGocciaBlockStatement(SetterExpression.Body).Nodes;
  Statements := TObjectList<TGocciaASTNode>.Create(False); // Don't own the objects
  for I := 0 to OriginalNodes.Count - 1 do
    Statements.Add(OriginalNodes[I]);

  // Create function with closure scope
  Result := TGocciaFunctionValue.Create(Parameters, Statements, Context.Scope.CreateChild);
end;

function EvaluateArrowFunction(ArrowFunctionExpression: TGocciaArrowFunctionExpression; Context: TGocciaEvaluationContext): TGocciaValue;
var
  Statements: TObjectList<TGocciaASTNode>;
  I: Integer;
  OriginalNodes: TObjectList<TGocciaASTNode>;
begin
  // Arrow function body can be either a block statement or a single expression
  if ArrowFunctionExpression.Body is TGocciaBlockStatement then
  begin
    // Body is a block statement: (n) => { return n * 2; }
    // Create a copy of the statements to avoid ownership issues
    OriginalNodes := TGocciaBlockStatement(ArrowFunctionExpression.Body).Nodes;
    Statements := TObjectList<TGocciaASTNode>.Create(False); // Don't own the objects
    for I := 0 to OriginalNodes.Count - 1 do
      Statements.Add(OriginalNodes[I]);
  end
  else
  begin
    // Body is a single expression: (n) => n * 2
    // Wrap the expression in a statement list
    Statements := TObjectList<TGocciaASTNode>.Create(False); // Don't own the objects
    Statements.Add(ArrowFunctionExpression.Body); // Add the expression directly
  end;

  // Create function with closure scope - we need to avoid dangling references to temporary call scopes
  // But we also need to preserve access to the current scope's variables
  // The simplest approach: create a child scope that will be persistent
  Result := TGocciaFunctionValue.Create(ArrowFunctionExpression.Parameters, Statements, Context.Scope.CreateChild);
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
    LastValue := TGocciaUndefinedValue.Create;

    for I := 0 to BlockStatement.Nodes.Count - 1 do
    begin
      try
        LastValue := Evaluate(BlockStatement.Nodes[I], BlockContext);
      except
        on E: TGocciaReturnValue do
        begin
          raise;
        end;
        on E: TGocciaThrowValue do
        begin
          // Let thrown values bubble up for try-catch handling
          raise;
        end;
        on E: Exception do
        begin
          raise TGocciaError.Create('Error executing statement: ' + E.Message, 0, 0, '', nil);
        end;
      end;
    end;

    if LastValue = nil then
      LastValue := TGocciaUndefinedValue.Create;
    Result := LastValue;
  finally
    // Only free the scope if we created a child scope
    if NeedsChildScope then
      BlockContext.Scope.Free;
  end;
end;

function EvaluateIf(IfStatement: TGocciaIfStatement; Context: TGocciaEvaluationContext): TGocciaValue;
begin
  if EvaluateExpression(IfStatement.Condition, Context).ToBoolean then
    Result := EvaluateStatement(IfStatement.Consequent, Context)
  else if Assigned(IfStatement.Alternate) then
    Result := EvaluateStatement(IfStatement.Alternate, Context)
  else
    Result := TGocciaUndefinedValue.Create;
end;

function EvaluateTry(TryStatement: TGocciaTryStatement; Context: TGocciaEvaluationContext): TGocciaValue;
var
  TryResult: TGocciaValue;
  CatchScope: TGocciaScope;
  CatchContext: TGocciaEvaluationContext;
  I: Integer;
begin
  Logger.Debug('EvaluateTry: Starting try-catch-finally evaluation');
  Result := TGocciaUndefinedValue.Create;

    try
                                          // Execute the try block using original context (no scope isolation)
      Logger.Debug('EvaluateTry: Executing try block');

      // Execute try block statements directly in original context
      TryResult := TGocciaUndefinedValue.Create;
      for I := 0 to TryStatement.Block.Nodes.Count - 1 do
      begin
        try
          TryResult := Evaluate(TryStatement.Block.Nodes[I], Context);
        except
          on E: TGocciaReturnValue do
          begin
            raise;
          end;
          on E: TGocciaThrowValue do
          begin
            raise;
          end;
          on E: Exception do
          begin
            raise TGocciaError.Create('Error executing statement: ' + E.Message, 0, 0, '', nil);
          end;
        end;
      end;
      Result := TryResult;
      Logger.Debug('EvaluateTry: Try block completed successfully');
    except
      on E: TGocciaThrowValue do
      begin
        // Handle JavaScript throw statements
        Logger.Debug('EvaluateTry: Caught TGocciaThrowValue: %s', [E.Value.ToString]);

        if Assigned(TryStatement.CatchBlock) then
        begin
          Logger.Debug('EvaluateTry: Executing catch block with parameter: %s', [TryStatement.CatchParam]);

                                                                                // Create child scope for catch parameter if needed (proper shadowing)
          if TryStatement.CatchParam <> '' then
          begin
            CatchScope := TGocciaCatchScope.Create(Context.Scope, TryStatement.CatchParam);
            try
              // Bind the thrown value to the catch parameter (shadows outer variables)
              CatchScope.DefineBuiltin(TryStatement.CatchParam, E.Value);

              // Set up context for catch block with child scope
              CatchContext := Context;
              CatchContext.Scope := CatchScope;

              // Execute catch block statements with proper scoping
              Result := TGocciaUndefinedValue.Create;
              for I := 0 to TryStatement.CatchBlock.Nodes.Count - 1 do
              begin
                try
                  Result := Evaluate(TryStatement.CatchBlock.Nodes[I], CatchContext);
                except
                  on E: TGocciaReturnValue do
                  begin
                    raise;
                  end;
                  on E: TGocciaThrowValue do
                  begin
                    raise;
                  end;
                  on E: Exception do
                  begin
                    raise TGocciaError.Create('Error executing statement: ' + E.Message, 0, 0, '', nil);
                  end;
                end;
              end;
            finally
              CatchScope.Free;
            end;
          end
          else
          begin
            // No catch parameter - use original context directly
            Result := TGocciaUndefinedValue.Create;
            for I := 0 to TryStatement.CatchBlock.Nodes.Count - 1 do
            begin
              try
                Result := Evaluate(TryStatement.CatchBlock.Nodes[I], Context);
              except
                on E: TGocciaReturnValue do
                begin
                  raise;
                end;
                on E: TGocciaThrowValue do
                begin
                  raise;
                end;
                on E: Exception do
                begin
                  raise TGocciaError.Create('Error executing statement: ' + E.Message, 0, 0, '', nil);
                end;
              end;
            end;
          end;
          Logger.Debug('EvaluateTry: Catch block completed');
        end
        else
        begin
          // No catch block, re-throw the exception
          Logger.Debug('EvaluateTry: No catch block, re-throwing exception');
          raise;
        end;
      end;
      on E: TGocciaReturnValue do
      begin
        // Return statements should bubble up through try-catch
        Logger.Debug('EvaluateTry: Return value bubbling up');
        raise;
      end;
      on E: Exception do
      begin
        // Handle other Pascal exceptions as runtime errors
        Logger.Debug('EvaluateTry: Caught Pascal exception: %s', [E.Message]);

        if Assigned(TryStatement.CatchBlock) then
        begin
          Logger.Debug('EvaluateTry: Converting Pascal exception to JavaScript Error');

                              // Create child scope for catch parameter if needed (proper shadowing)
          if TryStatement.CatchParam <> '' then
          begin
            CatchScope := TGocciaCatchScope.Create(Context.Scope, TryStatement.CatchParam);
            try
              // Create a JavaScript Error object for the Pascal exception
              CatchScope.DefineBuiltin(TryStatement.CatchParam, TGocciaStringValue.Create(E.Message));

              // Set up context for catch block with child scope of statement scope
              CatchContext := Context;
              CatchContext.Scope := CatchScope;

              // Execute catch block statements with proper scoping
              Result := TGocciaUndefinedValue.Create;
              for I := 0 to TryStatement.CatchBlock.Nodes.Count - 1 do
              begin
                try
                  Result := Evaluate(TryStatement.CatchBlock.Nodes[I], CatchContext);
                except
                  on E: TGocciaReturnValue do
                  begin
                    raise;
                  end;
                  on E: TGocciaThrowValue do
                  begin
                    raise;
                  end;
                  on E: Exception do
                  begin
                    raise TGocciaError.Create('Error executing statement: ' + E.Message, 0, 0, '', nil);
                  end;
                end;
              end;
            finally
              CatchScope.Free;
            end;
          end
                    else
          begin
            // No catch parameter - use original context directly
            Result := TGocciaUndefinedValue.Create;
            for I := 0 to TryStatement.CatchBlock.Nodes.Count - 1 do
            begin
              try
                Result := Evaluate(TryStatement.CatchBlock.Nodes[I], Context);
              except
                on E: TGocciaReturnValue do
                begin
                  raise;
                end;
                on E: TGocciaThrowValue do
                begin
                  raise;
                end;
                on E: Exception do
                begin
                  raise TGocciaError.Create('Error executing statement: ' + E.Message, 0, 0, '', nil);
                end;
              end;
            end;
          end;
          Logger.Debug('EvaluateTry: Catch block completed for Pascal exception');
        end
        else
        begin
          // No catch block, re-throw as JavaScript error
          Logger.Debug('EvaluateTry: No catch block, re-throwing as TGocciaThrowValue');
          raise TGocciaThrowValue.Create(TGocciaStringValue.Create(E.Message));
        end;
      end;
    end;

    // Always execute finally block if present
    if Assigned(TryStatement.FinallyBlock) then
    begin
      Logger.Debug('EvaluateTry: Executing finally block');
      try
                    // Execute finally block using original context (no scope isolation)

      // Execute finally block statements directly in original context
      for I := 0 to TryStatement.FinallyBlock.Nodes.Count - 1 do
      begin
        try
          Evaluate(TryStatement.FinallyBlock.Nodes[I], Context);
        except
          on E: TGocciaReturnValue do
          begin
            raise;
          end;
          on E: TGocciaThrowValue do
          begin
            raise;
          end;
          on E: Exception do
          begin
            raise TGocciaError.Create('Error executing statement: ' + E.Message, 0, 0, '', nil);
          end;
        end;
      end;
        Logger.Debug('EvaluateTry: Finally block completed');
      except
        on E: TGocciaReturnValue do
        begin
          // Return in finally block overrides any previous result
          Logger.Debug('EvaluateTry: Return in finally block');
          raise;
        end;
        on E: TGocciaThrowValue do
        begin
          // Exception in finally block overrides any previous result
          Logger.Debug('EvaluateTry: Exception in finally block');
          raise;
        end;
        on E: Exception do
        begin
          // Convert Pascal exceptions in finally block
          Logger.Debug('EvaluateTry: Pascal exception in finally block');
          raise TGocciaThrowValue.Create(TGocciaStringValue.Create(E.Message));
        end;
      end;
    end;

  Logger.Debug('EvaluateTry: Try-catch-finally evaluation completed');
end;

function EvaluateClassMethod(ClassMethod: TGocciaClassMethod; Context: TGocciaEvaluationContext; SuperClass: TGocciaValue = nil): TGocciaValue;
var
  Statements: TObjectList<TGocciaASTNode>;
  I: Integer;
  OriginalNodes: TObjectList<TGocciaASTNode>;
begin
  // Create a copy of the statements to avoid ownership issues
  OriginalNodes := TGocciaBlockStatement(ClassMethod.Body).Nodes;
  Statements := TObjectList<TGocciaASTNode>.Create(False); // Don't own the objects
  for I := 0 to OriginalNodes.Count - 1 do
    Statements.Add(OriginalNodes[I]);

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
  PropertyPair: TPair<string, TGocciaExpression>;
  PropertyValue: TGocciaValue;
begin
  // Initialize inherited properties first (parent to child order)
  if Assigned(ClassValue.SuperClass) then
    InitializeInstanceProperties(Instance, ClassValue.SuperClass, Context);

  // Then initialize this class's properties (child properties can shadow parent properties)
  for PropertyPair in ClassValue.InstancePropertyDefs do
  begin
    Logger.Debug('Initializing instance property: %s from class: %s', [PropertyPair.Key, ClassValue.Name]);
    // Evaluate the property expression in the current context
    PropertyValue := EvaluateExpression(PropertyPair.Value, Context);
    Instance.AssignProperty(PropertyPair.Key, PropertyValue);
  end;
end;

function EvaluateNewExpression(NewExpression: TGocciaNewExpression; Context: TGocciaEvaluationContext): TGocciaValue;
var
  Callee: TGocciaValue;
  Arguments: TObjectList<TGocciaValue>;
  I: Integer;
  Instance: TGocciaInstanceValue;
  ClassValue: TGocciaClassValue;
begin
  // Evaluate the callee (class)
  Callee := EvaluateExpression(NewExpression.Callee, Context);

  // Evaluate arguments
  Arguments := TObjectList<TGocciaValue>.Create(False);
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
      InitializeInstanceProperties(Instance, ClassValue, Context);
      InitializePrivateInstanceProperties(Instance, ClassValue, Context);

      // Step 3: Call constructor (properties are now available)
      if Assigned(ClassValue.ConstructorMethod) then
        ClassValue.ConstructorMethod.Call(Arguments, Instance)
      else if Assigned(ClassValue.SuperClass) and Assigned(ClassValue.SuperClass.ConstructorMethod) then
        ClassValue.SuperClass.ConstructorMethod.Call(Arguments, Instance);

      Result := Instance;
    end
    else
      Context.OnError(Format('Cannot instantiate non-class value: %s', [Callee.TypeName]), NewExpression.Line, NewExpression.Column);
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
  ClassValue: TGocciaClassValue;
  MethodPair: TPair<string, TGocciaClassMethod>;
  PropertyPair: TPair<string, TGocciaExpression>;
  GetterPair: TPair<string, TGocciaGetterExpression>;
  SetterPair: TPair<string, TGocciaSetterExpression>;
  Method: TGocciaMethodValue;
  PropertyValue: TGocciaValue;
  GetterFunction, SetterFunction: TGocciaFunctionValue;
  ClassName: string;
begin
  SuperClass := nil;
  if ClassDef.SuperClass <> '' then
  begin
    SuperClass := TGocciaClassValue(Context.Scope.GetValue(ClassDef.SuperClass));
    if SuperClass = nil then
      Context.OnError(Format('Superclass "%s" not found', [ClassDef.SuperClass]), Line, Column);
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

  // Store instance property definitions on the class (they will be evaluated during instantiation)
  for PropertyPair in ClassDef.InstanceProperties do
  begin
    ClassValue.AddInstanceProperty(PropertyPair.Key, PropertyPair.Value);
  end;

  // Store private instance property definitions on the class
  for PropertyPair in ClassDef.PrivateInstanceProperties do
  begin
    ClassValue.AddPrivateInstanceProperty(PropertyPair.Key, PropertyPair.Value);
  end;

  // Store private methods on the class
  for MethodPair in ClassDef.PrivateMethods do
  begin
    Method := TGocciaMethodValue(EvaluateClassMethod(MethodPair.Value, Context, SuperClass));
    ClassValue.AddPrivateMethod(MethodPair.Key, Method);
  end;

  // Handle getters and setters

  for GetterPair in ClassDef.Getters do
  begin
    GetterFunction := TGocciaFunctionValue(EvaluateGetter(GetterPair.Value, Context));
    ClassValue.AddGetter(GetterPair.Key, GetterFunction);
  end;

  for SetterPair in ClassDef.Setters do
  begin
    SetterFunction := TGocciaFunctionValue(EvaluateSetter(SetterPair.Value, Context));
    ClassValue.AddSetter(SetterPair.Key, SetterFunction);
  end;

  // For class expressions, don't automatically bind to scope - just return the class value
  // If it's a named class expression, the name is only visible inside the class methods
  Result := ClassValue;
end;

function EvaluatePrivateMember(PrivateMemberExpression: TGocciaPrivateMemberExpression; Context: TGocciaEvaluationContext): TGocciaValue;
var
  ObjectValue: TGocciaValue;
  Instance: TGocciaInstanceValue;
  ClassValue: TGocciaClassValue;
  AccessClass: TGocciaClassValue;
begin
  // Evaluate the object expression
  ObjectValue := EvaluateExpression(PrivateMemberExpression.ObjectExpr, Context);

  if ObjectValue is TGocciaInstanceValue then
  begin
    // Private field access on instance
    Instance := TGocciaInstanceValue(ObjectValue);

    // Determine the access class - the class that is trying to access the private field
    // This should be the class containing the current method
    AccessClass := Instance.ClassValue; // For now, assume access from the same class

    // Check if this is a private method call
    if Instance.ClassValue.PrivateMethods.ContainsKey(PrivateMemberExpression.PrivateName) then
    begin
      Result := Instance.ClassValue.GetPrivateMethod(PrivateMemberExpression.PrivateName);
      if Result = nil then
        Result := TGocciaUndefinedValue.Create;
    end
    else
    begin
      // It's a private property access
      Result := Instance.GetPrivateProperty(PrivateMemberExpression.PrivateName, AccessClass);
    end;
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
    Result := TGocciaUndefinedValue.Create;
  end;
end;

function EvaluatePrivatePropertyAssignment(PrivatePropertyAssignmentExpression: TGocciaPrivatePropertyAssignmentExpression; Context: TGocciaEvaluationContext): TGocciaValue;
var
  ObjectValue: TGocciaValue;
  Instance: TGocciaInstanceValue;
  ClassValue: TGocciaClassValue;
  AccessClass: TGocciaClassValue;
  Value: TGocciaValue;
begin
  // Evaluate the object expression
  ObjectValue := EvaluateExpression(PrivatePropertyAssignmentExpression.ObjectExpr, Context);

  // Evaluate the value to assign
  Value := EvaluateExpression(PrivatePropertyAssignmentExpression.Value, Context);

  if ObjectValue is TGocciaInstanceValue then
  begin
    // Private field assignment on instance
    Instance := TGocciaInstanceValue(ObjectValue);

    // Determine the access class - the class that is trying to access the private field
    AccessClass := Instance.ClassValue; // For now, assume access from the same class

    // Set the private property
    Instance.SetPrivateProperty(PrivatePropertyAssignmentExpression.PrivateName, Value, AccessClass);
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
    Result := TGocciaUndefinedValue.Create;
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

    // Determine the access class - the class that is trying to access the private field
    AccessClass := Instance.ClassValue; // For now, assume access from the same class

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
    Result := TGocciaUndefinedValue.Create;
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
  PropertyPair: TPair<string, TGocciaExpression>;
  PropertyValue: TGocciaValue;
begin
  // Initialize private instance properties (only from the exact class, not inherited)
  for PropertyPair in ClassValue.PrivateInstancePropertyDefs do
  begin
    Logger.Debug('Initializing private instance property: %s from class: %s', [PropertyPair.Key, ClassValue.Name]);
    // Evaluate the property expression in the current context
    PropertyValue := EvaluateExpression(PropertyPair.Value, Context);
    Instance.SetPrivateProperty(PropertyPair.Key, PropertyValue, ClassValue);
  end;
end;

function EvaluateTemplateLiteral(TemplateLiteralExpression: TGocciaTemplateLiteralExpression; Context: TGocciaEvaluationContext): TGocciaValue;
var
  Template: string;
  ResultStr: string;
  I, Start: Integer;
  BraceCount: Integer;
  ExpressionText: string;
  ExpressionValue: TGocciaValue;
begin
  Template := TemplateLiteralExpression.Value;
  ResultStr := '';
  I := 1;

  while I <= Length(Template) do
  begin
    if (I < Length(Template)) and (Template[I] = '$') and (Template[I + 1] = '{') then
    begin
      // Found interpolation start
      I := I + 2; // Skip ${
      Start := I;

      // Find the matching } (handle nested braces properly)
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
        Result := TGocciaStringValue.Create(Template);
        Exit;
      end;

      // Extract the expression text
      ExpressionText := Trim(Copy(Template, Start, I - Start));

      if ExpressionText = '' then
      begin
        // Empty expression
        ResultStr := ResultStr + '';
      end
      else
      begin
        // Evaluate the full expression using a safe parsing approach
        ExpressionValue := EvaluateTemplateExpression(ExpressionText, Context, TemplateLiteralExpression.Line, TemplateLiteralExpression.Column);
        if ExpressionValue <> nil then
          ResultStr := ResultStr + ExpressionValue.ToString
        else
          ResultStr := ResultStr + 'undefined';
      end;

      Inc(I); // Skip the closing }
    end
    else
    begin
      // Regular character
      ResultStr := ResultStr + Template[I];
      Inc(I);
    end;
  end;

  Result := TGocciaStringValue.Create(ResultStr);
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
    Result := TGocciaStringValue.Create('');
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
      Result := Context.Scope.GetValue(Trimmed);
      if Result = nil then
        Result := TGocciaUndefinedValue.Create;
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
          if (LeftVal is TGocciaStringValue) or (RightVal is TGocciaStringValue) then
            Result := TGocciaStringValue.Create(LeftVal.ToString + RightVal.ToString)
          else
            Result := TGocciaNumberValue.Create(LeftVal.ToNumber + RightVal.ToNumber);
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
          Result := TGocciaNumberValue.Create(LeftVal.ToNumber - RightVal.ToNumber);
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
          Result := TGocciaNumberValue.Create(LeftVal.ToNumber * RightVal.ToNumber);
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
          if RightVal.ToNumber = 0 then
            Result := TGocciaNumberValue.Create(Infinity)
          else
            Result := TGocciaNumberValue.Create(LeftVal.ToNumber / RightVal.ToNumber);
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
      Result := TGocciaUndefinedValue.Create;
      Exit;
    end;

    Parser := TGocciaParser.Create(Tokens, 'template-expression', SourceLines);
    Tokens := nil; // Parser owns it

    Expression := Parser.Expression;
    if Expression = nil then
    begin
      Result := TGocciaUndefinedValue.Create;
      Exit;
    end;

    Result := EvaluateExpression(Expression, Context);

  except
    Result := TGocciaUndefinedValue.Create;
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
  AssignPattern(DestructuringDeclaration.Pattern, Value, Context);

  Result := Value;
end;

procedure AssignPattern(Pattern: TGocciaDestructuringPattern; Value: TGocciaValue; Context: TGocciaEvaluationContext);
begin
  if Pattern is TGocciaIdentifierDestructuringPattern then
    AssignIdentifierPattern(TGocciaIdentifierDestructuringPattern(Pattern), Value, Context)
  else if Pattern is TGocciaArrayDestructuringPattern then
    AssignArrayPattern(TGocciaArrayDestructuringPattern(Pattern), Value, Context)
  else if Pattern is TGocciaObjectDestructuringPattern then
    AssignObjectPattern(TGocciaObjectDestructuringPattern(Pattern), Value, Context)
  else if Pattern is TGocciaAssignmentDestructuringPattern then
    AssignAssignmentPattern(TGocciaAssignmentDestructuringPattern(Pattern), Value, Context)
  else if Pattern is TGocciaRestDestructuringPattern then
    AssignRestPattern(TGocciaRestDestructuringPattern(Pattern), Value, Context);
end;

procedure AssignIdentifierPattern(Pattern: TGocciaIdentifierDestructuringPattern; Value: TGocciaValue; Context: TGocciaEvaluationContext);
begin
  Context.Scope.DefineLexicalBinding(Pattern.Name, Value, dtLet);
end;

procedure AssignArrayPattern(Pattern: TGocciaArrayDestructuringPattern; Value: TGocciaValue; Context: TGocciaEvaluationContext);
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
            RestElements.Elements.Add(TGocciaUndefinedValue.Create);
        end;
        AssignPattern(TGocciaRestDestructuringPattern(Pattern.Elements[I]).Argument, RestElements, Context);
        Break;
      end
      else
      begin
        // Regular element
        if (I < ArrayValue.Elements.Count) and (ArrayValue.Elements[I] <> nil) then
          ElementValue := ArrayValue.Elements[I]
        else
          ElementValue := TGocciaUndefinedValue.Create;

        AssignPattern(Pattern.Elements[I], ElementValue, Context);
      end;
    end;
  end
  else if Value is TGocciaStringValue then
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
        for J := I + 1 to Length(Value.ToString) do
          RestElements.Elements.Add(TGocciaStringValue.Create(Value.ToString[J]));
        AssignPattern(TGocciaRestDestructuringPattern(Pattern.Elements[I]).Argument, RestElements, Context);
        Break;
      end
      else
      begin
        // Regular character
        if I + 1 <= Length(Value.ToString) then
          ElementValue := TGocciaStringValue.Create(Value.ToString[I + 1])
        else
          ElementValue := TGocciaUndefinedValue.Create;

        AssignPattern(Pattern.Elements[I], ElementValue, Context);
      end;
    end;
  end
  else if (Value is TGocciaNullValue) or (Value is TGocciaUndefinedValue) then
  begin
    // Throw TypeError for null/undefined destructuring
    raise TGocciaError.Create('TypeError: Cannot destructure null or undefined', 0, 0, '', nil);
  end
  else
  begin
    // Throw TypeError for non-iterable values
    raise TGocciaError.Create('TypeError: Value is not iterable', 0, 0, '', nil);
  end;
end;

procedure AssignObjectPattern(Pattern: TGocciaObjectDestructuringPattern; Value: TGocciaValue; Context: TGocciaEvaluationContext);
var
  ObjectValue: TGocciaObjectValue;
  Prop: TGocciaDestructuringProperty;
  PropValue: TGocciaValue;
  Key: string;
  RestObject: TGocciaObjectValue;
  UsedKeys: TStringList;
  ObjectPair: TPair<string, TGocciaValue>;
begin
  // Check if value is an object
  if not (Value is TGocciaObjectValue) then
  begin
    if (Value is TGocciaNullValue) or (Value is TGocciaUndefinedValue) then
      raise TGocciaError.Create('TypeError: Cannot destructure null or undefined', 0, 0, '', nil)
    else
      raise TGocciaError.Create('TypeError: Cannot destructure non-object value', 0, 0, '', nil);
  end;

  ObjectValue := TGocciaObjectValue(Value);
  UsedKeys := TStringList.Create;

  try
    for Prop in Pattern.Properties do
    begin
      if Prop.Pattern is TGocciaRestDestructuringPattern then
      begin
        // Rest pattern: collect remaining properties
        RestObject := TGocciaObjectValue.Create;
        for Key in ObjectValue.GetEnumerablePropertyNames do
        begin
          if UsedKeys.IndexOf(Key) = -1 then
            RestObject.AssignProperty(Key, ObjectValue.GetProperty(Key));
        end;
        AssignPattern(TGocciaRestDestructuringPattern(Prop.Pattern).Argument, RestObject, Context);
      end
      else
      begin
        // Regular property
        if Prop.Computed then
        begin
          // Computed property key
          Key := EvaluateExpression(Prop.KeyExpression, Context).ToString;
        end
        else
        begin
          Key := Prop.Key;
        end;

        UsedKeys.Add(Key);
        PropValue := ObjectValue.GetProperty(Key);
        AssignPattern(Prop.Pattern, PropValue, Context);
      end;
    end;
  finally
    UsedKeys.Free;
  end;
end;

procedure AssignAssignmentPattern(Pattern: TGocciaAssignmentDestructuringPattern; Value: TGocciaValue; Context: TGocciaEvaluationContext);
var
  DefaultValue: TGocciaValue;
begin
  // Use default value if the value is undefined
  if Value is TGocciaUndefinedValue then
  begin
    DefaultValue := EvaluateExpression(Pattern.Right, Context);
    AssignPattern(Pattern.Left, DefaultValue, Context);
  end
  else
  begin
    AssignPattern(Pattern.Left, Value, Context);
  end;
end;

procedure AssignRestPattern(Pattern: TGocciaRestDestructuringPattern; Value: TGocciaValue; Context: TGocciaEvaluationContext);
begin
  // Rest patterns are handled by their containing array/object patterns
  AssignPattern(Pattern.Argument, Value, Context);
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

end.
