unit Goccia.Evaluator;

{$I Goccia.inc}

interface

uses
  Goccia.Interfaces, Goccia.Values.Base, Goccia.Token, Goccia.Scope, Goccia.Error, Goccia.Logger, Goccia.Modules, Goccia.Values.NativeFunction, Goccia.Values.UndefinedValue, Goccia.Values.ObjectValue, Goccia.Values.FunctionValue, Goccia.Values.ClassValue, Goccia.Values.ArrayValue, Goccia.Values.BooleanValue, Goccia.Values.NumberValue, Goccia.Values.StringValue, Goccia.Values.Error, Goccia.Values.NullValue, Goccia.AST.Node, Goccia.AST.Expressions, Goccia.AST.Statements, Goccia.Utils, Generics.Collections, SysUtils, Math;

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
function EvaluateMember(MemberExpression: TGocciaMemberExpression; Context: TGocciaEvaluationContext): TGocciaValue;
function EvaluateArray(ArrayExpression: TGocciaArrayExpression; Context: TGocciaEvaluationContext): TGocciaValue;
function EvaluateObject(ObjectExpression: TGocciaObjectExpression; Context: TGocciaEvaluationContext): TGocciaValue;
function EvaluateArrowFunction(ArrowFunctionExpression: TGocciaArrowFunctionExpression; Context: TGocciaEvaluationContext): TGocciaValue;
function EvaluateBlock(BlockStatement: TGocciaBlockStatement; Context: TGocciaEvaluationContext): TGocciaValue;
function EvaluateIf(IfStatement: TGocciaIfStatement; Context: TGocciaEvaluationContext): TGocciaValue;
function EvaluateClassMethod(ClassMethod: TGocciaClassMethod; Context: TGocciaEvaluationContext; SuperClass: TGocciaValue = nil): TGocciaValue;
function EvaluateClass(ClassDeclaration: TGocciaClassDeclaration; Context: TGocciaEvaluationContext): TGocciaValue;
function EvaluateClassExpression(ClassExpression: TGocciaClassExpression; Context: TGocciaEvaluationContext): TGocciaValue;
function EvaluateClassDefinition(ClassDef: TGocciaClassDefinition; Context: TGocciaEvaluationContext; Line, Column: Integer): TGocciaClassValue;
function EvaluateNewExpression(NewExpression: TGocciaNewExpression; Context: TGocciaEvaluationContext): TGocciaValue;
function EvaluatePrivateMember(PrivateMemberExpression: TGocciaPrivateMemberExpression; Context: TGocciaEvaluationContext): TGocciaValue;
function EvaluatePrivatePropertyAssignment(PrivatePropertyAssignmentExpression: TGocciaPrivatePropertyAssignmentExpression; Context: TGocciaEvaluationContext): TGocciaValue;
function EvaluatePrivatePropertyCompoundAssignment(PrivatePropertyCompoundAssignmentExpression: TGocciaPrivatePropertyCompoundAssignmentExpression; Context: TGocciaEvaluationContext): TGocciaValue;
procedure InitializeInstanceProperties(Instance: TGocciaInstanceValue; ClassValue: TGocciaClassValue; Context: TGocciaEvaluationContext);
procedure InitializePrivateInstanceProperties(Instance: TGocciaInstanceValue; ClassValue: TGocciaClassValue; Context: TGocciaEvaluationContext);

implementation

function Evaluate(Node: TGocciaASTNode; Context: TGocciaEvaluationContext): TGocciaValue;
begin
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
begin
  if Expression is TGocciaLiteralExpression then
    Result := TGocciaLiteralExpression(Expression).Value
  else if Expression is TGocciaIdentifierExpression then
  begin
    Result := Context.Scope.GetValue(TGocciaIdentifierExpression(Expression).Name);
    if Result = nil then
      Context.OnError(Format('Undefined variable: %s',
        [TGocciaIdentifierExpression(Expression).Name]), Expression.Line, Expression.Column);
  end
  else if Expression is TGocciaBinaryExpression then
    Result := EvaluateBinary(TGocciaBinaryExpression(Expression), Context)
  else if Expression is TGocciaUnaryExpression then
    Result := EvaluateUnary(TGocciaUnaryExpression(Expression), Context)
  else if Expression is TGocciaAssignmentExpression then
  begin
    // Variable assignment
    Result := EvaluateExpression(TGocciaAssignmentExpression(Expression).Value, Context);
    Context.Scope.Assign(TGocciaAssignmentExpression(Expression).Name, Result);
  end
  else if Expression is TGocciaPropertyAssignmentExpression then
  begin
    // Property assignment
    Obj := EvaluateExpression(TGocciaPropertyAssignmentExpression(Expression).ObjectExpr, Context);
    Value := EvaluateExpression(TGocciaPropertyAssignmentExpression(Expression).Value, Context);

    // Handle different object types for property assignment
    if (Obj is TGocciaInstanceValue) then
    begin
      TGocciaInstanceValue(Obj).SetProperty(TGocciaPropertyAssignmentExpression(Expression).PropertyName, Value);
      Result := Value;
    end
    else if (Obj is TGocciaObjectValue) then
    begin
      TGocciaObjectValue(Obj).SetProperty(TGocciaPropertyAssignmentExpression(Expression).PropertyName, Value);
      Result := Value;
    end
    else if (Obj is TGocciaClassValue) then
    begin
      // Handle static property assignment
      TGocciaClassValue(Obj).SetProperty(TGocciaPropertyAssignmentExpression(Expression).PropertyName, Value);
      Result := Value;
    end
    else
      Context.OnError('Cannot set property on non-object', Expression.Line, Expression.Column);
  end
  else if Expression is TGocciaCompoundAssignmentExpression then
  begin
    // Variable compound assignment (e.g., count += 5)
    Result := Context.Scope.GetValue(TGocciaCompoundAssignmentExpression(Expression).Name);
    if Result = nil then
      Context.OnError(Format('Undefined variable: %s',
        [TGocciaCompoundAssignmentExpression(Expression).Name]), Expression.Line, Expression.Column);

    Value := EvaluateExpression(TGocciaCompoundAssignmentExpression(Expression).Value, Context);

    case TGocciaCompoundAssignmentExpression(Expression).Operator of
      gttPlusAssign:
        begin
          if (Result is TGocciaStringValue) or (Value is TGocciaStringValue) then
            Result := TGocciaStringValue.Create(Result.ToString + Value.ToString)
          else
            Result := TGocciaNumberValue.Create(Result.ToNumber + Value.ToNumber);
        end;
      gttMinusAssign:
        Result := TGocciaNumberValue.Create(Result.ToNumber - Value.ToNumber);
      gttStarAssign:
        Result := TGocciaNumberValue.Create(Result.ToNumber * Value.ToNumber);
      gttSlashAssign:
        begin
          if Value.ToNumber = 0 then
            Result := TGocciaNumberValue.Create(Infinity)
          else
            Result := TGocciaNumberValue.Create(Result.ToNumber / Value.ToNumber);
        end;
      gttPercentAssign:
        Result := TGocciaNumberValue.Create(
          Trunc(Result.ToNumber) mod Trunc(Value.ToNumber));
      gttPowerAssign:
        Result := TGocciaNumberValue.Create(Power(Result.ToNumber, Value.ToNumber));
    end;

    Context.Scope.Assign(TGocciaCompoundAssignmentExpression(Expression).Name, Result);
  end
  else if Expression is TGocciaPropertyCompoundAssignmentExpression then
  begin
    // Property compound assignment (e.g., obj.count += 5)
    Obj := EvaluateExpression(TGocciaPropertyCompoundAssignmentExpression(Expression).ObjectExpr, Context);
    Value := EvaluateExpression(TGocciaPropertyCompoundAssignmentExpression(Expression).Value, Context);
    PropName := TGocciaPropertyCompoundAssignmentExpression(Expression).PropertyName;

    // Get current property value
    if (Obj is TGocciaInstanceValue) then
      Result := TGocciaInstanceValue(Obj).GetProperty(PropName)
    else if (Obj is TGocciaObjectValue) then
      Result := TGocciaObjectValue(Obj).GetProperty(PropName)
    else if (Obj is TGocciaClassValue) then
      Result := TGocciaClassValue(Obj).GetProperty(PropName)
    else
    begin
      Context.OnError('Cannot access property on non-object', Expression.Line, Expression.Column);
      Result := TGocciaUndefinedValue.Create;
      Exit;
    end;

    // Perform compound operation
    case TGocciaPropertyCompoundAssignmentExpression(Expression).Operator of
      gttPlusAssign:
        begin
          if (Result is TGocciaStringValue) or (Value is TGocciaStringValue) then
            Result := TGocciaStringValue.Create(Result.ToString + Value.ToString)
          else
            Result := TGocciaNumberValue.Create(Result.ToNumber + Value.ToNumber);
        end;
      gttMinusAssign:
        Result := TGocciaNumberValue.Create(Result.ToNumber - Value.ToNumber);
      gttStarAssign:
        Result := TGocciaNumberValue.Create(Result.ToNumber * Value.ToNumber);
      gttSlashAssign:
        begin
          if Value.ToNumber = 0 then
            Result := TGocciaNumberValue.Create(Infinity)
          else
            Result := TGocciaNumberValue.Create(Result.ToNumber / Value.ToNumber);
        end;
      gttPercentAssign:
        Result := TGocciaNumberValue.Create(
          Trunc(Result.ToNumber) mod Trunc(Value.ToNumber));
      gttPowerAssign:
        Result := TGocciaNumberValue.Create(Power(Result.ToNumber, Value.ToNumber));
    end;

    // Set the new value
    if (Obj is TGocciaInstanceValue) then
      TGocciaInstanceValue(Obj).SetProperty(PropName, Result)
    else if (Obj is TGocciaObjectValue) then
      TGocciaObjectValue(Obj).SetProperty(PropName, Result)
    else if (Obj is TGocciaClassValue) then
      TGocciaClassValue(Obj).SetProperty(PropName, Result);
  end
  else if Expression is TGocciaIncrementExpression then
  begin
    // Increment/decrement expressions (++x, --x, x++, x--)
    if TGocciaIncrementExpression(Expression).Operand is TGocciaIdentifierExpression then
    begin
            // Variable increment/decrement
      PropName := TGocciaIdentifierExpression(TGocciaIncrementExpression(Expression).Operand).Name;
      Result := Context.Scope.GetValue(PropName);

      // Calculate new value
      if TGocciaIncrementExpression(Expression).Operator = gttIncrement then
        Value := TGocciaNumberValue.Create(Result.ToNumber + 1)
      else
        Value := TGocciaNumberValue.Create(Result.ToNumber - 1);

      // Set the new value
      Context.Scope.SetValue(PropName, Value);

      // Return value depends on prefix/postfix
      if TGocciaIncrementExpression(Expression).IsPrefix then
        Result := Value  // Prefix: return new value (++x)
      else
        Result := Result;  // Postfix: return old value (x++)
    end
    else if TGocciaIncrementExpression(Expression).Operand is TGocciaMemberExpression then
    begin
      // Property increment/decrement
      Obj := EvaluateExpression(TGocciaMemberExpression(TGocciaIncrementExpression(Expression).Operand).ObjectExpr, Context);
      PropName := TGocciaMemberExpression(TGocciaIncrementExpression(Expression).Operand).PropertyName;

      // Get current property value
      if (Obj is TGocciaInstanceValue) then
        Result := TGocciaInstanceValue(Obj).GetProperty(PropName)
      else if (Obj is TGocciaObjectValue) then
        Result := TGocciaObjectValue(Obj).GetProperty(PropName)
      else if (Obj is TGocciaClassValue) then
        Result := TGocciaClassValue(Obj).GetProperty(PropName)
      else
      begin
        Context.OnError('Cannot access property on non-object', Expression.Line, Expression.Column);
        Result := TGocciaUndefinedValue.Create;
        Exit;
      end;

      // Calculate new value
      if TGocciaIncrementExpression(Expression).Operator = gttIncrement then
        Value := TGocciaNumberValue.Create(Result.ToNumber + 1)
      else
        Value := TGocciaNumberValue.Create(Result.ToNumber - 1);

      // Set the new value
      if (Obj is TGocciaInstanceValue) then
        TGocciaInstanceValue(Obj).SetProperty(PropName, Value)
      else if (Obj is TGocciaObjectValue) then
        TGocciaObjectValue(Obj).SetProperty(PropName, Value)
      else if (Obj is TGocciaClassValue) then
        TGocciaClassValue(Obj).SetProperty(PropName, Value);

      // Return value depends on prefix/postfix
      if TGocciaIncrementExpression(Expression).IsPrefix then
        Result := Value  // Prefix: return new value (++obj.prop)
      else
        Result := Result;  // Postfix: return old value (obj.prop++)
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
    Callee := EvaluateExpression(TGocciaNewExpression(Expression).Callee, Context);
    Arguments := TObjectList<TGocciaValue>.Create(False);
    try
      for I := 0 to TGocciaNewExpression(Expression).Arguments.Count - 1 do
        Arguments.Add(EvaluateExpression(TGocciaNewExpression(Expression).Arguments[I], Context));

            if Callee is TGocciaClassValue then
      begin
        // Create instance first
        Result := TGocciaInstanceValue.Create(TGocciaClassValue(Callee));
        TGocciaInstanceValue(Result).Prototype := TGocciaClassValue(Callee).Prototype;

        // Initialize instance properties BEFORE calling constructor
        InitializeInstanceProperties(TGocciaInstanceValue(Result), TGocciaClassValue(Callee), Context);

        // Now call constructor with the instance that has properties initialized
        if Assigned(TGocciaClassValue(Callee).ConstructorMethod) then
          TGocciaClassValue(Callee).ConstructorMethod.Call(Arguments, Result)
        else if Assigned(TGocciaClassValue(Callee).SuperClass) and Assigned(TGocciaClassValue(Callee).SuperClass.ConstructorMethod) then
          TGocciaClassValue(Callee).SuperClass.ConstructorMethod.Call(Arguments, Result);
      end
      else
        Context.OnError(Format('Can only instantiate classes, not %s', [Callee.TypeName]),
          Expression.Line, Expression.Column);
    finally
      Arguments.Free;
    end;
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
    Logger.Debug('EvaluateStatement: Variable name: %s', [Decl.Name]);
    Value := EvaluateExpression(Decl.Initializer, Context);
    Logger.Debug('EvaluateStatement: Initializer result type: %s', [Value.ClassName]);
    Context.Scope.SetValue(Decl.Name, Value);
    Logger.Debug('EvaluateStatement: Variable defined in scope');
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
    Result := TGocciaUndefinedValue.Create; // Placeholder
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
        Context.Scope.SetValue(ImportPair.Key, Value);
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
  LeftNum, RightNum: Double;
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
  end;

  // For all other operators, evaluate both operands
  Left := EvaluateExpression(BinaryExpression.Left, Context);
  Right := EvaluateExpression(BinaryExpression.Right, Context);

  case BinaryExpression.Operator of
    gttPlus:
      begin
        if (Left is TGocciaStringValue) or (Right is TGocciaStringValue) then
          Result := TGocciaStringValue.Create(Left.ToString + Right.ToString)
        else if (Left is TGocciaNumberValue) and (Right is TGocciaNumberValue) then
          Result := TGocciaNumberValue.Create(
            TGocciaNumberValue(Left).Value + TGocciaNumberValue(Right).Value)
        else if (Left is TGocciaNumberValue) or (Right is TGocciaNumberValue) then
          Result := TGocciaNumberValue.Create(Left.ToNumber + Right.ToNumber)
        else
          Result := TGocciaStringValue.Create(Left.ToString + Right.ToString);
      end;
    gttMinus:
      Result := TGocciaNumberValue.Create(Left.ToNumber - Right.ToNumber);
    gttStar:
      Result := TGocciaNumberValue.Create(Left.ToNumber * Right.ToNumber);
    gttSlash:
      begin
        RightNum := Right.ToNumber;
        if RightNum = 0 then
          Result := TGocciaNumberValue.Create(Infinity)
        else
          Result := TGocciaNumberValue.Create(Left.ToNumber / RightNum);
      end;
    gttPercent:
      Result := TGocciaNumberValue.Create(
        Trunc(Left.ToNumber) mod Trunc(Right.ToNumber));
    gttPower:
      Result := TGocciaNumberValue.Create(Power(Left.ToNumber, Right.ToNumber));
    gttEqual:
      Result := TGocciaBooleanValue.Create(IsEqual(Left, Right));
    gttNotEqual:
      Result := TGocciaBooleanValue.Create(not IsEqual(Left, Right));
    gttLess:
      Result := TGocciaBooleanValue.Create(Left.ToNumber < Right.ToNumber);
    gttGreater:
      Result := TGocciaBooleanValue.Create(Left.ToNumber > Right.ToNumber);
    gttLessEqual:
      Result := TGocciaBooleanValue.Create(Left.ToNumber <= Right.ToNumber);
    gttGreaterEqual:
      Result := TGocciaBooleanValue.Create(Left.ToNumber >= Right.ToNumber);
    gttInstanceof:
      begin
        // Implement instanceof operator according to JavaScript specification
        Logger.Debug('EvaluateBinary: instanceof operator called with Left: %s, Right: %s', [Left.ToString, Right.ToString]);

        // Right operand must be a constructor/class
        if not (Right is TGocciaClassValue) then
        begin
          // For built-in types, we need special handling
          Result := TGocciaBooleanValue.Create(False); // Default to false for now
        end
        else
        begin
          // Check if Left is an instance of the Right class
          if Left is TGocciaInstanceValue then
            Result := TGocciaBooleanValue.Create(TGocciaInstanceValue(Left).IsInstanceOf(TGocciaClassValue(Right)))
          else if (Left is TGocciaArrayValue) and (TGocciaClassValue(Right).Name = 'Array') then
            Result := TGocciaBooleanValue.Create(True)
          else if (Left is TGocciaObjectValue) and (TGocciaClassValue(Right).Name = 'Object') then
            Result := TGocciaBooleanValue.Create(True)
          else
            Result := TGocciaBooleanValue.Create(False);
        end;
      end;
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
    gttTypeof:
      begin
        Logger.Debug('EvaluateUnary: typeof operator called with operand: %s', [Operand.ToString]);
        Result := TGocciaStringValue.Create(Operand.TypeName);
      end;
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

  Callee := EvaluateExpression(CallExpression.Callee, Context);

  if CallExpression.Callee is TGocciaMemberExpression then
  begin
    MemberExpr := TGocciaMemberExpression(CallExpression.Callee);
    // Special handling for super.method() calls
    if MemberExpr.ObjectExpr is TGocciaSuperExpression then
      ThisValue := Context.Scope.ThisValue  // Use current instance's 'this'
    else
      ThisValue := EvaluateExpression(MemberExpr.ObjectExpr, Context);
  end
  else
    ThisValue := TGocciaUndefinedValue.Create;

  Arguments := TObjectList<TGocciaValue>.Create(False);
  try
    for ArgumentExpr in CallExpression.Arguments do
      Arguments.Add(EvaluateExpression(ArgumentExpr, Context));

    if Callee is TGocciaNativeFunctionValue then
      Result := TGocciaNativeFunctionValue(Callee).Call(Arguments, ThisValue)
    else if Callee is TGocciaFunctionValue then
      Result := TGocciaFunctionValue(Callee).Call(Arguments, ThisValue)
    else if Callee is IGocciaCallable then
      Result := (Callee as IGocciaCallable).Call(Arguments, ThisValue)
    else
      Result := TGocciaUndefinedValue.Create;

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
    Logger.Debug('EvaluateMember: Obj is not a supported object type');
    Result := TGocciaUndefinedValue.Create;
  end;
  Logger.Debug('EvaluateMember: Returning result type: %s', [Result.ClassName]);
  Logger.Debug('EvaluateMember: Result ToString: %s', [Result.ToString]);
end;

function EvaluateArray(ArrayExpression: TGocciaArrayExpression; Context: TGocciaEvaluationContext): TGocciaValue;
var
  Arr: TGocciaArrayValue;
  I: Integer;
begin
  Arr := TGocciaArrayValue.Create;
  for I := 0 to ArrayExpression.Elements.Count - 1 do
    Arr.Elements.Add(EvaluateExpression(ArrayExpression.Elements[I], Context));
  Result := Arr;
end;

function EvaluateObject(ObjectExpression: TGocciaObjectExpression; Context: TGocciaEvaluationContext): TGocciaValue;
var
  Obj: TGocciaObjectValue;
  Pair: TPair<string, TGocciaExpression>;
begin
  Obj := TGocciaObjectValue.Create;
  for Pair in ObjectExpression.Properties do
    Obj.SetProperty(Pair.Key, EvaluateExpression(Pair.Value, Context));
  Result := Obj;
end;

function EvaluateArrowFunction(ArrowFunctionExpression: TGocciaArrowFunctionExpression; Context: TGocciaEvaluationContext): TGocciaValue;
var
  BlockValue: TGocciaBlockValue;
  Statements: TObjectList<TGocciaASTNode>;
begin
  // Arrow function body can be either a block statement or a single expression
  if ArrowFunctionExpression.Body is TGocciaBlockStatement then
  begin
    // Body is a block statement: (n) => { return n * 2; }
    BlockValue := TGocciaBlockValue.Create(TGocciaBlockStatement(ArrowFunctionExpression.Body).Nodes, Context.Scope.CreateChild);
  end
  else
  begin
    // Body is a single expression: (n) => n * 2
    // Wrap the expression in a statement list
    Statements := TObjectList<TGocciaASTNode>.Create(True);
    Statements.Add(ArrowFunctionExpression.Body); // Add the expression directly
    BlockValue := TGocciaBlockValue.Create(Statements, Context.Scope.CreateChild);
  end;

  // Create function with closure scope
  Result := TGocciaFunctionValue.Create(ArrowFunctionExpression.Parameters, BlockValue, Context.Scope.CreateChild);
end;

function EvaluateBlock(BlockStatement: TGocciaBlockStatement; Context: TGocciaEvaluationContext): TGocciaValue;
var
  BlockValue: TGocciaBlockValue;
begin
  // Block does have a lexical scope, not a function scope so we don't need to push a new scope
  try
    Result := TGocciaUndefinedValue.Create;

    BlockValue := TGocciaBlockValue.Create(BlockStatement.Nodes, Context.Scope.CreateChild);

    Result := BlockValue.Execute(Context.Scope);
  finally
    BlockValue.Free;
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

function EvaluateClassMethod(ClassMethod: TGocciaClassMethod; Context: TGocciaEvaluationContext; SuperClass: TGocciaValue = nil): TGocciaValue;
var
  BlockValue: TGocciaBlockValue;
begin
  BlockValue := TGocciaBlockValue.Create(TGocciaBlockStatement(ClassMethod.Body).Nodes, Context.Scope.CreateChild);
  // Always create a unique child scope for the closure
  Result := TGocciaMethodValue.Create(ClassMethod.Parameters, BlockValue, Context.Scope.CreateChild, ClassMethod.Name, SuperClass);
end;

function EvaluateClass(ClassDeclaration: TGocciaClassDeclaration; Context: TGocciaEvaluationContext): TGocciaValue;
var
  ClassDef: TGocciaClassDefinition;
begin
  ClassDef := ClassDeclaration.ClassDefinition;
  Result := EvaluateClassDefinition(ClassDef, Context, ClassDeclaration.Line, ClassDeclaration.Column);

  // For class declarations, bind the class name to the scope
  Context.Scope.SetValue(ClassDef.Name, Result);
end;

procedure InitializeInstanceProperties(Instance: TGocciaInstanceValue; ClassValue: TGocciaClassValue; Context: TGocciaEvaluationContext);
var
  PropertyPair: TPair<string, TGocciaExpression>;
  PropertyValue: TGocciaValue;
  CurrentClass: TGocciaClassValue;
  ClassChain: TList<TGocciaClassValue>;
  I: Integer;
begin
  // Initialize instance properties (including inherited ones)
  // Walk up the inheritance chain to collect all classes, then process from parent to child
  // This ensures child properties can shadow parent properties
  ClassChain := TList<TGocciaClassValue>.Create;
  try
    // Collect the inheritance chain
    CurrentClass := ClassValue;
    while Assigned(CurrentClass) do
    begin
      ClassChain.Add(CurrentClass);
      CurrentClass := CurrentClass.SuperClass;
    end;

    // Process from parent to child (reverse order)
    for I := ClassChain.Count - 1 downto 0 do
    begin
      CurrentClass := ClassChain[I];
      for PropertyPair in CurrentClass.InstancePropertyDefs do
      begin
        Logger.Debug('Initializing instance property: %s from class: %s', [PropertyPair.Key, CurrentClass.Name]);
        // Evaluate the property expression in the current context
        PropertyValue := EvaluateExpression(PropertyPair.Value, Context);
        Instance.SetProperty(PropertyPair.Key, PropertyValue);
      end;
    end;
  finally
    ClassChain.Free;
  end;
end;

function EvaluateNewExpression(NewExpression: TGocciaNewExpression; Context: TGocciaEvaluationContext): TGocciaValue;
var
  Callee: TGocciaValue;
  Arguments: TObjectList<TGocciaValue>;
  I: Integer;
begin
  // Evaluate the callee (class)
  Callee := EvaluateExpression(NewExpression.Callee, Context);

  // Evaluate arguments
  Arguments := TObjectList<TGocciaValue>.Create(False);
  try
    for I := 0 to NewExpression.Arguments.Count - 1 do
      Arguments.Add(EvaluateExpression(NewExpression.Arguments[I], Context));

        // Create new instance
    if Callee is TGocciaClassValue then
    begin
      // Create instance first
      Result := TGocciaInstanceValue.Create(TGocciaClassValue(Callee));
      TGocciaInstanceValue(Result).Prototype := TGocciaClassValue(Callee).Prototype;

      // Initialize instance properties BEFORE calling constructor
      InitializeInstanceProperties(TGocciaInstanceValue(Result), TGocciaClassValue(Callee), Context);

      // Initialize private instance properties
      InitializePrivateInstanceProperties(TGocciaInstanceValue(Result), TGocciaClassValue(Callee), Context);

      // Now call constructor with the instance that has properties initialized
      if Assigned(TGocciaClassValue(Callee).ConstructorMethod) then
        TGocciaClassValue(Callee).ConstructorMethod.Call(Arguments, Result)
      else if Assigned(TGocciaClassValue(Callee).SuperClass) and Assigned(TGocciaClassValue(Callee).SuperClass.ConstructorMethod) then
        TGocciaClassValue(Callee).SuperClass.ConstructorMethod.Call(Arguments, Result);
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
  Method: TGocciaMethodValue;
  PropertyValue: TGocciaValue;
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

  // For class expressions, don't automatically bind to scope - just return the class value
  // If it's a named class expression, the name is only visible inside the class methods
  Result := ClassValue;
end;

function EvaluatePrivateMember(PrivateMemberExpression: TGocciaPrivateMemberExpression; Context: TGocciaEvaluationContext): TGocciaValue;
var
  ObjectValue: TGocciaValue;
  Instance: TGocciaInstanceValue;
  AccessClass: TGocciaClassValue;
begin
  // Evaluate the object expression
  ObjectValue := EvaluateExpression(PrivateMemberExpression.ObjectExpr, Context);

  if not (ObjectValue is TGocciaInstanceValue) then
  begin
    Context.OnError(Format('Private fields can only be accessed on class instances, not %s', [ObjectValue.TypeName]),
      PrivateMemberExpression.Line, PrivateMemberExpression.Column);
    Result := TGocciaUndefinedValue.Create;
    Exit;
  end;

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
end;

function EvaluatePrivatePropertyAssignment(PrivatePropertyAssignmentExpression: TGocciaPrivatePropertyAssignmentExpression; Context: TGocciaEvaluationContext): TGocciaValue;
var
  ObjectValue: TGocciaValue;
  Instance: TGocciaInstanceValue;
  AccessClass: TGocciaClassValue;
  Value: TGocciaValue;
begin
  // Evaluate the object expression
  ObjectValue := EvaluateExpression(PrivatePropertyAssignmentExpression.ObjectExpr, Context);

  if not (ObjectValue is TGocciaInstanceValue) then
  begin
    Context.OnError(Format('Private fields can only be assigned on class instances, not %s', [ObjectValue.TypeName]),
      PrivatePropertyAssignmentExpression.Line, PrivatePropertyAssignmentExpression.Column);
    Result := TGocciaUndefinedValue.Create;
    Exit;
  end;

  Instance := TGocciaInstanceValue(ObjectValue);

  // Determine the access class - the class that is trying to access the private field
  AccessClass := Instance.ClassValue; // For now, assume access from the same class

  // Evaluate the value to assign
  Value := EvaluateExpression(PrivatePropertyAssignmentExpression.Value, Context);

  // Set the private property
  Instance.SetPrivateProperty(PrivatePropertyAssignmentExpression.PrivateName, Value, AccessClass);

  Result := Value;
end;

function EvaluatePrivatePropertyCompoundAssignment(PrivatePropertyCompoundAssignmentExpression: TGocciaPrivatePropertyCompoundAssignmentExpression; Context: TGocciaEvaluationContext): TGocciaValue;
var
  ObjectValue: TGocciaValue;
  Instance: TGocciaInstanceValue;
  AccessClass: TGocciaClassValue;
  Value: TGocciaValue;
  CurrentValue: TGocciaValue;
begin
  // Evaluate the object expression
  ObjectValue := EvaluateExpression(PrivatePropertyCompoundAssignmentExpression.ObjectExpr, Context);

  if not (ObjectValue is TGocciaInstanceValue) then
  begin
    Context.OnError(Format('Private fields can only be accessed on class instances, not %s', [ObjectValue.TypeName]),
      PrivatePropertyCompoundAssignmentExpression.Line, PrivatePropertyCompoundAssignmentExpression.Column);
    Result := TGocciaUndefinedValue.Create;
    Exit;
  end;

  Instance := TGocciaInstanceValue(ObjectValue);

  // Determine the access class - the class that is trying to access the private field
  AccessClass := Instance.ClassValue; // For now, assume access from the same class

  // Get the current value of the private property
  CurrentValue := Instance.GetPrivateProperty(PrivatePropertyCompoundAssignmentExpression.PrivateName, AccessClass);

  // Evaluate the value to operate with
  Value := EvaluateExpression(PrivatePropertyCompoundAssignmentExpression.Value, Context);

  // Perform the compound operation
  case PrivatePropertyCompoundAssignmentExpression.Operator of
    gttPlusAssign:
      begin
        if (CurrentValue is TGocciaStringValue) or (Value is TGocciaStringValue) then
          Result := TGocciaStringValue.Create(CurrentValue.ToString + Value.ToString)
        else
          Result := TGocciaNumberValue.Create(CurrentValue.ToNumber + Value.ToNumber);
      end;
    gttMinusAssign:
      Result := TGocciaNumberValue.Create(CurrentValue.ToNumber - Value.ToNumber);
    gttStarAssign:
      Result := TGocciaNumberValue.Create(CurrentValue.ToNumber * Value.ToNumber);
    gttSlashAssign:
      begin
        if Value.ToNumber = 0 then
          Result := TGocciaNumberValue.Create(Infinity)
        else
          Result := TGocciaNumberValue.Create(CurrentValue.ToNumber / Value.ToNumber);
      end;
    gttPercentAssign:
      Result := TGocciaNumberValue.Create(
        Trunc(CurrentValue.ToNumber) mod Trunc(Value.ToNumber));
    gttPowerAssign:
      Result := TGocciaNumberValue.Create(Power(CurrentValue.ToNumber, Value.ToNumber));
  else
    Result := TGocciaUndefinedValue.Create;
  end;

  // Set the new value
  Instance.SetPrivateProperty(PrivatePropertyCompoundAssignmentExpression.PrivateName, Result, AccessClass);
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

end.
