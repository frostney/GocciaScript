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
function EvaluateClassMethod(ClassMethod: TGocciaClassMethod; Context: TGocciaEvaluationContext): TGocciaValue;
function EvaluateClass(ClassDeclaration: TGocciaClassDeclaration; Context: TGocciaEvaluationContext): TGocciaValue;
function EvaluateNewExpression(NewExpression: TGocciaNewExpression; Context: TGocciaEvaluationContext): TGocciaValue;

implementation

function Evaluate(Node: TGocciaASTNode; Context: TGocciaEvaluationContext): TGocciaValue;
begin
  TGocciaLogger.Debug('Evaluate: Node class is %s', [Node.ClassName]);
  TGocciaLogger.Debug('Evaluate: Node line: %d', [Node.Line]);
  TGocciaLogger.Debug('Evaluate: Node column: %d', [Node.Column]);

  if Node is TGocciaExpression then
  begin
    TGocciaLogger.Debug('Evaluate: Processing as Expression');
    Result := EvaluateExpression(TGocciaExpression(Node), Context);
    TGocciaLogger.Debug('Evaluate: Expression result type: %s', [Result.ClassName]);
  end
  else if Node is TGocciaStatement then
  begin
    TGocciaLogger.Debug('Evaluate: Processing as Statement');
    Result := EvaluateStatement(TGocciaStatement(Node), Context);
    TGocciaLogger.Debug('Evaluate: Statement result type: %s', [Result.ClassName]);
  end
  else
  begin
    TGocciaLogger.Debug('Evaluate: Unknown node type, returning undefined');
    Result := TGocciaUndefinedValue.Create;
  end;
  TGocciaLogger.Debug('Evaluate: Final result type: %s', [Result.ClassName]);
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

    // Special handling for 'this' property assignment
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
    else
      Context.OnError('Cannot set property on non-object', Expression.Line, Expression.Column);
  end
  else if Expression is TGocciaCallExpression then
  begin
    TGocciaLogger.Debug('EvaluateExpression: TGocciaCallExpression - before EvaluateCall');
    Result := EvaluateCall(TGocciaCallExpression(Expression), Context);
    TGocciaLogger.Debug('EvaluateExpression: TGocciaCallExpression - after EvaluateCall, result type: %s', [Result.ClassName]);
    TGocciaLogger.Debug('EvaluateExpression: TGocciaCallExpression - result ToString: %s', [Result.ToString]);
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
    Arguments := TObjectList<TGocciaValue>.Create(True);
    try
      for I := 0 to TGocciaNewExpression(Expression).Arguments.Count - 1 do
        Arguments.Add(EvaluateExpression(TGocciaNewExpression(Expression).Arguments[I], Context));

      if Callee is TGocciaClassValue then
      begin
        Result := TGocciaClassValue(Callee).Instantiate(Arguments);
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
    // Handle this expression
    Result := Context.Scope.ThisValue;
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
  TGocciaLogger.Debug('EvaluateExpression: Returning result type: %s .ToString: %s', [Result.ClassName, Result.ToString]);
end;

function EvaluateStatement(Statement: TGocciaStatement; Context: TGocciaEvaluationContext): TGocciaValue;
var
  Decl: TGocciaVariableDeclaration;
  ImportDecl: TGocciaImportDeclaration;
  Module: TGocciaModule;
  ImportPair: TPair<string, string>;
  Value: TGocciaValue;
begin
  TGocciaLogger.Debug('EvaluateStatement: Statement class is %s', [Statement.ClassName]);
  TGocciaLogger.Debug('EvaluateStatement: Statement line: %d', [Statement.Line]);
  TGocciaLogger.Debug('EvaluateStatement: Statement column: %d', [Statement.Column]);

  Result := TGocciaUndefinedValue.Create;

  if Statement is TGocciaExpressionStatement then
  begin
    TGocciaLogger.Debug('EvaluateStatement: Processing ExpressionStatement');
    Result := EvaluateExpression(TGocciaExpressionStatement(Statement).Expression, Context);
    TGocciaLogger.Debug('EvaluateStatement: ExpressionStatement result type: %s', [Result.ClassName]);
  end
  else if Statement is TGocciaVariableDeclaration then
  begin
    TGocciaLogger.Debug('EvaluateStatement: Processing VariableDeclaration');
    Decl := TGocciaVariableDeclaration(Statement);
    TGocciaLogger.Debug('EvaluateStatement: Variable name: %s', [Decl.Name]);
    Value := EvaluateExpression(Decl.Initializer, Context);
    TGocciaLogger.Debug('EvaluateStatement: Initializer result type: %s', [Value.ClassName]);
    Context.Scope.SetValue(Decl.Name, Value);
    TGocciaLogger.Debug('EvaluateStatement: Variable defined in scope');
  end
  else if Statement is TGocciaBlockStatement then
  begin
    TGocciaLogger.Debug('EvaluateStatement: Processing BlockStatement');
    Result := EvaluateBlock(TGocciaBlockStatement(Statement), Context);
    TGocciaLogger.Debug('EvaluateStatement: BlockStatement result type: %s', [Result.ClassName]);
  end
  else if Statement is TGocciaIfStatement then
  begin
    TGocciaLogger.Debug('EvaluateStatement: Processing IfStatement');
    Result := EvaluateIf(TGocciaIfStatement(Statement), Context);
    TGocciaLogger.Debug('EvaluateStatement: IfStatement result type: %s', [Result.ClassName]);
  end
  else if Statement is TGocciaReturnStatement then
  begin
    TGocciaLogger.Debug('EvaluateStatement: Handling TGocciaReturnStatement');
    if Assigned(TGocciaReturnStatement(Statement).Value) then
    begin
      TGocciaLogger.Debug('EvaluateStatement: Evaluating return value');
      Value := EvaluateExpression(TGocciaReturnStatement(Statement).Value, Context);
      TGocciaLogger.Debug('EvaluateStatement: Return value type: %s', [Value.ClassName]);
      TGocciaLogger.Debug('EvaluateStatement: Return value ToString: %s', [Value.ToString]);
    end
    else
    begin
      TGocciaLogger.Debug('EvaluateStatement: No return value, using undefined');
      Value := TGocciaUndefinedValue.Create;
    end;
    TGocciaLogger.Debug('EvaluateStatement: Raising TGocciaReturnValue with value type: %s', [Value.ClassName]);
    raise TGocciaReturnValue.Create(Value);
  end
  else if Statement is TGocciaThrowStatement then
  begin
    TGocciaLogger.Debug('EvaluateStatement: Processing ThrowStatement');
    Value := EvaluateExpression(TGocciaThrowStatement(Statement).Value, Context);
    TGocciaLogger.Debug('EvaluateStatement: Throw value type: %s', [Value.ClassName]);
    TGocciaLogger.Debug('EvaluateStatement: Throw value ToString: %s', [Value.ToString]);
    raise TGocciaThrowValue.Create(Value);
  end
  else if Statement is TGocciaTryStatement then
  begin
    TGocciaLogger.Debug('EvaluateStatement: Processing TryStatement');
    Result := TGocciaUndefinedValue.Create; // Placeholder
  end
  else if Statement is TGocciaClassDeclaration then
  begin
    TGocciaLogger.Debug('EvaluateStatement: Processing ClassDeclaration');
    Result := EvaluateClass(TGocciaClassDeclaration(Statement), Context);
    TGocciaLogger.Debug('EvaluateStatement: ClassDeclaration result type: %s', [Result.ClassName]);
  end
  else if Statement is TGocciaImportDeclaration then
  begin
    TGocciaLogger.Debug('EvaluateStatement: Processing ImportDeclaration');
    ImportDecl := TGocciaImportDeclaration(Statement);
    TGocciaLogger.Debug('EvaluateStatement: Importing module: %s', [ImportDecl.ModulePath]);
    Module := Context.LoadModule(ImportDecl.ModulePath);

    for ImportPair in ImportDecl.Imports do
    begin
      TGocciaLogger.Debug('EvaluateStatement: Importing %s as %s', [ImportPair.Key, ImportPair.Value]);
      if Module.ExportsTable.TryGetValue(ImportPair.Value, Value) then
      begin
        TGocciaLogger.Debug('EvaluateStatement: Found export, type: %s', [Value.ClassName]);
        Context.Scope.SetValue(ImportPair.Key, Value);
      end
      else
      begin
        TGocciaLogger.Debug('EvaluateStatement: Export not found');
        Context.OnError(Format('Module "%s" has no export named "%s"',
          [ImportDecl.ModulePath, ImportPair.Value]), Statement.Line, Statement.Column);
      end;
    end;
  end;

  TGocciaLogger.Debug('EvaluateStatement: Final result type: %s', [Result.ClassName]);
end;

function EvaluateBinary(BinaryExpression: TGocciaBinaryExpression; Context: TGocciaEvaluationContext): TGocciaValue;
var
  Left, Right: TGocciaValue;
  LeftNum, RightNum: Double;
begin
  Left := EvaluateExpression(BinaryExpression.Left, Context);
  Right := EvaluateExpression(BinaryExpression.Right, Context);

  case BinaryExpression.Operator of
    gttPlus:
      begin
        if (Left is TGocciaNumberValue) and (Right is TGocciaNumberValue) then
          Result := TGocciaNumberValue.Create(
            TGocciaNumberValue(Left).Value + TGocciaNumberValue(Right).Value)
        else if (Left is TGocciaStringValue) or (Right is TGocciaStringValue) then
          Result := TGocciaStringValue.Create(Left.ToString + Right.ToString)
        else
          Result := TGocciaNumberValue.Create(Left.ToNumber + Right.ToNumber);
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
    gttAnd:
      if not Left.ToBoolean then
        Result := Left
      else
        Result := Right;
    gttOr:
      if Left.ToBoolean then
        Result := Left
      else
        Result := Right;
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
  else
    Result := TGocciaUndefinedValue.Create;
  end;
end;

function EvaluateCall(CallExpression: TGocciaCallExpression; Context: TGocciaEvaluationContext): TGocciaValue;
var
  Callee: TGocciaValue;
  Arguments: TObjectList<TGocciaValue>;
  I: Integer;
  FunctionValue: TGocciaFunctionValue;
  ThisValue: TGocciaValue;
begin
  TGocciaLogger.Debug('EvaluateCall: Start');
  TGocciaLogger.Debug('  CallExpression.Callee: %s', [CallExpression.Callee.ToString]);
  Callee := EvaluateExpression(CallExpression.Callee, Context);
  TGocciaLogger.Debug(' EvaluateCall: Callee: %s', [Callee.ToString]);
  Arguments := TObjectList<TGocciaValue>.Create(True);
  try
    for I := 0 to CallExpression.Arguments.Count - 1 do
      Arguments.Add(EvaluateExpression(CallExpression.Arguments[I], Context));

    if Callee is IGocciaCallable then
    begin
      if CallExpression.Callee is TGocciaMemberExpression then
        ThisValue := EvaluateExpression(TGocciaMemberExpression(CallExpression.Callee).ObjectExpr, Context)
      else
        ThisValue := TGocciaUndefinedValue.Create;

      TGocciaLogger.Debug('EvaluateCall: About to call Callable');
      Result := (Callee as IGocciaCallable).Call(Arguments, ThisValue);
      TGocciaLogger.Debug('EvaluateCall: Callable returned, result type: %s', [Result.ClassName]);
      TGocciaLogger.Debug('EvaluateCall: Result ToString: %s', [Result.ToString]);
    end else
    begin
      TGocciaLogger.Debug('EvaluateCall: Callee is not IGocciaCallable');
      Result := TGocciaUndefinedValue.Create;
    end;
  finally
    Arguments.Free;
  end;
  TGocciaLogger.Debug('EvaluateCall: Returning result type: %s', [Result.ClassName]);
  TGocciaLogger.Debug('EvaluateCall: Result ToString: %s', [Result.ToString]);
end;

function EvaluateMember(MemberExpression: TGocciaMemberExpression; Context: TGocciaEvaluationContext): TGocciaValue;
var
  Obj: TGocciaValue;
begin
  TGocciaLogger.Debug('EvaluateMember: Start');
  TGocciaLogger.Debug('  MemberExpression.ObjectExpr: %s', [MemberExpression.ObjectExpr.ToString]);
  TGocciaLogger.Debug('  MemberExpression.PropertyName: %s', [MemberExpression.PropertyName]);
  Obj := EvaluateExpression(MemberExpression.ObjectExpr, Context);
  TGocciaLogger.Debug('EvaluateMember: Obj: %s', [Obj.ToString]);

  if Obj is TGocciaObjectValue then
  begin
    TGocciaLogger.Debug('EvaluateMember: Obj is TGocciaObjectValue');
    Result := TGocciaObjectValue(Obj).GetProperty(MemberExpression.PropertyName);
    TGocciaLogger.Debug('EvaluateMember: Result: %s', [Result.ToString]);
  end
  else if Obj is TGocciaArrayValue then
  begin
    TGocciaLogger.Debug('EvaluateMember: Obj is TGocciaArrayValue');
    Result := TGocciaArrayValue(Obj).GetProperty(MemberExpression.PropertyName);
    TGocciaLogger.Debug('EvaluateMember: Result: %s', [Result.ToString]);
  end
  else
  begin
    TGocciaLogger.Debug('EvaluateMember: Obj is not TGocciaObjectValue or TGocciaArrayValue');
    Result := TGocciaUndefinedValue.Create;
  end;
  TGocciaLogger.Debug('EvaluateMember: Returning result type: %s', [Result.ClassName]);
  TGocciaLogger.Debug('EvaluateMember: Result ToString: %s', [Result.ToString]);
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
begin
  BlockValue := TGocciaBlockValue.Create(TGocciaBlockStatement(ArrowFunctionExpression.Body).Nodes, Context.Scope.CreateChild);
  // Always create a unique child scope for the closure
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

    Result := BlockValue.Execute;
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

function EvaluateClassMethod(ClassMethod: TGocciaClassMethod; Context: TGocciaEvaluationContext): TGocciaValue;
var
  BlockValue: TGocciaBlockValue;
begin
  BlockValue := TGocciaBlockValue.Create(TGocciaBlockStatement(ClassMethod.Body).Nodes, Context.Scope.CreateChild);
  // Always create a unique child scope for the closure
  Result := TGocciaMethodValue.Create(ClassMethod.Parameters, BlockValue, Context.Scope.CreateChild, ClassMethod.Name);
end;

function EvaluateClass(ClassDeclaration: TGocciaClassDeclaration; Context: TGocciaEvaluationContext): TGocciaValue;
var
  SuperClass: TGocciaClassValue;
  ClassValue: TGocciaClassValue;
  MethodPair: TPair<string, TGocciaClassMethod>;
  Method: TGocciaMethodValue;
begin
  SuperClass := nil;
  if ClassDeclaration.SuperClass <> '' then
  begin
    SuperClass := TGocciaClassValue(Context.Scope.GetValue(ClassDeclaration.SuperClass));
    if SuperClass = nil then
      Context.OnError(Format('Superclass "%s" not found', [ClassDeclaration.SuperClass]), ClassDeclaration.Line, ClassDeclaration.Column);
  end;

  ClassValue := TGocciaClassValue.Create(ClassDeclaration.Name, SuperClass);

  for MethodPair in ClassDeclaration.Methods do
  begin
    Method := TGocciaMethodValue(EvaluateClassMethod(MethodPair.Value, Context));
    ClassValue.AddMethod(MethodPair.Key, Method);
  end;

  Context.Scope.SetValue(ClassDeclaration.Name, ClassValue);
  Result := ClassValue;
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
  Arguments := TObjectList<TGocciaValue>.Create(True);
  try
    for I := 0 to NewExpression.Arguments.Count - 1 do
      Arguments.Add(EvaluateExpression(NewExpression.Arguments[I], Context));

    // Create new instance
    if Callee is TGocciaClassValue then
      Result := TGocciaClassValue(Callee).Instantiate(Arguments)
    else
      Context.OnError(Format('Cannot instantiate non-class value: %s', [Callee.TypeName]), NewExpression.Line, NewExpression.Column);
  finally
    Arguments.Free;
  end;
end;

end.
