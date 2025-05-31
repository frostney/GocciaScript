unit Goccia.Interpreter;

{$I Goccia.inc}

interface

uses
  Goccia.Values.Base, Goccia.AST.Node, Goccia.AST.Expressions, Goccia.AST.Statements, Goccia.Modules,
  Goccia.Values.Undefined, Goccia.Values.BooleanValue, Goccia.Values.Number, Goccia.Values.ObjectValue,
  Goccia.Values.StringValue, Goccia.Values.ArrayValue, Goccia.Values.FunctionValue, Goccia.Values.ClassValue,
  Goccia.Values.Null, Goccia.Values.NativeFunction, Goccia.Token, Generics.Collections,
  Classes, SysUtils, Math, Goccia.Error, Goccia.Values.Error, Goccia.Utils, Goccia.Parser, Goccia.Lexer, Goccia.Scope, Goccia.Builtins.Console, Goccia.Builtins.GlobalObject, Goccia.Builtins.Math, Goccia.Interfaces, Goccia.Logger;

type

  TGocciaInterpreter = class(TInterfacedObject, IGocciaInterpreter)
  private
    FGlobalScope: TGocciaScope;
    FModules: TDictionary<string, TGocciaModule>;
    FCurrentModule: TGocciaModule;
    FFileName: string;
    FSourceLines: TStringList;
    FArrayPrototype: TGocciaObjectValue;

    // Built-in functions
    procedure RegisterBuiltIns;
    procedure RegisterConsole;
    procedure RegisterMath;
    procedure RegisterPromise;
    procedure RegisterArrayMethods;
    procedure RegisterObjectMethods;

    // Evaluation methods
    function Evaluate(Node: TGocciaASTNode): TGocciaValue;
    function EvaluateExpression(Expr: TGocciaExpression): TGocciaValue;
    function EvaluateStatement(Stmt: TGocciaStatement): TGocciaValue;
    function EvaluateBinary(Expr: TGocciaBinaryExpression): TGocciaValue;
    function EvaluateUnary(Expr: TGocciaUnaryExpression): TGocciaValue;
    function EvaluateCall(Expr: TGocciaCallExpression): TGocciaValue;
    function EvaluateMember(Expr: TGocciaMemberExpression): TGocciaValue;
    function EvaluateArray(Expr: TGocciaArrayExpression): TGocciaValue;
    function EvaluateObject(Expr: TGocciaObjectExpression): TGocciaValue;
    function EvaluateArrowFunction(Expr: TGocciaArrowFunctionExpression): TGocciaValue;
    function EvaluateBlock(Stmt: TGocciaBlockStatement): TGocciaValue;
    function EvaluateIf(Stmt: TGocciaIfStatement): TGocciaValue;
    function EvaluateClassMethod(MethodValue: TGocciaClassMethod): TGocciaValue;
    function EvaluateClass(Stmt: TGocciaClassDeclaration): TGocciaValue;

    // ============= Built-in Function Implementations =============
    function ArrayMap(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
    function ArrayFilter(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
    function ArrayReduce(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
    function ArrayForEach(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;

    // Helper methods
    procedure ThrowError(const Message: string; Line, Column: Integer);
    function CallFunction(FunctionValue: TGocciaFunctionValue; Arguments: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;

  public
    constructor Create(const AFileName: string; ASourceLines: TStringList);
    destructor Destroy; override;
    function Execute(AProgram: TGocciaProgram): TGocciaValue;
    function LoadModule(const APath: string): TGocciaModule;
    procedure CheckForModuleReload(Module: TGocciaModule);
  end;



implementation



{ TGocciaInterpreter }

constructor TGocciaInterpreter.Create(const AFileName: string;
  ASourceLines: TStringList);
begin
  FFileName := AFileName;
  FSourceLines := ASourceLines;
  FGlobalScope := TGocciaScope.Create(Self);
  FModules := TDictionary<string, TGocciaModule>.Create;
  RegisterBuiltIns;
end;

destructor TGocciaInterpreter.Destroy;
begin
  FGlobalScope.Free;
  FModules.Free;
  FSourceLines.Free;
  FArrayPrototype.Free;
  inherited;
end;

procedure TGocciaInterpreter.RegisterBuiltIns;
begin
  RegisterConsole;
  RegisterMath;
  RegisterPromise;
  RegisterArrayMethods;
  RegisterObjectMethods;
end;

procedure TGocciaInterpreter.RegisterConsole;
begin
  TGocciaConsole.Create('console', FGlobalScope);
end;

procedure TGocciaInterpreter.RegisterMath;
begin
  TGocciaMath.Create('Math', FGlobalScope, ThrowError);
end;

procedure TGocciaInterpreter.RegisterPromise;
begin
  // Promise implementation would go here
  // For now, we'll just register a placeholder
end;

procedure TGocciaInterpreter.RegisterArrayMethods;
begin
  // TODO: We are hacking the Array prototype here, we should add this into the ArrayValue class
  FArrayPrototype := TGocciaObjectValue.Create;
  // Register array methods as instance methods
  FArrayPrototype.SetProperty('map', TGocciaNativeFunctionValue.Create(ArrayMap, 'map', 1));
  FArrayPrototype.SetProperty('filter', TGocciaNativeFunctionValue.Create(ArrayFilter, 'filter', 1));
  FArrayPrototype.SetProperty('reduce', TGocciaNativeFunctionValue.Create(ArrayReduce, 'reduce', 1));
  FArrayPrototype.SetProperty('forEach', TGocciaNativeFunctionValue.Create(ArrayForEach, 'forEach', 1));
end;

procedure TGocciaInterpreter.RegisterObjectMethods;
begin
  TGocciaGlobalObject.Create('Object', FGlobalScope, ThrowError);
end;

function TGocciaInterpreter.Execute(AProgram: TGocciaProgram): TGocciaValue;
var
  I: Integer;
begin
  Result := TGocciaUndefinedValue.Create;
  for I := 0 to AProgram.Body.Count - 1 do
    Result := EvaluateStatement(AProgram.Body[I]);
end;

function TGocciaInterpreter.LoadModule(const APath: string): TGocciaModule;
var
  Source: TStringList;
  Lexer: TGocciaLexer;
  Parser: TGocciaParser;
  ProgramNode: TGocciaProgram;
  Module: TGocciaModule;
  I: Integer;
  Stmt: TGocciaStatement;
  ExportDecl: TGocciaExportDeclaration;
  ExportPair: TPair<string, string>;
  Value: TGocciaValue;
begin
  if FModules.TryGetValue(APath, Result) then
  begin
    CheckForModuleReload(Result);
    Exit;
  end;

  Source := TStringList.Create;
  try
    Source.LoadFromFile(APath);

    Lexer := TGocciaLexer.Create(Source.Text, APath);
    try
      Parser := TGocciaParser.Create(Lexer.ScanTokens, APath, Source);
      try
        ProgramNode := Parser.Parse;
        try
          Module := TGocciaModule.Create(APath);
          Module.LastModified := FileDateToDateTime(FileAge(APath));
          FModules.Add(APath, Module);

          // Execute module in its own scope
          FCurrentModule := Module;
          Execute(ProgramNode);

          // Process exports
          for I := 0 to ProgramNode.Body.Count - 1 do
          begin
            Stmt := ProgramNode.Body[I];
            if Stmt is TGocciaExportDeclaration then
            begin
              ExportDecl := TGocciaExportDeclaration(Stmt);
              for ExportPair in ExportDecl.ExportsTable do
              begin
                Value := FGlobalScope.GetValue(ExportPair.Value);
                if Assigned(Value) then
                  Module.ExportsTable.Add(ExportPair.Key, Value);
              end;
            end;
          end;

          FCurrentModule := nil;
          Result := Module;
        finally
          ProgramNode.Free;
        end;
      finally
        Parser.Free;
      end;
    finally
      Lexer.Free;
    end;
  finally
    Source.Free;
  end;
end;

procedure TGocciaInterpreter.CheckForModuleReload(Module: TGocciaModule);
var
  CurrentModified: TDateTime;
begin
  CurrentModified := FileDateToDateTime(FileAge(Module.Path));
  if CurrentModified > Module.LastModified then
  begin
    // Reload module
    Module.ExportsTable.Clear;
    Module.LastModified := CurrentModified;
    LoadModule(Module.Path);
  end;
end;

function TGocciaInterpreter.Evaluate(Node: TGocciaASTNode): TGocciaValue;
begin
  TGocciaLogger.Debug('Evaluate: Node class is %s', [Node.ClassName]);
  TGocciaLogger.Debug('Evaluate: Node line: %d', [Node.Line]);
  TGocciaLogger.Debug('Evaluate: Node column: %d', [Node.Column]);

  if Node is TGocciaExpression then
  begin
    TGocciaLogger.Debug('Evaluate: Processing as Expression');
    Result := EvaluateExpression(TGocciaExpression(Node));
    TGocciaLogger.Debug('Evaluate: Expression result type: %s', [Result.ClassName]);
  end
  else if Node is TGocciaStatement then
  begin
    TGocciaLogger.Debug('Evaluate: Processing as Statement');
    Result := EvaluateStatement(TGocciaStatement(Node));
    TGocciaLogger.Debug('Evaluate: Statement result type: %s', [Result.ClassName]);
  end
  else
  begin
    TGocciaLogger.Debug('Evaluate: Unknown node type, returning undefined');
    Result := TGocciaUndefinedValue.Create;
  end;
  TGocciaLogger.Debug('Evaluate: Final result type: %s', [Result.ClassName]);
end;

function TGocciaInterpreter.EvaluateExpression(Expr: TGocciaExpression): TGocciaValue;
var
  LeftExpr: TGocciaExpression;
  Value: TGocciaValue;
  Obj: TGocciaValue;
  PropName: string;
  I: Integer;
  Callee: TGocciaValue;
  Arguments: TObjectList<TGocciaValue>;
begin
  if Expr is TGocciaLiteralExpression then
    Result := TGocciaLiteralExpression(Expr).Value
  else if Expr is TGocciaIdentifierExpression then
  begin
    Result := FGlobalScope.GetValue(TGocciaIdentifierExpression(Expr).Name);
    if Result = nil then
      ThrowError(Format('Undefined variable: %s',
        [TGocciaIdentifierExpression(Expr).Name]), Expr.Line, Expr.Column);
  end
  else if Expr is TGocciaBinaryExpression then
    Result := EvaluateBinary(TGocciaBinaryExpression(Expr))
  else if Expr is TGocciaUnaryExpression then
    Result := EvaluateUnary(TGocciaUnaryExpression(Expr))
  else if Expr is TGocciaAssignmentExpression then
  begin
    // Variable assignment
    Result := EvaluateExpression(TGocciaAssignmentExpression(Expr).Value);
    FGlobalScope.Assign(TGocciaAssignmentExpression(Expr).Name, Result);
  end
  else if Expr is TGocciaPropertyAssignmentExpression then
  begin
    // Property assignment
    Obj := EvaluateExpression(TGocciaPropertyAssignmentExpression(Expr).ObjectExpr);
    Value := EvaluateExpression(TGocciaPropertyAssignmentExpression(Expr).Value);

    // Special handling for 'this' property assignment
    if (Obj is TGocciaInstanceValue) then
    begin
      TGocciaInstanceValue(Obj).SetProperty(TGocciaPropertyAssignmentExpression(Expr).PropertyName, Value);
      Result := Value;
    end
    else if (Obj is TGocciaObjectValue) then
    begin
      TGocciaObjectValue(Obj).SetProperty(TGocciaPropertyAssignmentExpression(Expr).PropertyName, Value);
      Result := Value;
    end
    else
      ThrowError('Cannot set property on non-object', Expr.Line, Expr.Column);
  end
  else if Expr is TGocciaCallExpression then
  begin
    TGocciaLogger.Debug('EvaluateExpression: TGocciaCallExpression - before EvaluateCall');
    Result := EvaluateCall(TGocciaCallExpression(Expr));
    TGocciaLogger.Debug('EvaluateExpression: TGocciaCallExpression - after EvaluateCall, result type: %s', [Result.ClassName]);
    TGocciaLogger.Debug('EvaluateExpression: TGocciaCallExpression - result ToString: %s', [Result.ToString]);
  end
  else if Expr is TGocciaMemberExpression then
    Result := EvaluateMember(TGocciaMemberExpression(Expr))
  else if Expr is TGocciaArrayExpression then
    Result := EvaluateArray(TGocciaArrayExpression(Expr))
  else if Expr is TGocciaObjectExpression then
    Result := EvaluateObject(TGocciaObjectExpression(Expr))
  else if Expr is TGocciaArrowFunctionExpression then
    Result := EvaluateArrowFunction(TGocciaArrowFunctionExpression(Expr))
  else if Expr is TGocciaConditionalExpression then
  begin
    if EvaluateExpression(TGocciaConditionalExpression(Expr).Condition).ToBoolean then
      Result := EvaluateExpression(TGocciaConditionalExpression(Expr).Consequent)
    else
      Result := EvaluateExpression(TGocciaConditionalExpression(Expr).Alternate);
  end
  else if Expr is TGocciaNewExpression then
  begin
    // Handle new expression - create instance
    Callee := EvaluateExpression(TGocciaNewExpression(Expr).Callee);
    Arguments := TObjectList<TGocciaValue>.Create(True);
    try
      for I := 0 to TGocciaNewExpression(Expr).Arguments.Count - 1 do
        Arguments.Add(EvaluateExpression(TGocciaNewExpression(Expr).Arguments[I]));

      if Callee is TGocciaClassValue then
      begin
        Result := TGocciaClassValue(Callee).Instantiate(Arguments, Self);
      end
      else
        ThrowError(Format('Can only instantiate classes, not %s', [Callee.TypeName]),
          Expr.Line, Expr.Column);
    finally
      Arguments.Free;
    end;
  end
  else if Expr is TGocciaThisExpression then
  begin
    // Handle this expression
    Result := FGlobalScope.ThisValue;
  end
  else if Expr is TGocciaMemberExpression then
  begin
    if TGocciaMemberExpression(Expr).ObjectExpr is TGocciaThisExpression then
    begin
      // When accessing a property through this, use the current scope's this value
      Result := FGlobalScope.GetThisProperty(TGocciaMemberExpression(Expr).PropertyName);
    end
    else
    begin
      // For normal member access, evaluate the object first
      Result := EvaluateMember(TGocciaMemberExpression(Expr));
    end;
  end
  else
    Result := TGocciaUndefinedValue.Create;
  TGocciaLogger.Debug('EvaluateExpression: Returning result type: %s .ToString: %s', [Result.ClassName, Result.ToString]);
end;

function TGocciaInterpreter.EvaluateStatement(Stmt: TGocciaStatement): TGocciaValue;
var
  Decl: TGocciaVariableDeclaration;
  ImportDecl: TGocciaImportDeclaration;
  Module: TGocciaModule;
  ImportPair: TPair<string, string>;
  Value: TGocciaValue;
begin
  TGocciaLogger.Debug('EvaluateStatement: Statement class is %s', [Stmt.ClassName]);
  TGocciaLogger.Debug('EvaluateStatement: Statement line: %d', [Stmt.Line]);
  TGocciaLogger.Debug('EvaluateStatement: Statement column: %d', [Stmt.Column]);

  Result := TGocciaUndefinedValue.Create;

  if Stmt is TGocciaExpressionStatement then
  begin
    TGocciaLogger.Debug('EvaluateStatement: Processing ExpressionStatement');
    Result := EvaluateExpression(TGocciaExpressionStatement(Stmt).Expression);
    TGocciaLogger.Debug('EvaluateStatement: ExpressionStatement result type: %s', [Result.ClassName]);
  end
  else if Stmt is TGocciaVariableDeclaration then
  begin
    TGocciaLogger.Debug('EvaluateStatement: Processing VariableDeclaration');
    Decl := TGocciaVariableDeclaration(Stmt);
    TGocciaLogger.Debug('EvaluateStatement: Variable name: %s', [Decl.Name]);
    Value := EvaluateExpression(Decl.Initializer);
    TGocciaLogger.Debug('EvaluateStatement: Initializer result type: %s', [Value.ClassName]);
    FGlobalScope.SetValue(Decl.Name, Value);
    TGocciaLogger.Debug('EvaluateStatement: Variable defined in scope');
  end
  else if Stmt is TGocciaBlockStatement then
  begin
    TGocciaLogger.Debug('EvaluateStatement: Processing BlockStatement');
    Result := EvaluateBlock(TGocciaBlockStatement(Stmt));
    TGocciaLogger.Debug('EvaluateStatement: BlockStatement result type: %s', [Result.ClassName]);
  end
  else if Stmt is TGocciaIfStatement then
  begin
    TGocciaLogger.Debug('EvaluateStatement: Processing IfStatement');
    Result := EvaluateIf(TGocciaIfStatement(Stmt));
    TGocciaLogger.Debug('EvaluateStatement: IfStatement result type: %s', [Result.ClassName]);
  end
  else if Stmt is TGocciaReturnStatement then
  begin
    TGocciaLogger.Debug('EvaluateStatement: Handling TGocciaReturnStatement');
    if Assigned(TGocciaReturnStatement(Stmt).Value) then
    begin
      TGocciaLogger.Debug('EvaluateStatement: Evaluating return value');
      Value := EvaluateExpression(TGocciaReturnStatement(Stmt).Value);
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
  else if Stmt is TGocciaThrowStatement then
  begin
    TGocciaLogger.Debug('EvaluateStatement: Processing ThrowStatement');
    Value := EvaluateExpression(TGocciaThrowStatement(Stmt).Value);
    TGocciaLogger.Debug('EvaluateStatement: Throw value type: %s', [Value.ClassName]);
    TGocciaLogger.Debug('EvaluateStatement: Throw value ToString: %s', [Value.ToString]);
    raise TGocciaThrowValue.Create(Value);
  end
  else if Stmt is TGocciaTryStatement then
  begin
    TGocciaLogger.Debug('EvaluateStatement: Processing TryStatement');
    Result := TGocciaUndefinedValue.Create; // Placeholder
  end
  else if Stmt is TGocciaClassDeclaration then
  begin
    TGocciaLogger.Debug('EvaluateStatement: Processing ClassDeclaration');
    Result := EvaluateClass(TGocciaClassDeclaration(Stmt));
    TGocciaLogger.Debug('EvaluateStatement: ClassDeclaration result type: %s', [Result.ClassName]);
  end
  else if Stmt is TGocciaImportDeclaration then
  begin
    TGocciaLogger.Debug('EvaluateStatement: Processing ImportDeclaration');
    ImportDecl := TGocciaImportDeclaration(Stmt);
    TGocciaLogger.Debug('EvaluateStatement: Importing module: %s', [ImportDecl.ModulePath]);
    Module := LoadModule(ImportDecl.ModulePath);

    for ImportPair in ImportDecl.Imports do
    begin
      TGocciaLogger.Debug('EvaluateStatement: Importing %s as %s', [ImportPair.Key, ImportPair.Value]);
      if Module.ExportsTable.TryGetValue(ImportPair.Value, Value) then
      begin
        TGocciaLogger.Debug('EvaluateStatement: Found export, type: %s', [Value.ClassName]);
        FGlobalScope.SetValue(ImportPair.Key, Value);
      end
      else
      begin
        TGocciaLogger.Debug('EvaluateStatement: Export not found');
        ThrowError(Format('Module "%s" has no export named "%s"',
          [ImportDecl.ModulePath, ImportPair.Value]), Stmt.Line, Stmt.Column);
      end;
    end;
  end;

  TGocciaLogger.Debug('EvaluateStatement: Final result type: %s', [Result.ClassName]);
end;

function TGocciaInterpreter.EvaluateBinary(Expr: TGocciaBinaryExpression): TGocciaValue;
var
  Left, Right: TGocciaValue;
  LeftNum, RightNum: Double;
begin
  Left := EvaluateExpression(Expr.Left);
  Right := EvaluateExpression(Expr.Right);

  case Expr.Operator of
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

function TGocciaInterpreter.EvaluateUnary(Expr: TGocciaUnaryExpression): TGocciaValue;
var
  Operand: TGocciaValue;
begin
  Operand := EvaluateExpression(Expr.Operand);

  case Expr.Operator of
    gttNot:
      Result := TGocciaBooleanValue.Create(not Operand.ToBoolean);
    gttMinus:
      Result := TGocciaNumberValue.Create(-Operand.ToNumber);
  else
    Result := TGocciaUndefinedValue.Create;
  end;
end;

function TGocciaInterpreter.EvaluateCall(Expr: TGocciaCallExpression): TGocciaValue;
var
  Callee: TGocciaValue;
  Arguments: TObjectList<TGocciaValue>;
  I: Integer;
  FunctionValue: TGocciaFunctionValue;
  ThisValue: TGocciaValue;
begin
  TGocciaLogger.Debug('EvaluateCall: Start');
  Callee := EvaluateExpression(Expr.Callee);
  TGocciaLogger.Debug('EvaluateCall: Callee type: %s', [Callee.ClassName]);
  Arguments := TObjectList<TGocciaValue>.Create(True);
  try
    for I := 0 to Expr.Arguments.Count - 1 do
      Arguments.Add(EvaluateExpression(Expr.Arguments[I]));

    if Callee is TGocciaFunctionValue then
    begin
      FunctionValue := TGocciaFunctionValue(Callee);
      if Expr.Callee is TGocciaMemberExpression then
        ThisValue := EvaluateExpression(TGocciaMemberExpression(Expr.Callee).ObjectExpr)
      else
        ThisValue := TGocciaUndefinedValue.Create;
      TGocciaLogger.Debug('EvaluateCall: About to call CallFunction');
      Result := CallFunction(FunctionValue, Arguments, ThisValue);
      TGocciaLogger.Debug('EvaluateCall: CallFunction returned, result type: %s', [Result.ClassName]);
      TGocciaLogger.Debug('EvaluateCall: Result ToString: %s', [Result.ToString]);
    end
    else if Callee is TGocciaNativeFunctionValue then
    begin
      if Expr.Callee is TGocciaMemberExpression then
        ThisValue := EvaluateExpression(TGocciaMemberExpression(Expr.Callee).ObjectExpr)
      else
        ThisValue := TGocciaUndefinedValue.Create;
      Result := TGocciaNativeFunctionValue(Callee).NativeFunction(Arguments, ThisValue);
      TGocciaLogger.Debug('EvaluateCall: NativeFunction returned, result type: %s', [Result.ClassName]);
      TGocciaLogger.Debug('EvaluateCall: Result ToString: %s', [Result.ToString]);
    end
    else
      ThrowError(Format('Can only call functions, not %s', [Callee.TypeName]), Expr.Line, Expr.Column);
  finally
    Arguments.Free;
  end;
  TGocciaLogger.Debug('EvaluateCall: Returning result type: %s', [Result.ClassName]);
  TGocciaLogger.Debug('EvaluateCall: Result ToString: %s', [Result.ToString]);
end;

function TGocciaInterpreter.EvaluateMember(Expr: TGocciaMemberExpression): TGocciaValue;
var
  Obj: TGocciaValue;
  ProtoFunction: TGocciaNativeFunctionValue;
begin
  Obj := EvaluateExpression(Expr.ObjectExpr);

  if Obj is TGocciaObjectValue then
    Result := TGocciaObjectValue(Obj).GetProperty(Expr.PropertyName)
  else if Obj is TGocciaArrayValue then
  begin
    // Handle array access
    if Expr.PropertyName = 'length' then
      Result := TGocciaNumberValue.Create(TGocciaArrayValue(Obj).Elements.Count)
    else if Assigned(FArrayPrototype) then
    begin
      Result := FArrayPrototype.GetProperty(Expr.PropertyName);
      if Result = nil then
        Result := TGocciaUndefinedValue.Create;
    end
    else
      Result := TGocciaUndefinedValue.Create;
  end
  else
    Result := TGocciaUndefinedValue.Create;
end;

function TGocciaInterpreter.EvaluateArray(Expr: TGocciaArrayExpression): TGocciaValue;
var
  Arr: TGocciaArrayValue;
  I: Integer;
begin
  Arr := TGocciaArrayValue.Create;
  for I := 0 to Expr.Elements.Count - 1 do
    Arr.Elements.Add(EvaluateExpression(Expr.Elements[I]));
  Result := Arr;
end;

function TGocciaInterpreter.EvaluateObject(Expr: TGocciaObjectExpression): TGocciaValue;
var
  Obj: TGocciaObjectValue;
  Pair: TPair<string, TGocciaExpression>;
begin
  Obj := TGocciaObjectValue.Create;
  for Pair in Expr.Properties do
    Obj.SetProperty(Pair.Key, EvaluateExpression(Pair.Value));
  Result := Obj;
end;

function TGocciaInterpreter.EvaluateArrowFunction(Expr: TGocciaArrowFunctionExpression): TGocciaValue;
begin
  // Always create a unique child scope for the closure
  Result := TGocciaFunctionValue.Create(Expr.Parameters, Expr.Body, FGlobalScope.CreateChild);
end;

function TGocciaInterpreter.EvaluateBlock(Stmt: TGocciaBlockStatement): TGocciaValue;
var
  I: Integer;
begin
  // Block does have a lexical scope, not a function scope so we don't need to push a new scope
  try
    Result := TGocciaUndefinedValue.Create;
    for I := 0 to Stmt.Statements.Count - 1 do
      Result := Evaluate(Stmt.Statements[I]);
  finally

  end;
end;

function TGocciaInterpreter.EvaluateIf(Stmt: TGocciaIfStatement): TGocciaValue;
begin
  if EvaluateExpression(Stmt.Condition).ToBoolean then
    Result := EvaluateStatement(Stmt.Consequent)
  else if Assigned(Stmt.Alternate) then
    Result := EvaluateStatement(Stmt.Alternate)
  else
    Result := TGocciaUndefinedValue.Create;
end;

function TGocciaInterpreter.EvaluateClassMethod(MethodValue: TGocciaClassMethod): TGocciaValue;
begin
  // Always create a unique child scope for the closure
  Result := TGocciaMethodValue.Create(MethodValue.Parameters, MethodValue.Body, FGlobalScope.CreateChild, MethodValue.Name);
end;

function TGocciaInterpreter.EvaluateClass(Stmt: TGocciaClassDeclaration): TGocciaValue;
var
  SuperClass: TGocciaClassValue;
  ClassValue: TGocciaClassValue;
  MethodPair: TPair<string, TGocciaClassMethod>;
  Method: TGocciaMethodValue;
begin
  SuperClass := nil;
  if Stmt.SuperClass <> '' then
  begin
    SuperClass := TGocciaClassValue(FGlobalScope.GetValue(Stmt.SuperClass));
    if SuperClass = nil then
      ThrowError(Format('Superclass "%s" not found', [Stmt.SuperClass]), Stmt.Line, Stmt.Column);
  end;

  ClassValue := TGocciaClassValue.Create(Stmt.Name, SuperClass);

  for MethodPair in Stmt.Methods do
  begin
    Method := TGocciaMethodValue(EvaluateClassMethod(MethodPair.Value));
    ClassValue.AddMethod(MethodPair.Key, Method);
  end;

  FGlobalScope.SetValue(Stmt.Name, ClassValue);
  Result := ClassValue;
end;

procedure TGocciaInterpreter.ThrowError(const Message: string; Line, Column: Integer);
begin
  raise TGocciaRuntimeError.Create(Message, Line, Column, FFileName, FSourceLines);
end;

// Built-in function implementations





function TGocciaInterpreter.ArrayMap(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
var
  ResultArray: TGocciaArrayValue;
  Callback: TGocciaValue;
begin
  TGocciaLogger.Debug('ArrayMap: Entered');
  TGocciaLogger.Debug('  ThisValue type: %s', [ThisValue.ClassName]);
  TGocciaLogger.Debug('  Args.Count: %d', [Args.Count]);

  if not (ThisValue is TGocciaArrayValue) then
    ThrowError('Array.map called on non-array', 0, 0);

  if Args.Count < 1 then
    ThrowError('Array.map expects callback function', 0, 0);

  Callback := Args[0];

  if not ((Callback is TGocciaFunctionValue) or (Callback is TGocciaNativeFunctionValue)) then
    ThrowError('Callback must be a function', 0, 0);

  ResultArray := TGocciaArrayValue.Create;

  if Callback is TGocciaFunctionValue then
  begin
    Result := TGocciaFunctionInvocation.Create(Self, FGlobalScope.CreateChild, TGocciaFunctionValue(Callback).Parameters, TGocciaFunctionValue(Callback).Body).Call(Args, ThisValue);
    Exit;
  end;

  if Callback is TGocciaNativeFunctionValue then
  begin
    Result := TGocciaNativeFunctionValue(Callback).NativeFunction(Args, ThisValue);
    Exit;
  end;

  TGocciaLogger.Debug('ArrayMap: Returning result type: %s', [ResultArray.ClassName]);
  TGocciaLogger.Debug('ArrayMap: Result ToString: %s', [ResultArray.ToString]);
  Result := ResultArray;
end;

function TGocciaInterpreter.ArrayFilter(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
var
  Arr: TGocciaArrayValue;
  Callback: TGocciaValue;
  NewArr: TGocciaArrayValue;
  CallArgs: TObjectList<TGocciaValue>;
  I, J: Integer;
  OldScope: TGocciaScope;
  NewScope: TGocciaScope;
  PredicateResult: TGocciaValue;
begin
  if not (ThisValue is TGocciaArrayValue) then
    ThrowError('Array.filter called on non-array', 0, 0);

  if Args.Count < 1 then
    ThrowError('Array.filter expects callback function', 0, 0);

  Arr := TGocciaArrayValue(ThisValue);
  Callback := Args[0];

  if not ((Callback is TGocciaFunctionValue) or (Callback is TGocciaNativeFunctionValue)) then
    ThrowError('Callback must be a function', 0, 0);

  NewArr := TGocciaArrayValue.Create;

  for I := 0 to Arr.Elements.Count - 1 do
  begin
    CallArgs := TObjectList<TGocciaValue>.Create(False);
    try
      // Add the current element as first argument
      CallArgs.Add(Arr.Elements[I]);
      // Add the index as second argument
      CallArgs.Add(TGocciaNumberValue.Create(I));
      // Add the array itself as third argument
      CallArgs.Add(Arr);

      if Callback is TGocciaNativeFunctionValue then
        PredicateResult := TGocciaNativeFunctionValue(Callback).NativeFunction(CallArgs, Arr)
      else if Callback is TGocciaFunctionValue then
      begin
        // Create new scope for function execution
        OldScope := FGlobalScope;
        NewScope := TGocciaFunctionValue(Callback).Closure.CreateChild;
        FGlobalScope := NewScope;
        try
          // Bind parameters
          for J := 0 to TGocciaFunctionValue(Callback).Parameters.Count - 1 do
          begin
            if J < CallArgs.Count then
              NewScope.SetValue(TGocciaFunctionValue(Callback).Parameters[J], CallArgs[J])
            else
              NewScope.SetValue(TGocciaFunctionValue(Callback).Parameters[J], TGocciaUndefinedValue.Create);
          end;

          // Execute function body
          try
            try
              PredicateResult := Evaluate(TGocciaFunctionValue(Callback).Body);
            except
              on E: TGocciaReturnValue do
                PredicateResult := E.Value;
            end;
          finally
            FGlobalScope.Free;
            FGlobalScope := OldScope;
          end;
        finally
        end;
      end;

      if PredicateResult is TGocciaBooleanValue then
      begin
        if TGocciaBooleanValue(PredicateResult).Value then
          NewArr.Elements.Add(Arr.Elements[I]);
      end
      else
        ThrowError('Filter callback must return boolean', 0, 0);
    finally
      CallArgs.Free;
    end;
  end;

  Result := NewArr;
end;

function TGocciaInterpreter.ArrayReduce(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
var
  Arr: TGocciaArrayValue;
  Callback: TGocciaValue;
  Accumulator: TGocciaValue;
  CallArgs: TObjectList<TGocciaValue>;
  I, StartIndex: Integer;
begin
  if Args.Count < 1 then
    ThrowError('Array.reduce expects array as this value', 0, 0);

  if not (Args[0] is TGocciaArrayValue) then
    ThrowError('Array.reduce called on non-array', 0, 0);

  if Args.Count < 2 then
    ThrowError('Array.reduce expects callback function', 0, 0);

  Arr := TGocciaArrayValue(Args[0]);
  Callback := Args[1];

  if not ((Callback is TGocciaFunctionValue) or (Callback is TGocciaNativeFunctionValue)) then
    ThrowError('Callback must be a function', 0, 0);

  if Args.Count >= 3 then
  begin
    Accumulator := Args[2];
    StartIndex := 0;
  end
  else
  begin
    if Arr.Elements.Count = 0 then
      ThrowError('Reduce of empty array with no initial value', 0, 0);
    Accumulator := Arr.Elements[0];
    StartIndex := 1;
  end;

  for I := StartIndex to Arr.Elements.Count - 1 do
  begin
    CallArgs := TObjectList<TGocciaValue>.Create(False);
    try
      CallArgs.Add(Accumulator);
      CallArgs.Add(Arr.Elements[I]);
      CallArgs.Add(TGocciaNumberValue.Create(I));
      CallArgs.Add(Arr);

      if Callback is TGocciaNativeFunctionValue then
        Accumulator := TGocciaNativeFunctionValue(Callback).NativeFunction(CallArgs, Arr)
      else
        // It's very likely it's a user function - TODO: Implement user function call
        Accumulator := TGocciaUndefinedValue.Create; // Simplified
    finally
      CallArgs.Free;
    end;
  end;

  Result := Accumulator;
end;

function TGocciaInterpreter.ArrayForEach(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
var
  Arr: TGocciaArrayValue;
  Callback: TGocciaValue;
  CallArgs: TObjectList<TGocciaValue>;
  I, J: Integer;
  OldScope: TGocciaScope;
  NewScope: TGocciaScope;
begin
  if not (ThisValue is TGocciaArrayValue) then
    ThrowError('Array.forEach called on non-array', 0, 0);

  if Args.Count < 1 then
    ThrowError('Array.forEach expects callback function', 0, 0);

  Arr := TGocciaArrayValue(ThisValue);
  Callback := Args[0];

  if not ((Callback is TGocciaFunctionValue) or (Callback is TGocciaNativeFunctionValue)) then
    ThrowError('Callback must be a function', 0, 0);



  for I := 0 to Arr.Elements.Count - 1 do
  begin
    CallArgs := TObjectList<TGocciaValue>.Create(False);
    try
      // Add the current element as first argument
      CallArgs.Add(Arr.Elements[I]);
      // Add the index as second argument
      CallArgs.Add(TGocciaNumberValue.Create(I));
      // Add the array itself as third argument
      CallArgs.Add(Arr);

      if Callback is TGocciaNativeFunctionValue then
        TGocciaNativeFunctionValue(Callback).NativeFunction(CallArgs, Arr)
      else if Callback is TGocciaFunctionValue then
      begin
        // Create new scope for function execution
        OldScope := FGlobalScope;
        NewScope := TGocciaFunctionValue(Callback).Closure.CreateChild;
        FGlobalScope := NewScope;
        try
          // Bind parameters
          for J := 0 to TGocciaFunctionValue(Callback).Parameters.Count - 1 do
          begin
            if J < CallArgs.Count then
              NewScope.SetValue(TGocciaFunctionValue(Callback).Parameters[J], CallArgs[J])
            else
              NewScope.SetValue(TGocciaFunctionValue(Callback).Parameters[J], TGocciaUndefinedValue.Create);
          end;

          // Execute function body
          try
            try
              Evaluate(TGocciaFunctionValue(Callback).Body);
            except
              on E: TGocciaReturnValue do
                ; // Ignore return values in forEach
            end;
          finally
            FGlobalScope.Free;
            FGlobalScope := OldScope;
          end;
        finally
        end;
      end;
    finally
      CallArgs.Free;
    end;
  end;

  Result := TGocciaUndefinedValue.Create;
end;



function TGocciaInterpreter.CallFunction(FunctionValue: TGocciaFunctionValue; Arguments: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
begin
  TGocciaLogger.Debug('CallFunction: FunctionValue type is %s (TypeName: %s)', [FunctionValue.ClassName, FunctionValue.TypeName]);
  TGocciaLogger.Debug('CallFunction: Body class is %s', [FunctionValue.Body.ClassName]);
  TGocciaLogger.Debug('CallFunction: Closure class is %s', [FunctionValue.Closure.ClassName]);
  TGocciaLogger.Debug('CallFunction: Number of parameters: %d', [FunctionValue.Parameters.Count]);
  TGocciaLogger.Debug('CallFunction: Number of arguments: %d', [Arguments.Count]);
  TGocciaLogger.Debug('CallFunction: ThisValue type: %s', [ThisValue.ClassName]);

  try
    Result := FunctionValue.Call(Arguments, ThisValue, Self);
    TGocciaLogger.Debug('CallFunction: Function call completed, result type: %s', [Result.ClassName]);
    TGocciaLogger.Debug('CallFunction: Result ToString: %s', [Result.ToString]);
  except
    on E: TGocciaReturnValue do
    begin
      TGocciaLogger.Debug('CallFunction: Caught TGocciaReturnValue with value type: %s', [E.Value.ClassName]);
      TGocciaLogger.Debug('CallFunction: Return value ToString: %s', [E.Value.ToString]);
      Result := E.Value;
    end;
  end;

  TGocciaLogger.Debug('CallFunction: Returning with Result type: %s', [Result.ClassName]);
  TGocciaLogger.Debug('CallFunction: Result ToString: %s', [Result.ToString]);
end;

function TGocciaInterpreter.EvaluateNewExpression(Node: TGocciaNewExpression): TGocciaValue;
var
  Callee: TGocciaValue;
  Arguments: TObjectList<TGocciaValue>;
  I: Integer;
begin
  // Evaluate the callee (class)
  Callee := EvaluateExpression(Node.Callee);

  // Evaluate arguments
  Arguments := TObjectList<TGocciaValue>.Create(True);
  try
    for I := 0 to Node.Arguments.Count - 1 do
      Arguments.Add(EvaluateExpression(Node.Arguments[I]));

    // Create new instance
    if Callee is TGocciaClassValue then
      Result := TGocciaClassValue(Callee).Instantiate(Arguments)
    else
      raise TGocciaError.Create('Cannot instantiate non-class value');
  finally
    Arguments.Free;
  end;
end;

end.
