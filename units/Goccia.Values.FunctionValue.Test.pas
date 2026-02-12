program Goccia.Values.FunctionValue.Test;

{$I Goccia.inc}

uses
  Goccia.Values.FunctionValue, Goccia.Values.ObjectValue, Goccia.Values.Primitives,
  Goccia.Arguments.Collection,
  TestRunner, Goccia.AST.Node, Generics.Collections,
  Goccia.AST.Statements, Goccia.AST.Expressions, Goccia.Scope,
  Goccia.Token, SysUtils, Classes, Goccia.Evaluator;

type
  TTestBlockValue = class(TTestSuite)
  public
    procedure SetupTests; override;
    procedure TestSimpleBlock;
    procedure TestArithmeticBlock;
  end;

  TTestFunctionValue = class(TTestSuite)
  public
    procedure SetupTests; override;

    procedure TestSimpleFunction;
    procedure TestFunctionWithParameters;
    procedure TestFunctionWithReturn;
    procedure TestFunctionWithScope;
    procedure TestFunctionWithScopeAndParameters;
    procedure TestFunctionWithScopeAndReturn;
    procedure TestFunctionWithScopeAndParametersAndReturn;
  end;

  TTestMethodValue = class(TTestSuite)
  public
    procedure SetupTests; override;

    procedure TestSimpleMethod;
    procedure TestMethodWithParameters;
    procedure TestMethodWithReturn;
    procedure TestMethodWithScope;
    procedure TestMethodWithScopeAndParameters;
    procedure TestMethodWithScopeAndReturn;
    procedure TestMethodWithScopeAndParametersAndReturn;
  end;

  function CreateSimpleFunction<T>(const AName: String; const AParameters: TStringList; const AScope: TGocciaScope): T;
  var
    Statements: TObjectList<TGocciaASTNode>;
    ParamArray: TGocciaParameterArray;
    I: Integer;
  begin
    Statements := TObjectList<TGocciaASTNode>.Create;
    Statements.Add(TGocciaLiteralExpression.Create(TGocciaUndefinedLiteralValue.UndefinedValue, 0, 0));

    SetLength(ParamArray, AParameters.Count);
    for I := 0 to AParameters.Count - 1 do
    begin
      ParamArray[I].Name := AParameters[I];
      ParamArray[I].DefaultValue := nil;
    end;

    Result := T.Create(ParamArray, Statements, AScope, AName);
  end;

  procedure TestSimpleFunctionFromTemplate<T>;
  var
    FunctionValue: T;
    Scope: TGocciaScope;
    Parameters: TStringList;
    ReturnValue: TGocciaValue;
    Args: TGocciaArgumentsCollection;
  begin
    Scope := TGocciaScope.Create(nil, skGlobal);
    Parameters := TStringList.Create;

    FunctionValue := CreateSimpleFunction<T>('test', Parameters, Scope);

    // Test basic function properties
    Expect<Boolean>(FunctionValue.ToBooleanLiteral.Value).ToBe(True);
    Expect<string>(FunctionValue.ToStringLiteral.Value).ToBe('[function Object]');
    Expect<Boolean>(FunctionValue.ToNumberLiteral.IsNaN).ToBe(True);
    Expect<string>(FunctionValue.TypeName).ToBe('function');

    // Test function metadata
    Expect<string>(FunctionValue.Name).ToBe('test');
    Expect<Integer>(Length(FunctionValue.Parameters)).ToBe(0);
    Expect<Boolean>(FunctionValue.Closure = Scope).ToBe(True);

    // Test function execution
    Args := TGocciaArgumentsCollection.Create;
    try
      ReturnValue := FunctionValue.Call(Args, TGocciaUndefinedLiteralValue.UndefinedValue);
      Expect<Boolean>(ReturnValue.ToBooleanLiteral.Value).ToBe(False);
      Expect<string>(ReturnValue.ToStringLiteral.Value).ToBe('undefined');
      Expect<Boolean>(ReturnValue.ToNumberLiteral.IsNaN).ToBe(True);
      Expect<string>(ReturnValue.TypeName).ToBe('undefined');
    finally
      Args.Free;
    end;

    Parameters.Free;
    Scope.Free;
  end;

  procedure TestFunctionWithParametersFromTemplate<T>;
  var
    FunctionValue: T;
    Scope: TGocciaScope;
    Parameters: TStringList;
    ReturnValue: TGocciaValue;
    Statements: TObjectList<TGocciaASTNode>;
    Args: TGocciaArgumentsCollection;
    ParamArray: TGocciaParameterArray;
    I: Integer;
  begin
    Scope := TGocciaScope.Create(nil, skGlobal);
    Parameters := TStringList.Create;
    Parameters.Add('a');
    Parameters.Add('b');

    Statements := TObjectList<TGocciaASTNode>.Create;
    Statements.Add(TGocciaBinaryExpression.Create(
      TGocciaIdentifierExpression.Create('a', 0, 0),
      gttPlus,
      TGocciaIdentifierExpression.Create('b', 0, 0),
      0, 0
    ));

    SetLength(ParamArray, Parameters.Count);
    for I := 0 to Parameters.Count - 1 do
    begin
      ParamArray[I].Name := Parameters[I];
      ParamArray[I].DefaultValue := nil;
    end;

    FunctionValue := T.Create(ParamArray, Statements, Scope, 'test');

    // Test function properties
    Expect<string>(FunctionValue.Name).ToBe('test');
    Expect<Integer>(Length(FunctionValue.Parameters)).ToBe(2);
    Expect<string>(FunctionValue.Parameters[0].Name).ToBe('a');
    Expect<string>(FunctionValue.Parameters[1].Name).ToBe('b');

    // Test function execution with arguments
    Args := TGocciaArgumentsCollection.Create;
    try
      Args.Add(TGocciaNumberLiteralValue.Create(1));
      Args.Add(TGocciaNumberLiteralValue.Create(2));
      ReturnValue := FunctionValue.Call(Args, TGocciaUndefinedLiteralValue.UndefinedValue);
      Expect<Double>(ReturnValue.ToNumberLiteral.Value).ToBe(3);
    finally
      Args.Free;
    end;

    // Test function execution with too few arguments
    Args := TGocciaArgumentsCollection.Create;
    try
      Args.Add(TGocciaNumberLiteralValue.Create(1));
      ReturnValue := FunctionValue.Call(Args, TGocciaUndefinedLiteralValue.UndefinedValue);
      Expect<Boolean>(ReturnValue.ToNumberLiteral.IsNaN).ToBe(True);
      Expect<string>(ReturnValue.TypeName).ToBe('number');
    finally
      Args.Free;
    end;

    // Test function execution with too many arguments
    Args := TGocciaArgumentsCollection.Create;
    try
      Args.Add(TGocciaNumberLiteralValue.Create(1));
      Args.Add(TGocciaNumberLiteralValue.Create(2));
      Args.Add(TGocciaNumberLiteralValue.Create(3));
      ReturnValue := FunctionValue.Call(Args, TGocciaUndefinedLiteralValue.UndefinedValue);
      Expect<Double>(ReturnValue.ToNumberLiteral.Value).ToBe(3);
    finally
      Args.Free;
    end;

    Parameters.Free;
    Scope.Free;
  end;

  procedure TestFunctionWithReturnFromTemplate<T>;
  var
    FunctionValue: T;
    Scope: TGocciaScope;
    Parameters: TStringList;
    ReturnValue: TGocciaValue;
    Statements: TObjectList<TGocciaASTNode>;
    Args: TGocciaArgumentsCollection;
    ParamArray: TGocciaParameterArray;
    I: Integer;
  begin
    Scope := TGocciaScope.Create(nil, skGlobal);
    Parameters := TStringList.Create;
    Parameters.Add('a');
    Parameters.Add('b');

    SetLength(ParamArray, Parameters.Count);
    for I := 0 to Parameters.Count - 1 do
    begin
      ParamArray[I].Name := Parameters[I];
      ParamArray[I].DefaultValue := nil;
    end;

    // Test returning a number
    Statements := TObjectList<TGocciaASTNode>.Create;
    Statements.Add(TGocciaReturnStatement.Create(
      TGocciaLiteralExpression.Create(TGocciaNumberLiteralValue.Create(1), 0, 0),
      0, 0
    ));
    FunctionValue := T.Create(ParamArray, Statements, Scope, 'test');

    Args := TGocciaArgumentsCollection.Create;
    try
      ReturnValue := FunctionValue.Call(Args, TGocciaUndefinedLiteralValue.UndefinedValue);
      Expect<Double>(ReturnValue.ToNumberLiteral.Value).ToBe(1);
      Expect<string>(ReturnValue.ToStringLiteral.Value).ToBe('1');
      Expect<Boolean>(ReturnValue.ToBooleanLiteral.Value).ToBe(True);
      Expect<string>(ReturnValue.TypeName).ToBe('number');
    finally
      Args.Free;
    end;

    // Test returning a string
    Statements := TObjectList<TGocciaASTNode>.Create;
    Statements.Add(TGocciaReturnStatement.Create(
      TGocciaLiteralExpression.Create(TGocciaStringLiteralValue.Create('hello'), 0, 0),
      0, 0
    ));
    FunctionValue := T.Create(ParamArray, Statements, Scope, 'test');

    Args := TGocciaArgumentsCollection.Create;
    try
      ReturnValue := FunctionValue.Call(Args, TGocciaUndefinedLiteralValue.UndefinedValue);
      Expect<string>(ReturnValue.ToStringLiteral.Value).ToBe('hello');
      Expect<Boolean>(ReturnValue.ToBooleanLiteral.Value).ToBe(True);
      Expect<Boolean>(ReturnValue.ToNumberLiteral.IsNaN).ToBe(True);
      Expect<string>(ReturnValue.TypeName).ToBe('string');
    finally
      Args.Free;
    end;

    // Test returning a boolean
    Statements := TObjectList<TGocciaASTNode>.Create;
    Statements.Add(TGocciaReturnStatement.Create(
      TGocciaLiteralExpression.Create(TGocciaBooleanLiteralValue.Create(True), 0, 0),
      0, 0
    ));
    FunctionValue := T.Create(ParamArray, Statements, Scope, 'test');

    Args := TGocciaArgumentsCollection.Create;
    try
      ReturnValue := FunctionValue.Call(Args, TGocciaUndefinedLiteralValue.UndefinedValue);
      Expect<Boolean>(ReturnValue.ToBooleanLiteral.Value).ToBe(True);
      Expect<string>(ReturnValue.ToStringLiteral.Value).ToBe('true');
      Expect<Double>(ReturnValue.ToNumberLiteral.Value).ToBe(1);
      Expect<string>(ReturnValue.TypeName).ToBe('boolean');
    finally
      Args.Free;
    end;

    // Test returning undefined
    Statements := TObjectList<TGocciaASTNode>.Create;
    Statements.Add(TGocciaReturnStatement.Create(
      TGocciaLiteralExpression.Create(TGocciaUndefinedLiteralValue.UndefinedValue, 0, 0),
      0, 0
    ));
    FunctionValue := T.Create(ParamArray, Statements, Scope, 'test');

    Args := TGocciaArgumentsCollection.Create;
    try
      ReturnValue := FunctionValue.Call(Args, TGocciaUndefinedLiteralValue.UndefinedValue);
      Expect<Boolean>(ReturnValue.ToBooleanLiteral.Value).ToBe(False);
      Expect<string>(ReturnValue.ToStringLiteral.Value).ToBe('undefined');
      Expect<Boolean>(ReturnValue.ToNumberLiteral.IsNaN).ToBe(True);
      Expect<string>(ReturnValue.TypeName).ToBe('undefined');
    finally
      Args.Free;
    end;

    Parameters.Free;
    Scope.Free;
  end;

  procedure TestFunctionWithScopeFromTemplate<T>;
  var
    FunctionValue: T;
    Scope: TGocciaScope;
    Parameters: TStringList;
    ReturnValue: TGocciaValue;
    Statements: TObjectList<TGocciaASTNode>;
    Args: TGocciaArgumentsCollection;
    ParamArray: TGocciaParameterArray;
    I: Integer;
  begin
    Scope := TGocciaScope.Create(nil, skGlobal);
    Scope.DefineLexicalBinding('x', TGocciaNumberLiteralValue.Create(42), dtLet);
    Parameters := TStringList.Create;

    Statements := TObjectList<TGocciaASTNode>.Create;
    Statements.Add(TGocciaReturnStatement.Create(
      TGocciaIdentifierExpression.Create('x', 0, 0),
      0, 0
    ));

    SetLength(ParamArray, Parameters.Count);
    for I := 0 to Parameters.Count - 1 do
    begin
      ParamArray[I].Name := Parameters[I];
      ParamArray[I].DefaultValue := nil;
    end;

    FunctionValue := T.Create(ParamArray, Statements, Scope, 'test');

    Args := TGocciaArgumentsCollection.Create;
    try
      ReturnValue := FunctionValue.Call(Args, TGocciaUndefinedLiteralValue.UndefinedValue);
      Expect<Double>(ReturnValue.ToNumberLiteral.Value).ToBe(42);
      Expect<string>(ReturnValue.ToStringLiteral.Value).ToBe('42');
      Expect<Boolean>(ReturnValue.ToBooleanLiteral.Value).ToBe(True);
      Expect<string>(ReturnValue.TypeName).ToBe('number');
    finally
      Args.Free;
    end;

    Parameters.Free;
    Scope.Free;
  end;

  procedure TestFunctionWithScopeAndParametersFromTemplate<T>;
  var
    FunctionValue: T;
    Scope: TGocciaScope;
    Parameters: TStringList;
    ReturnValue: TGocciaValue;
    Statements: TObjectList<TGocciaASTNode>;
    Args: TGocciaArgumentsCollection;
    ParamArray: TGocciaParameterArray;
    I: Integer;
  begin
    Scope := TGocciaScope.Create(nil, skGlobal);
    Scope.DefineLexicalBinding('x', TGocciaNumberLiteralValue.Create(42), dtLet);
    Parameters := TStringList.Create;
    Parameters.Add('x'); // Parameter shadows global variable

    Statements := TObjectList<TGocciaASTNode>.Create;
    Statements.Add(TGocciaReturnStatement.Create(
      TGocciaBinaryExpression.Create(
        TGocciaIdentifierExpression.Create('x', 0, 0),
        gttPlus,
        TGocciaIdentifierExpression.Create('x', 0, 0),
        0, 0
      ),
      0, 0
    ));

    SetLength(ParamArray, Parameters.Count);
    for I := 0 to Parameters.Count - 1 do
    begin
      ParamArray[I].Name := Parameters[I];
      ParamArray[I].DefaultValue := nil;
    end;

    FunctionValue := T.Create(ParamArray, Statements, Scope, 'test');

    Args := TGocciaArgumentsCollection.Create;
    try
      Args.Add(TGocciaNumberLiteralValue.Create(10));
      ReturnValue := FunctionValue.Call(Args, TGocciaUndefinedLiteralValue.UndefinedValue);
      Expect<Double>(ReturnValue.ToNumberLiteral.Value).ToBe(20); // 10 + 10
    finally
      Args.Free;
    end;

    Parameters.Free;
    Scope.Free;
  end;

  procedure TestFunctionWithScopeAndReturnFromTemplate<T>;
  var
    FunctionValue: T;
    Scope: TGocciaScope;
    Parameters: TStringList;
    ReturnValue: TGocciaValue;
    Statements: TObjectList<TGocciaASTNode>;
    Args: TGocciaArgumentsCollection;
    ParamArray: TGocciaParameterArray;
    I: Integer;
  begin
    Scope := TGocciaScope.Create(nil, skGlobal);
    Scope.DefineLexicalBinding('x', TGocciaNumberLiteralValue.Create(42), dtLet);
    Scope.DefineLexicalBinding('y', TGocciaStringLiteralValue.Create('hello'), dtLet);
    Parameters := TStringList.Create;

    Statements := TObjectList<TGocciaASTNode>.Create;
    Statements.Add(TGocciaReturnStatement.Create(
      TGocciaBinaryExpression.Create(
        TGocciaBinaryExpression.Create(
          TGocciaIdentifierExpression.Create('x', 0, 0),
          gttPlus,
          TGocciaLiteralExpression.Create(TGocciaNumberLiteralValue.Create(1), 0, 0),
          0, 0
        ),
        gttPlus,
        TGocciaIdentifierExpression.Create('y', 0, 0),
        0, 0
      ),
      0, 0
    ));

    SetLength(ParamArray, Parameters.Count);
    for I := 0 to Parameters.Count - 1 do
    begin
      ParamArray[I].Name := Parameters[I];
      ParamArray[I].DefaultValue := nil;
    end;

    FunctionValue := T.Create(ParamArray, Statements, Scope, 'test');

    Args := TGocciaArgumentsCollection.Create;
    try
      ReturnValue := FunctionValue.Call(Args, TGocciaUndefinedLiteralValue.UndefinedValue);
      Expect<string>(ReturnValue.ToStringLiteral.Value).ToBe('43hello');
      Expect<Boolean>(ReturnValue.ToBooleanLiteral.Value).ToBe(True);
      Expect<Boolean>(ReturnValue.ToNumberLiteral.IsNaN).ToBe(True);
      Expect<string>(ReturnValue.TypeName).ToBe('string');
    finally
      Args.Free;
    end;

    Parameters.Free;
    Scope.Free;
  end;

  procedure TestFunctionWithScopeAndParametersAndReturnFromTemplate<T>;
  var
    FunctionValue: T;
    Scope: TGocciaScope;
    Parameters: TStringList;
    ReturnValue: TGocciaValue;
    Statements: TObjectList<TGocciaASTNode>;
    Args: TGocciaArgumentsCollection;
    ParamArray: TGocciaParameterArray;
    I: Integer;
  begin
    Scope := TGocciaScope.Create(nil, skGlobal);
    Scope.DefineLexicalBinding('x', TGocciaNumberLiteralValue.Create(42), dtLet);
    Scope.DefineLexicalBinding('y', TGocciaStringLiteralValue.Create('hello'), dtLet);
    Parameters := TStringList.Create;
    Parameters.Add('a');
    Parameters.Add('b');

    Statements := TObjectList<TGocciaASTNode>.Create;
    Statements.Add(TGocciaReturnStatement.Create(
      TGocciaBinaryExpression.Create(
        TGocciaBinaryExpression.Create(
          TGocciaBinaryExpression.Create(
            TGocciaIdentifierExpression.Create('a', 0, 0),
            gttPlus,
            TGocciaIdentifierExpression.Create('b', 0, 0),
            0, 0
          ),
          gttPlus,
          TGocciaIdentifierExpression.Create('x', 0, 0),
          0, 0
        ),
        gttPlus,
        TGocciaIdentifierExpression.Create('y', 0, 0),
        0, 0
      ),
      0, 0
    ));

    SetLength(ParamArray, Parameters.Count);
    for I := 0 to Parameters.Count - 1 do
    begin
      ParamArray[I].Name := Parameters[I];
      ParamArray[I].DefaultValue := nil;
    end;

    FunctionValue := T.Create(ParamArray, Statements, Scope, 'test');

    Args := TGocciaArgumentsCollection.Create;
    try
      Args.Add(TGocciaNumberLiteralValue.Create(10));
      Args.Add(TGocciaNumberLiteralValue.Create(20));
      ReturnValue := FunctionValue.Call(Args, TGocciaUndefinedLiteralValue.UndefinedValue);
      Expect<string>(ReturnValue.ToStringLiteral.Value).ToBe('72hello'); // (10 + 20 + 42) + 'hello'
    finally
      Args.Free;
    end;

    Parameters.Free;
    Scope.Free;
  end;

  procedure TTestBlockValue.TestSimpleBlock;
  var
    Statements: TObjectList<TGocciaASTNode>;
    Scope: TGocciaScope;
    Context: TGocciaEvaluationContext;
    I: Integer;
    LastValue: TGocciaValue;
  begin
    Statements := TObjectList<TGocciaASTNode>.Create;
    Scope := TGocciaScope.Create(nil, skGlobal);

    Statements.Add(TGocciaLiteralExpression.Create(TGocciaNumberLiteralValue.Create(1), 0, 0));

    Context.Scope := Scope;
    LastValue := TGocciaUndefinedLiteralValue.UndefinedValue;

    for I := 0 to Statements.Count - 1 do
      LastValue := Evaluate(Statements[I], Context);

    Expect<Double>(LastValue.ToNumberLiteral.Value).ToBe(1);

    Statements.Free;
    Scope.Free;
  end;

  procedure TTestBlockValue.TestArithmeticBlock;
  var
    Statements: TObjectList<TGocciaASTNode>;
    Scope: TGocciaScope;
    Context: TGocciaEvaluationContext;
    I: Integer;
    LastValue: TGocciaValue;
  begin
    Statements := TObjectList<TGocciaASTNode>.Create;
    Scope := TGocciaScope.Create(nil, skGlobal);

    Statements.Add(TGocciaBinaryExpression.Create(
      TGocciaLiteralExpression.Create(TGocciaNumberLiteralValue.Create(1), 0, 0),
      gttPlus,
      TGocciaLiteralExpression.Create(TGocciaNumberLiteralValue.Create(2), 0, 0),
      0, 0
    ));

    Context.Scope := Scope;
    LastValue := TGocciaUndefinedLiteralValue.UndefinedValue;

    for I := 0 to Statements.Count - 1 do
      LastValue := Evaluate(Statements[I], Context);

    Expect<Double>(LastValue.ToNumberLiteral.Value).ToBe(3);

    Statements.Free;
    Scope.Free;
  end;

  procedure TTestBlockValue.SetupTests;
  begin
    Test('Simple Block', TestSimpleBlock);
    Test('Arithmetic Block', TestArithmeticBlock);
  end;

  procedure TTestFunctionValue.SetupTests;
  begin
    Test('Simple Function', TestSimpleFunction);
    Test('Function With Parameters', TestFunctionWithParameters);
    Test('Function With Return', TestFunctionWithReturn);
    Test('Function With Scope', TestFunctionWithScope);
    Test('Function With Scope And Parameters', TestFunctionWithScopeAndParameters);
    Test('Function With Scope And Return', TestFunctionWithScopeAndReturn);
    Test('Function With Scope And Parameters And Return', TestFunctionWithScopeAndParametersAndReturn);
  end;

  procedure TTestFunctionValue.TestSimpleFunction;
  begin
    TestSimpleFunctionFromTemplate<TGocciaFunctionValue>;
  end;

  procedure TTestFunctionValue.TestFunctionWithParameters;
  begin
    TestFunctionWithParametersFromTemplate<TGocciaFunctionValue>;
  end;

  procedure TTestFunctionValue.TestFunctionWithReturn;
  begin
    TestFunctionWithReturnFromTemplate<TGocciaFunctionValue>;
  end;

  procedure TTestFunctionValue.TestFunctionWithScope;
  begin
    TestFunctionWithScopeFromTemplate<TGocciaFunctionValue>;
  end;

  procedure TTestFunctionValue.TestFunctionWithScopeAndParameters;
  begin
    TestFunctionWithScopeAndParametersFromTemplate<TGocciaFunctionValue>;
  end;

  procedure TTestFunctionValue.TestFunctionWithScopeAndReturn;
  begin
    TestFunctionWithScopeAndReturnFromTemplate<TGocciaFunctionValue>;
  end;

  procedure TTestFunctionValue.TestFunctionWithScopeAndParametersAndReturn;
  begin
    TestFunctionWithScopeAndParametersAndReturnFromTemplate<TGocciaFunctionValue>;
  end;

  procedure TTestMethodValue.SetupTests;
  begin
    Test('Simple Method', TestSimpleMethod);
    Test('Method With Parameters', TestMethodWithParameters);
    Test('Method With Return', TestMethodWithReturn);
    Test('Method With Scope', TestMethodWithScope);
    Test('Method With Scope And Parameters', TestMethodWithScopeAndParameters);
    Test('Method With Scope And Return', TestMethodWithScopeAndReturn);
    Test('Method With Scope And Parameters And Return', TestMethodWithScopeAndParametersAndReturn);
  end;

  procedure TTestMethodValue.TestSimpleMethod;
  begin
    TestSimpleFunctionFromTemplate<TGocciaMethodValue>;
  end;

  procedure TTestMethodValue.TestMethodWithParameters;
  begin
    TestFunctionWithParametersFromTemplate<TGocciaMethodValue>;
  end;

  procedure TTestMethodValue.TestMethodWithReturn;
  begin
    TestFunctionWithReturnFromTemplate<TGocciaMethodValue>;
  end;

  procedure TTestMethodValue.TestMethodWithScope;
  begin
    TestFunctionWithScopeFromTemplate<TGocciaMethodValue>;
  end;

  procedure TTestMethodValue.TestMethodWithScopeAndParameters;
  begin
    TestFunctionWithScopeAndParametersFromTemplate<TGocciaMethodValue>;
  end;

  procedure TTestMethodValue.TestMethodWithScopeAndReturn;
  begin
    TestFunctionWithScopeAndReturnFromTemplate<TGocciaMethodValue>;
  end;

  procedure TTestMethodValue.TestMethodWithScopeAndParametersAndReturn;
  begin
    TestFunctionWithScopeAndParametersAndReturnFromTemplate<TGocciaMethodValue>;
  end;

begin
  TestRunnerProgram.AddSuite(TTestBlockValue.Create('Block Value'));
  TestRunnerProgram.AddSuite(TTestFunctionValue.Create('Function Value'));
  TestRunnerProgram.AddSuite(TTestMethodValue.Create('Method Value'));
  TestRunnerProgram.Run;

  ExitCode := TestResultToExitCode;
end.
