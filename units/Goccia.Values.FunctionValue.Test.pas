program Goccia.Values.FunctionValue.Test;

{$I Goccia.inc}

uses
  Goccia.Values.FunctionValue, Goccia.Values.ObjectValue, Goccia.Values.StringValue, Goccia.Values.NumberValue, Goccia.Values.BooleanValue, StrUtils, Math, TestRunner, Goccia.AST.Node, Generics.Collections, Goccia.AST.Statements, Goccia.AST.Expressions, Goccia.Scope, Goccia.Values.Base, Goccia.Token, SysUtils, Classes, Goccia.Values.UndefinedValue, Goccia.Values.NullValue, Goccia.Evaluator;

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
    Statements.Add(TGocciaLiteralExpression.Create(TGocciaUndefinedValue.Create, 0, 0));

    // Convert TStringList to TGocciaParameterArray
    SetLength(ParamArray, AParameters.Count);
    for I := 0 to AParameters.Count - 1 do
    begin
      ParamArray[I].Name := AParameters[I];
      ParamArray[I].DefaultValue := nil;
    end;

    Result := T.Create(ParamArray, Statements, AScope, AName);
  end;

  procedure TestSimpleFunctionFromTemplate<T>(TestSuite: TTestSuite; const ATypeDisplayName: String);
  var
    FunctionValue: T;
    Scope: TGocciaScope;
    Parameters: TStringList;
    ReturnValue: TGocciaValue;
    Statements: TObjectList<TGocciaASTNode>;
  begin
    Scope := TGocciaScope.Create(nil, skGlobal);
    Parameters := TStringList.Create;

    WriteLn('Parameters: ' + IntToStr(Parameters.Count));

    FunctionValue := CreateSimpleFunction<T>('test', Parameters, Scope);

    WriteLn('Parameters from FunctionValue: ' + IntToStr(Length(FunctionValue.Parameters)));

    // Test basic function properties
    TestSuite.Expect<Boolean>(FunctionValue.ToBoolean).ToBeTrue;
    TestSuite.Expect<String>(FunctionValue.ToString).ToBe('[' + ATypeDisplayName + ': test]');
    TestSuite.Expect<Boolean>(IsNaN(FunctionValue.ToNumber)).ToBeTrue;
    TestSuite.Expect<String>(FunctionValue.TypeName).ToBe('function');

    // Test function metadata
    TestSuite.Expect<String>(FunctionValue.Name).ToBe('test');
    TestSuite.Expect<Integer>(Length(FunctionValue.Parameters)).ToBe(0);
    TestSuite.Expect<Boolean>(FunctionValue.Closure = Scope).ToBeTrue;

    // Test function execution
    ReturnValue := FunctionValue.Call(TObjectList<TGocciaValue>.Create, TGocciaUndefinedValue.Create);
    TestSuite.Expect<Boolean>(ReturnValue.ToBoolean).ToBeFalse;
    TestSuite.Expect<String>(ReturnValue.ToString).ToBe('undefined');
    TestSuite.Expect<Boolean>(IsNaN(ReturnValue.ToNumber)).ToBeTrue;
    TestSuite.Expect<String>(ReturnValue.TypeName).ToBe('undefined');

    FunctionValue.Free;
    Scope.Free;
  end;

  procedure TestFunctionWithParametersFromTemplate<T>(TestSuite: TTestSuite);
  var
    FunctionValue: T;
    Scope: TGocciaScope;
    Parameters: TStringList;
    ReturnValue: TGocciaValue;
    Statements: TObjectList<TGocciaASTNode>;
    Arguments: TObjectList<TGocciaValue>;
    ParamArray: TGocciaParameterArray;
    I: Integer;
  begin
    Scope := TGocciaScope.Create(nil, skGlobal);
    Parameters := TStringList.Create;
    Parameters.Add('a');
    Parameters.Add('b');

    // Create a function body that uses the parameters
    Statements := TObjectList<TGocciaASTNode>.Create;
    Statements.Add(TGocciaBinaryExpression.Create(
      TGocciaIdentifierExpression.Create('a', 0, 0),
      gttPlus,
      TGocciaIdentifierExpression.Create('b', 0, 0),
      0, 0
    ));

    // Convert TStringList to TGocciaParameterArray
    SetLength(ParamArray, Parameters.Count);
    for I := 0 to Parameters.Count - 1 do
    begin
      ParamArray[I].Name := Parameters[I];
      ParamArray[I].DefaultValue := nil;
    end;

    FunctionValue := T.Create(ParamArray, Statements, Scope, 'test');

    // Test function properties
    TestSuite.Expect<String>(FunctionValue.Name).ToBe('test');
    TestSuite.Expect<Integer>(Length(FunctionValue.Parameters)).ToBe(2);
    TestSuite.Expect<String>(FunctionValue.Parameters[0].Name).ToBe('a');
    TestSuite.Expect<String>(FunctionValue.Parameters[1].Name).ToBe('b');

    // Test function execution with arguments
    Arguments := TObjectList<TGocciaValue>.Create;
    Arguments.Add(TGocciaNumberValue.Create(1));
    Arguments.Add(TGocciaNumberValue.Create(2));
    ReturnValue := FunctionValue.Call(Arguments, TGocciaUndefinedValue.Create);
    TestSuite.Expect<Double>(ReturnValue.ToNumber).ToBe(3);
    Arguments.Free;

    // Test function execution with too few arguments
    Arguments := TObjectList<TGocciaValue>.Create;
    Arguments.Add(TGocciaNumberValue.Create(1));
    ReturnValue := FunctionValue.Call(Arguments, TGocciaUndefinedValue.Create);
    TestSuite.Expect<Boolean>(IsNaN(ReturnValue.ToNumber)).ToBeTrue;
    TestSuite.Expect<String>(ReturnValue.TypeName).ToBe('number');
    Arguments.Free;

    // Test function execution with too many arguments
    Arguments := TObjectList<TGocciaValue>.Create;
    Arguments.Add(TGocciaNumberValue.Create(1));
    Arguments.Add(TGocciaNumberValue.Create(2));
    Arguments.Add(TGocciaNumberValue.Create(3));
    ReturnValue := FunctionValue.Call(Arguments, TGocciaUndefinedValue.Create);
    TestSuite.Expect<Double>(ReturnValue.ToNumber).ToBe(3);
    Arguments.Free;

    FunctionValue.Free;
    Scope.Free;
  end;

  procedure TestFunctionWithReturnFromTemplate<T>(TestSuite: TTestSuite);
  var
    FunctionValue: T;
    Scope: TGocciaScope;
    Parameters: TStringList;
    ReturnValue: TGocciaValue;
    Statements: TObjectList<TGocciaASTNode>;
    ParamArray: TGocciaParameterArray;
    I: Integer;
  begin
    Scope := TGocciaScope.Create(nil, skGlobal);
    Parameters := TStringList.Create;
    Parameters.Add('a');
    Parameters.Add('b');

    // Test returning a number
    Statements := TObjectList<TGocciaASTNode>.Create;
    Statements.Add(TGocciaReturnStatement.Create(
      TGocciaLiteralExpression.Create(TGocciaNumberValue.Create(1), 0, 0),
      0, 0
    ));

    // Convert TStringList to TGocciaParameterArray
    SetLength(ParamArray, Parameters.Count);
    for I := 0 to Parameters.Count - 1 do
    begin
      ParamArray[I].Name := Parameters[I];
      ParamArray[I].DefaultValue := nil;
    end;

    FunctionValue := T.Create(ParamArray, Statements, Scope, 'test');

    ReturnValue := FunctionValue.Call(TObjectList<TGocciaValue>.Create, TGocciaUndefinedValue.Create);
    TestSuite.Expect<Double>(ReturnValue.ToNumber).ToBe(1);
    TestSuite.Expect<String>(ReturnValue.ToString).ToBe('1');
    TestSuite.Expect<Boolean>(ReturnValue.ToBoolean).ToBeTrue;
    TestSuite.Expect<String>(ReturnValue.TypeName).ToBe('number');

    // Test returning a string
    Statements := TObjectList<TGocciaASTNode>.Create;
    Statements.Add(TGocciaReturnStatement.Create(
      TGocciaLiteralExpression.Create(TGocciaStringValue.Create('hello'), 0, 0),
      0, 0
    ));
    FunctionValue := T.Create(ParamArray, Statements, Scope, 'test');

    ReturnValue := FunctionValue.Call(TObjectList<TGocciaValue>.Create, TGocciaUndefinedValue.Create);
    TestSuite.Expect<String>(ReturnValue.ToString).ToBe('hello');
    TestSuite.Expect<Boolean>(ReturnValue.ToBoolean).ToBeTrue;
    TestSuite.Expect<Boolean>(IsNaN(ReturnValue.ToNumber)).ToBeTrue;
    TestSuite.Expect<String>(ReturnValue.TypeName).ToBe('string');

    // Test returning a boolean
    Statements := TObjectList<TGocciaASTNode>.Create;
    Statements.Add(TGocciaReturnStatement.Create(
      TGocciaLiteralExpression.Create(TGocciaBooleanValue.Create(True), 0, 0),
      0, 0
    ));
    FunctionValue := T.Create(ParamArray, Statements, Scope, 'test');

    ReturnValue := FunctionValue.Call(TObjectList<TGocciaValue>.Create, TGocciaUndefinedValue.Create);
    TestSuite.Expect<Boolean>(ReturnValue.ToBoolean).ToBe(True);
    TestSuite.Expect<String>(ReturnValue.ToString).ToBe('true');
    TestSuite.Expect<Double>(ReturnValue.ToNumber).ToBe(1);
    TestSuite.Expect<String>(ReturnValue.TypeName).ToBe('boolean');

    // Test returning undefined
    Statements := TObjectList<TGocciaASTNode>.Create;
    Statements.Add(TGocciaReturnStatement.Create(
      TGocciaLiteralExpression.Create(TGocciaUndefinedValue.Create, 0, 0),
      0, 0
    ));
    FunctionValue := T.Create(ParamArray, Statements, Scope, 'test');

    ReturnValue := FunctionValue.Call(TObjectList<TGocciaValue>.Create, TGocciaUndefinedValue.Create);
    TestSuite.Expect<Boolean>(ReturnValue.ToBoolean).ToBeFalse;
    TestSuite.Expect<String>(ReturnValue.ToString).ToBe('undefined');
    TestSuite.Expect<Boolean>(IsNaN(ReturnValue.ToNumber)).ToBeTrue;
    TestSuite.Expect<String>(ReturnValue.TypeName).ToBe('undefined');

    Parameters.Free;
    Scope.Free;
  end;

  procedure TestFunctionWithScopeFromTemplate<T>(TestSuite: TTestSuite);
  var
    FunctionValue: T;
    Scope: TGocciaScope;
    Parameters: TStringList;
    ReturnValue: TGocciaValue;
    Statements: TObjectList<TGocciaASTNode>;
    ParamArray: TGocciaParameterArray;
    I: Integer;
  begin
    // Create a global scope with a variable
    Scope := TGocciaScope.Create(nil, skGlobal);
    Scope.SetValue('x', TGocciaNumberValue.Create(42));
    Parameters := TStringList.Create;

    // Create a function body that uses the scope variable
    Statements := TObjectList<TGocciaASTNode>.Create;
    Statements.Add(TGocciaReturnStatement.Create(
      TGocciaIdentifierExpression.Create('x', 0, 0),
      0, 0
    ));

    // Convert TStringList to TGocciaParameterArray
    SetLength(ParamArray, Parameters.Count);
    for I := 0 to Parameters.Count - 1 do
    begin
      ParamArray[I].Name := Parameters[I];
      ParamArray[I].DefaultValue := nil;
    end;

    FunctionValue := T.Create(ParamArray, Statements, Scope, 'test');

    // Test function execution
    ReturnValue := FunctionValue.Call(TObjectList<TGocciaValue>.Create, TGocciaUndefinedValue.Create);
    TestSuite.Expect<Double>(ReturnValue.ToNumber).ToBe(42);
    TestSuite.Expect<String>(ReturnValue.ToString).ToBe('42');
    TestSuite.Expect<Boolean>(ReturnValue.ToBoolean).ToBe(True);
    TestSuite.Expect<String>(ReturnValue.TypeName).ToBe('number');

    FunctionValue.Free;
    Scope.Free;
  end;

  procedure TestFunctionWithScopeAndParametersFromTemplate<T>(TestSuite: TTestSuite);
  var
    FunctionValue: T;
    Scope: TGocciaScope;
    Parameters: TStringList;
    ReturnValue: TGocciaValue;
    Statements: TObjectList<TGocciaASTNode>;
    Arguments: TObjectList<TGocciaValue>;
    ParamArray: TGocciaParameterArray;
    I: Integer;
  begin
    // Create a global scope with a variable
    Scope := TGocciaScope.Create(nil, skGlobal);
    Scope.SetValue('x', TGocciaNumberValue.Create(42));
    Parameters := TStringList.Create;
    Parameters.Add('x'); // Parameter shadows global variable

    // Create a function body that uses both parameter and scope variable
    Statements := TObjectList<TGocciaASTNode>.Create;
    Statements.Add(TGocciaReturnStatement.Create(
      TGocciaBinaryExpression.Create(
        TGocciaIdentifierExpression.Create('x', 0, 0), // Parameter x
        gttPlus,
        TGocciaIdentifierExpression.Create('x', 0, 0), // Global x
        0, 0
      ),
      0, 0
    ));

    // Convert TStringList to TGocciaParameterArray
    SetLength(ParamArray, Parameters.Count);
    for I := 0 to Parameters.Count - 1 do
    begin
      ParamArray[I].Name := Parameters[I];
      ParamArray[I].DefaultValue := nil;
    end;

    FunctionValue := T.Create(ParamArray, Statements, Scope, 'test');

    // Test function execution
    Arguments := TObjectList<TGocciaValue>.Create;
    Arguments.Add(TGocciaNumberValue.Create(10));
    ReturnValue := FunctionValue.Call(Arguments, TGocciaUndefinedValue.Create);
    TestSuite.Expect<Double>(ReturnValue.ToNumber).ToBe(20); // 10 + 10
    Arguments.Free;

    FunctionValue.Free;
    Scope.Free;
  end;

  procedure TestFunctionWithScopeAndReturnFromTemplate<T>(TestSuite: TTestSuite);
  var
    FunctionValue: T;
    Scope: TGocciaScope;
    Parameters: TStringList;
    ReturnValue: TGocciaValue;
    Arguments: TObjectList<TGocciaValue>;
    Statements: TObjectList<TGocciaASTNode>;
    ParamArray: TGocciaParameterArray;
    I: Integer;
  begin
    // Create a global scope with variables
    Scope := TGocciaScope.Create(nil, skGlobal);
    Scope.SetValue('x', TGocciaNumberValue.Create(42));
    Scope.SetValue('y', TGocciaStringValue.Create('hello'));
    Parameters := TStringList.Create;

    // Create a function body that returns a complex expression using scope variables
    Statements := TObjectList<TGocciaASTNode>.Create;
    Statements.Add(TGocciaReturnStatement.Create(
      TGocciaBinaryExpression.Create(
        TGocciaBinaryExpression.Create(
          TGocciaIdentifierExpression.Create('x', 0, 0),
          gttPlus,
          TGocciaLiteralExpression.Create(TGocciaNumberValue.Create(1), 0, 0),
          0, 0
        ),
        gttPlus,
        TGocciaIdentifierExpression.Create('y', 0, 0),
        0, 0
      ),
      0, 0
    ));

    // Convert TStringList to TGocciaParameterArray
    SetLength(ParamArray, Parameters.Count);
    for I := 0 to Parameters.Count - 1 do
    begin
      ParamArray[I].Name := Parameters[I];
      ParamArray[I].DefaultValue := nil;
    end;

    FunctionValue := T.Create(ParamArray, Statements, Scope, 'test');

    // Test function execution
    Arguments := TObjectList<TGocciaValue>.Create;
    ReturnValue := FunctionValue.Call(Arguments, TGocciaUndefinedValue.Create);
    TestSuite.Expect<String>(ReturnValue.ToString).ToBe('43hello');
    TestSuite.Expect<Boolean>(ReturnValue.ToBoolean).ToBe(True);
    TestSuite.Expect<Boolean>(IsNaN(ReturnValue.ToNumber)).ToBeTrue;
    TestSuite.Expect<String>(ReturnValue.TypeName).ToBe('string');
    Arguments.Free;

    FunctionValue.Free;
    Scope.Free;
  end;

  procedure TestFunctionWithScopeAndParametersAndReturnFromTemplate<T>(TestSuite: TTestSuite);
  var
    FunctionValue: T;
    Scope: TGocciaScope;
    Parameters: TStringList;
    ReturnValue: TGocciaValue;
    Statements: TObjectList<TGocciaASTNode>;
    Arguments: TObjectList<TGocciaValue>;
    ParamArray: TGocciaParameterArray;
    I: Integer;
  begin
    // Create a global scope with variables
    Scope := TGocciaScope.Create(nil, skGlobal);
    Scope.SetValue('x', TGocciaNumberValue.Create(42));
    Scope.SetValue('y', TGocciaStringValue.Create('hello'));
    Parameters := TStringList.Create;
    Parameters.Add('a');
    Parameters.Add('b');

    // Create a function body that uses parameters, scope variables, and returns a complex expression
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

    // Convert TStringList to TGocciaParameterArray
    SetLength(ParamArray, Parameters.Count);
    for I := 0 to Parameters.Count - 1 do
    begin
      ParamArray[I].Name := Parameters[I];
      ParamArray[I].DefaultValue := nil;
    end;

    FunctionValue := T.Create(ParamArray, Statements, Scope, 'test');

    // Test function execution
    Arguments := TObjectList<TGocciaValue>.Create;
    Arguments.Add(TGocciaNumberValue.Create(10));
    Arguments.Add(TGocciaNumberValue.Create(20));
    ReturnValue := FunctionValue.Call(Arguments, TGocciaUndefinedValue.Create);
    TestSuite.Expect<String>(ReturnValue.ToString).ToBe('72hello'); // (10 + 20 + 42) + 'hello'
    Arguments.Free;

    FunctionValue.Free;
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

    Statements.Add(TGocciaLiteralExpression.Create(TGocciaNumberValue.Create(1), 0, 0));

    // Execute statements directly
    Context.Scope := Scope;
    LastValue := TGocciaUndefinedValue.Create;

    for I := 0 to Statements.Count - 1 do
    begin
      LastValue := Evaluate(Statements[I], Context);
    end;

    Expect<Double>(LastValue.ToNumber).ToBe(1);

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

    Statements.Add(TGocciaBinaryExpression.Create(TGocciaLiteralExpression.Create(TGocciaNumberValue.Create(1), 0, 0), gttPlus, TGocciaLiteralExpression.Create(TGocciaNumberValue.Create(2), 0, 0), 0, 0));

    // Execute statements directly
    Context.Scope := Scope;
    LastValue := TGocciaUndefinedValue.Create;

    for I := 0 to Statements.Count - 1 do
    begin
      LastValue := Evaluate(Statements[I], Context);
    end;

    Expect<Double>(LastValue.ToNumber).ToBe(3);

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
    TestSimpleFunctionFromTemplate<TGocciaFunctionValue>(Self, 'Function');
  end;

  procedure TTestFunctionValue.TestFunctionWithParameters;
  begin
    TestFunctionWithParametersFromTemplate<TGocciaFunctionValue>(Self);
  end;

  procedure TTestFunctionValue.TestFunctionWithReturn;
  begin
    TestFunctionWithReturnFromTemplate<TGocciaFunctionValue>(Self);
  end;

  procedure TTestFunctionValue.TestFunctionWithScope;
  begin
    TestFunctionWithScopeFromTemplate<TGocciaFunctionValue>(Self);
  end;

  procedure TTestFunctionValue.TestFunctionWithScopeAndParameters;
  begin
    TestFunctionWithScopeAndParametersFromTemplate<TGocciaFunctionValue>(Self);
  end;

  procedure TTestFunctionValue.TestFunctionWithScopeAndReturn;
  begin
    TestFunctionWithScopeAndReturnFromTemplate<TGocciaFunctionValue>(Self);
  end;

  procedure TTestFunctionValue.TestFunctionWithScopeAndParametersAndReturn;
  begin
    TestFunctionWithScopeAndParametersAndReturnFromTemplate<TGocciaFunctionValue>(Self);
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
    TestSimpleFunctionFromTemplate<TGocciaMethodValue>(Self, 'Method');
  end;

  procedure TTestMethodValue.TestMethodWithParameters;
  begin
    TestFunctionWithParametersFromTemplate<TGocciaMethodValue>(Self);
  end;

  procedure TTestMethodValue.TestMethodWithReturn;
  begin
    TestFunctionWithReturnFromTemplate<TGocciaMethodValue>(Self);
  end;

  procedure TTestMethodValue.TestMethodWithScope;
  begin
    TestFunctionWithScopeFromTemplate<TGocciaMethodValue>(Self);
  end;

  procedure TTestMethodValue.TestMethodWithScopeAndParameters;
  begin
    TestFunctionWithScopeAndParametersFromTemplate<TGocciaMethodValue>(Self);
  end;

  procedure TTestMethodValue.TestMethodWithScopeAndReturn;
  begin
    TestFunctionWithScopeAndReturnFromTemplate<TGocciaMethodValue>(Self);
  end;

  procedure TTestMethodValue.TestMethodWithScopeAndParametersAndReturn;
  begin
    TestFunctionWithScopeAndParametersAndReturnFromTemplate<TGocciaMethodValue>(Self);
  end;

begin
  TestRunnerProgram.AddSuite(TTestBlockValue.Create('Block Value'));
  TestRunnerProgram.AddSuite(TTestFunctionValue.Create('Function Value'));
  TestRunnerProgram.AddSuite(TTestMethodValue.Create('Method Value'));
  TestRunnerProgram.Run;

  ExitCodeCheck(ExitCode);
end.
