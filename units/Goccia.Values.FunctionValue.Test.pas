program Goccia.Values.FunctionValue.Test;

{$I Goccia.inc}

uses
  Goccia.Values.FunctionValue, Goccia.Values.ObjectValue, Goccia.Values.StringValue, Goccia.Values.NumberValue, Goccia.Values.BooleanValue, StrUtils, Math, TestRunner, Goccia.AST.Node, Generics.Collections, Goccia.AST.Statements, Goccia.AST.Expressions, Goccia.Scope, Goccia.Values.Base, Goccia.Token, SysUtils, Classes, Goccia.Values.UndefinedValue;

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
  end;

  TTestFunctionInvocation = class(TTestSuite)
  public
    procedure SetupTests; override;
  end;

  procedure TTestBlockValue.TestSimpleBlock;
  var
    BlockValue: TGocciaBlockValue;
    Statements: TObjectList<TGocciaASTNode>;
    Scope: TGocciaScope;
  begin
    Statements := TObjectList<TGocciaASTNode>.Create;
    Scope := TGocciaScope.Create(nil, skGlobal);

    Statements.Add(TGocciaLiteralExpression.Create(TGocciaNumberValue.Create(1), 0, 0));

    BlockValue := TGocciaBlockValue.Create(Statements, Scope);
    Expect<Double>(BlockValue.Execute(Scope).ToNumber).ToBe(1);

    BlockValue.Free;
    Scope.Free;
  end;

  procedure TTestBlockValue.TestArithmeticBlock;
  var
    BlockValue: TGocciaBlockValue;
    Statements: TObjectList<TGocciaASTNode>;
    Scope: TGocciaScope;
  begin
    Statements := TObjectList<TGocciaASTNode>.Create;
    Scope := TGocciaScope.Create(nil, skGlobal);

    Statements.Add(TGocciaBinaryExpression.Create(TGocciaLiteralExpression.Create(TGocciaNumberValue.Create(1), 0, 0), gttPlus, TGocciaLiteralExpression.Create(TGocciaNumberValue.Create(2), 0, 0), 0, 0));

    BlockValue := TGocciaBlockValue.Create(Statements, Scope);
    Expect<Double>(BlockValue.Execute(Scope).ToNumber).ToBe(3);

    BlockValue.Free;
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
  var
    FunctionValue: TGocciaFunctionValue;
    Scope: TGocciaScope;
    Parameters: TStringList;
    Body: TGocciaBlockValue;
    ReturnValue: TGocciaValue;
    Statements: TObjectList<TGocciaASTNode>;
  begin
    Scope := TGocciaScope.Create(nil, skGlobal);
    Parameters := TStringList.Create;

    WriteLn('Parameters: ' + IntToStr(Parameters.Count));

    Statements := TObjectList<TGocciaASTNode>.Create;
    Statements.Add(TGocciaLiteralExpression.Create(TGocciaUndefinedValue.Create, 0, 0));
    Body := TGocciaBlockValue.Create(Statements, Scope);
    FunctionValue := TGocciaFunctionValue.Create(Parameters, Body, Scope, 'test');

    WriteLn('Parameters from FunctionValue: ' + IntToStr(FunctionValue.Parameters.Count));

    // Test basic function properties
    Expect<Boolean>(FunctionValue.ToBoolean).ToBeTrue;
    Expect<String>(FunctionValue.ToString).ToBe('[Function: test]');
    Expect<Boolean>(IsNaN(FunctionValue.ToNumber)).ToBeTrue;
    Expect<String>(FunctionValue.TypeName).ToBe('function');

    // Test function metadata
    Expect<String>(FunctionValue.Name).ToBe('test');
    Expect<Integer>(FunctionValue.Parameters.Count).ToBe(0);
    Expect<Boolean>(FunctionValue.Body = Body).ToBeTrue;
    Expect<Boolean>(FunctionValue.Closure = Scope).ToBeTrue;

    // Test function execution
    ReturnValue := FunctionValue.Call(TObjectList<TGocciaValue>.Create, TGocciaUndefinedValue.Create);
    Expect<Boolean>(ReturnValue.ToBoolean).ToBeFalse;
    Expect<String>(ReturnValue.ToString).ToBe('undefined');
    Expect<Boolean>(IsNaN(ReturnValue.ToNumber)).ToBeTrue;
    Expect<String>(ReturnValue.TypeName).ToBe('undefined');

    FunctionValue.Free;
    Scope.Free;
  end;

  procedure TTestFunctionValue.TestFunctionWithParameters;
  var
    FunctionValue: TGocciaFunctionValue;
    Scope: TGocciaScope;
    Parameters: TStringList;
    Body: TGocciaBlockValue;
    ReturnValue: TGocciaValue;
    Statements: TObjectList<TGocciaASTNode>;
    Arguments: TObjectList<TGocciaValue>;
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
    Body := TGocciaBlockValue.Create(Statements, Scope);
    FunctionValue := TGocciaFunctionValue.Create(Parameters, Body, Scope, 'test');

    // Test function properties
    Expect<String>(FunctionValue.Name).ToBe('test');
    Expect<Integer>(FunctionValue.Parameters.Count).ToBe(2);
    Expect<String>(FunctionValue.Parameters[0]).ToBe('a');
    Expect<String>(FunctionValue.Parameters[1]).ToBe('b');

    // Test function execution with arguments
    Arguments := TObjectList<TGocciaValue>.Create;
    Arguments.Add(TGocciaNumberValue.Create(1));
    Arguments.Add(TGocciaNumberValue.Create(2));
    ReturnValue := FunctionValue.Call(Arguments, TGocciaUndefinedValue.Create);
    Expect<Double>(ReturnValue.ToNumber).ToBe(3);
    Arguments.Free;

    // Test function execution with too few arguments
    Arguments := TObjectList<TGocciaValue>.Create;
    Arguments.Add(TGocciaNumberValue.Create(1));
    ReturnValue := FunctionValue.Call(Arguments, TGocciaUndefinedValue.Create);
    Expect<Boolean>(IsNaN(ReturnValue.ToNumber)).ToBeTrue;
    Expect<String>(ReturnValue.TypeName).ToBe('number');
    Arguments.Free;

    // Test function execution with too many arguments
    Arguments := TObjectList<TGocciaValue>.Create;
    Arguments.Add(TGocciaNumberValue.Create(1));
    Arguments.Add(TGocciaNumberValue.Create(2));
    Arguments.Add(TGocciaNumberValue.Create(3));
    ReturnValue := FunctionValue.Call(Arguments, TGocciaUndefinedValue.Create);
    Expect<Double>(ReturnValue.ToNumber).ToBe(3);
    Arguments.Free;

    FunctionValue.Free;
    Scope.Free;
  end;

  procedure TTestFunctionValue.TestFunctionWithReturn;
  var
    FunctionValue: TGocciaFunctionValue;
    Scope: TGocciaScope;
    Parameters: TStringList;
    Body: TGocciaBlockValue;
    ReturnValue: TGocciaValue;
    Statements: TObjectList<TGocciaASTNode>;
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
    Body := TGocciaBlockValue.Create(Statements, Scope);
    FunctionValue := TGocciaFunctionValue.Create(Parameters, Body, Scope, 'test');

    ReturnValue := FunctionValue.Call(TObjectList<TGocciaValue>.Create, TGocciaUndefinedValue.Create);
    Expect<Double>(ReturnValue.ToNumber).ToBe(1);
    Expect<String>(ReturnValue.ToString).ToBe('1');
    Expect<Boolean>(ReturnValue.ToBoolean).ToBeTrue;
    Expect<String>(ReturnValue.TypeName).ToBe('number');

    // Test returning a string
    Statements := TObjectList<TGocciaASTNode>.Create;
    Statements.Add(TGocciaReturnStatement.Create(
      TGocciaLiteralExpression.Create(TGocciaStringValue.Create('hello'), 0, 0),
      0, 0
    ));
    Body := TGocciaBlockValue.Create(Statements, Scope);
    FunctionValue := TGocciaFunctionValue.Create(Parameters, Body, Scope, 'test');

    ReturnValue := FunctionValue.Call(TObjectList<TGocciaValue>.Create, TGocciaUndefinedValue.Create);
    Expect<String>(ReturnValue.ToString).ToBe('hello');
    Expect<Boolean>(ReturnValue.ToBoolean).ToBeTrue;
    Expect<Boolean>(IsNaN(ReturnValue.ToNumber)).ToBeTrue;
    Expect<String>(ReturnValue.TypeName).ToBe('string');

    // Test returning a boolean
    Statements := TObjectList<TGocciaASTNode>.Create;
    Statements.Add(TGocciaReturnStatement.Create(
      TGocciaLiteralExpression.Create(TGocciaBooleanValue.Create(True), 0, 0),
      0, 0
    ));
    Body := TGocciaBlockValue.Create(Statements, Scope);
    FunctionValue := TGocciaFunctionValue.Create(Parameters, Body, Scope, 'test');

    ReturnValue := FunctionValue.Call(TObjectList<TGocciaValue>.Create, TGocciaUndefinedValue.Create);
    Expect<Boolean>(ReturnValue.ToBoolean).ToBe(True);
    Expect<String>(ReturnValue.ToString).ToBe('true');
    Expect<Double>(ReturnValue.ToNumber).ToBe(1);
    Expect<String>(ReturnValue.TypeName).ToBe('boolean');

    // Test returning undefined
    Statements := TObjectList<TGocciaASTNode>.Create;
    Statements.Add(TGocciaReturnStatement.Create(
      TGocciaLiteralExpression.Create(TGocciaUndefinedValue.Create, 0, 0),
      0, 0
    ));
    Body := TGocciaBlockValue.Create(Statements, Scope);
    FunctionValue := TGocciaFunctionValue.Create(Parameters, Body, Scope, 'test');

    ReturnValue := FunctionValue.Call(TObjectList<TGocciaValue>.Create, TGocciaUndefinedValue.Create);
    Expect<Boolean>(ReturnValue.ToBoolean).ToBeFalse;
    Expect<String>(ReturnValue.ToString).ToBe('undefined');
    Expect<Boolean>(IsNaN(ReturnValue.ToNumber)).ToBeTrue;
    Expect<String>(ReturnValue.TypeName).ToBe('undefined');

    Parameters.Free;
    Scope.Free;
  end;

  procedure TTestFunctionValue.TestFunctionWithScope;
  var
    FunctionValue: TGocciaFunctionValue;
    Scope: TGocciaScope;
    Parameters: TStringList;
    Body: TGocciaBlockValue;
    ReturnValue: TGocciaValue;
    Statements: TObjectList<TGocciaASTNode>;
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
    Body := TGocciaBlockValue.Create(Statements, Scope);
    FunctionValue := TGocciaFunctionValue.Create(Parameters, Body, Scope, 'test');

    // Test function execution
    ReturnValue := FunctionValue.Call(TObjectList<TGocciaValue>.Create, TGocciaUndefinedValue.Create);
    Expect<Double>(ReturnValue.ToNumber).ToBe(42);
    Expect<String>(ReturnValue.ToString).ToBe('42');
    Expect<Boolean>(ReturnValue.ToBoolean).ToBe(True);
    Expect<String>(ReturnValue.TypeName).ToBe('number');

    FunctionValue.Free;
    Scope.Free;
  end;

  procedure TTestFunctionValue.TestFunctionWithScopeAndParameters;
  var
    FunctionValue: TGocciaFunctionValue;
    Scope: TGocciaScope;
    Parameters: TStringList;
    Body: TGocciaBlockValue;
    ReturnValue: TGocciaValue;
    Statements: TObjectList<TGocciaASTNode>;
    Arguments: TObjectList<TGocciaValue>;
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
    Body := TGocciaBlockValue.Create(Statements, Scope);
    FunctionValue := TGocciaFunctionValue.Create(Parameters, Body, Scope, 'test');

    // Test function execution
    Arguments := TObjectList<TGocciaValue>.Create;
    Arguments.Add(TGocciaNumberValue.Create(10));
    ReturnValue := FunctionValue.Call(Arguments, TGocciaUndefinedValue.Create);
    Expect<Double>(ReturnValue.ToNumber).ToBe(20); // 10 + 10
    Arguments.Free;

    FunctionValue.Free;
    Scope.Free;
  end;

  procedure TTestFunctionValue.TestFunctionWithScopeAndReturn;
  var
    FunctionValue: TGocciaFunctionValue;
    Scope: TGocciaScope;
    Parameters: TStringList;
    Body: TGocciaBlockValue;
    ReturnValue: TGocciaValue;
    Arguments: TObjectList<TGocciaValue>;
    Statements: TObjectList<TGocciaASTNode>;
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
    Body := TGocciaBlockValue.Create(Statements, Scope);
    FunctionValue := TGocciaFunctionValue.Create(Parameters, Body, Scope, 'test');

    // Test function execution
    Arguments := TObjectList<TGocciaValue>.Create;
    ReturnValue := FunctionValue.Call(Arguments, TGocciaUndefinedValue.Create);
    Expect<String>(ReturnValue.ToString).ToBe('43hello');
    Expect<Boolean>(ReturnValue.ToBoolean).ToBe(True);
    Expect<Boolean>(IsNaN(ReturnValue.ToNumber)).ToBeTrue;
    Expect<String>(ReturnValue.TypeName).ToBe('string');
    Arguments.Free;

    FunctionValue.Free;
    Scope.Free;
  end;

  procedure TTestFunctionValue.TestFunctionWithScopeAndParametersAndReturn;
  var
    FunctionValue: TGocciaFunctionValue;
    Scope: TGocciaScope;
    Parameters: TStringList;
    Body: TGocciaBlockValue;
    ReturnValue: TGocciaValue;
    Statements: TObjectList<TGocciaASTNode>;
    Arguments: TObjectList<TGocciaValue>;
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
    Body := TGocciaBlockValue.Create(Statements, Scope);
    FunctionValue := TGocciaFunctionValue.Create(Parameters, Body, Scope, 'test');

    // Test function execution
    Arguments := TObjectList<TGocciaValue>.Create;
    Arguments.Add(TGocciaNumberValue.Create(10));
    Arguments.Add(TGocciaNumberValue.Create(20));
    ReturnValue := FunctionValue.Call(Arguments, TGocciaUndefinedValue.Create);
    Expect<String>(ReturnValue.ToString).ToBe('72hello'); // (10 + 20 + 42) + 'hello'
    Arguments.Free;

    FunctionValue.Free;
    Scope.Free;
  end;

  procedure TTestMethodValue.SetupTests;
  begin

  end;

  procedure TTestFunctionInvocation.SetupTests;
  begin

  end;

begin
  TestRunnerProgram.AddSuite(TTestBlockValue.Create('Block Value'));
  TestRunnerProgram.AddSuite(TTestFunctionValue.Create('Function Value'));
  TestRunnerProgram.AddSuite(TTestMethodValue.Create('Method Value'));
  TestRunnerProgram.AddSuite(TTestFunctionInvocation.Create('Function Invocation'));
  TestRunnerProgram.Run;

  ExitCodeCheck(ExitCode);
end.
