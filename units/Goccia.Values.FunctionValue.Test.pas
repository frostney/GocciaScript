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
    Expect<Double>(BlockValue.Execute.ToNumber).ToBe(1);

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
    Expect<Double>(BlockValue.Execute.ToNumber).ToBe(3);

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
    Statements := TObjectList<TGocciaASTNode>.Create;
    Statements.Add(TGocciaLiteralExpression.Create(TGocciaUndefinedValue.Create, 0, 0));

    Body := TGocciaBlockValue.Create(Statements, Scope);
    FunctionValue := TGocciaFunctionValue.Create(Parameters, Body, Scope, 'test');

    Expect<Boolean>(FunctionValue.ToBoolean).ToBe(True);
    Expect<String>(FunctionValue.ToString).ToBe('[Function: test]');
    Expect<Boolean>(IsNaN(FunctionValue.ToNumber)).ToBe(True);
    Expect<String>(FunctionValue.TypeName).ToBe('function');

    ReturnValue := FunctionValue.Call(TObjectList<TGocciaValue>.Create, TGocciaUndefinedValue.Create);
    Expect<Boolean>(ReturnValue.ToBoolean).ToBe(False);
    Expect<String>(ReturnValue.ToString).ToBe('undefined');
    Expect<Boolean>(IsNaN(ReturnValue.ToNumber)).ToBe(True);
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
  begin
    Scope := TGocciaScope.Create(nil, skGlobal);
    Parameters := TStringList.Create;
    Parameters.Add('a');
    Parameters.Add('b');
    Statements := TObjectList<TGocciaASTNode>.Create;
    Statements.Add(TGocciaLiteralExpression.Create(TGocciaUndefinedValue.Create, 0, 0));
    Body := TGocciaBlockValue.Create(Statements, Scope);
    FunctionValue := TGocciaFunctionValue.Create(Parameters, Body, Scope, 'test');

    Expect<Boolean>(FunctionValue.ToBoolean).ToBe(True);
    Expect<String>(FunctionValue.ToString).ToBe('[Function: test]');
    Expect<Boolean>(IsNaN(FunctionValue.ToNumber)).ToBe(True);
    Expect<String>(FunctionValue.TypeName).ToBe('function');

    ReturnValue := FunctionValue.Call(TObjectList<TGocciaValue>.Create, TGocciaUndefinedValue.Create);
    Expect<Boolean>(ReturnValue.ToBoolean).ToBe(False);
    Expect<String>(ReturnValue.ToString).ToBe('undefined');
    Expect<Boolean>(IsNaN(ReturnValue.ToNumber)).ToBe(True);
    Expect<String>(ReturnValue.TypeName).ToBe('undefined');

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

    Statements := TObjectList<TGocciaASTNode>.Create;
    Statements.Add(TGocciaReturnStatement.Create(TGocciaLiteralExpression.Create(TGocciaNumberValue.Create(1), 0, 0), 0, 0));
    Body := TGocciaBlockValue.Create(Statements, Scope);
    FunctionValue := TGocciaFunctionValue.Create(Parameters, Body, Scope, 'test');

    ReturnValue := FunctionValue.Call(TObjectList<TGocciaValue>.Create, TGocciaUndefinedValue.Create);
    Expect<Boolean>(ReturnValue.ToBoolean).ToBe(True);
    Expect<String>(ReturnValue.ToString).ToBe('1');
    Expect<Double>(ReturnValue.ToNumber).ToBe(1);
    Expect<String>(ReturnValue.TypeName).ToBe('number');

    FunctionValue.Free;
    Scope.Free;
  end;

  procedure TTestFunctionValue.TestFunctionWithScope;
  begin

  end;

  procedure TTestFunctionValue.TestFunctionWithScopeAndParameters;
  begin

  end;

  procedure TTestFunctionValue.TestFunctionWithScopeAndReturn;
  begin

  end;

  procedure TTestFunctionValue.TestFunctionWithScopeAndParametersAndReturn;
  begin

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
end.
