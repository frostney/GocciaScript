program VariablesTest;

{$I Goccia.inc}

uses
  Classes, SysUtils,
  Goccia.Interpreter,
  Goccia.Values.UndefinedValue,
  Goccia.Values.NumberValue,
  Goccia.Values.StringValue,
  Goccia.Values.BooleanValue,
  Goccia.Values.NullValue,
  Goccia.Values.ObjectValue,
  Goccia.Values.ArrayValue,
  Goccia.Values.FunctionValue,
  TestUtils,
  TestRunner;

type
  TTestVariables = class(TTestSuite)
  public
    procedure SetupTests; override;

    procedure TestVariableDeclaration;
    procedure TestVariableAssignment;
    procedure TestVariableReassignment;
  end;

  procedure TTestVariables.SetupTests;
  begin
    Test('Variable Declaration', TestVariableDeclaration);
    Test('Variable Assignment', TestVariableAssignment);
    Test('Variable Reassignment', TestVariableReassignment);
  end;

  procedure TTestVariables.TestVariableDeclaration;
  var
    LetVar, ConstVar: String;
    LetInterpreterResult, ConstInterpreterResult: TInterpreterResult;
  begin
    LetVar := 'let a;';

    LetInterpreterResult := RunFromString(LetVar);

    Expect<String>(LetInterpreterResult.Interpreter.GlobalScope.GetValue('a').ToString).ToBe('undefined');
    Expect<String>(LetInterpreterResult.Value.ToString).ToBe('undefined');

    ConstVar := 'const b;';

    ConstInterpreterResult := RunFromString(ConstVar);

    Expect<String>(ConstInterpreterResult.Interpreter.GlobalScope.GetValue('b').ToString).ToBe('undefined');
    Expect<String>(ConstInterpreterResult.Value.ToString).ToBe('undefined');
  end;

  procedure TTestVariables.TestVariableAssignment;
  var
    LetReturnUndefined, ConstReturnUndefined, LetReturnNumber, ConstReturnNumber, LetReturnString, ConstReturnString, LetReturnBoolean, ConstReturnBoolean, LetReturnNull, ConstReturnNull: String;
    LetInterpreterResultUndefined, ConstInterpreterResultUndefined, LetInterpreterResultNumber, ConstInterpreterResultNumber, LetInterpreterResultString, ConstInterpreterResultString, LetInterpreterResultBoolean, ConstInterpreterResultBoolean, LetInterpreterResultNull, ConstInterpreterResultNull: TInterpreterResult;
  begin
    LetReturnUndefined := 'let a = undefined;';
    ConstReturnUndefined := 'const b = undefined;';
    LetReturnNumber := 'let a = 1;';
    ConstReturnNumber := 'const b = 1;';
    LetReturnString := 'let a = "string";';
    ConstReturnString := 'const b = "string";';
    LetReturnBoolean := 'let a = true;';
    ConstReturnBoolean := 'const b = true;';
    LetReturnNull := 'let a = null;';
    ConstReturnNull := 'const b = null;';

    LetInterpreterResultUndefined := RunFromString(LetReturnUndefined);
    ConstInterpreterResultUndefined := RunFromString(ConstReturnUndefined);
    LetInterpreterResultNumber := RunFromString(LetReturnNumber);
    ConstInterpreterResultNumber := RunFromString(ConstReturnNumber);
    LetInterpreterResultString := RunFromString(LetReturnString);
    ConstInterpreterResultString := RunFromString(ConstReturnString);
    LetInterpreterResultBoolean := RunFromString(LetReturnBoolean);
    ConstInterpreterResultBoolean := RunFromString(ConstReturnBoolean);
    LetInterpreterResultNull := RunFromString(LetReturnNull);
    ConstInterpreterResultNull := RunFromString(ConstReturnNull);

    Expect<String>(LetInterpreterResultUndefined.Interpreter.GlobalScope.GetValue('a').ToString).ToBe('undefined');
    Expect<String>(LetInterpreterResultUndefined.Value.ToString).ToBe('undefined');
    Expect<String>(ConstInterpreterResultUndefined.Interpreter.GlobalScope.GetValue('b').ToString).ToBe('undefined');
    Expect<String>(ConstInterpreterResultUndefined.Value.ToString).ToBe('undefined');

    Expect<String>(LetInterpreterResultNumber.Interpreter.GlobalScope.GetValue('a').ToString).ToBe('1');
    Expect<String>(LetInterpreterResultNumber.Value.ToString).ToBe('1');
    Expect<String>(ConstInterpreterResultNumber.Interpreter.GlobalScope.GetValue('b').ToString).ToBe('1');
    Expect<String>(ConstInterpreterResultNumber.Value.ToString).ToBe('1');

    Expect<String>(LetInterpreterResultString.Interpreter.GlobalScope.GetValue('a').ToString).ToBe('string');
    Expect<String>(LetInterpreterResultString.Value.ToString).ToBe('string');
    Expect<String>(ConstInterpreterResultString.Interpreter.GlobalScope.GetValue('b').ToString).ToBe('string');
    Expect<String>(ConstInterpreterResultString.Value.ToString).ToBe('string');

    Expect<String>(LetInterpreterResultBoolean.Interpreter.GlobalScope.GetValue('a').ToString).ToBe('true');
    Expect<String>(LetInterpreterResultBoolean.Value.ToString).ToBe('true');
    Expect<String>(ConstInterpreterResultBoolean.Interpreter.GlobalScope.GetValue('b').ToString).ToBe('true');
    Expect<String>(ConstInterpreterResultBoolean.Value.ToString).ToBe('true');

    Expect<String>(LetInterpreterResultNull.Interpreter.GlobalScope.GetValue('a').ToString).ToBe('null');
    Expect<String>(LetInterpreterResultNull.Value.ToString).ToBe('null');
    Expect<String>(ConstInterpreterResultNull.Interpreter.GlobalScope.GetValue('b').ToString).ToBe('null');
    Expect<String>(ConstInterpreterResultNull.Value.ToString).ToBe('null');
  end;

  procedure TTestVariables.TestVariableReassignment;
  var
    Text: String;
    InterpreterResult: TInterpreterResult;
  begin
    Text := 'let a = 1; a = 2;';

    InterpreterResult := RunFromString(Text);

    Expect<String>(InterpreterResult.Interpreter.GlobalScope.GetValue('a').ToString).ToBe('2');
    Expect<String>(InterpreterResult.Value.ToString).ToBe('2');
  end;

begin
  TestRunnerProgram.AddSuite(TTestVariables.Create('Variables'));
  TestRunnerProgram.Run;

  ExitCodeCheck(ExitCode);
end.
