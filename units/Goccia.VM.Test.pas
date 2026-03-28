program Goccia.VM.Test;

{$I Goccia.inc}

uses
  SysUtils,

  TestRunner,

  Goccia.Arguments.Collection,
  Goccia.Bytecode,
  Goccia.Bytecode.Chunk,
  Goccia.TestSetup,
  Goccia.Values.FunctionBase,
  Goccia.Values.HoleValue,
  Goccia.Values.Primitives,
  Goccia.VM;

type
  TTestGocciaVM = class(TTestSuite)
  private
    procedure TestExecuteIntegerAddition;
    procedure TestExecuteLocalsRoundTrip;
    procedure TestExecuteLiteralLoads;
    procedure TestExecuteConstString;
    procedure TestExecuteComparisons;
    procedure TestExecuteArrayOps;
    procedure TestExecuteArrayPop;
    procedure TestExecuteObjectOps;
    procedure TestExecuteIndexedObjectOps;
    procedure TestExecuteClosureCall;
    procedure TestExecuteCapturedClosure;
  public
    procedure SetupTests; override;
  end;

procedure TTestGocciaVM.SetupTests;
begin
  Test('Execute integer addition', TestExecuteIntegerAddition);
  Test('Execute locals round trip', TestExecuteLocalsRoundTrip);
  Test('Execute literal loads', TestExecuteLiteralLoads);
  Test('Execute constant string', TestExecuteConstString);
  Test('Execute comparisons', TestExecuteComparisons);
  Test('Execute array ops', TestExecuteArrayOps);
  Test('Execute array pop', TestExecuteArrayPop);
  Test('Execute object ops', TestExecuteObjectOps);
  Test('Execute indexed object ops', TestExecuteIndexedObjectOps);
  Test('Execute closure call', TestExecuteClosureCall);
  Test('Execute captured closure', TestExecuteCapturedClosure);
end;

procedure TTestGocciaVM.TestExecuteIntegerAddition;
var
  Template: TGocciaFunctionTemplate;
  VM: TGocciaVM;
  ResultValue: TGocciaValue;
begin
  Template := TGocciaFunctionTemplate.Create('add');
  VM := TGocciaVM.Create;
  try
    Template.MaxRegisters := 3;
    Template.EmitInstruction(EncodeAsBx(OP_LOAD_INT, 0, 1));
    Template.EmitInstruction(EncodeAsBx(OP_LOAD_INT, 1, 2));
    Template.EmitInstruction(EncodeABC(OP_ADD_INT, 2, 0, 1));
    Template.EmitInstruction(EncodeABC(OP_RETURN, 2, 0, 0));

    ResultValue := VM.ExecuteFunction(Template);
    Expect<Double>(ResultValue.ToNumberLiteral.Value).ToBe(3);
  finally
    VM.Free;
    Template.Free;
  end;
end;

procedure TTestGocciaVM.TestExecuteLocalsRoundTrip;
var
  Template: TGocciaFunctionTemplate;
  VM: TGocciaVM;
  ResultValue: TGocciaValue;
begin
  Template := TGocciaFunctionTemplate.Create('locals');
  VM := TGocciaVM.Create;
  try
    Template.MaxRegisters := 2;
    Template.EmitInstruction(EncodeAsBx(OP_LOAD_INT, 0, 7));
    Template.EmitInstruction(EncodeABx(OP_SET_LOCAL, 0, 0));
    Template.EmitInstruction(EncodeABx(OP_GET_LOCAL, 1, 0));
    Template.EmitInstruction(EncodeABC(OP_RETURN, 1, 0, 0));

    ResultValue := VM.ExecuteFunction(Template);
    Expect<Double>(ResultValue.ToNumberLiteral.Value).ToBe(7);
  finally
    VM.Free;
    Template.Free;
  end;
end;

procedure TTestGocciaVM.TestExecuteLiteralLoads;
var
  Template: TGocciaFunctionTemplate;
  VM: TGocciaVM;
  UndefinedValue, NullValue, HoleValue: TGocciaValue;
begin
  Template := TGocciaFunctionTemplate.Create('literal-loads');
  VM := TGocciaVM.Create;
  try
    Template.MaxRegisters := 3;

    Template.EmitInstruction(EncodeABC(OP_LOAD_UNDEFINED, 0, 0, 0));
    Template.EmitInstruction(EncodeABC(OP_RETURN, 0, 0, 0));
    UndefinedValue := VM.ExecuteFunction(Template);
    Expect<Boolean>(UndefinedValue = TGocciaUndefinedLiteralValue.UndefinedValue).ToBe(True);

    Template.PatchInstruction(0, EncodeABC(OP_LOAD_NULL, 0, 0, 0));
    NullValue := VM.ExecuteFunction(Template);
    Expect<Boolean>(NullValue = TGocciaNullLiteralValue.NullValue).ToBe(True);

    Template.PatchInstruction(0, EncodeABC(OP_LOAD_HOLE, 0, 0, 0));
    HoleValue := VM.ExecuteFunction(Template);
    Expect<Boolean>(HoleValue = TGocciaHoleValue.HoleValue).ToBe(True);
  finally
    VM.Free;
    Template.Free;
  end;
end;

procedure TTestGocciaVM.TestExecuteConstString;
var
  Template: TGocciaFunctionTemplate;
  VM: TGocciaVM;
  ResultValue: TGocciaValue;
  ConstIdx: UInt16;
begin
  Template := TGocciaFunctionTemplate.Create('const-string');
  VM := TGocciaVM.Create;
  try
    Template.MaxRegisters := 1;
    ConstIdx := Template.AddConstantString('hello');
    Template.EmitInstruction(EncodeABx(OP_LOAD_CONST, 0, ConstIdx));
    Template.EmitInstruction(EncodeABC(OP_RETURN, 0, 0, 0));

    ResultValue := VM.ExecuteFunction(Template);
    Expect<string>(ResultValue.ToStringLiteral.Value).ToBe('hello');
  finally
    VM.Free;
    Template.Free;
  end;
end;

procedure TTestGocciaVM.TestExecuteComparisons;
var
  Template: TGocciaFunctionTemplate;
  VM: TGocciaVM;
  ResultValue: TGocciaValue;
begin
  Template := TGocciaFunctionTemplate.Create('compare');
  VM := TGocciaVM.Create;
  try
    Template.MaxRegisters := 3;
    Template.EmitInstruction(EncodeAsBx(OP_LOAD_INT, 0, 2));
    Template.EmitInstruction(EncodeAsBx(OP_LOAD_INT, 1, 3));
    Template.EmitInstruction(EncodeABC(OP_LT_INT, 2, 0, 1));
    Template.EmitInstruction(EncodeABC(OP_RETURN, 2, 0, 0));

    ResultValue := VM.ExecuteFunction(Template);
    Expect<Boolean>(ResultValue.ToBooleanLiteral.Value).ToBe(True);
  finally
    VM.Free;
    Template.Free;
  end;
end;

procedure TTestGocciaVM.TestExecuteArrayOps;
var
  Template: TGocciaFunctionTemplate;
  VM: TGocciaVM;
  ResultValue: TGocciaValue;
begin
  Template := TGocciaFunctionTemplate.Create('array');
  VM := TGocciaVM.Create;
  try
    Template.MaxRegisters := 4;
    Template.EmitInstruction(EncodeABC(OP_NEW_ARRAY, 0, 0, 0));
    Template.EmitInstruction(EncodeAsBx(OP_LOAD_INT, 1, 5));
    Template.EmitInstruction(EncodeABC(OP_ARRAY_PUSH, 0, 1, 0));
    Template.EmitInstruction(EncodeAsBx(OP_LOAD_INT, 2, 0));
    Template.EmitInstruction(EncodeABC(OP_ARRAY_GET, 3, 0, 2));
    Template.EmitInstruction(EncodeABC(OP_RETURN, 3, 0, 0));

    ResultValue := VM.ExecuteFunction(Template);
    Expect<Double>(ResultValue.ToNumberLiteral.Value).ToBe(5);
  finally
    VM.Free;
    Template.Free;
  end;
end;

procedure TTestGocciaVM.TestExecuteArrayPop;
var
  Template: TGocciaFunctionTemplate;
  VM: TGocciaVM;
  ResultValue: TGocciaValue;
begin
  Template := TGocciaFunctionTemplate.Create('array-pop');
  VM := TGocciaVM.Create;
  try
    Template.MaxRegisters := 3;
    Template.EmitInstruction(EncodeABC(OP_NEW_ARRAY, 0, 0, 0));
    Template.EmitInstruction(EncodeAsBx(OP_LOAD_INT, 1, 9));
    Template.EmitInstruction(EncodeABC(OP_ARRAY_PUSH, 0, 1, 0));
    Template.EmitInstruction(EncodeABC(OP_ARRAY_POP, 2, 0, 0));
    Template.EmitInstruction(EncodeABC(OP_RETURN, 2, 0, 0));

    ResultValue := VM.ExecuteFunction(Template);
    Expect<Double>(ResultValue.ToNumberLiteral.Value).ToBe(9);
  finally
    VM.Free;
    Template.Free;
  end;
end;

procedure TTestGocciaVM.TestExecuteObjectOps;
var
  Template: TGocciaFunctionTemplate;
  DeleteTemplate: TGocciaFunctionTemplate;
  VM: TGocciaVM;
  ResultValue: TGocciaValue;
  NameIdx: UInt16;
begin
  Template := TGocciaFunctionTemplate.Create('object');
  DeleteTemplate := TGocciaFunctionTemplate.Create('object-delete');
  VM := TGocciaVM.Create;
  try
    Template.MaxRegisters := 2;
    NameIdx := Template.AddConstantString('answer');
    Template.EmitInstruction(EncodeABx(OP_NEW_OBJECT, 0, 0));
    Template.EmitInstruction(EncodeAsBx(OP_LOAD_INT, 1, 42));
    Template.EmitInstruction(EncodeABC(OP_SET_PROP_CONST, 0, UInt8(NameIdx), 1));
    Template.EmitInstruction(EncodeABC(OP_GET_PROP_CONST, 1, 0, UInt8(NameIdx)));
    Template.EmitInstruction(EncodeABC(OP_RETURN, 1, 0, 0));

    ResultValue := VM.ExecuteFunction(Template);
    Expect<Double>(ResultValue.ToNumberLiteral.Value).ToBe(42);

    DeleteTemplate.MaxRegisters := 2;
    NameIdx := DeleteTemplate.AddConstantString('answer');
    DeleteTemplate.EmitInstruction(EncodeABx(OP_NEW_OBJECT, 0, 0));
    DeleteTemplate.EmitInstruction(EncodeAsBx(OP_LOAD_INT, 1, 42));
    DeleteTemplate.EmitInstruction(EncodeABC(OP_SET_PROP_CONST, 0, UInt8(NameIdx), 1));
    DeleteTemplate.EmitInstruction(EncodeABx(OP_DELETE_PROP_CONST, 0, NameIdx));
    DeleteTemplate.EmitInstruction(EncodeABC(OP_GET_PROP_CONST, 1, 0, UInt8(NameIdx)));
    DeleteTemplate.EmitInstruction(EncodeABC(OP_RETURN, 1, 0, 0));

    ResultValue := VM.ExecuteFunction(DeleteTemplate);
    Expect<Boolean>(ResultValue = TGocciaUndefinedLiteralValue.UndefinedValue).ToBe(True);
  finally
    VM.Free;
    DeleteTemplate.Free;
    Template.Free;
  end;
end;

procedure TTestGocciaVM.TestExecuteIndexedObjectOps;
var
  Template: TGocciaFunctionTemplate;
  VM: TGocciaVM;
  ResultValue: TGocciaValue;
  KeyIdx: UInt16;
begin
  Template := TGocciaFunctionTemplate.Create('indexed-object');
  VM := TGocciaVM.Create;
  try
    Template.MaxRegisters := 3;
    KeyIdx := Template.AddConstantString('dynamicKey');
    Template.EmitInstruction(EncodeABx(OP_NEW_OBJECT, 0, 0));
    Template.EmitInstruction(EncodeABx(OP_LOAD_CONST, 1, KeyIdx));
    Template.EmitInstruction(EncodeAsBx(OP_LOAD_INT, 2, 11));
    Template.EmitInstruction(EncodeABC(OP_SET_INDEX, 0, 1, 2));
    Template.EmitInstruction(EncodeABC(OP_GET_INDEX, 2, 0, 1));
    Template.EmitInstruction(EncodeABC(OP_RETURN, 2, 0, 0));

    ResultValue := VM.ExecuteFunction(Template);
    Expect<Double>(ResultValue.ToNumberLiteral.Value).ToBe(11);
  finally
    VM.Free;
    Template.Free;
  end;
end;

procedure TTestGocciaVM.TestExecuteClosureCall;
var
  Template: TGocciaFunctionTemplate;
  ChildTemplate: TGocciaFunctionTemplate;
  VM: TGocciaVM;
  ResultValue: TGocciaValue;
begin
  Template := TGocciaFunctionTemplate.Create('caller');
  ChildTemplate := TGocciaFunctionTemplate.Create('add');
  VM := TGocciaVM.Create;
  try
    ChildTemplate.MaxRegisters := 3;
    ChildTemplate.ParameterCount := 2;
    ChildTemplate.EmitInstruction(EncodeABx(OP_GET_LOCAL, 0, 1));
    ChildTemplate.EmitInstruction(EncodeABx(OP_GET_LOCAL, 1, 2));
    ChildTemplate.EmitInstruction(EncodeABC(OP_ADD_INT, 2, 0, 1));
    ChildTemplate.EmitInstruction(EncodeABC(OP_RETURN, 2, 0, 0));

    Template.MaxRegisters := 3;
    Template.AddFunction(ChildTemplate);
    Template.EmitInstruction(EncodeABx(OP_CLOSURE, 0, 0));
    Template.EmitInstruction(EncodeAsBx(OP_LOAD_INT, 1, 4));
    Template.EmitInstruction(EncodeAsBx(OP_LOAD_INT, 2, 5));
    Template.EmitInstruction(EncodeABC(OP_CALL, 0, 2, 0));
    Template.EmitInstruction(EncodeABC(OP_RETURN, 0, 0, 0));

    ResultValue := VM.ExecuteFunction(Template);
    Expect<Double>(ResultValue.ToNumberLiteral.Value).ToBe(9);
  finally
    VM.Free;
    Template.Free;
  end;
end;

procedure TTestGocciaVM.TestExecuteCapturedClosure;
var
  Template: TGocciaFunctionTemplate;
  ChildTemplate: TGocciaFunctionTemplate;
  VM: TGocciaVM;
  ClosureValue: TGocciaValue;
  CallArgs: TGocciaArgumentsCollection;
begin
  Template := TGocciaFunctionTemplate.Create('makeCounter');
  ChildTemplate := TGocciaFunctionTemplate.Create('next');
  VM := TGocciaVM.Create;
  CallArgs := TGocciaArgumentsCollection.Create;
  try
    ChildTemplate.MaxRegisters := 3;
    ChildTemplate.AddUpvalueDescriptor(True, 1);
    ChildTemplate.EmitInstruction(EncodeABx(OP_GET_UPVALUE, 0, 0));
    ChildTemplate.EmitInstruction(EncodeAsBx(OP_LOAD_INT, 1, 1));
    ChildTemplate.EmitInstruction(EncodeABC(OP_ADD_INT, 2, 0, 1));
    ChildTemplate.EmitInstruction(EncodeABx(OP_SET_UPVALUE, 2, 0));
    ChildTemplate.EmitInstruction(EncodeABC(OP_RETURN, 2, 0, 0));

    Template.MaxRegisters := 3;
    Template.EmitInstruction(EncodeAsBx(OP_LOAD_INT, 0, 0));
    Template.EmitInstruction(EncodeABx(OP_SET_LOCAL, 0, 1));
    Template.AddFunction(ChildTemplate);
    Template.EmitInstruction(EncodeABx(OP_CLOSURE, 2, 0));
    Template.EmitInstruction(EncodeABC(OP_CLOSE_UPVALUE, 1, 0, 0));
    Template.EmitInstruction(EncodeABC(OP_RETURN, 2, 0, 0));

    ClosureValue := VM.ExecuteFunction(Template);
    Expect<Boolean>(ClosureValue.IsCallable).ToBe(True);
    Expect<Double>(TGocciaFunctionBase(ClosureValue).Call(
      CallArgs, TGocciaUndefinedLiteralValue.UndefinedValue).ToNumberLiteral.Value).ToBe(1);
    Expect<Double>(TGocciaFunctionBase(ClosureValue).Call(
      CallArgs, TGocciaUndefinedLiteralValue.UndefinedValue).ToNumberLiteral.Value).ToBe(2);
    Expect<Double>(TGocciaFunctionBase(ClosureValue).Call(
      CallArgs, TGocciaUndefinedLiteralValue.UndefinedValue).ToNumberLiteral.Value).ToBe(3);
  finally
    CallArgs.Free;
    VM.Free;
    Template.Free;
  end;
end;

begin
  TestRunnerProgram.AddSuite(TTestGocciaVM.Create('Goccia VM'));
  TestRunnerProgram.Run;

  ExitCode := TestResultToExitCode;
end.
