program Goccia.VM.Test;

{$I Goccia.inc}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes,
  Math,
  SysUtils,

  TestingPascalLibrary,

  Goccia.Arguments.Collection,
  Goccia.Bytecode,
  Goccia.Bytecode.Chunk,
  Goccia.Constants.PropertyNames,
  Goccia.ExecutionContext,
  Goccia.InstructionLimit,
  Goccia.Modules,
  Goccia.Profiler,
  Goccia.Realm,
  Goccia.Scope,
  Goccia.TestSetup,
  Goccia.ThreadPolls,
  Goccia.Timeout,
  Goccia.Values.Error,
  Goccia.Values.FunctionBase,
  Goccia.Values.HoleValue,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives,
  Goccia.VM,
  Goccia.VM.Exception;

type
  TLimitedRunScenario = (lrsTimeout, lrsInstructionLimit);

  // Runs one template on its own thread under a limit armed on that thread.
  TLimitedRunThread = class(TThread)
  public
    VM: TGocciaVM;
    Template: TGocciaFunctionTemplate;
    Scenario: TLimitedRunScenario;
    Outcome: string;
    Finished: Boolean;
    procedure Execute; override;
  end;

  TTestGocciaVM = class(TTestSuite)
  private
    FRealm: TGocciaRealm;
    FRealmExecutionContext: TGocciaExecutionContextScope;
    procedure TestExecuteIntegerAddition;
    procedure TestExecuteIntegerMultiplicationSignOfZero;
    procedure TestExecuteLocalsRoundTrip;
    procedure TestExecuteLiteralLoads;
    procedure TestExecuteConstString;
    procedure TestExecuteComparisons;
    procedure TestExecuteSubtractNumberImmediate;
    procedure TestExecuteAddNumberImmediate;
    procedure TestExecuteNumberImmediateBranch;
    procedure TestExecuteJumpIfNotLessThan;
    procedure TestExecuteArrayOps;
    procedure TestExecuteArrayPop;
    procedure TestExecuteObjectOps;
    procedure TestExecuteLocalPropConst;
    procedure TestExecuteIndexedObjectOps;
    procedure TestExecuteClosureCall;
    procedure TestExecuteCapturedClosure;
    procedure TestProfileHostCallbackAllocations;
    procedure TestRestoreAllocationProfiling;
    function RunMissingImportBinding(const ADetachModule: Boolean): string;
    procedure TestDetachedModuleNamespaceImportRaisesSyntaxError;
    procedure TestMissingImportBindingNamesSpecifier;
    procedure TestGlobalReadCacheFollowsBindingTurnedImport;
    procedure TestPropertyCacheSlotsStayWithTheirConstant;
    procedure TestLimitsOfTheRunningThreadApplyAfterAnotherThreadEnteredFirst;
  protected
    procedure BeforeEach; override;
    procedure AfterEach; override;
  public
    procedure SetupTests; override;
  end;

procedure TTestGocciaVM.BeforeEach;
begin
  inherited BeforeEach;
  FRealm := TGocciaRealm.Create('<vm-test>');
  FRealmExecutionContext := TGocciaExecutionContextScope.Create(
    CreateExecutionContext(FRealm, nil, '<vm-test>'));
  TGocciaObjectValue.InitializeSharedPrototype;
  TGocciaFunctionBase.SetSharedPrototypeParent(
    TGocciaObjectValue.SharedObjectPrototype);
end;

procedure TTestGocciaVM.AfterEach;
begin
  FRealmExecutionContext.Free;
  FRealmExecutionContext := nil;
  FRealm.Free;
  FRealm := nil;
  inherited AfterEach;
end;

procedure TTestGocciaVM.SetupTests;
begin
  Test('Execute integer addition', TestExecuteIntegerAddition);
  Test('Integer multiplication gives -0 for a zero product with a negative ' +
    'operand', TestExecuteIntegerMultiplicationSignOfZero);
  Test('Execute locals round trip', TestExecuteLocalsRoundTrip);
  Test('Execute literal loads', TestExecuteLiteralLoads);
  Test('Execute constant string', TestExecuteConstString);
  Test('Execute comparisons', TestExecuteComparisons);
  Test('Execute Number subtract immediate', TestExecuteSubtractNumberImmediate);
  Test('Execute Number add immediate', TestExecuteAddNumberImmediate);
  Test('Execute Number immediate branch', TestExecuteNumberImmediateBranch);
  Test('Execute jump if not less than', TestExecuteJumpIfNotLessThan);
  Test('Execute array ops', TestExecuteArrayOps);
  Test('Execute array pop', TestExecuteArrayPop);
  Test('Execute object ops', TestExecuteObjectOps);
  Test('Execute local property const', TestExecuteLocalPropConst);
  Test('Execute indexed object ops', TestExecuteIndexedObjectOps);
  Test('Execute closure call', TestExecuteClosureCall);
  Test('Execute captured closure', TestExecuteCapturedClosure);
  Test('Profile host callback allocations', TestProfileHostCallbackAllocations);
  Test('Restore allocation profiling after return and throw',
    TestRestoreAllocationProfiling);
  Test('Detached module namespace import raises SyntaxError',
    TestDetachedModuleNamespaceImportRaisesSyntaxError);
  Test('Missing import binding names the specifier, not the host path',
    TestMissingImportBindingNamesSpecifier);
  Test('Global read cache follows a binding that becomes an import',
    TestGlobalReadCacheFollowsBindingTurnedImport);
  Test('Property cache slots stay with their constant and exist only inside the pool',
    TestPropertyCacheSlotsStayWithTheirConstant);
  Test('A VM entered on one thread reads the limits of the thread that runs it next',
    TestLimitsOfTheRunningThreadApplyAfterAnotherThreadEnteredFirst);
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

{ The compiler does not emit OP_MUL_INT for any source today, but a bytecode
  artifact can carry it, so its integer path is exercised directly. }
procedure TTestGocciaVM.TestExecuteIntegerMultiplicationSignOfZero;

  function Multiply(const ALeft, ARight: Integer): TGocciaNumberLiteralValue;
  var
    Template: TGocciaFunctionTemplate;
    VM: TGocciaVM;
  begin
    Template := TGocciaFunctionTemplate.Create('multiply');
    VM := TGocciaVM.Create;
    try
      Template.MaxRegisters := 3;
      Template.EmitInstruction(EncodeAsBx(OP_LOAD_INT, 0, ALeft));
      Template.EmitInstruction(EncodeAsBx(OP_LOAD_INT, 1, ARight));
      Template.EmitInstruction(EncodeABC(OP_MUL_INT, 2, 0, 1));
      Template.EmitInstruction(EncodeABC(OP_RETURN, 2, 0, 0));
      Result := VM.ExecuteFunction(Template).ToNumberLiteral;
    finally
      VM.Free;
      Template.Free;
    end;
  end;

begin
  Expect<Boolean>(Multiply(0, -1).IsNegativeZero).ToBe(True);
  Expect<Boolean>(Multiply(-1, 0).IsNegativeZero).ToBe(True);
  Expect<Boolean>(Multiply(0, 1).IsNegativeZero).ToBe(False);
  Expect<Boolean>(Multiply(0, 0).IsNegativeZero).ToBe(False);
  Expect<Double>(Multiply(-3, 7).Value).ToBe(-21);
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
  FirstValue, SecondValue: TGocciaValue;
  ConstIdx: UInt16;
begin
  Template := TGocciaFunctionTemplate.Create('const-string');
  VM := TGocciaVM.Create;
  try
    Template.MaxRegisters := 1;
    ConstIdx := Template.AddConstantString('hello');
    Template.EmitInstruction(EncodeABx(OP_LOAD_CONST, 0, ConstIdx));
    Template.EmitInstruction(EncodeABC(OP_RETURN, 0, 0, 0));

    FirstValue := VM.ExecuteFunction(Template);
    SecondValue := VM.ExecuteFunction(Template);
    Expect<string>(FirstValue.ToStringLiteral.Value).ToBe('hello');
    Expect<Boolean>(SecondValue = FirstValue).ToBe(True);
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

procedure TTestGocciaVM.TestExecuteSubtractNumberImmediate;
var
  Template: TGocciaFunctionTemplate;
  VM: TGocciaVM;
  ResultValue: TGocciaValue;
  FloatIndex: UInt16;
begin
  Template := TGocciaFunctionTemplate.Create('subtract-number-immediate');
  VM := TGocciaVM.Create;
  try
    Template.MaxRegisters := 2;
    Template.EmitInstruction(EncodeAsBx(OP_LOAD_INT, 0, 7));
    Template.EmitInstruction(EncodeABC(OP_SUB_NUM_IMM, 1, 0,
      UInt16(Int16(-2))));
    Template.EmitInstruction(EncodeABC(OP_RETURN, 1, 0, 0));
    ResultValue := VM.ExecuteFunction(Template);
    Expect<Double>(ResultValue.ToNumberLiteral.Value).ToBe(9);

    FloatIndex := Template.AddConstantFloat(7.5);
    Template.PatchInstruction(0, EncodeABx(OP_LOAD_CONST, 0, FloatIndex));
    ResultValue := VM.ExecuteFunction(Template);
    Expect<Double>(ResultValue.ToNumberLiteral.Value).ToBe(9.5);
  finally
    VM.Free;
    Template.Free;
  end;
end;

procedure TTestGocciaVM.TestExecuteAddNumberImmediate;
var
  Template: TGocciaFunctionTemplate;
  VM: TGocciaVM;
  ResultValue: TGocciaValue;
  FloatIndex: UInt16;
begin
  Template := TGocciaFunctionTemplate.Create('add-number-immediate');
  VM := TGocciaVM.Create;
  try
    Template.MaxRegisters := 2;
    Template.EmitInstruction(EncodeAsBx(OP_LOAD_INT, 0, 7));
    Template.EmitInstruction(EncodeABC(OP_ADD_NUM_IMM, 1, 0,
      UInt16(Int16(-2))));
    Template.EmitInstruction(EncodeABC(OP_RETURN, 1, 0, 0));
    ResultValue := VM.ExecuteFunction(Template);
    Expect<Double>(ResultValue.ToNumberLiteral.Value).ToBe(5);

    FloatIndex := Template.AddConstantFloat(7.5);
    Template.PatchInstruction(0, EncodeABx(OP_LOAD_CONST, 0, FloatIndex));
    ResultValue := VM.ExecuteFunction(Template);
    Expect<Double>(ResultValue.ToNumberLiteral.Value).ToBe(5.5);
  finally
    VM.Free;
    Template.Free;
  end;
end;

procedure TTestGocciaVM.TestExecuteNumberImmediateBranch;
var
  Template: TGocciaFunctionTemplate;
  VM: TGocciaVM;
  ResultValue: TGocciaValue;
  NaNIndex: UInt16;
begin
  Template := TGocciaFunctionTemplate.Create('number-immediate-branch');
  VM := TGocciaVM.Create;
  try
    Template.MaxRegisters := 2;
    Template.EmitInstruction(EncodeAsBx(OP_LOAD_INT, 0, 2));
    Template.EmitInstruction(EncodeABC(OP_JUMP_IF_NUM_NOT_LTE_IMM,
      0, UInt16(Int16(1)), UInt16(Int16(2))), True);
    Template.EmitInstruction(EncodeAsBx(OP_LOAD_INT, 1, 99));
    Template.EmitInstruction(EncodeABC(OP_RETURN, 1, 0, 0));
    Template.EmitInstruction(EncodeAsBx(OP_LOAD_INT, 1, 7));
    Template.EmitInstruction(EncodeABC(OP_RETURN, 1, 0, 0));

    ResultValue := VM.ExecuteFunction(Template);
    Expect<Double>(ResultValue.ToNumberLiteral.Value).ToBe(7);

    Template.PatchInstruction(0, EncodeAsBx(OP_LOAD_INT, 0, 1));
    ResultValue := VM.ExecuteFunction(Template);
    Expect<Double>(ResultValue.ToNumberLiteral.Value).ToBe(99);

    NaNIndex := Template.AddConstantFloat(NaN);
    Template.PatchInstruction(0, EncodeABx(OP_LOAD_CONST, 0, NaNIndex));
    ResultValue := VM.ExecuteFunction(Template);
    Expect<Double>(ResultValue.ToNumberLiteral.Value).ToBe(7);
  finally
    VM.Free;
    Template.Free;
  end;
end;

procedure TTestGocciaVM.TestExecuteJumpIfNotLessThan;
var
  Template: TGocciaFunctionTemplate;
  VM: TGocciaVM;
  ResultValue: TGocciaValue;
begin
  Template := TGocciaFunctionTemplate.Create('jump-if-not-lt');
  VM := TGocciaVM.Create;
  try
    Template.MaxRegisters := 3;
    Template.EmitInstruction(EncodeAsBx(OP_LOAD_INT, 0, 2));
    Template.EmitInstruction(EncodeAsBx(OP_LOAD_INT, 1, 5));
    Template.EmitInstruction(EncodeABC(OP_JUMP_IF_NOT_LT, 0, 1,
      UInt16(Int16(2))), True);
    Template.EmitInstruction(EncodeAsBx(OP_LOAD_INT, 2, 99));
    Template.EmitInstruction(EncodeABC(OP_RETURN, 2, 0, 0));
    Template.EmitInstruction(EncodeAsBx(OP_LOAD_INT, 2, 7));
    Template.EmitInstruction(EncodeABC(OP_RETURN, 2, 0, 0));

    ResultValue := VM.ExecuteFunction(Template);
    Expect<Double>(ResultValue.ToNumberLiteral.Value).ToBe(99);

    Template.PatchInstruction(0, EncodeAsBx(OP_LOAD_INT, 0, 5));
    Template.PatchInstruction(1, EncodeAsBx(OP_LOAD_INT, 1, 2));
    ResultValue := VM.ExecuteFunction(Template);
    Expect<Double>(ResultValue.ToNumberLiteral.Value).ToBe(7);
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

procedure TTestGocciaVM.TestExecuteLocalPropConst;
var
  Template: TGocciaFunctionTemplate;
  VM: TGocciaVM;
  ResultValue: TGocciaValue;
  NameIdx: UInt16;
  RaisedExpected: Boolean;
  ErrorObject: TGocciaObjectValue;
begin
  Template := TGocciaFunctionTemplate.Create('local-prop-const');
  VM := TGocciaVM.Create;
  try
    Template.MaxRegisters := 2;
    NameIdx := Template.AddConstantString('answer');
    Template.EmitInstruction(EncodeABx(OP_NEW_OBJECT, 0, 0));
    Template.EmitInstruction(EncodeAsBx(OP_LOAD_INT, 1, 42));
    Template.EmitInstruction(EncodeABC(OP_SET_PROP_CONST, 0, UInt8(NameIdx), 1));
    Template.EmitInstruction(EncodeABC(OP_GET_LOCAL_PROP_CONST, 1, 0,
      UInt8(NameIdx)));
    Template.EmitInstruction(EncodeABC(OP_RETURN, 1, 0, 0));

    ResultValue := VM.ExecuteFunction(Template);
    Expect<Double>(ResultValue.ToNumberLiteral.Value).ToBe(42);
  finally
    VM.Free;
    Template.Free;
  end;

  Template := TGocciaFunctionTemplate.Create('local-prop-const-tdz');
  VM := TGocciaVM.Create;
  try
    Template.MaxRegisters := 2;
    NameIdx := Template.AddConstantString('answer');
    Template.EmitInstruction(EncodeABC(OP_LOAD_HOLE, 0, 0, 0));
    Template.EmitInstruction(EncodeABC(OP_GET_LOCAL_PROP_CONST, 1, 0,
      UInt8(NameIdx)));
    Template.EmitInstruction(EncodeABC(OP_RETURN, 1, 0, 0));

    RaisedExpected := False;
    try
      VM.ExecuteFunction(Template);
    except
      on E: EGocciaBytecodeThrow do
        if E.ThrownValue is TGocciaObjectValue then
        begin
          ErrorObject := TGocciaObjectValue(E.ThrownValue);
          RaisedExpected :=
            ErrorObject.GetProperty(PROP_NAME).ToStringLiteral.Value =
              'ReferenceError';
        end;
      on E: TGocciaThrowValue do
        if E.Value is TGocciaObjectValue then
        begin
          ErrorObject := TGocciaObjectValue(E.Value);
          RaisedExpected :=
            ErrorObject.GetProperty(PROP_NAME).ToStringLiteral.Value =
              'ReferenceError';
        end;
    end;
    Expect<Boolean>(RaisedExpected).ToBe(True);
  finally
    VM.Free;
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
    Template.EmitInstruction(EncodeABx(OP_CLOSE_UPVALUE, 0, 1));
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

procedure TTestGocciaVM.TestProfileHostCallbackAllocations;
var
  Template, ChildTemplate: TGocciaFunctionTemplate;
  VM: TGocciaVM;
  Callback: TGocciaFunctionBase;
  Arguments: TGocciaArgumentsCollection;
  FirstAllocations: Int64;
begin
  TGocciaProfiler.Initialize;
  Template := TGocciaFunctionTemplate.Create('register');
  ChildTemplate := TGocciaFunctionTemplate.Create('allocate');
  VM := TGocciaVM.Create;
  Arguments := TGocciaArgumentsCollection.Create;
  try
    VM.ProfilingFunctions := True;
    ChildTemplate.MaxRegisters := 1;
    ChildTemplate.EmitInstruction(EncodeABC(OP_NEW_OBJECT, 0, 0, 0));
    ChildTemplate.EmitInstruction(EncodeABC(OP_RETURN, 0, 0, 0));
    Template.MaxRegisters := 1;
    Template.AddFunction(ChildTemplate);
    Template.EmitInstruction(EncodeABx(OP_CLOSURE, 0, 0));
    Template.EmitInstruction(EncodeABC(OP_RETURN, 0, 0, 0));
    Callback := TGocciaFunctionBase(VM.ExecuteFunction(Template));
    TGocciaProfiler.Instance.ResetCounts;

    Callback.Call(Arguments, TGocciaUndefinedLiteralValue.UndefinedValue);
    FirstAllocations := TGocciaProfiler.Instance.GetFunctionProfile(
      ChildTemplate.ProfileIndex).Allocations;
    Expect<Boolean>(FirstAllocations > 0).ToBe(True);
    Expect<Boolean>(GThreadPolls.ProfilingAllocations).ToBe(False);
    Callback.Call(Arguments, TGocciaUndefinedLiteralValue.UndefinedValue);
    Expect<Int64>(TGocciaProfiler.Instance.GetFunctionProfile(
      ChildTemplate.ProfileIndex).Allocations).ToBe(FirstAllocations * 2);
    Expect<Int64>(TGocciaProfiler.Instance.GetFunctionProfile(
      Template.ProfileIndex).Allocations).ToBe(0);
    Expect<Boolean>(GThreadPolls.ProfilingAllocations).ToBe(False);
  finally
    Arguments.Free;
    VM.Free;
    Template.Free;
    TGocciaProfiler.Shutdown;
  end;
end;

procedure TTestGocciaVM.TestRestoreAllocationProfiling;
var
  CallerObject: TGocciaObjectValue;
  Template: TGocciaFunctionTemplate;
  VM: TGocciaVM;
  PreviousEnabled, Profiled, Throws, RaisedExpected: Boolean;
  CallerIndex: Integer;
begin
  TGocciaProfiler.Initialize;
  Template := TGocciaFunctionTemplate.Create('allocate');
  VM := TGocciaVM.Create;
  try
    Template.MaxRegisters := 1;
    Template.EmitInstruction(EncodeABC(OP_NEW_OBJECT, 0, 0, 0));
    Template.EmitInstruction(EncodeABC(OP_RETURN, 0, 0, 0));
    CallerIndex := TGocciaProfiler.Instance.RegisterTemplate('caller', '', 0);
    for PreviousEnabled := False to True do
      for Profiled := False to True do
        for Throws := False to True do
        begin
          TGocciaProfiler.Instance.ResetCounts;
          TGocciaProfiler.Instance.PushFunction(CallerIndex, 0);
          GThreadPolls.ProfilingAllocations := PreviousEnabled;
          VM.ProfilingFunctions := Profiled;
          if Throws then
            Template.PatchInstruction(1, EncodeABC(OP_THROW, 0, 0, 0))
          else
            Template.PatchInstruction(1, EncodeABC(OP_RETURN, 0, 0, 0));
          RaisedExpected := False;
          try
            VM.ExecuteFunction(Template);
          except
            on E: EGocciaBytecodeThrow do
              RaisedExpected := True;
          end;
          Expect<Boolean>(RaisedExpected).ToBe(Throws);
          Expect<Boolean>(GThreadPolls.ProfilingAllocations).ToBe(PreviousEnabled);
          // An unprofiled entry must not charge its objects to the caller.
          Expect<Int64>(TGocciaProfiler.Instance.GetFunctionProfile(
            CallerIndex).Allocations).ToBe(0);
          // The caller remains on the profiling stack after either exit path.
          GThreadPolls.ProfilingAllocations := True;
          CallerObject := TGocciaObjectValue.Create;
          try
            Expect<Int64>(TGocciaProfiler.Instance.GetFunctionProfile(
              CallerIndex).Allocations).ToBe(1);
          finally
            CallerObject.Free;
          end;
        end;
  finally
    GThreadPolls.ProfilingAllocations := False;
    VM.Free;
    Template.Free;
    TGocciaProfiler.Shutdown;
  end;
end;

{ Runs OP_GET_IMPORT_BINDING for a missing export of a namespace whose module
  was loaded from a host path through the specifier "./dependency.js", and
  returns the thrown error as "<name>: <message>". }
function TTestGocciaVM.RunMissingImportBinding(
  const ADetachModule: Boolean): string;
var
  ErrorObject: TGocciaObjectValue;
  MissingNameIndex: UInt16;
  Module: TGocciaModule;
  NamespaceNameIndex: UInt16;
  NamespaceObject: TGocciaModuleNamespaceObject;
  RequestIndex: UInt16;
  Scope: TGocciaScope;
  Template: TGocciaFunctionTemplate;
  VM: TGocciaVM;
begin
  Result := '';
  Module := TGocciaModule.Create('/host/project/dependency.js');
  Scope := TGocciaScope.Create(nil, skGlobal, 'missing-import');
  Template := TGocciaFunctionTemplate.Create('missing-import');
  VM := TGocciaVM.Create;
  try
    NamespaceObject := TGocciaModuleNamespaceObject(
      Module.GetNamespaceObject);
    Scope.DefineLexicalBinding('namespace', NamespaceObject, dtConst);
    if ADetachModule then
    begin
      Module.Free;
      Module := nil;
    end;

    VM.GlobalScope := Scope;
    VM.Realm := FRealm;
    Template.MaxRegisters := 1;
    NamespaceNameIndex := Template.AddConstantString('namespace');
    MissingNameIndex := Template.AddConstantString('missing');
    RequestIndex := Template.AddConstantString('./dependency.js');
    Template.EmitInstruction(EncodeABx(OP_GET_GLOBAL, 0,
      NamespaceNameIndex));
    Template.EmitInstruction(EncodeABC(OP_GET_IMPORT_BINDING, 0,
      MissingNameIndex, RequestIndex));
    Template.EmitInstruction(EncodeABC(OP_RETURN, 0, 0, 0));

    ErrorObject := nil;
    try
      VM.ExecuteFunction(Template);
    except
      on E: EGocciaBytecodeThrow do
        if E.ThrownValue is TGocciaObjectValue then
          ErrorObject := TGocciaObjectValue(E.ThrownValue);
      on E: TGocciaThrowValue do
        if E.Value is TGocciaObjectValue then
          ErrorObject := TGocciaObjectValue(E.Value);
    end;
    if Assigned(ErrorObject) then
      Result := ErrorObject.GetProperty(PROP_NAME).ToStringLiteral.Value +
        ': ' + ErrorObject.GetProperty(PROP_MESSAGE).ToStringLiteral.Value;
  finally
    if Assigned(Module) then
      Module.Free;
    VM.GlobalScope := nil;
    VM.Realm := nil;
    VM.Free;
    Template.Free;
    Scope.Free;
  end;
end;

procedure TTestGocciaVM.TestDetachedModuleNamespaceImportRaisesSyntaxError;
begin
  Expect<string>(RunMissingImportBinding(True)).ToBe(
    'SyntaxError: Module has no export named "missing"');
end;

procedure TTestGocciaVM.TestMissingImportBindingNamesSpecifier;
begin
  Expect<string>(RunMissingImportBinding(False)).ToBe(
    'SyntaxError: Module "./dependency.js" has no export named "missing"');
end;

procedure TTestGocciaVM.TestGlobalReadCacheFollowsBindingTurnedImport;
var
  Module: TGocciaModule;
  Scope: TGocciaScope;
  Template: TGocciaFunctionTemplate;
  VM: TGocciaVM;
begin
  Module := TGocciaModule.Create('/host/project/dependency.js');
  Scope := TGocciaScope.Create(nil, skGlobal, 'import-cache');
  Template := TGocciaFunctionTemplate.Create('import-cache');
  VM := TGocciaVM.Create;
  try
    Scope.DefineLexicalBinding('value', TGocciaNumberLiteralValue.Create(3),
      dtLet);
    VM.GlobalScope := Scope;
    VM.Realm := FRealm;
    Template.MaxRegisters := 1;
    Template.EmitInstruction(EncodeABx(OP_GET_GLOBAL, 0,
      Template.AddConstantString('value')));
    Template.EmitInstruction(EncodeABC(OP_RETURN, 0, 0, 0));

    // The first read fills the site's cache; the later ones are served by it
    // and must still observe the binding's current value.
    Expect<Double>(VM.ExecuteFunction(Template).ToNumberLiteral.Value).ToBe(3);
    Expect<Double>(VM.ExecuteFunction(Template).ToNumberLiteral.Value).ToBe(3);
    Scope.AssignBinding('value', TGocciaNumberLiteralValue.Create(4));
    Expect<Double>(VM.ExecuteFunction(Template).ToNumberLiteral.Value).ToBe(4);

    // Turning the name into an import binding keeps its entry in place, so
    // the cached site has to notice and resolve through the import from then
    // on, including later changes of the exported value.
    Module.AddExportValue('value', TGocciaNumberLiteralValue.Create(7));
    Scope.CreateImportBinding('value', Module, 'value');
    Expect<Double>(VM.ExecuteFunction(Template).ToNumberLiteral.Value).ToBe(7);
    Module.UpdateExportValue('value', TGocciaNumberLiteralValue.Create(8));
    Expect<Double>(VM.ExecuteFunction(Template).ToNumberLiteral.Value).ToBe(8);
    Expect<Double>(VM.ExecuteFunction(Template).ToNumberLiteral.Value).ToBe(8);
  finally
    VM.GlobalScope := nil;
    VM.Realm := nil;
    VM.Free;
    Template.Free;
    Scope.Free;
    Module.Free;
  end;
end;

procedure TTestGocciaVM.TestPropertyCacheSlotsStayWithTheirConstant;
const
  CONSTANT_COUNT = 12;
  FIRST_USED = 3;
var
  Template: TGocciaFunctionTemplate;
  FirstRead: PGocciaPropertyReadCacheEntry;
  FirstProto: PGocciaProtoReadCacheEntry;
  FirstWrite: PGocciaPropertyWriteCacheEntry;
  I, BelowPool, AbovePool: Integer;
begin
  Template := TGocciaFunctionTemplate.Create('cache-slots');
  // Variables, not literals: the accessors are inlined, and FPC rejects a
  // literal index it can see is out of range.
  BelowPool := -1;
  AbovePool := CONSTANT_COUNT;
  try
    for I := 0 to CONSTANT_COUNT - 1 do
      Template.AddConstantString('name' + IntToStr(I));

    // An index outside the constant pool has no slot: the VM writes through
    // the pointer, so anything but nil there is a wild write.
    Expect<Boolean>(Template.PropertyReadCacheSlot(BelowPool) = nil).ToBe(True);
    Expect<Boolean>(Template.ProtoReadCacheSlot(BelowPool) = nil).ToBe(True);
    Expect<Boolean>(Template.PropertyWriteCacheSlot(BelowPool) = nil).ToBe(True);
    Expect<Boolean>(Template.PropertyReadCacheSlot(AbovePool) = nil).ToBe(True);
    Expect<Boolean>(Template.ProtoReadCacheSlot(AbovePool) = nil).ToBe(True);
    Expect<Boolean>(Template.PropertyWriteCacheSlot(AbovePool) = nil).ToBe(True);

    // The first use of a constant assigns its slot; the same constant then
    // resolves to that slot without the assignment path. The read and the
    // prototype tier share one slot map, so the read's first use assigns the
    // prototype slot too.
    FirstRead := Template.PropertyReadCacheSlot(FIRST_USED);
    FirstProto := Template.ProtoReadCacheSlot(FIRST_USED);
    FirstWrite := Template.PropertyWriteCacheSlot(FIRST_USED);
    Expect<Boolean>(Template.PropertyReadCacheSlot(FIRST_USED) =
      FirstRead).ToBe(True);
    Expect<Boolean>(Template.ProtoReadCacheSlot(FIRST_USED) =
      FirstProto).ToBe(True);
    Expect<Boolean>(Template.PropertyWriteCacheSlot(FIRST_USED) =
      FirstWrite).ToBe(True);
    FirstRead^.EntryIndex := 1000 + FIRST_USED;
    FirstProto^.EntryIndex := 2000 + FIRST_USED;
    FirstWrite^.EntryIndex := 3000 + FIRST_USED;

    // Every other constant gets a slot of its own, in an order that is not
    // the constant order, and enough of them to grow the entry arrays.
    for I := CONSTANT_COUNT - 1 downto 0 do
      if I <> FIRST_USED then
      begin
        Template.PropertyReadCacheSlot(I)^.EntryIndex := 1000 + I;
        Template.ProtoReadCacheSlot(I)^.EntryIndex := 2000 + I;
        Template.PropertyWriteCacheSlot(I)^.EntryIndex := 3000 + I;
      end;
    for I := 0 to CONSTANT_COUNT - 1 do
    begin
      Expect<Integer>(Template.PropertyReadCacheSlot(I)^.EntryIndex).ToBe(1000 + I);
      Expect<Integer>(Template.ProtoReadCacheSlot(I)^.EntryIndex).ToBe(2000 + I);
      Expect<Integer>(Template.PropertyWriteCacheSlot(I)^.EntryIndex).ToBe(3000 + I);
    end;

    Expect<Boolean>(Template.PropertyReadCacheSlot(BelowPool) = nil).ToBe(True);
    Expect<Boolean>(Template.ProtoReadCacheSlot(AbovePool) = nil).ToBe(True);
    AbovePool := High(Integer);
    Expect<Boolean>(Template.PropertyWriteCacheSlot(AbovePool) = nil).ToBe(True);
  finally
    Template.Free;
  end;
end;

procedure TLimitedRunThread.Execute;
const
  // Long enough never to fire in a passing run. It ends the loop when the
  // limit under test was not seen and the test thread releases this one.
  SAFETY_TIMEOUT_MS = 1500;
begin
  try
    case Scenario of
      lrsTimeout:
        StartExecutionTimeout(50);
      lrsInstructionLimit:
      begin
        StartExecutionTimeout(SAFETY_TIMEOUT_MS);
        StartInstructionLimit(1000);
      end;
    end;
    try
      VM.ExecuteFunction(Template);
      Outcome := 'returned';
    except
      on E: Exception do
        Outcome := E.ClassName;
    end;
  finally
    ClearExecutionTimeout;
    ClearInstructionLimit;
    Finished := True;
  end;
end;

procedure TTestGocciaVM.TestLimitsOfTheRunningThreadApplyAfterAnotherThreadEnteredFirst;
const
  WAIT_STEP_MS = 10;
  WAIT_LIMIT_MS = 1000;
var
  VM: TGocciaVM;
  Warmup, Spin: TGocciaFunctionTemplate;
  Worker: TLimitedRunThread;
  Scenario: TLimitedRunScenario;
  Waited: Integer;
  SawLimitInTime: Boolean;
begin
  for Scenario := Low(TLimitedRunScenario) to High(TLimitedRunScenario) do
  begin
    VM := TGocciaVM.Create;
    Warmup := TGocciaFunctionTemplate.Create('warmup');
    Spin := TGocciaFunctionTemplate.Create('spin');
    Worker := TLimitedRunThread.Create(True);
    try
      // This thread creates the VM and enters it first, so whatever the VM
      // binds per thread is bound here, where nothing is armed.
      Warmup.MaxRegisters := 1;
      Warmup.EmitInstruction(EncodeAsBx(OP_LOAD_INT, 0, 1));
      Warmup.EmitInstruction(EncodeABC(OP_RETURN, 0, 0, 0));
      VM.ExecuteFunction(Warmup);
      Expect<Boolean>(GThreadPolls.Any = 0).ToBe(True);

      // A jump to itself: no allocation and no call, so the loop ends only
      // if the dispatch loop sees the limit armed on the thread running it.
      Spin.MaxRegisters := 1;
      Spin.EmitInstruction(EncodeAx(OP_JUMP, -1));
      Spin.EmitInstruction(EncodeABC(OP_RETURN, 0, 0, 0));

      Worker.VM := VM;
      Worker.Template := Spin;
      Worker.Scenario := Scenario;
      Worker.Start;
      Waited := 0;
      while (not Worker.Finished) and (Waited < WAIT_LIMIT_MS) do
      begin
        Sleep(WAIT_STEP_MS);
        Inc(Waited, WAIT_STEP_MS);
      end;
      SawLimitInTime := Worker.Finished;
      if not SawLimitInTime then
        // The loop is reading this thread's word. Arming it here makes the
        // loop poll, and the worker's own deadline then ends it.
        StartExecutionTimeout(60000);
      Worker.WaitFor;
      ClearExecutionTimeout;

      Expect<Boolean>(SawLimitInTime).ToBe(True);
      if Scenario = lrsTimeout then
        Expect<string>(Worker.Outcome).ToBe('TGocciaTimeoutError')
      else
        Expect<string>(Worker.Outcome).ToBe('TGocciaInstructionLimitError');
      Expect<Boolean>(GThreadPolls.Any = 0).ToBe(True);
    finally
      Worker.Free;
      VM.Free;
      Spin.Free;
      Warmup.Free;
    end;
  end;
end;

begin
  TestRunnerProgram.AddSuite(TTestGocciaVM.Create('Goccia VM'));
  RunGocciaTests;

  ExitCode := TestResultToExitCode;
end.
