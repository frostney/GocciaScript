program Goccia.Bytecode.Binary.Test;

{$I Goccia.inc}

uses
  Classes,
  SysUtils,

  TestingPascalLibrary,

  Goccia.Bytecode,
  Goccia.Bytecode.Binary,
  Goccia.Bytecode.Chunk,
  Goccia.Bytecode.Debug,
  Goccia.Bytecode.Module;

const
  // Small enough that an operand above it is unambiguously out of range.
  TEST_MAX_REGISTERS = 4;
  TEST_OUT_OF_RANGE_REGISTER = 9;
  TEST_IN_RANGE_REGISTER = 1;
  TEST_RUNTIME_TAG = 'test';
  TEST_SOURCE_PATH = 'validate.js';
  TEST_LOCAL_NAME = 'binding';
  TEST_LOCAL_SLOT = 3;
  TEST_LOCAL_START_PC = 7;
  TEST_LOCAL_END_PC = 11;

type
  TBytecodeBinaryTests = class(TTestSuite)
  private
    function LoadRejectionReason(const AInstruction: UInt64): string;
    procedure TestRejectsMemberValidateKeyRegisterOutOfRange;
    procedure TestAcceptsMemberValidateKeyRegisterInRange;
    procedure TestAcceptsIterableValidateBoundAboveRegisterCount;
    procedure TestAcceptsObjectValidateWithUnusedOperandC;
    procedure TestLoadedClosedNumericSelfCallIsDeSpecialized;
    procedure TestLoadedClosedNumericSelfCallSavesAndReloads;
    procedure TestRejectsClosedNumericSelfCallInNonArrowTemplate;
    procedure TestRoundTripsDebugLocals;
    procedure TestRoundTripsDirectEvalSiteRecord;
  public
    procedure SetupTests; override;
  end;

procedure TBytecodeBinaryTests.SetupTests;
begin
  Test('Rejects a member-validate key register outside MaxRegisters',
    TestRejectsMemberValidateKeyRegisterOutOfRange);
  Test('Accepts a member-validate key register inside MaxRegisters',
    TestAcceptsMemberValidateKeyRegisterInRange);
  Test('Accepts an iterable-validate bound above MaxRegisters',
    TestAcceptsIterableValidateBoundAboveRegisterCount);
  Test('Accepts an object-validate with an unused operand C',
    TestAcceptsObjectValidateWithUnusedOperandC);
  Test('A loaded closed numeric self-call is de-specialized to OP_CALL_SELF',
    TestLoadedClosedNumericSelfCallIsDeSpecialized);
  Test('A loaded closed numeric self-call saves and loads again',
    TestLoadedClosedNumericSelfCallSavesAndReloads);
  Test('Rejects a closed numeric self-call in a non-arrow template',
    TestRejectsClosedNumericSelfCallInNonArrowTemplate);
  Test('Round-trips debug locals field by field',
    TestRoundTripsDebugLocals);
  Test('Round-trips a direct eval site record field by field',
    TestRoundTripsDirectEvalSiteRecord);
end;

// Serialises a one-instruction module and reads it back through the ordinary
// loader path, so the verifier sees exactly the operands under test. Returns
// the rejection message, or an empty string when the module loaded.
function TBytecodeBinaryTests.LoadRejectionReason(
  const AInstruction: UInt64): string;
var
  Loaded, Module: TGocciaBytecodeModule;
  Reader: TGocciaBytecodeReader;
  Stream: TMemoryStream;
  Template: TGocciaFunctionTemplate;
  Writer: TGocciaBytecodeWriter;
begin
  Result := '';
  Stream := TMemoryStream.Create;
  try
    Module := TGocciaBytecodeModule.Create(TEST_RUNTIME_TAG, TEST_SOURCE_PATH);
    try
      Template := TGocciaFunctionTemplate.Create('main');
      Template.MaxRegisters := TEST_MAX_REGISTERS;
      Template.EmitInstruction(AInstruction);
      Module.TopLevel := Template;
      Module.HasDebugInfo := False;

      Writer := TGocciaBytecodeWriter.Create(Stream);
      try
        Writer.WriteModule(Module);
      finally
        Writer.Free;
      end;
    finally
      Module.Free;
    end;

    Stream.Position := 0;
    Reader := TGocciaBytecodeReader.Create(Stream);
    try
      try
        Loaded := Reader.ReadModule;
        Loaded.Free;
      except
        on E: Exception do
          Result := E.Message;
      end;
    finally
      Reader.Free;
    end;
  finally
    Stream.Free;
  end;
end;

// Operand C of OP_VALIDATE_VALUE is a register only in the computed-member
// mode, so GocciaOpCodeUsesRegisterC cannot cover it and the verifier has to
// check it per mode. Without that check a crafted .gbc reaches the VM's
// FRegisters[C] read with an out-of-bounds index.
procedure TBytecodeBinaryTests.TestRejectsMemberValidateKeyRegisterOutOfRange;
var
  Reason: string;
begin
  Reason := LoadRejectionReason(EncodeABC(OP_VALIDATE_VALUE, 0,
    VALIDATE_OP_REQUIRE_OBJECT_FOR_MEMBER, TEST_OUT_OF_RANGE_REGISTER));
  Expect<Boolean>(Pos('register 9 is outside MaxRegisters 4', Reason) > 0)
    .ToBe(True);
end;

procedure TBytecodeBinaryTests.TestAcceptsMemberValidateKeyRegisterInRange;
begin
  Expect<string>(LoadRejectionReason(EncodeABC(OP_VALIDATE_VALUE, 0,
    VALIDATE_OP_REQUIRE_OBJECT_FOR_MEMBER, TEST_IN_RANGE_REGISTER))).ToBe('');
end;

// The iterable mode encodes an element count in C, not a register, so a value
// above MaxRegisters is legitimate and must still load.
procedure TBytecodeBinaryTests.TestAcceptsIterableValidateBoundAboveRegisterCount;
begin
  Expect<string>(LoadRejectionReason(EncodeABC(OP_VALIDATE_VALUE, 0,
    VALIDATE_OP_REQUIRE_ITERABLE, ITERABLE_LIMIT_UNBOUNDED))).ToBe('');
end;

procedure TBytecodeBinaryTests.TestAcceptsObjectValidateWithUnusedOperandC;
begin
  Expect<string>(LoadRejectionReason(EncodeABC(OP_VALIDATE_VALUE, 0,
    VALIDATE_OP_REQUIRE_OBJECT, TEST_OUT_OF_RANGE_REGISTER))).ToBe('');
end;

// ADR 0101's closed numeric proof is a compiler-only fact that is not
// serialized, and ADR 0127's collector change stops marking the registers of a
// closed numeric frame. A loaded .gbc could otherwise hold an object in that
// unmarked window and have the collector reclaim it. The loader de-specializes
// a loaded OP_CALL_SELF_NUM in a synchronous arrow to the ordinary self-call
// OP_CALL_SELF, whose frame the collector marks. This builds a module whose
// one (arrow) function recursively calls itself numerically, loads it, and
// checks the loaded code holds OP_CALL_SELF, not OP_CALL_SELF_NUM.
// A module loaded from a file holds the runtime-only OP_CALL_SELF, which the
// verifier rejects in a file. Saving that module must write OP_CALL_SELF_NUM
// back, so a load-save-load round trip succeeds and de-specializes again.
procedure TBytecodeBinaryTests.TestLoadedClosedNumericSelfCallSavesAndReloads;
var
  Module, Loaded, Reloaded: TGocciaBytecodeModule;
  Arrow: TGocciaFunctionTemplate;
  Reader: TGocciaBytecodeReader;
  Writer: TGocciaBytecodeWriter;
  Stream: TMemoryStream;
  Reason: string;
begin
  Stream := TMemoryStream.Create;
  try
    Module := TGocciaBytecodeModule.Create(TEST_RUNTIME_TAG,
      TEST_SOURCE_PATH);
    try
      Arrow := TGocciaFunctionTemplate.Create('fib');
      Arrow.MaxRegisters := TEST_MAX_REGISTERS;
      Arrow.ParameterCount := 1;
      Arrow.IsArrow := True;
      Arrow.EmitInstruction(EncodeABC(OP_CALL_SELF_NUM, 2, 1, 1));
      Arrow.EmitInstruction(EncodeABC(OP_RETURN, 2, 0, 0));
      Module.TopLevel := TGocciaFunctionTemplate.Create('main');
      Module.TopLevel.MaxRegisters := TEST_MAX_REGISTERS;
      Module.TopLevel.AddFunction(Arrow);
      Module.TopLevel.EmitInstruction(EncodeABC(OP_RETURN, 0, 0, 0));
      Module.HasDebugInfo := False;
      Writer := TGocciaBytecodeWriter.Create(Stream);
      try
        Writer.WriteModule(Module);
      finally
        Writer.Free;
      end;
    finally
      Module.Free;
    end;

    Stream.Position := 0;
    Reader := TGocciaBytecodeReader.Create(Stream);
    try
      Loaded := Reader.ReadModule;
    finally
      Reader.Free;
    end;

    // Save the loaded module, which holds OP_CALL_SELF, and load it again.
    Stream.Clear;
    try
      Writer := TGocciaBytecodeWriter.Create(Stream);
      try
        Writer.WriteModule(Loaded);
      finally
        Writer.Free;
      end;
    finally
      Loaded.Free;
    end;

    Reason := '';
    Reloaded := nil;
    Stream.Position := 0;
    Reader := TGocciaBytecodeReader.Create(Stream);
    try
      try
        Reloaded := Reader.ReadModule;
      except
        on E: Exception do
          Reason := E.Message;
      end;
    finally
      Reader.Free;
    end;
    try
      Expect<string>(Reason).ToBe('');
      if Assigned(Reloaded) then
        Expect<Integer>(DecodeOp(Reloaded.TopLevel.GetFunction(0)
          .GetInstruction(0))).ToBe(OP_CALL_SELF);
    finally
      Reloaded.Free;
    end;
  finally
    Stream.Free;
  end;
end;

procedure TBytecodeBinaryTests.TestLoadedClosedNumericSelfCallIsDeSpecialized;
var
  Module, Loaded: TGocciaBytecodeModule;
  Arrow, LoadedArrow: TGocciaFunctionTemplate;
  Reader: TGocciaBytecodeReader;
  Writer: TGocciaBytecodeWriter;
  Stream: TMemoryStream;
  I: Integer;
  SelfNumCount, SelfCount: Integer;
begin
  Stream := TMemoryStream.Create;
  Module := TGocciaBytecodeModule.Create(TEST_RUNTIME_TAG, TEST_SOURCE_PATH);
  try
    Arrow := TGocciaFunctionTemplate.Create('fib');
    Arrow.MaxRegisters := TEST_MAX_REGISTERS;
    Arrow.ParameterCount := 1;
    Arrow.IsArrow := True;
    // OP_CALL_SELF_NUM dest=2, argbase=1, count=1 — a numeric self-call. The
    // body around it does not matter to the loader's instruction rewrite.
    Arrow.EmitInstruction(EncodeABC(OP_CALL_SELF_NUM, 2, 1, 1));
    Arrow.EmitInstruction(EncodeABC(OP_RETURN, 2, 0, 0));

    Module.TopLevel := TGocciaFunctionTemplate.Create('main');
    Module.TopLevel.MaxRegisters := TEST_MAX_REGISTERS;
    Module.TopLevel.AddFunction(Arrow);
    Module.TopLevel.EmitInstruction(EncodeABC(OP_RETURN, 0, 0, 0));
    Module.HasDebugInfo := False;

    Writer := TGocciaBytecodeWriter.Create(Stream);
    try
      Writer.WriteModule(Module);
    finally
      Writer.Free;
    end;
  finally
    Module.Free;
  end;

  Stream.Position := 0;
  Reader := TGocciaBytecodeReader.Create(Stream);
  try
    Loaded := Reader.ReadModule;
  finally
    Reader.Free;
    Stream.Free;
  end;

  try
    LoadedArrow := Loaded.TopLevel.GetFunction(0);
    SelfNumCount := 0;
    SelfCount := 0;
    for I := 0 to LoadedArrow.CodeCount - 1 do
    begin
      if DecodeOp(LoadedArrow.GetInstruction(I)) = Ord(OP_CALL_SELF_NUM) then
        Inc(SelfNumCount);
      if DecodeOp(LoadedArrow.GetInstruction(I)) = OP_CALL_SELF then
        Inc(SelfCount);
    end;
    Expect<Integer>(SelfNumCount).ToBe(0);
    Expect<Integer>(SelfCount).ToBe(1);
  finally
    Loaded.Free;
  end;
end;

// The closed numeric frame contract only ever applies to a synchronous arrow.
// A crafted .gbc that places OP_CALL_SELF_NUM in a non-arrow template is
// rejected rather than rewritten, so the opcode can never reach the VM from a
// template whose frame kind the rewrite does not cover.
procedure TBytecodeBinaryTests.TestRejectsClosedNumericSelfCallInNonArrowTemplate;
var
  Reason: string;
begin
  Reason := LoadRejectionReason(EncodeABC(OP_CALL_SELF_NUM, 2, 1, 1));
  Expect<Boolean>(Pos('closed numeric self-call outside a synchronous arrow',
    Reason) > 0).ToBe(True);
end;

// The VM names a temporal-dead-zone binding from these entries, so a module
// loaded from .gbc must read each one back with its fields in order.
procedure TBytecodeBinaryTests.TestRoundTripsDebugLocals;
var
  Loaded, Module: TGocciaBytecodeModule;
  Reader: TGocciaBytecodeReader;
  Stream: TMemoryStream;
  Template: TGocciaFunctionTemplate;
  Writer: TGocciaBytecodeWriter;
  Local: TGocciaLocalInfo;
  Name: string;
begin
  Stream := TMemoryStream.Create;
  try
    Module := TGocciaBytecodeModule.Create(TEST_RUNTIME_TAG, TEST_SOURCE_PATH);
    try
      Template := TGocciaFunctionTemplate.Create('main');
      Template.MaxRegisters := TEST_MAX_REGISTERS;
      Template.EmitInstruction(EncodeABC(OP_LOAD_UNDEFINED, 0, 0, 0));
      Template.DebugInfo := TGocciaDebugInfo.Create(TEST_SOURCE_PATH);
      Template.DebugInfo.AddLocal(TEST_LOCAL_NAME, TEST_LOCAL_SLOT,
        TEST_LOCAL_START_PC, TEST_LOCAL_END_PC);
      Module.TopLevel := Template;

      Writer := TGocciaBytecodeWriter.Create(Stream);
      try
        Writer.WriteModule(Module);
      finally
        Writer.Free;
      end;
    finally
      Module.Free;
    end;

    Stream.Position := 0;
    Reader := TGocciaBytecodeReader.Create(Stream);
    try
      Loaded := Reader.ReadModule;
      try
        Expect<Integer>(Loaded.TopLevel.DebugInfo.LocalCount).ToBe(1);
        Local := Loaded.TopLevel.DebugInfo.GetLocalInfo(0);
        Expect<string>(Local.Name).ToBe(TEST_LOCAL_NAME);
        Expect<Integer>(Local.Slot).ToBe(TEST_LOCAL_SLOT);
        Expect<Integer>(Local.StartPC).ToBe(TEST_LOCAL_START_PC);
        Expect<Integer>(Local.EndPC).ToBe(TEST_LOCAL_END_PC);
        Expect<Boolean>(Loaded.TopLevel.DebugInfo.TryGetLocalName(
          TEST_LOCAL_SLOT, TEST_LOCAL_START_PC, Name)).ToBe(True);
        Expect<string>(Name).ToBe(TEST_LOCAL_NAME);
        Expect<Boolean>(Loaded.TopLevel.DebugInfo.TryGetLocalName(
          TEST_LOCAL_SLOT, TEST_LOCAL_END_PC, Name)).ToBe(False);
      finally
        Loaded.Free;
      end;
    finally
      Reader.Free;
    end;
  finally
    Stream.Free;
  end;
end;

// The loader rebuilds a direct eval site record from the module, and the VM
// reads the call's strictness and each binding's flags from it.
procedure TBytecodeBinaryTests.TestRoundTripsDirectEvalSiteRecord;
const
  EVAL_PC = 5;
var
  Loaded, Module: TGocciaBytecodeModule;
  Reader: TGocciaBytecodeReader;
  Stream: TMemoryStream;
  Template: TGocciaFunctionTemplate;
  Writer: TGocciaBytecodeWriter;
  Bindings: TGocciaDirectEvalBindingArray;
  Env: TGocciaDirectEvalEnvironment;
begin
  SetLength(Bindings, 2);
  Bindings[0] := Default(TGocciaDirectEvalBindingInfo);
  Bindings[0].Name := 'caught';
  Bindings[0].Kind := debLocal;
  Bindings[0].Index := 2;
  Bindings[0].IsCatchParameter := True;
  Bindings[1] := Default(TGocciaDirectEvalBindingInfo);
  Bindings[1].Name := 'outer';
  Bindings[1].Kind := debUpvalue;
  Bindings[1].Index := 1;
  Bindings[1].IsConst := True;
  Bindings[1].IsVarEnvironmentBinding := True;
  Stream := TMemoryStream.Create;
  try
    Module := TGocciaBytecodeModule.Create(TEST_RUNTIME_TAG, TEST_SOURCE_PATH);
    try
      Template := TGocciaFunctionTemplate.Create('main');
      Template.MaxRegisters := TEST_MAX_REGISTERS;
      Template.EmitInstruction(EncodeABC(OP_LOAD_UNDEFINED, 0, 0, 0));
      Template.AddDirectEvalEnvironment(EVAL_PC, False, True, Bindings);
      Template.AddDirectEvalEnvironment(EVAL_PC + 1, True, False, nil);
      Module.TopLevel := Template;
      Module.HasDebugInfo := False;

      Writer := TGocciaBytecodeWriter.Create(Stream);
      try
        Writer.WriteModule(Module);
      finally
        Writer.Free;
      end;
    finally
      Module.Free;
    end;

    Stream.Position := 0;
    Reader := TGocciaBytecodeReader.Create(Stream);
    try
      Loaded := Reader.ReadModule;
      try
        Expect<Integer>(Loaded.TopLevel.DirectEvalEnvironmentCount).ToBe(2);
        Env := Loaded.TopLevel.GetDirectEvalEnvironment(0);
        Expect<Integer>(Env.PC).ToBe(EVAL_PC);
        Expect<Boolean>(Env.RejectArgumentsReference).ToBe(False);
        Expect<Boolean>(Env.StrictCaller).ToBe(True);
        Expect<Integer>(Length(Env.Bindings)).ToBe(2);
        Expect<string>(Env.Bindings[0].Name).ToBe('caught');
        Expect<Integer>(Ord(Env.Bindings[0].Kind)).ToBe(Ord(debLocal));
        Expect<Integer>(Env.Bindings[0].Index).ToBe(2);
        Expect<Boolean>(Env.Bindings[0].IsCatchParameter).ToBe(True);
        Expect<Boolean>(Env.Bindings[0].IsVarEnvironmentBinding).ToBe(False);
        Expect<string>(Env.Bindings[1].Name).ToBe('outer');
        Expect<Integer>(Ord(Env.Bindings[1].Kind)).ToBe(Ord(debUpvalue));
        Expect<Boolean>(Env.Bindings[1].IsConst).ToBe(True);
        Expect<Boolean>(Env.Bindings[1].IsVarEnvironmentBinding).ToBe(True);
        Expect<Boolean>(Env.Bindings[1].IsCatchParameter).ToBe(False);
        Env := Loaded.TopLevel.GetDirectEvalEnvironment(1);
        Expect<Boolean>(Env.RejectArgumentsReference).ToBe(True);
        Expect<Boolean>(Env.StrictCaller).ToBe(False);
        Expect<Integer>(Length(Env.Bindings)).ToBe(0);
      finally
        Loaded.Free;
      end;
    finally
      Reader.Free;
    end;
  finally
    Stream.Free;
  end;
end;

begin
  TestRunnerProgram.AddSuite(TBytecodeBinaryTests.Create('Bytecode Binary'));
  TestRunnerProgram.Run;
  ExitCode := TestResultToExitCode;
end.
