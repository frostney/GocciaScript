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
    procedure TestRoundTripsDebugLocals;
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
  Test('Round-trips debug locals field by field',
    TestRoundTripsDebugLocals);
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

begin
  TestRunnerProgram.AddSuite(TBytecodeBinaryTests.Create('Bytecode Binary'));
  TestRunnerProgram.Run;
  ExitCode := TestResultToExitCode;
end.
