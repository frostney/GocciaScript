program Goccia.Bytecode.Binary.Test;

{$I Goccia.inc}

uses
  Classes,
  SysUtils,

  TestingPascalLibrary,

  Goccia.Bytecode,
  Goccia.Bytecode.Binary,
  Goccia.Bytecode.Chunk,
  Goccia.Bytecode.Module;

const
  // Small enough that an operand above it is unambiguously out of range.
  TEST_MAX_REGISTERS = 4;
  TEST_OUT_OF_RANGE_REGISTER = 9;
  TEST_IN_RANGE_REGISTER = 1;
  TEST_RUNTIME_TAG = 'test';
  TEST_SOURCE_PATH = 'validate.js';

type
  TBytecodeBinaryTests = class(TTestSuite)
  private
    function LoadRejectionReason(const AInstruction: UInt64): string;
    procedure TestRejectsMemberValidateKeyRegisterOutOfRange;
    procedure TestAcceptsMemberValidateKeyRegisterInRange;
    procedure TestAcceptsIterableValidateBoundAboveRegisterCount;
    procedure TestAcceptsObjectValidateWithUnusedOperandC;
    procedure TestLoadedClosedNumericSelfCallIsDeSpecialized;
    procedure TestRejectsClosedNumericSelfCallInNonArrowTemplate;
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
  Test('Rejects a closed numeric self-call in a non-arrow template',
    TestRejectsClosedNumericSelfCallInNonArrowTemplate);
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

begin
  TestRunnerProgram.AddSuite(TBytecodeBinaryTests.Create('Bytecode Binary'));
  TestRunnerProgram.Run;
  ExitCode := TestResultToExitCode;
end.
