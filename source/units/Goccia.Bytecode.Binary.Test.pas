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
    procedure TestRejectsMalformedDirectEvalSite;
    procedure TestRefusesGlobalEvalCode;
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
  Test('Rejects a malformed direct eval site',
    TestRejectsMalformedDirectEvalSite);
  Test('Refuses to write eval code, which lives only in memory',
    TestRefusesGlobalEvalCode);
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

// Writes ATemplate as the top level of a module and reads it back through the
// ordinary loader path. Returns the loaded module, or nil with the rejection
// message in AReason.
function LoadTemplateThroughBinary(const ATemplate: TGocciaFunctionTemplate;
  out AReason: string): TGocciaBytecodeModule;
var
  Module: TGocciaBytecodeModule;
  Reader: TGocciaBytecodeReader;
  Stream: TMemoryStream;
  Writer: TGocciaBytecodeWriter;
begin
  Result := nil;
  AReason := '';
  Stream := TMemoryStream.Create;
  try
    Module := TGocciaBytecodeModule.Create(TEST_RUNTIME_TAG, TEST_SOURCE_PATH);
    try
      Module.TopLevel := ATemplate;
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
        Result := Reader.ReadModule;
      except
        on E: Exception do
          AReason := E.Message;
      end;
    finally
      Reader.Free;
    end;
  finally
    Stream.Free;
  end;
end;

function DirectEvalBinding(const AName: string;
  const AKind: TGocciaDirectEvalBindingKind; const AIndex: UInt16;
  const AIsConst, AIsVarEnvironmentBinding, AIsEvalSyntheticArguments,
  AIsCatchParameter: Boolean): TGocciaDirectEvalBindingInfo;
begin
  Result := Default(TGocciaDirectEvalBindingInfo);
  Result.Name := AName;
  Result.Kind := AKind;
  Result.Index := AIndex;
  Result.IsConst := AIsConst;
  Result.IsVarEnvironmentBinding := AIsVarEnvironmentBinding;
  Result.IsEvalSyntheticArguments := AIsEvalSyntheticArguments;
  Result.IsCatchParameter := AIsCatchParameter;
end;

procedure ExpectDirectEvalBinding(const ABinding,
  AExpected: TGocciaDirectEvalBindingInfo);
begin
  Expect<string>(ABinding.Name).ToBe(AExpected.Name);
  Expect<Integer>(Ord(ABinding.Kind)).ToBe(Ord(AExpected.Kind));
  Expect<Integer>(ABinding.Index).ToBe(AExpected.Index);
  Expect<Boolean>(ABinding.IsConst).ToBe(AExpected.IsConst);
  Expect<Boolean>(ABinding.IsVarEnvironmentBinding).ToBe(
    AExpected.IsVarEnvironmentBinding);
  Expect<Boolean>(ABinding.IsEvalSyntheticArguments).ToBe(
    AExpected.IsEvalSyntheticArguments);
  Expect<Boolean>(ABinding.IsCatchParameter).ToBe(
    AExpected.IsCatchParameter);
end;

// The loader rebuilds every field of a direct eval site record from the
// module, and the OP_DIRECT_EVAL that names it. The records cover every
// binding kind, every flag both ways and every variable environment, so a
// field the writer or the reader drops or reorders fails here.
procedure TBytecodeBinaryTests.TestRoundTripsDirectEvalSiteRecord;
const
  EVAL_MAX_REGISTERS = 8;
  FUNCTION_VAR_OBJECT_SLOT = 6;
  PARAMETER_VAR_OBJECT_SLOT = 7;
var
  Loaded: TGocciaBytecodeModule;
  Template: TGocciaFunctionTemplate;
  Bindings: TGocciaDirectEvalBindingArray;
  Env: TGocciaDirectEvalEnvironment;
  Instruction: UInt32;
  Reason: string;
  I: Integer;
begin
  SetLength(Bindings, 5);
  Bindings[0] := DirectEvalBinding('caught', debLocal, 2, True, False, False,
    True);
  Bindings[1] := DirectEvalBinding('outer', debUpvalue, 1, False, True,
    False, False);
  Bindings[2] := DirectEvalBinding('global', debGlobal, 0, True, True, False,
    False);
  Bindings[3] := DirectEvalBinding('`#with`:0', debWithLocal, 3, False,
    False, True, False);
  Bindings[4] := DirectEvalBinding('`#with`:1', debWithUpvalue, 4, False,
    False, False, False);

  Template := TGocciaFunctionTemplate.Create('main');
  Template.MaxRegisters := EVAL_MAX_REGISTERS;
  // eval(x): a plain call with one argument.
  Template.EmitInstruction(EncodeABx(OP_DIRECT_EVAL, 1, 0));
  Template.EmitInstruction(EncodeABC(OP_CALL, 1, 1, 0));
  // eval(...args) found on a `with` object: a method call with a spread.
  Template.EmitInstruction(EncodeABx(OP_DIRECT_EVAL, 2, 1));
  Template.EmitInstruction(EncodeABC(OP_CALL_METHOD, 2, 3, CALL_FLAG_SPREAD));
  Template.EmitInstruction(EncodeABx(OP_DIRECT_EVAL, 1, 2));
  Template.EmitInstruction(EncodeABC(OP_CALL, 1, 0, 0));
  Template.EmitInstruction(EncodeABx(OP_DIRECT_EVAL, 1, 3));
  Template.EmitInstruction(EncodeABC(OP_CALL, 1, 0, 0));
  Template.AddDirectEvalEnvironment(0, False, False, devFunction,
    FUNCTION_VAR_OBJECT_SLOT, Bindings);
  Template.AddDirectEvalEnvironment(2, True, False, devParameters,
    PARAMETER_VAR_OBJECT_SLOT, nil);
  Template.AddDirectEvalEnvironment(4, False, True, devNone, -1, nil);
  Template.AddDirectEvalEnvironment(6, False, False, devGlobal, -1, nil);

  Loaded := LoadTemplateThroughBinary(Template, Reason);
  try
    Expect<string>(Reason).ToBe('');
    Expect<Boolean>(Assigned(Loaded)).ToBe(True);
    if not Assigned(Loaded) then
      Exit;

    Instruction := Loaded.TopLevel.GetInstruction(2);
    Expect<Integer>(DecodeOp(Instruction)).ToBe(Ord(OP_DIRECT_EVAL));
    Expect<Integer>(DecodeA(Instruction)).ToBe(2);
    Expect<Integer>(DecodeBx(Instruction)).ToBe(1);

    Expect<Integer>(Loaded.TopLevel.DirectEvalEnvironmentCount).ToBe(4);
    Env := Loaded.TopLevel.GetDirectEvalEnvironment(0);
    Expect<Integer>(Env.PC).ToBe(0);
    Expect<Boolean>(Env.RejectArgumentsReference).ToBe(False);
    Expect<Boolean>(Env.StrictCaller).ToBe(False);
    Expect<Integer>(Ord(Env.VarEnvironment)).ToBe(Ord(devFunction));
    Expect<Integer>(Env.VarObjectSlot).ToBe(FUNCTION_VAR_OBJECT_SLOT);
    Expect<Integer>(Length(Env.Bindings)).ToBe(Length(Bindings));
    for I := 0 to High(Bindings) do
      ExpectDirectEvalBinding(Env.Bindings[I], Bindings[I]);

    Env := Loaded.TopLevel.GetDirectEvalEnvironment(1);
    Expect<Integer>(Env.PC).ToBe(2);
    Expect<Boolean>(Env.RejectArgumentsReference).ToBe(True);
    Expect<Boolean>(Env.StrictCaller).ToBe(False);
    Expect<Integer>(Ord(Env.VarEnvironment)).ToBe(Ord(devParameters));
    Expect<Integer>(Env.VarObjectSlot).ToBe(PARAMETER_VAR_OBJECT_SLOT);
    Expect<Integer>(Length(Env.Bindings)).ToBe(0);

    Env := Loaded.TopLevel.GetDirectEvalEnvironment(2);
    Expect<Integer>(Env.PC).ToBe(4);
    Expect<Boolean>(Env.StrictCaller).ToBe(True);
    Expect<Integer>(Ord(Env.VarEnvironment)).ToBe(Ord(devNone));
    Expect<Integer>(Env.VarObjectSlot).ToBe(-1);

    Env := Loaded.TopLevel.GetDirectEvalEnvironment(3);
    Expect<Integer>(Env.PC).ToBe(6);
    Expect<Boolean>(Env.StrictCaller).ToBe(False);
    Expect<Integer>(Ord(Env.VarEnvironment)).ToBe(Ord(devGlobal));
    Expect<Integer>(Env.VarObjectSlot).ToBe(-1);
  finally
    Loaded.Free;
  end;
end;

// The VM reads the operands of the call after OP_DIRECT_EVAL and the record it
// names, so the loader rejects a file in which either is missing or wrong.
procedure TBytecodeBinaryTests.TestRejectsMalformedDirectEvalSite;
const
  WIDE_MAX_REGISTERS = 400;
  WIDE_CALLEE_REGISTER = 300;
var
  Loaded: TGocciaBytecodeModule;
  Template: TGocciaFunctionTemplate;
  Message: string;

  function Reason(const ASiteIndex: UInt16; const ACall: UInt64;
    const AVarEnvironment: TGocciaDirectEvalVarEnvironment;
    const AVarObjectSlot: Integer): string;
  var
    Loaded: TGocciaBytecodeModule;
    Template: TGocciaFunctionTemplate;
  begin
    Template := TGocciaFunctionTemplate.Create('main');
    Template.MaxRegisters := TEST_MAX_REGISTERS;
    Template.EmitInstruction(EncodeABx(OP_DIRECT_EVAL, 1, ASiteIndex));
    Template.EmitInstruction(ACall);
    Template.AddDirectEvalEnvironment(0, False, False, AVarEnvironment,
      AVarObjectSlot, nil);
    Loaded := LoadTemplateThroughBinary(Template, Result);
    Loaded.Free;
  end;

  // Two eval sites, at PC 0 and PC 2, whose records claim PCs AFirstPC and
  // ASecondPC.
  function TwoSiteReason(const AFirstPC, ASecondPC: UInt32): string;
  var
    Loaded: TGocciaBytecodeModule;
    Template: TGocciaFunctionTemplate;
  begin
    Template := TGocciaFunctionTemplate.Create('main');
    Template.MaxRegisters := TEST_MAX_REGISTERS;
    Template.EmitInstruction(EncodeABx(OP_DIRECT_EVAL, 1, 0));
    Template.EmitInstruction(EncodeABC(OP_CALL, 1, 0, 0));
    Template.EmitInstruction(EncodeABx(OP_DIRECT_EVAL, 1, 1));
    Template.EmitInstruction(EncodeABC(OP_CALL, 1, 0, 0));
    Template.AddDirectEvalEnvironment(AFirstPC, False, True, devNone, -1, nil);
    Template.AddDirectEvalEnvironment(ASecondPC, False, True, devNone, -1,
      nil);
    Loaded := LoadTemplateThroughBinary(Template, Result);
    Loaded.Free;
  end;

begin
  Expect<string>(Reason(0, EncodeABC(OP_CALL, 1, 0, 0), devFunction, 2))
    .ToBe('');
  Expect<Boolean>(Pos('eval site record 1 is outside 0..0',
    Reason(1, EncodeABC(OP_CALL, 1, 0, 0), devFunction, 2)) > 0).ToBe(True);
  Expect<Boolean>(Pos('not followed by a call on its callee register',
    Reason(0, EncodeABC(OP_LOAD_UNDEFINED, 1, 0, 0), devFunction, 2)) > 0)
    .ToBe(True);
  Expect<Boolean>(Pos('not followed by a call on its callee register',
    Reason(0, EncodeABC(OP_CALL, 2, 0, 0), devFunction, 2)) > 0).ToBe(True);
  Expect<Boolean>(Pos('does not match its variable environment',
    Reason(0, EncodeABC(OP_CALL, 1, 0, 0), devFunction, -1)) > 0).ToBe(True);
  Expect<Boolean>(Pos('does not match its variable environment',
    Reason(0, EncodeABC(OP_CALL, 1, 0, 0), devGlobal, 2)) > 0).ToBe(True);
  Expect<Boolean>(Pos('names register 9, outside MaxRegisters 4',
    Reason(0, EncodeABC(OP_CALL, 1, 0, 0), devParameters,
      TEST_OUT_OF_RANGE_REGISTER)) > 0).ToBe(True);

  // OP_DIRECT_EVAL as the last instruction, with no call to read.
  Template := TGocciaFunctionTemplate.Create('main');
  Template.MaxRegisters := TEST_MAX_REGISTERS;
  Template.EmitInstruction(EncodeABx(OP_DIRECT_EVAL, 1, 0));
  Template.AddDirectEvalEnvironment(0, False, True, devNone, -1, nil);
  Loaded := LoadTemplateThroughBinary(Template, Message);
  Loaded.Free;
  Expect<Boolean>(Pos('not followed by a call', Message) > 0).ToBe(True);

  // A record's PC is the OP_DIRECT_EVAL that names it, not the call after
  // it, nor another site's OP_DIRECT_EVAL.
  Expect<Boolean>(Pos('is not the OP_DIRECT_EVAL that names it',
    TwoSiteReason(1, 2)) > 0).ToBe(True);
  Expect<Boolean>(Pos('record 0 is at PC 2, which is not the OP_DIRECT_EVAL',
    TwoSiteReason(2, 0)) > 0).ToBe(True);
  Expect<string>(TwoSiteReason(0, 2)).ToBe('');

  // With an OP_WIDE prefix, the PC is the prefix, as the compiler records it.
  Template := TGocciaFunctionTemplate.Create('main');
  Template.MaxRegisters := WIDE_MAX_REGISTERS;
  Template.EmitInstruction(EncodeABx(OP_DIRECT_EVAL, WIDE_CALLEE_REGISTER, 0));
  Template.EmitInstruction(EncodeABC(OP_CALL, WIDE_CALLEE_REGISTER, 0, 0));
  Template.AddDirectEvalEnvironment(1, False, True, devNone, -1, nil);
  Loaded := LoadTemplateThroughBinary(Template, Message);
  Loaded.Free;
  Expect<Boolean>(Pos('inside an OP_WIDE instruction', Message) > 0)
    .ToBe(True);

  // A call whose operands need an OP_WIDE prefix is still the call.
  Template := TGocciaFunctionTemplate.Create('main');
  Template.MaxRegisters := WIDE_MAX_REGISTERS;
  Template.EmitInstruction(EncodeABx(OP_DIRECT_EVAL, WIDE_CALLEE_REGISTER, 0));
  Template.EmitInstruction(EncodeABC(OP_CALL, WIDE_CALLEE_REGISTER, 1, 0));
  Template.AddDirectEvalEnvironment(0, False, True, devNone, -1, nil);
  Loaded := LoadTemplateThroughBinary(Template, Message);
  try
    Expect<string>(Message).ToBe('');
    if Assigned(Loaded) then
      Expect<Integer>(DecodeOp(Loaded.TopLevel.GetInstruction(2)))
        .ToBe(Ord(OP_WIDE));
  finally
    Loaded.Free;
  end;
end;

// ADR 0134 Phase 2: global eval code is compiled when the eval runs, and its
// template's GlobalEvalCode flag has no field in the format, so the writer
// refuses it instead of writing a template that would load without the flag.
procedure TBytecodeBinaryTests.TestRefusesGlobalEvalCode;
var
  Module: TGocciaBytecodeModule;
  Stream: TMemoryStream;
  Writer: TGocciaBytecodeWriter;
  Refused: Boolean;
begin
  Stream := TMemoryStream.Create;
  Module := TGocciaBytecodeModule.Create(TEST_RUNTIME_TAG, TEST_SOURCE_PATH);
  try
    Module.TopLevel := TGocciaFunctionTemplate.Create('<module>');
    Module.TopLevel.MaxRegisters := TEST_MAX_REGISTERS;
    Module.TopLevel.GlobalEvalCode := True;
    Module.TopLevel.EmitInstruction(EncodeABC(OP_RETURN, 0, 0, 0));
    Module.HasDebugInfo := False;
    Refused := False;
    Writer := TGocciaBytecodeWriter.Create(Stream);
    try
      try
        Writer.WriteModule(Module);
      except
        on E: Exception do
          Refused := True;
      end;
    finally
      Writer.Free;
    end;
    Expect<Boolean>(Refused).ToBe(True);
  finally
    Module.Free;
    Stream.Free;
  end;
end;

begin
  TestRunnerProgram.AddSuite(TBytecodeBinaryTests.Create('Bytecode Binary'));
  TestRunnerProgram.Run;
  ExitCode := TestResultToExitCode;
end.
