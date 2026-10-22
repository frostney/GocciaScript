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
  Goccia.Bytecode.Module,
  Goccia.Bytecode.Profile;

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
    procedure TestRoundTripsBigIntConstantSlots;
    procedure TestRoundTripsPrivateNameConstants;
    procedure TestRejectsMalformedBigIntPayloads;
    procedure TestRejectsDuplicateInternedConstants;
    procedure TestRoundTripsBigIntConstants;
    procedure TestRoundTripsDebugLineMappings;
    procedure TestRoundTripsExceptionHandlers;
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
  Test('BigInt payloads and following constant slots survive serialization',
    TestRoundTripsBigIntConstantSlots);
  Test('A private name and a string with the same text stay two constants',
    TestRoundTripsPrivateNameConstants);
  Test('Rejects malformed BigInt payloads before execution',
    TestRejectsMalformedBigIntPayloads);
  Test('BigInt bytes survive a save/load/save cycle', TestRoundTripsBigIntConstants);
  Test('Debug line mappings survive a save/load/save cycle', TestRoundTripsDebugLineMappings);
  Test('Exception handlers survive a save/load/save cycle', TestRoundTripsExceptionHandlers);
  Test('Rejects duplicate wire constants instead of collapsing their slots',
    TestRejectsDuplicateInternedConstants);
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
    Module.CompileProfile := Default(TGocciaCompileProfile);
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
    Module.CompileProfile := Default(TGocciaCompileProfile);
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
  Module.CompileProfile := Default(TGocciaCompileProfile);
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
    Module.CompileProfile := Default(TGocciaCompileProfile);
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
    Module.CompileProfile := Default(TGocciaCompileProfile);
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
  Expect<Boolean>(ABinding.IsOutsideVarEnvironment).ToBe(
    AExpected.IsOutsideVarEnvironment);
  Expect<Boolean>(ABinding.IsCallerFunctionBinding).ToBe(
    AExpected.IsCallerFunctionBinding);
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
  PRIVATE_CLASS_REGISTER = 5;
var
  Loaded: TGocciaBytecodeModule;
  Template: TGocciaFunctionTemplate;
  Bindings: TGocciaDirectEvalBindingArray;
  PrivateNames: TGocciaDirectEvalPrivateNameArray;
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
  Bindings[0].IsCallerFunctionBinding := True;
  Bindings[1].IsOutsideVarEnvironment := True;
  Bindings[1].IsCallerFunctionBinding := True;
  // The outer class body's #x first; the inner one's #x shadows it.
  SetLength(PrivateNames, 3);
  PrivateNames[0].Name := 'x';
  PrivateNames[0].Prefix := '0$';
  PrivateNames[1].Name := 'y';
  PrivateNames[1].Prefix := '0$';
  PrivateNames[2].Name := 'x';
  PrivateNames[2].Prefix := '1$';

  Template := TGocciaFunctionTemplate.Create('main');
  Template.MaxRegisters := EVAL_MAX_REGISTERS;
  // The upvalues the record's upvalue bindings name.
  for I := 0 to 4 do
    Template.AddUpvalueDescriptor(False, UInt16(I), 'u' + IntToStr(I));
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
  Template.AddDirectEvalEnvironment(4, False, True, devNone, -1, nil,
    PRIVATE_CLASS_REGISTER, PrivateNames);
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
    Expect<Integer>(Env.PrivateClassRegister).ToBe(-1);
    Expect<Integer>(Length(Env.PrivateNames)).ToBe(0);
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
    Expect<Integer>(Env.PrivateClassRegister).ToBe(PRIVATE_CLASS_REGISTER);
    Expect<Integer>(Length(Env.PrivateNames)).ToBe(Length(PrivateNames));
    for I := 0 to High(PrivateNames) do
    begin
      Expect<string>(Env.PrivateNames[I].Name).ToBe(PrivateNames[I].Name);
      Expect<string>(Env.PrivateNames[I].Prefix).ToBe(PrivateNames[I].Prefix);
    end;

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
  Bindings: TGocciaDirectEvalBindingArray;
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

  // The class being defined is read from the caller's register.
  Template := TGocciaFunctionTemplate.Create('main');
  Template.MaxRegisters := TEST_MAX_REGISTERS;
  Template.EmitInstruction(EncodeABx(OP_DIRECT_EVAL, 1, 0));
  Template.EmitInstruction(EncodeABC(OP_CALL, 1, 0, 0));
  Template.AddDirectEvalEnvironment(0, False, True, devNone, -1, nil,
    TEST_OUT_OF_RANGE_REGISTER);
  Loaded := LoadTemplateThroughBinary(Template, Message);
  Loaded.Free;
  Expect<Boolean>(Pos('names private class register 9, outside ' +
    'MaxRegisters 4', Message) > 0).ToBe(True);

  // Eval code captures the caller registers and upvalues the record names.
  SetLength(Bindings, 1);
  Bindings[0] := DirectEvalBinding('local', debLocal,
    TEST_OUT_OF_RANGE_REGISTER, False, False, False, False);
  Template := TGocciaFunctionTemplate.Create('main');
  Template.MaxRegisters := TEST_MAX_REGISTERS;
  Template.EmitInstruction(EncodeABx(OP_DIRECT_EVAL, 1, 0));
  Template.EmitInstruction(EncodeABC(OP_CALL, 1, 0, 0));
  Template.AddDirectEvalEnvironment(0, False, True, devNone, -1, Bindings);
  Loaded := LoadTemplateThroughBinary(Template, Message);
  Loaded.Free;
  Expect<Boolean>(Pos('binds ''local'' to register 9, outside ' +
    'MaxRegisters 4', Message) > 0).ToBe(True);
  Bindings[0] := DirectEvalBinding('outer', debUpvalue, 0, False, False,
    False, False);
  Template := TGocciaFunctionTemplate.Create('main');
  Template.MaxRegisters := TEST_MAX_REGISTERS;
  Template.EmitInstruction(EncodeABx(OP_DIRECT_EVAL, 1, 0));
  Template.EmitInstruction(EncodeABC(OP_CALL, 1, 0, 0));
  Template.AddDirectEvalEnvironment(0, False, True, devNone, -1, Bindings);
  Loaded := LoadTemplateThroughBinary(Template, Message);
  Loaded.Free;
  Expect<Boolean>(Pos('binds ''outer'' to upvalue 0, outside the 0 ' +
    'upvalues', Message) > 0).ToBe(True);

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
  Module.CompileProfile := Default(TGocciaCompileProfile);
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

procedure TBytecodeBinaryTests.TestRoundTripsBigIntConstantSlots;
const
  BIG_DECIMAL = '1234567890123456789012345678901234567890';
var
  Template, LoadedTemplate: TGocciaFunctionTemplate;
  Loaded: TGocciaBytecodeModule;
  Reason: string;
begin
  Template := TGocciaFunctionTemplate.Create('bigint');
  Template.MaxRegisters := 1;
  Template.AddConstantBigInt('0');
  Template.AddConstantBigInt(BIG_DECIMAL);
  Template.AddConstantBigInt(BIG_DECIMAL);
  Template.AddConstantBigInt('-' + BIG_DECIMAL);
  Template.AddConstantString('after BigInt');
  Template.EmitInstruction(EncodeABx(OP_LOAD_CONST, 0, 4));
  Template.EmitInstruction(EncodeABC(OP_RETURN, 0, 0, 0));
  Loaded := LoadTemplateThroughBinary(Template, Reason);
  try
    Expect<string>(Reason).ToBe('');
    Expect<Boolean>(Assigned(Loaded)).ToBe(True);
    if not Assigned(Loaded) then
      Exit;
    LoadedTemplate := Loaded.TopLevel;
    Expect<Integer>(LoadedTemplate.ConstantCount).ToBe(5);
    Expect<Integer>(Ord(LoadedTemplate.GetConstant(0).Kind))
      .ToBe(Ord(bckBigInt));
    Expect<string>(LoadedTemplate.GetConstant(0).StringValue).ToBe('0');
    Expect<string>(LoadedTemplate.GetConstant(1).StringValue).ToBe(BIG_DECIMAL);
    Expect<string>(LoadedTemplate.GetConstant(2).StringValue).ToBe(BIG_DECIMAL);
    Expect<string>(LoadedTemplate.GetConstant(3).StringValue)
      .ToBe('-' + BIG_DECIMAL);
    Expect<string>(LoadedTemplate.GetConstant(4).StringValue)
      .ToBe('after BigInt');
  finally
    Loaded.Free;
  end;
end;

procedure TBytecodeBinaryTests.TestRoundTripsPrivateNameConstants;
const
  STORAGE_KEY = '#slot:0$value';
var
  Template, LoadedTemplate: TGocciaFunctionTemplate;
  Loaded: TGocciaBytecodeModule;
  Reason: string;
begin
  Template := TGocciaFunctionTemplate.Create('private-name');
  Template.MaxRegisters := 1;
  Expect<Integer>(Template.AddConstantString(STORAGE_KEY)).ToBe(0);
  Expect<Integer>(Template.AddConstantPrivateName(STORAGE_KEY)).ToBe(1);
  Expect<Integer>(Template.AddConstantPrivateName(STORAGE_KEY)).ToBe(1);
  Expect<Integer>(Template.AddConstantString(STORAGE_KEY)).ToBe(0);
  Template.EmitInstruction(EncodeABx(OP_LOAD_CONST, 0, 1));
  Template.EmitInstruction(EncodeABC(OP_RETURN, 0, 0, 0));
  Loaded := LoadTemplateThroughBinary(Template, Reason);
  try
    Expect<string>(Reason).ToBe('');
    Expect<Boolean>(Assigned(Loaded)).ToBe(True);
    if not Assigned(Loaded) then
      Exit;
    LoadedTemplate := Loaded.TopLevel;
    Expect<Integer>(LoadedTemplate.ConstantCount).ToBe(2);
    Expect<Integer>(Ord(LoadedTemplate.GetConstant(0).Kind))
      .ToBe(Ord(bckString));
    Expect<Integer>(Ord(LoadedTemplate.GetConstant(1).Kind))
      .ToBe(Ord(bckPrivateName));
    Expect<string>(LoadedTemplate.GetConstant(1).StringValue)
      .ToBe(STORAGE_KEY);
  finally
    Loaded.Free;
  end;
end;

procedure TBytecodeBinaryTests.TestRejectsMalformedBigIntPayloads;

  procedure CheckRejected(const APayload: string);
  var
    Template: TGocciaFunctionTemplate;
    Loaded: TGocciaBytecodeModule;
    Reason: string;
  begin
    Template := TGocciaFunctionTemplate.Create('bigint');
    Template.MaxRegisters := 1;
    Template.AddConstantBigInt(APayload);
    Template.EmitInstruction(EncodeABx(OP_LOAD_CONST, 0, 0));
    Template.EmitInstruction(EncodeABC(OP_RETURN, 0, 0, 0));
    Loaded := LoadTemplateThroughBinary(Template, Reason);
    try
      Expect<Boolean>(Assigned(Loaded)).ToBe(False);
      Expect<Boolean>(Pos('Invalid bytecode BigInt decimal payload', Reason) > 0)
        .ToBe(True);
    finally
      Loaded.Free;
    end;
  end;

begin
  CheckRejected('');
  CheckRejected('-');
  CheckRejected('+');
  CheckRejected('+1');
  CheckRejected('00');
  CheckRejected('-0');
  CheckRejected('12x');
  CheckRejected('0x10');
  CheckRejected('1' + #0 + '2');
  CheckRejected(' 1');
  CheckRejected('1 ');
  CheckRejected(StringOfChar('1', 65536) + 'x');
end;

procedure TBytecodeBinaryTests.TestRejectsDuplicateInternedConstants;
var
  Module, Loaded: TGocciaBytecodeModule;
  Stream: TMemoryStream;
  Writer: TGocciaBytecodeWriter;
  Reader: TGocciaBytecodeReader;
  Bytes: PByte;
  I, J, Matches: Integer;
  Reason: string;
const
  FIRST_TEXT = 'first';
  OTHER_TEXT = 'other';
begin
  Stream := TMemoryStream.Create;
  Module := TGocciaBytecodeModule.Create(TEST_RUNTIME_TAG, TEST_SOURCE_PATH);
  Module.CompileProfile := Default(TGocciaCompileProfile);
  try
    Module.TopLevel := TGocciaFunctionTemplate.Create('duplicates');
    Module.TopLevel.MaxRegisters := 1;
    Module.TopLevel.AddConstantString(FIRST_TEXT);
    Module.TopLevel.AddConstantString(OTHER_TEXT);
    Module.TopLevel.EmitInstruction(EncodeABC(OP_RETURN, 0, 0, 0));
    Module.HasDebugInfo := False;
    Writer := TGocciaBytecodeWriter.Create(Stream);
    try
      Writer.WriteModule(Module);
    finally
      Writer.Free;
    end;
    { Locate the unique UTF-16LE payload, independent of header layout, and
      replace it with the earlier constant. Neither string changes length. }
    Bytes := Stream.Memory;
    Matches := 0;
    for I := 0 to Integer(Stream.Size) - Length(OTHER_TEXT) * 2 do
    begin
      J := 1;
      while (J <= Length(OTHER_TEXT)) and
        (Bytes[I + (J - 1) * 2] = Ord(OTHER_TEXT[J])) and
        (Bytes[I + (J - 1) * 2 + 1] = 0) do
        Inc(J);
      if J > Length(OTHER_TEXT) then
      begin
        Inc(Matches);
        for J := 1 to Length(FIRST_TEXT) do
          Bytes[I + (J - 1) * 2] := Ord(FIRST_TEXT[J]);
      end;
    end;
    Expect<Integer>(Matches).ToBe(1);
    Stream.Position := 0;
    Reader := TGocciaBytecodeReader.Create(Stream);
    try
      Reason := '';
      Loaded := nil;
      try
        Loaded := Reader.ReadModule;
      except
        on E: Exception do
          Reason := E.Message;
      end;
      Loaded.Free;
      Expect<Boolean>(Pos('duplicate constant at index 1', Reason) > 0)
        .ToBe(True);
    finally
      Reader.Free;
    end;
  finally
    Module.Free;
    Stream.Free;
  end;
end;

// A save/load/save cycle must preserve the module bytes: heap snapshots name
// compiled templates by this encoding, including their debug information.
function CheckBinaryRoundTrip(const AModule: TGocciaBytecodeModule):
  TGocciaBytecodeModule;
var
  Original, Rewritten: TMemoryStream;
  Loaded: TGocciaBytecodeModule;
  Reader: TGocciaBytecodeReader;
  Writer: TGocciaBytecodeWriter;
begin
  Loaded := nil;
  Original := TMemoryStream.Create;
  Rewritten := TMemoryStream.Create;
  try
    Writer := TGocciaBytecodeWriter.Create(Original);
    try
      Writer.WriteModule(AModule);
    finally
      Writer.Free;
    end;
    Original.Position := 0;
    Reader := TGocciaBytecodeReader.Create(Original);
    try
      Loaded := Reader.ReadModule;
    finally
      Reader.Free;
    end;
    Writer := TGocciaBytecodeWriter.Create(Rewritten);
    try
      Writer.WriteModule(Loaded);
    finally
      Writer.Free;
    end;
    Expect<Int64>(Rewritten.Size).ToBe(Original.Size);
    Expect<Boolean>((Original.Size = Rewritten.Size) and
      CompareMem(Original.Memory, Rewritten.Memory, SizeInt(Original.Size)))
      .ToBe(True);
    Result := Loaded;
    Loaded := nil;
  finally
    Loaded.Free;
    Rewritten.Free;
    Original.Free;
  end;
end;

procedure TBytecodeBinaryTests.TestRoundTripsBigIntConstants;
const
  LARGE = '1234567890123456789012345678901234567890';
var
  Module, Loaded: TGocciaBytecodeModule;
  Template: TGocciaFunctionTemplate;
  I: Integer;
begin
  Loaded := nil;
  Module := TGocciaBytecodeModule.Create(TEST_RUNTIME_TAG, TEST_SOURCE_PATH);
  Module.CompileProfile := Default(TGocciaCompileProfile);
  try
    Template := TGocciaFunctionTemplate.Create('bigints');
    Module.TopLevel := Template;
    Template.MaxRegisters := TEST_MAX_REGISTERS;
    Template.AddConstantBigInt(LARGE);
    Template.AddConstantBigInt('-' + LARGE);
    Template.AddConstantBigInt(LARGE);
    Template.AddConstantString('after BigInts');
    for I := 0 to TEST_MAX_REGISTERS - 1 do
      Template.EmitInstruction(EncodeABx(OP_LOAD_CONST, UInt16(I), UInt16(I)));
    Template.EmitInstruction(EncodeABC(OP_RETURN, 0, 0, 0));
    Loaded := CheckBinaryRoundTrip(Module);
    Expect<Integer>(Loaded.TopLevel.ConstantCount).ToBe(TEST_MAX_REGISTERS);
    for I := 0 to TEST_MAX_REGISTERS - 1 do
    begin
      Expect<Integer>(Ord(Loaded.TopLevel.GetConstant(I).Kind)).ToBe(
        Ord(Template.GetConstant(I).Kind));
      Expect<string>(Loaded.TopLevel.GetConstant(I).StringValue).ToBe(
        Template.GetConstant(I).StringValue);
    end;
  finally
    Loaded.Free;
    Module.Free;
  end;
end;

procedure TBytecodeBinaryTests.TestRoundTripsDebugLineMappings;
var
  Module, Loaded: TGocciaBytecodeModule;
  Template: TGocciaFunctionTemplate;
  Entry: TGocciaLineMapEntry;
begin
  Loaded := nil;
  Module := TGocciaBytecodeModule.Create(TEST_RUNTIME_TAG, TEST_SOURCE_PATH);
  Module.CompileProfile := Default(TGocciaCompileProfile);
  try
    Template := TGocciaFunctionTemplate.Create('main');
    Module.TopLevel := Template;
    Template.MaxRegisters := TEST_MAX_REGISTERS;
    Template.EmitInstruction(EncodeABC(OP_RETURN, 0, 0, 0));
    Template.EmitInstruction(EncodeABC(OP_RETURN, 0, 0, 0));
    Template.DebugInfo := TGocciaDebugInfo.Create(TEST_SOURCE_PATH, 3, 5);
    Template.DebugInfo.AddLineMapping(0, 17, 9);
    Template.DebugInfo.AddLineMapping(1, 23, 11);
    Loaded := CheckBinaryRoundTrip(Module);
    Expect<Integer>(Loaded.TopLevel.DebugInfo.LineMapCount).ToBe(2);
    Entry := Loaded.TopLevel.DebugInfo.GetLineMapEntry(0);
    Expect<UInt32>(Entry.PC).ToBe(0);
    Expect<UInt32>(Entry.Line).ToBe(17);
    Expect<UInt16>(Entry.Column).ToBe(9);
    Entry := Loaded.TopLevel.DebugInfo.GetLineMapEntry(1);
    Expect<UInt32>(Entry.PC).ToBe(1);
    Expect<UInt32>(Entry.Line).ToBe(23);
    Expect<UInt16>(Entry.Column).ToBe(11);
  finally
    Loaded.Free;
    Module.Free;
  end;
end;

procedure TBytecodeBinaryTests.TestRoundTripsExceptionHandlers;
var
  Module, Loaded: TGocciaBytecodeModule;
  Template: TGocciaFunctionTemplate;
  Handler: TGocciaExceptionHandler;
  I: Integer;
begin
  Loaded := nil;
  Module := TGocciaBytecodeModule.Create(TEST_RUNTIME_TAG, TEST_SOURCE_PATH);
  Module.CompileProfile := Default(TGocciaCompileProfile);
  try
    Template := TGocciaFunctionTemplate.Create('main');
    Module.TopLevel := Template;
    Template.MaxRegisters := TEST_MAX_REGISTERS;
    for I := 0 to 7 do
      Template.EmitInstruction(EncodeABC(OP_RETURN, 0, 0, 0));
    Template.AddExceptionHandler(0, 2, 3, 5, 1);
    Template.AddExceptionHandler(1, 4, High(UInt32), 6, 2);
    Loaded := CheckBinaryRoundTrip(Module);
    Expect<Integer>(Loaded.TopLevel.ExceptionHandlerCount).ToBe(2);
    Handler := Loaded.TopLevel.GetExceptionHandler(0);
    Expect<UInt32>(Handler.TryStart).ToBe(0);
    Expect<UInt32>(Handler.TryEnd).ToBe(2);
    Expect<UInt32>(Handler.CatchTarget).ToBe(3);
    Expect<UInt32>(Handler.FinallyTarget).ToBe(5);
    Expect<UInt16>(Handler.CatchRegister).ToBe(1);
    Handler := Loaded.TopLevel.GetExceptionHandler(1);
    Expect<UInt32>(Handler.TryStart).ToBe(1);
    Expect<UInt32>(Handler.TryEnd).ToBe(4);
    Expect<UInt32>(Handler.CatchTarget).ToBe(High(UInt32));
    Expect<UInt32>(Handler.FinallyTarget).ToBe(6);
    Expect<UInt16>(Handler.CatchRegister).ToBe(2);
  finally
    Loaded.Free;
    Module.Free;
  end;
end;

begin
  TestRunnerProgram.AddSuite(TBytecodeBinaryTests.Create('Bytecode Binary'));
  TestRunnerProgram.Run;
  ExitCode := TestResultToExitCode;
end.
