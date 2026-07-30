unit Goccia.Bytecode.Binary;

{$I Goccia.inc}

interface

uses
  Classes,

  Goccia.Bytecode.Chunk,
  Goccia.Bytecode.Module;

type
  TGocciaBytecodeWriter = class
  private
    FStream: TStream;
    procedure WriteUInt8(const AValue: UInt8);
    procedure WriteUInt16(const AValue: UInt16);
    procedure WriteUInt32(const AValue: UInt32);
    procedure WriteUInt64(const AValue: UInt64);
    procedure WriteInt64(const AValue: Int64);
    procedure WriteDouble(const AValue: Double);
    procedure WriteString(const AValue: string);
    procedure WriteBoolean(const AValue: Boolean);
    procedure WriteFunctionTemplate(const AProto: TGocciaFunctionTemplate);
  public
    constructor Create(const AStream: TStream);
    procedure WriteModule(const AModule: TGocciaBytecodeModule);
  end;

  TGocciaBytecodeReader = class
  private
    FStream: TStream;
    FFunctionDepth: Integer;
    procedure RequireRemaining(const AByteCount: Int64;
      const AContext: string);
    function ReadUInt8: UInt8;
    function ReadUInt16: UInt16;
    function ReadUInt32: UInt32;
    function ReadUInt64: UInt64;
    function ReadInt64: Int64;
    function ReadDouble: Double;
    function ReadString: string;
    function ReadBoolean: Boolean;
    function ReadFunctionTemplate: TGocciaFunctionTemplate;
  public
    constructor Create(const AStream: TStream);
    function ReadModule: TGocciaBytecodeModule;
  end;

procedure SaveModuleToFile(const AModule: TGocciaBytecodeModule;
  const AFileName: string);
function LoadModuleFromFile(const AFileName: string): TGocciaBytecodeModule;

implementation

uses
  SysUtils,

  NumberBits,

  Goccia.Bytecode,
  Goccia.Bytecode.Debug;

const
  MAX_BYTECODE_FILE_BYTES = 64 * 1024 * 1024;
  MAX_BYTECODE_FUNCTION_DEPTH = 256;

procedure RejectInvalidBytecode(const ATemplate: TGocciaFunctionTemplate;
  const APC: Integer; const AReason: string);
begin
  raise Exception.CreateFmt('Invalid bytecode in function "%s" at PC %d: %s',
    [ATemplate.Name, APC, AReason]);
end;

procedure VerifyFunctionTemplate(const ATemplate: TGocciaFunctionTemplate;
  const ADepth: Integer);
var
  A, B, C: UInt16;
  Handler: TGocciaExceptionHandler;
  I, PC: Integer;
  Instruction, Prefix: UInt32;
  JumpTarget: Int64;
  Op: UInt8;

  procedure RequireConstant(const AIndex: Integer);
  begin
    if (AIndex < 0) or (AIndex >= ATemplate.ConstantCount) then
      RejectInvalidBytecode(ATemplate, PC, Format(
        'constant index %d is outside 0..%d',
        [AIndex, ATemplate.ConstantCount - 1]));
  end;

  procedure RequireRegister(const AIndex: Integer);
  begin
    if (AIndex < 0) or (AIndex >= ATemplate.MaxRegisters) then
      RejectInvalidBytecode(ATemplate, PC, Format(
        'register %d is outside MaxRegisters %d',
        [AIndex, ATemplate.MaxRegisters]));
  end;

  procedure RequireUpvalue(const AIndex: Integer);
  begin
    if (AIndex < 0) or (AIndex >= ATemplate.UpvalueCount) then
      RejectInvalidBytecode(ATemplate, PC, Format(
        'upvalue index %d is outside 0..%d',
        [AIndex, ATemplate.UpvalueCount - 1]));
  end;

  procedure RequireJumpTarget(const ATarget: Int64);
  begin
    if (ATarget < 0) or (ATarget > ATemplate.CodeCount) then
      RejectInvalidBytecode(ATemplate, PC, Format(
        'jump target %d is outside 0..%d',
        [ATarget, ATemplate.CodeCount]));
  end;

begin
  if ADepth > MAX_BYTECODE_FUNCTION_DEPTH then
    raise Exception.Create('Goccia bytecode function nesting limit exceeded');
  if not Assigned(ATemplate) then
    raise Exception.Create('Invalid bytecode: missing function template');

  PC := 0;
  while PC < ATemplate.CodeCount do
  begin
    Instruction := ATemplate.GetInstruction(PC);
    Prefix := 0;
    Op := DecodeOp(Instruction);
    if Op = Ord(OP_WIDE) then
    begin
      Prefix := Instruction;
      Inc(PC);
      if PC >= ATemplate.CodeCount then
        RejectInvalidBytecode(ATemplate, PC - 1, 'truncated OP_WIDE prefix');
      Instruction := ATemplate.GetInstruction(PC);
      Op := DecodeOp(Instruction);
      if Op = Ord(OP_WIDE) then
        RejectInvalidBytecode(ATemplate, PC, 'nested OP_WIDE prefix');
    end;

    if Op > Ord(High(TGocciaOpCode)) then
      RejectInvalidBytecode(ATemplate, PC, Format('unknown opcode %d', [Op]));

    A := DecodeA(Instruction);
    B := DecodeB(Instruction);
    C := DecodeC(Instruction);
    if Prefix <> 0 then
    begin
      A := A or (UInt16(DecodeA(Prefix)) shl 8);
      B := B or (UInt16(DecodeB(Prefix)) shl 8);
      C := C or (UInt16(DecodeC(Prefix)) shl 8);
    end;

    // A is the destination or primary source register for executable
    // instructions. Structural opcodes encode no register in A.
    if not (TGocciaOpCode(Op) in
      [OP_NOP, OP_LINE, OP_JUMP, OP_POP_HANDLER, OP_WIDE]) and
       (A >= ATemplate.MaxRegisters) then
      RejectInvalidBytecode(ATemplate, PC, Format(
        'register %d is outside MaxRegisters %d',
        [A, ATemplate.MaxRegisters]));
    // Non-wide B/C operands are physically bounded by the VM's 256-slot
    // defensive register window. A non-zero wide prefix can name an arbitrary
    // UInt16 slot, so reject a prefixed operand that escapes MaxRegisters.
    // Constant/immediate Bx forms do not use the B/C wide-prefix bytes.
    if (Prefix <> 0) and (DecodeB(Prefix) <> 0) and
       (B >= ATemplate.MaxRegisters) then
      RejectInvalidBytecode(ATemplate, PC, Format(
        'register %d is outside MaxRegisters %d',
        [B, ATemplate.MaxRegisters]));
    if (Prefix <> 0) and (DecodeC(Prefix) <> 0) and
       (C >= ATemplate.MaxRegisters) then
      RejectInvalidBytecode(ATemplate, PC, Format(
        'register %d is outside MaxRegisters %d',
        [C, ATemplate.MaxRegisters]));

    case TGocciaOpCode(Op) of
      OP_TO_PRIMITIVE, OP_GET_LOCAL, OP_SET_LOCAL, OP_CLOSE_UPVALUE:
        RequireRegister(DecodeBx(Instruction));

      OP_GET_UPVALUE, OP_SET_UPVALUE, OP_SET_UPVALUE_DYNAMIC:
        RequireUpvalue(DecodeBx(Instruction));

      OP_RESOLVE_UPVALUE_REF:
        RequireUpvalue(B);

      OP_SET_UPVALUE_REF:
        RequireUpvalue(C);

      OP_LOAD_CONST, OP_LOAD_REGEXP, OP_SET_GLOBAL_STATIC, OP_NEW_CLASS,
      OP_SET_CLASS_SOURCE_CONST, OP_DELETE_PROP_CONST,
      OP_DELETE_PROP_CONST_LOOSE, OP_GET_GLOBAL, OP_SET_GLOBAL,
      OP_SET_GLOBAL_LOOSE, OP_HAS_GLOBAL, OP_DELETE_GLOBAL, OP_IMPORT,
      OP_IMPORT_DEFER, OP_IMPORT_SOURCE, OP_GET_IMPORT_BINDING, OP_EXPORT,
      OP_THROW_TYPE_ERROR_CONST_LONG, OP_DEFINE_GLOBAL_VAR_DECL_LONG,
      OP_DEFINE_GLOBAL_VAR_LONG, OP_DEFINE_GLOBAL_LET_LONG,
      OP_DEFINE_GLOBAL_CONST_LONG, OP_DEFINE_GLOBAL_FUNCTION_LONG,
      OP_PREDECLARE_GLOBAL_LET_LONG, OP_PREDECLARE_GLOBAL_CONST_LONG:
        RequireConstant(DecodeBx(Instruction));

      OP_CLASS_ADD_METHOD_CONST, OP_CLASS_DECLARE_PRIVATE_STATIC_CONST,
      OP_SET_PROP_CONST, OP_SET_PROP_CONST_LOOSE,
      OP_DEFINE_STATIC_PROP_CONST, OP_DEFINE_STATIC_METHOD_CONST:
        RequireConstant(B);

      OP_GET_PROP_CONST, OP_SETUP_AUTO_ACCESSOR_CONST,
      OP_SETUP_AUTO_ACCESSOR_DYNAMIC, OP_APPLY_ELEMENT_DECORATOR_CONST,
      OP_DEFINE_ACCESSOR_CONST, OP_THROW_TYPE_ERROR_CONST, OP_FINALIZE_ENUM,
      OP_SUPER_GET_CONST:
        RequireConstant(C);

      OP_CLOSURE:
        if DecodeBx(Instruction) >= ATemplate.FunctionCount then
          RejectInvalidBytecode(ATemplate, PC, Format(
            'function index %d is outside 0..%d',
            [DecodeBx(Instruction), ATemplate.FunctionCount - 1]));

      OP_JUMP:
        begin
          JumpTarget := Int64(PC) + 1 + DecodeAx(Instruction);
          RequireJumpTarget(JumpTarget);
        end;
      OP_JUMP_IF_TRUE, OP_JUMP_IF_FALSE:
        begin
          JumpTarget := Int64(PC) + 1 + DecodesBx(Instruction);
          RequireJumpTarget(JumpTarget);
        end;
      OP_JUMP_IF_NUM_NOT_LTE_IMM:
        begin
          JumpTarget := Int64(PC) + 1 + Int16(C);
          RequireJumpTarget(JumpTarget);
        end;
      OP_JUMP_IF_NULLISH, OP_JUMP_IF_NOT_NULLISH:
        begin
          JumpTarget := Int64(PC) + 1 + C;
          RequireJumpTarget(JumpTarget);
        end;
      OP_PUSH_HANDLER, OP_PUSH_FINALLY_HANDLER:
        begin
          JumpTarget := Int64(PC) + 1 + DecodeBx(Instruction);
          RequireJumpTarget(JumpTarget);
        end;
    end;
    Inc(PC);
  end;

  for I := 0 to ATemplate.ExceptionHandlerCount - 1 do
  begin
    Handler := ATemplate.GetExceptionHandler(I);
    if (Handler.TryStart > Handler.TryEnd) or
       (Handler.TryEnd > UInt32(ATemplate.CodeCount)) then
      RejectInvalidBytecode(ATemplate, 0, 'invalid exception-handler range');
    if (Handler.CatchTarget <> High(UInt32)) and
       (Handler.CatchTarget > UInt32(ATemplate.CodeCount)) then
      RejectInvalidBytecode(ATemplate, 0, 'invalid exception catch target');
    if (Handler.FinallyTarget <> High(UInt32)) and
       (Handler.FinallyTarget > UInt32(ATemplate.CodeCount)) then
      RejectInvalidBytecode(ATemplate, 0, 'invalid exception finally target');
    if Handler.CatchRegister >= ATemplate.MaxRegisters then
      RejectInvalidBytecode(ATemplate, 0, 'invalid exception catch register');
  end;

  for I := 0 to ATemplate.FunctionCount - 1 do
    VerifyFunctionTemplate(ATemplate.GetFunction(I), ADepth + 1);
end;

constructor TGocciaBytecodeWriter.Create(const AStream: TStream);
begin
  inherited Create;
  FStream := AStream;
end;

procedure TGocciaBytecodeWriter.WriteUInt8(const AValue: UInt8);
begin
  FStream.WriteBuffer(AValue, SizeOf(UInt8));
end;

procedure TGocciaBytecodeWriter.WriteUInt16(const AValue: UInt16);
var
  Bytes: array[0..1] of Byte;
begin
  Bytes[0] := Byte(AValue);
  Bytes[1] := Byte(AValue shr 8);
  FStream.WriteBuffer(Bytes[0], SizeOf(Bytes));
end;

procedure TGocciaBytecodeWriter.WriteUInt32(const AValue: UInt32);
var
  Bytes: array[0..3] of Byte;
begin
  Bytes[0] := Byte(AValue);
  Bytes[1] := Byte(AValue shr 8);
  Bytes[2] := Byte(AValue shr 16);
  Bytes[3] := Byte(AValue shr 24);
  FStream.WriteBuffer(Bytes[0], SizeOf(Bytes));
end;

procedure TGocciaBytecodeWriter.WriteUInt64(const AValue: UInt64);
var
  Bytes: array[0..7] of Byte;
  I: Integer;
begin
  for I := 0 to High(Bytes) do
    Bytes[I] := Byte(AValue shr (I * 8));
  FStream.WriteBuffer(Bytes[0], SizeOf(Bytes));
end;

procedure TGocciaBytecodeWriter.WriteInt64(const AValue: Int64);
var
  Bits: UInt64;
begin
  Move(AValue, Bits, SizeOf(Bits));
  WriteUInt64(Bits);
end;

procedure TGocciaBytecodeWriter.WriteDouble(const AValue: Double);
begin
  WriteUInt64(DoubleToBits(AValue));
end;

procedure TGocciaBytecodeWriter.WriteString(const AValue: string);
var
  I: Integer;
  Len: UInt32;
begin
  Len := Length(AValue);
  WriteUInt32(Len);
  for I := 1 to Len do
    WriteUInt16(Ord(AValue[I]));
end;

procedure TGocciaBytecodeWriter.WriteBoolean(const AValue: Boolean);
begin
  if AValue then
    WriteUInt8(1)
  else
    WriteUInt8(0);
end;

procedure TGocciaBytecodeWriter.WriteFunctionTemplate(
  const AProto: TGocciaFunctionTemplate);
var
  I, J: Integer;
  Constant: TGocciaBytecodeConstant;
  Descriptor: TGocciaUpvalueDescriptor;
  EvalEnv: TGocciaDirectEvalEnvironment;
  EvalBinding: TGocciaDirectEvalBindingInfo;
  Handler: TGocciaExceptionHandler;
begin
  WriteString(AProto.Name);
  WriteUInt16(AProto.MaxRegisters);
  WriteUInt16(AProto.ParameterCount);
  WriteUInt16(AProto.FormalParameterCount);
  WriteUInt16(AProto.UpvalueCount);
  WriteBoolean(AProto.IsArrow);
  WriteBoolean(AProto.IsGenerator);
  WriteBoolean(AProto.IsAsync);
  WriteBoolean(AProto.HasOwnPrototype);
  WriteBoolean(AProto.StrictThis);

  WriteUInt32(UInt32(AProto.CodeCount));
  for I := 0 to AProto.CodeCount - 1 do
    WriteUInt32(AProto.GetInstruction(I));

  WriteUInt16(UInt16(AProto.ConstantCount));
  for I := 0 to AProto.ConstantCount - 1 do
  begin
    Constant := AProto.GetConstant(I);
    WriteUInt8(Ord(Constant.Kind));
    case Constant.Kind of
      bckNil, bckTrue, bckFalse:
        ;
      bckInteger:
        WriteInt64(Constant.IntValue);
      bckFloat:
        WriteDouble(Constant.FloatValue);
      bckString:
        WriteString(Constant.StringValue);
      bckRegExpLiteral:
      begin
        WriteString(Constant.StringValue);
        WriteString(Constant.RegExpFlags);
      end;
      bckTemplateObject:
      begin
        // Serialise cooked and raw string arrays; CachedValue is runtime-only
        WriteUInt16(UInt16(Length(Constant.CookedStrings)));
        for J := 0 to Length(Constant.CookedStrings) - 1 do
          WriteString(Constant.CookedStrings[J]);
        WriteUInt16(UInt16(Length(Constant.RawStrings)));
        for J := 0 to Length(Constant.RawStrings) - 1 do
          WriteString(Constant.RawStrings[J]);
        // TC39 Template Literal Revision: per-segment cooked validity flags
        for J := 0 to Length(Constant.CookedValid) - 1 do
          WriteBoolean(Constant.CookedValid[J]);
      end;
    end;
  end;

  for I := 0 to AProto.UpvalueCount - 1 do
  begin
    Descriptor := AProto.GetUpvalueDescriptor(I);
    WriteBoolean(Descriptor.IsLocal);
    WriteUInt16(Descriptor.Index);
    WriteString(Descriptor.Name);
  end;

  WriteUInt16(UInt16(AProto.DirectEvalEnvironmentCount));
  for I := 0 to AProto.DirectEvalEnvironmentCount - 1 do
  begin
    EvalEnv := AProto.GetDirectEvalEnvironment(I);
    WriteUInt32(EvalEnv.PC);
    WriteBoolean(EvalEnv.RejectArgumentsReference);
    WriteUInt16(UInt16(Length(EvalEnv.Bindings)));
    for J := 0 to High(EvalEnv.Bindings) do
    begin
      EvalBinding := EvalEnv.Bindings[J];
      WriteString(EvalBinding.Name);
      WriteUInt8(Ord(EvalBinding.Kind));
      WriteUInt16(EvalBinding.Index);
      WriteBoolean(EvalBinding.IsConst);
      WriteBoolean(EvalBinding.IsVarEnvironmentBinding);
      WriteBoolean(EvalBinding.IsEvalSyntheticArguments);
    end;
  end;

  WriteUInt16(UInt16(AProto.ExceptionHandlerCount));
  for I := 0 to AProto.ExceptionHandlerCount - 1 do
  begin
    Handler := AProto.GetExceptionHandler(I);
    WriteUInt32(Handler.TryStart);
    WriteUInt32(Handler.TryEnd);
    WriteUInt32(Handler.CatchTarget);
    WriteUInt32(Handler.FinallyTarget);
    WriteUInt16(Handler.CatchRegister);
  end;

  WriteUInt16(UInt16(AProto.FunctionCount));
  for I := 0 to AProto.FunctionCount - 1 do
    WriteFunctionTemplate(AProto.GetFunction(I));

  WriteBoolean(Assigned(AProto.DebugInfo));
  if Assigned(AProto.DebugInfo) then
  begin
    WriteString(AProto.DebugInfo.SourceFile);
    WriteUInt32(UInt32(AProto.DebugInfo.LineMapCount));
    for I := 0 to AProto.DebugInfo.LineMapCount - 1 do
    begin
      WriteUInt32(AProto.DebugInfo.GetLineMapEntry(I).PC);
      WriteUInt32(AProto.DebugInfo.GetLineMapEntry(I).Line);
      WriteUInt16(AProto.DebugInfo.GetLineMapEntry(I).Column);
    end;
    WriteUInt32(UInt32(AProto.DebugInfo.LocalCount));
    for I := 0 to AProto.DebugInfo.LocalCount - 1 do
    begin
      WriteString(AProto.DebugInfo.GetLocalInfo(I).Name);
      WriteUInt16(AProto.DebugInfo.GetLocalInfo(I).Slot);
      WriteUInt32(AProto.DebugInfo.GetLocalInfo(I).StartPC);
      WriteUInt32(AProto.DebugInfo.GetLocalInfo(I).EndPC);
    end;
  end;

  WriteUInt16(AProto.LocalTypeCount);
  for I := 0 to AProto.LocalTypeCount - 1 do
    WriteUInt8(Ord(AProto.GetLocalType(UInt16(I))));

  WriteUInt16(AProto.LocalStrictCount);
  for I := 0 to AProto.LocalStrictCount - 1 do
    WriteBoolean(AProto.GetLocalStrictFlag(UInt16(I)));

  WriteUInt16(AProto.TypeCheckPreambleSize);
  WriteUInt16(AProto.ParameterPreambleSize);
  if AProto.DirectEvalSyntheticArgumentsSlot < 0 then
    WriteUInt16(High(UInt16))
  else
    WriteUInt16(UInt16(AProto.DirectEvalSyntheticArgumentsSlot));
  WriteBoolean(AProto.RejectArgumentsInDirectEval);
end;

procedure TGocciaBytecodeWriter.WriteModule(
  const AModule: TGocciaBytecodeModule);
var
  I, J: Integer;
  Import_: TGocciaModuleImport;
  Export_: TGocciaModuleExport;
begin
  FStream.WriteBuffer(GOCCIA_BINARY_MAGIC, 4);
  WriteUInt16(AModule.FormatVersion);
  WriteString(AModule.RuntimeTag);
  WriteString(AModule.SourcePath);
  WriteBoolean(AModule.HasDebugInfo);

  WriteUInt16(UInt16(AModule.ImportCount));
  for I := 0 to AModule.ImportCount - 1 do
  begin
    Import_ := AModule.GetImport(I);
    WriteString(Import_.ModulePath);
    WriteUInt16(UInt16(Length(Import_.Bindings)));
    for J := 0 to High(Import_.Bindings) do
    begin
      WriteString(Import_.Bindings[J].ExportName);
      WriteUInt16(Import_.Bindings[J].LocalSlot);
    end;
  end;

  WriteUInt16(UInt16(AModule.ExportCount));
  for I := 0 to AModule.ExportCount - 1 do
  begin
    Export_ := AModule.GetExport(I);
    WriteString(Export_.Name);
    WriteUInt16(Export_.LocalSlot);
  end;

  WriteFunctionTemplate(AModule.TopLevel);
end;

constructor TGocciaBytecodeReader.Create(const AStream: TStream);
begin
  inherited Create;
  if not Assigned(AStream) then
    raise Exception.Create('Bytecode stream is required');
  if AStream.Size > MAX_BYTECODE_FILE_BYTES then
    raise Exception.CreateFmt(
      'Goccia bytecode file exceeds the %d byte limit',
      [MAX_BYTECODE_FILE_BYTES]);
  FStream := AStream;
  FFunctionDepth := 0;
end;

procedure TGocciaBytecodeReader.RequireRemaining(const AByteCount: Int64;
  const AContext: string);
begin
  if (AByteCount < 0) or (FStream.Position < 0) or
     (AByteCount > FStream.Size - FStream.Position) then
    raise Exception.CreateFmt('Truncated Goccia bytecode %s', [AContext]);
end;

function TGocciaBytecodeReader.ReadUInt8: UInt8;
begin
  FStream.ReadBuffer(Result, SizeOf(UInt8));
end;

function TGocciaBytecodeReader.ReadUInt16: UInt16;
var
  Bytes: array[0..1] of Byte;
begin
  FStream.ReadBuffer(Bytes[0], SizeOf(Bytes));
  Result := UInt16(Bytes[0]) or (UInt16(Bytes[1]) shl 8);
end;

function TGocciaBytecodeReader.ReadUInt32: UInt32;
var
  Bytes: array[0..3] of Byte;
begin
  FStream.ReadBuffer(Bytes[0], SizeOf(Bytes));
  Result := UInt32(Bytes[0]) or
    (UInt32(Bytes[1]) shl 8) or
    (UInt32(Bytes[2]) shl 16) or
    (UInt32(Bytes[3]) shl 24);
end;

function TGocciaBytecodeReader.ReadUInt64: UInt64;
var
  Bytes: array[0..7] of Byte;
  I: Integer;
begin
  FStream.ReadBuffer(Bytes[0], SizeOf(Bytes));
  Result := 0;
  for I := 0 to High(Bytes) do
    Result := Result or (UInt64(Bytes[I]) shl (I * 8));
end;

function TGocciaBytecodeReader.ReadInt64: Int64;
var
  Bits: UInt64;
begin
  Bits := ReadUInt64;
  Move(Bits, Result, SizeOf(Result));
end;

function TGocciaBytecodeReader.ReadDouble: Double;
begin
  Result := BitsToDouble(ReadUInt64);
end;

function TGocciaBytecodeReader.ReadString: string;
var
  I: Integer;
  Len: UInt32;
begin
  Len := ReadUInt32;
  if Len = 0 then
    Exit('');
  if Len > UInt32(High(Integer)) then
    raise Exception.Create('Invalid Goccia bytecode string length');
  RequireRemaining(Int64(Len) * SizeOf(UInt16), 'string data');
  SetLength(Result, Len);
  for I := 1 to Len do
    Result[I] := Char(ReadUInt16);
end;

function TGocciaBytecodeReader.ReadBoolean: Boolean;
begin
  Result := ReadUInt8 <> 0;
end;

function TGocciaBytecodeReader.ReadFunctionTemplate: TGocciaFunctionTemplate;
var
  Name: string;
  ParamCount, UpvalueCount, MaxRegs, LocalTypeCount, LocalStrictCount: UInt16;
  CodeCount: UInt32;
  ConstCount, FuncCount, HandlerCount, StrCount, EvalEnvCount,
    EvalBindingCount: UInt16;
  I, J: Integer;
  ConstKind: UInt8;
  EvalPC: UInt32;
  EvalRejectArgumentsReference: Boolean;
  EvalBindings: TGocciaDirectEvalBindingArray;
  EvalBinding: TGocciaDirectEvalBindingInfo;
  UpvalueIsLocal: Boolean;
  UpvalueIndex: UInt16;
  UpvalueName: string;
  HasDebug: Boolean;
  DebugInfo: TGocciaDebugInfo;
  SourceFile, RegExpPattern, RegExpFlags: string;
  LineMapCount, LocalCount: UInt32;
  CookedStrings, RawStrings: TGocciaBytecodeStringArray;
  CookedValid: TGocciaBytecodeTemplateCookedValid;
begin
  Inc(FFunctionDepth);
  if FFunctionDepth > MAX_BYTECODE_FUNCTION_DEPTH then
  begin
    Dec(FFunctionDepth);
    raise Exception.Create('Goccia bytecode function nesting limit exceeded');
  end;
  try
  Name := ReadString;
  MaxRegs := ReadUInt16;
  ParamCount := ReadUInt16;
  LocalTypeCount := 0;
  LocalStrictCount := 0;
  HasDebug := False;

  Result := TGocciaFunctionTemplate.Create(Name);
  Result.MaxRegisters := MaxRegs;
  Result.ParameterCount := ParamCount;
  Result.FormalParameterCount := ReadUInt16;
  UpvalueCount := ReadUInt16;
  Result.IsArrow := ReadBoolean;
  Result.IsGenerator := ReadBoolean;
  Result.IsAsync := ReadBoolean;
  Result.HasOwnPrototype := ReadBoolean;
  Result.StrictThis := ReadBoolean;

  CodeCount := ReadUInt32;
  RequireRemaining(Int64(CodeCount) * SizeOf(UInt32), 'instruction data');
  for I := 0 to Integer(CodeCount) - 1 do
    Result.EmitInstruction(ReadUInt32);

  ConstCount := ReadUInt16;
  for I := 0 to ConstCount - 1 do
  begin
    ConstKind := ReadUInt8;
    if ConstKind > Ord(High(TGocciaBytecodeConstantKind)) then
      raise Exception.CreateFmt('Invalid bytecode constant kind: %d',
        [ConstKind]);
    case TGocciaBytecodeConstantKind(ConstKind) of
      bckNil:     Result.AddConstantNil;
      bckTrue:    Result.AddConstantBoolean(True);
      bckFalse:   Result.AddConstantBoolean(False);
      bckInteger: Result.AddConstantInteger(ReadInt64);
      bckFloat:   Result.AddConstantFloat(ReadDouble);
      bckString:  Result.AddConstantString(ReadString);
      bckRegExpLiteral:
      begin
        RegExpPattern := ReadString;
        RegExpFlags := ReadString;
        Result.AddConstantRegExpLiteral(RegExpPattern, RegExpFlags);
      end;
      bckTemplateObject:
      begin
        StrCount := ReadUInt16;
        SetLength(CookedStrings, StrCount);
        for J := 0 to StrCount - 1 do
          CookedStrings[J] := ReadString;
        StrCount := ReadUInt16;
        SetLength(RawStrings, StrCount);
        for J := 0 to StrCount - 1 do
          RawStrings[J] := ReadString;
        // TC39 Template Literal Revision: per-segment cooked validity flags
        SetLength(CookedValid, Length(CookedStrings));
        for J := 0 to Length(CookedStrings) - 1 do
          CookedValid[J] := ReadBoolean;
        Result.AddConstantTemplateObject(CookedStrings, RawStrings, CookedValid);
      end;
    end;
  end;

  for I := 0 to UpvalueCount - 1 do
  begin
    UpvalueIsLocal := ReadBoolean;
    UpvalueIndex := ReadUInt16;
    UpvalueName := ReadString;
    Result.AddUpvalueDescriptor(UpvalueIsLocal, UpvalueIndex, UpvalueName);
  end;

  EvalEnvCount := ReadUInt16;
  for I := 0 to EvalEnvCount - 1 do
  begin
    EvalPC := ReadUInt32;
    EvalRejectArgumentsReference := ReadBoolean;
    EvalBindingCount := ReadUInt16;
    SetLength(EvalBindings, EvalBindingCount);
    for J := 0 to EvalBindingCount - 1 do
    begin
      EvalBinding.Name := ReadString;
      ConstKind := ReadUInt8;
      if ConstKind > Ord(High(TGocciaDirectEvalBindingKind)) then
        raise Exception.CreateFmt('Invalid direct-eval binding kind: %d',
          [ConstKind]);
      EvalBinding.Kind := TGocciaDirectEvalBindingKind(ConstKind);
      EvalBinding.Index := ReadUInt16;
      EvalBinding.IsConst := ReadBoolean;
      EvalBinding.IsVarEnvironmentBinding := ReadBoolean;
      EvalBinding.IsEvalSyntheticArguments := ReadBoolean;
      EvalBindings[J] := EvalBinding;
    end;
    Result.AddDirectEvalEnvironment(EvalPC, EvalRejectArgumentsReference,
      EvalBindings);
  end;

  HandlerCount := ReadUInt16;
  for I := 0 to HandlerCount - 1 do
    Result.AddExceptionHandler(ReadUInt32, ReadUInt32, ReadUInt32,
      ReadUInt32, ReadUInt16);

  FuncCount := ReadUInt16;
  for I := 0 to FuncCount - 1 do
    Result.AddFunction(ReadFunctionTemplate);

  HasDebug := ReadBoolean;
  if HasDebug then
  begin
    SourceFile := ReadString;
    DebugInfo := TGocciaDebugInfo.Create(SourceFile);

    LineMapCount := ReadUInt32;
    RequireRemaining(Int64(LineMapCount) * 10, 'debug line mappings');
    for I := 0 to Integer(LineMapCount) - 1 do
      DebugInfo.AddLineMapping(ReadUInt32, ReadUInt32, ReadUInt16);

    LocalCount := ReadUInt32;
    RequireRemaining(Int64(LocalCount) * 14, 'debug local mappings');
    for I := 0 to Integer(LocalCount) - 1 do
      DebugInfo.AddLocal(ReadString, ReadUInt16, ReadUInt32, ReadUInt32);

    Result.DebugInfo := DebugInfo;
  end;

  LocalTypeCount := ReadUInt16;
  for I := 0 to LocalTypeCount - 1 do
  begin
    ConstKind := ReadUInt8;
    if ConstKind > Ord(High(TGocciaLocalType)) then
      raise Exception.CreateFmt('Invalid local type kind: %d', [ConstKind]);
    Result.SetLocalType(UInt16(I), TGocciaLocalType(ConstKind));
  end;

  LocalStrictCount := ReadUInt16;
  for I := 0 to LocalStrictCount - 1 do
    Result.SetLocalStrictFlag(UInt16(I), ReadBoolean);

  Result.TypeCheckPreambleSize := ReadUInt16;
  Result.ParameterPreambleSize := ReadUInt16;
  I := ReadUInt16;
  if I = High(UInt16) then
    Result.DirectEvalSyntheticArgumentsSlot := -1
  else
    Result.DirectEvalSyntheticArgumentsSlot := I;
  Result.RejectArgumentsInDirectEval := ReadBoolean;
  finally
    Dec(FFunctionDepth);
  end;
end;

function TGocciaBytecodeReader.ReadModule: TGocciaBytecodeModule;
var
  Magic: array[0..3] of Byte;
  Version: UInt16;
  RuntimeTag, SourcePath: string;
  HasDebug: Boolean;
  ImportCount, ExportCount: UInt16;
  I, J: Integer;
  ModulePath: string;
  BindingCount: UInt16;
  Bindings: array of TGocciaModuleBinding;
begin
  FStream.ReadBuffer(Magic, 4);
  if (Magic[0] <> GOCCIA_BINARY_MAGIC[0]) or (Magic[1] <> GOCCIA_BINARY_MAGIC[1]) or
     (Magic[2] <> GOCCIA_BINARY_MAGIC[2]) or (Magic[3] <> GOCCIA_BINARY_MAGIC[3]) then
    raise Exception.Create('Invalid Goccia bytecode file: bad magic');

  Version := ReadUInt16;
  if Version <> GOCCIA_FORMAT_VERSION then
    raise Exception.CreateFmt('Unsupported bytecode format version: %d (expected %d)',
      [Version, GOCCIA_FORMAT_VERSION]);

  RuntimeTag := ReadString;
  SourcePath := ReadString;
  HasDebug := ReadBoolean;

  Result := TGocciaBytecodeModule.Create(RuntimeTag, SourcePath);
  Result.HasDebugInfo := HasDebug;

  ImportCount := ReadUInt16;
  for I := 0 to ImportCount - 1 do
  begin
    ModulePath := ReadString;
    BindingCount := ReadUInt16;
    SetLength(Bindings, BindingCount);
    for J := 0 to BindingCount - 1 do
    begin
      Bindings[J].ExportName := ReadString;
      Bindings[J].LocalSlot := ReadUInt16;
    end;
    Result.AddImport(ModulePath, Bindings);
  end;

  ExportCount := ReadUInt16;
  for I := 0 to ExportCount - 1 do
    Result.AddExport(ReadString, ReadUInt16);

  Result.TopLevel := ReadFunctionTemplate;
  VerifyFunctionTemplate(Result.TopLevel, 1);
  if FStream.Position <> FStream.Size then
    raise Exception.Create('Invalid trailing data in Goccia bytecode file');
end;

procedure SaveModuleToFile(const AModule: TGocciaBytecodeModule;
  const AFileName: string);
var
  Stream: TFileStream;
  Writer: TGocciaBytecodeWriter;
begin
  Stream := TFileStream.Create(AFileName, fmCreate);
  try
    Writer := TGocciaBytecodeWriter.Create(Stream);
    try
      Writer.WriteModule(AModule);
    finally
      Writer.Free;
    end;
  finally
    Stream.Free;
  end;
end;

function LoadModuleFromFile(const AFileName: string): TGocciaBytecodeModule;
var
  Stream: TFileStream;
  Reader: TGocciaBytecodeReader;
begin
  Stream := TFileStream.Create(AFileName, fmOpenRead);
  try
    Reader := TGocciaBytecodeReader.Create(Stream);
    try
      Result := Reader.ReadModule;
    finally
      Reader.Free;
    end;
  finally
    Stream.Free;
  end;
end;

end.
