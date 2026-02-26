unit Souffle.Bytecode.Binary;

{$I Souffle.inc}

interface

uses
  Classes,

  Souffle.Bytecode.Chunk,
  Souffle.Bytecode.Module;

type
  TSouffleBytecodeWriter = class
  private
    FStream: TStream;
    procedure WriteUInt8(const AValue: UInt8);
    procedure WriteUInt16(const AValue: UInt16);
    procedure WriteUInt32(const AValue: UInt32);
    procedure WriteInt64(const AValue: Int64);
    procedure WriteDouble(const AValue: Double);
    procedure WriteString(const AValue: string);
    procedure WriteBoolean(const AValue: Boolean);
    procedure WriteFunctionPrototype(const AProto: TSouffleFunctionPrototype);
  public
    constructor Create(const AStream: TStream);
    procedure WriteModule(const AModule: TSouffleBytecodeModule);
  end;

  TSouffleBytecodeReader = class
  private
    FStream: TStream;
    function ReadUInt8: UInt8;
    function ReadUInt16: UInt16;
    function ReadUInt32: UInt32;
    function ReadInt64: Int64;
    function ReadDouble: Double;
    function ReadString: string;
    function ReadBoolean: Boolean;
    function ReadFunctionPrototype: TSouffleFunctionPrototype;
  public
    constructor Create(const AStream: TStream);
    function ReadModule: TSouffleBytecodeModule;
  end;

procedure SaveModuleToFile(const AModule: TSouffleBytecodeModule;
  const AFileName: string);
function LoadModuleFromFile(const AFileName: string): TSouffleBytecodeModule;

implementation

uses
  SysUtils,

  Souffle.Bytecode,
  Souffle.Bytecode.Debug;

{ TSouffleBytecodeWriter }

constructor TSouffleBytecodeWriter.Create(const AStream: TStream);
begin
  inherited Create;
  FStream := AStream;
end;

procedure TSouffleBytecodeWriter.WriteUInt8(const AValue: UInt8);
begin
  FStream.WriteBuffer(AValue, SizeOf(UInt8));
end;

procedure TSouffleBytecodeWriter.WriteUInt16(const AValue: UInt16);
begin
  FStream.WriteBuffer(AValue, SizeOf(UInt16));
end;

procedure TSouffleBytecodeWriter.WriteUInt32(const AValue: UInt32);
begin
  FStream.WriteBuffer(AValue, SizeOf(UInt32));
end;

procedure TSouffleBytecodeWriter.WriteInt64(const AValue: Int64);
begin
  FStream.WriteBuffer(AValue, SizeOf(Int64));
end;

procedure TSouffleBytecodeWriter.WriteDouble(const AValue: Double);
begin
  FStream.WriteBuffer(AValue, SizeOf(Double));
end;

procedure TSouffleBytecodeWriter.WriteString(const AValue: string);
var
  Len: UInt32;
  UTF8Str: UTF8String;
begin
  UTF8Str := UTF8String(AValue);
  Len := Length(UTF8Str);
  WriteUInt32(Len);
  if Len > 0 then
    FStream.WriteBuffer(UTF8Str[1], Len);
end;

procedure TSouffleBytecodeWriter.WriteBoolean(const AValue: Boolean);
begin
  if AValue then
    WriteUInt8(1)
  else
    WriteUInt8(0);
end;

procedure TSouffleBytecodeWriter.WriteFunctionPrototype(
  const AProto: TSouffleFunctionPrototype);
var
  I: Integer;
  Constant: TSouffleBytecodeConstant;
  Descriptor: TSouffleUpvalueDescriptor;
  Handler: TSouffleExceptionHandler;
begin
  WriteString(AProto.Name);
  WriteUInt8(AProto.MaxRegisters);
  WriteUInt8(AProto.ParameterCount);
  WriteUInt8(AProto.UpvalueCount);

  // Code
  WriteUInt32(UInt32(AProto.CodeCount));
  for I := 0 to AProto.CodeCount - 1 do
    WriteUInt32(AProto.GetInstruction(I));

  // Constants
  WriteUInt16(UInt16(AProto.ConstantCount));
  for I := 0 to AProto.ConstantCount - 1 do
  begin
    Constant := AProto.GetConstant(I);
    WriteUInt8(Ord(Constant.Kind));
    case Constant.Kind of
      bckNil, bckTrue, bckFalse:
        ; // Tag only
      bckInteger:
        WriteInt64(Constant.IntValue);
      bckFloat:
        WriteDouble(Constant.FloatValue);
      bckString:
        WriteString(Constant.StringValue);
    end;
  end;

  // Upvalue descriptors
  for I := 0 to AProto.UpvalueCount - 1 do
  begin
    Descriptor := AProto.GetUpvalueDescriptor(I);
    WriteBoolean(Descriptor.IsLocal);
    WriteUInt8(Descriptor.Index);
  end;

  // Exception handlers
  WriteUInt16(UInt16(AProto.ExceptionHandlerCount));
  for I := 0 to AProto.ExceptionHandlerCount - 1 do
  begin
    Handler := AProto.GetExceptionHandler(I);
    WriteUInt32(Handler.TryStart);
    WriteUInt32(Handler.TryEnd);
    WriteUInt32(Handler.CatchTarget);
    WriteUInt32(Handler.FinallyTarget);
    WriteUInt8(Handler.CatchRegister);
  end;

  // Nested functions
  WriteUInt16(UInt16(AProto.FunctionCount));
  for I := 0 to AProto.FunctionCount - 1 do
    WriteFunctionPrototype(AProto.GetFunction(I));

  // Debug info
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
      WriteUInt8(AProto.DebugInfo.GetLocalInfo(I).Slot);
      WriteUInt32(AProto.DebugInfo.GetLocalInfo(I).StartPC);
      WriteUInt32(AProto.DebugInfo.GetLocalInfo(I).EndPC);
    end;
  end;
end;

procedure TSouffleBytecodeWriter.WriteModule(
  const AModule: TSouffleBytecodeModule);
var
  I, J: Integer;
  Import: TSouffleModuleImport;
  Export_: TSouffleModuleExport;
begin
  // Header: magic + version
  FStream.WriteBuffer(SOUFFLE_BINARY_MAGIC, 4);
  WriteUInt16(AModule.FormatVersion);

  // Metadata
  WriteString(AModule.RuntimeTag);
  WriteString(AModule.SourcePath);
  WriteBoolean(AModule.HasDebugInfo);

  // Imports
  WriteUInt16(UInt16(AModule.ImportCount));
  for I := 0 to AModule.ImportCount - 1 do
  begin
    Import := AModule.GetImport(I);
    WriteString(Import.ModulePath);
    WriteUInt16(UInt16(Length(Import.Bindings)));
    for J := 0 to High(Import.Bindings) do
    begin
      WriteString(Import.Bindings[J].ExportName);
      WriteUInt16(Import.Bindings[J].LocalSlot);
    end;
  end;

  // Exports
  WriteUInt16(UInt16(AModule.ExportCount));
  for I := 0 to AModule.ExportCount - 1 do
  begin
    Export_ := AModule.GetExport(I);
    WriteString(Export_.Name);
    WriteUInt16(Export_.LocalSlot);
  end;

  // Top-level function
  WriteFunctionPrototype(AModule.TopLevel);
end;

{ TSouffleBytecodeReader }

constructor TSouffleBytecodeReader.Create(const AStream: TStream);
begin
  inherited Create;
  FStream := AStream;
end;

function TSouffleBytecodeReader.ReadUInt8: UInt8;
begin
  FStream.ReadBuffer(Result, SizeOf(UInt8));
end;

function TSouffleBytecodeReader.ReadUInt16: UInt16;
begin
  FStream.ReadBuffer(Result, SizeOf(UInt16));
end;

function TSouffleBytecodeReader.ReadUInt32: UInt32;
begin
  FStream.ReadBuffer(Result, SizeOf(UInt32));
end;

function TSouffleBytecodeReader.ReadInt64: Int64;
begin
  FStream.ReadBuffer(Result, SizeOf(Int64));
end;

function TSouffleBytecodeReader.ReadDouble: Double;
begin
  FStream.ReadBuffer(Result, SizeOf(Double));
end;

function TSouffleBytecodeReader.ReadString: string;
var
  Len: UInt32;
  UTF8Str: UTF8String;
begin
  Len := ReadUInt32;
  if Len = 0 then
    Exit('');
  SetLength(UTF8Str, Len);
  FStream.ReadBuffer(UTF8Str[1], Len);
  Result := string(UTF8Str);
end;

function TSouffleBytecodeReader.ReadBoolean: Boolean;
begin
  Result := ReadUInt8 <> 0;
end;

function TSouffleBytecodeReader.ReadFunctionPrototype: TSouffleFunctionPrototype;
var
  Name: string;
  MaxRegs, ParamCount, UpvalueCount: UInt8;
  CodeCount: UInt32;
  ConstCount, FuncCount, HandlerCount: UInt16;
  I: Integer;
  ConstKind: UInt8;
  HasDebug: Boolean;
  DebugInfo: TSouffleDebugInfo;
  SourceFile: string;
  LineMapCount, LocalCount: UInt32;
begin
  Name := ReadString;
  MaxRegs := ReadUInt8;
  ParamCount := ReadUInt8;
  UpvalueCount := ReadUInt8;

  Result := TSouffleFunctionPrototype.Create(Name);
  Result.MaxRegisters := MaxRegs;
  Result.ParameterCount := ParamCount;

  // Code
  CodeCount := ReadUInt32;
  for I := 0 to CodeCount - 1 do
    Result.EmitInstruction(ReadUInt32);

  // Constants
  ConstCount := ReadUInt16;
  for I := 0 to ConstCount - 1 do
  begin
    ConstKind := ReadUInt8;
    case TSouffleBytecodeConstantKind(ConstKind) of
      bckNil:     Result.AddConstantNil;
      bckTrue:    Result.AddConstantBoolean(True);
      bckFalse:   Result.AddConstantBoolean(False);
      bckInteger: Result.AddConstantInteger(ReadInt64);
      bckFloat:   Result.AddConstantFloat(ReadDouble);
      bckString:  Result.AddConstantString(ReadString);
    end;
  end;

  // Upvalue descriptors
  for I := 0 to UpvalueCount - 1 do
    Result.AddUpvalueDescriptor(ReadBoolean, ReadUInt8);

  // Exception handlers
  HandlerCount := ReadUInt16;
  for I := 0 to HandlerCount - 1 do
    Result.AddExceptionHandler(ReadUInt32, ReadUInt32, ReadUInt32,
      ReadUInt32, ReadUInt8);

  // Nested functions
  FuncCount := ReadUInt16;
  for I := 0 to FuncCount - 1 do
    Result.AddFunction(ReadFunctionPrototype);

  // Debug info
  HasDebug := ReadBoolean;
  if HasDebug then
  begin
    SourceFile := ReadString;
    DebugInfo := TSouffleDebugInfo.Create(SourceFile);

    LineMapCount := ReadUInt32;
    for I := 0 to LineMapCount - 1 do
      DebugInfo.AddLineMapping(ReadUInt32, ReadUInt32, ReadUInt16);

    LocalCount := ReadUInt32;
    for I := 0 to LocalCount - 1 do
      DebugInfo.AddLocal(ReadString, ReadUInt8, ReadUInt32, ReadUInt32);

    Result.DebugInfo := DebugInfo;
  end;
end;

function TSouffleBytecodeReader.ReadModule: TSouffleBytecodeModule;
var
  Magic: array[0..3] of Byte;
  Version: UInt16;
  RuntimeTag, SourcePath: string;
  HasDebug: Boolean;
  ImportCount, ExportCount: UInt16;
  I, J: Integer;
  ModulePath: string;
  BindingCount: UInt16;
  Bindings: array of TSouffleModuleBinding;
begin
  FStream.ReadBuffer(Magic, 4);
  if (Magic[0] <> SOUFFLE_BINARY_MAGIC[0]) or (Magic[1] <> SOUFFLE_BINARY_MAGIC[1]) or
     (Magic[2] <> SOUFFLE_BINARY_MAGIC[2]) or (Magic[3] <> SOUFFLE_BINARY_MAGIC[3]) then
    raise Exception.Create('Invalid Souffle bytecode file: bad magic');

  Version := ReadUInt16;
  if Version <> SOUFFLE_FORMAT_VERSION then
    raise Exception.CreateFmt('Unsupported bytecode format version: %d (expected %d)',
      [Version, SOUFFLE_FORMAT_VERSION]);

  RuntimeTag := ReadString;
  SourcePath := ReadString;
  HasDebug := ReadBoolean;

  Result := TSouffleBytecodeModule.Create(RuntimeTag, SourcePath);
  Result.HasDebugInfo := HasDebug;

  // Imports
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

  // Exports
  ExportCount := ReadUInt16;
  for I := 0 to ExportCount - 1 do
    Result.AddExport(ReadString, ReadUInt16);

  // Top-level function
  Result.TopLevel := ReadFunctionPrototype;
end;

{ File I/O helpers }

procedure SaveModuleToFile(const AModule: TSouffleBytecodeModule;
  const AFileName: string);
var
  Stream: TFileStream;
  Writer: TSouffleBytecodeWriter;
begin
  Stream := TFileStream.Create(AFileName, fmCreate);
  try
    Writer := TSouffleBytecodeWriter.Create(Stream);
    try
      Writer.WriteModule(AModule);
    finally
      Writer.Free;
    end;
  finally
    Stream.Free;
  end;
end;

function LoadModuleFromFile(const AFileName: string): TSouffleBytecodeModule;
var
  Stream: TFileStream;
  Reader: TSouffleBytecodeReader;
begin
  Stream := TFileStream.Create(AFileName, fmOpenRead);
  try
    Reader := TSouffleBytecodeReader.Create(Stream);
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
