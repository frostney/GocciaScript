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
    function ReadUInt8: UInt8;
    function ReadUInt16: UInt16;
    function ReadUInt32: UInt32;
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

  TextSemantics,

  Goccia.Bytecode,
  Goccia.Bytecode.Debug;

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
  LE: UInt16;
begin
  LE := NtoLE(AValue);
  FStream.WriteBuffer(LE, SizeOf(UInt16));
end;

procedure TGocciaBytecodeWriter.WriteUInt32(const AValue: UInt32);
var
  LE: UInt32;
begin
  LE := NtoLE(AValue);
  FStream.WriteBuffer(LE, SizeOf(UInt32));
end;

procedure TGocciaBytecodeWriter.WriteInt64(const AValue: Int64);
var
  LE: Int64;
begin
  LE := NtoLE(AValue);
  FStream.WriteBuffer(LE, SizeOf(Int64));
end;

procedure TGocciaBytecodeWriter.WriteDouble(const AValue: Double);
var
  Bits: Int64;
begin
  Move(AValue, Bits, SizeOf(Double));
  Bits := NtoLE(Bits);
  FStream.WriteBuffer(Bits, SizeOf(Int64));
end;

procedure TGocciaBytecodeWriter.WriteString(const AValue: string);
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
  Handler: TGocciaExceptionHandler;
begin
  WriteString(AProto.Name);
  WriteUInt8(AProto.MaxRegisters);
  WriteUInt8(AProto.ParameterCount);
  WriteUInt8(AProto.FormalParameterCount);
  WriteUInt8(AProto.UpvalueCount);
  WriteBoolean(AProto.IsArrow);
  WriteBoolean(AProto.IsGenerator);
  WriteBoolean(AProto.IsAsync);

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
    WriteUInt8(Descriptor.Index);
  end;

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
      WriteUInt8(AProto.DebugInfo.GetLocalInfo(I).Slot);
      WriteUInt32(AProto.DebugInfo.GetLocalInfo(I).StartPC);
      WriteUInt32(AProto.DebugInfo.GetLocalInfo(I).EndPC);
    end;
  end;

  WriteUInt8(AProto.LocalTypeCount);
  for I := 0 to AProto.LocalTypeCount - 1 do
    WriteUInt8(Ord(AProto.GetLocalType(UInt8(I))));

  WriteUInt8(AProto.LocalStrictCount);
  for I := 0 to AProto.LocalStrictCount - 1 do
    WriteBoolean(AProto.GetLocalStrictFlag(UInt8(I)));

  WriteUInt8(AProto.TypeCheckPreambleSize);
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
  FStream := AStream;
end;

function TGocciaBytecodeReader.ReadUInt8: UInt8;
begin
  FStream.ReadBuffer(Result, SizeOf(UInt8));
end;

function TGocciaBytecodeReader.ReadUInt16: UInt16;
begin
  FStream.ReadBuffer(Result, SizeOf(UInt16));
  Result := LEtoN(Result);
end;

function TGocciaBytecodeReader.ReadUInt32: UInt32;
begin
  FStream.ReadBuffer(Result, SizeOf(UInt32));
  Result := LEtoN(Result);
end;

function TGocciaBytecodeReader.ReadInt64: Int64;
begin
  FStream.ReadBuffer(Result, SizeOf(Int64));
  Result := LEtoN(Result);
end;

function TGocciaBytecodeReader.ReadDouble: Double;
var
  Bits: Int64;
begin
  FStream.ReadBuffer(Bits, SizeOf(Int64));
  Bits := LEtoN(Bits);
  Move(Bits, Result, SizeOf(Double));
end;

function TGocciaBytecodeReader.ReadString: string;
var
  Len: UInt32;
  UTF8Str: UTF8String;
begin
  Len := ReadUInt32;
  if Len = 0 then
    Exit('');
  SetLength(UTF8Str, Len);
  FStream.ReadBuffer(UTF8Str[1], Len);
  Result := RetagUTF8Text(RawByteString(UTF8Str));
end;

function TGocciaBytecodeReader.ReadBoolean: Boolean;
begin
  Result := ReadUInt8 <> 0;
end;

function TGocciaBytecodeReader.ReadFunctionTemplate: TGocciaFunctionTemplate;
var
  Name: string;
  MaxRegs, ParamCount, UpvalueCount, LocalTypeCount, LocalStrictCount: UInt8;
  CodeCount: UInt32;
  ConstCount, FuncCount, HandlerCount, StrCount: UInt16;
  I, J: Integer;
  ConstKind: UInt8;
  HasDebug: Boolean;
  DebugInfo: TGocciaDebugInfo;
  SourceFile: string;
  LineMapCount, LocalCount: UInt32;
  CookedStrings, RawStrings: TGocciaBytecodeStringArray;
  CookedValid: TGocciaBytecodeTemplateCookedValid;
begin
  Name := ReadString;
  MaxRegs := ReadUInt8;
  ParamCount := ReadUInt8;
  LocalTypeCount := 0;
  LocalStrictCount := 0;
  HasDebug := False;

  Result := TGocciaFunctionTemplate.Create(Name);
  Result.MaxRegisters := MaxRegs;
  Result.ParameterCount := ParamCount;
  Result.FormalParameterCount := ReadUInt8;
  UpvalueCount := ReadUInt8;
  Result.IsArrow := ReadBoolean;
  Result.IsGenerator := ReadBoolean;
  Result.IsAsync := ReadBoolean;

  CodeCount := ReadUInt32;
  for I := 0 to CodeCount - 1 do
    Result.EmitInstruction(ReadUInt32);

  ConstCount := ReadUInt16;
  for I := 0 to ConstCount - 1 do
  begin
    ConstKind := ReadUInt8;
    case TGocciaBytecodeConstantKind(ConstKind) of
      bckNil:     Result.AddConstantNil;
      bckTrue:    Result.AddConstantBoolean(True);
      bckFalse:   Result.AddConstantBoolean(False);
      bckInteger: Result.AddConstantInteger(ReadInt64);
      bckFloat:   Result.AddConstantFloat(ReadDouble);
      bckString:  Result.AddConstantString(ReadString);
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
    Result.AddUpvalueDescriptor(ReadBoolean, ReadUInt8);

  HandlerCount := ReadUInt16;
  for I := 0 to HandlerCount - 1 do
    Result.AddExceptionHandler(ReadUInt32, ReadUInt32, ReadUInt32,
      ReadUInt32, ReadUInt8);

  FuncCount := ReadUInt16;
  for I := 0 to FuncCount - 1 do
    Result.AddFunction(ReadFunctionTemplate);

  HasDebug := ReadBoolean;
  if HasDebug then
  begin
    SourceFile := ReadString;
    DebugInfo := TGocciaDebugInfo.Create(SourceFile);

    LineMapCount := ReadUInt32;
    for I := 0 to LineMapCount - 1 do
      DebugInfo.AddLineMapping(ReadUInt32, ReadUInt32, ReadUInt16);

    LocalCount := ReadUInt32;
    for I := 0 to LocalCount - 1 do
      DebugInfo.AddLocal(ReadString, ReadUInt8, ReadUInt32, ReadUInt32);

    Result.DebugInfo := DebugInfo;
  end;

  LocalTypeCount := ReadUInt8;
  for I := 0 to LocalTypeCount - 1 do
    Result.SetLocalType(UInt8(I), TGocciaLocalType(ReadUInt8));

  LocalStrictCount := ReadUInt8;
  for I := 0 to LocalStrictCount - 1 do
    Result.SetLocalStrictFlag(UInt8(I), ReadBoolean);

  Result.TypeCheckPreambleSize := ReadUInt8;
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
