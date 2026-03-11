unit Souffle.Bytecode.Chunk;

{$I Souffle.inc}

interface

uses
  Generics.Collections,

  OrderedStringMap,
  Souffle.Bytecode.Debug;

type
  TSouffleBytecodeConstantKind = (
    bckNil,
    bckTrue,
    bckFalse,
    bckInteger,
    bckFloat,
    bckString
  );

  TSouffleBytecodeConstant = record
    Kind: TSouffleBytecodeConstantKind;
    IntValue: Int64;
    FloatValue: Double;
    StringValue: string;
  end;

  TSouffleUpvalueDescriptor = record
    IsLocal: Boolean;
    Index: UInt8;
  end;

  TSouffleExceptionHandler = record
    TryStart: UInt32;
    TryEnd: UInt32;
    CatchTarget: UInt32;
    FinallyTarget: UInt32;
    CatchRegister: UInt8;
  end;

  TSouffleLocalType = (
    sltUntyped,
    sltInteger,
    sltFloat,
    sltBoolean,
    sltString,
    sltReference
  );

  TSouffleFunctionTemplate = class
  private
    FName: string;
    FCode: array of UInt32;
    FCodeCount: Integer;
    FConstants: array of TSouffleBytecodeConstant;
    FConstantCount: Integer;
    FFunctions: TObjectList<TSouffleFunctionTemplate>;
    FUpvalueDescriptors: array of TSouffleUpvalueDescriptor;
    FExceptionHandlers: array of TSouffleExceptionHandler;
    FExceptionHandlerCount: Integer;
    FMaxRegisters: UInt8;
    FParameterCount: UInt8;
    FUpvalueCount: UInt8;
    FDebugInfo: TSouffleDebugInfo;
    FLocalTypes: array of TSouffleLocalType;
    FLocalTypeCount: UInt8;
    FLocalStrictFlags: array of Boolean;
    FLocalStrictCount: UInt8;
    FIsAsync: Boolean;
    FTypeCheckPreambleSize: UInt8;
    FStringConstantIndex: TOrderedStringMap<UInt16>;
    function GetFunctionCount: Integer;
  public
    constructor Create(const AName: string);
    destructor Destroy; override;

    function EmitInstruction(const AInstruction: UInt32): Integer;
    procedure PatchInstruction(const AIndex: Integer; const AInstruction: UInt32);
    function AddConstantNil: UInt16;
    function AddConstantBoolean(const AValue: Boolean): UInt16;
    function AddConstantInteger(const AValue: Int64): UInt16;
    function AddConstantFloat(const AValue: Double): UInt16;
    function AddConstantString(const AValue: string): UInt16;
    function AddFunction(const AFunction: TSouffleFunctionTemplate): UInt16;
    procedure AddUpvalueDescriptor(const AIsLocal: Boolean; const AIndex: UInt8);
    procedure AddExceptionHandler(const ATryStart, ATryEnd, ACatchTarget,
      AFinallyTarget: UInt32; const ACatchRegister: UInt8);

    function GetInstruction(const AIndex: Integer): UInt32; inline;
    function GetConstant(const AIndex: Integer): TSouffleBytecodeConstant; inline;
    function GetFunction(const AIndex: Integer): TSouffleFunctionTemplate;
    function GetUpvalueDescriptor(const AIndex: Integer): TSouffleUpvalueDescriptor;
    function GetExceptionHandler(const AIndex: Integer): TSouffleExceptionHandler;

    property Name: string read FName write FName;
    property CodeCount: Integer read FCodeCount;
    property ConstantCount: Integer read FConstantCount;
    property FunctionCount: Integer read GetFunctionCount;
    property ExceptionHandlerCount: Integer read FExceptionHandlerCount;
    property MaxRegisters: UInt8 read FMaxRegisters write FMaxRegisters;
    property ParameterCount: UInt8 read FParameterCount write FParameterCount;
    property UpvalueCount: UInt8 read FUpvalueCount;
    property DebugInfo: TSouffleDebugInfo read FDebugInfo write FDebugInfo;

    procedure SetLocalType(const ASlot: UInt8; const AKind: TSouffleLocalType);
    function GetLocalType(const ASlot: UInt8): TSouffleLocalType;
    property LocalTypeCount: UInt8 read FLocalTypeCount;

    procedure SetLocalStrictFlag(const ASlot: UInt8; const AStrict: Boolean);
    function GetLocalStrictFlag(const ASlot: UInt8): Boolean;
    property LocalStrictCount: UInt8 read FLocalStrictCount;

    property IsAsync: Boolean read FIsAsync write FIsAsync;
    property TypeCheckPreambleSize: UInt8 read FTypeCheckPreambleSize write FTypeCheckPreambleSize;
  end;

const
  NO_FINALLY: UInt32 = $FFFFFFFF;

implementation

uses
  SysUtils;

function FloatBitsAreNaN(const AValue: Double): Boolean; inline;
var
  Bits: UInt64;
begin
  Move(AValue, Bits, SizeOf(Double));
  Result := ((Bits and $7FF0000000000000) = $7FF0000000000000) and
            ((Bits and $000FFFFFFFFFFFFF) <> 0);
end;

{ TSouffleFunctionTemplate }

constructor TSouffleFunctionTemplate.Create(const AName: string);
begin
  inherited Create;
  FName := AName;
  FCodeCount := 0;
  FConstantCount := 0;
  FFunctions := TObjectList<TSouffleFunctionTemplate>.Create(True);
  FStringConstantIndex := TOrderedStringMap<UInt16>.Create;
  FExceptionHandlerCount := 0;
  FMaxRegisters := 0;
  FParameterCount := 0;
  FUpvalueCount := 0;
  FDebugInfo := nil;
  FLocalTypeCount := 0;
  FLocalStrictCount := 0;
end;

destructor TSouffleFunctionTemplate.Destroy;
begin
  FStringConstantIndex.Free;
  FFunctions.Free;
  FDebugInfo.Free;
  inherited;
end;

function TSouffleFunctionTemplate.EmitInstruction(
  const AInstruction: UInt32): Integer;
begin
  if FCodeCount >= Length(FCode) then
    SetLength(FCode, FCodeCount * 2 + 16);
  FCode[FCodeCount] := AInstruction;
  Result := FCodeCount;
  Inc(FCodeCount);
end;

procedure TSouffleFunctionTemplate.PatchInstruction(const AIndex: Integer;
  const AInstruction: UInt32);
begin
  if (AIndex < 0) or (AIndex >= FCodeCount) then
    raise ERangeError.CreateFmt('PatchInstruction: index %d out of range 0..%d', [AIndex, FCodeCount - 1]);
  FCode[AIndex] := AInstruction;
end;

function TSouffleFunctionTemplate.AddConstantNil: UInt16;
var
  I: Integer;
begin
  for I := 0 to FConstantCount - 1 do
    if FConstants[I].Kind = bckNil then
      Exit(UInt16(I));

  if FConstantCount > High(UInt16) then
    raise Exception.Create('Constant pool overflow: exceeds 65535 entries');
  if FConstantCount >= Length(FConstants) then
    SetLength(FConstants, FConstantCount * 2 + 8);
  FConstants[FConstantCount].Kind := bckNil;
  Result := UInt16(FConstantCount);
  Inc(FConstantCount);
end;

function TSouffleFunctionTemplate.AddConstantBoolean(
  const AValue: Boolean): UInt16;
var
  I: Integer;
  Target: TSouffleBytecodeConstantKind;
begin
  if AValue then
    Target := bckTrue
  else
    Target := bckFalse;

  for I := 0 to FConstantCount - 1 do
    if FConstants[I].Kind = Target then
      Exit(UInt16(I));

  if FConstantCount > High(UInt16) then
    raise Exception.Create('Constant pool overflow: exceeds 65535 entries');
  if FConstantCount >= Length(FConstants) then
    SetLength(FConstants, FConstantCount * 2 + 8);
  FConstants[FConstantCount].Kind := Target;
  Result := UInt16(FConstantCount);
  Inc(FConstantCount);
end;

function TSouffleFunctionTemplate.AddConstantInteger(
  const AValue: Int64): UInt16;
var
  I: Integer;
begin
  for I := 0 to FConstantCount - 1 do
    if (FConstants[I].Kind = bckInteger) and (FConstants[I].IntValue = AValue) then
      Exit(UInt16(I));

  if FConstantCount > High(UInt16) then
    raise Exception.Create('Constant pool overflow: exceeds 65535 entries');
  if FConstantCount >= Length(FConstants) then
    SetLength(FConstants, FConstantCount * 2 + 8);
  FConstants[FConstantCount].Kind := bckInteger;
  FConstants[FConstantCount].IntValue := AValue;
  Result := UInt16(FConstantCount);
  Inc(FConstantCount);
end;

function TSouffleFunctionTemplate.AddConstantFloat(
  const AValue: Double): UInt16;
var
  I: Integer;
begin
  for I := 0 to FConstantCount - 1 do
    if (FConstants[I].Kind = bckFloat) and
       ((FConstants[I].FloatValue = AValue) or
        (FloatBitsAreNaN(FConstants[I].FloatValue) and FloatBitsAreNaN(AValue))) then
      Exit(UInt16(I));

  if FConstantCount > High(UInt16) then
    raise Exception.Create('Constant pool overflow: exceeds 65535 entries');
  if FConstantCount >= Length(FConstants) then
    SetLength(FConstants, FConstantCount * 2 + 8);
  FConstants[FConstantCount].Kind := bckFloat;
  FConstants[FConstantCount].FloatValue := AValue;
  Result := UInt16(FConstantCount);
  Inc(FConstantCount);
end;

function TSouffleFunctionTemplate.AddConstantString(
  const AValue: string): UInt16;
begin
  if FStringConstantIndex.TryGetValue(AValue, Result) then
    Exit;

  if FConstantCount > High(UInt16) then
    raise Exception.Create('Constant pool overflow: exceeds 65535 entries');
  if FConstantCount >= Length(FConstants) then
    SetLength(FConstants, FConstantCount * 2 + 8);
  FConstants[FConstantCount].Kind := bckString;
  FConstants[FConstantCount].StringValue := AValue;
  Result := UInt16(FConstantCount);
  FStringConstantIndex.Add(AValue, Result);
  Inc(FConstantCount);
end;

function TSouffleFunctionTemplate.AddFunction(
  const AFunction: TSouffleFunctionTemplate): UInt16;
begin
  if FFunctions.Count > High(UInt16) then
    raise Exception.Create('Function pool overflow: exceeds 65535 entries');
  Result := UInt16(FFunctions.Count);
  FFunctions.Add(AFunction);
end;

procedure TSouffleFunctionTemplate.AddUpvalueDescriptor(
  const AIsLocal: Boolean; const AIndex: UInt8);
begin
  if FUpvalueCount >= High(UInt8) then
    raise Exception.Create('Upvalue descriptor overflow: exceeds 255 entries');
  if FUpvalueCount >= Length(FUpvalueDescriptors) then
    SetLength(FUpvalueDescriptors, FUpvalueCount * 2 + 4);
  FUpvalueDescriptors[FUpvalueCount].IsLocal := AIsLocal;
  FUpvalueDescriptors[FUpvalueCount].Index := AIndex;
  Inc(FUpvalueCount);
end;

procedure TSouffleFunctionTemplate.AddExceptionHandler(
  const ATryStart, ATryEnd, ACatchTarget, AFinallyTarget: UInt32;
  const ACatchRegister: UInt8);
begin
  if FExceptionHandlerCount >= Length(FExceptionHandlers) then
    SetLength(FExceptionHandlers, FExceptionHandlerCount * 2 + 4);
  FExceptionHandlers[FExceptionHandlerCount].TryStart := ATryStart;
  FExceptionHandlers[FExceptionHandlerCount].TryEnd := ATryEnd;
  FExceptionHandlers[FExceptionHandlerCount].CatchTarget := ACatchTarget;
  FExceptionHandlers[FExceptionHandlerCount].FinallyTarget := AFinallyTarget;
  FExceptionHandlers[FExceptionHandlerCount].CatchRegister := ACatchRegister;
  Inc(FExceptionHandlerCount);
end;

function TSouffleFunctionTemplate.GetInstruction(
  const AIndex: Integer): UInt32;
begin
  {$IFDEF DEBUG}
  if (AIndex < 0) or (AIndex >= FCodeCount) then
    raise ERangeError.CreateFmt('GetInstruction: index %d out of range 0..%d', [AIndex, FCodeCount - 1]);
  {$ENDIF}
  Result := FCode[AIndex];
end;

function TSouffleFunctionTemplate.GetConstant(
  const AIndex: Integer): TSouffleBytecodeConstant;
begin
  {$IFDEF DEBUG}
  if (AIndex < 0) or (AIndex >= FConstantCount) then
    raise ERangeError.CreateFmt('GetConstant: index %d out of range 0..%d', [AIndex, FConstantCount - 1]);
  {$ENDIF}
  Result := FConstants[AIndex];
end;

function TSouffleFunctionTemplate.GetFunction(
  const AIndex: Integer): TSouffleFunctionTemplate;
begin
  {$IFDEF DEBUG}
  if (AIndex < 0) or (AIndex >= FFunctions.Count) then
    raise ERangeError.CreateFmt('GetFunction: index %d out of range 0..%d', [AIndex, FFunctions.Count - 1]);
  {$ENDIF}
  Result := FFunctions[AIndex];
end;

function TSouffleFunctionTemplate.GetUpvalueDescriptor(
  const AIndex: Integer): TSouffleUpvalueDescriptor;
begin
  {$IFDEF DEBUG}
  if (AIndex < 0) or (AIndex >= FUpvalueCount) then
    raise ERangeError.CreateFmt('GetUpvalueDescriptor: index %d out of range 0..%d', [AIndex, FUpvalueCount - 1]);
  {$ENDIF}
  Result := FUpvalueDescriptors[AIndex];
end;

function TSouffleFunctionTemplate.GetFunctionCount: Integer;
begin
  Result := FFunctions.Count;
end;

function TSouffleFunctionTemplate.GetExceptionHandler(
  const AIndex: Integer): TSouffleExceptionHandler;
begin
  {$IFDEF DEBUG}
  if (AIndex < 0) or (AIndex >= FExceptionHandlerCount) then
    raise ERangeError.CreateFmt('GetExceptionHandler: index %d out of range 0..%d', [AIndex, FExceptionHandlerCount - 1]);
  {$ENDIF}
  Result := FExceptionHandlers[AIndex];
end;

procedure TSouffleFunctionTemplate.SetLocalType(const ASlot: UInt8;
  const AKind: TSouffleLocalType);
begin
  if ASlot >= Length(FLocalTypes) then
    SetLength(FLocalTypes, ASlot + 1);
  FLocalTypes[ASlot] := AKind;
  if ASlot >= FLocalTypeCount then
    FLocalTypeCount := ASlot + 1;
end;

function TSouffleFunctionTemplate.GetLocalType(
  const ASlot: UInt8): TSouffleLocalType;
begin
  if ASlot < FLocalTypeCount then
    Result := FLocalTypes[ASlot]
  else
    Result := sltUntyped;
end;

procedure TSouffleFunctionTemplate.SetLocalStrictFlag(const ASlot: UInt8;
  const AStrict: Boolean);
begin
  if ASlot >= Length(FLocalStrictFlags) then
    SetLength(FLocalStrictFlags, ASlot + 1);
  FLocalStrictFlags[ASlot] := AStrict;
  if ASlot >= FLocalStrictCount then
    FLocalStrictCount := ASlot + 1;
end;

function TSouffleFunctionTemplate.GetLocalStrictFlag(
  const ASlot: UInt8): Boolean;
begin
  if ASlot < FLocalStrictCount then
    Result := FLocalStrictFlags[ASlot]
  else
    Result := False;
end;

end.
