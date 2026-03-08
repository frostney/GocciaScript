unit Souffle.Wasm.Emitter;

{$I Souffle.inc}

interface

uses
  Classes,
  SysUtils;

const
  WASM_MAGIC   = $6D736100;
  WASM_VERSION = $01;

  // Section IDs (WASM 3.0 order)
  SEC_TYPE      = 1;
  SEC_IMPORT    = 2;
  SEC_FUNCTION  = 3;
  SEC_TABLE     = 4;
  SEC_MEMORY    = 5;
  SEC_GLOBAL    = 6;
  SEC_EXPORT    = 7;
  SEC_START     = 8;
  SEC_ELEMENT   = 9;
  SEC_CODE      = 10;
  SEC_DATA      = 11;
  SEC_DATACOUNT = 12;
  SEC_TAG       = 13;

  // Value types
  WT_I32       = $7F;
  WT_I64       = $7E;
  WT_F32       = $7D;
  WT_F64       = $7C;
  WT_FUNCREF   = $70;
  WT_EXTERNREF = $6F;
  WT_EXNREF    = $69;
  WT_VOID      = $40;

  // Heap types (abstract)
  HT_FUNC      = $70;
  HT_EXTERN    = $6F;
  HT_ANY       = $6E;
  HT_EQ        = $6D;
  HT_I31       = $6C;
  HT_STRUCT    = $6B;
  HT_ARRAY     = $6A;
  HT_EXN       = $69;
  HT_NOEXN     = $68;
  HT_NONE      = $71;
  HT_NOFUNC    = $73;
  HT_NOEXTERN  = $72;

  // Export / Import kinds
  EK_FUNCTION  = $00;
  EK_TABLE     = $01;
  EK_MEMORY    = $02;
  EK_GLOBAL    = $03;
  EK_TAG       = $04;

  IK_FUNCTION  = $00;
  IK_TABLE     = $01;
  IK_MEMORY    = $02;
  IK_GLOBAL    = $03;
  IK_TAG       = $04;

  // Composite type constructors
  CT_FUNC      = $60;
  CT_STRUCT    = $5F;
  CT_ARRAY     = $5E;

  // Sub type constructors
  ST_SUB       = $50;
  ST_SUB_FINAL = $4F;
  ST_REC       = $4E;

  // Field mutability
  MUT_CONST    = $00;
  MUT_VAR      = $01;

  // Packed storage types
  PACK_I8      = $78;
  PACK_I16     = $77;

  // Catch clause kinds (for try_table)
  CATCH_TAG          = $00;
  CATCH_TAG_REF      = $01;
  CATCH_ALL_KIND     = $02;
  CATCH_ALL_REF      = $03;

type
  TWasmValType = Byte;

  TWasmValTypeArray = array of TWasmValType;

  TWasmFieldType = record
    StorageType: Byte;
    IsRefType: Boolean;
    Nullable: Boolean;
    Mutable: Boolean;
  end;

  TWasmCompositeTypeKind = (ctkFunc, ctkStruct, ctkArray);

  TWasmCompositeType = record
    Kind: TWasmCompositeTypeKind;
    Params: TWasmValTypeArray;
    Results: TWasmValTypeArray;
    Fields: array of TWasmFieldType;
    ElementType: TWasmFieldType;
  end;

  TWasmImport = record
    Module: string;
    Name: string;
    Kind: Byte;
    TypeIdx: UInt32;
    Min, Max: UInt32;
    HasMax: Boolean;
  end;

  TWasmExport = record
    Name: string;
    Kind: Byte;
    Index: UInt32;
  end;

  TWasmLocal = record
    Count: UInt32;
    ValType: TWasmValType;
  end;

  TWasmTag = record
    TypeIdx: UInt32;
  end;

  TWasmCatchClause = record
    Kind: Byte;
    TagIdx: UInt32;
    LabelIdx: UInt32;
  end;

  { TWasmCodeBuilder - builds the bytecode for a single WASM function }

  TWasmCodeBuilder = class
  private
    FCode: TMemoryStream;
    FLocals: array of TWasmLocal;
    FParamCount: Integer;

    procedure EmitGCOp(ASubOp: UInt32);
    procedure EmitGCOpWithType(ASubOp: UInt32; ATypeIdx: UInt32);
    procedure EmitGCOpWithTypeAndField(ASubOp: UInt32; ATypeIdx, AFieldIdx: UInt32);

  public
    constructor Create(AParamCount: Integer);
    destructor Destroy; override;

    procedure AddLocal(AValType: TWasmValType; ACount: UInt32 = 1);

    // Control flow
    procedure EmitBlock(ABlockType: Byte = WT_VOID);
    procedure EmitLoop(ABlockType: Byte = WT_VOID);
    procedure EmitIf(ABlockType: Byte = WT_VOID);
    procedure EmitElse;
    procedure EmitEnd;
    procedure EmitBr(ALabelIdx: UInt32);
    procedure EmitBrIf(ALabelIdx: UInt32);
    procedure EmitReturn;
    procedure EmitUnreachable;
    procedure EmitCall(AFuncIdx: UInt32);
    procedure EmitCallIndirect(ATypeIdx: UInt32; ATableIdx: UInt32 = 0);

    // Variables
    procedure EmitLocalGet(AIdx: UInt32);
    procedure EmitLocalSet(AIdx: UInt32);
    procedure EmitLocalTee(AIdx: UInt32);
    procedure EmitGlobalGet(AIdx: UInt32);
    procedure EmitGlobalSet(AIdx: UInt32);

    // Constants
    procedure EmitI32Const(AValue: Int32);
    procedure EmitI64Const(AValue: Int64);
    procedure EmitF64Const(AValue: Double);

    // i32 Arithmetic
    procedure EmitI32Add;
    procedure EmitI32Sub;
    procedure EmitI32Mul;
    procedure EmitI32DivS;
    procedure EmitI32RemS;
    procedure EmitI32And;
    procedure EmitI32Or;
    procedure EmitI32Xor;
    procedure EmitI32Shl;
    procedure EmitI32ShrS;

    // i32 Comparison
    procedure EmitI32Eqz;
    procedure EmitI32Eq;
    procedure EmitI32Ne;
    procedure EmitI32LtS;
    procedure EmitI32GtS;
    procedure EmitI32LeS;
    procedure EmitI32GeS;

    // f64 Arithmetic
    procedure EmitF64Add;
    procedure EmitF64Sub;
    procedure EmitF64Mul;
    procedure EmitF64Div;

    // f64 Comparison
    procedure EmitF64Eq;
    procedure EmitF64Ne;
    procedure EmitF64Lt;
    procedure EmitF64Gt;
    procedure EmitF64Le;
    procedure EmitF64Ge;

    // Conversions
    procedure EmitI64ExtendI32S;
    procedure EmitF64ConvertI32S;
    procedure EmitI32WrapI64;

    // Memory
    procedure EmitI32Load(AAlign: UInt32 = 2; AOffset: UInt32 = 0);
    procedure EmitI32Store(AAlign: UInt32 = 2; AOffset: UInt32 = 0);
    procedure EmitMemoryGrow;

    // Parametric
    procedure EmitDrop;
    procedure EmitSelect;

    // Exception Handling
    procedure EmitTryTable(ABlockType: Byte; const ACatches: array of TWasmCatchClause);
    procedure EmitThrow(ATagIdx: UInt32);
    procedure EmitThrowRef;
    procedure EmitRethrow(ADepth: UInt32);
    procedure EmitTry(ABlockType: Byte = WT_VOID);
    procedure EmitCatch(ATagIdx: UInt32);
    procedure EmitCatchAll;
    procedure EmitDelegate(ADepth: UInt32);

    // Tail Calls
    procedure EmitReturnCall(AFuncIdx: UInt32);
    procedure EmitReturnCallIndirect(ATypeIdx: UInt32; ATableIdx: UInt32 = 0);
    procedure EmitCallRef(ATypeIdx: UInt32);
    procedure EmitReturnCallRef(ATypeIdx: UInt32);

    // Reference Types
    procedure EmitRefNull(AHeapType: Byte);
    procedure EmitRefIsNull;
    procedure EmitRefFunc(AFuncIdx: UInt32);
    procedure EmitRefEq;
    procedure EmitRefAsNonNull;
    procedure EmitBrOnNull(ALabelIdx: UInt32);
    procedure EmitBrOnNonNull(ALabelIdx: UInt32);

    // GC: Struct
    procedure EmitStructNew(ATypeIdx: UInt32);
    procedure EmitStructNewDefault(ATypeIdx: UInt32);
    procedure EmitStructGet(ATypeIdx: UInt32; AFieldIdx: UInt32);
    procedure EmitStructGetS(ATypeIdx: UInt32; AFieldIdx: UInt32);
    procedure EmitStructGetU(ATypeIdx: UInt32; AFieldIdx: UInt32);
    procedure EmitStructSet(ATypeIdx: UInt32; AFieldIdx: UInt32);

    // GC: Array
    procedure EmitArrayNew(ATypeIdx: UInt32);
    procedure EmitArrayNewDefault(ATypeIdx: UInt32);
    procedure EmitArrayNewFixed(ATypeIdx: UInt32; ALength: UInt32);
    procedure EmitArrayGet(ATypeIdx: UInt32);
    procedure EmitArrayGetS(ATypeIdx: UInt32);
    procedure EmitArrayGetU(ATypeIdx: UInt32);
    procedure EmitArraySet(ATypeIdx: UInt32);
    procedure EmitArrayLen;
    procedure EmitArrayFill(ATypeIdx: UInt32);
    procedure EmitArrayCopy(ADstTypeIdx, ASrcTypeIdx: UInt32);

    // GC: i31
    procedure EmitRefI31;
    procedure EmitI31GetS;
    procedure EmitI31GetU;

    // GC: Casting
    procedure EmitRefTest(AHeapType: Byte);
    procedure EmitRefTestNull(AHeapType: Byte);
    procedure EmitRefCast(AHeapType: Byte);
    procedure EmitRefCastNull(AHeapType: Byte);

    // GC: Extern Conversion
    procedure EmitAnyConvertExtern;
    procedure EmitExternConvertAny;

    // Raw emission
    procedure EmitByte(AB: Byte);
    procedure EmitBytes(const ABytes: array of Byte);
    procedure EmitLEB128U(AValue: UInt64);
    procedure EmitLEB128S(AValue: Int64);

    function Encode: TBytes;
  end;

  { TWasmModule - builds a complete WASM 3.0 module }

  TWasmModule = class
  private
    FTypes: array of TWasmCompositeType;
    FImports: array of TWasmImport;
    FFuncTypeIndices: array of UInt32;
    FExports: array of TWasmExport;
    FCodeBuilders: array of TWasmCodeBuilder;
    FTags: array of TWasmTag;
    FHasMemory: Boolean;
    FMemoryMin: UInt32;
    FMemoryMax: UInt32;
    FMemoryHasMax: Boolean;
    FDataSegments: array of record
      Offset: UInt32;
      Data: TBytes;
    end;
    FCustomSections: array of record
      Name: string;
      Data: TBytes;
    end;

  protected
    procedure WriteSection(AStream: TStream; ASectionID: Byte; const AContent: TBytes);
    procedure WriteTypeSection(AStream: TStream);
    procedure WriteImportSection(AStream: TStream);
    procedure WriteFunctionSection(AStream: TStream);
    procedure WriteMemorySection(AStream: TStream);
    procedure WriteTagSection(AStream: TStream);
    procedure WriteExportSection(AStream: TStream);
    procedure WriteCodeSection(AStream: TStream);
    procedure WriteDataSection(AStream: TStream);
    procedure WriteCustomSections(AStream: TStream);

  public
    constructor Create;
    destructor Destroy; override;

    // Types
    function AddFuncType(const AParams, AResults: array of TWasmValType): UInt32;
    function FindOrAddFuncType(const AParams, AResults: array of TWasmValType): UInt32;
    function AddStructType(const AFields: array of TWasmFieldType): UInt32;
    function AddArrayType(const AElementType: TWasmFieldType): UInt32;

    // Imports
    function ImportFunction(const AModuleName, AFuncName: string;
      const AParams, AResults: array of TWasmValType): UInt32;
    procedure ImportMemory(const AModuleName, AMemName: string;
      AMin: UInt32; AMax: UInt32 = 0; AHasMax: Boolean = False);
    function ImportTag(const AModuleName, ATagName: string;
      const APayloadTypes: array of TWasmValType): UInt32;

    // Functions
    function AddFunction(ATypeIdx: UInt32): TWasmCodeBuilder;
    function AddSimpleFunction(const AParams, AResults: array of TWasmValType): TWasmCodeBuilder;

    // Tags
    function AddTag(ATypeIdx: UInt32): UInt32;
    function AddSimpleTag(const APayloadTypes: array of TWasmValType): UInt32;
    procedure ExportTag(const AName: string; ATagIdx: UInt32);

    // Memory
    procedure SetMemory(AMin: UInt32; AMax: UInt32 = 0; AHasMax: Boolean = False);
    function AddDataSegment(AOffset: UInt32; const AData: TBytes): UInt32;
    function AddStringData(AOffset: UInt32; const AStr: string): UInt32;

    // Exports
    procedure ExportFunction(const AName: string; AFuncIdx: UInt32);
    procedure ExportMemory(const AName: string; AMemIdx: UInt32 = 0);

    // Custom sections
    procedure AddCustomSection(const AName: string; const AData: TBytes);

    function ImportFuncCount: UInt32;
    function TotalFuncCount: UInt32;

    // Output
    procedure SaveToStream(AStream: TStream);
    procedure SaveToFile(const AFileName: string);
    function SaveToBytes: TBytes;
  end;

function EncodeLEB128U(AValue: UInt64): TBytes;
function EncodeLEB128S(AValue: Int64): TBytes;
function EncodeString(const AStr: string): TBytes;
function EncodeVector(const AItems: array of TBytes): TBytes;

function WasmField(AValType: Byte; AMutable: Boolean = True): TWasmFieldType;
function WasmPackedField(APackedType: Byte; AMutable: Boolean = True): TWasmFieldType;
function WasmRefField(AHeapType: Byte; ANullable: Boolean = True; AMutable: Boolean = True): TWasmFieldType;

function CatchTag(ATagIdx, ALabelIdx: UInt32): TWasmCatchClause;
function CatchTagRef(ATagIdx, ALabelIdx: UInt32): TWasmCatchClause;
function CatchAllClause(ALabelIdx: UInt32): TWasmCatchClause;
function CatchAllRefClause(ALabelIdx: UInt32): TWasmCatchClause;

implementation

const
  // WASM opcodes — kept in implementation to avoid name collisions with Souffle.Bytecode

  // Control
  OP_UNREACHABLE     = $00;
  OP_NOP             = $01;
  OP_BLOCK           = $02;
  OP_LOOP            = $03;
  OP_IF              = $04;
  OP_ELSE            = $05;
  OP_END             = $0B;
  OP_BR              = $0C;
  OP_BR_IF           = $0D;
  OP_BR_TABLE        = $0E;
  OP_RETURN          = $0F;
  OP_CALL            = $10;
  OP_CALL_INDIRECT   = $11;

  // Parametric
  OP_DROP            = $1A;
  OP_SELECT          = $1B;

  // Variable
  OP_LOCAL_GET       = $20;
  OP_LOCAL_SET       = $21;
  OP_LOCAL_TEE       = $22;
  OP_GLOBAL_GET      = $23;
  OP_GLOBAL_SET      = $24;

  // Memory
  OP_I32_LOAD        = $28;
  OP_I64_LOAD        = $29;
  OP_F32_LOAD        = $2A;
  OP_F64_LOAD        = $2B;
  OP_I32_STORE       = $36;
  OP_I64_STORE       = $37;
  OP_F32_STORE       = $38;
  OP_F64_STORE       = $39;
  OP_MEMORY_SIZE     = $3F;
  OP_MEMORY_GROW     = $40;

  // Numeric i32
  OP_I32_CONST       = $41;
  OP_I64_CONST       = $42;
  OP_F32_CONST       = $43;
  OP_F64_CONST       = $44;

  OP_I32_EQZ         = $45;
  OP_I32_EQ          = $46;
  OP_I32_NE          = $47;
  OP_I32_LT_S        = $48;
  OP_I32_LT_U        = $49;
  OP_I32_GT_S        = $4A;
  OP_I32_GT_U        = $4B;
  OP_I32_LE_S        = $4C;
  OP_I32_LE_U        = $4D;
  OP_I32_GE_S        = $4E;
  OP_I32_GE_U        = $4F;

  OP_I32_ADD         = $6A;
  OP_I32_SUB         = $6B;
  OP_I32_MUL         = $6C;
  OP_I32_DIV_S       = $6D;
  OP_I32_DIV_U       = $6E;
  OP_I32_REM_S       = $6F;
  OP_I32_REM_U       = $70;
  OP_I32_AND         = $71;
  OP_I32_OR          = $72;
  OP_I32_XOR         = $73;
  OP_I32_SHL         = $74;
  OP_I32_SHR_S       = $75;
  OP_I32_SHR_U       = $76;

  // Numeric i64
  OP_I64_ADD         = $7C;
  OP_I64_SUB         = $7D;
  OP_I64_MUL         = $7E;

  // Numeric f64
  OP_F64_EQ          = $61;
  OP_F64_NE          = $62;
  OP_F64_LT          = $63;
  OP_F64_GT          = $64;
  OP_F64_LE          = $65;
  OP_F64_GE          = $66;

  OP_F64_ADD         = $A0;
  OP_F64_SUB         = $A1;
  OP_F64_MUL         = $A2;
  OP_F64_DIV         = $A3;

  // Conversions
  OP_I32_WRAP_I64      = $A7;
  OP_I32_TRUNC_F64_S   = $AA;
  OP_I64_EXTEND_I32_S  = $AC;
  OP_I64_EXTEND_I32_U  = $AD;
  OP_F64_CONVERT_I32_S = $B7;
  OP_F64_CONVERT_I64_S = $B9;

  // Exception Handling
  OP_TRY             = $06;
  OP_CATCH           = $07;
  OP_THROW           = $08;
  OP_RETHROW         = $09;
  OP_THROW_REF       = $0A;
  OP_TRY_TABLE       = $1F;
  OP_DELEGATE        = $18;
  OP_CATCH_ALL       = $19;

  // Tail Calls
  OP_RETURN_CALL          = $12;
  OP_RETURN_CALL_INDIRECT = $13;
  OP_CALL_REF             = $14;
  OP_RETURN_CALL_REF      = $15;

  // Reference Types
  OP_REF_NULL        = $D0;
  OP_REF_IS_NULL     = $D1;
  OP_REF_FUNC        = $D2;
  OP_REF_EQ          = $D3;
  OP_REF_AS_NON_NULL = $D4;
  OP_BR_ON_NULL      = $D5;
  OP_BR_ON_NON_NULL  = $D6;

  // GC (0xFB prefix)
  GC_PREFIX                = $FB;

  GC_STRUCT_NEW            = 0;
  GC_STRUCT_NEW_DEFAULT    = 1;
  GC_STRUCT_GET            = 2;
  GC_STRUCT_GET_S          = 3;
  GC_STRUCT_GET_U          = 4;
  GC_STRUCT_SET            = 5;

  GC_ARRAY_NEW             = 6;
  GC_ARRAY_NEW_DEFAULT     = 7;
  GC_ARRAY_NEW_FIXED       = 8;
  GC_ARRAY_NEW_DATA        = 9;
  GC_ARRAY_NEW_ELEM        = 10;
  GC_ARRAY_GET             = 11;
  GC_ARRAY_GET_S           = 12;
  GC_ARRAY_GET_U           = 13;
  GC_ARRAY_SET             = 14;
  GC_ARRAY_LEN             = 15;
  GC_ARRAY_FILL            = 16;
  GC_ARRAY_COPY            = 17;
  GC_ARRAY_INIT_DATA       = 18;
  GC_ARRAY_INIT_ELEM       = 19;

  GC_REF_TEST              = 20;
  GC_REF_TEST_NULL         = 21;
  GC_REF_CAST              = 22;
  GC_REF_CAST_NULL         = 23;

  GC_BR_ON_CAST            = 24;
  GC_BR_ON_CAST_FAIL       = 25;

  GC_ANY_CONVERT_EXTERN    = 26;
  GC_EXTERN_CONVERT_ANY    = 27;

  GC_REF_I31               = 28;
  GC_I31_GET_S             = 29;
  GC_I31_GET_U             = 30;

{ Utility functions }

function EncodeLEB128U(AValue: UInt64): TBytes;
var
  Buf: array[0..9] of Byte;
  Len: Integer;
  B: Byte;
begin
  Len := 0;
  repeat
    B := Byte(AValue and $7F);
    AValue := AValue shr 7;
    if AValue <> 0 then
      B := B or $80;
    Buf[Len] := B;
    Inc(Len);
  until AValue = 0;
  SetLength(Result, Len);
  Move(Buf[0], Result[0], Len);
end;

function EncodeLEB128S(AValue: Int64): TBytes;
var
  Buf: array[0..9] of Byte;
  Len: Integer;
  B: Byte;
  More: Boolean;
begin
  Len := 0;
  More := True;
  while More do
  begin
    B := Byte(AValue and $7F);
    AValue := SarInt64(AValue, 7);
    if ((AValue = 0) and ((B and $40) = 0)) or
       ((AValue = -1) and ((B and $40) <> 0)) then
      More := False
    else
      B := B or $80;
    Buf[Len] := B;
    Inc(Len);
  end;
  SetLength(Result, Len);
  Move(Buf[0], Result[0], Len);
end;

function EncodeString(const AStr: string): TBytes;
var
  LenBytes: TBytes;
  SLen: Integer;
begin
  SLen := Length(AStr);
  LenBytes := EncodeLEB128U(SLen);
  SetLength(Result, Length(LenBytes) + SLen);
  Move(LenBytes[0], Result[0], Length(LenBytes));
  if SLen > 0 then
    Move(AStr[1], Result[Length(LenBytes)], SLen);
end;

function EncodeVector(const AItems: array of TBytes): TBytes;
var
  MS: TMemoryStream;
  CountBytes: TBytes;
  I: Integer;
begin
  MS := TMemoryStream.Create;
  try
    CountBytes := EncodeLEB128U(Length(AItems));
    MS.Write(CountBytes[0], Length(CountBytes));
    for I := 0 to High(AItems) do
      if Length(AItems[I]) > 0 then
        MS.Write(AItems[I][0], Length(AItems[I]));
    SetLength(Result, MS.Size);
    MS.Position := 0;
    MS.Read(Result[0], MS.Size);
  finally
    MS.Free;
  end;
end;

function WasmField(AValType: Byte; AMutable: Boolean): TWasmFieldType;
begin
  Result.StorageType := AValType;
  Result.IsRefType := False;
  Result.Nullable := False;
  Result.Mutable := AMutable;
end;

function WasmPackedField(APackedType: Byte; AMutable: Boolean): TWasmFieldType;
begin
  Result.StorageType := APackedType;
  Result.IsRefType := False;
  Result.Nullable := False;
  Result.Mutable := AMutable;
end;

function WasmRefField(AHeapType: Byte; ANullable: Boolean; AMutable: Boolean): TWasmFieldType;
begin
  Result.StorageType := AHeapType;
  Result.IsRefType := True;
  Result.Nullable := ANullable;
  Result.Mutable := AMutable;
end;

function CatchTag(ATagIdx, ALabelIdx: UInt32): TWasmCatchClause;
begin
  Result.Kind := CATCH_TAG;
  Result.TagIdx := ATagIdx;
  Result.LabelIdx := ALabelIdx;
end;

function CatchTagRef(ATagIdx, ALabelIdx: UInt32): TWasmCatchClause;
begin
  Result.Kind := CATCH_TAG_REF;
  Result.TagIdx := ATagIdx;
  Result.LabelIdx := ALabelIdx;
end;

function CatchAllClause(ALabelIdx: UInt32): TWasmCatchClause;
begin
  Result.Kind := CATCH_ALL_KIND;
  Result.TagIdx := 0;
  Result.LabelIdx := ALabelIdx;
end;

function CatchAllRefClause(ALabelIdx: UInt32): TWasmCatchClause;
begin
  Result.Kind := CATCH_ALL_REF;
  Result.TagIdx := 0;
  Result.LabelIdx := ALabelIdx;
end;

{ TWasmCodeBuilder }

constructor TWasmCodeBuilder.Create(AParamCount: Integer);
begin
  inherited Create;
  FCode := TMemoryStream.Create;
  FParamCount := AParamCount;
end;

destructor TWasmCodeBuilder.Destroy;
begin
  FCode.Free;
  inherited;
end;

procedure TWasmCodeBuilder.AddLocal(AValType: TWasmValType; ACount: UInt32);
var
  Idx: Integer;
begin
  if (Length(FLocals) > 0) and (FLocals[High(FLocals)].ValType = AValType) then
    Inc(FLocals[High(FLocals)].Count, ACount)
  else
  begin
    Idx := Length(FLocals);
    SetLength(FLocals, Idx + 1);
    FLocals[Idx].Count := ACount;
    FLocals[Idx].ValType := AValType;
  end;
end;

procedure TWasmCodeBuilder.EmitByte(AB: Byte);
begin
  FCode.Write(AB, 1);
end;

procedure TWasmCodeBuilder.EmitBytes(const ABytes: array of Byte);
var
  I: Integer;
begin
  for I := 0 to High(ABytes) do
    EmitByte(ABytes[I]);
end;

procedure TWasmCodeBuilder.EmitLEB128U(AValue: UInt64);
var
  Encoded: TBytes;
begin
  Encoded := EncodeLEB128U(AValue);
  FCode.Write(Encoded[0], Length(Encoded));
end;

procedure TWasmCodeBuilder.EmitLEB128S(AValue: Int64);
var
  Encoded: TBytes;
begin
  Encoded := EncodeLEB128S(AValue);
  FCode.Write(Encoded[0], Length(Encoded));
end;

// Control flow

procedure TWasmCodeBuilder.EmitBlock(ABlockType: Byte);
begin
  EmitByte(OP_BLOCK);
  EmitByte(ABlockType);
end;

procedure TWasmCodeBuilder.EmitLoop(ABlockType: Byte);
begin
  EmitByte(OP_LOOP);
  EmitByte(ABlockType);
end;

procedure TWasmCodeBuilder.EmitIf(ABlockType: Byte);
begin
  EmitByte(OP_IF);
  EmitByte(ABlockType);
end;

procedure TWasmCodeBuilder.EmitElse;
begin
  EmitByte(OP_ELSE);
end;

procedure TWasmCodeBuilder.EmitEnd;
begin
  EmitByte(OP_END);
end;

procedure TWasmCodeBuilder.EmitBr(ALabelIdx: UInt32);
begin
  EmitByte(OP_BR);
  EmitLEB128U(ALabelIdx);
end;

procedure TWasmCodeBuilder.EmitBrIf(ALabelIdx: UInt32);
begin
  EmitByte(OP_BR_IF);
  EmitLEB128U(ALabelIdx);
end;

procedure TWasmCodeBuilder.EmitReturn;
begin
  EmitByte(OP_RETURN);
end;

procedure TWasmCodeBuilder.EmitUnreachable;
begin
  EmitByte(OP_UNREACHABLE);
end;

procedure TWasmCodeBuilder.EmitCall(AFuncIdx: UInt32);
begin
  EmitByte(OP_CALL);
  EmitLEB128U(AFuncIdx);
end;

procedure TWasmCodeBuilder.EmitCallIndirect(ATypeIdx: UInt32; ATableIdx: UInt32);
begin
  EmitByte(OP_CALL_INDIRECT);
  EmitLEB128U(ATypeIdx);
  EmitLEB128U(ATableIdx);
end;

// Variables

procedure TWasmCodeBuilder.EmitLocalGet(AIdx: UInt32);
begin
  EmitByte(OP_LOCAL_GET);
  EmitLEB128U(AIdx);
end;

procedure TWasmCodeBuilder.EmitLocalSet(AIdx: UInt32);
begin
  EmitByte(OP_LOCAL_SET);
  EmitLEB128U(AIdx);
end;

procedure TWasmCodeBuilder.EmitLocalTee(AIdx: UInt32);
begin
  EmitByte(OP_LOCAL_TEE);
  EmitLEB128U(AIdx);
end;

procedure TWasmCodeBuilder.EmitGlobalGet(AIdx: UInt32);
begin
  EmitByte(OP_GLOBAL_GET);
  EmitLEB128U(AIdx);
end;

procedure TWasmCodeBuilder.EmitGlobalSet(AIdx: UInt32);
begin
  EmitByte(OP_GLOBAL_SET);
  EmitLEB128U(AIdx);
end;

// Constants

procedure TWasmCodeBuilder.EmitI32Const(AValue: Int32);
begin
  EmitByte(OP_I32_CONST);
  EmitLEB128S(AValue);
end;

procedure TWasmCodeBuilder.EmitI64Const(AValue: Int64);
begin
  EmitByte(OP_I64_CONST);
  EmitLEB128S(AValue);
end;

procedure TWasmCodeBuilder.EmitF64Const(AValue: Double);
begin
  EmitByte(OP_F64_CONST);
  FCode.Write(AValue, 8);
end;

// i32 Arithmetic

procedure TWasmCodeBuilder.EmitI32Add;  begin EmitByte(OP_I32_ADD); end;
procedure TWasmCodeBuilder.EmitI32Sub;  begin EmitByte(OP_I32_SUB); end;
procedure TWasmCodeBuilder.EmitI32Mul;  begin EmitByte(OP_I32_MUL); end;
procedure TWasmCodeBuilder.EmitI32DivS; begin EmitByte(OP_I32_DIV_S); end;
procedure TWasmCodeBuilder.EmitI32RemS; begin EmitByte(OP_I32_REM_S); end;
procedure TWasmCodeBuilder.EmitI32And;  begin EmitByte(OP_I32_AND); end;
procedure TWasmCodeBuilder.EmitI32Or;   begin EmitByte(OP_I32_OR); end;
procedure TWasmCodeBuilder.EmitI32Xor;  begin EmitByte(OP_I32_XOR); end;
procedure TWasmCodeBuilder.EmitI32Shl;  begin EmitByte(OP_I32_SHL); end;
procedure TWasmCodeBuilder.EmitI32ShrS; begin EmitByte(OP_I32_SHR_S); end;

// i32 Comparison

procedure TWasmCodeBuilder.EmitI32Eqz; begin EmitByte(OP_I32_EQZ); end;
procedure TWasmCodeBuilder.EmitI32Eq;  begin EmitByte(OP_I32_EQ); end;
procedure TWasmCodeBuilder.EmitI32Ne;  begin EmitByte(OP_I32_NE); end;
procedure TWasmCodeBuilder.EmitI32LtS; begin EmitByte(OP_I32_LT_S); end;
procedure TWasmCodeBuilder.EmitI32GtS; begin EmitByte(OP_I32_GT_S); end;
procedure TWasmCodeBuilder.EmitI32LeS; begin EmitByte(OP_I32_LE_S); end;
procedure TWasmCodeBuilder.EmitI32GeS; begin EmitByte(OP_I32_GE_S); end;

// f64 Arithmetic

procedure TWasmCodeBuilder.EmitF64Add; begin EmitByte(OP_F64_ADD); end;
procedure TWasmCodeBuilder.EmitF64Sub; begin EmitByte(OP_F64_SUB); end;
procedure TWasmCodeBuilder.EmitF64Mul; begin EmitByte(OP_F64_MUL); end;
procedure TWasmCodeBuilder.EmitF64Div; begin EmitByte(OP_F64_DIV); end;

// f64 Comparison

procedure TWasmCodeBuilder.EmitF64Eq; begin EmitByte(OP_F64_EQ); end;
procedure TWasmCodeBuilder.EmitF64Ne; begin EmitByte(OP_F64_NE); end;
procedure TWasmCodeBuilder.EmitF64Lt; begin EmitByte(OP_F64_LT); end;
procedure TWasmCodeBuilder.EmitF64Gt; begin EmitByte(OP_F64_GT); end;
procedure TWasmCodeBuilder.EmitF64Le; begin EmitByte(OP_F64_LE); end;
procedure TWasmCodeBuilder.EmitF64Ge; begin EmitByte(OP_F64_GE); end;

// Conversions

procedure TWasmCodeBuilder.EmitI64ExtendI32S;  begin EmitByte(OP_I64_EXTEND_I32_S); end;
procedure TWasmCodeBuilder.EmitF64ConvertI32S; begin EmitByte(OP_F64_CONVERT_I32_S); end;
procedure TWasmCodeBuilder.EmitI32WrapI64;     begin EmitByte(OP_I32_WRAP_I64); end;

// Memory

procedure TWasmCodeBuilder.EmitI32Load(AAlign: UInt32; AOffset: UInt32);
begin
  EmitByte(OP_I32_LOAD);
  EmitLEB128U(AAlign);
  EmitLEB128U(AOffset);
end;

procedure TWasmCodeBuilder.EmitI32Store(AAlign: UInt32; AOffset: UInt32);
begin
  EmitByte(OP_I32_STORE);
  EmitLEB128U(AAlign);
  EmitLEB128U(AOffset);
end;

procedure TWasmCodeBuilder.EmitMemoryGrow;
begin
  EmitByte(OP_MEMORY_GROW);
  EmitByte($00);
end;

// Parametric

procedure TWasmCodeBuilder.EmitDrop;   begin EmitByte(OP_DROP); end;
procedure TWasmCodeBuilder.EmitSelect; begin EmitByte(OP_SELECT); end;

// Exception Handling

procedure TWasmCodeBuilder.EmitTryTable(ABlockType: Byte; const ACatches: array of TWasmCatchClause);
var
  I: Integer;
begin
  EmitByte(OP_TRY_TABLE);
  EmitByte(ABlockType);
  EmitLEB128U(Length(ACatches));
  for I := 0 to High(ACatches) do
  begin
    EmitByte(ACatches[I].Kind);
    if ACatches[I].Kind in [CATCH_TAG, CATCH_TAG_REF] then
      EmitLEB128U(ACatches[I].TagIdx);
    EmitLEB128U(ACatches[I].LabelIdx);
  end;
end;

procedure TWasmCodeBuilder.EmitThrow(ATagIdx: UInt32);
begin
  EmitByte(OP_THROW);
  EmitLEB128U(ATagIdx);
end;

procedure TWasmCodeBuilder.EmitThrowRef;
begin
  EmitByte(OP_THROW_REF);
end;

procedure TWasmCodeBuilder.EmitRethrow(ADepth: UInt32);
begin
  EmitByte(OP_RETHROW);
  EmitLEB128U(ADepth);
end;

procedure TWasmCodeBuilder.EmitTry(ABlockType: Byte);
begin
  EmitByte(OP_TRY);
  EmitByte(ABlockType);
end;

procedure TWasmCodeBuilder.EmitCatch(ATagIdx: UInt32);
begin
  EmitByte(OP_CATCH);
  EmitLEB128U(ATagIdx);
end;

procedure TWasmCodeBuilder.EmitCatchAll;
begin
  EmitByte(OP_CATCH_ALL);
end;

procedure TWasmCodeBuilder.EmitDelegate(ADepth: UInt32);
begin
  EmitByte(OP_DELEGATE);
  EmitLEB128U(ADepth);
end;

// Tail Calls

procedure TWasmCodeBuilder.EmitReturnCall(AFuncIdx: UInt32);
begin
  EmitByte(OP_RETURN_CALL);
  EmitLEB128U(AFuncIdx);
end;

procedure TWasmCodeBuilder.EmitReturnCallIndirect(ATypeIdx: UInt32; ATableIdx: UInt32);
begin
  EmitByte(OP_RETURN_CALL_INDIRECT);
  EmitLEB128U(ATypeIdx);
  EmitLEB128U(ATableIdx);
end;

procedure TWasmCodeBuilder.EmitCallRef(ATypeIdx: UInt32);
begin
  EmitByte(OP_CALL_REF);
  EmitLEB128U(ATypeIdx);
end;

procedure TWasmCodeBuilder.EmitReturnCallRef(ATypeIdx: UInt32);
begin
  EmitByte(OP_RETURN_CALL_REF);
  EmitLEB128U(ATypeIdx);
end;

// Reference Types

procedure TWasmCodeBuilder.EmitRefNull(AHeapType: Byte);
begin
  EmitByte(OP_REF_NULL);
  EmitByte(AHeapType);
end;

procedure TWasmCodeBuilder.EmitRefIsNull;
begin
  EmitByte(OP_REF_IS_NULL);
end;

procedure TWasmCodeBuilder.EmitRefFunc(AFuncIdx: UInt32);
begin
  EmitByte(OP_REF_FUNC);
  EmitLEB128U(AFuncIdx);
end;

procedure TWasmCodeBuilder.EmitRefEq;
begin
  EmitByte(OP_REF_EQ);
end;

procedure TWasmCodeBuilder.EmitRefAsNonNull;
begin
  EmitByte(OP_REF_AS_NON_NULL);
end;

procedure TWasmCodeBuilder.EmitBrOnNull(ALabelIdx: UInt32);
begin
  EmitByte(OP_BR_ON_NULL);
  EmitLEB128U(ALabelIdx);
end;

procedure TWasmCodeBuilder.EmitBrOnNonNull(ALabelIdx: UInt32);
begin
  EmitByte(OP_BR_ON_NON_NULL);
  EmitLEB128U(ALabelIdx);
end;

// GC helpers

procedure TWasmCodeBuilder.EmitGCOp(ASubOp: UInt32);
begin
  EmitByte(GC_PREFIX);
  EmitLEB128U(ASubOp);
end;

procedure TWasmCodeBuilder.EmitGCOpWithType(ASubOp: UInt32; ATypeIdx: UInt32);
begin
  EmitByte(GC_PREFIX);
  EmitLEB128U(ASubOp);
  EmitLEB128U(ATypeIdx);
end;

procedure TWasmCodeBuilder.EmitGCOpWithTypeAndField(ASubOp: UInt32; ATypeIdx, AFieldIdx: UInt32);
begin
  EmitByte(GC_PREFIX);
  EmitLEB128U(ASubOp);
  EmitLEB128U(ATypeIdx);
  EmitLEB128U(AFieldIdx);
end;

// GC: Struct

procedure TWasmCodeBuilder.EmitStructNew(ATypeIdx: UInt32);
begin
  EmitGCOpWithType(GC_STRUCT_NEW, ATypeIdx);
end;

procedure TWasmCodeBuilder.EmitStructNewDefault(ATypeIdx: UInt32);
begin
  EmitGCOpWithType(GC_STRUCT_NEW_DEFAULT, ATypeIdx);
end;

procedure TWasmCodeBuilder.EmitStructGet(ATypeIdx: UInt32; AFieldIdx: UInt32);
begin
  EmitGCOpWithTypeAndField(GC_STRUCT_GET, ATypeIdx, AFieldIdx);
end;

procedure TWasmCodeBuilder.EmitStructGetS(ATypeIdx: UInt32; AFieldIdx: UInt32);
begin
  EmitGCOpWithTypeAndField(GC_STRUCT_GET_S, ATypeIdx, AFieldIdx);
end;

procedure TWasmCodeBuilder.EmitStructGetU(ATypeIdx: UInt32; AFieldIdx: UInt32);
begin
  EmitGCOpWithTypeAndField(GC_STRUCT_GET_U, ATypeIdx, AFieldIdx);
end;

procedure TWasmCodeBuilder.EmitStructSet(ATypeIdx: UInt32; AFieldIdx: UInt32);
begin
  EmitGCOpWithTypeAndField(GC_STRUCT_SET, ATypeIdx, AFieldIdx);
end;

// GC: Array

procedure TWasmCodeBuilder.EmitArrayNew(ATypeIdx: UInt32);
begin
  EmitGCOpWithType(GC_ARRAY_NEW, ATypeIdx);
end;

procedure TWasmCodeBuilder.EmitArrayNewDefault(ATypeIdx: UInt32);
begin
  EmitGCOpWithType(GC_ARRAY_NEW_DEFAULT, ATypeIdx);
end;

procedure TWasmCodeBuilder.EmitArrayNewFixed(ATypeIdx: UInt32; ALength: UInt32);
begin
  EmitByte(GC_PREFIX);
  EmitLEB128U(GC_ARRAY_NEW_FIXED);
  EmitLEB128U(ATypeIdx);
  EmitLEB128U(ALength);
end;

procedure TWasmCodeBuilder.EmitArrayGet(ATypeIdx: UInt32);
begin
  EmitGCOpWithType(GC_ARRAY_GET, ATypeIdx);
end;

procedure TWasmCodeBuilder.EmitArrayGetS(ATypeIdx: UInt32);
begin
  EmitGCOpWithType(GC_ARRAY_GET_S, ATypeIdx);
end;

procedure TWasmCodeBuilder.EmitArrayGetU(ATypeIdx: UInt32);
begin
  EmitGCOpWithType(GC_ARRAY_GET_U, ATypeIdx);
end;

procedure TWasmCodeBuilder.EmitArraySet(ATypeIdx: UInt32);
begin
  EmitGCOpWithType(GC_ARRAY_SET, ATypeIdx);
end;

procedure TWasmCodeBuilder.EmitArrayLen;
begin
  EmitGCOp(GC_ARRAY_LEN);
end;

procedure TWasmCodeBuilder.EmitArrayFill(ATypeIdx: UInt32);
begin
  EmitGCOpWithType(GC_ARRAY_FILL, ATypeIdx);
end;

procedure TWasmCodeBuilder.EmitArrayCopy(ADstTypeIdx, ASrcTypeIdx: UInt32);
begin
  EmitByte(GC_PREFIX);
  EmitLEB128U(GC_ARRAY_COPY);
  EmitLEB128U(ADstTypeIdx);
  EmitLEB128U(ASrcTypeIdx);
end;

// GC: i31

procedure TWasmCodeBuilder.EmitRefI31;  begin EmitGCOp(GC_REF_I31); end;
procedure TWasmCodeBuilder.EmitI31GetS; begin EmitGCOp(GC_I31_GET_S); end;
procedure TWasmCodeBuilder.EmitI31GetU; begin EmitGCOp(GC_I31_GET_U); end;

// GC: Casting

procedure TWasmCodeBuilder.EmitRefTest(AHeapType: Byte);
begin
  EmitByte(GC_PREFIX);
  EmitLEB128U(GC_REF_TEST);
  EmitByte(AHeapType);
end;

procedure TWasmCodeBuilder.EmitRefTestNull(AHeapType: Byte);
begin
  EmitByte(GC_PREFIX);
  EmitLEB128U(GC_REF_TEST_NULL);
  EmitByte(AHeapType);
end;

procedure TWasmCodeBuilder.EmitRefCast(AHeapType: Byte);
begin
  EmitByte(GC_PREFIX);
  EmitLEB128U(GC_REF_CAST);
  EmitByte(AHeapType);
end;

procedure TWasmCodeBuilder.EmitRefCastNull(AHeapType: Byte);
begin
  EmitByte(GC_PREFIX);
  EmitLEB128U(GC_REF_CAST_NULL);
  EmitByte(AHeapType);
end;

// GC: Extern Conversion

procedure TWasmCodeBuilder.EmitAnyConvertExtern; begin EmitGCOp(GC_ANY_CONVERT_EXTERN); end;
procedure TWasmCodeBuilder.EmitExternConvertAny; begin EmitGCOp(GC_EXTERN_CONVERT_ANY); end;

// Encode function body

function TWasmCodeBuilder.Encode: TBytes;
var
  Body, LenBytes: TBytes;
  MS: TMemoryStream;
  I: Integer;
  CountBytes, EntryCount: TBytes;
begin
  MS := TMemoryStream.Create;
  try
    CountBytes := EncodeLEB128U(Length(FLocals));
    MS.Write(CountBytes[0], Length(CountBytes));
    for I := 0 to High(FLocals) do
    begin
      EntryCount := EncodeLEB128U(FLocals[I].Count);
      MS.Write(EntryCount[0], Length(EntryCount));
      MS.Write(FLocals[I].ValType, 1);
    end;

    FCode.Position := 0;
    MS.CopyFrom(FCode, FCode.Size);

    SetLength(Body, MS.Size);
    MS.Position := 0;
    MS.Read(Body[0], MS.Size);

    LenBytes := EncodeLEB128U(Length(Body));
    SetLength(Result, Length(LenBytes) + Length(Body));
    Move(LenBytes[0], Result[0], Length(LenBytes));
    Move(Body[0], Result[Length(LenBytes)], Length(Body));
  finally
    MS.Free;
  end;
end;

{ TWasmModule }

constructor TWasmModule.Create;
begin
  inherited;
  FHasMemory := False;
end;

destructor TWasmModule.Destroy;
var
  I: Integer;
begin
  for I := 0 to High(FCodeBuilders) do
    FCodeBuilders[I].Free;
  inherited;
end;

function TWasmModule.AddFuncType(const AParams, AResults: array of TWasmValType): UInt32;
var
  Idx, I: Integer;
begin
  Idx := Length(FTypes);
  SetLength(FTypes, Idx + 1);
  FTypes[Idx].Kind := ctkFunc;
  SetLength(FTypes[Idx].Params, Length(AParams));
  for I := 0 to High(AParams) do
    FTypes[Idx].Params[I] := AParams[I];
  SetLength(FTypes[Idx].Results, Length(AResults));
  for I := 0 to High(AResults) do
    FTypes[Idx].Results[I] := AResults[I];
  Result := Idx;
end;

function TWasmModule.FindOrAddFuncType(const AParams, AResults: array of TWasmValType): UInt32;
var
  I, J: Integer;
  Match: Boolean;
begin
  for I := 0 to High(FTypes) do
  begin
    if FTypes[I].Kind <> ctkFunc then
      Continue;
    if (Length(FTypes[I].Params) <> Length(AParams)) or
       (Length(FTypes[I].Results) <> Length(AResults)) then
      Continue;
    Match := True;
    for J := 0 to High(AParams) do
      if FTypes[I].Params[J] <> AParams[J] then begin Match := False; Break; end;
    if Match then
      for J := 0 to High(AResults) do
        if FTypes[I].Results[J] <> AResults[J] then begin Match := False; Break; end;
    if Match then
      Exit(I);
  end;
  Result := AddFuncType(AParams, AResults);
end;

function TWasmModule.AddStructType(const AFields: array of TWasmFieldType): UInt32;
var
  Idx, I: Integer;
begin
  Idx := Length(FTypes);
  SetLength(FTypes, Idx + 1);
  FTypes[Idx].Kind := ctkStruct;
  SetLength(FTypes[Idx].Fields, Length(AFields));
  for I := 0 to High(AFields) do
    FTypes[Idx].Fields[I] := AFields[I];
  Result := Idx;
end;

function TWasmModule.AddArrayType(const AElementType: TWasmFieldType): UInt32;
var
  Idx: Integer;
begin
  Idx := Length(FTypes);
  SetLength(FTypes, Idx + 1);
  FTypes[Idx].Kind := ctkArray;
  FTypes[Idx].ElementType := AElementType;
  Result := Idx;
end;

function TWasmModule.ImportFunction(const AModuleName, AFuncName: string;
  const AParams, AResults: array of TWasmValType): UInt32;
var
  Idx: Integer;
  TypeIdx: UInt32;
begin
  TypeIdx := FindOrAddFuncType(AParams, AResults);
  Idx := Length(FImports);
  SetLength(FImports, Idx + 1);
  FImports[Idx].Module := AModuleName;
  FImports[Idx].Name := AFuncName;
  FImports[Idx].Kind := IK_FUNCTION;
  FImports[Idx].TypeIdx := TypeIdx;
  Result := Idx;
end;

procedure TWasmModule.ImportMemory(const AModuleName, AMemName: string;
  AMin: UInt32; AMax: UInt32; AHasMax: Boolean);
var
  Idx: Integer;
begin
  Idx := Length(FImports);
  SetLength(FImports, Idx + 1);
  FImports[Idx].Module := AModuleName;
  FImports[Idx].Name := AMemName;
  FImports[Idx].Kind := IK_MEMORY;
  FImports[Idx].Min := AMin;
  FImports[Idx].Max := AMax;
  FImports[Idx].HasMax := AHasMax;
end;

function TWasmModule.ImportTag(const AModuleName, ATagName: string;
  const APayloadTypes: array of TWasmValType): UInt32;
var
  Idx, I: Integer;
  TypeIdx, TagIdx: UInt32;
begin
  TypeIdx := FindOrAddFuncType(APayloadTypes, []);
  TagIdx := 0;
  for I := 0 to High(FImports) do
    if FImports[I].Kind = IK_TAG then
      Inc(TagIdx);
  Idx := Length(FImports);
  SetLength(FImports, Idx + 1);
  FImports[Idx].Module := AModuleName;
  FImports[Idx].Name := ATagName;
  FImports[Idx].Kind := IK_TAG;
  FImports[Idx].TypeIdx := TypeIdx;
  Result := TagIdx;
end;

function TWasmModule.AddFunction(ATypeIdx: UInt32): TWasmCodeBuilder;
var
  FIdx, CIdx: Integer;
  ParamCount: Integer;
begin
  FIdx := Length(FFuncTypeIndices);
  SetLength(FFuncTypeIndices, FIdx + 1);
  FFuncTypeIndices[FIdx] := ATypeIdx;

  ParamCount := Length(FTypes[ATypeIdx].Params);
  CIdx := Length(FCodeBuilders);
  SetLength(FCodeBuilders, CIdx + 1);
  FCodeBuilders[CIdx] := TWasmCodeBuilder.Create(ParamCount);
  Result := FCodeBuilders[CIdx];
end;

function TWasmModule.AddSimpleFunction(const AParams, AResults: array of TWasmValType): TWasmCodeBuilder;
var
  TypeIdx: UInt32;
begin
  TypeIdx := FindOrAddFuncType(AParams, AResults);
  Result := AddFunction(TypeIdx);
end;

function TWasmModule.AddTag(ATypeIdx: UInt32): UInt32;
var
  Idx: Integer;
begin
  Idx := Length(FTags);
  SetLength(FTags, Idx + 1);
  FTags[Idx].TypeIdx := ATypeIdx;
  Result := Idx;
end;

function TWasmModule.AddSimpleTag(const APayloadTypes: array of TWasmValType): UInt32;
var
  TypeIdx: UInt32;
begin
  TypeIdx := FindOrAddFuncType(APayloadTypes, []);
  Result := AddTag(TypeIdx);
end;

procedure TWasmModule.ExportTag(const AName: string; ATagIdx: UInt32);
var
  Idx: Integer;
begin
  Idx := Length(FExports);
  SetLength(FExports, Idx + 1);
  FExports[Idx].Name := AName;
  FExports[Idx].Kind := EK_TAG;
  FExports[Idx].Index := ATagIdx;
end;

procedure TWasmModule.SetMemory(AMin: UInt32; AMax: UInt32; AHasMax: Boolean);
begin
  FHasMemory := True;
  FMemoryMin := AMin;
  FMemoryMax := AMax;
  FMemoryHasMax := AHasMax;
end;

function TWasmModule.AddDataSegment(AOffset: UInt32; const AData: TBytes): UInt32;
var
  Idx: Integer;
begin
  Idx := Length(FDataSegments);
  SetLength(FDataSegments, Idx + 1);
  FDataSegments[Idx].Offset := AOffset;
  FDataSegments[Idx].Data := Copy(AData, 0, Length(AData));
  Result := Idx;
end;

function TWasmModule.AddStringData(AOffset: UInt32; const AStr: string): UInt32;
var
  Data: TBytes;
  I: Integer;
begin
  SetLength(Data, Length(AStr) + 1);
  for I := 1 to Length(AStr) do
    Data[I - 1] := Byte(AStr[I]);
  Data[Length(AStr)] := 0;
  Result := AddDataSegment(AOffset, Data);
end;

procedure TWasmModule.ExportFunction(const AName: string; AFuncIdx: UInt32);
var
  Idx: Integer;
begin
  Idx := Length(FExports);
  SetLength(FExports, Idx + 1);
  FExports[Idx].Name := AName;
  FExports[Idx].Kind := EK_FUNCTION;
  FExports[Idx].Index := AFuncIdx;
end;

procedure TWasmModule.ExportMemory(const AName: string; AMemIdx: UInt32);
var
  Idx: Integer;
begin
  Idx := Length(FExports);
  SetLength(FExports, Idx + 1);
  FExports[Idx].Name := AName;
  FExports[Idx].Kind := EK_MEMORY;
  FExports[Idx].Index := AMemIdx;
end;

procedure TWasmModule.AddCustomSection(const AName: string; const AData: TBytes);
var
  Idx: Integer;
begin
  Idx := Length(FCustomSections);
  SetLength(FCustomSections, Idx + 1);
  FCustomSections[Idx].Name := AName;
  FCustomSections[Idx].Data := Copy(AData, 0, Length(AData));
end;

function TWasmModule.ImportFuncCount: UInt32;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to High(FImports) do
    if FImports[I].Kind = IK_FUNCTION then
      Inc(Result);
end;

function TWasmModule.TotalFuncCount: UInt32;
begin
  Result := ImportFuncCount + UInt32(Length(FFuncTypeIndices));
end;

procedure TWasmModule.WriteSection(AStream: TStream; ASectionID: Byte; const AContent: TBytes);
var
  LenBytes: TBytes;
begin
  AStream.Write(ASectionID, 1);
  LenBytes := EncodeLEB128U(Length(AContent));
  AStream.Write(LenBytes[0], Length(LenBytes));
  if Length(AContent) > 0 then
    AStream.Write(AContent[0], Length(AContent));
end;

procedure TWasmModule.WriteTypeSection(AStream: TStream);

  procedure EncodeFieldType(AMS: TMemoryStream; const AField: TWasmFieldType);
  var
    FieldBytes: TBytes;
    MutByte: Byte;
  begin
    if AField.IsRefType then
    begin
      if AField.Nullable then
        AMS.Write(AField.StorageType, 1)
      else
      begin
        MutByte := $64; // ref non-null prefix
        AMS.Write(MutByte, 1);
        AMS.Write(AField.StorageType, 1);
      end;
    end
    else
      AMS.Write(AField.StorageType, 1);

    if AField.Mutable then
      MutByte := MUT_VAR
    else
      MutByte := MUT_CONST;
    AMS.Write(MutByte, 1);
  end;

var
  MS: TMemoryStream;
  Content: TBytes;
  CountBytes, ParamLen, ResultLen, FieldCount: TBytes;
  I, J: Integer;
  Marker: Byte;
begin
  if Length(FTypes) = 0 then Exit;
  MS := TMemoryStream.Create;
  try
    CountBytes := EncodeLEB128U(Length(FTypes));
    MS.Write(CountBytes[0], Length(CountBytes));
    for I := 0 to High(FTypes) do
    begin
      case FTypes[I].Kind of
        ctkFunc:
        begin
          Marker := CT_FUNC;
          MS.Write(Marker, 1);
          ParamLen := EncodeLEB128U(Length(FTypes[I].Params));
          MS.Write(ParamLen[0], Length(ParamLen));
          for J := 0 to High(FTypes[I].Params) do
            MS.Write(FTypes[I].Params[J], 1);
          ResultLen := EncodeLEB128U(Length(FTypes[I].Results));
          MS.Write(ResultLen[0], Length(ResultLen));
          for J := 0 to High(FTypes[I].Results) do
            MS.Write(FTypes[I].Results[J], 1);
        end;
        ctkStruct:
        begin
          Marker := CT_STRUCT;
          MS.Write(Marker, 1);
          FieldCount := EncodeLEB128U(Length(FTypes[I].Fields));
          MS.Write(FieldCount[0], Length(FieldCount));
          for J := 0 to High(FTypes[I].Fields) do
            EncodeFieldType(MS, FTypes[I].Fields[J]);
        end;
        ctkArray:
        begin
          Marker := CT_ARRAY;
          MS.Write(Marker, 1);
          EncodeFieldType(MS, FTypes[I].ElementType);
        end;
      end;
    end;
    SetLength(Content, MS.Size);
    MS.Position := 0;
    MS.Read(Content[0], MS.Size);
    WriteSection(AStream, SEC_TYPE, Content);
  finally
    MS.Free;
  end;
end;

procedure TWasmModule.WriteImportSection(AStream: TStream);
var
  MS: TMemoryStream;
  Content: TBytes;
  CountBytes, ModBytes, NameBytes, IdxBytes: TBytes;
  I: Integer;
  Flags, Attribute: Byte;
begin
  if Length(FImports) = 0 then Exit;
  MS := TMemoryStream.Create;
  try
    CountBytes := EncodeLEB128U(Length(FImports));
    MS.Write(CountBytes[0], Length(CountBytes));
    for I := 0 to High(FImports) do
    begin
      ModBytes := EncodeString(FImports[I].Module);
      MS.Write(ModBytes[0], Length(ModBytes));
      NameBytes := EncodeString(FImports[I].Name);
      MS.Write(NameBytes[0], Length(NameBytes));
      MS.Write(FImports[I].Kind, 1);
      case FImports[I].Kind of
        IK_FUNCTION:
        begin
          IdxBytes := EncodeLEB128U(FImports[I].TypeIdx);
          MS.Write(IdxBytes[0], Length(IdxBytes));
        end;
        IK_MEMORY:
        begin
          if FImports[I].HasMax then Flags := $01 else Flags := $00;
          MS.Write(Flags, 1);
          IdxBytes := EncodeLEB128U(FImports[I].Min);
          MS.Write(IdxBytes[0], Length(IdxBytes));
          if FImports[I].HasMax then
          begin
            IdxBytes := EncodeLEB128U(FImports[I].Max);
            MS.Write(IdxBytes[0], Length(IdxBytes));
          end;
        end;
        IK_TAG:
        begin
          Attribute := $00;
          MS.Write(Attribute, 1);
          IdxBytes := EncodeLEB128U(FImports[I].TypeIdx);
          MS.Write(IdxBytes[0], Length(IdxBytes));
        end;
      end;
    end;
    SetLength(Content, MS.Size);
    MS.Position := 0;
    MS.Read(Content[0], MS.Size);
    WriteSection(AStream, SEC_IMPORT, Content);
  finally
    MS.Free;
  end;
end;

procedure TWasmModule.WriteFunctionSection(AStream: TStream);
var
  MS: TMemoryStream;
  Content, CountBytes, IdxBytes: TBytes;
  I: Integer;
begin
  if Length(FFuncTypeIndices) = 0 then Exit;
  MS := TMemoryStream.Create;
  try
    CountBytes := EncodeLEB128U(Length(FFuncTypeIndices));
    MS.Write(CountBytes[0], Length(CountBytes));
    for I := 0 to High(FFuncTypeIndices) do
    begin
      IdxBytes := EncodeLEB128U(FFuncTypeIndices[I]);
      MS.Write(IdxBytes[0], Length(IdxBytes));
    end;
    SetLength(Content, MS.Size);
    MS.Position := 0;
    MS.Read(Content[0], MS.Size);
    WriteSection(AStream, SEC_FUNCTION, Content);
  finally
    MS.Free;
  end;
end;

procedure TWasmModule.WriteMemorySection(AStream: TStream);
var
  MS: TMemoryStream;
  Content, CountBytes, ValBytes: TBytes;
  Flags: Byte;
begin
  if not FHasMemory then Exit;
  MS := TMemoryStream.Create;
  try
    CountBytes := EncodeLEB128U(1);
    MS.Write(CountBytes[0], Length(CountBytes));
    if FMemoryHasMax then Flags := $01 else Flags := $00;
    MS.Write(Flags, 1);
    ValBytes := EncodeLEB128U(FMemoryMin);
    MS.Write(ValBytes[0], Length(ValBytes));
    if FMemoryHasMax then
    begin
      ValBytes := EncodeLEB128U(FMemoryMax);
      MS.Write(ValBytes[0], Length(ValBytes));
    end;
    SetLength(Content, MS.Size);
    MS.Position := 0;
    MS.Read(Content[0], MS.Size);
    WriteSection(AStream, SEC_MEMORY, Content);
  finally
    MS.Free;
  end;
end;

procedure TWasmModule.WriteTagSection(AStream: TStream);
var
  MS: TMemoryStream;
  Content, CountBytes, IdxBytes: TBytes;
  I: Integer;
  Attribute: Byte;
begin
  if Length(FTags) = 0 then Exit;
  MS := TMemoryStream.Create;
  try
    CountBytes := EncodeLEB128U(Length(FTags));
    MS.Write(CountBytes[0], Length(CountBytes));
    for I := 0 to High(FTags) do
    begin
      Attribute := $00;
      MS.Write(Attribute, 1);
      IdxBytes := EncodeLEB128U(FTags[I].TypeIdx);
      MS.Write(IdxBytes[0], Length(IdxBytes));
    end;
    SetLength(Content, MS.Size);
    MS.Position := 0;
    MS.Read(Content[0], MS.Size);
    WriteSection(AStream, SEC_TAG, Content);
  finally
    MS.Free;
  end;
end;

procedure TWasmModule.WriteExportSection(AStream: TStream);
var
  MS: TMemoryStream;
  Content, CountBytes, NameBytes, IdxBytes: TBytes;
  I: Integer;
begin
  if Length(FExports) = 0 then Exit;
  MS := TMemoryStream.Create;
  try
    CountBytes := EncodeLEB128U(Length(FExports));
    MS.Write(CountBytes[0], Length(CountBytes));
    for I := 0 to High(FExports) do
    begin
      NameBytes := EncodeString(FExports[I].Name);
      MS.Write(NameBytes[0], Length(NameBytes));
      MS.Write(FExports[I].Kind, 1);
      IdxBytes := EncodeLEB128U(FExports[I].Index);
      MS.Write(IdxBytes[0], Length(IdxBytes));
    end;
    SetLength(Content, MS.Size);
    MS.Position := 0;
    MS.Read(Content[0], MS.Size);
    WriteSection(AStream, SEC_EXPORT, Content);
  finally
    MS.Free;
  end;
end;

procedure TWasmModule.WriteCodeSection(AStream: TStream);
var
  MS: TMemoryStream;
  Content, CountBytes: TBytes;
  Bodies: array of TBytes;
  I: Integer;
begin
  if Length(FCodeBuilders) = 0 then Exit;
  SetLength(Bodies, Length(FCodeBuilders));
  for I := 0 to High(FCodeBuilders) do
    Bodies[I] := FCodeBuilders[I].Encode;

  MS := TMemoryStream.Create;
  try
    CountBytes := EncodeLEB128U(Length(Bodies));
    MS.Write(CountBytes[0], Length(CountBytes));
    for I := 0 to High(Bodies) do
      MS.Write(Bodies[I][0], Length(Bodies[I]));
    SetLength(Content, MS.Size);
    MS.Position := 0;
    MS.Read(Content[0], MS.Size);
    WriteSection(AStream, SEC_CODE, Content);
  finally
    MS.Free;
  end;
end;

procedure TWasmModule.WriteDataSection(AStream: TStream);
var
  MS: TMemoryStream;
  Content, CountBytes, OffsetBytes, LenBytes: TBytes;
  I: Integer;
  Mode, TmpByte: Byte;
begin
  if Length(FDataSegments) = 0 then Exit;
  MS := TMemoryStream.Create;
  try
    CountBytes := EncodeLEB128U(Length(FDataSegments));
    MS.Write(CountBytes[0], Length(CountBytes));
    for I := 0 to High(FDataSegments) do
    begin
      Mode := $00;
      MS.Write(Mode, 1);
      TmpByte := OP_I32_CONST;
      MS.Write(TmpByte, 1);
      OffsetBytes := EncodeLEB128S(FDataSegments[I].Offset);
      MS.Write(OffsetBytes[0], Length(OffsetBytes));
      TmpByte := OP_END;
      MS.Write(TmpByte, 1);
      LenBytes := EncodeLEB128U(Length(FDataSegments[I].Data));
      MS.Write(LenBytes[0], Length(LenBytes));
      if Length(FDataSegments[I].Data) > 0 then
        MS.Write(FDataSegments[I].Data[0], Length(FDataSegments[I].Data));
    end;
    SetLength(Content, MS.Size);
    MS.Position := 0;
    MS.Read(Content[0], MS.Size);
    WriteSection(AStream, SEC_DATA, Content);
  finally
    MS.Free;
  end;
end;

procedure TWasmModule.WriteCustomSections(AStream: TStream);
var
  MS: TMemoryStream;
  Content: TBytes;
  NameBytes: TBytes;
  I: Integer;
begin
  for I := 0 to High(FCustomSections) do
  begin
    MS := TMemoryStream.Create;
    try
      NameBytes := EncodeString(FCustomSections[I].Name);
      MS.Write(NameBytes[0], Length(NameBytes));
      if Length(FCustomSections[I].Data) > 0 then
        MS.Write(FCustomSections[I].Data[0], Length(FCustomSections[I].Data));
      SetLength(Content, MS.Size);
      MS.Position := 0;
      MS.Read(Content[0], MS.Size);
      WriteSection(AStream, 0, Content);
    finally
      MS.Free;
    end;
  end;
end;

procedure TWasmModule.SaveToStream(AStream: TStream);
var
  Magic: UInt32;
  Version: UInt32;
begin
  Magic := WASM_MAGIC;
  Version := WASM_VERSION;
  AStream.Write(Magic, 4);
  AStream.Write(Version, 4);

  WriteTypeSection(AStream);
  WriteImportSection(AStream);
  WriteFunctionSection(AStream);
  WriteMemorySection(AStream);
  WriteTagSection(AStream);
  WriteExportSection(AStream);
  WriteCodeSection(AStream);
  WriteDataSection(AStream);
  WriteCustomSections(AStream);
end;

procedure TWasmModule.SaveToFile(const AFileName: string);
var
  FS: TFileStream;
begin
  FS := TFileStream.Create(AFileName, fmCreate);
  try
    SaveToStream(FS);
  finally
    FS.Free;
  end;
end;

function TWasmModule.SaveToBytes: TBytes;
var
  MS: TMemoryStream;
begin
  MS := TMemoryStream.Create;
  try
    SaveToStream(MS);
    SetLength(Result, MS.Size);
    MS.Position := 0;
    if MS.Size > 0 then
      MS.Read(Result[0], MS.Size);
  finally
    MS.Free;
  end;
end;

end.
