unit Souffle.Wasm.Types;

{$I Souffle.inc}

interface

uses
  Souffle.Bytecode.Chunk,
  Souffle.Wasm.Emitter;

type
  { TSouffleWasmTypeLayout - registers the WASM GC type definitions
    that represent Souffle's value system into a TWasmModule.
    After calling RegisterTypes, the type indices are available
    for use by the translator. }

  TSouffleWasmTypeLayout = class
  private
    FBoxedI64Idx: UInt32;
    FBoxedF64Idx: UInt32;
    FStringIdx: UInt32;
    FUpvalueIdx: UInt32;
    FUpvalueArrayIdx: UInt32;
    FClosureIdx: UInt32;
    FArrayIdx: UInt32;
    FExceptionTagTypeIdx: UInt32;

  public
    procedure RegisterTypes(const AModule: TWasmModule);

    property BoxedI64Idx: UInt32 read FBoxedI64Idx;
    property BoxedF64Idx: UInt32 read FBoxedF64Idx;
    property StringIdx: UInt32 read FStringIdx;
    property UpvalueIdx: UInt32 read FUpvalueIdx;
    property UpvalueArrayIdx: UInt32 read FUpvalueArrayIdx;
    property ClosureIdx: UInt32 read FClosureIdx;
    property ArrayIdx: UInt32 read FArrayIdx;
    property ExceptionTagTypeIdx: UInt32 read FExceptionTagTypeIdx;
  end;

function SouffleLocalTypeToWasmValType(const ALocalType: TSouffleLocalType): TWasmValType;

implementation

function SouffleLocalTypeToWasmValType(const ALocalType: TSouffleLocalType): TWasmValType;
begin
  case ALocalType of
    sltInteger:  Result := WT_I64;
    sltFloat:    Result := WT_F64;
    sltBoolean:  Result := WT_I32;
    sltString:   Result := WT_EXTERNREF;
    sltReference: Result := WT_EXTERNREF;
  else
    Result := WT_EXTERNREF;
  end;
end;

{ TSouffleWasmTypeLayout }

procedure TSouffleWasmTypeLayout.RegisterTypes(const AModule: TWasmModule);
begin
  // $boxed_i64 = (struct (field $value (mut i64)))
  FBoxedI64Idx := AModule.AddStructType([
    WasmField(WT_I64, True)
  ]);

  // $boxed_f64 = (struct (field $value (mut f64)))
  FBoxedF64Idx := AModule.AddStructType([
    WasmField(WT_F64, True)
  ]);

  // $souffle_string = (array (mut i8))
  FStringIdx := AModule.AddArrayType(
    WasmPackedField(PACK_I8, True)
  );

  // $upvalue = (struct (field $value (mut externref)))
  FUpvalueIdx := AModule.AddStructType([
    WasmRefField(HT_EXTERN, True, True)
  ]);

  // $upvalue_array = (array (mut (ref $upvalue)))
  FUpvalueArrayIdx := AModule.AddArrayType(
    WasmRefField(HT_STRUCT, True, True)
  );

  // $closure = (struct (field $func funcref) (field $upvalues (ref $upvalue_array)))
  FClosureIdx := AModule.AddStructType([
    WasmRefField(HT_FUNC, True, False),
    WasmRefField(HT_ARRAY, True, False)
  ]);

  // $souffle_array = (array (mut externref))
  FArrayIdx := AModule.AddArrayType(
    WasmRefField(HT_EXTERN, True, True)
  );

  // Exception tag type: (func (param externref) (result))
  FExceptionTagTypeIdx := AModule.FindOrAddFuncType([WT_EXTERNREF], []);
end;

end.
