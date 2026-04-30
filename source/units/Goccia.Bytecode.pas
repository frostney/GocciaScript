unit Goccia.Bytecode;

{$I Goccia.inc}

interface

const
  // Bytecode format version.  Bumped whenever an opcode's operand
  // encoding or runtime semantics change in a way that makes pre-bump
  // .gbc files unsafe to execute under the new VM.  The binary loader
  // rejects mismatched versions outright, so a producer/consumer
  // version disagreement fails fast at load time rather than silently
  // misinterpreting bytes.
  //
  //   v18 -> v19: main branch (#475) added own `prototype` to
  //               --compat-function functions and generators, which
  //               changed the runtime shape produced by OP_CLOSURE.
  //   v19 -> v20: this branch's IteratorClose work changed
  //               OP_VALIDATE_VALUE / VALIDATE_OP_REQUIRE_ITERABLE
  //               operand C semantics.  Pre-v20, C = 0 meant
  //               "unbounded".  v20 introduces ITERABLE_LIMIT_UNBOUNDED
  //               (255) as the explicit unbounded sentinel and treats
  //               C = 0 as "consume exactly zero elements then close"
  //               (`const [] = iter`).  Loading a pre-v20 file in a
  //               v20 VM would silently switch unbounded patterns to
  //               short-circuit close, so the version bump prevents
  //               that.
  GOCCIA_FORMAT_VERSION = 20;
  GOCCIA_BINARY_MAGIC: array[0..3] of Byte = (Ord('G'), Ord('B'), Ord('C'), 0);
  GOCCIA_NULLISH_MATCH_UNDEFINED = 0;
  GOCCIA_NULLISH_MATCH_NULL = 1;
  GOCCIA_NULLISH_MATCH_HOLE = 2;
  GOCCIA_NULLISH_MATCH_ANY = 255;
  ACCESSOR_FLAG_SETTER = 1;
  ACCESSOR_FLAG_STATIC = 2;
  COLLECTION_OP_SPREAD_OBJECT = 0;
  COLLECTION_OP_OBJECT_REST = 1;
  COLLECTION_OP_SPREAD_ITERABLE_INTO_ARRAY = 2;
  COLLECTION_OP_TRY_ITERABLE_TO_ARRAY = 3;
  VALIDATE_OP_REQUIRE_OBJECT = 0;
  VALIDATE_OP_REQUIRE_ITERABLE = 1;

  // Sentinel for the C operand of OP_VALIDATE_VALUE / VALIDATE_OP_REQUIRE_ITERABLE.
  //   0..254 = exact element count to consume (0 = empty pattern, drain
  //            nothing then close);
  //   255    = unbounded — emitted *only* for rest-pattern destructuring
  //            (`[a, b, ...rest] = iter`) where the iterator must drain
  //            completely.
  // Oversized fixed patterns (Count >= 255 without a rest element) are
  // rejected at compile time by EmitDestructuring in
  // Goccia.Compiler.Expressions.pas rather than collapsed to this
  // sentinel — that would silently switch "consume exactly N" to "drain
  // entirely" and hang on infinite iterators.  Likewise, rest patterns
  // whose start index would not fit in OP_UNPACK's UInt8 operand are
  // rejected there too.  This sentinel therefore only ever appears when
  // the caller actually wants the full-drain semantics.
  ITERABLE_LIMIT_UNBOUNDED = 255;

  MIN_SBX: Int16 = -32768;
  MAX_SBX: Int16 = 32767;

type
  { Core operations (hot/stable VM semantics) — opcodes 0..127 }
  { Non-core opcode space — generic arithmetic/bitwise ops in 128..166,
    semantic helper/orchestration ops in 167..255 }
  TGocciaOpCode = (
    OP_LOAD_CONST    = 0,
    OP_LOAD_TRUE     = 2,
    OP_LOAD_FALSE    = 3,
    OP_LOAD_INT      = 4,
    OP_MOVE          = 5,
    OP_GET_LOCAL     = 6,
    OP_SET_LOCAL     = 7,
    OP_GET_UPVALUE   = 8,
    OP_SET_UPVALUE   = 9,
    OP_CLOSE_UPVALUE = 10,
    OP_JUMP          = 11,
    OP_JUMP_IF_TRUE  = 12,
    OP_JUMP_IF_FALSE = 13,
    OP_JUMP_IF_NULLISH = 14,
    OP_JUMP_IF_NOT_NULLISH = 15,
    OP_CLOSURE       = 16,
    OP_PUSH_HANDLER  = 17,
    OP_POP_HANDLER   = 18,
    OP_THROW         = 19,
    OP_RETURN        = 20,
    OP_GET_THIS_BINDING = 21,
    OP_LOAD_UNDEFINED = 22,
    OP_ARRAY_POP     = 23,
    OP_NEW_ARRAY     = 24,
    OP_ARRAY_PUSH    = 25,
    OP_ARRAY_GET     = 26,
    OP_ARRAY_SET     = 27,
    OP_NEW_OBJECT    = 28,
    OP_GET_PROP_CONST = 29,
    OP_SET_PROP_CONST = 30,
    OP_DELETE_PROP_CONST = 31,
    OP_GET_LENGTH    = 32,
    OP_ARG_COUNT     = 33,
    OP_PACK_ARGS     = 34,
    OP_NOP           = 35,
    OP_LINE          = 36,
    OP_ADD_INT       = 37,
    OP_SUB_INT       = 38,
    OP_MUL_INT       = 39,
    OP_DIV_INT       = 40,
    OP_MOD_INT       = 41,
    OP_NEG_INT       = 42,
    OP_ADD_FLOAT     = 43,
    OP_SUB_FLOAT     = 44,
    OP_MUL_FLOAT     = 45,
    OP_DIV_FLOAT     = 46,
    OP_MOD_FLOAT     = 47,
    OP_NEG_FLOAT     = 48,
    OP_EQ_INT        = 49,
    OP_NEQ_INT       = 50,
    OP_LT_INT        = 51,
    OP_GT_INT        = 52,
    OP_LTE_INT       = 53,
    OP_GTE_INT       = 54,
    OP_CONCAT        = 55,
    OP_GET_GLOBAL    = 56,
    OP_SET_GLOBAL    = 57,
    OP_HAS_GLOBAL    = 58,
    OP_CALL          = 59,
    OP_CALL_METHOD   = 60,
    OP_CONSTRUCT     = 61,
    OP_GET_ITER      = 62,
    OP_ITER_NEXT     = 63,
    OP_TO_STRING     = 64,
    OP_NEW_CLASS     = 66,
    OP_CLASS_SET_SUPER = 67,
    OP_CLASS_ADD_METHOD_CONST = 68,
    OP_CLASS_SET_FIELD_INITIALIZER = 69,
    OP_CLASS_DECLARE_PRIVATE_STATIC_CONST = 70,
    OP_CLASS_EXEC_STATIC_BLOCK = 71,
    OP_TO_PRIMITIVE  = 72,
    OP_UNPACK        = 73,
    OP_LOAD_NULL     = 74,
    OP_LOAD_HOLE     = 75,
    OP_CHECK_TYPE    = 76,
    OP_EQ_FLOAT      = 77,
    OP_NEQ_FLOAT     = 78,
    OP_LT_FLOAT      = 79,
    OP_GT_FLOAT      = 80,
    OP_LTE_FLOAT     = 81,
    OP_GTE_FLOAT     = 82,
    OP_NOT           = 83,
    OP_TO_BOOL       = 84,
    OP_DEFINE_ACCESSOR_CONST = 85,
    OP_DEFINE_ACCESSOR_DYNAMIC = 86,
    OP_COLLECTION_OP = 93,
    OP_VALIDATE_VALUE = 95,
    OP_THROW_TYPE_ERROR_CONST = 98,
    OP_DEFINE_GLOBAL_CONST = 99,
    OP_FINALIZE_ENUM = 100,
    OP_SUPER_GET_CONST = 101,
    OP_SETUP_AUTO_ACCESSOR_CONST = 102,
    OP_BEGIN_DECORATORS = 103,
    OP_APPLY_CLASS_DECORATOR = 104,
    OP_FINISH_DECORATORS = 105,
    OP_APPLY_ELEMENT_DECORATOR_CONST = 106,
    OP_EQ            = 107,
    OP_NEQ           = 108,
    OP_LT            = 109,
    OP_GT            = 110,
    OP_LTE           = 111,
    OP_GTE           = 112,
    OP_TYPEOF        = 113,
    OP_IS_INSTANCE   = 114,
    OP_HAS_PROPERTY  = 115,
    OP_TO_NUMBER     = 116,
    OP_NEG           = 117,
    OP_BNOT          = 118,
    OP_GET_INDEX     = 119,
    OP_SET_INDEX     = 120,
    OP_DEL_INDEX     = 121,
    OP_ADD           = 128,
    OP_SUB           = 129,
    OP_MUL           = 130,
    OP_DIV           = 131,
    OP_MOD           = 132,
    OP_POW           = 133,
    OP_BAND          = 135,
    OP_BOR           = 136,
    OP_BXOR          = 137,
    OP_SHL           = 138,
    OP_SHR           = 139,
    OP_USHR          = 140,
    OP_IMPORT        = 167,
    OP_EXPORT        = 168,
    OP_AWAIT         = 169,
    OP_IMPORT_META   = 170,
    OP_DYNAMIC_IMPORT = 171,
    OP_USING_INIT    = 172,
    OP_USING_DISPOSE = 173,
    OP_YIELD         = 174,
    OP_MATCH_VALUE   = 175,
    OP_MATCH_HAS_PROPERTY = 176,
    OP_MATCH_EXTRACTOR = 177
  );

function EncodeABC(const AOp: TGocciaOpCode; const A, B, C: UInt8): UInt32; inline;
function EncodeABx(const AOp: TGocciaOpCode; const A: UInt8; const ABx: UInt16): UInt32; inline;
function EncodeAsBx(const AOp: TGocciaOpCode; const A: UInt8; const AAsBx: Int16): UInt32; inline;
function EncodeAx(const AOp: TGocciaOpCode; const AAx: Int32): UInt32; inline;

function DecodeOp(const AInstruction: UInt32): UInt8; inline;
function DecodeA(const AInstruction: UInt32): UInt8; inline;
function DecodeB(const AInstruction: UInt32): UInt8; inline;
function DecodeC(const AInstruction: UInt32): UInt8; inline;
function DecodeBx(const AInstruction: UInt32): UInt16; inline;
function DecodesBx(const AInstruction: UInt32): Int16; inline;
function DecodeAx(const AInstruction: UInt32): Int32; inline;

implementation

const
  SBIAS_16 = 32767;
  SBIAS_24 = 8388607;

function EncodeABC(const AOp: TGocciaOpCode; const A, B, C: UInt8): UInt32;
begin
  Result := UInt32(Ord(AOp)) or (UInt32(A) shl 8) or (UInt32(B) shl 16) or
    (UInt32(C) shl 24);
end;

function EncodeABx(const AOp: TGocciaOpCode; const A: UInt8;
  const ABx: UInt16): UInt32;
begin
  Result := UInt32(Ord(AOp)) or (UInt32(A) shl 8) or (UInt32(ABx) shl 16);
end;

function EncodeAsBx(const AOp: TGocciaOpCode; const A: UInt8;
  const AAsBx: Int16): UInt32;
var
  Biased: UInt16;
begin
  Biased := UInt16(Integer(AAsBx) + SBIAS_16);
  Result := UInt32(Ord(AOp)) or (UInt32(A) shl 8) or (UInt32(Biased) shl 16);
end;

function EncodeAx(const AOp: TGocciaOpCode; const AAx: Int32): UInt32;
var
  Biased: UInt32;
begin
  Biased := UInt32(AAx + SBIAS_24) and $FFFFFF;
  Result := UInt32(Ord(AOp)) or (Biased shl 8);
end;

function DecodeOp(const AInstruction: UInt32): UInt8;
begin
  Result := UInt8(AInstruction and $FF);
end;

function DecodeA(const AInstruction: UInt32): UInt8;
begin
  Result := UInt8((AInstruction shr 8) and $FF);
end;

function DecodeB(const AInstruction: UInt32): UInt8;
begin
  Result := UInt8((AInstruction shr 16) and $FF);
end;

function DecodeC(const AInstruction: UInt32): UInt8;
begin
  Result := UInt8((AInstruction shr 24) and $FF);
end;

function DecodeBx(const AInstruction: UInt32): UInt16;
begin
  Result := UInt16((AInstruction shr 16) and $FFFF);
end;

function DecodesBx(const AInstruction: UInt32): Int16;
begin
  Result := Int16(Integer(DecodeBx(AInstruction)) - SBIAS_16);
end;

function DecodeAx(const AInstruction: UInt32): Int32;
begin
  Result := Int32((AInstruction shr 8) and $FFFFFF) - SBIAS_24;
end;

end.
