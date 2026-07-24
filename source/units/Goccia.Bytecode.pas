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
  //   v19 -> v20: iterator destructuring close work changed
  //               OP_VALIDATE_VALUE / VALIDATE_OP_REQUIRE_ITERABLE
  //               operand C semantics.  Pre-v20, C = 0 meant
  //               "unbounded".  v20 introduces ITERABLE_LIMIT_UNBOUNDED
  //               (255) as the explicit unbounded sentinel and treats
  //               C = 0 as "consume exactly zero elements then close"
  //               (`const [] = iter`).  Loading a pre-v20 file in a
  //               v20 VM would silently switch unbounded patterns to
  //               short-circuit close, so the version bump prevents
  //               that.
  //   v20 -> v21: #482 added OP_ITER_CLOSE so bytecode for abrupt
  //               for...of completion can close iterator records in
  //               the VM.
  //   v21 -> v22: #490 added OP_SUPER_GET so computed super property
  //               access uses receiver-aware lookup instead of generic
  //               index access.
  //   v22 -> v23: #490 gave OP_SUPER_GET_CONST/OP_SUPER_GET operand B
  //               call-context semantics so only direct super() reads use
  //               the synthetic super-constructor helper.
  //   v23 -> v24: #491 added OP_THROW_TYPE_ERROR_CONST_LONG so const-assignment
  //               diagnostics can reference constant-pool strings above 255.
  //   v24 -> v25: #540 added OP_INC, OP_DEC, OP_TO_NUMERIC for spec-correct
  //               BigInt increment/decrement (ES2026 §13.4.4.1 ToNumeric).
  //   v25 -> v26: #529 added OP_NEW_TARGET for the new.target meta-property.
  //   v26 -> v27: added OP_LOOSE_EQ/OP_LOOSE_NEQ for
  //               --compat-loose-equality bytecode.
  //   v27 -> v28: added OP_CREATE_ARGUMENTS plus with-statement helper
  //               opcodes for object environment records.
  //   v28 -> v29: added non-strict delete opcodes for
  //               --compat-non-strict-mode bytecode.
  //   v29 -> v30: serialized function strict-this mode for
  //               --compat-non-strict-mode bytecode.
  //   v30 -> v31: added non-strict assignment/global-delete opcodes for
  //               --compat-non-strict-mode bytecode.
  //   v31 -> v32: added OP_DEFINE_STATIC_PROP_CONST and
  //               OP_DEFINE_STATIC_METHOD_CONST so class static fields and
  //               methods define own data properties instead of using ordinary
  //               assignment semantics.
  //   v32 -> v33: added OP_DEFINE_DATA_PROP so bytecode object literals define
  //               own data properties instead of assigning through prototypes.
  //   v33 -> v34: added OP_DEFINE_METHOD_PROP so concise object methods get
  //               [[HomeObject]] without affecting plain data properties.
  //   v34 -> v35: added OP_DEFINE_PROP_DYNAMIC for computed public
  //               class fields.
  //   v35 -> v36: added OP_TO_PROPERTY_KEY so delayed computed class field
  //               definitions can reuse source-order property keys.
  //   v36 -> v37: added OP_SETUP_AUTO_ACCESSOR_DYNAMIC for computed
  //               auto-accessor keys.
  //   v37 -> v38: added OP_ENUM_KEYS for --compat-for-in-loop bytecode.
  //   v38 -> v39: OP_ENUM_KEYS now creates revalidating for-in entry arrays,
  //               and OP_ENUM_ENTRY validates entries.
  //   v39 -> v40: serialized ParameterPreambleSize so bytecode generators
  //               run FunctionDeclarationInstantiation-side parameter
  //               binding at call time while keeping the body suspended.
  //   v40 -> v41: serialized direct-eval caller binding snapshots and added
  //               an OP_CALL flag for syntactic eval call sites.
  //   v41 -> v42: direct-eval binding snapshots mark arrow synthetic
  //               arguments bindings and templates serialize that local slot.
  //   v42 -> v43: direct-eval binding snapshots include hidden with-object
  //               environment slots.
  //   v43 -> v44: direct-eval snapshots mark caller var-environment bindings
  //               separately from intervening lexical bindings.
  //   v44 -> v45: added bckRegExpLiteral constants so bytecode regexp
  //               literals create fresh RegExp objects from cached compiled
  //               programs instead of calling the global RegExp constructor.
  //   v45 -> v46: split for-await-of iteration into OP_ASYNC_ITER_NEXT,
  //               OP_AWAIT, and OP_ITER_UNPACK so IteratorResult promises
  //               are awaited before done/value extraction.
  //   v46 -> v47: added OP_SET_FUNCTION_NAME so computed class elements can
  //               derive method/accessor names from runtime property keys.
  //   v47 -> v48: added OP_LOAD_ARGUMENT so default-parameter preambles can
  //               restore actual argument values while enforcing TDZ for
  //               later parameters during earlier default initializers, and
  //               OP_CHECK_DERIVED_THIS for derived constructor `this` TDZ.
  //   v48 -> v49: serialized the class-field-initializer direct-eval
  //               `arguments` rejection flag on function templates.
  //   v49 -> v50: serialized upvalue descriptor names for bytecode
  //               direct-eval dynamic-scope lookups.
  //   v50 -> v51: added OP_SUPER_SET so super property writes use the
  //               super base with the actual receiver instead of ordinary
  //               assignment to this.
  //   v51 -> v52: serialized class-field direct-eval `arguments`
  //               rejection at each direct-eval call site, so static
  //               field initializers compiled into enclosing templates do
  //               not affect unrelated eval calls.
  //   v52 -> v53: added OP_IMPORT_DEFER so bytecode can bind static
  //               `import defer * as ns from "mod"` to a synchronous
  //               deferred module namespace.
  //   v53 -> v54: added OP_IMPORT_SOURCE so bytecode can bind static
  //               `import source x from "mod"` to a ModuleSource object.
  //   v54 -> v55: added OP_DYNAMIC_IMPORT_OPTIONS so bytecode can preserve
  //               dynamic import attribute option validation.
  //   v55 -> v56: OP_CREATE_ARGUMENTS operand B now selects mapped
  //               arguments-object semantics for sloppy simple parameter
  //               lists, and operand C carries the formal parameter count.
  //   v56 -> v57: added phase-specific dynamic import option opcodes so
  //               import.source/import.defer preserve import attributes.
  //   v57 -> v58: OP_DEFINE_GLOBAL_CONST operand B gained global lexical
  //               predeclaration modes so top-level bytecode scripts preserve
  //               TDZ for global-backed let/const/class/enum bindings.
  //   v58 -> v59: added OP_INC_NUMERIC/OP_DEC_NUMERIC and postfix variants so
  //               update expressions can fuse ToNumeric with unit arithmetic.
  //   v59 -> v60: OP_DEFINE_GLOBAL_CONST operand B gained a global function
  //               declaration mode to preserve CreateGlobalFunctionBinding.
  //   v60 -> v61: added OP_SET_OBJECT_PROTO for object-literal
  //               non-computed __proto__ data properties.
  //   v61 -> v62: OP_ITER_CLOSE operand C gained a conditional mode for
  //               destructuring iterator-close handlers: preserve thrown
  //               completions, but let generator-return close errors replace
  //               the return completion.
  //   v62 -> v63: added OP_DEFINE_CLASS_METHOD_DYNAMIC so computed class
  //               methods define non-enumerable method properties.
  //   v63 -> v64: added OP_GET_WITH_BINDING(_STRICT) and
  //               OP_SET_WITH_BINDING(_LOOSE) so bytecode object environment
  //               records re-check binding-object properties at get/set time.
  //   v64 -> v65: added explicit super-base capture opcodes for computed
  //               super property references.
  //   v65 -> v66: added long-name global var declaration and dynamic
  //               static/private field definition opcodes for oversized
  //               test262 identifier/class-field generated sources.
  //   v66 -> v67: added OP_GET_IMPORT_BINDING so bytecode rejects a named
  //               import of a missing export at its use site, matching the
  //               interpreter's link-time SyntaxError (ADR 0014).
  //   v67 -> v68: added long-name global define/predeclare opcodes so
  //               generated sources with more than 255 global names can keep
  //               using 16-bit constant-pool indices.
  //   v68 -> v69: added OP_LOAD_CHAR so generated sources with many distinct
  //               one-code-unit string literals do not exhaust the 16-bit
  //               constant pool.
  //   v69 -> v70: added OP_SET_CLASS_SOURCE_CONST so Function.prototype.toString
  //               can return exact class source text in bytecode mode.
  //   v70 -> v71: added resolved environment-reference and static global-set
  //               opcodes so assignment preserves its left-hand Reference
  //               across direct eval.
  //   v71 -> v72: OP_WIDE prefixes extend A/B/C operands to 16 bits. Function
  //               metadata that names registers, locals, upvalues, parameters,
  //               or preamble instructions uses matching 16-bit fields.
  //               OP_CLOSE_UPVALUE stores its local slot in Bx, and
  //               OP_CONSTRUCT_SPREAD carries its argument-array register
  //               without the old 7-bit flag packing.
  //   v72 -> v73: serialized strings changed from UTF-8 bytes to raw
  //               little-endian UTF-16 code units so bytecode constants
  //               preserve every ECMAScript string value, including lone
  //               surrogates.
  //   v73 -> v74: added numeric immediate superinstructions for proven
  //               Number subtraction and less-than-or-equal branches.
  //   v74 -> v75: added a direct scalar-frame self-call opcode for functions
  //               accepted by the closed-world numeric-call proof.
  GOCCIA_FORMAT_VERSION = 75;
  GOCCIA_BINARY_MAGIC: array[0..3] of Byte = (Ord('G'), Ord('B'), Ord('C'), 0);
  GOCCIA_NULLISH_MATCH_UNDEFINED = 0;
  GOCCIA_NULLISH_MATCH_NULL = 1;
  GOCCIA_NULLISH_MATCH_HOLE = 2;
  GOCCIA_NULLISH_MATCH_ANY = 255;
  ACCESSOR_FLAG_SETTER = 1;
  ACCESSOR_FLAG_STATIC = 2;
  FUNCTION_NAME_PREFIX_NONE = 0;
  FUNCTION_NAME_PREFIX_GET  = 1;
  FUNCTION_NAME_PREFIX_SET  = 2;
  COLLECTION_OP_SPREAD_OBJECT = 0;
  COLLECTION_OP_OBJECT_REST = 1;
  COLLECTION_OP_SPREAD_ITERABLE_INTO_ARRAY = 2;
  COLLECTION_OP_TRY_ITERABLE_TO_ARRAY = 3;
  VALIDATE_OP_REQUIRE_OBJECT = 0;
  VALIDATE_OP_REQUIRE_ITERABLE = 1;
  ITER_CLOSE_NORMAL = 0;
  ITER_CLOSE_PRESERVE_ERROR = 1;
  ITER_CLOSE_PRESERVE_UNLESS_GENERATOR_RETURN = 2;
  CALL_FLAG_SPREAD = 1;
  CALL_FLAG_TRUSTED = 2;
  CALL_FLAG_DIRECT_EVAL = 4;
  // ES2026 §15.10 Tail Position Calls: the call sits in tail position of its
  // enclosing function, so the VM reuses the current call frame instead of
  // pushing a new one (PrepareForTailCall).
  CALL_FLAG_TAIL = 8;

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
    OP_LOAD_REGEXP   = 1,
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
    OP_ITER_CLOSE    = 65,
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
    OP_SUPER_GET     = 87,
    OP_LOOSE_EQ      = 88,
    OP_LOOSE_NEQ     = 89,
    OP_DELETE_PROP_CONST_LOOSE = 90,
    OP_DEL_INDEX_LOOSE = 91,
    OP_SET_PROP_CONST_LOOSE = 92,
    OP_COLLECTION_OP = 93,
    OP_SET_INDEX_LOOSE = 94,
    OP_VALIDATE_VALUE = 95,
    OP_SET_GLOBAL_LOOSE = 96,
    OP_THROW_TYPE_ERROR_CONST_LONG = 97,
    OP_THROW_TYPE_ERROR_CONST = 98,
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
    OP_DELETE_GLOBAL = 122,
    OP_DEFINE_STATIC_PROP_CONST = 123,
    OP_DEFINE_STATIC_METHOD_CONST = 124,
    OP_DEFINE_DATA_PROP = 125,
    OP_DEFINE_METHOD_PROP = 126,
    OP_SET_OBJECT_PROTO = 127,
    OP_DEFINE_PROP_DYNAMIC = 128,
    OP_ADD           = 129,
    OP_SUB           = 130,
    OP_MUL           = 131,
    OP_DIV           = 132,
    OP_MOD           = 133,
    OP_POW           = 134,
    OP_SETUP_AUTO_ACCESSOR_DYNAMIC = 135,
    OP_BAND          = 136,
    OP_BOR           = 137,
    OP_BXOR          = 138,
    OP_SHL           = 139,
    OP_SHR           = 140,
    OP_USHR          = 141,
    OP_DEFINE_CLASS_METHOD_DYNAMIC = 142,
    OP_SET_CLASS_SOURCE_CONST = 143,
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
    OP_MATCH_EXTRACTOR = 177,
    OP_INC           = 178,
    OP_DEC           = 179,
    OP_TO_NUMERIC    = 180,
    OP_NEW_TARGET    = 181,
    OP_CREATE_ARGUMENTS = 182,
    OP_TO_OBJECT     = 183,
    OP_HAS_WITH_BINDING = 184,
    OP_TO_PROPERTY_KEY = 185,
    OP_ENUM_KEYS     = 186,
    OP_ENUM_ENTRY    = 187,
    OP_ASYNC_ITER_NEXT = 188,
    OP_ITER_UNPACK   = 189,
    OP_SET_FUNCTION_NAME = 190,
    OP_LOAD_ARGUMENT = 191,
    OP_CHECK_DERIVED_THIS = 192,
    OP_SUPER_SET     = 193,
    OP_IMPORT_DEFER  = 194,
    OP_IMPORT_SOURCE = 195,
    OP_DYNAMIC_IMPORT_OPTIONS = 196,
    OP_DYNAMIC_IMPORT_SOURCE_OPTIONS = 197,
    OP_DYNAMIC_IMPORT_DEFER_OPTIONS = 198,
    OP_INC_NUMERIC    = 199,
    OP_DEC_NUMERIC    = 200,
    OP_POST_INC_NUMERIC = 201,
    OP_POST_DEC_NUMERIC = 202,
    OP_GET_WITH_BINDING = 203,
    OP_GET_WITH_BINDING_STRICT = 204,
    OP_SET_WITH_BINDING = 205,
    OP_SET_WITH_BINDING_LOOSE = 206,
    OP_SUPER_BASE    = 207,
    OP_SUPER_GET_BASE = 208,
    OP_SUPER_SET_BASE = 209,
    OP_DEFINE_GLOBAL_VAR_DECL_LONG = 210,
    OP_DEFINE_STATIC_PROP_DYNAMIC = 211,
    OP_PUSH_FINALLY_HANDLER = 212,
    OP_GET_IMPORT_BINDING = 213,
    OP_DEFINE_GLOBAL_VAR_LONG = 214,
    OP_DEFINE_GLOBAL_LET_LONG = 215,
    OP_DEFINE_GLOBAL_CONST_LONG = 216,
    OP_DEFINE_GLOBAL_FUNCTION_LONG = 217,
    OP_PREDECLARE_GLOBAL_LET_LONG = 218,
    OP_PREDECLARE_GLOBAL_CONST_LONG = 219,
    OP_LOAD_CHAR     = 220,
    OP_RESOLVE_UPVALUE_REF = 221,
    OP_SET_UPVALUE_REF = 222,
    OP_SET_GLOBAL_STATIC = 223,
    OP_SET_UPVALUE_DYNAMIC = 224,
    // Prefix for the following instruction. A/B/C contain the high byte of
    // the following instruction's A/B/C operands; the base word keeps the
    // ordinary compact encoding.
    OP_WIDE          = 225,
    OP_CONSTRUCT_SPREAD = 226,
    // A = destination, B = proven Number source, C = signed Int16 immediate.
    OP_SUB_NUM_IMM   = 227,
    // A = proven Number source, B = signed Int16 immediate,
    // C = signed Int16 forward jump offset when A <= B is false.
    OP_JUMP_IF_NUM_NOT_LTE_IMM = 228,
    // A = destination, B = first contiguous argument register,
    // C = argument count (1..3). Valid only in a compiler-proven closed-world
    // numeric self-recursive template.
    OP_CALL_SELF_NUM = 229
  );

function EncodeABC(const AOp: TGocciaOpCode; const A, B, C: UInt16): UInt64; {$IFDEF FPC}inline;{$ENDIF}
function EncodeABx(const AOp: TGocciaOpCode; const A: UInt16; const ABx: UInt16): UInt64; {$IFDEF FPC}inline;{$ENDIF}
function EncodeAsBx(const AOp: TGocciaOpCode; const A: UInt16;
  const AAsBx: Int16): UInt64; {$IFDEF FPC}inline;{$ENDIF}
function EncodeAx(const AOp: TGocciaOpCode;
  const AAx: Int32): UInt64; {$IFDEF FPC}inline;{$ENDIF}

function DecodeOp(const AInstruction: UInt32): UInt8; {$IFDEF FPC}inline;{$ENDIF}
function DecodeA(const AInstruction: UInt32): UInt8; {$IFDEF FPC}inline;{$ENDIF}
function DecodeB(const AInstruction: UInt32): UInt8; {$IFDEF FPC}inline;{$ENDIF}
function DecodeC(const AInstruction: UInt32): UInt8; {$IFDEF FPC}inline;{$ENDIF}
function DecodeBx(const AInstruction: UInt32): UInt16; {$IFDEF FPC}inline;{$ENDIF}
function DecodesBx(const AInstruction: UInt32): Int16; {$IFDEF FPC}inline;{$ENDIF}
function DecodeAx(const AInstruction: UInt32): Int32; {$IFDEF FPC}inline;{$ENDIF}

implementation

const
  SBIAS_16 = 32767;
  SBIAS_24 = 8388607;

function EncodeABC(const AOp: TGocciaOpCode; const A, B, C: UInt16): UInt64;
begin
  Result := UInt64(Ord(AOp)) or (UInt64(A and $FF) shl 8) or
    (UInt64(B and $FF) shl 16) or (UInt64(C and $FF) shl 24) or
    (UInt64(A shr 8) shl 32) or (UInt64(B shr 8) shl 40) or
    (UInt64(C shr 8) shl 48);
end;

function EncodeABx(const AOp: TGocciaOpCode; const A: UInt16;
  const ABx: UInt16): UInt64;
begin
  Result := UInt64(Ord(AOp)) or (UInt64(A and $FF) shl 8) or
    (UInt64(ABx) shl 16) or (UInt64(A shr 8) shl 32);
end;

function EncodeAsBx(const AOp: TGocciaOpCode; const A: UInt16;
  const AAsBx: Int16): UInt64;
var
  Biased: UInt16;
begin
  Biased := UInt16(Integer(AAsBx) + SBIAS_16);
  Result := UInt64(Ord(AOp)) or (UInt64(A and $FF) shl 8) or
    (UInt64(Biased) shl 16) or (UInt64(A shr 8) shl 32);
end;

function EncodeAx(const AOp: TGocciaOpCode; const AAx: Int32): UInt64;
var
  Biased: UInt32;
begin
  Biased := UInt32(AAx + SBIAS_24) and $FFFFFF;
  Result := UInt64(Ord(AOp)) or (UInt64(Biased) shl 8);
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
