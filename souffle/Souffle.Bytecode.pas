unit Souffle.Bytecode;

{$I Souffle.inc}

interface

const
  SOUFFLE_FORMAT_VERSION = 4;
  SOUFFLE_BINARY_MAGIC: array[0..3] of Byte = (Ord('S'), Ord('B'), Ord('C'), 0);

  OP_RT_FIRST = 128;

  MIN_SBX: Int16 = -32768;
  MAX_SBX: Int16 =  32767;

type
  { Core operations (VM-intrinsic, fixed semantics) — opcodes 0..127 }
  { Runtime operations (pluggable semantics) — opcodes 128..255 }
  TSouffleOpCode = (
    // ── Core: Load / Store / Move ──
    OP_LOAD_CONST    = 0,   // ABx   R[A] := Constants[Bx]
    OP_LOAD_NIL      = 1,   // AB    R[A] := Nil(flags=B)
    OP_LOAD_TRUE     = 2,   // A     R[A] := Boolean(true)
    OP_LOAD_FALSE    = 3,   // A     R[A] := Boolean(false)
    OP_LOAD_INT      = 4,   // AsBx  R[A] := Integer(sBx)
    OP_MOVE          = 5,   // AB    R[A] := R[B]

    // ── Core: Variables ──
    OP_GET_LOCAL     = 6,   // ABx   R[A] := Locals[Bx]
    OP_SET_LOCAL     = 7,   // ABx   Locals[Bx] := R[A]
    OP_GET_UPVALUE   = 8,   // ABx   R[A] := Upvalues[Bx]
    OP_SET_UPVALUE   = 9,   // ABx   Upvalues[Bx] := R[A]
    OP_CLOSE_UPVALUE = 10,  // A     Close upvalue for R[A]

    // ── Core: Control Flow ──
    OP_JUMP          = 11,  // Ax    PC += Ax (signed 24-bit)
    OP_JUMP_IF_TRUE  = 12,  // AsBx  if IsTrue(R[A]): PC += sBx
    OP_JUMP_IF_FALSE = 13,  // AsBx  if not IsTrue(R[A]): PC += sBx
    OP_JUMP_IF_NIL   = 14,  // ABC   if R[A] is nil(flags match B): PC += C — B=255 matches any nil, B=0..254 matches specific flag
    OP_JUMP_IF_NOT_NIL = 15, // ABC   if R[A] is not nil(flags match B): PC += C — B=255 skips unless any nil, B=0..254 skips unless specific flag

    // ── Core: Closures ──
    OP_CLOSURE       = 16,  // ABx   R[A] := Closure(FunctionTemplates[Bx])

    // ── Core: Exception Handling ──
    OP_PUSH_HANDLER  = 17,  // ABx   Push handler: catch at PC+Bx, value in R[A]
    OP_POP_HANDLER   = 18,  //       Pop innermost handler
    OP_THROW         = 19,  // A     Throw R[A]

    // ── Core: Return ──
    OP_RETURN        = 20,  // A     Return R[A] to caller
    OP_RETURN_NIL    = 21,  //       Return Nil to caller

    // ── Core: Compound Types ──
    OP_ARRAY_POP     = 23,  // AB    R[A] := Array(R[B]).Pop()
    OP_NEW_ARRAY     = 24,  // AB    R[A] := new Array(capacity=B)
    OP_ARRAY_PUSH    = 25,  // AB    Array(R[A]).push(R[B])
    OP_ARRAY_GET     = 26,  // ABC   R[A] := Array(R[B])[R[C].AsInteger]
    OP_ARRAY_SET     = 27,  // ABC   Array(R[A])[R[B].AsInteger] := R[C]
    OP_NEW_RECORD    = 28,  // AB    R[A] := new Record(capacity=B)
    OP_RECORD_GET    = 29,  // ABC   R[A] := Record(R[B])[Constants[C]]
    OP_RECORD_SET    = 30,  // ABC   Record(R[A])[Constants[B]] := R[C]
    OP_RECORD_DELETE = 31,  // ABx   delete Record(R[A])[Constants[Bx]]
    OP_GET_LENGTH    = 32,  // AB    R[A] := Length(R[B])

    // ── Core: Arguments ──
    OP_ARG_COUNT     = 33,  // A     R[A] := ArgCount (number of actual arguments passed)
    OP_PACK_ARGS     = 34,  // AB    R[A] := Array(args[B..ArgCount-1])

    // ── Core: Debug ──
    OP_NOP           = 35,  //       No operation
    OP_LINE          = 36,  // Bx    Source line annotation

    // ── Core: Integer Arithmetic ──
    OP_ADD_INT       = 37,  // ABC   R[A] := R[B].AsInteger + R[C].AsInteger
    OP_SUB_INT       = 38,  // ABC   R[A] := R[B].AsInteger - R[C].AsInteger
    OP_MUL_INT       = 39,  // ABC   R[A] := R[B].AsInteger * R[C].AsInteger
    OP_DIV_INT       = 40,  // ABC   R[A] := Float(R[B].AsInteger) / Float(R[C].AsInteger)
    OP_MOD_INT       = 41,  // ABC   R[A] := R[B].AsInteger mod R[C].AsInteger
    OP_NEG_INT       = 42,  // AB    R[A] := -R[B].AsInteger

    // ── Core: Float Arithmetic ──
    OP_ADD_FLOAT     = 43,  // ABC   R[A] := R[B].AsFloat + R[C].AsFloat
    OP_SUB_FLOAT     = 44,  // ABC   R[A] := R[B].AsFloat - R[C].AsFloat
    OP_MUL_FLOAT     = 45,  // ABC   R[A] := R[B].AsFloat * R[C].AsFloat
    OP_DIV_FLOAT     = 46,  // ABC   R[A] := R[B].AsFloat / R[C].AsFloat
    OP_MOD_FLOAT     = 47,  // ABC   R[A] := FMod(R[B].AsFloat, R[C].AsFloat)
    OP_NEG_FLOAT     = 48,  // AB    R[A] := -R[B].AsFloat

    // ── Core: Integer Comparison ──
    OP_EQ_INT        = 49,  // ABC   R[A] := Boolean(R[B].AsInteger = R[C].AsInteger)
    OP_NEQ_INT       = 50,  // ABC   R[A] := Boolean(R[B].AsInteger <> R[C].AsInteger)
    OP_LT_INT        = 51,  // ABC   R[A] := Boolean(R[B].AsInteger < R[C].AsInteger)
    OP_GT_INT        = 52,  // ABC   R[A] := Boolean(R[B].AsInteger > R[C].AsInteger)
    OP_LTE_INT       = 53,  // ABC   R[A] := Boolean(R[B].AsInteger <= R[C].AsInteger)
    OP_GTE_INT       = 54,  // ABC   R[A] := Boolean(R[B].AsInteger >= R[C].AsInteger)

    // ── Core: String ──
    OP_CONCAT        = 55,  // ABC   R[A] := String(R[B]) + String(R[C])

    // ── Core: Blueprint ──
    OP_NEW_BLUEPRINT = 66,  // ABx   R[A] := new Blueprint(name=Constants[Bx])
    OP_INHERIT       = 67,  // AB    Blueprint(R[A]).super := Blueprint(R[B])
    // opcode 68 removed: was OP_BLUEPRINT_METHOD, use OP_RECORD_SET on blueprint instead
    OP_INSTANTIATE   = 69,  // AB    R[A] := new Record(blueprint=Blueprint(R[B]))
    OP_GET_SLOT      = 70,  // ABC   R[A] := Record(R[B]).slots[C]
    OP_SET_SLOT      = 71,  // ABC   Record(R[A]).slots[B] := R[C]

    // ── Core: Type Coercion ──
    OP_TO_PRIMITIVE  = 72,  // AB    R[A] := ToPrimitive(R[B]) — fast-path for nil/bool/int/float/string, runtime callback for references

    // ── Core: Destructuring ──
    OP_UNPACK        = 73,  // ABC   R[A] := Array(R[B])[C..] — unpack rest of TSouffleArray from index C

    // ── Core: Type Checking ──
    OP_CHECK_TYPE    = 76,  // ABC   if R[A] not matches TSouffleLocalType(B): RuntimeOps.CheckLocalType(R[A], B)

    // ── Core: Float Comparison ──
    OP_EQ_FLOAT      = 77,  // ABC   R[A] := Boolean(AsNumber(R[B]) = AsNumber(R[C]))
    OP_NEQ_FLOAT     = 78,  // ABC   R[A] := Boolean(AsNumber(R[B]) <> AsNumber(R[C]))
    OP_LT_FLOAT      = 79,  // ABC   R[A] := Boolean(AsNumber(R[B]) < AsNumber(R[C]))
    OP_GT_FLOAT      = 80,  // ABC   R[A] := Boolean(AsNumber(R[B]) > AsNumber(R[C]))
    OP_LTE_FLOAT     = 81,  // ABC   R[A] := Boolean(AsNumber(R[B]) <= AsNumber(R[C]))
    OP_GTE_FLOAT     = 82,  // ABC   R[A] := Boolean(AsNumber(R[B]) >= AsNumber(R[C]))

    // ── Core: Boolean ──
    OP_NOT           = 83,  // AB    R[A] := Boolean(not IsTrue(R[B]))
    OP_TO_BOOL       = 84,  // AB    R[A] := Boolean(IsTrue(R[B]))

    // ── Runtime: Polymorphic Arithmetic ──
    OP_RT_ADD        = 128, // ABC   R[A] := Runtime.Add(R[B], R[C])
    OP_RT_SUB        = 129, // ABC   R[A] := Runtime.Subtract(R[B], R[C])
    OP_RT_MUL        = 130, // ABC   R[A] := Runtime.Multiply(R[B], R[C])
    OP_RT_DIV        = 131, // ABC   R[A] := Runtime.Divide(R[B], R[C])
    OP_RT_MOD        = 132, // ABC   R[A] := Runtime.Modulo(R[B], R[C])
    OP_RT_POW        = 133, // ABC   R[A] := Runtime.Power(R[B], R[C])
    OP_RT_NEG        = 134, // AB    R[A] := Runtime.Negate(R[B])

    // ── Runtime: Polymorphic Bitwise ──
    OP_RT_BAND       = 135, // ABC   R[A] := Runtime.BitwiseAnd(R[B], R[C])
    OP_RT_BOR        = 136, // ABC   R[A] := Runtime.BitwiseOr(R[B], R[C])
    OP_RT_BXOR       = 137, // ABC   R[A] := Runtime.BitwiseXor(R[B], R[C])
    OP_RT_SHL        = 138, // ABC   R[A] := Runtime.ShiftLeft(R[B], R[C])
    OP_RT_SHR        = 139, // ABC   R[A] := Runtime.ShiftRight(R[B], R[C])
    OP_RT_USHR       = 140, // ABC   R[A] := Runtime.UnsignedShiftRight(R[B], R[C])
    OP_RT_BNOT       = 141, // AB    R[A] := Runtime.BitwiseNot(R[B])

    // ── Runtime: Polymorphic Comparison ──
    OP_RT_EQ         = 142, // ABC   R[A] := Runtime.Equal(R[B], R[C])
    OP_RT_NEQ        = 143, // ABC   R[A] := Runtime.NotEqual(R[B], R[C])
    OP_RT_LT         = 144, // ABC   R[A] := Runtime.LessThan(R[B], R[C])
    OP_RT_GT         = 145, // ABC   R[A] := Runtime.GreaterThan(R[B], R[C])
    OP_RT_LTE        = 146, // ABC   R[A] := Runtime.LessThanOrEqual(R[B], R[C])
    OP_RT_GTE        = 147, // ABC   R[A] := Runtime.GreaterThanOrEqual(R[B], R[C])

    // ── Runtime: Logical / Type ──
    OP_RT_TYPEOF     = 149, // AB    R[A] := Runtime.TypeOf(R[B])
    OP_RT_IS_INSTANCE = 150, // ABC  R[A] := Runtime.IsInstance(R[B], R[C])
    OP_RT_HAS_PROPERTY = 151, // ABC R[A] := Runtime.HasProperty(R[B], R[C])
    OP_RT_TO_NUMBER  = 153, // AB    R[A] := ToNumber(R[B]) — numeric identity, else Runtime.ToNumber

    // ── Runtime: Property Access ──
    OP_RT_GET_PROP   = 156, // ABC   R[A] := Runtime.GetProperty(R[B], Constants[C])
    OP_RT_SET_PROP   = 157, // ABC   Runtime.SetProperty(R[A], Constants[B], R[C])
    OP_RT_GET_INDEX  = 158, // ABC   R[A] := Runtime.GetIndex(R[B], R[C])
    OP_RT_SET_INDEX  = 159, // ABC   Runtime.SetIndex(R[A], R[B], R[C])
    OP_RT_DEL_PROP   = 160, // ABC   R[A] := Runtime.DeleteProperty(R[B], Constants[C])

    // ── Runtime: Invocation ──
    OP_RT_CALL       = 161, // ABC   R[A] := Runtime.Invoke(R[A], args, B=argc, C=flags); C bit 0: spread (B=argsArray reg)
    OP_RT_CALL_METHOD = 162, // ABC  Like RT_CALL but receiver = R[A-1]; C bit 0: spread (B=argsArray reg)
    OP_RT_CONSTRUCT  = 163, // ABC   R[A] := Runtime.Construct(R[B], args, C=argc)

    // ── Runtime: Iteration ──
    OP_RT_GET_ITER   = 164, // ABC   R[A] := Runtime.GetIterator(R[B], C) — C: 0=sync, 1=try-async-first
    OP_RT_ITER_NEXT  = 165, // ABC   (R[A], R[B]) := Runtime.IteratorNext(R[C])

    // ── Runtime: Modules ──
    OP_RT_IMPORT     = 167, // ABx   R[A] := Runtime.ImportModule(Constants[Bx])
    OP_RT_EXPORT     = 168, // ABx   Runtime.ExportBinding(R[A], Constants[Bx])

    // ── Runtime: Async ──
    OP_RT_AWAIT      = 169, // AB    R[A] := Runtime.Await(R[B])

    // ── Runtime: Globals ──
    OP_RT_GET_GLOBAL = 170, // ABx   R[A] := Runtime.GetGlobal(Constants[Bx])
    OP_RT_SET_GLOBAL = 171, // ABx   Runtime.SetGlobal(Constants[Bx], R[A])
    OP_RT_HAS_GLOBAL = 172, // ABx   R[A] := Boolean(Runtime.HasGlobal(Constants[Bx]))

    // ── Runtime: Extended Property ──
    OP_RT_DEL_INDEX  = 175, // ABC   R[A] := Runtime.DeleteIndex(R[B], R[C])

    // ── Runtime: Coercion ──
    OP_RT_TO_STRING = 182, // AB    R[A] := Runtime.CoerceValueToString(R[B])

    // ── Runtime: Language Extension ──
    OP_RT_EXT = 190             // ABC   Runtime.ExtendedOperation(B=sub-opcode, R[A], R[C], R[A+1], Template, C)
  );

{ Instruction encoding/decoding helpers }

{ Encoding: pack opcode + operands into a 32-bit instruction word }
function EncodeABC(const AOp: TSouffleOpCode; const A, B, C: UInt8): UInt32; inline;
function EncodeABx(const AOp: TSouffleOpCode; const A: UInt8; const ABx: UInt16): UInt32; inline;
function EncodeAsBx(const AOp: TSouffleOpCode; const A: UInt8; const AAsBx: Int16): UInt32; inline;
function EncodeAx(const AOp: TSouffleOpCode; const AAx: Int32): UInt32; inline;

{ Decoding: extract fields from a 32-bit instruction word }
function DecodeOp(const AInstruction: UInt32): UInt8; inline;
function DecodeA(const AInstruction: UInt32): UInt8; inline;
function DecodeB(const AInstruction: UInt32): UInt8; inline;
function DecodeC(const AInstruction: UInt32): UInt8; inline;
function DecodeBx(const AInstruction: UInt32): UInt16; inline;
function DecodesBx(const AInstruction: UInt32): Int16; inline;
function DecodeAx(const AInstruction: UInt32): Int32; inline;

function IsRuntimeOp(const AInstruction: UInt32): Boolean; inline;

implementation

{ Instruction layout:
    ABC:  [op:8][A:8][B:8][C:8]
    ABx:  [op:8][A:8][Bx:16]
    AsBx: [op:8][A:8][sBx:16]  (sBx is signed via bias: stored as sBx + 32767)
    Ax:   [op:8][Ax:24]        (Ax is signed via bias: stored as Ax + 8388607) }

const
  SBIAS_16 = 32767;
  SBIAS_24 = 8388607;

{ Encoding }

function EncodeABC(const AOp: TSouffleOpCode; const A, B, C: UInt8): UInt32;
begin
  Result := UInt32(Ord(AOp)) or (UInt32(A) shl 8) or (UInt32(B) shl 16) or (UInt32(C) shl 24);
end;

function EncodeABx(const AOp: TSouffleOpCode; const A: UInt8; const ABx: UInt16): UInt32;
begin
  Result := UInt32(Ord(AOp)) or (UInt32(A) shl 8) or (UInt32(ABx) shl 16);
end;

function EncodeAsBx(const AOp: TSouffleOpCode; const A: UInt8; const AAsBx: Int16): UInt32;
var
  Biased: UInt16;
begin
  Biased := UInt16(Integer(AAsBx) + SBIAS_16);
  Result := UInt32(Ord(AOp)) or (UInt32(A) shl 8) or (UInt32(Biased) shl 16);
end;

function EncodeAx(const AOp: TSouffleOpCode; const AAx: Int32): UInt32;
var
  Biased: UInt32;
begin
  Biased := UInt32(AAx + SBIAS_24) and $FFFFFF;
  Result := UInt32(Ord(AOp)) or (Biased shl 8);
end;

{ Decoding }

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

function IsRuntimeOp(const AInstruction: UInt32): Boolean;
begin
  Result := (AInstruction and $FF) >= OP_RT_FIRST;
end;

end.
