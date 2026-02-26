unit Souffle.Bytecode;

{$I Souffle.inc}

interface

const
  SOUFFLE_FORMAT_VERSION = 1;
  SOUFFLE_BINARY_MAGIC: array[0..3] of Byte = (Ord('S'), Ord('B'), Ord('C'), 0);

  OP_RT_FIRST = 128;

type
  { Tier 1: Core operations (VM-intrinsic, fixed semantics) — opcodes 0..127 }
  { Tier 2: Runtime operations (pluggable semantics) — opcodes 128..255 }
  TSouffleOpCode = (
    // ── Tier 1: Load / Store / Move ──
    OP_LOAD_CONST    = 0,   // ABx   R[A] := Constants[Bx]
    OP_LOAD_NIL      = 1,   // A     R[A] := Nil
    OP_LOAD_TRUE     = 2,   // A     R[A] := Boolean(true)
    OP_LOAD_FALSE    = 3,   // A     R[A] := Boolean(false)
    OP_LOAD_INT      = 4,   // AsBx  R[A] := Integer(sBx)
    OP_MOVE          = 5,   // AB    R[A] := R[B]

    // ── Tier 1: Variables ──
    OP_GET_LOCAL     = 6,   // ABx   R[A] := Locals[Bx]
    OP_SET_LOCAL     = 7,   // ABx   Locals[Bx] := R[A]
    OP_GET_UPVALUE   = 8,   // ABx   R[A] := Upvalues[Bx]
    OP_SET_UPVALUE   = 9,   // ABx   Upvalues[Bx] := R[A]
    OP_CLOSE_UPVALUE = 10,  // A     Close upvalue for R[A]

    // ── Tier 1: Control Flow ──
    OP_JUMP          = 11,  // Ax    PC += Ax (signed 24-bit)
    OP_JUMP_IF_TRUE  = 12,  // AsBx  if IsTrue(R[A]): PC += sBx
    OP_JUMP_IF_FALSE = 13,  // AsBx  if not IsTrue(R[A]): PC += sBx
    OP_JUMP_IF_NIL   = 14,  // AsBx  if R[A].Kind = svkNil: PC += sBx
    OP_JUMP_IF_NOT_NIL = 15, // AsBx if R[A].Kind <> svkNil: PC += sBx

    // ── Tier 1: Closures ──
    OP_CLOSURE       = 16,  // ABx   R[A] := Closure(FunctionPrototypes[Bx])

    // ── Tier 1: Exception Handling ──
    OP_PUSH_HANDLER  = 17,  // ABx   Push handler: catch at PC+Bx, value in R[A]
    OP_POP_HANDLER   = 18,  //       Pop innermost handler
    OP_THROW         = 19,  // A     Throw R[A]

    // ── Tier 1: Return ──
    OP_RETURN        = 20,  // A     Return R[A] to caller
    OP_RETURN_NIL    = 21,  //       Return Nil to caller

    // ── Tier 1: Debug ──
    OP_NOP           = 22,  //       No operation
    OP_LINE          = 23,  // Bx    Source line annotation

    // ── Tier 2: Polymorphic Arithmetic ──
    OP_RT_ADD        = 128, // ABC   R[A] := Runtime.Add(R[B], R[C])
    OP_RT_SUB        = 129, // ABC   R[A] := Runtime.Subtract(R[B], R[C])
    OP_RT_MUL        = 130, // ABC   R[A] := Runtime.Multiply(R[B], R[C])
    OP_RT_DIV        = 131, // ABC   R[A] := Runtime.Divide(R[B], R[C])
    OP_RT_MOD        = 132, // ABC   R[A] := Runtime.Modulo(R[B], R[C])
    OP_RT_POW        = 133, // ABC   R[A] := Runtime.Power(R[B], R[C])
    OP_RT_NEG        = 134, // AB    R[A] := Runtime.Negate(R[B])

    // ── Tier 2: Polymorphic Bitwise ──
    OP_RT_BAND       = 135, // ABC   R[A] := Runtime.BitwiseAnd(R[B], R[C])
    OP_RT_BOR        = 136, // ABC   R[A] := Runtime.BitwiseOr(R[B], R[C])
    OP_RT_BXOR       = 137, // ABC   R[A] := Runtime.BitwiseXor(R[B], R[C])
    OP_RT_SHL        = 138, // ABC   R[A] := Runtime.ShiftLeft(R[B], R[C])
    OP_RT_SHR        = 139, // ABC   R[A] := Runtime.ShiftRight(R[B], R[C])
    OP_RT_USHR       = 140, // ABC   R[A] := Runtime.UnsignedShiftRight(R[B], R[C])
    OP_RT_BNOT       = 141, // AB    R[A] := Runtime.BitwiseNot(R[B])

    // ── Tier 2: Polymorphic Comparison ──
    OP_RT_EQ         = 142, // ABC   R[A] := Runtime.Equal(R[B], R[C])
    OP_RT_NEQ        = 143, // ABC   R[A] := Runtime.NotEqual(R[B], R[C])
    OP_RT_LT         = 144, // ABC   R[A] := Runtime.LessThan(R[B], R[C])
    OP_RT_GT         = 145, // ABC   R[A] := Runtime.GreaterThan(R[B], R[C])
    OP_RT_LTE        = 146, // ABC   R[A] := Runtime.LessThanOrEqual(R[B], R[C])
    OP_RT_GTE        = 147, // ABC   R[A] := Runtime.GreaterThanOrEqual(R[B], R[C])

    // ── Tier 2: Logical / Type ──
    OP_RT_NOT        = 148, // AB    R[A] := Runtime.LogicalNot(R[B])
    OP_RT_TYPEOF     = 149, // AB    R[A] := Runtime.TypeOf(R[B])
    OP_RT_IS_INSTANCE = 150, // ABC  R[A] := Runtime.IsInstance(R[B], R[C])
    OP_RT_HAS_PROPERTY = 151, // ABC R[A] := Runtime.HasProperty(R[B], R[C])
    OP_RT_TO_BOOLEAN = 152, // AB    R[A] := Runtime.ToBoolean(R[B])

    // ── Tier 2: Compound Creation ──
    OP_RT_NEW_COMPOUND = 153, // AB  R[A] := Runtime.CreateCompound(B)
    OP_RT_INIT_FIELD = 154, // ABC   Runtime.InitField(R[A], Constants[B], R[C])
    OP_RT_INIT_INDEX = 155, // ABC   Runtime.InitIndex(R[A], R[B], R[C])

    // ── Tier 2: Property Access ──
    OP_RT_GET_PROP   = 156, // ABC   R[A] := Runtime.GetProperty(R[B], Constants[C])
    OP_RT_SET_PROP   = 157, // ABC   Runtime.SetProperty(R[A], Constants[B], R[C])
    OP_RT_GET_INDEX  = 158, // ABC   R[A] := Runtime.GetIndex(R[B], R[C])
    OP_RT_SET_INDEX  = 159, // ABC   Runtime.SetIndex(R[A], R[B], R[C])
    OP_RT_DEL_PROP   = 160, // ABC   R[A] := Runtime.DeleteProperty(R[B], Constants[C])

    // ── Tier 2: Invocation ──
    OP_RT_CALL       = 161, // ABC   R[A] := Runtime.Invoke(R[A], args, B=argc, C=flags)
    OP_RT_CALL_METHOD = 162, // ABC  Like RT_CALL but receiver = R[A-1]
    OP_RT_CONSTRUCT  = 163, // ABC   R[A] := Runtime.Construct(R[B], args, C=argc)

    // ── Tier 2: Iteration ──
    OP_RT_GET_ITER   = 164, // AB    R[A] := Runtime.GetIterator(R[B])
    OP_RT_ITER_NEXT  = 165, // ABC   (R[A], R[B]) := Runtime.IteratorNext(R[C])
    OP_RT_SPREAD     = 166, // AB    Runtime.SpreadInto(R[A], R[B])

    // ── Tier 2: Modules ──
    OP_RT_IMPORT     = 167, // ABx   R[A] := Runtime.ImportModule(Constants[Bx])
    OP_RT_EXPORT     = 168, // ABx   Runtime.ExportBinding(R[A], Constants[Bx])

    // ── Tier 2: Async ──
    OP_RT_AWAIT      = 169, // AB    R[A] := Runtime.Await(R[B])

    // ── Tier 2: Globals ──
    OP_RT_GET_GLOBAL = 170, // ABx   R[A] := Runtime.GetGlobal(Constants[Bx])
    OP_RT_SET_GLOBAL = 171  // ABx   Runtime.SetGlobal(Constants[Bx], R[A])
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
