unit Souffle.VM.RuntimeOperations;

{$I Souffle.inc}

interface

uses
  Souffle.Bytecode.Chunk,
  Souffle.Value;

type
  { TSouffleRuntimeOperations — abstract contract between the VM and a
    language frontend. Every method operates exclusively on TSouffleValue;
    frontends must NOT expose their own value systems through this interface.

    Grouped by domain (45 abstract + 4 virtual with defaults). A new
    language frontend implements this class and passes it to the VM. }

  TSouffleRuntimeOperations = class abstract
  public

    { ── Arithmetic (7) ──
      Operands may be any TSouffleValue kind. Implementations must coerce
      non-numeric operands to numbers following the language's rules (e.g.
      string-to-number, boolean-to-number). Add typically also handles
      string concatenation when either operand is a string. }

    function Add(const A, B: TSouffleValue): TSouffleValue; virtual; abstract;
    function Subtract(const A, B: TSouffleValue): TSouffleValue; virtual; abstract;
    function Multiply(const A, B: TSouffleValue): TSouffleValue; virtual; abstract;
    function Divide(const A, B: TSouffleValue): TSouffleValue; virtual; abstract;
    function Modulo(const A, B: TSouffleValue): TSouffleValue; virtual; abstract;
    function Power(const A, B: TSouffleValue): TSouffleValue; virtual; abstract;
    function Negate(const A: TSouffleValue): TSouffleValue; virtual; abstract;

    { ── Bitwise (7) ──
      Operands are coerced to 32-bit integers before the operation.
      UnsignedShiftRight returns a float/unsigned result. }

    function BitwiseAnd(const A, B: TSouffleValue): TSouffleValue; virtual; abstract;
    function BitwiseOr(const A, B: TSouffleValue): TSouffleValue; virtual; abstract;
    function BitwiseXor(const A, B: TSouffleValue): TSouffleValue; virtual; abstract;
    function ShiftLeft(const A, B: TSouffleValue): TSouffleValue; virtual; abstract;
    function ShiftRight(const A, B: TSouffleValue): TSouffleValue; virtual; abstract;
    function UnsignedShiftRight(const A, B: TSouffleValue): TSouffleValue; virtual; abstract;
    function BitwiseNot(const A: TSouffleValue): TSouffleValue; virtual; abstract;

    { ── Comparison (6) ──
      Strict equality: no implicit type coercion — values of different
      kinds are never equal. Relational operators coerce operands as
      needed (string comparison when both are strings, numeric otherwise).
      All return SouffleBoolean. }

    function Equal(const A, B: TSouffleValue): TSouffleValue; virtual; abstract;
    function NotEqual(const A, B: TSouffleValue): TSouffleValue; virtual; abstract;
    function LessThan(const A, B: TSouffleValue): TSouffleValue; virtual; abstract;
    function GreaterThan(const A, B: TSouffleValue): TSouffleValue; virtual; abstract;
    function LessThanOrEqual(const A, B: TSouffleValue): TSouffleValue; virtual; abstract;
    function GreaterThanOrEqual(const A, B: TSouffleValue): TSouffleValue; virtual; abstract;

    { ── Logic / Type (6) ──
      LogicalNot: returns the boolean negation of the truthiness of A.
      TypeOf: returns a SouffleString with the type name.
      IsInstance: returns SouffleBoolean — true when A is an instance of B.
      HasProperty: returns SouffleBoolean — true when AKey exists on AObject.
      ToBoolean: coerces A to a SouffleBoolean (truthiness).
      ToPrimitive: converts reference types to a primitive value. }

    function LogicalNot(const A: TSouffleValue): TSouffleValue; virtual; abstract;
    function TypeOf(const A: TSouffleValue): TSouffleValue; virtual; abstract;
    function IsInstance(const A, B: TSouffleValue): TSouffleValue; virtual; abstract;
    function HasProperty(const AObject, AKey: TSouffleValue): TSouffleValue; virtual; abstract;
    function ToBoolean(const A: TSouffleValue): TSouffleValue; virtual; abstract;
    function ToPrimitive(const A: TSouffleValue): TSouffleValue; virtual; abstract;

    { ── Property access (6) ──
      GetProperty/SetProperty: named property access on any value.
        Implementations handle prototype chain walking, auto-boxing
        of primitives, and delegate chain lookup.
      GetIndex/SetIndex: computed property access where AKey is a
        TSouffleValue (integer index, string key, or symbol key).
      DeleteProperty/DeleteIndex: remove a property. Returns
        SouffleBoolean(True) on success. }

    function GetProperty(const AObject: TSouffleValue;
      const AKey: string): TSouffleValue; virtual; abstract;
    procedure SetProperty(const AObject: TSouffleValue; const AKey: string;
      const AValue: TSouffleValue); virtual; abstract;
    function GetIndex(const AObject, AKey: TSouffleValue): TSouffleValue; virtual; abstract;
    procedure SetIndex(const AObject: TSouffleValue;
      const AKey, AValue: TSouffleValue); virtual; abstract;
    function DeleteProperty(const AObject: TSouffleValue;
      const AKey: string): TSouffleValue; virtual; abstract;
    function DeleteIndex(const AObject, AKey: TSouffleValue): TSouffleValue; virtual;

    { ── Invocation (2) ──
      Invoke: call ACallee with AArgs (AArgCount entries starting at
        AArgs^). AReceiver is the `this` binding (SouffleNil for bare
        calls). Returns the function's return value.
      Construct: call AConstructor as a constructor (`new`). Returns
        the newly created instance. }

    function Invoke(const ACallee: TSouffleValue; const AArgs: PSouffleValue;
      const AArgCount: Integer; const AReceiver: TSouffleValue): TSouffleValue; virtual; abstract;
    function Construct(const AConstructor: TSouffleValue; const AArgs: PSouffleValue;
      const AArgCount: Integer): TSouffleValue; virtual; abstract;

    { ── Globals (3) ──
      Language-level global variable access. The VM calls these for
      OP_GET_GLOBAL / OP_SET_GLOBAL / OP_HAS_GLOBAL. Implementations
      manage const enforcement and undefined-variable errors. }

    function GetGlobal(const AName: string): TSouffleValue; virtual; abstract;
    procedure SetGlobal(const AName: string;
      const AValue: TSouffleValue); virtual; abstract;
    function HasGlobal(const AName: string): Boolean; virtual; abstract;

    { ── Iteration (2) ──
      GetIterator: obtain an iterator from AIterable. ATryAsync hints
        that an async iterator is preferred (for `for await`).
      IteratorNext: advance the iterator, returning the next value.
        Sets ADone to True when the iterator is exhausted. }

    function GetIterator(const AIterable: TSouffleValue;
      const ATryAsync: Boolean = False): TSouffleValue; virtual; abstract;
    function IteratorNext(const AIterator: TSouffleValue;
      out ADone: Boolean): TSouffleValue; virtual; abstract;

    { ── Modules (2) ──
      ImportModule: load and return the module namespace for APath.
      ExportBinding: register AValue as a named export. }

    function ImportModule(const APath: string): TSouffleValue; virtual; abstract;
    procedure ExportBinding(const AValue: TSouffleValue;
      const AName: string); virtual; abstract;

    { ── Async (2) ──
      AwaitValue: suspend until AValue resolves (for `await`).
      WrapInPromise: wrap AValue in a promise. Default returns AValue
        unchanged (for languages without promises). }

    function AwaitValue(const AValue: TSouffleValue): TSouffleValue; virtual; abstract;
    function WrapInPromise(const AValue: TSouffleValue;
      const AIsRejected: Boolean): TSouffleValue; virtual;

    { ── Coercion (1) ──
      CoerceValueToString: convert A to its string representation for
        OP_RT_TO_STRING. Default returns SouffleNil (caller falls back
        to SouffleValueToString). }

    function CoerceValueToString(const A: TSouffleValue): TSouffleValue; virtual;

    { ── Extension dispatch (1) ──
      Language-specific operations dispatched by OP_RT_EXT. ASubOp is
      the sub-opcode ID defined by the frontend. Default is a no-op.
      Frontends define their own sub-opcode constants and handle them
      in their override. }

    procedure ExtendedOperation(const ASubOp: UInt8;
      var ADest: TSouffleValue; const AOperand, AExtra: TSouffleValue;
      const ATemplate: TSouffleFunctionTemplate;
      const AOperandIndex: UInt8); virtual;

    { ── Type enforcement (1) ──
      Called by the VM when a strictly-typed local SET fails the type
      check. AValue is the value being stored, AExpectedType is the
      declared type. Default is a no-op (type mismatch silently
      allowed). Frontends override to throw a type error. }

    procedure CheckLocalType(const AValue: TSouffleValue;
      const AExpectedType: TSouffleLocalType); virtual;

    { ── GC coordination (1) ──
      Called by the VM's external root marker during Souffle GC mark
      phase. Frontends that cache Souffle heap objects outside the VM
      (e.g. bridge caches, global tables) must override this to mark
      those objects so the Souffle GC does not collect them. Default
      is a no-op. }

    procedure MarkExternalRoots; virtual;

  end;

implementation

function TSouffleRuntimeOperations.DeleteIndex(
  const AObject, AKey: TSouffleValue): TSouffleValue;
begin
  Result := DeleteProperty(AObject, SouffleValueToString(AKey));
end;

function TSouffleRuntimeOperations.CoerceValueToString(
  const A: TSouffleValue): TSouffleValue;
begin
  Result := SouffleNil;
end;

function TSouffleRuntimeOperations.WrapInPromise(const AValue: TSouffleValue;
  const AIsRejected: Boolean): TSouffleValue;
begin
  Result := AValue;
end;

procedure TSouffleRuntimeOperations.ExtendedOperation(const ASubOp: UInt8;
  var ADest: TSouffleValue; const AOperand, AExtra: TSouffleValue;
  const ATemplate: TSouffleFunctionTemplate;
  const AOperandIndex: UInt8);
begin
end;

procedure TSouffleRuntimeOperations.CheckLocalType(
  const AValue: TSouffleValue; const AExpectedType: TSouffleLocalType);
begin
end;

procedure TSouffleRuntimeOperations.MarkExternalRoots;
begin
end;

end.
