unit Souffle.VM.RuntimeOperations;

{$I Souffle.inc}

interface

uses
  Souffle.Bytecode.Chunk,
  Souffle.Value;

type
  TSouffleRuntimeOperations = class abstract
  public
    // Arithmetic (7)
    function Add(const A, B: TSouffleValue): TSouffleValue; virtual; abstract;
    function Subtract(const A, B: TSouffleValue): TSouffleValue; virtual; abstract;
    function Multiply(const A, B: TSouffleValue): TSouffleValue; virtual; abstract;
    function Divide(const A, B: TSouffleValue): TSouffleValue; virtual; abstract;
    function Modulo(const A, B: TSouffleValue): TSouffleValue; virtual; abstract;
    function Power(const A, B: TSouffleValue): TSouffleValue; virtual; abstract;
    function Negate(const A: TSouffleValue): TSouffleValue; virtual; abstract;

    // Bitwise (7)
    function BitwiseAnd(const A, B: TSouffleValue): TSouffleValue; virtual; abstract;
    function BitwiseOr(const A, B: TSouffleValue): TSouffleValue; virtual; abstract;
    function BitwiseXor(const A, B: TSouffleValue): TSouffleValue; virtual; abstract;
    function ShiftLeft(const A, B: TSouffleValue): TSouffleValue; virtual; abstract;
    function ShiftRight(const A, B: TSouffleValue): TSouffleValue; virtual; abstract;
    function UnsignedShiftRight(const A, B: TSouffleValue): TSouffleValue; virtual; abstract;
    function BitwiseNot(const A: TSouffleValue): TSouffleValue; virtual; abstract;

    // Comparison (6)
    function Equal(const A, B: TSouffleValue): TSouffleValue; virtual; abstract;
    function NotEqual(const A, B: TSouffleValue): TSouffleValue; virtual; abstract;
    function LessThan(const A, B: TSouffleValue): TSouffleValue; virtual; abstract;
    function GreaterThan(const A, B: TSouffleValue): TSouffleValue; virtual; abstract;
    function LessThanOrEqual(const A, B: TSouffleValue): TSouffleValue; virtual; abstract;
    function GreaterThanOrEqual(const A, B: TSouffleValue): TSouffleValue; virtual; abstract;

    // Logic / Type (6)
    function LogicalNot(const A: TSouffleValue): TSouffleValue; virtual; abstract;
    function TypeOf(const A: TSouffleValue): TSouffleValue; virtual; abstract;
    function IsInstance(const A, B: TSouffleValue): TSouffleValue; virtual; abstract;
    function HasProperty(const AObject, AKey: TSouffleValue): TSouffleValue; virtual; abstract;
    function ToBoolean(const A: TSouffleValue): TSouffleValue; virtual; abstract;
    function ToPrimitive(const A: TSouffleValue): TSouffleValue; virtual; abstract;

    // Property access (6)
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

    // Invocation (2)
    function Invoke(const ACallee: TSouffleValue; const AArgs: PSouffleValue;
      const AArgCount: Integer; const AReceiver: TSouffleValue): TSouffleValue; virtual; abstract;
    function Construct(const AConstructor: TSouffleValue; const AArgs: PSouffleValue;
      const AArgCount: Integer): TSouffleValue; virtual; abstract;

    // Globals (3)
    function GetGlobal(const AName: string): TSouffleValue; virtual; abstract;
    procedure SetGlobal(const AName: string;
      const AValue: TSouffleValue); virtual; abstract;
    function HasGlobal(const AName: string): Boolean; virtual; abstract;

    // Iteration (2)
    function GetIterator(const AIterable: TSouffleValue;
      const ATryAsync: Boolean = False): TSouffleValue; virtual; abstract;
    function IteratorNext(const AIterator: TSouffleValue;
      out ADone: Boolean): TSouffleValue; virtual; abstract;

    // Modules (2)
    function ImportModule(const APath: string): TSouffleValue; virtual; abstract;
    procedure ExportBinding(const AValue: TSouffleValue;
      const AName: string); virtual; abstract;

    // Async (2)
    function AwaitValue(const AValue: TSouffleValue): TSouffleValue; virtual; abstract;
    function WrapInPromise(const AValue: TSouffleValue;
      const AIsRejected: Boolean): TSouffleValue; virtual;

    // Coercion (1)
    function CoerceValueToString(const A: TSouffleValue): TSouffleValue; virtual;

    // Language-specific extension dispatch (1)
    procedure ExtendedOperation(const ASubOp: UInt8;
      var ADest: TSouffleValue; const AOperand, AExtra: TSouffleValue;
      const ATemplate: TSouffleFunctionTemplate;
      const AOperandIndex: UInt8); virtual;

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

end.
