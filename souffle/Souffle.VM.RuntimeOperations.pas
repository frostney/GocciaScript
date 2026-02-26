unit Souffle.VM.RuntimeOperations;

{$I Souffle.inc}

interface

uses
  Souffle.Value;

type
  TSouffleRuntimeOperations = class abstract
  public
    // Arithmetic
    function Add(const A, B: TSouffleValue): TSouffleValue; virtual; abstract;
    function Subtract(const A, B: TSouffleValue): TSouffleValue; virtual; abstract;
    function Multiply(const A, B: TSouffleValue): TSouffleValue; virtual; abstract;
    function Divide(const A, B: TSouffleValue): TSouffleValue; virtual; abstract;
    function Modulo(const A, B: TSouffleValue): TSouffleValue; virtual; abstract;
    function Power(const A, B: TSouffleValue): TSouffleValue; virtual; abstract;
    function Negate(const A: TSouffleValue): TSouffleValue; virtual; abstract;

    // Bitwise
    function BitwiseAnd(const A, B: TSouffleValue): TSouffleValue; virtual; abstract;
    function BitwiseOr(const A, B: TSouffleValue): TSouffleValue; virtual; abstract;
    function BitwiseXor(const A, B: TSouffleValue): TSouffleValue; virtual; abstract;
    function ShiftLeft(const A, B: TSouffleValue): TSouffleValue; virtual; abstract;
    function ShiftRight(const A, B: TSouffleValue): TSouffleValue; virtual; abstract;
    function UnsignedShiftRight(const A, B: TSouffleValue): TSouffleValue; virtual; abstract;
    function BitwiseNot(const A: TSouffleValue): TSouffleValue; virtual; abstract;

    // Comparison
    function Equal(const A, B: TSouffleValue): TSouffleValue; virtual; abstract;
    function NotEqual(const A, B: TSouffleValue): TSouffleValue; virtual; abstract;
    function LessThan(const A, B: TSouffleValue): TSouffleValue; virtual; abstract;
    function GreaterThan(const A, B: TSouffleValue): TSouffleValue; virtual; abstract;
    function LessThanOrEqual(const A, B: TSouffleValue): TSouffleValue; virtual; abstract;
    function GreaterThanOrEqual(const A, B: TSouffleValue): TSouffleValue; virtual; abstract;

    // Logical / Type
    function LogicalNot(const A: TSouffleValue): TSouffleValue; virtual; abstract;
    function TypeOf(const A: TSouffleValue): TSouffleValue; virtual; abstract;
    function IsInstance(const A, B: TSouffleValue): TSouffleValue; virtual; abstract;
    function HasProperty(const AObject, AKey: TSouffleValue): TSouffleValue; virtual; abstract;
    function ToBoolean(const A: TSouffleValue): TSouffleValue; virtual; abstract;

    // Compound creation
    function CreateCompound(const ATypeTag: UInt8): TSouffleValue; virtual; abstract;
    procedure InitField(const ACompound: TSouffleValue; const AKey: string;
      const AValue: TSouffleValue); virtual; abstract;
    procedure InitIndex(const ACompound: TSouffleValue; const AIndex: TSouffleValue;
      const AValue: TSouffleValue); virtual; abstract;

    // Property access
    function GetProperty(const AObject: TSouffleValue;
      const AKey: string): TSouffleValue; virtual; abstract;
    procedure SetProperty(const AObject: TSouffleValue; const AKey: string;
      const AValue: TSouffleValue); virtual; abstract;
    function GetIndex(const AObject, AKey: TSouffleValue): TSouffleValue; virtual; abstract;
    procedure SetIndex(const AObject: TSouffleValue;
      const AKey, AValue: TSouffleValue); virtual; abstract;
    function DeleteProperty(const AObject: TSouffleValue;
      const AKey: string): TSouffleValue; virtual; abstract;

    // Invocation
    function Invoke(const ACallee: TSouffleValue; const AArgs: PSouffleValue;
      const AArgCount: Integer; const AReceiver: TSouffleValue): TSouffleValue; virtual; abstract;
    function Construct(const AConstructor: TSouffleValue; const AArgs: PSouffleValue;
      const AArgCount: Integer): TSouffleValue; virtual; abstract;

    // Iteration
    function GetIterator(const AIterable: TSouffleValue): TSouffleValue; virtual; abstract;
    function IteratorNext(const AIterator: TSouffleValue;
      out ADone: Boolean): TSouffleValue; virtual; abstract;
    procedure SpreadInto(const ATarget, ASource: TSouffleValue); virtual; abstract;

    // Modules
    function ImportModule(const APath: string): TSouffleValue; virtual; abstract;
    procedure ExportBinding(const AValue: TSouffleValue;
      const AName: string); virtual; abstract;

    // Async
    function AwaitValue(const AValue: TSouffleValue): TSouffleValue; virtual; abstract;

    // Globals
    function GetGlobal(const AName: string): TSouffleValue; virtual; abstract;
    procedure SetGlobal(const AName: string;
      const AValue: TSouffleValue); virtual; abstract;

  end;

implementation

end.
