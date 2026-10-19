unit Goccia.Builtins.Atomics;

{$I Goccia.inc}

interface

uses
  CriticalSections,

  Goccia.Arguments.Collection,
  Goccia.Builtins.Base,
  Goccia.Error.ThrowErrorCallback,
  Goccia.ObjectModel,
  Goccia.Scope,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives,
  Goccia.Values.PromiseValue;

function PumpAtomicsWaitAsyncCompletions: Integer;
function HasPendingAtomicsWaitAsyncCompletions: Boolean;
function WaitForAtomicsPromise(const APromise: TGocciaPromiseValue): Boolean;
procedure SetAtomicsAgentCanSuspend(AValue: Boolean);
procedure ShutdownAtomicsWaiters;
procedure ShutdownAtomicsWaitersForCurrentThread;

type
  TAtomicsWaitSuspensionEndedHook = procedure;

{ Test seam: AHook runs on the waiting agent after an Atomics.wait suspension
  ends and before the wait enters the waiter list's critical section again,
  so a test can order an Atomics.notify against a timeout. It applies to the
  calling thread's agent only. Nil outside tests. }
procedure SetAtomicsWaitSuspensionEndedHookForTesting(
  const AHook: TAtomicsWaitSuspensionEndedHook);

type
  TGocciaAtomics = class(TGocciaBuiltin)
  public
    constructor Create(const AName: string; AScope: TGocciaScope;
      AThrowError: TGocciaThrowErrorCallback;
      const ADefineGlobalBinding: Boolean = True);
    destructor Destroy; override;
  published
    function AtomicsAdd(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function AtomicsAnd(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function AtomicsCompareExchange(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function AtomicsExchange(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function AtomicsIsLockFree(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function AtomicsLoad(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function AtomicsNotify(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function AtomicsOr(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function AtomicsPause(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function AtomicsStore(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function AtomicsSub(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function AtomicsWait(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function AtomicsWaitAsync(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function AtomicsXor(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
  end;

{ True while AOwner (one engine's Atomics) has an async waiter on this thread
  that settles without an Atomics.notify: one already notified, or one with a
  timeout. A waiter with no timeout that nothing notifies never settles. An
  engine nested on the thread does not wait out its caller's waiters. }
function HasSelfSettlingAtomicsWaitAsyncWaiters(
  const AOwner: TGocciaAtomics): Boolean;

implementation

uses
  Classes,
  Generics.Collections,
  Math,
  SyncObjs,
  SysUtils,

  BigInteger,
  TimingUtils,

  Goccia.BinaryData,
  Goccia.Constants.ConstructorNames,
  Goccia.Constants.NumericLimits,
  Goccia.Error.Messages,
  Goccia.GarbageCollector,
  Goccia.InstructionLimit,
  Goccia.ThreadCleanupRegistry,
  Goccia.Timeout,
  Goccia.Values.ArrayBufferValue,
  Goccia.Values.BigIntValue,
  Goccia.Values.ErrorHelper,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.SharedArrayBufferValue,
  Goccia.Values.SymbolValue,
  Goccia.Values.ToPrimitive,
  Goccia.Values.TypedArrayValue;

const
  ATOMICS_LITTLE_ENDIAN = True;
  ATOMICS_WAIT_OK = 'ok';
  ATOMICS_WAIT_NOT_EQUAL = 'not-equal';
  ATOMICS_WAIT_TIMED_OUT = 'timed-out';

type
  TGocciaAtomicsOperation = (aoAdd, aoAnd, aoExchange, aoOr, aoSub, aoXor);

  TAtomicsWaiter = class
  private
    FAsync: Boolean;
    FBuffer: TGocciaSharedArrayBufferValue;
    FByteOffset: Integer;
    FDeadlineMilliseconds: UInt64;
    FEvent: TEvent;
    FHasDeadline: Boolean;
    FInList: Boolean;
    FOwner: TGocciaAtomics;
    FOwnerThreadId: TThreadID;
    FPromise: TGocciaPromiseValue;
    FReady: Boolean;
    FResult: string;
    procedure SetInList(const AValue: Boolean);
  public
    constructor Create(AOwner: TGocciaAtomics;
      ABuffer: TGocciaSharedArrayBufferValue; AByteOffset: Integer;
      AAsync: Boolean; APromise: TGocciaPromiseValue;
      ADeadlineMilliseconds: UInt64 = 0; AHasDeadline: Boolean = False);
    destructor Destroy; override;

    property Async: Boolean read FAsync;
    property Buffer: TGocciaSharedArrayBufferValue read FBuffer;
    property ByteOffset: Integer read FByteOffset;
    property DeadlineMilliseconds: UInt64 read FDeadlineMilliseconds;
    property Event: TEvent read FEvent;
    property HasDeadline: Boolean read FHasDeadline;
    property InList: Boolean read FInList write SetInList;
    property Owner: TGocciaAtomics read FOwner;
    property OwnerThreadId: TThreadID read FOwnerThreadId;
    property Promise: TGocciaPromiseValue read FPromise;
    property Ready: Boolean read FReady write FReady;
    property ResultText: string read FResult write FResult;
  end;

var
  GAtomicsLock: TGocciaCriticalSection;
  GAtomicsWaiters: TObjectList<TAtomicsWaiter>;
  // How many Atomics.waitAsync waiters GAtomicsWaiters holds, across every
  // thread. A thread's own waiters are added by that thread, so a read of 0
  // here proves it has none to complete, and the per-job completion pump
  // returns before taking the lock.
  GAtomicsAsyncWaiterCount: Integer;

threadvar
  // Per thread, so a hook a test installs runs only for its own agent.
  GAtomicsWaitSuspensionEndedHook: TAtomicsWaitSuspensionEndedHook;
  GAtomicsAgentCanSuspend: Boolean;
  GAtomicsAgentCanSuspendConfigured: Boolean;

function AtomicsAgentCanSuspend: Boolean;
begin
  if not GAtomicsAgentCanSuspendConfigured then
    Exit(True);
  Result := GAtomicsAgentCanSuspend;
end;

procedure SetAtomicsAgentCanSuspend(AValue: Boolean);
begin
  GAtomicsAgentCanSuspend := AValue;
  GAtomicsAgentCanSuspendConfigured := True;
end;

procedure SetAtomicsWaitSuspensionEndedHookForTesting(
  const AHook: TAtomicsWaitSuspensionEndedHook);
begin
  GAtomicsWaitSuspensionEndedHook := AHook;
end;

function GetAtomicsWaiters: TObjectList<TAtomicsWaiter>;
begin
  if not Assigned(GAtomicsWaiters) then
    GAtomicsWaiters := TObjectList<TAtomicsWaiter>.Create(False);
  Result := GAtomicsWaiters;
end;

constructor TAtomicsWaiter.Create(AOwner: TGocciaAtomics;
  ABuffer: TGocciaSharedArrayBufferValue; AByteOffset: Integer; AAsync: Boolean;
  APromise: TGocciaPromiseValue; ADeadlineMilliseconds: UInt64;
  AHasDeadline: Boolean);
begin
  inherited Create;
  FOwner := AOwner;
  FOwnerThreadId := GetGocciaThreadId;
  FAsync := AAsync;
  FBuffer := ABuffer;
  FByteOffset := AByteOffset;
  FDeadlineMilliseconds := ADeadlineMilliseconds;
  FPromise := APromise;
  FReady := False;
  FResult := ATOMICS_WAIT_OK;
  FHasDeadline := AHasDeadline;
  FEvent := TEvent.Create(nil, True, False, '');
  FInList := False;

  if FAsync then
  begin
    TGarbageCollector.Instance.AddQueuedRoot(FBuffer);
    TGarbageCollector.Instance.AddQueuedRoot(FPromise);
  end;
end;

procedure TAtomicsWaiter.SetInList(const AValue: Boolean);
begin
  if FAsync and (AValue <> FInList) then
  begin
    if AValue then
      AtomicIncrementInt32(GAtomicsAsyncWaiterCount)
    else
      AtomicDecrementInt32(GAtomicsAsyncWaiterCount);
  end;
  FInList := AValue;
end;

destructor TAtomicsWaiter.Destroy;
begin
  if FAsync then
  begin
    TGarbageCollector.Instance.RemoveQueuedRoot(FPromise);
    TGarbageCollector.Instance.RemoveQueuedRoot(FBuffer);
  end;
  FEvent.Free;
  inherited Destroy;
end;

function AtomicsErrorMessage(const AMethodName, AMessage: string): string;
begin
  Result := Format('Atomics.%s: %s', [AMethodName, AMessage]);
end;

function AtomicsSharedArrayBufferErrorMessage(const AMethodName: string): string;
begin
  Result := Format(SErrorRequiresSharedArrayBuffer, ['Atomics.' + AMethodName]);
end;

function GetArgumentOrUndefined(const AArgs: TGocciaArgumentsCollection;
  AIndex: Integer): TGocciaValue;
begin
  if AIndex < AArgs.Length then
    Result := AArgs.GetElement(AIndex)
  else
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function BinaryKindForTypedArray(AKind: TGocciaTypedArrayKind):
  TGocciaBinaryElementKind;
begin
  case AKind of
    takInt8:
      Result := bekInt8;
    takUint8:
      Result := bekUint8;
    takUint8Clamped:
      Result := bekUint8Clamped;
    takInt16:
      Result := bekInt16;
    takUint16:
      Result := bekUint16;
    takInt32:
      Result := bekInt32;
    takUint32:
      Result := bekUint32;
    takBigInt64:
      Result := bekBigInt64;
    takBigUint64:
      Result := bekBigUint64;
    takFloat16:
      Result := bekFloat16;
    takFloat32:
      Result := bekFloat32;
    takFloat64:
      Result := bekFloat64;
  else
    Result := bekUint8;
  end;
end;

function IsAtomicIntegerKind(AKind: TGocciaTypedArrayKind): Boolean;
begin
  Result := AKind in [takInt8, takUint8, takInt16, takUint16, takInt32,
    takUint32, takBigInt64, takBigUint64];
end;

function IsWaitableKind(AKind: TGocciaTypedArrayKind): Boolean;
begin
  Result := AKind in [takInt32, takBigInt64];
end;

function IsBigIntKind(AKind: TGocciaTypedArrayKind): Boolean;
begin
  Result := AKind in [takBigInt64, takBigUint64];
end;

function ToIndexForAtomics(const AValue: TGocciaValue;
  const AMethodName: string): Integer;
var
  IndexNumber: Double;
  IntegerIndex: Double;
begin
  IndexNumber := AValue.ToNumberLiteral.Value;
  if IsNan(IndexNumber) then
    IndexNumber := 0;

  if IsInfinite(IndexNumber) or
    (IndexNumber > MAX_SAFE_INTEGER_F) or (IndexNumber > High(Integer)) then
    ThrowRangeError(AtomicsErrorMessage(AMethodName,
      'index must be a non-negative integer'));

  IntegerIndex := Trunc(IndexNumber);
  if IntegerIndex < 0 then
    ThrowRangeError(AtomicsErrorMessage(AMethodName,
      'index must be a non-negative integer'));

  Result := Trunc(IntegerIndex);
end;

function ToTimeoutMilliseconds(const AValue: TGocciaValue): Int64;
var
  TimeoutNumber: Double;
begin
  if AValue is TGocciaUndefinedLiteralValue then
    Exit(-1);

  TimeoutNumber := AValue.ToNumberLiteral.Value;
  if IsNan(TimeoutNumber) or IsInfinite(TimeoutNumber) then
    Exit(-1);

  if TimeoutNumber <= 0 then
    Exit(0);

  if TimeoutNumber > High(Int64) then
    Exit(-1);

  Result := Trunc(TimeoutNumber);
end;

function ToWaiterCount(const AValue: TGocciaValue): Integer;
var
  CountNumber: Double;
begin
  if AValue is TGocciaUndefinedLiteralValue then
    Exit(-1);

  CountNumber := AValue.ToNumberLiteral.Value;
  if IsNan(CountNumber) or (CountNumber <= 0) then
    Exit(0);

  if IsInfinite(CountNumber) or (CountNumber > High(Integer)) then
    Exit(-1);

  Result := Trunc(CountNumber);
end;

// ES2026 §23.2.4.4 ValidateTypedArray(O, order): a detached view, or one past
// the end of its shrunk buffer (IsTypedArrayOutOfBounds), is a TypeError.
// Only a resizable ArrayBuffer can shrink under a view; a fixed-length one
// keeps its length until it is detached, and a SharedArrayBuffer only grows.
procedure EnsureAtomicTypedArrayInBounds(const ATypedArray: TGocciaTypedArrayValue;
  const AMethodName: string);
var
  Buffer: TGocciaArrayBufferValue;
begin
  if not (ATypedArray.BufferValue is TGocciaArrayBufferValue) then
    Exit;
  Buffer := TGocciaArrayBufferValue(ATypedArray.BufferValue);
  if Buffer.Detached then
    ThrowTypeError(Format(SErrorCannotUseDetachedTypedArray,
      ['Atomics.' + AMethodName]));
  if (Buffer.MaxByteLength >= 0) and IsTypedArrayOutOfBounds(ATypedArray) then
    ThrowTypeError(AtomicsErrorMessage(AMethodName,
      'typedArray is out of bounds of its ArrayBuffer'));
end;

procedure EnsureAtomicTypedArrayUsable(const ATypedArray: TGocciaTypedArrayValue;
  const AMethodName: string; AWriteAccess: Boolean);
begin
  // ES2026 §25.4.3.1 ValidateIntegerTypedArray(typedArray, waitable) step 1:
  // ? ValidateTypedArray(typedArray, unordered)
  EnsureAtomicTypedArrayInBounds(ATypedArray, AMethodName);
  if AWriteAccess and (ATypedArray.BufferValue is TGocciaArrayBufferValue) and
     TGocciaArrayBufferValue(ATypedArray.BufferValue).Immutable then
    ThrowTypeError(AtomicsErrorMessage(AMethodName,
      'cannot write to an immutable ArrayBuffer'));
end;

procedure ValidateAtomicTypedArrayAccess(const AArgs: TGocciaArgumentsCollection;
  const AMethodName: string; ARequireWaitable, AWriteAccess: Boolean;
  out ATypedArray: TGocciaTypedArrayValue; out AIndex, AByteOffset: Integer);
var
  ArrayLength: Integer;
  Bpe: Integer;
  IndexValue: TGocciaValue;
  TypedArrayValue: TGocciaValue;
begin
  TypedArrayValue := GetArgumentOrUndefined(AArgs, 0);
  if not (TypedArrayValue is TGocciaTypedArrayValue) then
    ThrowTypeError(AtomicsErrorMessage(AMethodName,
      'typedArray must be an integer TypedArray'));

  ATypedArray := TGocciaTypedArrayValue(TypedArrayValue);

  if not IsAtomicIntegerKind(ATypedArray.Kind) then
    ThrowTypeError(AtomicsErrorMessage(AMethodName,
      'typedArray must be an integer TypedArray'));

  if ARequireWaitable and not IsWaitableKind(ATypedArray.Kind) then
    ThrowTypeError(AtomicsErrorMessage(AMethodName,
      'typedArray must be an Int32Array or BigInt64Array'));

  EnsureAtomicTypedArrayUsable(ATypedArray, AMethodName, AWriteAccess);
  ArrayLength := ATypedArray.Length;
  IndexValue := GetArgumentOrUndefined(AArgs, 1);
  AIndex := ToIndexForAtomics(IndexValue, AMethodName);
  if AIndex >= ArrayLength then
    ThrowRangeError(AtomicsErrorMessage(AMethodName,
      SErrorInvalidTypedArrayIndex));

  Bpe := TGocciaTypedArrayValue.BytesPerElement(ATypedArray.Kind);
  AByteOffset := ATypedArray.ByteOffset + AIndex * Bpe;
end;

procedure ValidateAtomicAccess(const AArgs: TGocciaArgumentsCollection;
  const AMethodName: string; ARequireWaitable, AWriteAccess: Boolean;
  out ATypedArray: TGocciaTypedArrayValue;
  out ABuffer: TGocciaValue; out AIndex, AByteOffset: Integer);
begin
  ValidateAtomicTypedArrayAccess(AArgs, AMethodName, ARequireWaitable, AWriteAccess,
    ATypedArray, AIndex, AByteOffset);
  ABuffer := ATypedArray.BufferValue;
end;

procedure ValidateAtomicsWaitAccess(const AArgs: TGocciaArgumentsCollection;
  const AMethodName: string; out ATypedArray: TGocciaTypedArrayValue;
  out ABuffer: TGocciaSharedArrayBufferValue; out AIndex,
  AByteOffset: Integer);
var
  ArrayLength: Integer;
  Bpe: Integer;
  IndexValue: TGocciaValue;
  TypedArrayValue: TGocciaValue;
begin
  TypedArrayValue := GetArgumentOrUndefined(AArgs, 0);
  if not (TypedArrayValue is TGocciaTypedArrayValue) then
    ThrowTypeError(AtomicsErrorMessage(AMethodName,
      'typedArray must be an integer TypedArray'));

  ATypedArray := TGocciaTypedArrayValue(TypedArrayValue);

  if not IsAtomicIntegerKind(ATypedArray.Kind) then
    ThrowTypeError(AtomicsErrorMessage(AMethodName,
      'typedArray must be an integer TypedArray'));

  if not IsWaitableKind(ATypedArray.Kind) then
    ThrowTypeError(AtomicsErrorMessage(AMethodName,
      'typedArray must be an Int32Array or BigInt64Array'));

  if not (ATypedArray.BufferValue is TGocciaSharedArrayBufferValue) then
    ThrowTypeError(AtomicsSharedArrayBufferErrorMessage(AMethodName));

  ABuffer := TGocciaSharedArrayBufferValue(ATypedArray.BufferValue);
  ArrayLength := ATypedArray.Length;
  IndexValue := GetArgumentOrUndefined(AArgs, 1);
  AIndex := ToIndexForAtomics(IndexValue, AMethodName);
  if AIndex >= ArrayLength then
    ThrowRangeError(AtomicsErrorMessage(AMethodName,
      SErrorInvalidTypedArrayIndex));

  Bpe := TGocciaTypedArrayValue.BytesPerElement(ATypedArray.Kind);
  AByteOffset := ATypedArray.ByteOffset + AIndex * Bpe;
end;

// ES2026 §25.4.3.4 RevalidateAtomicAccess(typedArray, byteIndexInBuffer)
// The index was validated against the length measured before ToIndex and the
// value conversions, which run script code that can shrink or detach the
// buffer. Run after the last conversion and before the element is touched;
// it returns the buffer's current block, which the access reads and writes.
function RevalidateAtomicAccess(const ATypedArray: TGocciaTypedArrayValue;
  const AByteIndexInBuffer: Integer; const AMethodName: string): TBytes;
var
  ElementSize: Integer;
begin
  // Steps 1-3: a detached or out-of-bounds view is a TypeError.
  EnsureAtomicTypedArrayInBounds(ATypedArray, AMethodName);
  // Step 5: an element past the buffer's current byte length is a RangeError.
  // ES2026 compares byteIndexInBuffer; the current draft compares the
  // element's last byte, which also rejects an element that only partly
  // fits, so this compares the last byte. Only a length-tracking view of a
  // shrunk buffer gets here: a fixed-length view that is in bounds still
  // holds every element it had.
  ElementSize := TGocciaTypedArrayValue.BytesPerElement(ATypedArray.Kind);
  Result := ATypedArray.BufferData;
  if Int64(AByteIndexInBuffer) + ElementSize >
     ATypedArray.BufferByteLength then
    ThrowRangeError(AtomicsErrorMessage(AMethodName,
      SErrorInvalidTypedArrayIndex));
end;

{ The element helpers take the block the caller read after its last argument
  conversion: RevalidateAtomicAccess's result, or BufferData for a wait on a
  SharedArrayBuffer, which cannot shrink. }
function ReadNumberElement(const ABlock: TBytes;
  AKind: TGocciaTypedArrayKind; AByteOffset: Integer): Double;
begin
  Result := ReadBinaryNumberElement(ABlock, AByteOffset,
    BinaryKindForTypedArray(AKind), ATOMICS_LITTLE_ENDIAN);
end;

procedure WriteNumberElement(var ABlock: TBytes;
  AKind: TGocciaTypedArrayKind; AByteOffset: Integer; AValue: Double);
begin
  WriteBinaryNumberElement(ABlock, AByteOffset, BinaryKindForTypedArray(AKind),
    AValue, ATOMICS_LITTLE_ENDIAN);
end;

function ConvertNumberForKind(AKind: TGocciaTypedArrayKind;
  AValue: TGocciaValue): Double;
var
  BufferData: TBytes;
begin
  SetLength(BufferData, TGocciaTypedArrayValue.BytesPerElement(AKind));
  WriteBinaryNumberElement(BufferData, 0, BinaryKindForTypedArray(AKind),
    AValue.ToNumberLiteral.Value, ATOMICS_LITTLE_ENDIAN);
  Result := ReadBinaryNumberElement(BufferData, 0, BinaryKindForTypedArray(AKind),
    ATOMICS_LITTLE_ENDIAN);
end;

function ToIntegerOrInfinityNumber(const AValue: TGocciaValue): Double;
var
  NumberValue: Double;
begin
  NumberValue := AValue.ToNumberLiteral.Value;
  if IsNan(NumberValue) then
    Exit(0);
  if (NumberValue = 0) or IsInfinite(NumberValue) then
    Exit(NumberValue);
  if NumberValue < 0 then
    Exit(-Floor(Abs(NumberValue)));
  Result := Floor(NumberValue);
end;

function ReadBigIntRawElement(const ABlock: TBytes;
  AKind: TGocciaTypedArrayKind; AByteOffset: Integer): Int64;
begin
  Result := ReadBinaryBigIntElement(ABlock, AByteOffset,
    BinaryKindForTypedArray(AKind), ATOMICS_LITTLE_ENDIAN);
end;

procedure WriteBigIntRawElement(var ABlock: TBytes; AByteOffset: Integer;
  AValue: Int64);
begin
  WriteBinaryBigIntElement(ABlock, AByteOffset, AValue, ATOMICS_LITTLE_ENDIAN);
end;

function ToBigIntValueForAtomics(const AValue: TGocciaValue): TBigInteger;
var
  Primitive: TGocciaValue;
  TextValue: string;
  Parsed: TBigInteger;
begin
  Primitive := ToPrimitive(AValue, tphNumber);
  if Primitive is TGocciaBigIntValue then
    Exit(TGocciaBigIntValue(Primitive).Value);

  if Primitive is TGocciaBooleanLiteralValue then
  begin
    if TGocciaBooleanLiteralValue(Primitive).Value then
      Exit(TGocciaBigIntValue.BigIntOne.Value)
    else
      Exit(TGocciaBigIntValue.BigIntZero.Value);
  end;

  if Primitive is TGocciaStringLiteralValue then
  begin
    TextValue := Trim(TGocciaStringLiteralValue(Primitive).Value);
    if TryStringToBigInt(TextValue, Parsed) then
      Exit(Parsed);
  end;

  ThrowTypeError(SErrorBigIntTypedArrayRequiresBigInt);
  Result := TBigInteger.Zero;
end;

function ConvertBigIntRawForKind(AKind: TGocciaTypedArrayKind;
  const AValue: TGocciaValue): Int64; overload;
var
  BigIntValue: TBigInteger;
begin
  BigIntValue := ToBigIntValueForAtomics(AValue);
  case AKind of
    takBigInt64:
      Result := BigIntValue.AsIntN(64).ToInt64;
    takBigUint64:
      Result := BigIntValue.AsUintN(64).ToInt64;
  else
    Result := 0;
  end;
end;

function ConvertBigIntRawForKind(AKind: TGocciaTypedArrayKind;
  const ABigIntValue: TBigInteger): Int64; overload;
begin
  case AKind of
    takBigInt64:
      Result := ABigIntValue.AsIntN(64).ToInt64;
    takBigUint64:
      Result := ABigIntValue.AsUintN(64).ToInt64;
  else
    Result := 0;
  end;
end;

function BigIntFromUInt64(AValue: UInt64): TBigInteger;
var
  HighPart: TBigInteger;
  LowPart: TBigInteger;
begin
  HighPart := TBigInteger.FromInt64(Int64(AValue shr 32)).
    Multiply(TBigInteger.FromInt64($100000000));
  LowPart := TBigInteger.FromInt64(Int64(AValue and $FFFFFFFF));
  Result := HighPart.Add(LowPart);
end;

function BigIntValueFromRaw(AKind: TGocciaTypedArrayKind;
  AValue: Int64): TGocciaBigIntValue;
begin
  if AKind = takBigUint64 then
    Result := TGocciaBigIntValue.Create(BigIntFromUInt64(UInt64(AValue)))
  else
    Result := TGocciaBigIntValue.Create(TBigInteger.FromInt64(AValue));
end;

function ApplyNumberOperation(AOperation: TGocciaAtomicsOperation; AOldValue,
  AOperand: Double): Double;
var
  OldInteger: Int64;
  OperandInteger: Int64;
begin
  case AOperation of
    aoAdd:
      Result := AOldValue + AOperand;
    aoExchange:
      Result := AOperand;
    aoSub:
      Result := AOldValue - AOperand;
    aoAnd, aoOr, aoXor:
      begin
        OldInteger := Trunc(AOldValue);
        OperandInteger := Trunc(AOperand);
        case AOperation of
          aoAnd:
            Result := OldInteger and OperandInteger;
          aoOr:
            Result := OldInteger or OperandInteger;
          aoXor:
            Result := OldInteger xor OperandInteger;
        else
          Result := AOldValue;
        end;
      end;
  else
    Result := AOldValue;
  end;
end;

function ApplyBigIntOperation(AOperation: TGocciaAtomicsOperation; AOldValue,
  AOperand: Int64): Int64;
var
  NewValue: UInt64;
  OldValue: UInt64;
  OperandValue: UInt64;
begin
  OldValue := UInt64(AOldValue);
  OperandValue := UInt64(AOperand);
  case AOperation of
    aoAdd:
      NewValue := OldValue + OperandValue;
    aoAnd:
      NewValue := OldValue and OperandValue;
    aoExchange:
      NewValue := OperandValue;
    aoOr:
      NewValue := OldValue or OperandValue;
    aoSub:
      NewValue := OldValue - OperandValue;
    aoXor:
      NewValue := OldValue xor OperandValue;
  else
    NewValue := OldValue;
  end;
  Result := Int64(NewValue);
end;

// ES2026 §25.4.3.17 AtomicReadModifyWrite(typedArray, index, value, op)
function AtomicReadModifyWrite(const AArgs: TGocciaArgumentsCollection;
  const AMethodName: string; AOperation: TGocciaAtomicsOperation): TGocciaValue;
var
  Block: TBytes;
  Buffer: TGocciaValue;
  ByteOffset: Integer;
  Index: Integer;
  NewBigIntRaw: Int64;
  NewNumber: Double;
  OldBigIntRaw: Int64;
  OldNumber: Double;
  OperandBigIntRaw: Int64;
  OperandNumber: Double;
  TypedArray: TGocciaTypedArrayValue;
begin
  // Step 1: ValidateAtomicAccessOnIntegerTypedArray(typedArray, index)
  ValidateAtomicAccess(AArgs, AMethodName, False, True, TypedArray, Buffer, Index,
    ByteOffset);
  // Step 2: convert the operand before the element is read; ToBigInt and
  // ToIntegerOrInfinity can run script code.
  OperandBigIntRaw := 0;
  OperandNumber := 0;
  if IsBigIntKind(TypedArray.Kind) then
    OperandBigIntRaw := ConvertBigIntRawForKind(TypedArray.Kind,
      GetArgumentOrUndefined(AArgs, 2))
  else
    OperandNumber := ToIntegerOrInfinityNumber(GetArgumentOrUndefined(AArgs,
      2));
  // Step 3: RevalidateAtomicAccess(typedArray, byteIndexInBuffer)
  Block := RevalidateAtomicAccess(TypedArray, ByteOffset, AMethodName);

  CriticalSectionEnter(GAtomicsLock);
  try
    if IsBigIntKind(TypedArray.Kind) then
    begin
      OldBigIntRaw := ReadBigIntRawElement(Block, TypedArray.Kind, ByteOffset);
      NewBigIntRaw := ApplyBigIntOperation(AOperation, OldBigIntRaw,
        OperandBigIntRaw);
      WriteBigIntRawElement(Block, ByteOffset, NewBigIntRaw);
      Result := BigIntValueFromRaw(TypedArray.Kind, OldBigIntRaw);
    end
    else
    begin
      OldNumber := ReadNumberElement(Block, TypedArray.Kind, ByteOffset);
      NewNumber := ApplyNumberOperation(AOperation, OldNumber, OperandNumber);
      WriteNumberElement(Block, TypedArray.Kind, ByteOffset, NewNumber);
      Result := TGocciaNumberLiteralValue.Create(OldNumber);
    end;
  finally
    CriticalSectionLeave(GAtomicsLock);
  end;
end;

function CreateWaitAsyncResult(AAsync: Boolean;
  const AValue: TGocciaValue): TGocciaObjectValue;
begin
  Result := TGocciaObjectValue.Create;
  Result.AssignProperty('async', TGocciaBooleanLiteralValue.Create(AAsync));
  Result.AssignProperty('value', AValue);
end;

procedure AddWaiter(AWaiter: TAtomicsWaiter);
begin
  AWaiter.InList := True;
  GetAtomicsWaiters.Add(AWaiter);
end;

procedure RemoveWaiter(AWaiter: TAtomicsWaiter);
var
  Index: Integer;
  Waiters: TObjectList<TAtomicsWaiter>;
begin
  if not AWaiter.InList then
    Exit;

  Waiters := GetAtomicsWaiters;
  Index := Waiters.IndexOf(AWaiter);
  if Index >= 0 then
    Waiters.Delete(Index);
  AWaiter.InList := False;
end;

function PromiseHasAtomicsWaiter(const APromise: TGocciaPromiseValue): Boolean;
var
  I: Integer;
  Waiters: TObjectList<TAtomicsWaiter>;
begin
  Result := False;
  CriticalSectionEnter(GAtomicsLock);
  try
    Waiters := GetAtomicsWaiters;
    for I := 0 to Waiters.Count - 1 do
      if Waiters[I].Async and (Waiters[I].Promise = APromise) then
        Exit(True);
  finally
    CriticalSectionLeave(GAtomicsLock);
  end;
end;

function PumpAtomicsWaitAsyncCompletions: Integer;
var
  DueWaiters: TList<TAtomicsWaiter>;
  I: Integer;
  NowMilliseconds: UInt64;
  OwnerThreadId: TThreadID;
  Waiter: TAtomicsWaiter;
  Waiters: TObjectList<TAtomicsWaiter>;
begin
  Result := 0;
  if GAtomicsAsyncWaiterCount = 0 then
    Exit;
  DueWaiters := TList<TAtomicsWaiter>.Create;
  try
    NowMilliseconds := UInt64(GetMilliseconds);
    OwnerThreadId := GetGocciaThreadId;
    CriticalSectionEnter(GAtomicsLock);
    try
      Waiters := GetAtomicsWaiters;
      I := 0;
      while I < Waiters.Count do
      begin
        Waiter := Waiters[I];
        if Waiter.Async and (Waiter.OwnerThreadId = OwnerThreadId) and
          (Waiter.Ready or (Waiter.HasDeadline and
          (NowMilliseconds >= Waiter.DeadlineMilliseconds))) then
        begin
          if not Waiter.Ready then
            Waiter.ResultText := ATOMICS_WAIT_TIMED_OUT;
          Waiter.InList := False;
          Waiters.Delete(I);
          DueWaiters.Add(Waiter);
        end
        else
          Inc(I);
      end;
    finally
      CriticalSectionLeave(GAtomicsLock);
    end;

    for Waiter in DueWaiters do
    begin
      Waiter.Promise.Resolve(TGocciaStringLiteralValue.Create(
        Waiter.ResultText));
      Waiter.Free;
      Inc(Result);
    end;
  finally
    DueWaiters.Free;
  end;
end;

function HasPendingAtomicsWaitAsyncCompletions: Boolean;
var
  I: Integer;
  Waiters: TObjectList<TAtomicsWaiter>;
begin
  Result := False;
  CriticalSectionEnter(GAtomicsLock);
  try
    Waiters := GetAtomicsWaiters;
    for I := 0 to Waiters.Count - 1 do
      if Waiters[I].Async and (Waiters[I].OwnerThreadId = GetGocciaThreadId) then
        Exit(True);
  finally
    CriticalSectionLeave(GAtomicsLock);
  end;
end;

function HasSelfSettlingAtomicsWaitAsyncWaiters(
  const AOwner: TGocciaAtomics): Boolean;
var
  I: Integer;
  OwnerThreadId: TThreadID;
  Waiters: TObjectList<TAtomicsWaiter>;
begin
  Result := False;
  if (GAtomicsAsyncWaiterCount = 0) or not Assigned(AOwner) then
    Exit;
  OwnerThreadId := GetGocciaThreadId;
  CriticalSectionEnter(GAtomicsLock);
  try
    Waiters := GetAtomicsWaiters;
    for I := 0 to Waiters.Count - 1 do
      if Waiters[I].Async and (Waiters[I].Owner = AOwner) and
         (Waiters[I].OwnerThreadId = OwnerThreadId) and
         (Waiters[I].Ready or Waiters[I].HasDeadline) then
        Exit(True);
  finally
    CriticalSectionLeave(GAtomicsLock);
  end;
end;

function WaitForAtomicsPromise(const APromise: TGocciaPromiseValue): Boolean;
begin
  if not Assigned(APromise) then
    Exit(False);

  while APromise.State = gpsPending do
  begin
    PumpAtomicsWaitAsyncCompletions;
    if APromise.State <> gpsPending then
      Exit(True);

    if not PromiseHasAtomicsWaiter(APromise) then
      Exit(False);

    CheckInstructionLimit;
    CheckExecutionTimeoutNow;
    Sleep(1);
  end;

  Result := True;
end;

{ Suspends until AWaiter's event is set or its timeout passes. Whether the wait
  was notified is decided afterwards, inside the waiter list's critical
  section, not here. }
procedure WaitForSynchronousWaiter(AWaiter: TAtomicsWaiter;
  ATimeoutMilliseconds: Int64);
var
  Deadline: UInt64;
  NowMilliseconds: UInt64;
  Remaining: Int64;
  Slice: LongWord;
begin
  Deadline := 0;
  if ATimeoutMilliseconds >= 0 then
    Deadline := UInt64(GetMilliseconds) + UInt64(ATimeoutMilliseconds);

  repeat
    CheckInstructionLimit;
    CheckExecutionTimeoutNow;

    if ATimeoutMilliseconds < 0 then
      Slice := 50
    else
    begin
      NowMilliseconds := UInt64(GetMilliseconds);
      if NowMilliseconds >= Deadline then
        Break;
      Remaining := Int64(Deadline - NowMilliseconds);
      if Remaining > 50 then
        Slice := 50
      else
        Slice := LongWord(Remaining);
    end;

    if AWaiter.Event.WaitFor(Slice) = wrSignaled then
      Exit;
  until False;
end;

procedure CancelAtomicsWaitAsyncForOwner(AOwner: TGocciaAtomics);
var
  CancelledWaiters: TList<TAtomicsWaiter>;
  I: Integer;
  Waiter: TAtomicsWaiter;
  Waiters: TObjectList<TAtomicsWaiter>;
begin
  if not Assigned(GAtomicsWaiters) then
    Exit;

  CancelledWaiters := TList<TAtomicsWaiter>.Create;
  try
    CriticalSectionEnter(GAtomicsLock);
    try
      Waiters := GetAtomicsWaiters;
      I := 0;
      while I < Waiters.Count do
      begin
        Waiter := Waiters[I];
        if Waiter.Async and (Waiter.Owner = AOwner) then
        begin
          Waiter.InList := False;
          Waiters.Delete(I);
          CancelledWaiters.Add(Waiter);
        end
        else
          Inc(I);
      end;
    finally
      CriticalSectionLeave(GAtomicsLock);
    end;

    for Waiter in CancelledWaiters do
      Waiter.Free;
  finally
    CancelledWaiters.Free;
  end;
end;

procedure ShutdownAtomicsWaiters;
var
  Waiter: TAtomicsWaiter;
  Waiters: TObjectList<TAtomicsWaiter>;
begin
  Waiters := nil;
  CriticalSectionEnter(GAtomicsLock);
  try
    if not Assigned(GAtomicsWaiters) then
      Exit;
    Waiters := GAtomicsWaiters;
    GAtomicsWaiters := nil;
  finally
    CriticalSectionLeave(GAtomicsLock);
  end;

  while Assigned(Waiters) and (Waiters.Count > 0) do
  begin
    Waiter := Waiters[Waiters.Count - 1];
    Waiter.InList := False;
    Waiters.Delete(Waiters.Count - 1);
    Waiter.Free;
  end;
  Waiters.Free;
end;

procedure ShutdownAtomicsWaitersForCurrentThread;
var
  CurrentThreadId: TThreadID;
  I: Integer;
  RemovedWaiters: TList<TAtomicsWaiter>;
  Waiter: TAtomicsWaiter;
  Waiters: TObjectList<TAtomicsWaiter>;
begin
  // Registered with Goccia.ThreadCleanupRegistry, so the drain runs this on
  // worker exit AND again at main-thread finalization — by which point this
  // unit's own finalization has already run ShutdownAtomicsWaiters (nil-ing
  // GAtomicsWaiters) and CriticalSectionDone'd GAtomicsLock. Bail out before
  // touching the now-destroyed lock when the list is already gone.
  //
  // This pre-lock read of the shared GAtomicsWaiters is deliberately unlocked.
  // On the main-thread finalization path the process is single-threaded, so it
  // cannot race. On a worker-exit path the lock is alive and another worker may
  // be creating GAtomicsWaiters (nil -> non-nil) under it, but the read still
  // gives THIS thread a correct answer: a thread only has waiters to remove
  // after the list was created (creation precedes any Add), so reading nil means
  // this thread has nothing to clean. It is a single aligned-pointer load —
  // atomic on supported targets, never torn.
  if not Assigned(GAtomicsWaiters) then
    Exit;
  RemovedWaiters := TList<TAtomicsWaiter>.Create;
  try
    CurrentThreadId := GetGocciaThreadId;
    CriticalSectionEnter(GAtomicsLock);
    try
      if not Assigned(GAtomicsWaiters) then
        Exit;

      Waiters := GAtomicsWaiters;
      I := 0;
      while I < Waiters.Count do
      begin
        Waiter := Waiters[I];
        if Waiter.OwnerThreadId = CurrentThreadId then
        begin
          Waiter.InList := False;
          Waiters.Delete(I);
          RemovedWaiters.Add(Waiter);
        end
        else
          Inc(I);
      end;
    finally
      CriticalSectionLeave(GAtomicsLock);
    end;

    for Waiter in RemovedWaiters do
      Waiter.Free;
  finally
    RemovedWaiters.Free;
  end;
end;

constructor TGocciaAtomics.Create(const AName: string; AScope: TGocciaScope;
  AThrowError: TGocciaThrowErrorCallback;
  const ADefineGlobalBinding: Boolean = True);
var
  Members: TGocciaMemberCollection;
  StaticMembers: TArray<TGocciaMemberDefinition>;
begin
  inherited Create(AName, AScope, AThrowError);

  Members := TGocciaMemberCollection.Create;
  try
    Members.AddNamedMethod('add', AtomicsAdd, 3, gmkStaticMethod,
      [gmfNotConstructable]);
    Members.AddNamedMethod('and', AtomicsAnd, 3, gmkStaticMethod,
      [gmfNotConstructable]);
    Members.AddNamedMethod('compareExchange', AtomicsCompareExchange, 4,
      gmkStaticMethod, [gmfNotConstructable]);
    Members.AddNamedMethod('exchange', AtomicsExchange, 3, gmkStaticMethod,
      [gmfNotConstructable]);
    Members.AddNamedMethod('isLockFree', AtomicsIsLockFree, 1,
      gmkStaticMethod, [gmfNotConstructable]);
    Members.AddNamedMethod('load', AtomicsLoad, 2, gmkStaticMethod,
      [gmfNotConstructable]);
    Members.AddNamedMethod('notify', AtomicsNotify, 3, gmkStaticMethod,
      [gmfNotConstructable]);
    Members.AddNamedMethod('or', AtomicsOr, 3, gmkStaticMethod,
      [gmfNotConstructable]);
    Members.AddNamedMethod('pause', AtomicsPause, 0, gmkStaticMethod,
      [gmfNotConstructable]);
    Members.AddNamedMethod('store', AtomicsStore, 3, gmkStaticMethod,
      [gmfNotConstructable]);
    Members.AddNamedMethod('sub', AtomicsSub, 3, gmkStaticMethod,
      [gmfNotConstructable]);
    Members.AddNamedMethod('wait', AtomicsWait, 4, gmkStaticMethod,
      [gmfNotConstructable]);
    Members.AddNamedMethod('waitAsync', AtomicsWaitAsync, 4, gmkStaticMethod,
      [gmfNotConstructable]);
    Members.AddNamedMethod('xor', AtomicsXor, 3, gmkStaticMethod,
      [gmfNotConstructable]);
    Members.AddSymbolDataProperty(
      TGocciaSymbolValue.WellKnownToStringTag,
      TGocciaStringLiteralValue.Create('Atomics'),
      [pfConfigurable]);
    StaticMembers := Members.ToDefinitions;
  finally
    Members.Free;
  end;

  RegisterMemberDefinitions(FBuiltinObject, StaticMembers);
  if ADefineGlobalBinding then
    AScope.DefineLexicalBinding(AName, FBuiltinObject, dtLet, True);
end;

destructor TGocciaAtomics.Destroy;
begin
  CancelAtomicsWaitAsyncForOwner(Self);
  inherited Destroy;
end;

function TGocciaAtomics.AtomicsAdd(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := AtomicReadModifyWrite(AArgs, 'add', aoAdd);
end;

function TGocciaAtomics.AtomicsAnd(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := AtomicReadModifyWrite(AArgs, 'and', aoAnd);
end;

// ES2026 §25.4.6 Atomics.compareExchange(typedArray, index, expectedValue, replacementValue)
function TGocciaAtomics.AtomicsCompareExchange(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Block: TBytes;
  Buffer: TGocciaValue;
  ByteOffset: Integer;
  ExpectedBigIntRaw: Int64;
  ExpectedNumber: Double;
  Index: Integer;
  OldBigIntRaw: Int64;
  OldNumber: Double;
  ReplacementBigIntRaw: Int64;
  ReplacementNumber: Double;
  TypedArray: TGocciaTypedArrayValue;
begin
  ValidateAtomicAccess(AArgs, 'compareExchange', False, True, TypedArray, Buffer,
    Index, ByteOffset);
  // Steps 4-5: convert both operands, which can run script code, before
  // revalidating and reading the element.
  ExpectedBigIntRaw := 0;
  ReplacementBigIntRaw := 0;
  ExpectedNumber := 0;
  ReplacementNumber := 0;
  if IsBigIntKind(TypedArray.Kind) then
  begin
    ExpectedBigIntRaw := ConvertBigIntRawForKind(TypedArray.Kind,
      GetArgumentOrUndefined(AArgs, 2));
    ReplacementBigIntRaw := ConvertBigIntRawForKind(TypedArray.Kind,
      GetArgumentOrUndefined(AArgs, 3));
  end
  else
  begin
    ExpectedNumber := ConvertNumberForKind(TypedArray.Kind,
      GetArgumentOrUndefined(AArgs, 2));
    ReplacementNumber := ToIntegerOrInfinityNumber(GetArgumentOrUndefined(
      AArgs, 3));
  end;
  // Step 6: RevalidateAtomicAccess(typedArray, byteIndexInBuffer)
  Block := RevalidateAtomicAccess(TypedArray, ByteOffset, 'compareExchange');

  CriticalSectionEnter(GAtomicsLock);
  try
    if IsBigIntKind(TypedArray.Kind) then
    begin
      OldBigIntRaw := ReadBigIntRawElement(Block, TypedArray.Kind, ByteOffset);
      if OldBigIntRaw = ExpectedBigIntRaw then
        WriteBigIntRawElement(Block, ByteOffset, ReplacementBigIntRaw);
      Result := BigIntValueFromRaw(TypedArray.Kind, OldBigIntRaw);
    end
    else
    begin
      OldNumber := ReadNumberElement(Block, TypedArray.Kind, ByteOffset);
      if OldNumber = ExpectedNumber then
        WriteNumberElement(Block, TypedArray.Kind, ByteOffset,
          ReplacementNumber);
      Result := TGocciaNumberLiteralValue.Create(OldNumber);
    end;
  finally
    CriticalSectionLeave(GAtomicsLock);
  end;
end;

function TGocciaAtomics.AtomicsExchange(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := AtomicReadModifyWrite(AArgs, 'exchange', aoExchange);
end;

function TGocciaAtomics.AtomicsIsLockFree(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Size: Double;
begin
  Size := GetArgumentOrUndefined(AArgs, 0).ToNumberLiteral.Value;
  Result := TGocciaBooleanLiteralValue.Create((Size = 1) or (Size = 2) or
    (Size = 4) or (Size = 8));
end;

// ES2026 §25.4.9 Atomics.load(typedArray, index)
function TGocciaAtomics.AtomicsLoad(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Block: TBytes;
  Buffer: TGocciaValue;
  ByteOffset: Integer;
  Index: Integer;
  TypedArray: TGocciaTypedArrayValue;
begin
  ValidateAtomicAccess(AArgs, 'load', False, False, TypedArray, Buffer, Index,
    ByteOffset);
  // Step 2: RevalidateAtomicAccess(typedArray, byteIndexInBuffer); ToIndex
  // can have shrunk or detached the buffer.
  Block := RevalidateAtomicAccess(TypedArray, ByteOffset, 'load');

  CriticalSectionEnter(GAtomicsLock);
  try
    if IsBigIntKind(TypedArray.Kind) then
      Result := BigIntValueFromRaw(TypedArray.Kind,
        ReadBigIntRawElement(Block, TypedArray.Kind, ByteOffset))
    else
      Result := TGocciaNumberLiteralValue.Create(ReadNumberElement(Block,
        TypedArray.Kind, ByteOffset));
  finally
    CriticalSectionLeave(GAtomicsLock);
  end;
end;

function TGocciaAtomics.AtomicsNotify(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Buffer: TGocciaValue;
  ByteOffset: Integer;
  Count: Integer;
  Index: Integer;
  NotifiedCount: Integer;
  ResolveWaiters: TList<TAtomicsWaiter>;
  TypedArray: TGocciaTypedArrayValue;
  Waiter: TAtomicsWaiter;
  WaiterIndex: Integer;
  Waiters: TObjectList<TAtomicsWaiter>;
begin
  ValidateAtomicTypedArrayAccess(AArgs, 'notify', True, False, TypedArray, Index,
    ByteOffset);
  Count := ToWaiterCount(GetArgumentOrUndefined(AArgs, 2));
  if not (TypedArray.BufferValue is TGocciaSharedArrayBufferValue) then
    Exit(TGocciaNumberLiteralValue.Create(0));

  Buffer := TGocciaSharedArrayBufferValue(TypedArray.BufferValue);
  NotifiedCount := 0;
  ResolveWaiters := TList<TAtomicsWaiter>.Create;
  try
    CriticalSectionEnter(GAtomicsLock);
    try
      Waiters := GetAtomicsWaiters;
      WaiterIndex := 0;
      while WaiterIndex < Waiters.Count do
      begin
        Waiter := Waiters[WaiterIndex];
        if (not Waiter.Ready) and (Waiter.Buffer = Buffer) and
          (Waiter.ByteOffset = ByteOffset) and
          ((Count < 0) or (NotifiedCount < Count)) then
        begin
          Inc(NotifiedCount);
          if Waiter.Async then
          begin
            Waiter.Ready := True;
            Waiter.ResultText := ATOMICS_WAIT_OK;
            if Waiter.OwnerThreadId = GetGocciaThreadId then
            begin
              Waiter.InList := False;
              Waiters.Delete(WaiterIndex);
              ResolveWaiters.Add(Waiter);
            end
            else
              Inc(WaiterIndex);
          end
          else
          begin
            Waiter.InList := False;
            Waiters.Delete(WaiterIndex);
            Waiter.Event.SetEvent;
          end;
        end
        else
          Inc(WaiterIndex);
      end;
    finally
      CriticalSectionLeave(GAtomicsLock);
    end;

    for Waiter in ResolveWaiters do
    begin
      Waiter.Promise.Resolve(TGocciaStringLiteralValue.Create(ATOMICS_WAIT_OK));
      Waiter.Free;
    end;
  finally
    ResolveWaiters.Free;
  end;

  Result := TGocciaNumberLiteralValue.Create(NotifiedCount);
end;

function TGocciaAtomics.AtomicsOr(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := AtomicReadModifyWrite(AArgs, 'or', aoOr);
end;

function TGocciaAtomics.AtomicsPause(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  IterationValue: TGocciaValue;
  Iterations: Double;
begin
  IterationValue := GetArgumentOrUndefined(AArgs, 0);
  if IterationValue is TGocciaUndefinedLiteralValue then
    Exit(TGocciaUndefinedLiteralValue.UndefinedValue);

  if not (IterationValue is TGocciaNumberLiteralValue) then
    ThrowTypeError(AtomicsErrorMessage('pause',
      'iterationNumber must be an integral Number'));

  Iterations := TGocciaNumberLiteralValue(IterationValue).Value;
  if IsNan(Iterations) or IsInfinite(Iterations) or (Frac(Iterations) <> 0) or
    (Iterations < 0) or (Iterations > MAX_SAFE_INTEGER_F) then
    ThrowTypeError(AtomicsErrorMessage('pause',
      'iterationNumber must be an integral Number'));

  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

// ES2026 §25.4.11 Atomics.store(typedArray, index, value)
function TGocciaAtomics.AtomicsStore(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Block: TBytes;
  Buffer: TGocciaValue;
  ByteOffset: Integer;
  Index: Integer;
  NewBigIntValue: TBigInteger;
  NewBigIntRaw: Int64;
  NewNumber: Double;
  TypedArray: TGocciaTypedArrayValue;
begin
  ValidateAtomicAccess(AArgs, 'store', False, True, TypedArray, Buffer, Index,
    ByteOffset);
  // Step 2: convert the value, which can run script code.
  NewBigIntRaw := 0;
  NewNumber := 0;
  if IsBigIntKind(TypedArray.Kind) then
  begin
    NewBigIntValue := ToBigIntValueForAtomics(GetArgumentOrUndefined(AArgs,
      2));
    NewBigIntRaw := ConvertBigIntRawForKind(TypedArray.Kind, NewBigIntValue);
  end
  else
  begin
    NewNumber := ToIntegerOrInfinityNumber(GetArgumentOrUndefined(AArgs, 2));
    if NewNumber = 0 then
      NewNumber := 0.0;
  end;
  // Step 3: RevalidateAtomicAccess(typedArray, byteIndexInBuffer)
  Block := RevalidateAtomicAccess(TypedArray, ByteOffset, 'store');

  CriticalSectionEnter(GAtomicsLock);
  try
    if IsBigIntKind(TypedArray.Kind) then
    begin
      WriteBigIntRawElement(Block, ByteOffset, NewBigIntRaw);
      Result := TGocciaBigIntValue.Create(NewBigIntValue);
    end
    else
    begin
      WriteNumberElement(Block, TypedArray.Kind, ByteOffset, NewNumber);
      Result := TGocciaNumberLiteralValue.Create(NewNumber);
    end;
  finally
    CriticalSectionLeave(GAtomicsLock);
  end;
end;

function TGocciaAtomics.AtomicsSub(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := AtomicReadModifyWrite(AArgs, 'sub', aoSub);
end;

function TGocciaAtomics.AtomicsWait(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Block: TBytes;
  Buffer: TGocciaSharedArrayBufferValue;
  ByteOffset: Integer;
  ExpectedBigIntRaw: Int64;
  ExpectedNumber: Double;
  Index: Integer;
  TimeoutMilliseconds: Int64;
  TypedArray: TGocciaTypedArrayValue;
  Notified: Boolean;
  Waiter: TAtomicsWaiter;
  WaitValueMatches: Boolean;
begin
  ValidateAtomicsWaitAccess(AArgs, 'wait', TypedArray, Buffer, Index,
    ByteOffset);
  if TypedArray.Kind = takBigInt64 then
    ExpectedBigIntRaw := ConvertBigIntRawForKind(TypedArray.Kind,
      GetArgumentOrUndefined(AArgs, 2))
  else
    ExpectedNumber := ConvertNumberForKind(TypedArray.Kind,
      GetArgumentOrUndefined(AArgs, 2));
  TimeoutMilliseconds := ToTimeoutMilliseconds(GetArgumentOrUndefined(AArgs, 3));

  CriticalSectionEnter(GAtomicsLock);
  try
    // A SharedArrayBuffer only grows, and keeps its block when it does, so
    // the index is still inside the block after the value conversion above.
    Block := TypedArray.BufferData;
    if TypedArray.Kind = takBigInt64 then
      WaitValueMatches := ReadBigIntRawElement(Block, TypedArray.Kind,
        ByteOffset) = ExpectedBigIntRaw
    else
      WaitValueMatches := ReadNumberElement(Block, TypedArray.Kind,
        ByteOffset) = ExpectedNumber;

    if not WaitValueMatches then
      Exit(TGocciaStringLiteralValue.Create(ATOMICS_WAIT_NOT_EQUAL));

    if not AtomicsAgentCanSuspend then
      ThrowTypeError('Atomics.wait cannot suspend in this host context');

    if TimeoutMilliseconds = 0 then
      Exit(TGocciaStringLiteralValue.Create(ATOMICS_WAIT_TIMED_OUT));

    Waiter := TAtomicsWaiter.Create(Self, Buffer, ByteOffset, False, nil);
    AddWaiter(Waiter);
  finally
    CriticalSectionLeave(GAtomicsLock);
  end;

  Notified := False;
  try
    try
      WaitForSynchronousWaiter(Waiter, TimeoutMilliseconds);
      if Assigned(GAtomicsWaitSuspensionEndedHook) then
        GAtomicsWaitSuspensionEndedHook;
    finally
      // ES2026 §25.4.3.11 SuspendThisAgent step 8: the agent re-enters the
      // critical section, and only there is it settled whether the wait was
      // notified (ES2023 §25.4.12 Atomics.wait steps 18-23 spell this out;
      // §25.4.3.15 decides an async timeout the same way). The wait was
      // notified exactly when Atomics.notify removed the waiter: a notify
      // that lands after the suspension timed out but before this point
      // counted the waiter, so the wait returns "ok", and a waiter still in
      // the list timed out and removes itself, so no later notify counts it.
      // This also leaves the waiter list as it was before the wait on every
      // exit, including the instruction limit or execution timeout raising
      // inside the wait: the waiter is freed below, and a waiter left in the
      // list would be read, written and freed again by Atomics.notify, the
      // owner's destructor and the thread's shutdown.
      CriticalSectionEnter(GAtomicsLock);
      try
        Notified := not Waiter.InList;
        RemoveWaiter(Waiter);
      finally
        CriticalSectionLeave(GAtomicsLock);
      end;
    end;
    if Notified then
      Result := TGocciaStringLiteralValue.Create(ATOMICS_WAIT_OK)
    else
      Result := TGocciaStringLiteralValue.Create(ATOMICS_WAIT_TIMED_OUT);
  finally
    Waiter.Free;
  end;
end;

function TGocciaAtomics.AtomicsWaitAsync(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Block: TBytes;
  Buffer: TGocciaSharedArrayBufferValue;
  ByteOffset: Integer;
  DeadlineMilliseconds: UInt64;
  ExpectedBigIntRaw: Int64;
  ExpectedNumber: Double;
  HasDeadline: Boolean;
  Index: Integer;
  Promise: TGocciaPromiseValue;
  TimeoutMilliseconds: Int64;
  TypedArray: TGocciaTypedArrayValue;
  Waiter: TAtomicsWaiter;
  WaitValueMatches: Boolean;
begin
  ValidateAtomicsWaitAccess(AArgs, 'waitAsync', TypedArray, Buffer, Index,
    ByteOffset);
  if TypedArray.Kind = takBigInt64 then
    ExpectedBigIntRaw := ConvertBigIntRawForKind(TypedArray.Kind,
      GetArgumentOrUndefined(AArgs, 2))
  else
    ExpectedNumber := ConvertNumberForKind(TypedArray.Kind,
      GetArgumentOrUndefined(AArgs, 2));
  TimeoutMilliseconds := ToTimeoutMilliseconds(GetArgumentOrUndefined(AArgs, 3));

  CriticalSectionEnter(GAtomicsLock);
  try
    // A SharedArrayBuffer only grows, and keeps its block when it does, so
    // the index is still inside the block after the value conversion above.
    Block := TypedArray.BufferData;
    if TypedArray.Kind = takBigInt64 then
      WaitValueMatches := ReadBigIntRawElement(Block, TypedArray.Kind,
        ByteOffset) = ExpectedBigIntRaw
    else
      WaitValueMatches := ReadNumberElement(Block, TypedArray.Kind,
        ByteOffset) = ExpectedNumber;

    if not WaitValueMatches then
      Exit(CreateWaitAsyncResult(False,
        TGocciaStringLiteralValue.Create(ATOMICS_WAIT_NOT_EQUAL)));

    if TimeoutMilliseconds = 0 then
      Exit(CreateWaitAsyncResult(False,
        TGocciaStringLiteralValue.Create(ATOMICS_WAIT_TIMED_OUT)));

    HasDeadline := TimeoutMilliseconds >= 0;
    if HasDeadline then
      DeadlineMilliseconds := UInt64(GetMilliseconds) + UInt64(TimeoutMilliseconds)
    else
      DeadlineMilliseconds := 0;

    Promise := TGocciaPromiseValue.Create;
    Waiter := TAtomicsWaiter.Create(Self, Buffer, ByteOffset, True, Promise,
      DeadlineMilliseconds, HasDeadline);
    AddWaiter(Waiter);
    Result := CreateWaitAsyncResult(True, Promise);
  finally
    CriticalSectionLeave(GAtomicsLock);
  end;
end;

function TGocciaAtomics.AtomicsXor(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := AtomicReadModifyWrite(AArgs, 'xor', aoXor);
end;

initialization
  CriticalSectionInit(GAtomicsLock);
  // Worker threads release their own Atomics waiters via the registry drain in
  // ShutdownThreadRuntime. ShutdownAtomicsWaitersForCurrentThread removes only
  // the calling thread's entries from the shared GAtomicsWaiters list; the
  // main thread's all-threads teardown stays in this unit's finalization below
  // (the per-thread/all-threads distinction is deliberate).
  RegisterThreadvarCleanup(@ShutdownAtomicsWaitersForCurrentThread);

finalization
  ShutdownAtomicsWaiters;
  CriticalSectionDone(GAtomicsLock);

end.
