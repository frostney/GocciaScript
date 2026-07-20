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
    property InList: Boolean read FInList write FInList;
    property Owner: TGocciaAtomics read FOwner;
    property OwnerThreadId: TThreadID read FOwnerThreadId;
    property Promise: TGocciaPromiseValue read FPromise;
    property Ready: Boolean read FReady write FReady;
    property ResultText: string read FResult write FResult;
  end;

var
  GAtomicsLock: TGocciaCriticalSection;
  GAtomicsWaiters: TObjectList<TAtomicsWaiter>;

threadvar
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

procedure EnsureAtomicTypedArrayUsable(const ATypedArray: TGocciaTypedArrayValue;
  const AMethodName: string; AWriteAccess: Boolean);
begin
  if (ATypedArray.BufferValue is TGocciaArrayBufferValue) and
     TGocciaArrayBufferValue(ATypedArray.BufferValue).Detached then
    ThrowTypeError(Format(SErrorCannotUseDetachedTypedArray,
      ['Atomics.' + AMethodName]));
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

function ReadNumberElement(ATypedArray: TGocciaTypedArrayValue;
  AByteOffset: Integer): Double;
begin
  Result := ReadBinaryNumberElement(ATypedArray.BufferData, AByteOffset,
    BinaryKindForTypedArray(ATypedArray.Kind), ATOMICS_LITTLE_ENDIAN);
end;

procedure WriteNumberElement(ATypedArray: TGocciaTypedArrayValue;
  AByteOffset: Integer; AValue: Double);
var
  BufferData: TBytes;
begin
  BufferData := ATypedArray.BufferData;
  WriteBinaryNumberElement(BufferData, AByteOffset,
    BinaryKindForTypedArray(ATypedArray.Kind), AValue, ATOMICS_LITTLE_ENDIAN);
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

function ReadBigIntRawElement(ATypedArray: TGocciaTypedArrayValue;
  AByteOffset: Integer): Int64;
begin
  Result := ReadBinaryBigIntElement(ATypedArray.BufferData, AByteOffset,
    BinaryKindForTypedArray(ATypedArray.Kind), ATOMICS_LITTLE_ENDIAN);
end;

procedure WriteBigIntRawElement(ATypedArray: TGocciaTypedArrayValue;
  AByteOffset: Integer; AValue: Int64);
var
  BufferData: TBytes;
begin
  BufferData := ATypedArray.BufferData;
  WriteBinaryBigIntElement(BufferData, AByteOffset, AValue, ATOMICS_LITTLE_ENDIAN);
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

function AtomicReadModifyWrite(const AArgs: TGocciaArgumentsCollection;
  const AMethodName: string; AOperation: TGocciaAtomicsOperation): TGocciaValue;
var
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
  ValidateAtomicAccess(AArgs, AMethodName, False, True, TypedArray, Buffer, Index,
    ByteOffset);

  CriticalSectionEnter(GAtomicsLock);
  try
    if IsBigIntKind(TypedArray.Kind) then
    begin
      OldBigIntRaw := ReadBigIntRawElement(TypedArray, ByteOffset);
      OperandBigIntRaw := ConvertBigIntRawForKind(TypedArray.Kind,
        GetArgumentOrUndefined(AArgs, 2));
      NewBigIntRaw := ApplyBigIntOperation(AOperation, OldBigIntRaw,
        OperandBigIntRaw);
      WriteBigIntRawElement(TypedArray, ByteOffset, NewBigIntRaw);
      Result := BigIntValueFromRaw(TypedArray.Kind, OldBigIntRaw);
    end
    else
    begin
      OldNumber := ReadNumberElement(TypedArray, ByteOffset);
      OperandNumber := ToIntegerOrInfinityNumber(GetArgumentOrUndefined(AArgs,
        2));
      NewNumber := ApplyNumberOperation(AOperation, OldNumber, OperandNumber);
      WriteNumberElement(TypedArray, ByteOffset, NewNumber);
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
    CheckExecutionTimeout;
    Sleep(1);
  end;

  Result := True;
end;

function WaitForSynchronousWaiter(AWaiter: TAtomicsWaiter;
  ATimeoutMilliseconds: Int64): string;
var
  Deadline: UInt64;
  NowMilliseconds: UInt64;
  Remaining: Int64;
  Slice: LongWord;
  WaitResult: TWaitResult;
begin
  Result := ATOMICS_WAIT_TIMED_OUT;
  Deadline := 0;
  if ATimeoutMilliseconds >= 0 then
    Deadline := UInt64(GetMilliseconds) + UInt64(ATimeoutMilliseconds);

  repeat
    CheckInstructionLimit;
    CheckExecutionTimeout;

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

    WaitResult := AWaiter.Event.WaitFor(Slice);
    if WaitResult = wrSignaled then
      Exit(ATOMICS_WAIT_OK);
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

function TGocciaAtomics.AtomicsCompareExchange(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
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

  CriticalSectionEnter(GAtomicsLock);
  try
    if IsBigIntKind(TypedArray.Kind) then
    begin
      ExpectedBigIntRaw := ConvertBigIntRawForKind(TypedArray.Kind,
        GetArgumentOrUndefined(AArgs, 2));
      ReplacementBigIntRaw := ConvertBigIntRawForKind(TypedArray.Kind,
        GetArgumentOrUndefined(AArgs, 3));
      OldBigIntRaw := ReadBigIntRawElement(TypedArray, ByteOffset);
      if OldBigIntRaw = ExpectedBigIntRaw then
        WriteBigIntRawElement(TypedArray, ByteOffset, ReplacementBigIntRaw);
      Result := BigIntValueFromRaw(TypedArray.Kind, OldBigIntRaw);
    end
    else
    begin
      ExpectedNumber := ConvertNumberForKind(TypedArray.Kind,
        GetArgumentOrUndefined(AArgs, 2));
      ReplacementNumber := ToIntegerOrInfinityNumber(GetArgumentOrUndefined(
        AArgs, 3));
      OldNumber := ReadNumberElement(TypedArray, ByteOffset);
      if OldNumber = ExpectedNumber then
        WriteNumberElement(TypedArray, ByteOffset, ReplacementNumber);
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

function TGocciaAtomics.AtomicsLoad(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Buffer: TGocciaValue;
  ByteOffset: Integer;
  Index: Integer;
  TypedArray: TGocciaTypedArrayValue;
begin
  ValidateAtomicAccess(AArgs, 'load', False, False, TypedArray, Buffer, Index,
    ByteOffset);

  CriticalSectionEnter(GAtomicsLock);
  try
    if IsBigIntKind(TypedArray.Kind) then
      Result := BigIntValueFromRaw(TypedArray.Kind,
        ReadBigIntRawElement(TypedArray, ByteOffset))
    else
      Result := TGocciaNumberLiteralValue.Create(ReadNumberElement(TypedArray,
        ByteOffset));
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

function TGocciaAtomics.AtomicsStore(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
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

  CriticalSectionEnter(GAtomicsLock);
  try
    if IsBigIntKind(TypedArray.Kind) then
    begin
      NewBigIntValue := ToBigIntValueForAtomics(GetArgumentOrUndefined(AArgs,
        2));
      NewBigIntRaw := ConvertBigIntRawForKind(TypedArray.Kind, NewBigIntValue);
      WriteBigIntRawElement(TypedArray, ByteOffset, NewBigIntRaw);
      Result := TGocciaBigIntValue.Create(NewBigIntValue);
    end
    else
    begin
      NewNumber := ToIntegerOrInfinityNumber(GetArgumentOrUndefined(AArgs, 2));
      if NewNumber = 0 then
        NewNumber := 0.0;
      WriteNumberElement(TypedArray, ByteOffset, NewNumber);
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
  Buffer: TGocciaSharedArrayBufferValue;
  ByteOffset: Integer;
  ExpectedBigIntRaw: Int64;
  ExpectedNumber: Double;
  Index: Integer;
  TimeoutMilliseconds: Int64;
  TypedArray: TGocciaTypedArrayValue;
  Waiter: TAtomicsWaiter;
  WaitValueMatches: Boolean;
  WaitResult: string;
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
    if TypedArray.Kind = takBigInt64 then
      WaitValueMatches := ReadBigIntRawElement(TypedArray, ByteOffset) =
        ExpectedBigIntRaw
    else
      WaitValueMatches := ReadNumberElement(TypedArray, ByteOffset) =
        ExpectedNumber;

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

  try
    WaitResult := WaitForSynchronousWaiter(Waiter, TimeoutMilliseconds);
    CriticalSectionEnter(GAtomicsLock);
    try
      RemoveWaiter(Waiter);
    finally
      CriticalSectionLeave(GAtomicsLock);
    end;
    Result := TGocciaStringLiteralValue.Create(WaitResult);
  finally
    Waiter.Free;
  end;
end;

function TGocciaAtomics.AtomicsWaitAsync(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
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
    if TypedArray.Kind = takBigInt64 then
      WaitValueMatches := ReadBigIntRawElement(TypedArray, ByteOffset) =
        ExpectedBigIntRaw
    else
      WaitValueMatches := ReadNumberElement(TypedArray, ByteOffset) =
        ExpectedNumber;

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
