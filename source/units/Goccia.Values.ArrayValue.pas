unit Goccia.Values.ArrayValue;

{$I Goccia.inc}

interface

uses
  Goccia.Arguments.Collection,
  Goccia.ObjectModel,
  Goccia.Realm,
  Goccia.Values.ClassValue,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  TGocciaArrayValue = class(TGocciaInstanceValue)
  private
    // Helper methods for reducing duplication
    function ValidateArrayMethodCall(const AMethodName: string; const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue; const ARequiresCallback: Boolean = True): TGocciaValue;
    function IsArrayHole(const AElement: TGocciaValue): Boolean; inline;

    procedure FlattenInto(const ATarget: TGocciaObjectValue; const ADepth: Integer;
      var ATargetIndex: Integer);
    procedure ThrowError(const AMessage: string; const AArgs: array of const); overload; inline;
    procedure ThrowError(const AMessage: string); overload; inline;
    procedure ThrowError(const AMessage: string; const AArgs: array of const; const ASuggestion: string); overload; inline;
  protected
    FElements: TGocciaValueList;
    FLength: Int64;
    FLengthWritable: Boolean;
  public
    constructor Create(const AClass: TGocciaClassValue = nil;
      const AElementCapacity: Integer = 0);
    destructor Destroy; override;
    procedure InitializePrototype;

    function ToStringTag: string; override;

    function ToStringLiteral: TGocciaStringLiteralValue; override;
    function ToBooleanLiteral: TGocciaBooleanLiteralValue; override;

    // Index-based element access
    function GetLength: Integer;
    function HasDenseElementLength: Boolean;
    function GetElement(const AIndex: Integer): TGocciaValue;
    function SetElement(const AIndex: Integer; const AValue: TGocciaValue): Boolean;
    procedure SetIndexProperty(const AIndex: Integer; const AValue: TGocciaValue);
    function TypeName: string; override;
    function GetProperty(const AName: string): TGocciaValue; override;
    function GetPropertyWithContext(const AName: string; const AThisContext: TGocciaValue): TGocciaValue; override;
    procedure SetProperty(const AName: string; const AValue: TGocciaValue); override;
    function HasOwnProperty(const AName: string): Boolean; override;
    function DeleteProperty(const AName: string): Boolean; override;
    function GetOwnPropertyKeys: TArray<string>; override;
    function GetAllPropertyNames: TArray<string>; override;
    function GetOwnPropertyDescriptor(const AName: string): TGocciaPropertyDescriptor; override;
    procedure DefineProperty(const AName: string; const ADescriptor: TGocciaPropertyDescriptor); override;
    function TryDefineProperty(const AName: string; const ADescriptor: TGocciaPropertyDescriptor): Boolean; override;
    function Includes(const AValue: TGocciaValue; AFromIndex: Integer = 0): Boolean;

    procedure InitializeNativeFromArguments(const AArguments: TGocciaArgumentsCollection); override;
    procedure MarkReferences; override;
    procedure Freeze; override;

    class procedure ExposePrototype(const AConstructor: TGocciaValue);

    property Elements: TGocciaValueList read FElements;
  published
    function ArrayMap(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ArrayFilter(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ArrayReduce(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ArrayReduceRight(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ArrayForEach(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ArraySome(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ArrayEvery(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ArrayFlat(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ArrayFlatMap(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ArrayJoin(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ArrayToString(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ArrayIncludes(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ArrayPush(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ArrayPop(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ArraySlice(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ArrayFind(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ArrayFindIndex(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ArrayIndexOf(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ArrayLastIndexOf(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ArrayConcat(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ArrayReverse(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ArrayToReversed(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ArrayToSorted(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ArrayToSpliced(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ArraySort(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ArraySplice(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ArrayShift(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ArrayUnshift(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ArrayFill(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ArrayAt(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ArrayFindLast(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ArrayFindLastIndex(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ArrayWith(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ArrayCopyWithin(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ArrayValues(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ArrayKeys(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ArrayEntries(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ArraySymbolIterator(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function GetLengthProperty(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
  end;

  function DefaultCompare(constref A, B: TGocciaValue): Integer;
  function GetSharedArrayPrototypeForRealm(const ARealm: TGocciaRealm): TGocciaObjectValue;

implementation

uses
  Generics.Collections,
  Generics.Defaults,
  Math,
  SysUtils,

  StringBuffer,

  Goccia.Arguments.Callbacks,
  Goccia.Arithmetic,
  Goccia.Constants.NumericLimits,
  Goccia.Constants.PropertyNames,
  Goccia.Error,
  Goccia.Error.Messages,
  Goccia.Error.Suggestions,
  Goccia.GarbageCollector,
  Goccia.Generator.Continuation,
  Goccia.Timeout,
  Goccia.Utils,
  Goccia.Utils.Arrays,
  Goccia.Values.ClassHelper,
  Goccia.Values.ErrorHelper,
  Goccia.Values.FunctionBase,
  Goccia.Values.HoleValue,
  Goccia.Values.Iterator.Concrete,
  Goccia.Values.ProxyValue,
  Goccia.Values.StringObjectValue,
  Goccia.Values.SymbolValue,
  Goccia.Values.ToObject,
  Goccia.Values.ToPrimitive;

// Per-realm slots for Array.prototype and its method host.  The host is the
// first TGocciaArrayValue constructed in the realm (Self in InitializePrototype);
// the native prototype methods bind to it.  Both live in realm slots, so the
// realm pins them on SetSlot and unpins them on Destroy — the host is reclaimed
// with its realm rather than pinned per-thread for the process lifetime.  The
// member definitions are rebuilt per realm because they bind to this realm's
// host. #892
var
  GArrayPrototypeSlot: TGocciaRealmSlotId;
  GArrayMethodHostSlot: TGocciaRealmSlotId;

function GetSharedArrayPrototype: TGocciaObjectValue; inline;
begin
  if Assigned(CurrentRealm) then
    Result := TGocciaObjectValue(CurrentRealm.GetSlot(GArrayPrototypeSlot))
  else
    Result := nil;
end;

function GetSharedArrayPrototypeForRealm(const ARealm: TGocciaRealm): TGocciaObjectValue;
begin
  if Assigned(ARealm) then
    Result := TGocciaObjectValue(ARealm.GetSlot(GArrayPrototypeSlot))
  else
    Result := nil;
end;

type
  // Uniform facade over native arrays and generic array-like objects.
  // Methods use this to operate on any ToObject'd receiver, keeping a fast
  // path for TGocciaArrayValue and a generic path for plain objects.
  TArrayLikeView = record
    Obj: TGocciaObjectValue;
    Len: Integer;
    // Raw post-ToLength length, capped at 2^53-1 but not truncated to MaxInt.
    // Used by callers that need to detect lengths that exceed spec bounds
    // (ArrayCreate: 2^32-1; safe-integer length: 2^53-1) before truncation.
    RawLen: Double;
    Arr: TGocciaArrayValue; // non-nil only when Obj is a native array
    SpeciesArr: TGocciaArrayValue; // non-nil when receiver is an Array or Array Proxy
    procedure Init(const AThisValue: TGocciaValue);
    function Get(const AIndex: Integer): TGocciaValue;
    procedure Put(const AIndex: Integer; const AValue: TGocciaValue);
    function HasIndex(const AIndex: Integer): Boolean;
    // Int64 variants used by the sparse-iteration path for generic
    // array-likes whose length exceeds 2^32 - 1 (ES2026 array methods are
    // spec-required to walk indices up to len, but iterating naively is
    // intractable past MaxInt; callers use CollectSparseIndicesInRange to
    // bound the work to actual indexed properties).
    function Get64(const AIndex: Int64): TGocciaValue;
    function HasIndex64(const AIndex: Int64): Boolean;
    procedure DeleteIndex(const AIndex: Integer);
    procedure SetLen(const ANewLen: Integer);
    // True when iteration should bypass the dense Integer-bounded loop and
    // dispatch through the sparse-key enumeration: the receiver is a
    // generic object (no fast TGocciaArrayValue.FElements) and its
    // claimed length exceeds the dense path's representable maximum.
    function NeedsSparsePath: Boolean;
    // Truncated raw length as Int64.  Safe up to 2^53 - 1 (ToLength upper
    // bound).  Used by the sparse iteration path.
    function Len64: Int64;
    // Spec §10.4.2.2 ArrayCreate: RangeError if RawLen > 2^32 - 1
    procedure CheckArrayCreateLen;
    // RangeError if ANewLen > 2^32 - 1 (for ArrayCreate(newLen))
    procedure CheckArrayCreateLenValue(const ANewLen: Double);
    // TypeError if ANewLen > 2^53 - 1 (for length updates on generic objects)
    procedure CheckSafeIntegerLen(const ANewLen: Double);
  end;

// Walks Obj's own properties and prototype chain for keys that parse as
// canonical integer indexes per ES2026 §7.1.21 CanonicalNumericIndexString,
// returning those whose value falls in [AStartInclusive, AEndExclusive)
// in ascending order with duplicates removed.  Used by Array.prototype
// methods to bound iteration over array-likes whose claimed length exceeds
// 2^32 - 1: in such receivers the spec-mandated [[HasProperty]] / [[Get]]
// probes only succeed for indices that actually exist on the chain, so the
// observable set of work is exactly this enumeration.
function CollectSparseIndicesInRange(const AObj: TGocciaObjectValue;
  const AStartInclusive, AEndExclusive: Int64): TArray<Int64>; forward;

// Int64 -> Double widening conversion.  In FPC Delphi mode, an explicit
// `Double(int64Var)` is a bit-pattern type cast (yielding a denormal for
// small integers), and `int64Var + 0.0` may evaluate at Single precision.
// Assigning to a Double-typed variable is the only form FPC compiles as a
// proper widening; passing it as a parameter then preserves precision.
function Int64ToDouble(const AValue: Int64): Double; inline;
begin
  Result := AValue;
end;

function GetArrayCallbackThisArg(const AArgs: TGocciaArgumentsCollection): TGocciaValue; inline;
begin
  if AArgs.Length > 1 then
    Result := AArgs.GetElement(1)
  else
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function InvokeArrayCallback(const ACallback: TGocciaValue;
  const ATypedCallback: TGocciaFunctionBase;
  const ACallArgs: TGocciaArgumentsCollection;
  const AThisArg: TGocciaValue): TGocciaValue; inline;
var
  PreviousContinuation: TGocciaGeneratorContinuation;
  CallbackRoot, ThisRoot: TGocciaTempRoot;
  ArgRoots: array of TGocciaTempRoot;
  I: Integer;
begin
  InitializeTempRoot(CallbackRoot);
  InitializeTempRoot(ThisRoot);
  AddTempRootIfNeeded(CallbackRoot, ACallback);
  AddTempRootIfNeeded(ThisRoot, AThisArg);
  SetLength(ArgRoots, ACallArgs.Length);
  for I := 0 to High(ArgRoots) do
    InitializeTempRoot(ArgRoots[I]);
  for I := 0 to High(ArgRoots) do
    AddTempRootIfNeeded(ArgRoots[I], ACallArgs.GetElement(I));
  PreviousContinuation := SuspendCurrentGeneratorContinuation;
  try
    try
      if Assigned(ATypedCallback) then
        Result := ATypedCallback.Call(ACallArgs, AThisArg)
      else
        Result := InvokeCallable(ACallback, ACallArgs, AThisArg);
    finally
      RestoreCurrentGeneratorContinuation(PreviousContinuation);
    end;
  finally
    for I := High(ArgRoots) downto 0 do
      RemoveTempRootIfNeeded(ArgRoots[I]);
    SetLength(ArgRoots, 0);
    RemoveTempRootIfNeeded(ThisRoot);
    RemoveTempRootIfNeeded(CallbackRoot);
  end;
end;


procedure TArrayLikeView.Init(const AThisValue: TGocciaValue);
var
  ArrayProbe: TGocciaValue;
begin
  Obj := ToObject(AThisValue);
  SpeciesArr := nil;
  if Obj is TGocciaArrayValue then
  begin
    Arr := TGocciaArrayValue(Obj);
    SpeciesArr := Arr;
    if Arr.FLength < Arr.Elements.Count then
      Arr.FLength := Arr.Elements.Count;
    RawLen := Arr.FLength;
    if RawLen > MaxInt then
      Len := MaxInt
    else
      Len := Trunc(RawLen);
  end
  else
  begin
    Arr := nil;
    ArrayProbe := Obj;
    while ArrayProbe is TGocciaProxyValue do
    begin
      if TGocciaProxyValue(ArrayProbe).Revoked then
        ThrowTypeError(SErrorProxyRevoked, SSuggestProxyRevoked);
      ArrayProbe := TGocciaProxyValue(ArrayProbe).Target;
    end;
    if ArrayProbe is TGocciaArrayValue then
      SpeciesArr := TGocciaArrayValue(ArrayProbe);
    Len := LengthOfArrayLikeEx(Obj, RawLen);
  end;
end;

procedure TArrayLikeView.CheckArrayCreateLen;
begin
  if RawLen > MAX_ARRAY_LENGTH_F then
    ThrowRangeError(SErrorInvalidArrayLength, SSuggestArrayLengthRange);
  // Materialised native arrays use TList<TGocciaValue>, whose Count is
  // a 32-bit Integer.  A generic array-like with length in
  // (MaxInt, 2^32 - 1] passes the spec ceiling but would silently
  // truncate to View.Len once we materialise it, leaving callers like
  // map / slice / toReversed / toSorted with a shortened result.
  if RawLen > MaxInt then
    ThrowRangeError(
      'Array length exceeds engine maximum (MaxInt)',
      'use a smaller length');
end;

procedure TArrayLikeView.CheckArrayCreateLenValue(const ANewLen: Double);
begin
  if ANewLen > MAX_ARRAY_LENGTH_F then
    ThrowRangeError(SErrorInvalidArrayLength, SSuggestArrayLengthRange);
  if ANewLen > MaxInt then
    ThrowRangeError(
      'Array length exceeds engine maximum (MaxInt)',
      'use a smaller length');
end;

procedure TArrayLikeView.CheckSafeIntegerLen(const ANewLen: Double);
begin
  if ANewLen > MAX_SAFE_INTEGER_F then
    ThrowTypeError(
      'Array length exceeds maximum safe integer (2^53 - 1)',
      'use a smaller length');
  // Goccia's array-like generic path stores indices and length in 32-bit
  // Integer.  Once a write index would exceed MaxInt the dense Put/SetLen
  // operations would overflow into negative values, corrupting the receiver.
  // Reject lengths outside Integer range here so the generic path never
  // observes an unsafe value.  Native arrays go through the fast Arr branch
  // and never reach this for the same operation.
  if ANewLen > MaxInt then
    ThrowRangeError(
      'Array length exceeds engine maximum (MaxInt)',
      'use a smaller length');
end;

function TArrayLikeView.Get(const AIndex: Integer): TGocciaValue;
begin
  // Per-iteration cooperative timeout poll for the same reason as
  // HasIndex: toReversed/toSorted/flat/slice and friends call Get
  // without HasIndex, so the timeout has to be reachable from here too.
  CheckExecutionTimeout;

  if Assigned(Arr) then
  begin
    if (AIndex >= 0) and (AIndex < Arr.Elements.Count) and
       (Arr.Elements[AIndex] <> TGocciaHoleValue.HoleValue) then
    begin
      Result := Arr.Elements[AIndex];
      Exit;
    end;
    // Hole or out-of-range: fall back to Obj.GetProperty for prototype lookup
    Result := Obj.GetProperty(IntToStr(AIndex));
    if not Assigned(Result) then
      Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  end
  else
  begin
    Result := Obj.GetProperty(IntToStr(AIndex));
    if not Assigned(Result) then
      Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  end;
end;

procedure TArrayLikeView.Put(const AIndex: Integer; const AValue: TGocciaValue);
begin
  if Assigned(Arr) then
  begin
    if not Arr.SetElement(AIndex, AValue) then
      ThrowTypeError(Format(SErrorCannotAssignReadOnly, [IntToStr(AIndex)]),
        SSuggestCannotDeleteNonConfigurable);
  end
  else
    Obj.AssignProperty(IntToStr(AIndex), AValue);
end;

function TArrayLikeView.HasIndex(const AIndex: Integer): Boolean;
begin
  // Poll the cooperative execution timeout from the per-iteration chokepoint
  // used by every native array iterator (indexOf, lastIndexOf, forEach, map,
  // filter, reduce, find, etc.). Pathological array-likes with length close
  // to 2^32 would otherwise loop inside native code for hundreds of seconds
  // without ever yielding to the timeout — long enough for the test-runner
  // watchdog to fire and silently drop every remaining file in the batch.
  // CheckExecutionTimeout is essentially free (1023/1024 calls are a single
  // masked increment), so taking it once per element is negligible overhead.
  CheckExecutionTimeout;

  if Assigned(Arr) then
  begin
    // Fast check: own element present and not a hole
    if (AIndex >= 0) and (AIndex < Arr.Elements.Count) and
       (Arr.Elements[AIndex] <> TGocciaHoleValue.HoleValue) then
    begin
      Result := True;
      Exit;
    end;
    // Hole or out-of-range: fall back to HasProperty for prototype lookup
    Result := Obj.HasProperty(IntToStr(AIndex));
  end
  else
    // ES spec uses [[HasProperty]] which traverses the prototype chain
    Result := Obj.HasProperty(IntToStr(AIndex));
end;

function TArrayLikeView.Get64(const AIndex: Int64): TGocciaValue;
begin
  CheckExecutionTimeout;
  if Assigned(Arr) and (AIndex >= 0) and (AIndex < Arr.Elements.Count) and
     (Arr.Elements[AIndex] <> TGocciaHoleValue.HoleValue) then
  begin
    Result := Arr.Elements[AIndex];
    Exit;
  end;
  Result := Obj.GetProperty(IntToStr(AIndex));
  if not Assigned(Result) then
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TArrayLikeView.HasIndex64(const AIndex: Int64): Boolean;
begin
  CheckExecutionTimeout;
  if Assigned(Arr) and (AIndex >= 0) and (AIndex < Arr.Elements.Count) and
     (Arr.Elements[AIndex] <> TGocciaHoleValue.HoleValue) then
  begin
    Result := True;
    Exit;
  end;
  Result := Obj.HasProperty(IntToStr(AIndex));
end;

function TArrayLikeView.NeedsSparsePath: Boolean;
begin
  // Len is a 32-bit Integer, so the dense loop saturates at 2^31 - 1 and
  // would silently skip every index beyond that.  Native arrays also need
  // this path for sparse property-map elements near the 2^32 - 1 array limit.
  // Callers like filter / reduce / forEach / includes / some / every /
  // indexOf / lastIndexOf / find* all rely on this gate.
  Result := RawLen > MaxInt;
end;

function TArrayLikeView.Len64: Int64;
begin
  // RawLen has already been clamped to [0, 2^53 - 1] by ToLength, so Trunc
  // preserves the exact integer value as Int64.
  if RawLen > MAX_SAFE_INTEGER_F then
    Result := MAX_SAFE_INTEGER
  else if RawLen < 0 then
    Result := 0
  else
    Result := Trunc(RawLen);
end;

procedure TArrayLikeView.DeleteIndex(const AIndex: Integer);
begin
  if Assigned(Arr) then
  begin
    if not Arr.DeleteProperty(IntToStr(AIndex)) then
      ThrowTypeError(Format(SErrorCannotRedefineNonConfigurable, [IntToStr(AIndex)]),
        SSuggestCannotDeleteNonConfigurable);
  end
  else
  begin
    if not Obj.DeleteProperty(IntToStr(AIndex)) then
      ThrowTypeError(Format(SErrorCannotRedefineNonConfigurable, [IntToStr(AIndex)]),
        SSuggestCannotDeleteNonConfigurable);
  end;
end;

procedure TArrayLikeView.SetLen(const ANewLen: Integer);
begin
  if Assigned(Arr) then
    Arr.SetProperty(PROP_LENGTH, TGocciaNumberLiteralValue.Create(ANewLen))
  else
    Obj.AssignProperty(PROP_LENGTH, TGocciaNumberLiteralValue.Create(ANewLen));
end;

function DefaultCompare(constref A, B: TGocciaValue): Integer;
var
  StrA, StrB: string;
begin
  if A is TGocciaUndefinedLiteralValue then
  begin
    if B is TGocciaUndefinedLiteralValue then
      Exit(0);
    Exit(1);
  end;
  if B is TGocciaUndefinedLiteralValue then
    Exit(-1);

  StrA := A.ToStringLiteral.Value;
  StrB := B.ToStringLiteral.Value;
  if StrA < StrB then
    Result := -1
  else if StrA > StrB then
    Result := 1
  else
    Result := 0;
end;

// ES2026 §7.1.21 CanonicalNumericIndexString — restricted to non-negative
// integer indices in [0, 2^53 - 1].  Accepts only canonical decimal digit
// strings: empty rejected, leading zero rejected unless the entire string
// is "0", any non-digit rejected, value beyond MAX_SAFE_INTEGER rejected.
// Returns False (and AIndex=0) for non-canonical / out-of-range keys; the
// caller then ignores them, matching the spec's [[HasProperty]] semantics
// where only canonical index strings participate in array iteration.
function TryParseArrayIndex(const AKey: string; out AIndex: Int64): Boolean;
var
  I: Integer;
  Digit: Int64;
begin
  AIndex := 0;
  Result := False;
  if AKey = '' then Exit;
  // Reject leading zero except for the single "0" string
  if (AKey[1] = '0') and (Length(AKey) > 1) then Exit;
  for I := 1 to Length(AKey) do
  begin
    if (AKey[I] < '0') or (AKey[I] > '9') then Exit;
    Digit := Ord(AKey[I]) - Ord('0');
    // Overflow check before AIndex * 10 + Digit could exceed MAX_SAFE_INTEGER
    if AIndex > (MAX_SAFE_INTEGER - Digit) div 10 then Exit;
    AIndex := AIndex * 10 + Digit;
  end;
  Result := True;
end;

function TryParseArrayElementIndex(const AKey: string; out AIndex: Int64): Boolean;
begin
  Result := TryParseArrayIndex(AKey, AIndex) and
    (AIndex < MAX_ARRAY_LENGTH);
end;

// Largest hole-pad a single index assignment may add to dense storage.
// A write within this gap of the current element count stays dense (the
// pad is at most ~8 MB of pointers); a farther jump routes to the ordinary
// property map — the same sparse storage indices >= MaxInt always use —
// so `x[2 ** 31 - 2] = v` no longer materializes billions of dense slots.
const
  MAX_DENSE_EXTENSION_GAP = 1024 * 1024;

function CanStoreDenseElementIndex(const AIndex: Int64;
  const ACurrentCount: Integer): Boolean;
begin
  // FElements is backed by Integer-indexed TList; keep the final legal dense
  // slot below MaxInt so growing Count never needs MaxInt + 1 entries.
  // Indices below ACurrentCount always pass (existing dense storage).
  Result := (AIndex >= 0) and (AIndex < MaxInt) and
    (AIndex <= Int64(ACurrentCount) + MAX_DENSE_EXTENSION_GAP);
end;

function CompareInt64(constref A, B: Int64): Integer;
begin
  if A < B then Result := -1
  else if A > B then Result := 1
  else Result := 0;
end;

function NormalizeRelativeIndex64(const ARelative, ALength: Int64): Int64;
begin
  if ARelative < 0 then
  begin
    Result := ALength + ARelative;
    if Result < 0 then
      Result := 0;
  end
  else if ARelative > ALength then
    Result := ALength
  else
    Result := ARelative;
end;

// ES2026 §10.4.2.4 ArraySetLength(A, Desc): delete own array-index
// properties in descending numeric order.  Dense elements live in FElements,
// while sparse/accessor descriptors live on the object descriptor map.
function DeleteArrayIndexesAtOrAbove(const AArray: TGocciaArrayValue;
  const ANewLength: Int64; out AFailedIndex: Int64): Boolean;
var
  SparseIndices: TList<Int64>;
  Keys: TArray<string>;
  Key: string;
  Index, DenseIndex, SparseIndex, CandidateIndex: Int64;
  SparsePos: Integer;
begin
  Result := True;
  AFailedIndex := -1;
  Keys := AArray.GetOwnPropertyKeys;
  SparseIndices := TList<Int64>.Create;
  try
    for Key in Keys do
    begin
      if not TryParseArrayIndex(Key, Index) then
        Continue;
      if (Index >= ANewLength) and (Index < MAX_ARRAY_LENGTH) then
        SparseIndices.Add(Index);
    end;
    SparseIndices.Sort(TComparer<Int64>.Construct(CompareInt64));

    DenseIndex := AArray.FElements.Count - 1;
    SparsePos := SparseIndices.Count - 1;
    while (DenseIndex >= ANewLength) or (SparsePos >= 0) do
    begin
      if SparsePos >= 0 then
        SparseIndex := SparseIndices[SparsePos]
      else
        SparseIndex := -1;

      if (SparseIndex > DenseIndex) or (DenseIndex < ANewLength) then
        CandidateIndex := SparseIndex
      else
        CandidateIndex := DenseIndex;

      if (SparsePos >= 0) and (SparseIndex = CandidateIndex) then
      begin
        if not AArray.DeleteProperty(IntToStr(CandidateIndex)) then
        begin
          AFailedIndex := CandidateIndex;
          Exit(False);
        end;
        Dec(SparsePos);
      end;

      if (DenseIndex = CandidateIndex) and (DenseIndex >= ANewLength) then
      begin
        AArray.FElements.Delete(Integer(DenseIndex));
        Dec(DenseIndex);
      end;
    end;
  finally
    SparseIndices.Free;
  end;
end;

function HasPropertyMapArrayIndexAtOrAbove(const AArray: TGocciaArrayValue;
  const ANewLength: Int64): Boolean;
var
  Keys: TArray<string>;
  Key: string;
  Index: Int64;
begin
  Keys := AArray.FProperties.Keys;
  for Key in Keys do
    if TryParseArrayElementIndex(Key, Index) and (Index >= ANewLength) then
      Exit(True);
  Result := False;
end;

function IsArrayLengthDescriptorCompatible(
  const AArray: TGocciaArrayValue;
  const ADescriptor: TGocciaPropertyDescriptor): Boolean;
begin
  if ADescriptor.HasConfigurableField and ADescriptor.Configurable then
    Exit(False);
  if ADescriptor.HasEnumerableField and ADescriptor.Enumerable then
    Exit(False);
  if IsAccessorDescriptor(ADescriptor) then
    Exit(False);
  if ADescriptor.HasWritableField and ADescriptor.Writable and
     not AArray.FLengthWritable then
    Exit(False);
  Result := True;
end;

function ArrayLengthFromDescriptorValue(const AValue: TGocciaValue): Int64;
var
  NewLen: Cardinal;
  NumberValue: TGocciaNumberLiteralValue;
begin
  NewLen := ToUint32Value(AValue);
  NumberValue := AValue.ToNumberLiteral;
  if NumberValue.IsNaN or NumberValue.IsInfinite or
     NumberValue.IsNegativeInfinity or
     (NumberValue.Value <> NewLen) then
    ThrowRangeError(SErrorInvalidArrayLength, SSuggestArrayLengthRange);
  Result := NewLen;
end;

function TryApplyArrayLengthDescriptor(
  const AArray: TGocciaArrayValue;
  const ADescriptor: TGocciaPropertyDescriptor): Boolean;
var
  NewLen: Int64;
  FailedIndex: Int64;
  NewWritable: Boolean;
begin
  if AArray.FLength < AArray.FElements.Count then
    AArray.FLength := AArray.FElements.Count;
  if ADescriptor.HasValue then
  begin
    // ES2026 §10.4.2.4 steps 3-5: value conversion and RangeError happen
    // before ordinary descriptor validation.
    NewLen := ArrayLengthFromDescriptorValue(
      TGocciaPropertyDescriptorData(ADescriptor).Value);
    if not IsArrayLengthDescriptorCompatible(AArray, ADescriptor) then
      Exit(False);
    if (NewLen <> AArray.FLength) and not AArray.FLengthWritable then
      Exit(False);

    // Grow/keep vs shrink must compare against the spec-visible length, not
    // the dense element count: sparse arrays (property-map indices) keep
    // FLength above FElements.Count, and a shrink must still delete those
    // map-stored indices via DeleteArrayIndexesAtOrAbove below.
    if NewLen >= AArray.FLength then
    begin
      // Only materialize dense holes when the new length stays within the
      // dense extension gap; a far-out length leaves the tail sparse, the
      // same storage a far index assignment uses.
      if (NewLen <= MaxInt) and
         (NewLen <= Int64(AArray.FElements.Count) + MAX_DENSE_EXTENSION_GAP) then
        ExtendElementsWithHoles(AArray.FElements, NewLen);
      AArray.FLength := NewLen;
      if ADescriptor.HasWritableField and not ADescriptor.Writable then
        AArray.FLengthWritable := False;
      Exit(True);
    end;

    NewWritable := not (ADescriptor.HasWritableField and not ADescriptor.Writable);
    if not HasPropertyMapArrayIndexAtOrAbove(AArray, NewLen) and
       (NewLen <= AArray.FElements.Count) then
    begin
      AArray.FElements.Count := Integer(NewLen);
      AArray.FLength := NewLen;
      if not NewWritable then
        AArray.FLengthWritable := False;
      Exit(True);
    end;
    if not DeleteArrayIndexesAtOrAbove(AArray, NewLen, FailedIndex) then
    begin
      if CanStoreDenseElementIndex(FailedIndex, AArray.FElements.Count) then
        ExtendElementsWithHoles(AArray.FElements, FailedIndex + 1);
      AArray.FLength := FailedIndex + 1;
      if not NewWritable then
        AArray.FLengthWritable := False;
      Exit(False);
    end;
    AArray.FLength := NewLen;
    if not NewWritable then
      AArray.FLengthWritable := False;
    Exit(True);
  end;

  if not IsArrayLengthDescriptorCompatible(AArray, ADescriptor) then
    Exit(False);
  if ADescriptor.HasWritableField and not ADescriptor.Writable then
    AArray.FLengthWritable := False;
  Result := True;
end;

function CanKeepDenseArrayElement(
  const ADescriptor: TGocciaPropertyDescriptor): Boolean;
begin
  Result := IsDataDescriptor(ADescriptor) and
    (not ADescriptor.HasConfigurableField or ADescriptor.Configurable) and
    (not ADescriptor.HasEnumerableField or ADescriptor.Enumerable) and
    (not ADescriptor.HasWritableField or ADescriptor.Writable);
end;

function CanCreateDenseArrayElement(
  const ADescriptor: TGocciaPropertyDescriptor): Boolean;
begin
  Result := IsDataDescriptor(ADescriptor) and ADescriptor.HasValue and
    ADescriptor.HasConfigurableField and ADescriptor.Configurable and
    ADescriptor.HasEnumerableField and ADescriptor.Enumerable and
    ADescriptor.HasWritableField and ADescriptor.Writable;
end;

procedure MaterializeDenseArrayElement(
  const AArray: TGocciaArrayValue;
  const AName: string;
  const AIndex: Integer);
begin
  if (AIndex >= 0) and (AIndex < AArray.FElements.Count) and
     (AArray.FElements[AIndex] <> TGocciaHoleValue.HoleValue) and
     not AArray.FProperties.ContainsKey(AName) then
  begin
    AArray.FProperties.Add(AName, TGocciaPropertyDescriptorData.Create(
      AArray.FElements[AIndex], [pfEnumerable, pfConfigurable, pfWritable]));
    AArray.FElements[AIndex] := TGocciaHoleValue.HoleValue;
  end;
end;

function CollectSparseIndicesInRange(const AObj: TGocciaObjectValue;
  const AStartInclusive, AEndExclusive: Int64): TArray<Int64>;
var
  Seen: TDictionary<Int64, Boolean>;
  Found: TList<Int64>;
  Current: TGocciaObjectValue;
  Keys: TArray<string>;
  Key: string;
  Idx: Int64;
  I: Integer;
  KeyScanCounter: Integer;
begin
  if AStartInclusive >= AEndExclusive then
  begin
    SetLength(Result, 0);
    Exit;
  end;
  Seen := TDictionary<Int64, Boolean>.Create;
  Found := TList<Int64>.Create;
  KeyScanCounter := 0;
  try
    Current := AObj;
    while Assigned(Current) do
    begin
      // Walking the prototype chain on a frozen-large array-like is bounded
      // by the actual key count, but each layer still incurs property-map
      // enumeration; poll the cooperative timeout once per layer.
      CheckExecutionTimeout;
      Keys := Current.GetOwnPropertyKeys;
      for Key in Keys do
      begin
        // Per-layer poll above only fires once per prototype hop; a single
        // layer with millions of own keys would otherwise iterate
        // uninterrupted.  Mask-poll every 1024 keys keeps the cooperative
        // timeout reachable without dominating throughput.
        Inc(KeyScanCounter);
        if (KeyScanCounter and 1023) = 0 then
          CheckExecutionTimeout;
        if not TryParseArrayIndex(Key, Idx) then Continue;
        if (Idx < AStartInclusive) or (Idx >= AEndExclusive) then Continue;
        if Seen.ContainsKey(Idx) then Continue;
        Seen.Add(Idx, True);
        Found.Add(Idx);
      end;
      Current := Current.Prototype;
    end;
    // Sorting a large Found can itself stall; poll once before handing off
    // to TList.Sort so the caller's deadline is not bypassed by the sort.
    CheckExecutionTimeout;
    Found.Sort(TComparer<Int64>.Construct(CompareInt64));
    SetLength(Result, Found.Count);
    for I := 0 to Found.Count - 1 do
      Result[I] := Found[I];
  finally
    Seen.Free;
    Found.Free;
  end;
end;

// ES2026 §7.3.35 ArraySpeciesCreate(originalArray, length)
function ArrayCreateWithLength(const ALength: Int64): TGocciaArrayValue;
begin
  Result := TGocciaArrayValue.Create;
  if ALength > 0 then
    Result.SetProperty(PROP_LENGTH, TGocciaNumberLiteralValue.Create(Int64ToDouble(ALength)));
end;

function ArraySpeciesCreate(const AOriginal: TGocciaObjectValue; const ALength: Int64): TGocciaObjectValue;
var
  ConstructorMethod, Species: TGocciaValue;
  LengthArgs: TGocciaArgumentsCollection;
  Instance: TGocciaValue;
begin
  // Step 3: Let C be Get(originalArray, "constructor")
  ConstructorMethod := AOriginal.GetProperty(PROP_CONSTRUCTOR);

  // Steps 4-5: If C is an object, get @@species; if null/undefined, fall through
  Species := ConstructorMethod;
  if ConstructorMethod is TGocciaObjectValue then
  begin
    // Cross-realm arrays whose constructor is the other realm's intrinsic
    // Array constructor fall back to the current realm's %Array%.
    if (ConstructorMethod is TGocciaArrayClassValue) and
       (TGocciaArrayClassValue(ConstructorMethod).CreationRealm <> CurrentRealm) then
      Species := TGocciaUndefinedLiteralValue.UndefinedValue
    else
      Species := TGocciaObjectValue(ConstructorMethod).GetSymbolProperty(
        TGocciaSymbolValue.WellKnownSpecies);

    if (Species is TGocciaUndefinedLiteralValue) or (Species is TGocciaNullLiteralValue) then
    begin
      // Step 6: If C is undefined, return ArrayCreate(length)
      Result := ArrayCreateWithLength(ALength);
      Exit;
    end;
  end;

  // Step 6: If C is undefined, return ArrayCreate(length)
  if (Species is TGocciaUndefinedLiteralValue) then
    Exit(ArrayCreateWithLength(ALength));

  // Step 7: If IsConstructor(C) is false, throw TypeError
  if (not Assigned(Species)) or not Species.IsConstructable then
    ThrowTypeError(SErrorSpeciesNotConstructor,
      SSuggestSpeciesConstructor);

  // Step 8: Return Construct(C, « length »)
  LengthArgs := TGocciaArgumentsCollection.Create([TGocciaNumberLiteralValue.Create(Int64ToDouble(ALength))]);
  try
    Instance := ConstructValue(Species, LengthArgs, Species);
  finally
    LengthArgs.Free;
  end;

  if not (Instance is TGocciaObjectValue) then
    ThrowTypeError(SErrorSpeciesNotConstructor, SSuggestSpeciesConstructor);

  Result := TGocciaObjectValue(Instance);
end;


function CallCompareFunc(const ACompareFunc: TGocciaValue; const ACallArgs: TGocciaArgumentsCollection;
  const A, B: TGocciaValue; const AThisValue: TGocciaValue): Double;
var
  CompResult: TGocciaNumberLiteralValue;
  PreviousContinuation: TGocciaGeneratorContinuation;
  CompareRoot, AValueRoot, BValueRoot, ThisRoot: TGocciaTempRoot;
begin
  if A is TGocciaUndefinedLiteralValue then
  begin
    if B is TGocciaUndefinedLiteralValue then
      Exit(0);
    Exit(1);
  end;
  if B is TGocciaUndefinedLiteralValue then
    Exit(-1);

  ACallArgs.SetElement(0, A);
  ACallArgs.SetElement(1, B);
  InitializeTempRoot(CompareRoot);
  InitializeTempRoot(AValueRoot);
  InitializeTempRoot(BValueRoot);
  InitializeTempRoot(ThisRoot);
  AddTempRootIfNeeded(CompareRoot, ACompareFunc);
  AddTempRootIfNeeded(AValueRoot, A);
  AddTempRootIfNeeded(BValueRoot, B);
  AddTempRootIfNeeded(ThisRoot, AThisValue);
  PreviousContinuation := SuspendCurrentGeneratorContinuation;
  try
    try
      CompResult := InvokeCallable(ACompareFunc, ACallArgs,
        TGocciaUndefinedLiteralValue.UndefinedValue).ToNumberLiteral;
    finally
      RestoreCurrentGeneratorContinuation(PreviousContinuation);
    end;
  finally
    RemoveTempRootIfNeeded(ThisRoot);
    RemoveTempRootIfNeeded(BValueRoot);
    RemoveTempRootIfNeeded(AValueRoot);
    RemoveTempRootIfNeeded(CompareRoot);
  end;

  if CompResult.IsNaN then
    Result := 0
  else if CompResult.IsInfinity then
    Result := 1
  else if CompResult.IsNegativeInfinity then
    Result := -1
  else
    Result := CompResult.Value;
end;

procedure StableSortElements(const AElements: TGocciaValueList; const ACompareFunc: TGocciaValue;
  const ACallArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue);
var
  Buffer: array of TGocciaValue;

  procedure SortRange(const ALo, AHi: Integer);
  var
    Mid, I, J, K: Integer;
  begin
    if AHi - ALo < 2 then
      Exit;

    Mid := ALo + ((AHi - ALo) div 2);
    SortRange(ALo, Mid);
    SortRange(Mid, AHi);

    I := ALo;
    J := Mid;
    K := ALo;
    while (I < Mid) and (J < AHi) do
    begin
      if CallCompareFunc(ACompareFunc, ACallArgs, AElements[I], AElements[J], AThisValue) <= 0 then
      begin
        Buffer[K] := AElements[I];
        Inc(I);
      end
      else
      begin
        Buffer[K] := AElements[J];
        Inc(J);
      end;
      Inc(K);
    end;
    while I < Mid do
    begin
      Buffer[K] := AElements[I];
      Inc(I);
      Inc(K);
    end;
    while J < AHi do
    begin
      Buffer[K] := AElements[J];
      Inc(J);
      Inc(K);
    end;

    for K := ALo to AHi - 1 do
      AElements[K] := Buffer[K];
  end;

begin
  if AElements.Count < 2 then
    Exit;

  SetLength(Buffer, AElements.Count);
  SortRange(0, AElements.Count);
end;

procedure StableSortElementsDefault(const AElements: TGocciaValueList);
var
  Buffer: array of TGocciaValue;

  procedure SortRange(const ALo, AHi: Integer);
  var
    Mid, I, J, K: Integer;
  begin
    if AHi - ALo < 2 then
      Exit;

    Mid := ALo + ((AHi - ALo) div 2);
    SortRange(ALo, Mid);
    SortRange(Mid, AHi);

    I := ALo;
    J := Mid;
    K := ALo;
    while (I < Mid) and (J < AHi) do
    begin
      if DefaultCompare(AElements[I], AElements[J]) <= 0 then
      begin
        Buffer[K] := AElements[I];
        Inc(I);
      end
      else
      begin
        Buffer[K] := AElements[J];
        Inc(J);
      end;
      Inc(K);
    end;
    while I < Mid do
    begin
      Buffer[K] := AElements[I];
      Inc(I);
      Inc(K);
    end;
    while J < AHi do
    begin
      Buffer[K] := AElements[J];
      Inc(J);
      Inc(K);
    end;

    for K := ALo to AHi - 1 do
      AElements[K] := Buffer[K];
  end;

begin
  if AElements.Count < 2 then
    Exit;

  SetLength(Buffer, AElements.Count);
  SortRange(0, AElements.Count);
end;

constructor TGocciaArrayValue.Create(const AClass: TGocciaClassValue = nil;
  const AElementCapacity: Integer = 0);
var
  SharedPrototype: TGocciaObjectValue;
begin
  inherited Create(AClass);
  FElements := TGocciaValueList.Create(False);
  FLength := 0;
  FLengthWritable := True;
  if AElementCapacity > 0 then
    FElements.Capacity := AElementCapacity;
  InitializePrototype;
  SharedPrototype := GetSharedArrayPrototype;
  if not Assigned(AClass) and Assigned(SharedPrototype) then
    FPrototype := SharedPrototype;
end;

procedure TGocciaArrayValue.InitializePrototype;
var
  Members: TGocciaMemberCollection;
  SharedPrototype, Unscopables: TGocciaObjectValue;
  PrototypeMembers: TArray<TGocciaMemberDefinition>;
  ValuesMethod: TGocciaValue;
begin
  // No realm yet - happens during very early bootstrap (e.g. PinSingletons
  // creating BigInt singletons before SetCurrentRealm runs on the engine).
  // Skip prototype init: the constructor will run again once the realm is
  // installed and there's something to bind to.
  if not Assigned(CurrentRealm) then Exit;
  if Assigned(GetSharedArrayPrototype) then Exit;

  SharedPrototype := Self;
  CurrentRealm.SetSlot(GArrayPrototypeSlot, SharedPrototype);
  // The native prototype methods below bind to this instance (Self).  Keep it
  // alive for the realm's lifetime in its own slot: the realm pins it here and
  // unpins it on Destroy, together with the prototype whose method pointers
  // embed it, so no per-thread pin outlives every realm. #892
  CurrentRealm.SetSlot(GArrayMethodHostSlot, Self);

  Members := TGocciaMemberCollection.Create;
  try
    Members.AddDataProperty(PROP_LENGTH, TGocciaNumberLiteralValue.ZeroValue, [pfWritable]);
    Members.AddMethod(ArrayMap, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddMethod(ArrayFilter, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddMethod(ArrayReduce, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddMethod(ArrayReduceRight, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddMethod(ArrayForEach, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddMethod(ArraySome, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddMethod(ArrayEvery, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddMethod(ArrayFlat, 0, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddMethod(ArrayFlatMap, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddMethod(ArrayJoin, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddMethod(ArrayToString, 0, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddMethod(ArrayIncludes, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddMethod(ArrayPush, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddMethod(ArrayPop, 0, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddMethod(ArraySlice, 2, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddMethod(ArrayFind, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddMethod(ArrayFindIndex, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddMethod(ArrayIndexOf, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddMethod(ArrayLastIndexOf, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddMethod(ArrayConcat, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddMethod(ArrayReverse, 0, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddMethod(ArrayToReversed, 0, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddMethod(ArrayToSorted, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddMethod(ArrayToSpliced, 2, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddMethod(ArraySort, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddMethod(ArraySplice, 2, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddMethod(ArrayShift, 0, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddMethod(ArrayUnshift, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddMethod(ArrayFill, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddMethod(ArrayAt, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddMethod(ArrayFindLast, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddMethod(ArrayFindLastIndex, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddMethod(ArrayWith, 2, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddMethod(ArrayCopyWithin, 2, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddMethod(ArrayValues, 0, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddMethod(ArrayKeys, 0, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddMethod(ArrayEntries, 0, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddSymbolMethod(
      TGocciaSymbolValue.WellKnownIterator,
      '[Symbol.iterator]',
      ArraySymbolIterator,
      0,
      [pfConfigurable, pfWritable]);
    PrototypeMembers := Members.ToDefinitions;
  finally
    Members.Free;
  end;
  RegisterMemberDefinitions(SharedPrototype, PrototypeMembers);
  ValuesMethod := SharedPrototype.GetProperty('values');
  SharedPrototype.DefineSymbolProperty(
    TGocciaSymbolValue.WellKnownIterator,
    TGocciaPropertyDescriptorData.Create(ValuesMethod, [pfConfigurable, pfWritable]));

  Unscopables := TGocciaObjectValue.Create(nil);
  Unscopables.DefineProperty('at',
    TGocciaPropertyDescriptorData.Create(TGocciaBooleanLiteralValue.TrueValue, [pfEnumerable, pfConfigurable, pfWritable]));
  Unscopables.DefineProperty('copyWithin',
    TGocciaPropertyDescriptorData.Create(TGocciaBooleanLiteralValue.TrueValue, [pfEnumerable, pfConfigurable, pfWritable]));
  Unscopables.DefineProperty('entries',
    TGocciaPropertyDescriptorData.Create(TGocciaBooleanLiteralValue.TrueValue, [pfEnumerable, pfConfigurable, pfWritable]));
  Unscopables.DefineProperty('fill',
    TGocciaPropertyDescriptorData.Create(TGocciaBooleanLiteralValue.TrueValue, [pfEnumerable, pfConfigurable, pfWritable]));
  Unscopables.DefineProperty('find',
    TGocciaPropertyDescriptorData.Create(TGocciaBooleanLiteralValue.TrueValue, [pfEnumerable, pfConfigurable, pfWritable]));
  Unscopables.DefineProperty('findIndex',
    TGocciaPropertyDescriptorData.Create(TGocciaBooleanLiteralValue.TrueValue, [pfEnumerable, pfConfigurable, pfWritable]));
  Unscopables.DefineProperty('findLast',
    TGocciaPropertyDescriptorData.Create(TGocciaBooleanLiteralValue.TrueValue, [pfEnumerable, pfConfigurable, pfWritable]));
  Unscopables.DefineProperty('findLastIndex',
    TGocciaPropertyDescriptorData.Create(TGocciaBooleanLiteralValue.TrueValue, [pfEnumerable, pfConfigurable, pfWritable]));
  Unscopables.DefineProperty('flat',
    TGocciaPropertyDescriptorData.Create(TGocciaBooleanLiteralValue.TrueValue, [pfEnumerable, pfConfigurable, pfWritable]));
  Unscopables.DefineProperty('flatMap',
    TGocciaPropertyDescriptorData.Create(TGocciaBooleanLiteralValue.TrueValue, [pfEnumerable, pfConfigurable, pfWritable]));
  Unscopables.DefineProperty('includes',
    TGocciaPropertyDescriptorData.Create(TGocciaBooleanLiteralValue.TrueValue, [pfEnumerable, pfConfigurable, pfWritable]));
  Unscopables.DefineProperty('keys',
    TGocciaPropertyDescriptorData.Create(TGocciaBooleanLiteralValue.TrueValue, [pfEnumerable, pfConfigurable, pfWritable]));
  Unscopables.DefineProperty('toReversed',
    TGocciaPropertyDescriptorData.Create(TGocciaBooleanLiteralValue.TrueValue, [pfEnumerable, pfConfigurable, pfWritable]));
  Unscopables.DefineProperty('toSorted',
    TGocciaPropertyDescriptorData.Create(TGocciaBooleanLiteralValue.TrueValue, [pfEnumerable, pfConfigurable, pfWritable]));
  Unscopables.DefineProperty('toSpliced',
    TGocciaPropertyDescriptorData.Create(TGocciaBooleanLiteralValue.TrueValue, [pfEnumerable, pfConfigurable, pfWritable]));
  Unscopables.DefineProperty('values',
    TGocciaPropertyDescriptorData.Create(TGocciaBooleanLiteralValue.TrueValue, [pfEnumerable, pfConfigurable, pfWritable]));
  SharedPrototype.DefineSymbolProperty(TGocciaSymbolValue.WellKnownUnscopables,
    TGocciaPropertyDescriptorData.Create(Unscopables, [pfConfigurable]));
end;

class procedure TGocciaArrayValue.ExposePrototype(const AConstructor: TGocciaValue);
var
  SharedPrototype: TGocciaObjectValue;
begin
  SharedPrototype := GetSharedArrayPrototype;
  if not Assigned(SharedPrototype) then
  begin
    TGocciaArrayValue.Create;
    SharedPrototype := GetSharedArrayPrototype;
  end;
  if AConstructor is TGocciaClassValue then
    TGocciaClassValue(AConstructor).ReplacePrototype(SharedPrototype)
  else if AConstructor is TGocciaObjectValue then
    TGocciaObjectValue(AConstructor).AssignProperty(PROP_PROTOTYPE, SharedPrototype);
  SharedPrototype.DefineProperty(PROP_CONSTRUCTOR,
    TGocciaPropertyDescriptorData.Create(AConstructor, [pfConfigurable, pfWritable]));
end;

destructor TGocciaArrayValue.Destroy;
begin
  FElements.Free;
  inherited;
end;

procedure TGocciaArrayValue.InitializeNativeFromArguments(const AArguments: TGocciaArgumentsCollection);
var
  LenArg: TGocciaValue;
  Len: Double;
  IntLen: Int64;
  I: Integer;
begin
  if AArguments.Length = 0 then
    Exit;

  if AArguments.Length = 1 then
  begin
    LenArg := AArguments.GetElement(0);
    if LenArg is TGocciaNumberLiteralValue then
    begin
      Len := TGocciaNumberLiteralValue(LenArg).Value;
      if (Len <> Trunc(Len)) or (Len < 0) or (Len > MAX_ARRAY_LENGTH_F) then
        ThrowRangeError(SErrorInvalidArrayLength,
          SSuggestArrayLengthRange);
      IntLen := Trunc(Len);
      FLength := IntLen;
      if IntLen > MaxInt then
        Exit;
      ExtendElementsWithHoles(FElements, IntLen);
    end
    else
    begin
      FElements.Add(LenArg);
      FLength := FElements.Count;
    end;
  end
  else
  begin
    FElements.Capacity := AArguments.Length;
    for I := 0 to AArguments.Length - 1 do
      FElements.Add(AArguments.GetElement(I));
    FLength := FElements.Count;
  end;
end;

procedure TGocciaArrayValue.MarkReferences;
var
  I: Integer;
begin
  if GCMarked then Exit;
  inherited;

  for I := 0 to FElements.Count - 1 do
  begin
    if Assigned(FElements[I]) then
      FElements[I].MarkReferences;
  end;
end;

procedure TGocciaArrayValue.Freeze;
begin
  inherited Freeze;
  FLengthWritable := False;
end;

function TGocciaArrayValue.ToStringTag: string;
begin
  Result := 'Array';
end;

procedure TGocciaArrayValue.ThrowError(const AMessage: string; const AArgs: array of const);
begin
  raise TGocciaError.Create(Format(AMessage, AArgs), 0, 0, '', nil);
end;

procedure TGocciaArrayValue.ThrowError(const AMessage: string);
begin
  ThrowError(AMessage, []);
end;

procedure TGocciaArrayValue.ThrowError(const AMessage: string; const AArgs: array of const; const ASuggestion: string);
begin
  raise TGocciaError.Create(Format(AMessage, AArgs), 0, 0, '', nil, ASuggestion);
end;

function TGocciaArrayValue.GetLengthProperty(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  if TGocciaArrayValue(AThisValue).FLength < TGocciaArrayValue(AThisValue).Elements.Count then
    TGocciaArrayValue(AThisValue).FLength := TGocciaArrayValue(AThisValue).Elements.Count;
  Result := TGocciaNumberLiteralValue.Create(TGocciaArrayValue(AThisValue).FLength);
end;

// Index-based element access implementations
function TGocciaArrayValue.GetLength: Integer;
begin
  if FLength < FElements.Count then
    FLength := FElements.Count;
  Result := FElements.Count;
end;

function TGocciaArrayValue.HasDenseElementLength: Boolean;
begin
  if FLength < FElements.Count then
    FLength := FElements.Count;
  Result := FLength = FElements.Count;
end;

function TGocciaArrayValue.GetElement(const AIndex: Integer): TGocciaValue;
begin
  if (AIndex >= 0) and (AIndex < FElements.Count) then
    Result := FElements[AIndex]
  else
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaArrayValue.SetElement(const AIndex: Integer; const AValue: TGocciaValue): Boolean;
var
  Accessor: TGocciaPropertyDescriptorAccessor;
  Args: TGocciaArgumentsCollection;
  Descriptor: TGocciaPropertyDescriptor;
  IndexName: string;
begin
  if AIndex < 0 then
  begin
    Result := False;
    Exit;
  end;

  IndexName := IntToStr(AIndex);
  Descriptor := GetOwnPropertyDescriptor(IndexName);
  if Descriptor is TGocciaPropertyDescriptorAccessor then
  begin
    Accessor := TGocciaPropertyDescriptorAccessor(Descriptor);
    if Assigned(Accessor.Setter) and Accessor.Setter.IsCallable then
    begin
      Args := TGocciaArgumentsCollection.Create([AValue]);
      try
        InvokeCallable(Accessor.Setter, Args, Self);
      finally
        Args.Free;
      end;
      Result := True;
      Exit;
    end;
    ThrowTypeError(Format(SErrorSetPropertyOnlyGetter, [IndexName, ToStringTag]),
      SSuggestPropertyHasOnlyGetter);
  end;

  Descriptor := inherited GetOwnPropertyDescriptor(IndexName);
  if Assigned(Descriptor) then
  begin
    inherited AssignProperty(IndexName, AValue);
    if AIndex + 1 > FLength then
      FLength := AIndex + 1;
    Result := True;
    Exit;
  end;

  if Assigned(FPrototype) then
  begin
    Result := FPrototype.AssignPropertyWithReceiver(IndexName, AValue, Self);
    Exit;
  end;

  if FFrozen and (AIndex < FElements.Count) and
     not IsArrayHole(FElements[AIndex]) then
  begin
    Result := False;
    Exit;
  end;

  if (AIndex >= FLength) and not FLengthWritable then
  begin
    Result := False;
    Exit;
  end;

  if (not Extensible) and
     not ((AIndex < FElements.Count) and not IsArrayHole(FElements[AIndex])) then
  begin
    Result := False;
    Exit;
  end;

  if AIndex = FElements.Count then
    FElements.Add(AValue)
  else
  begin
    ExtendElementsWithHoles(FElements, Int64(AIndex) + 1);
    FElements[AIndex] := AValue;
  end;
  if AIndex + 1 > FLength then
    FLength := AIndex + 1;
  Result := True;
end;

procedure TGocciaArrayValue.SetIndexProperty(const AIndex: Integer; const AValue: TGocciaValue);
var
  Accessor: TGocciaPropertyDescriptorAccessor;
  Args: TGocciaArgumentsCollection;
  Descriptor: TGocciaPropertyDescriptor;
  IndexName: string;
begin
  if AIndex < 0 then
  begin
    SetProperty(IntToStr(AIndex), AValue);
    Exit;
  end;

  IndexName := IntToStr(AIndex);
  Descriptor := GetOwnPropertyDescriptor(IndexName);
  if Descriptor is TGocciaPropertyDescriptorAccessor then
  begin
    Accessor := TGocciaPropertyDescriptorAccessor(Descriptor);
    if Assigned(Accessor.Setter) and Accessor.Setter.IsCallable then
    begin
      Args := TGocciaArgumentsCollection.Create([AValue]);
      try
        InvokeCallable(Accessor.Setter, Args, Self);
      finally
        Args.Free;
      end;
      Exit;
    end;
    ThrowTypeError(Format(SErrorSetPropertyOnlyGetter, [IndexName, ToStringTag]),
      SSuggestPropertyHasOnlyGetter);
  end;

  Descriptor := inherited GetOwnPropertyDescriptor(IndexName);
  if Assigned(Descriptor) then
  begin
    inherited AssignProperty(IndexName, AValue);
    if Int64(AIndex) + 1 > FLength then
      FLength := Int64(AIndex) + 1;
    Exit;
  end;

  if FFrozen and (AIndex < FElements.Count) and
     not IsArrayHole(FElements[AIndex]) then
    ThrowTypeError(Format(SErrorCannotAssignReadOnly, [IntToStr(AIndex)]),
      SSuggestCannotDeleteNonConfigurable);

  if (AIndex >= FLength) and not FLengthWritable then
    ThrowTypeError(Format(SErrorCannotRedefineNonConfigurable, [IntToStr(AIndex)]),
      SSuggestCannotDeleteNonConfigurable);

  if (not Extensible) and
     not (CanStoreDenseElementIndex(AIndex, FElements.Count) and
          (AIndex < FElements.Count) and
          not IsArrayHole(FElements[AIndex])) then
    ThrowTypeError(Format(SErrorCannotAddPropertyNotExtensible,
      [IntToStr(AIndex)]), SSuggestObjectNotExtensible);

  if CanStoreDenseElementIndex(AIndex, FElements.Count) then
  begin
    if AIndex = FElements.Count then
      FElements.Add(AValue)
    else
    begin
      ExtendElementsWithHoles(FElements, Int64(AIndex) + 1);
      FElements[AIndex] := AValue;
    end;
  end
  else
    inherited DefineProperty(IntToStr(AIndex),
      TGocciaPropertyDescriptorData.Create(AValue,
        [pfEnumerable, pfConfigurable, pfWritable]));

  if Int64(AIndex) + 1 > FLength then
    FLength := Int64(AIndex) + 1;
end;

function TGocciaArrayValue.ValidateArrayMethodCall(const AMethodName: string; const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue; const ARequiresCallback: Boolean = True): TGocciaValue;
begin
  // ToObject check moved to each caller via TArrayLikeView.Init — no array
  // type guard here so that generic receivers are accepted per ES spec.

  if AArgs.Length < 1 then
    ThrowTypeError(Format(SErrorArrayMethodExpectsCallback, [AMethodName]), SSuggestIteratorCallable);

  Result := AArgs.GetElement(0);

  if ARequiresCallback then
  begin
    if not Result.IsCallable then
      ThrowTypeError(SErrorCallbackMustBeFunction, SSuggestIteratorCallable);
  end
  else
  begin
    if Result is TGocciaUndefinedLiteralValue then
      ThrowTypeError(SErrorCallbackMustNotBeUndefined, SSuggestIteratorCallable);
  end;
end;

function TGocciaArrayValue.IsArrayHole(const AElement: TGocciaValue): Boolean;
begin
  Result := AElement = TGocciaHoleValue.HoleValue;
end;

function TGocciaArrayValue.ArrayMap(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  View: TArrayLikeView;
  ResultArray: TGocciaObjectValue;
  Callback: TGocciaValue;
  TypedCallback: TGocciaFunctionBase;
  ThisArg: TGocciaValue;
  CallArgs: TGocciaArrayCallbackArgs;
  I: Integer;
  ResultRoot: TGocciaTempRoot;
begin
  View.Init(AThisValue);
  Callback := ValidateArrayMethodCall('map', AArgs, AThisValue, True);
  ThisArg := GetArrayCallbackThisArg(AArgs);
  if Assigned(View.SpeciesArr) then
    ResultArray := ArraySpeciesCreate(View.Obj, View.Len64)
  else
  begin
    // Step 5: Let A be ArraySpeciesCreate(O, len) — default ArrayCreate
    // throws RangeError if len > 2^32 - 1.
    View.CheckArrayCreateLen;
    ResultArray := ArrayCreateWithLength(View.Len);
  end;
  InitializeTempRoot(ResultRoot);
  AddTempRootIfNeeded(ResultRoot, ResultArray);
  try
    TypedCallback := nil;
    if Callback is TGocciaFunctionBase then
      TypedCallback := TGocciaFunctionBase(Callback);

    CallArgs := TGocciaArrayCallbackArgs.Create(View.Obj);
    try
      for I := 0 to View.Len - 1 do
      begin
        if not View.HasIndex(I) then
          Continue;

        CallArgs.Element := View.Get(I);
        CallArgs.Index := TGocciaNumberLiteralValue.Create(I);
        ArrayCreateDataProperty(ResultArray, I,
          InvokeArrayCallback(Callback, TypedCallback, CallArgs, ThisArg));
      end;
    finally
      CallArgs.Free;
    end;

    Result := ResultArray;
  finally
    RemoveTempRootIfNeeded(ResultRoot);
  end;
end;

// ES2026 §23.1.3.8 Array.prototype.filter(callbackfn [, thisArg])
function TGocciaArrayValue.ArrayFilter(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  View: TArrayLikeView;
  Callback: TGocciaValue;
  TypedCallback: TGocciaFunctionBase;
  ThisArg: TGocciaValue;
  ResultArray: TGocciaObjectValue;
  CallArgs: TGocciaArrayCallbackArgs;
  PredicateResult, Element: TGocciaValue;
  I, ToIndex: Integer;
  Sparse: TArray<Int64>;
  K: Int64;
  ResultRoot: TGocciaTempRoot;
begin
  View.Init(AThisValue);
  Callback := ValidateArrayMethodCall('filter', AArgs, AThisValue, True);
  ThisArg := GetArrayCallbackThisArg(AArgs);
  if Assigned(View.SpeciesArr) then
    ResultArray := ArraySpeciesCreate(View.Obj, 0)
  else
    ResultArray := TGocciaArrayValue.Create;
  ToIndex := 0;
  InitializeTempRoot(ResultRoot);
  AddTempRootIfNeeded(ResultRoot, ResultArray);
  try

    TypedCallback := nil;
    if Callback is TGocciaFunctionBase then
      TypedCallback := TGocciaFunctionBase(Callback);

    CallArgs := TGocciaArrayCallbackArgs.Create(View.Obj);
    try
      if View.NeedsSparsePath then
      begin
        Sparse := CollectSparseIndicesInRange(View.Obj, 0, View.Len64);
        for K in Sparse do
        begin
          Element := View.Get64(K);
          CallArgs.Element := Element;
          CallArgs.Index := TGocciaNumberLiteralValue.Create(Int64ToDouble(K));
          PredicateResult := InvokeArrayCallback(Callback, TypedCallback, CallArgs, ThisArg);
          if PredicateResult.ToBooleanLiteral.Value then
          begin
            ArrayCreateDataProperty(ResultArray, ToIndex, Element);
            Inc(ToIndex);
          end;
        end;
      end
      else
      begin
        for I := 0 to View.Len - 1 do
        begin
          if not View.HasIndex(I) then
            Continue;

          Element := View.Get(I);
          CallArgs.Element := Element;
          CallArgs.Index := TGocciaNumberLiteralValue.Create(I);
          PredicateResult := InvokeArrayCallback(Callback, TypedCallback, CallArgs, ThisArg);

          if PredicateResult.ToBooleanLiteral.Value then
          begin
            ArrayCreateDataProperty(ResultArray, ToIndex, Element);
            Inc(ToIndex);
          end;
        end;
      end;
    finally
      CallArgs.Free;
    end;

    Result := ResultArray;
  finally
    RemoveTempRootIfNeeded(ResultRoot);
  end;
end;

// ES2026 §23.1.3.22 Array.prototype.reduce(callbackfn [, initialValue])
function TGocciaArrayValue.ArrayReduce(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  View: TArrayLikeView;
  Callback: TGocciaValue;
  TypedCallback: TGocciaFunctionBase;
  Accumulator: TGocciaValue;
  CallArgs: TGocciaReduceCallbackArgs;
  I, StartIndex: Integer;
  Sparse: TArray<Int64>;
  SparseStart: Integer;
  J: Integer;
begin
  View.Init(AThisValue);
  Callback := ValidateArrayMethodCall('reduce', AArgs, AThisValue, True);

  TypedCallback := nil;
  if Callback is TGocciaFunctionBase then
    TypedCallback := TGocciaFunctionBase(Callback);

  if View.NeedsSparsePath then
  begin
    Sparse := CollectSparseIndicesInRange(View.Obj, 0, View.Len64);
    if AArgs.Length >= 2 then
    begin
      Accumulator := AArgs.GetElement(1);
      SparseStart := 0;
    end
    else
    begin
      if Length(Sparse) = 0 then
        ThrowTypeError(SErrorReduceEmptyArray,
          SSuggestReduceInitialValue);
      Accumulator := View.Get64(Sparse[0]);
      SparseStart := 1;
    end;

    CallArgs := TGocciaReduceCallbackArgs.Create(View.Obj);
    try
      for J := SparseStart to High(Sparse) do
      begin
        CallArgs.Accumulator := Accumulator;
        CallArgs.Element := View.Get64(Sparse[J]);
        CallArgs.Index := TGocciaNumberLiteralValue.Create(Int64ToDouble(Sparse[J]));
        Accumulator := InvokeArrayCallback(Callback, TypedCallback, CallArgs,
          TGocciaUndefinedLiteralValue.UndefinedValue);
      end;
    finally
      CallArgs.Free;
    end;

    Result := Accumulator;
    Exit;
  end;

  if AArgs.Length >= 2 then
  begin
    Accumulator := AArgs.GetElement(1);
    StartIndex := 0;
  end
  else
  begin
    // ES2026 §23.1.3.22 steps 6-8: scan for first present element
    StartIndex := -1;
    for I := 0 to View.Len - 1 do
    begin
      if View.HasIndex(I) then
      begin
        Accumulator := View.Get(I);
        StartIndex := I + 1;
        Break;
      end;
    end;
    if StartIndex < 0 then
      ThrowTypeError(SErrorReduceEmptyArray,
        SSuggestReduceInitialValue);
  end;

  CallArgs := TGocciaReduceCallbackArgs.Create(View.Obj);
  try
    for I := StartIndex to View.Len - 1 do
    begin
      if not View.HasIndex(I) then
        Continue;

      CallArgs.Accumulator := Accumulator;
      CallArgs.Element := View.Get(I);
      CallArgs.Index := TGocciaNumberLiteralValue.Create(I);
      Accumulator := InvokeArrayCallback(Callback, TypedCallback, CallArgs,
        TGocciaUndefinedLiteralValue.UndefinedValue);
    end;
  finally
    CallArgs.Free;
  end;

  Result := Accumulator;
end;

// ES2026 §23.1.3.25 Array.prototype.reduceRight(callbackfn [, initialValue])
function TGocciaArrayValue.ArrayReduceRight(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  View: TArrayLikeView;
  Callback: TGocciaValue;
  TypedCallback: TGocciaFunctionBase;
  Accumulator: TGocciaValue;
  CallArgs: TGocciaReduceCallbackArgs;
  I, StartIndex: Integer;
  Sparse: TArray<Int64>;
  SparseEnd: Integer;
  J: Integer;
begin
  View.Init(AThisValue);
  Callback := ValidateArrayMethodCall('reduceRight', AArgs, AThisValue, True);

  TypedCallback := nil;
  if Callback is TGocciaFunctionBase then
    TypedCallback := TGocciaFunctionBase(Callback);

  if View.NeedsSparsePath then
  begin
    Sparse := CollectSparseIndicesInRange(View.Obj, 0, View.Len64);
    if AArgs.Length >= 2 then
    begin
      Accumulator := AArgs.GetElement(1);
      SparseEnd := High(Sparse);
    end
    else
    begin
      if Length(Sparse) = 0 then
        ThrowTypeError(SErrorReduceEmptyArray,
          SSuggestReduceInitialValue);
      Accumulator := View.Get64(Sparse[High(Sparse)]);
      SparseEnd := High(Sparse) - 1;
    end;

    CallArgs := TGocciaReduceCallbackArgs.Create(View.Obj);
    try
      for J := SparseEnd downto 0 do
      begin
        CallArgs.Accumulator := Accumulator;
        CallArgs.Element := View.Get64(Sparse[J]);
        CallArgs.Index := TGocciaNumberLiteralValue.Create(Int64ToDouble(Sparse[J]));
        Accumulator := InvokeArrayCallback(Callback, TypedCallback, CallArgs,
          TGocciaUndefinedLiteralValue.UndefinedValue);
      end;
    finally
      CallArgs.Free;
    end;

    Result := Accumulator;
    Exit;
  end;

  if AArgs.Length >= 2 then
  begin
    Accumulator := AArgs.GetElement(1);
    StartIndex := View.Len - 1;
  end
  else
  begin
    // ES2026 §23.1.3.25 steps 6-8: scan for last present element
    StartIndex := -2;
    for I := View.Len - 1 downto 0 do
    begin
      if View.HasIndex(I) then
      begin
        Accumulator := View.Get(I);
        StartIndex := I - 1;
        Break;
      end;
    end;
    if StartIndex = -2 then
      ThrowTypeError(SErrorReduceEmptyArray,
        SSuggestReduceInitialValue);
  end;

  CallArgs := TGocciaReduceCallbackArgs.Create(View.Obj);
  try
    for I := StartIndex downto 0 do
    begin
      if not View.HasIndex(I) then
        Continue;

      CallArgs.Accumulator := Accumulator;
      CallArgs.Element := View.Get(I);
      CallArgs.Index := TGocciaNumberLiteralValue.Create(I);
      Accumulator := InvokeArrayCallback(Callback, TypedCallback, CallArgs,
        TGocciaUndefinedLiteralValue.UndefinedValue);
    end;
  finally
    CallArgs.Free;
  end;

  Result := Accumulator;
end;

// ES2026 §23.1.3.12 Array.prototype.forEach(callbackfn [, thisArg])
function TGocciaArrayValue.ArrayForEach(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  View: TArrayLikeView;
  Callback: TGocciaValue;
  TypedCallback: TGocciaFunctionBase;
  ThisArg: TGocciaValue;
  CallArgs: TGocciaArrayCallbackArgs;
  I: Integer;
  Sparse: TArray<Int64>;
  K: Int64;
begin
  View.Init(AThisValue);
  Callback := ValidateArrayMethodCall('forEach', AArgs, AThisValue, True);
  ThisArg := GetArrayCallbackThisArg(AArgs);

  TypedCallback := nil;
  if Callback is TGocciaFunctionBase then
    TypedCallback := TGocciaFunctionBase(Callback);

  CallArgs := TGocciaArrayCallbackArgs.Create(View.Obj);
  try
    if View.NeedsSparsePath then
    begin
      Sparse := CollectSparseIndicesInRange(View.Obj, 0, View.Len64);
      for K in Sparse do
      begin
        CallArgs.Element := View.Get64(K);
        CallArgs.Index := TGocciaNumberLiteralValue.Create(Int64ToDouble(K));
        InvokeArrayCallback(Callback, TypedCallback, CallArgs, ThisArg);
      end;
    end
    else
    begin
      for I := 0 to View.Len - 1 do
      begin
        if not View.HasIndex(I) then
          Continue;

        CallArgs.Element := View.Get(I);
        CallArgs.Index := TGocciaNumberLiteralValue.Create(I);
        InvokeArrayCallback(Callback, TypedCallback, CallArgs, ThisArg);
      end;
    end;
  finally
    CallArgs.Free;
  end;

  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

// ES2026 §23.1.3.16 Array.prototype.join(separator)
function TGocciaArrayValue.ArrayJoin(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  View: TArrayLikeView;
  Separator: string;
  I: Integer;
  ResultString: string;
  Element: TGocciaValue;
begin
  // Step 1: Let O be ToObject(this value)
  View.Init(AThisValue);

  // Step 3: If separator is undefined, let sep be ","
  // Step 4: Else let sep be ToString(separator)
  if (AArgs.Length < 1) or (AArgs.GetElement(0) is TGocciaUndefinedLiteralValue) then
    Separator := ','
  else
    Separator := AArgs.GetElement(0).ToStringLiteral.Value;

  // Steps 5-8: Build result string, treating undefined/null elements as empty string
  ResultString := '';
  for I := 0 to View.Len - 1 do
  begin
    if I > 0 then
      ResultString := ResultString + Separator;
    // Step 7b: If element is undefined or null, let next be ""
    Element := View.Get(I);
    if (Element is TGocciaUndefinedLiteralValue) or
       (Element is TGocciaNullLiteralValue) then
      Continue
    else
      // ES2026 §23.1.3.16 step 7c: Let next be ? ToString(element)
      ResultString := ResultString + Element.ToStringLiteral.Value;
  end;

  // Step 9: Return R
  Result := TGocciaStringLiteralValue.Create(ResultString);
end;

// ES2026 §23.1.3.35 Array.prototype.toString()
function TGocciaArrayValue.ArrayToString(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  EmptyArgs: TGocciaArgumentsCollection;
  Obj: TGocciaObjectValue;
  JoinMethod: TGocciaValue;
begin
  Obj := ToObject(AThisValue);
  EmptyArgs := TGocciaArgumentsCollection.Create([]);
  try
    JoinMethod := Obj.GetPropertyWithContext('join', AThisValue);
    if Assigned(JoinMethod) and JoinMethod.IsCallable then
      Result := InvokeCallable(JoinMethod, EmptyArgs, AThisValue)
    else
      Result := Obj.ObjectPrototypeToString(EmptyArgs, AThisValue);
  finally
    EmptyArgs.Free;
  end;
end;

// ES2026 §23.1.3.14 Array.prototype.includes(searchElement [, fromIndex])
function TGocciaArrayValue.ArrayIncludes(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  View: TArrayLikeView;
  SearchValue, Element: TGocciaValue;
  FromIndex, I: Integer;
  Len64Val, FromIndex64, K: Int64;
  Sparse: TArray<Int64>;
begin
  // Step 1: Let O be ToObject(this value)
  View.Init(AThisValue);

  // Step 3: If len is 0, return false — before ToIntegerOrInfinity(fromIndex).
  if View.Len = 0 then
  begin
    Result := TGocciaBooleanLiteralValue.FalseValue;
    Exit;
  end;

  if AArgs.Length < 1 then
    SearchValue := TGocciaUndefinedLiteralValue.UndefinedValue
  else
    SearchValue := AArgs.GetElement(0);

  // Sparse iteration path for generic array-likes whose claimed length
  // exceeds 2^32 - 1.  Includes uses SameValueZero on Get(O, k) for every
  // index in [fromIndex, len); holes return undefined.  When SearchValue
  // is undefined and the range is non-empty, every non-existent slot
  // already produces a match, so the answer is True without enumerating.
  if View.NeedsSparsePath then
  begin
    Len64Val := View.Len64;
    FromIndex64 := ToInteger64FromArgs(AArgs, 1);
    if FromIndex64 < 0 then
      FromIndex64 := Len64Val + FromIndex64;
    if FromIndex64 < 0 then
      FromIndex64 := 0;
    if FromIndex64 >= Len64Val then
    begin
      Result := TGocciaBooleanLiteralValue.FalseValue;
      Exit;
    end;
    Sparse := CollectSparseIndicesInRange(View.Obj, FromIndex64, Len64Val);
    if SearchValue is TGocciaUndefinedLiteralValue then
    begin
      // For undefined we need to distinguish two sub-cases: a hole in the
      // window means Get(O, k) returns undefined for that k → true.  When
      // every index in [fromIndex, len) has an own property (e.g. fromIndex
      // narrows the window to a single materialised index) we still have
      // to compare the actual values, since they may not be undefined.
      if Int64(Length(Sparse)) < (Len64Val - FromIndex64) then
      begin
        Result := TGocciaBooleanLiteralValue.TrueValue;
        Exit;
      end;
    end;
    for K in Sparse do
      if IsSameValueZero(View.Get64(K), SearchValue) then
      begin
        Result := TGocciaBooleanLiteralValue.TrueValue;
        Exit;
      end;
    Result := TGocciaBooleanLiteralValue.FalseValue;
    Exit;
  end;

  // Steps 4-5: Let n be ToIntegerOrInfinity(fromIndex), compute k
  FromIndex := ToIntegerFromArgs(AArgs, 1);
  if FromIndex < 0 then
    FromIndex := View.Len + FromIndex;
  if FromIndex < 0 then
    FromIndex := 0;

  // Steps 6-8: Search using SameValueZero comparison
  for I := FromIndex to View.Len - 1 do
  begin
    Element := View.Get(I);
    if IsSameValueZero(Element, SearchValue) then
    begin
      Result := TGocciaBooleanLiteralValue.TrueValue;
      Exit;
    end;
  end;

  Result := TGocciaBooleanLiteralValue.FalseValue;
end;

// ES2026 §23.1.3.28 Array.prototype.some(callbackfn [, thisArg])
function TGocciaArrayValue.ArraySome(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  View: TArrayLikeView;
  Callback: TGocciaValue;
  TypedCallback: TGocciaFunctionBase;
  ThisArg: TGocciaValue;
  CallArgs: TGocciaArrayCallbackArgs;
  I: Integer;
  SomeResult: TGocciaValue;
  Sparse: TArray<Int64>;
  K: Int64;
begin
  View.Init(AThisValue);
  Callback := ValidateArrayMethodCall('some', AArgs, AThisValue, True);
  ThisArg := GetArrayCallbackThisArg(AArgs);

  TypedCallback := nil;
  if Callback is TGocciaFunctionBase then
    TypedCallback := TGocciaFunctionBase(Callback);

  CallArgs := TGocciaArrayCallbackArgs.Create(View.Obj);
  try
    if View.NeedsSparsePath then
    begin
      Sparse := CollectSparseIndicesInRange(View.Obj, 0, View.Len64);
      for K in Sparse do
      begin
        CallArgs.Element := View.Get64(K);
        CallArgs.Index := TGocciaNumberLiteralValue.Create(Int64ToDouble(K));
        SomeResult := InvokeArrayCallback(Callback, TypedCallback, CallArgs, ThisArg);
        if SomeResult.ToBooleanLiteral.Value then
        begin
          Result := TGocciaBooleanLiteralValue.TrueValue;
          Exit;
        end;
      end;
    end
    else
    begin
      for I := 0 to View.Len - 1 do
      begin
        if not View.HasIndex(I) then
          Continue;

        CallArgs.Element := View.Get(I);
        CallArgs.Index := TGocciaNumberLiteralValue.Create(I);
        SomeResult := InvokeArrayCallback(Callback, TypedCallback, CallArgs, ThisArg);
        if SomeResult.ToBooleanLiteral.Value then
        begin
          Result := TGocciaBooleanLiteralValue.TrueValue;
          Exit;
        end;
      end;
    end;
  finally
    CallArgs.Free;
  end;

  Result := TGocciaBooleanLiteralValue.FalseValue;
end;

// ES2026 §23.1.3.6 Array.prototype.every(callbackfn [, thisArg])
function TGocciaArrayValue.ArrayEvery(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  View: TArrayLikeView;
  Callback: TGocciaValue;
  TypedCallback: TGocciaFunctionBase;
  ThisArg: TGocciaValue;
  CallArgs: TGocciaArrayCallbackArgs;
  I: Integer;
  EveryResult: TGocciaValue;
  Sparse: TArray<Int64>;
  K: Int64;
begin
  View.Init(AThisValue);
  Callback := ValidateArrayMethodCall('every', AArgs, AThisValue, True);
  ThisArg := GetArrayCallbackThisArg(AArgs);

  TypedCallback := nil;
  if Callback is TGocciaFunctionBase then
    TypedCallback := TGocciaFunctionBase(Callback);

  CallArgs := TGocciaArrayCallbackArgs.Create(View.Obj);
  try
    if View.NeedsSparsePath then
    begin
      Sparse := CollectSparseIndicesInRange(View.Obj, 0, View.Len64);
      for K in Sparse do
      begin
        CallArgs.Element := View.Get64(K);
        CallArgs.Index := TGocciaNumberLiteralValue.Create(Int64ToDouble(K));
        EveryResult := InvokeArrayCallback(Callback, TypedCallback, CallArgs, ThisArg);
        if not EveryResult.ToBooleanLiteral.Value then
        begin
          Result := TGocciaBooleanLiteralValue.FalseValue;
          Exit;
        end;
      end;
    end
    else
    begin
      for I := 0 to View.Len - 1 do
      begin
        if not View.HasIndex(I) then
          Continue;

        CallArgs.Element := View.Get(I);
        CallArgs.Index := TGocciaNumberLiteralValue.Create(I);
        EveryResult := InvokeArrayCallback(Callback, TypedCallback, CallArgs, ThisArg);
        if not EveryResult.ToBooleanLiteral.Value then
        begin
          Result := TGocciaBooleanLiteralValue.FalseValue;
          Exit;
        end;
      end;
    end;
  finally
    CallArgs.Free;
  end;

  Result := TGocciaBooleanLiteralValue.TrueValue;
end;

// ES2026 §23.1.3.11.1 FlattenIntoArray(target, source, ..., depth)
procedure TGocciaArrayValue.FlattenInto(const ATarget: TGocciaObjectValue;
  const ADepth: Integer; var ATargetIndex: Integer);
var
  View: TArrayLikeView;
  I: Integer;
  Element: TGocciaValue;
begin
  // Use View-based iteration for prototype-aware access
  View.Init(Self);
  for I := 0 to View.Len - 1 do
  begin
    if not View.HasIndex(I) then
      Continue;

    Element := View.Get(I);
    // Step 3c-v: If depth > 0 and IsConcatSpreadable(element), recurse
    if (Element is TGocciaArrayValue) and (ADepth > 0) then
      TGocciaArrayValue(Element).FlattenInto(ATarget, ADepth - 1, ATargetIndex)
    else
    begin
      // Step 3c-v-2: Else, CreateDataPropertyOrThrow(target, targetIndex, element)
      ArrayCreateDataProperty(ATarget, ATargetIndex, Element);
      Inc(ATargetIndex);
    end;
  end;
end;

// ES2026 §23.1.3.11 Array.prototype.flat([depth])
function TGocciaArrayValue.ArrayFlat(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  View: TArrayLikeView;
  ResultArray: TGocciaObjectValue;
  Depth, I, TargetIndex: Integer;
  Element: TGocciaValue;
begin
  // Step 1: Let O be ToObject(this value)
  View.Init(AThisValue);
  // Step 3: Let depthNum be 1 (default)
  Depth := 1;

  // Step 4: If depth is not undefined, let depthNum be ToIntegerOrInfinity(depth)
  if (AArgs.Length > 0) and
     not (AArgs.GetElement(0) is TGocciaUndefinedLiteralValue) then
  begin
    Depth := ToIntegerValue(AArgs.GetElement(0));
    if Depth < 0 then
      Depth := 0;
  end;

  // Step 5: Let A be ArraySpeciesCreate(O, 0)
  if Assigned(View.SpeciesArr) then
    ResultArray := ArraySpeciesCreate(View.Obj, 0)
  else
    ResultArray := TGocciaArrayValue.Create;

  // Step 6: FlattenIntoArray via View for prototype-aware semantics
  TargetIndex := 0;
  for I := 0 to View.Len - 1 do
  begin
    if not View.HasIndex(I) then
      Continue;
    Element := View.Get(I);
    if (Element is TGocciaArrayValue) and (Depth > 0) then
      TGocciaArrayValue(Element).FlattenInto(ResultArray, Depth - 1, TargetIndex)
    else
    begin
      ArrayCreateDataProperty(ResultArray, TargetIndex, Element);
      Inc(TargetIndex);
    end;
  end;
  // Step 7: Return A
  Result := ResultArray;
end;

// ES2026 §23.1.3.10 Array.prototype.flatMap(mapperFunction [, thisArg])
function TGocciaArrayValue.ArrayFlatMap(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  View, MappedView: TArrayLikeView;
  ResultArray: TGocciaObjectValue;
  Callback: TGocciaValue;
  TypedCallback: TGocciaFunctionBase;
  ThisArg: TGocciaValue;
  CallArgs: TGocciaArrayCallbackArgs;
  I, J, TargetIndex: Integer;
  MappedValue: TGocciaValue;
  ResultRoot, MappedValueRoot: TGocciaTempRoot;
begin
  View.Init(AThisValue);
  Callback := ValidateArrayMethodCall('flatMap', AArgs, AThisValue, True);
  ThisArg := GetArrayCallbackThisArg(AArgs);
  if Assigned(View.SpeciesArr) then
    ResultArray := ArraySpeciesCreate(View.Obj, 0)
  else
    ResultArray := TGocciaArrayValue.Create;
  TargetIndex := 0;
  InitializeTempRoot(ResultRoot);
  InitializeTempRoot(MappedValueRoot);
  AddTempRootIfNeeded(ResultRoot, ResultArray);
  try

    TypedCallback := nil;
    if Callback is TGocciaFunctionBase then
      TypedCallback := TGocciaFunctionBase(Callback);

    CallArgs := TGocciaArrayCallbackArgs.Create(View.Obj);
    try
      for I := 0 to View.Len - 1 do
      begin
        if not View.HasIndex(I) then
          Continue;

        CallArgs.Element := View.Get(I);
        CallArgs.Index := TGocciaNumberLiteralValue.Create(I);
        MappedValue := InvokeArrayCallback(Callback, TypedCallback, CallArgs, ThisArg);
        try
          AddTempRootIfNeeded(MappedValueRoot, MappedValue);
          if MappedValue is TGocciaArrayValue then
          begin
            // Use View-based iteration for prototype-aware access
            MappedView.Init(MappedValue);
            for J := 0 to MappedView.Len - 1 do
            begin
              if MappedView.HasIndex(J) then
              begin
                ArrayCreateDataProperty(ResultArray, TargetIndex, MappedView.Get(J));
                Inc(TargetIndex);
              end;
            end;
          end
          else
          begin
            ArrayCreateDataProperty(ResultArray, TargetIndex, MappedValue);
            Inc(TargetIndex);
          end;
        finally
          RemoveTempRootIfNeeded(MappedValueRoot);
        end;
      end;
    finally
      CallArgs.Free;
    end;

    Result := ResultArray;
  finally
    RemoveTempRootIfNeeded(ResultRoot);
  end;
end;

// ES2026 §23.1.3.21 Array.prototype.push(...items)
function TGocciaArrayValue.ArrayPush(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  View: TArrayLikeView;
  NewLen, I: Integer;
  RawNewLen: Double;
  K: Int64;
begin
  // Step 1: Let O be ToObject(this value)
  View.Init(AThisValue);

  // Step 4: If len + argCount > 2^53 - 1, throw TypeError.
  // Skip the safety check on a zero-arg call: nothing is being added,
  // so a huge existing length can never overflow further.
  if AArgs.Length = 0 then
  begin
    // ES2026 step 6: Perform Set(O, "length", F(len), true).  For native
    // arrays length is intrinsic so the Set is implicit; for generic
    // receivers we still write so accessor/writability semantics fire.
    // Use RawLen so we don't truncate receivers whose length exceeds
    // MaxInt back down to View.Len.
    if Assigned(View.Arr) and View.Arr.FLengthWritable then
    begin
      Result := TGocciaNumberLiteralValue.Create(View.RawLen);
      Exit;
    end;
    View.Obj.AssignProperty(PROP_LENGTH,
      TGocciaNumberLiteralValue.Create(View.RawLen));
    Result := TGocciaNumberLiteralValue.Create(View.RawLen);
    Exit;
  end;

  RawNewLen := View.RawLen + AArgs.Length;
  if RawNewLen > MAX_SAFE_INTEGER_F then
    ThrowTypeError(
      'Array length exceeds maximum safe integer (2^53 - 1)',
      'use a smaller length');

  if RawNewLen > MaxInt then
  begin
    K := View.Len64;
    for I := 0 to AArgs.Length - 1 do
    begin
      View.Obj.AssignProperty(IntToStr(K), AArgs.GetElement(I));
      Inc(K);
    end;
    View.Obj.AssignProperty(PROP_LENGTH,
      TGocciaNumberLiteralValue.Create(RawNewLen));
    Result := TGocciaNumberLiteralValue.Create(RawNewLen);
    Exit;
  end;

  // Set(O, ToString(len), E), len = len + 1
  NewLen := View.Len;
  for I := 0 to AArgs.Length - 1 do
  begin
    View.Obj.AssignProperty(IntToStr(NewLen), AArgs.GetElement(I));
    Inc(NewLen);
  end;
  if (not Assigned(View.Arr)) or (View.Arr.FLength <> NewLen) then
    View.SetLen(NewLen);
  Result := TGocciaNumberLiteralValue.Create(NewLen);
end;

// ES2026 §23.1.3.20 Array.prototype.pop()
function TGocciaArrayValue.ArrayPop(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  View: TArrayLikeView;
  NewLen64: Int64;
begin
  // Step 1: Let O be ToObject(this value)
  View.Init(AThisValue);

  // Step 3: If len = 0, set length to 0 and return undefined
  if View.Len = 0 then
  begin
    View.SetLen(0);
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
    Exit;
  end;

  if View.RawLen > MaxInt then
  begin
    NewLen64 := View.Len64 - 1;
    Result := View.Get64(NewLen64);
    if not View.Obj.DeleteProperty(IntToStr(NewLen64)) then
      ThrowTypeError(Format(SErrorCannotRedefineNonConfigurable, [IntToStr(NewLen64)]),
        SSuggestCannotDeleteNonConfigurable);
    View.Obj.AssignProperty(PROP_LENGTH,
      TGocciaNumberLiteralValue.Create(Int64ToDouble(NewLen64)));
    Exit;
  end;

  // Step 5: Let element be Get(O, ToString(newLen))
  Result := View.Get(View.Len - 1);

  // Step 6: DeletePropertyOrThrow(O, ToString(newLen))
  View.DeleteIndex(View.Len - 1);
  // Step 7: Set(O, "length", newLen)
  View.SetLen(View.Len - 1);
end;

// ES2026 §23.1.3.26 Array.prototype.slice(start, end)
function TGocciaArrayValue.ArraySlice(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  View: TArrayLikeView;
  ResultArray: TGocciaObjectValue;
  StartIndex, EndIndex, Needed: Integer;
  I, N: Integer;
  K: Int64;
  RawStart, RawEnd, RawK, RawFinal, RawNeeded: Double;
begin
  // Step 1: Let O be ToObject(this value)
  View.Init(AThisValue);

  // Step 3: Let relativeStart be ToIntegerOrInfinity(start).  Compute in
  // Double against RawLen for spec correctness on array-likes whose length
  // exceeds MaxInt — using View.Len would silently truncate the default
  // end on a length-2^32 receiver and let an oversized count slip past
  // the ArrayCreate ceiling check below.
  // ES2026 §7.1.5 ToIntegerOrInfinity: NaN → 0; ±Infinity propagates to
  // the Min/Max clamps below.  Trunc on non-finite values under a masked
  // FPU returns Low(Int64), which would corrupt the start/final clamping
  // (e.g. slice(+Infinity) would yield k = 0 instead of len).
  if AArgs.Length > 0 then
  begin
    RawStart := AArgs.GetElement(0).ToNumberLiteral.Value;
    if IsNaN(RawStart) then
      RawStart := 0
    else if not IsInfinite(RawStart) then
      RawStart := Trunc(RawStart);
  end
  else
    RawStart := 0;

  // Step 4: If relativeStart < 0, let k be max(len + relativeStart, 0); else min(relativeStart, len)
  // The `0.0` literal forces Math.Max to bind to its Double overload; the
  // bare integer `0` resolves to the Single overload on FPC, narrowing
  // RawLen-sized arguments and losing precision past 2^24 (e.g. RawSkipCount
  // = 2^31 - 1 round-trips through Single as 2^31).
  if RawStart < 0 then
    RawK := Max(View.RawLen + RawStart, 0.0)
  else
    RawK := Min(RawStart, View.RawLen);

  // Step 5: If end is undefined, let relativeEnd be len; else ToIntegerOrInfinity(end)
  if (AArgs.Length > 1) and
     not (AArgs.GetElement(1) is TGocciaUndefinedLiteralValue) then
  begin
    RawEnd := AArgs.GetElement(1).ToNumberLiteral.Value;
    if IsNaN(RawEnd) then
      RawEnd := 0
    else if not IsInfinite(RawEnd) then
      RawEnd := Trunc(RawEnd);
  end
  else
    RawEnd := View.RawLen;

  // Step 6: If relativeEnd < 0, let final be max(len + relativeEnd, 0); else min(relativeEnd, len)
  if RawEnd < 0 then
    RawFinal := Max(View.RawLen + RawEnd, 0.0)
  else
    RawFinal := Min(RawEnd, View.RawLen);

  // Step 7: Let count be max(final - k, 0)
  // Step 8: Let A be ArraySpeciesCreate(O, count) — ArrayCreate throws
  // RangeError only when *count* exceeds 2^32 - 1.  Validate the
  // computed result length, not the source's RawLen — checking RawLen
  // would spuriously reject a tiny slice taken from a huge array-like
  // (e.g. slice(0, 2) on a {length: 2^32} receiver).
  RawNeeded := Max(RawFinal - RawK, 0.0);
  View.CheckArrayCreateLenValue(RawNeeded);

  // After the check, RawNeeded ≤ MaxInt — safe to truncate for the loop.
  Needed := Integer(Trunc(RawNeeded));

  if Assigned(View.SpeciesArr) then
    ResultArray := ArraySpeciesCreate(View.Obj, Needed)
  else
    ResultArray := ArrayCreateWithLength(Needed);

  // Step 9: Let n be 0; repeat while k < final
  N := 0;
  // Use 64-bit probing whenever the copy window can touch indices > MaxInt.
  // Branching on RawFinal (not RawK) catches boundary-crossing windows
  // where start <= MaxInt but start + count overflows the Integer range.
  if RawFinal > MaxInt then
  begin
    K := Trunc(RawK);
    while N < Needed do
    begin
      if View.HasIndex64(K) then
        ArrayCreateDataProperty(ResultArray, N, View.Get64(K));
      Inc(K);
      Inc(N);
    end;
  end
  else
  begin
    StartIndex := Integer(Trunc(RawK));
    EndIndex := Integer(Trunc(RawFinal));
    if EndIndex > View.Len then
      EndIndex := View.Len;
    for I := StartIndex to EndIndex - 1 do
    begin
      if View.HasIndex(I) then
        ArrayCreateDataProperty(ResultArray, N, View.Get(I));
      Inc(N);
    end;
  end;

  // Ensure result has correct length (preserve trailing holes)
  ResultArray.SetProperty(PROP_LENGTH, TGocciaNumberLiteralValue.Create(N));

  // Step 11: Return A
  Result := ResultArray;
end;

// ES2026 §23.1.3.9 Array.prototype.find(predicate [, thisArg])
function TGocciaArrayValue.ArrayFind(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  View: TArrayLikeView;
  Callback: TGocciaValue;
  TypedCallback: TGocciaFunctionBase;
  ThisArg: TGocciaValue;
  CallArgs: TGocciaArrayCallbackArgs;
  I: Integer;
  Element, CallResult: TGocciaValue;
  Sparse: TArray<Int64>;
  K: Int64;
begin
  View.Init(AThisValue);
  Callback := ValidateArrayMethodCall('find', AArgs, AThisValue, True);
  ThisArg := GetArrayCallbackThisArg(AArgs);

  TypedCallback := nil;
  if Callback is TGocciaFunctionBase then
    TypedCallback := TGocciaFunctionBase(Callback);

  CallArgs := TGocciaArrayCallbackArgs.Create(View.Obj);
  try
    if View.NeedsSparsePath then
    begin
      Element := View.Get64(0);
      CallArgs.Element := Element;
      CallArgs.Index := TGocciaNumberLiteralValue.Create(0);
      CallResult := InvokeArrayCallback(Callback, TypedCallback, CallArgs, ThisArg);
      if CallResult.ToBooleanLiteral.Value then
      begin
        Result := Element;
        Exit;
      end;

      Sparse := CollectSparseIndicesInRange(View.Obj, 1, View.Len64);
      for K in Sparse do
      begin
        Element := View.Get64(K);
        CallArgs.Element := Element;
        CallArgs.Index := TGocciaNumberLiteralValue.Create(Int64ToDouble(K));
        CallResult := InvokeArrayCallback(Callback, TypedCallback, CallArgs, ThisArg);
        if CallResult.ToBooleanLiteral.Value then
        begin
          Result := Element;
          Exit;
        end;
      end;
      Result := TGocciaUndefinedLiteralValue.UndefinedValue;
      Exit;
    end;

    for I := 0 to View.Len - 1 do
    begin
      Element := View.Get(I);

      CallArgs.Element := Element;
      CallArgs.Index := TGocciaNumberLiteralValue.Create(I);
      CallResult := InvokeArrayCallback(Callback, TypedCallback, CallArgs, ThisArg);
      if CallResult.ToBooleanLiteral.Value then
      begin
        Result := Element;
        Exit;
      end;
    end;
  finally
    CallArgs.Free;
  end;

  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

// ES2026 §23.1.3.10 Array.prototype.findIndex(predicate [, thisArg])
function TGocciaArrayValue.ArrayFindIndex(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  View: TArrayLikeView;
  Callback: TGocciaValue;
  TypedCallback: TGocciaFunctionBase;
  ThisArg: TGocciaValue;
  CallArgs: TGocciaArrayCallbackArgs;
  I: Integer;
  Element, CallResult: TGocciaValue;
  Sparse: TArray<Int64>;
  K: Int64;
begin
  View.Init(AThisValue);
  Callback := ValidateArrayMethodCall('findIndex', AArgs, AThisValue, True);
  ThisArg := GetArrayCallbackThisArg(AArgs);

  TypedCallback := nil;
  if Callback is TGocciaFunctionBase then
    TypedCallback := TGocciaFunctionBase(Callback);

  CallArgs := TGocciaArrayCallbackArgs.Create(View.Obj);
  try
    if View.NeedsSparsePath then
    begin
      Element := View.Get64(0);
      CallArgs.Element := Element;
      CallArgs.Index := TGocciaNumberLiteralValue.Create(0);
      CallResult := InvokeArrayCallback(Callback, TypedCallback, CallArgs, ThisArg);
      if CallResult.ToBooleanLiteral.Value then
      begin
        Result := TGocciaNumberLiteralValue.Create(0);
        Exit;
      end;

      Sparse := CollectSparseIndicesInRange(View.Obj, 1, View.Len64);
      for K in Sparse do
      begin
        Element := View.Get64(K);
        CallArgs.Element := Element;
        CallArgs.Index := TGocciaNumberLiteralValue.Create(Int64ToDouble(K));
        CallResult := InvokeArrayCallback(Callback, TypedCallback, CallArgs, ThisArg);
        if CallResult.ToBooleanLiteral.Value then
        begin
          Result := TGocciaNumberLiteralValue.Create(Int64ToDouble(K));
          Exit;
        end;
      end;
      Result := TGocciaNumberLiteralValue.Create(-1);
      Exit;
    end;

    for I := 0 to View.Len - 1 do
    begin
      Element := View.Get(I);

      CallArgs.Element := Element;
      CallArgs.Index := TGocciaNumberLiteralValue.Create(I);
      CallResult := InvokeArrayCallback(Callback, TypedCallback, CallArgs, ThisArg);
      if CallResult.ToBooleanLiteral.Value then
      begin
        Result := TGocciaNumberLiteralValue.Create(I);
        Exit;
      end;
    end;
  finally
    CallArgs.Free;
  end;

  Result := TGocciaNumberLiteralValue.Create(-1);
end;

// ES2026 §23.1.3.11 Array.prototype.findLast(predicate [, thisArg])
function TGocciaArrayValue.ArrayFindLast(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  View: TArrayLikeView;
  Callback: TGocciaValue;
  TypedCallback: TGocciaFunctionBase;
  ThisArg: TGocciaValue;
  CallArgs: TGocciaArrayCallbackArgs;
  I: Integer;
  Element, CallResult: TGocciaValue;
  Sparse: TArray<Int64>;
  LastIndex: Int64;
  J: Integer;
begin
  View.Init(AThisValue);
  Callback := ValidateArrayMethodCall('findLast', AArgs, AThisValue, True);
  ThisArg := GetArrayCallbackThisArg(AArgs);

  TypedCallback := nil;
  if Callback is TGocciaFunctionBase then
    TypedCallback := TGocciaFunctionBase(Callback);

  CallArgs := TGocciaArrayCallbackArgs.Create(View.Obj);
  try
    if View.NeedsSparsePath then
    begin
      LastIndex := View.Len64 - 1;
      Element := View.Get64(LastIndex);
      CallArgs.Element := Element;
      CallArgs.Index := TGocciaNumberLiteralValue.Create(Int64ToDouble(LastIndex));
      CallResult := InvokeArrayCallback(Callback, TypedCallback, CallArgs, ThisArg);
      if CallResult.ToBooleanLiteral.Value then
      begin
        Result := Element;
        Exit;
      end;

      Sparse := CollectSparseIndicesInRange(View.Obj, 0, LastIndex);
      for J := High(Sparse) downto 0 do
      begin
        Element := View.Get64(Sparse[J]);
        CallArgs.Element := Element;
        CallArgs.Index := TGocciaNumberLiteralValue.Create(Int64ToDouble(Sparse[J]));
        CallResult := InvokeArrayCallback(Callback, TypedCallback, CallArgs, ThisArg);
        if CallResult.ToBooleanLiteral.Value then
        begin
          Result := Element;
          Exit;
        end;
      end;
      Result := TGocciaUndefinedLiteralValue.UndefinedValue;
      Exit;
    end;

    for I := View.Len - 1 downto 0 do
    begin
      Element := View.Get(I);

      CallArgs.Element := Element;
      CallArgs.Index := TGocciaNumberLiteralValue.Create(I);
      CallResult := InvokeArrayCallback(Callback, TypedCallback, CallArgs, ThisArg);
      if CallResult.ToBooleanLiteral.Value then
      begin
        Result := Element;
        Exit;
      end;
    end;
  finally
    CallArgs.Free;
  end;

  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

// ES2026 §23.1.3.12 Array.prototype.findLastIndex(predicate [, thisArg])
function TGocciaArrayValue.ArrayFindLastIndex(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  View: TArrayLikeView;
  Callback: TGocciaValue;
  TypedCallback: TGocciaFunctionBase;
  ThisArg: TGocciaValue;
  CallArgs: TGocciaArrayCallbackArgs;
  I: Integer;
  Element, CallResult: TGocciaValue;
  Sparse: TArray<Int64>;
  LastIndex: Int64;
  J: Integer;
begin
  View.Init(AThisValue);
  Callback := ValidateArrayMethodCall('findLastIndex', AArgs, AThisValue, True);
  ThisArg := GetArrayCallbackThisArg(AArgs);

  TypedCallback := nil;
  if Callback is TGocciaFunctionBase then
    TypedCallback := TGocciaFunctionBase(Callback);

  CallArgs := TGocciaArrayCallbackArgs.Create(View.Obj);
  try
    if View.NeedsSparsePath then
    begin
      LastIndex := View.Len64 - 1;
      Element := View.Get64(LastIndex);
      CallArgs.Element := Element;
      CallArgs.Index := TGocciaNumberLiteralValue.Create(Int64ToDouble(LastIndex));
      CallResult := InvokeArrayCallback(Callback, TypedCallback, CallArgs, ThisArg);
      if CallResult.ToBooleanLiteral.Value then
      begin
        Result := TGocciaNumberLiteralValue.Create(Int64ToDouble(LastIndex));
        Exit;
      end;

      Sparse := CollectSparseIndicesInRange(View.Obj, 0, LastIndex);
      for J := High(Sparse) downto 0 do
      begin
        Element := View.Get64(Sparse[J]);
        CallArgs.Element := Element;
        CallArgs.Index := TGocciaNumberLiteralValue.Create(Int64ToDouble(Sparse[J]));
        CallResult := InvokeArrayCallback(Callback, TypedCallback, CallArgs, ThisArg);
        if CallResult.ToBooleanLiteral.Value then
        begin
          Result := TGocciaNumberLiteralValue.Create(Int64ToDouble(Sparse[J]));
          Exit;
        end;
      end;
      Result := TGocciaNumberLiteralValue.Create(-1);
      Exit;
    end;

    for I := View.Len - 1 downto 0 do
    begin
      Element := View.Get(I);

      CallArgs.Element := Element;
      CallArgs.Index := TGocciaNumberLiteralValue.Create(I);
      CallResult := InvokeArrayCallback(Callback, TypedCallback, CallArgs, ThisArg);
      if CallResult.ToBooleanLiteral.Value then
      begin
        Result := TGocciaNumberLiteralValue.Create(I);
        Exit;
      end;
    end;
  finally
    CallArgs.Free;
  end;

  Result := TGocciaNumberLiteralValue.Create(-1);
end;

// ES2026 §23.1.3.38 Array.prototype.with(index, value)
function TGocciaArrayValue.ArrayWith(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  View: TArrayLikeView;
  ResultArray: TGocciaArrayValue;
  Index, ActualIndex, I: Integer;
  NewValue: TGocciaValue;
begin
  // Step 1: Let O be ToObject(this value)
  View.Init(AThisValue);

  if AArgs.Length < 2 then
    ThrowError(SErrorArrayWithRequiresArgs, [], SSuggestArrayThisType);

  // Step 3: Let relativeIndex be ToIntegerOrInfinity(index)
  Index := ToIntegerFromArgs(AArgs);
  // Step 5: Let value be args[1]
  NewValue := AArgs.GetElement(1);

  // Step 4: If relativeIndex >= 0, let actualIndex be relativeIndex; else len + relativeIndex
  if Index < 0 then
    ActualIndex := View.Len + Index
  else
    ActualIndex := Index;

  // Step 6: If actualIndex >= len or actualIndex < 0, throw RangeError
  if (ActualIndex < 0) or (ActualIndex >= View.Len) then
    ThrowRangeError(SErrorInvalidArrayWithIndex,
      Format('index must be between %d and %d', [-View.Len, View.Len - 1]));

  // Step 7: Let A be ArrayCreate(len)
  ResultArray := TGocciaArrayValue.Create;
  // Step 8: Let k be 0; repeat while k < len
  for I := 0 to View.Len - 1 do
  begin
    // Step 8b: If k = actualIndex, let fromValue be value; else Get(O, Pk)
    if I = ActualIndex then
      ResultArray.Elements.Add(NewValue)
    else
      ResultArray.Elements.Add(View.Get(I));
  end;

  // Step 9: Return A
  Result := ResultArray;
end;

// ES2026 §23.1.3.4 Array.prototype.copyWithin(target, start [, end])
function TGocciaArrayValue.ArrayCopyWithin(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  View: TArrayLikeView;
  Target, Start, EndIdx, Count, I: Integer;
  Target64, Start64, End64, Count64, From64, To64: Int64;
  Direction: Integer;
  Temp: array of TGocciaValue;
  Present: array of Boolean;
begin
  // Step 1: Let O be ToObject(this value)
  View.Init(AThisValue);

  if View.NeedsSparsePath then
  begin
    Target64 := NormalizeRelativeIndex64(ToInteger64FromArgs(AArgs, 0), View.Len64);
    Start64 := NormalizeRelativeIndex64(ToInteger64FromArgs(AArgs, 1), View.Len64);
    if (AArgs.Length > 2) and
       not (AArgs.GetElement(2) is TGocciaUndefinedLiteralValue) then
      End64 := NormalizeRelativeIndex64(ToInteger64FromArgs(AArgs, 2), View.Len64)
    else
      End64 := View.Len64;
    Count64 := End64 - Start64;
    if Count64 > View.Len64 - Target64 then
      Count64 := View.Len64 - Target64;
    if Count64 <= 0 then
    begin
      Result := View.Obj;
      Exit;
    end;

    From64 := Start64;
    To64 := Target64;
    Direction := 1;
    if (From64 < To64) and (To64 < From64 + Count64) then
    begin
      Direction := -1;
      From64 := From64 + Count64 - 1;
      To64 := To64 + Count64 - 1;
    end;

    while Count64 > 0 do
    begin
      if View.HasIndex64(From64) then
        View.Obj.AssignProperty(IntToStr(To64), View.Get64(From64))
      else if not View.Obj.DeleteProperty(IntToStr(To64)) then
        ThrowTypeError(Format(SErrorCannotRedefineNonConfigurable, [IntToStr(To64)]),
          SSuggestCannotDeleteNonConfigurable);
      From64 := From64 + Direction;
      To64 := To64 + Direction;
      Dec(Count64);
    end;

    Result := View.Obj;
    Exit;
  end;

  // Step 3: Let relativeTarget be ToIntegerOrInfinity(target)
  Target := ToIntegerFromArgs(AArgs);
  // Step 4: If relativeTarget < 0, let to be max(len + relativeTarget, 0); else min(relativeTarget, len)
  Target := NormalizeRelativeIndex(Target, View.Len);

  // Step 5: Let relativeStart be ToIntegerOrInfinity(start)
  Start := ToIntegerFromArgs(AArgs, 1);
  // Step 6: If relativeStart < 0, let from be max(len + relativeStart, 0); else min(relativeStart, len)
  Start := NormalizeRelativeIndex(Start, View.Len);

  // Step 7: If end is undefined, let relativeEnd be len; else ToIntegerOrInfinity(end)
  if (AArgs.Length > 2) and
     not (AArgs.GetElement(2) is TGocciaUndefinedLiteralValue) then
    EndIdx := ToIntegerFromArgs(AArgs, 2)
  else
    EndIdx := View.Len;
  // Step 8: If relativeEnd < 0, let final be max(len + relativeEnd, 0); else min(relativeEnd, len)
  EndIdx := NormalizeRelativeIndex(EndIdx, View.Len);

  // Step 9: Let count be min(final - from, len - to)
  Count := Min(EndIdx - Start, View.Len - Target);
  if Count <= 0 then
  begin
    Result := View.Obj;
    Exit;
  end;

  // Steps 10-12: Copy elements (using temp buffer for overlap safety)
  // Preserve sparsity per ES spec: delete destination when source is absent.
  SetLength(Temp, Count);
  SetLength(Present, Count);
  for I := 0 to Count - 1 do
  begin
    Present[I] := View.HasIndex(Start + I);
    if Present[I] then
      Temp[I] := View.Get(Start + I);
  end;
  for I := 0 to Count - 1 do
  begin
    if Present[I] then
      View.Put(Target + I, Temp[I])
    else
      View.DeleteIndex(Target + I);
  end;

  // Step 13: Return O
  Result := View.Obj;
end;

// ES2026 §23.1.3.15 Array.prototype.indexOf(searchElement [, fromIndex])
function TGocciaArrayValue.ArrayIndexOf(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  View: TArrayLikeView;
  SearchValue: TGocciaValue;
  I, FromIndex: Integer;
  Len64Val, FromIndex64, K: Int64;
  Sparse: TArray<Int64>;
begin
  // Step 1: Let O be ToObject(this value)
  View.Init(AThisValue);

  // Step 3: If len is 0, return -1 — before ToIntegerOrInfinity(fromIndex).
  if View.Len = 0 then
  begin
    Result := TGocciaNumberLiteralValue.Create(-1);
    Exit;
  end;

  if AArgs.Length < 1 then
    SearchValue := TGocciaUndefinedLiteralValue.UndefinedValue
  else
    SearchValue := AArgs.GetElement(0);

  // Sparse iteration path for generic array-likes whose claimed length
  // exceeds 2^32 - 1.  The dense Integer loop would saturate to MaxInt and
  // walk billions of phantom indices; the spec only observes indices that
  // actually exist on the chain, so we enumerate exactly those.
  if View.NeedsSparsePath then
  begin
    Len64Val := View.Len64;
    FromIndex64 := ToInteger64FromArgs(AArgs, 1);
    if FromIndex64 < 0 then
      FromIndex64 := Len64Val + FromIndex64;
    if FromIndex64 < 0 then
      FromIndex64 := 0;
    if FromIndex64 >= Len64Val then
    begin
      Result := TGocciaNumberLiteralValue.Create(-1);
      Exit;
    end;
    Sparse := CollectSparseIndicesInRange(View.Obj, FromIndex64, Len64Val);
    for K in Sparse do
      if IsStrictEqual(View.Get64(K), SearchValue) then
      begin
        Result := TGocciaNumberLiteralValue.Create(Int64ToDouble(K));
        Exit;
      end;
    Result := TGocciaNumberLiteralValue.Create(-1);
    Exit;
  end;

  // Step 4: Let n be ToIntegerOrInfinity(fromIndex)
  // Step 6: If n >= 0, let k be n; else let k be max(len + n, 0)
  FromIndex := ToIntegerFromArgs(AArgs, 1);
  if FromIndex < 0 then
    FromIndex := View.Len + FromIndex;
  if FromIndex < 0 then
    FromIndex := 0;

  // Step 7: Repeat, while k < len
  for I := FromIndex to View.Len - 1 do
  begin
    // Step 7a: Let kPresent be HasProperty(O, Pk)
    if not View.HasIndex(I) then
      Continue;
    // Step 7b: If kPresent, let elementK be Get(O, Pk)
    // Step 7c: If IsStrictlyEqual(searchElement, elementK) is true, return k
    if IsStrictEqual(View.Get(I), SearchValue) then
    begin
      Result := TGocciaNumberLiteralValue.Create(I);
      Exit;
    end;
  end;

  // Step 8: Return -1𝔽
  Result := TGocciaNumberLiteralValue.Create(-1);
end;

// ES2026 §23.1.3.17 Array.prototype.lastIndexOf(searchElement [, fromIndex])
function TGocciaArrayValue.ArrayLastIndexOf(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  View: TArrayLikeView;
  SearchValue: TGocciaValue;
  I, FromIndex: Integer;
  Len64Val, FromIndex64: Int64;
  Sparse: TArray<Int64>;
  J: Integer;
begin
  // Step 1: Let O be ToObject(this value)
  View.Init(AThisValue);

  // Step 3: If len is 0, return -1 — before ToIntegerOrInfinity(fromIndex).
  if View.Len = 0 then
  begin
    Result := TGocciaNumberLiteralValue.Create(-1);
    Exit;
  end;

  if AArgs.Length < 1 then
    SearchValue := TGocciaUndefinedLiteralValue.UndefinedValue
  else
    SearchValue := AArgs.GetElement(0);

  // Sparse iteration path for generic array-likes whose claimed length
  // exceeds 2^32 - 1.  See ArrayIndexOf for rationale.
  if View.NeedsSparsePath then
  begin
    Len64Val := View.Len64;
    if AArgs.Length >= 2 then
      FromIndex64 := ToInteger64FromArgs(AArgs, 1)
    else
      FromIndex64 := Len64Val - 1;
    if FromIndex64 < 0 then
      FromIndex64 := Len64Val + FromIndex64;
    if FromIndex64 >= Len64Val then
      FromIndex64 := Len64Val - 1;
    if FromIndex64 < 0 then
    begin
      Result := TGocciaNumberLiteralValue.Create(-1);
      Exit;
    end;
    Sparse := CollectSparseIndicesInRange(View.Obj, 0, FromIndex64 + 1);
    for J := High(Sparse) downto 0 do
      if IsStrictEqual(View.Get64(Sparse[J]), SearchValue) then
      begin
        Result := TGocciaNumberLiteralValue.Create(Int64ToDouble(Sparse[J]));
        Exit;
      end;
    Result := TGocciaNumberLiteralValue.Create(-1);
    Exit;
  end;

  // Step 4: If fromIndex is present, let n be ToIntegerOrInfinity(fromIndex); else let n be len - 1
  FromIndex := ToIntegerFromArgs(AArgs, 1, View.Len - 1);
  // Step 6: If n < 0, let k be len + n
  if FromIndex < 0 then
    FromIndex := View.Len + FromIndex;

  if FromIndex >= View.Len then
    FromIndex := View.Len - 1;

  // Step 7: Repeat, while k >= 0
  for I := FromIndex downto 0 do
  begin
    // Step 7a: Let kPresent be HasProperty(O, Pk)
    if not View.HasIndex(I) then
      Continue;
    // Step 7c: If IsStrictlyEqual(searchElement, elementK) is true, return k
    if IsStrictEqual(View.Get(I), SearchValue) then
    begin
      Result := TGocciaNumberLiteralValue.Create(I);
      Exit;
    end;
  end;

  // Step 8: Return -1𝔽
  Result := TGocciaNumberLiteralValue.Create(-1);
end;

// ES2026 §7.2.15 IsConcatSpreadable(O)
function IsConcatSpreadable(const AValue: TGocciaValue): Boolean;
var
  Spreadable: TGocciaValue;
  Probe: TGocciaValue;
begin
  if not (AValue is TGocciaObjectValue) then
  begin
    Result := False;
    Exit;
  end;
  // Step 1: If Type(O) is Object, let spreadable be Get(O, @@isConcatSpreadable)
  Spreadable := TGocciaObjectValue(AValue).GetSymbolProperty(TGocciaSymbolValue.WellKnownIsConcatSpreadable);
  if Assigned(Spreadable) and not (Spreadable is TGocciaUndefinedLiteralValue) then
  begin
    Result := Spreadable.ToBooleanLiteral.Value;
    Exit;
  end;
  // Step 2: Return IsArray(O)
  Probe := AValue;
  while Probe is TGocciaProxyValue do
  begin
    if TGocciaProxyValue(Probe).Revoked then
      ThrowTypeError(SErrorProxyRevoked, SSuggestProxyRevoked);
    Probe := TGocciaProxyValue(Probe).Target;
  end;
  Result := Probe is TGocciaArrayValue;
end;

// Spread a concat-spreadable value into the result array using array-like
// iteration so that non-array spreadable objects (e.g., objects with
// Symbol.isConcatSpreadable) are handled correctly.
procedure SpreadIntoConcat(const ASource: TGocciaValue; const AResult: TGocciaObjectValue; var N: Integer);
var
  SrcView: TArrayLikeView;
  J: Integer;
  EndN: Double;
  Sparse: TArray<Int64>;
  K, Target64: Int64;
  Element: TGocciaValue;
begin
  SrcView.Init(ASource);
  // ES2026 §23.1.3.1 step 5.c.iii.2: if n + len > 2^53 - 1, throw TypeError.
  EndN := N + SrcView.RawLen;
  if EndN > MAX_SAFE_INTEGER_F then
    ThrowTypeError(
      'Array length exceeds maximum safe integer (2^53 - 1)',
      'use a smaller length');

  if SrcView.NeedsSparsePath then
  begin
    Sparse := CollectSparseIndicesInRange(SrcView.Obj, 0, SrcView.Len64);
    for K in Sparse do
    begin
      Element := SrcView.Get64(K);
      Target64 := Int64(N) + K;
      if Target64 > MaxInt then
        ThrowRangeError(
          'Array length exceeds engine maximum (MaxInt)',
          'use a smaller length');
      ArrayCreateDataProperty(AResult, Integer(Target64), Element);
    end;
    if EndN > MAX_ARRAY_LENGTH_F then
      ThrowRangeError(SErrorInvalidArrayLength, SSuggestArrayLengthRange);
    if EndN > MaxInt then
      ThrowRangeError(
        'Array length exceeds engine maximum (MaxInt)',
        'use a smaller length');
    N := Trunc(EndN);
    Exit;
  end;

  // Pragmatic guards: this implementation tracks `n` as an Integer and
  // cannot materialise more than 2^32 - 1 elements in a native array.  A
  // source length that would carry `n` past that bound is rejected up-front
  // rather than silently truncated mid-iteration.  The MaxInt guard catches
  // the narrower window where EndN is still within the spec ceiling but
  // already exceeds Integer range — the `Inc(N)` loop below would otherwise
  // overflow N into negative territory.
  if EndN > MAX_ARRAY_LENGTH_F then
    ThrowRangeError(SErrorInvalidArrayLength, SSuggestArrayLengthRange);
  if EndN > MaxInt then
    ThrowRangeError(
      'Array length exceeds engine maximum (MaxInt)',
      'use a smaller length');
  for J := 0 to SrcView.Len - 1 do
  begin
    if SrcView.HasIndex(J) then
      ArrayCreateDataProperty(AResult, N, SrcView.Get(J));
    Inc(N);
  end;
end;

// ES2026 §23.1.3.1 Array.prototype.concat(...arguments)
function TGocciaArrayValue.ArrayConcat(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Obj: TGocciaObjectValue;
  ArrayProbe: TGocciaValue;
  ResultArray: TGocciaObjectValue;
  I, N: Integer;
  Arg: TGocciaValue;
begin
  // Step 1: Let O be ToObject(this value)
  Obj := ToObject(AThisValue);
  ArrayProbe := Obj;
  while ArrayProbe is TGocciaProxyValue do
  begin
    if TGocciaProxyValue(ArrayProbe).Revoked then
      ThrowTypeError(SErrorProxyRevoked, SSuggestProxyRevoked);
    ArrayProbe := TGocciaProxyValue(ArrayProbe).Target;
  end;

  // Step 2: Let A be ArraySpeciesCreate(O, 0)
  if ArrayProbe is TGocciaArrayValue then
    ResultArray := ArraySpeciesCreate(Obj, 0)
  else
    ResultArray := TGocciaArrayValue.Create;

  // Step 3: Let n be 0
  N := 0;

  // Step 4: Prepend O to items — check IsConcatSpreadable
  if IsConcatSpreadable(Obj) then
    SpreadIntoConcat(Obj, ResultArray, N)
  else
  begin
    ArrayCreateDataProperty(ResultArray, N, Obj);
    Inc(N);
  end;

  // Step 5: For each element E of items
  for I := 0 to AArgs.Length - 1 do
  begin
    Arg := AArgs.GetElement(I);
    // Step 5b: If IsConcatSpreadable(E) is true, spread via array-like iteration
    if IsConcatSpreadable(Arg) then
      SpreadIntoConcat(Arg, ResultArray, N)
    // Step 5c: Else, CreateDataPropertyOrThrow(A, ToString(n), E)
    else
    begin
      ArrayCreateDataProperty(ResultArray, N, Arg);
      Inc(N);
    end;
  end;

  ResultArray.SetProperty(PROP_LENGTH, TGocciaNumberLiteralValue.Create(N));

  // Step 6: Return A
  Result := ResultArray;
end;

// ES2026 §23.1.3.23 Array.prototype.reverse()
function TGocciaArrayValue.ArrayReverse(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  View: TArrayLikeView;
  I, J: Integer;
  LowerExists, UpperExists: Boolean;
  Lower, Upper: TGocciaValue;
begin
  // Step 1: Let O be ToObject(this value)
  View.Init(AThisValue);
  // Step 3: Let middle be floor(len / 2), let lower be 0
  I := 0;
  J := View.Len - 1;
  // Step 4: Repeat, while lower ≠ middle
  while I < J do
  begin
    // ES2026 §23.1.3.23 steps 4a-i: preserve sparsity via HasProperty/Delete
    LowerExists := View.HasIndex(I);
    if LowerExists then
      Lower := View.Get(I);
    UpperExists := View.HasIndex(J);
    if UpperExists then
      Upper := View.Get(J);

    if LowerExists and UpperExists then
    begin
      View.Put(I, Upper);
      View.Put(J, Lower);
    end
    else if UpperExists then
    begin
      View.Put(I, Upper);
      View.DeleteIndex(J);
    end
    else if LowerExists then
    begin
      View.Put(J, Lower);
      View.DeleteIndex(I);
    end;
    // else: both absent — nothing to do
    Inc(I);
    Dec(J);
  end;

  // Step 5: Return O
  Result := View.Obj;
end;

// ES2026 §23.1.3.33 Array.prototype.toReversed()
function TGocciaArrayValue.ArrayToReversed(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  View: TArrayLikeView;
  ResultArray: TGocciaArrayValue;
  I: Integer;
begin
  // Step 1: Let O be ToObject(this value)
  View.Init(AThisValue);
  // Step 3: Let A be ArrayCreate(len) — RangeError if len > 2^32-1
  View.CheckArrayCreateLen;
  ResultArray := TGocciaArrayValue.Create;

  // Step 4: Let k be 0; repeat while k < len
  for I := View.Len - 1 downto 0 do
    ResultArray.Elements.Add(View.Get(I));

  // Step 5: Return A
  Result := ResultArray;
end;

// ES2026 §23.1.3.34 Array.prototype.toSorted(comparefn)
function TGocciaArrayValue.ArrayToSorted(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  View: TArrayLikeView;
  ResultArray: TGocciaArrayValue;
  CustomSortFunction: TGocciaValue;
  I: Integer;
  CallArgs: TGocciaArgumentsCollection;
  ReceiverRoot, ResultRoot: TGocciaTempRoot;
begin
  // Step 1: If comparefn is not undefined and IsCallable(comparefn) is false,
  // throw a TypeError. Spec §23.1.3.34 step 1 — must run BEFORE LengthOfArrayLike
  // so a comparator-shape error is observed before any property reads on `this`.
  if AArgs.Length > 0 then
  begin
    CustomSortFunction := AArgs.GetElement(0);
    if (not (CustomSortFunction is TGocciaUndefinedLiteralValue)) and
       (not CustomSortFunction.IsCallable) then
      ThrowTypeError(SErrorCustomSortMustBeFunction, SSuggestCallbackRequired);
  end
  else
    CustomSortFunction := TGocciaUndefinedLiteralValue.UndefinedValue;

  // Step 2-3: Let O be ToObject(this value); Let len be LengthOfArrayLike(O).
  View.Init(AThisValue);
  InitializeTempRoot(ReceiverRoot);
  InitializeTempRoot(ResultRoot);
  AddTempRootIfNeeded(ReceiverRoot, View.Obj);
  try
    // Step 4: Let A be ArrayCreate(len) — RangeError if len > 2^32-1
    View.CheckArrayCreateLen;
    ResultArray := TGocciaArrayValue.Create;
    AddTempRootIfNeeded(ResultRoot, ResultArray);
    try
      // Step 5: Copy elements from O into A
      for I := 0 to View.Len - 1 do
        ResultArray.Elements.Add(View.Get(I));

      if not (CustomSortFunction is TGocciaUndefinedLiteralValue) then
      begin
        CallArgs := TGocciaArgumentsCollection.Create([nil, nil]);
        try
          StableSortElements(ResultArray.Elements, CustomSortFunction, CallArgs, AThisValue);
        finally
          CallArgs.Free;
        end;
      end else
        StableSortElementsDefault(ResultArray.Elements);

      // Step 7: Return A
      Result := ResultArray;
    finally
      RemoveTempRootIfNeeded(ResultRoot);
    end;
  finally
    RemoveTempRootIfNeeded(ReceiverRoot);
  end;
end;

// ES2026 §23.1.3.35 Array.prototype.toSpliced(start, skipCount, ...items)
function TGocciaArrayValue.ArrayToSpliced(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  View: TArrayLikeView;
  ResultArray: TGocciaArrayValue;
  ActualStartIndex, DeleteCount, InsertCount, NewLenInt, TailLen, DestBase, I: Integer;
  RawStart, RawSkipCount, RawActualStart, RawActualSkipCount, NewLen: Double;
  StartKey, EndKey, K: Int64;
  Sparse: TArray<Int64>;
begin
  // Step 1: Let O be ToObject(this value)
  View.Init(AThisValue);

  // Step 3: Let relativeStart be ToIntegerOrInfinity(start).  Computed in
  // Double against RawLen so a negative or out-of-range start clamps to the
  // spec length, not to the truncated View.Len, on receivers whose length
  // exceeds MaxInt (matches ArraySlice).
  if AArgs.Length > 0 then
  begin
    RawStart := AArgs.GetElement(0).ToNumberLiteral.Value;
    if IsNaN(RawStart) then
      RawStart := 0
    else if not IsInfinite(RawStart) then
      RawStart := Trunc(RawStart);
  end
  else
    RawStart := 0;

  // Step 4: If relativeStart < 0, let actualStart be max(len + relativeStart, 0); else min(relativeStart, len)
  // The `0.0` literal forces Math.Max to bind to its Double overload (see
  // ArraySlice for the Single-narrowing rationale).
  if RawStart < 0 then
    RawActualStart := Max(View.RawLen + RawStart, 0.0)
  else
    RawActualStart := Min(RawStart, View.RawLen);

  // Step 5: If skipCount is not present, let actualSkipCount be len - actualStart
  // Step 6: Else let actualSkipCount be min(max(ToIntegerOrInfinity(skipCount), 0), len - actualStart)
  if AArgs.Length = 0 then
    RawActualSkipCount := 0
  else if AArgs.Length > 1 then
  begin
    RawSkipCount := AArgs.GetElement(1).ToNumberLiteral.Value;
    if IsNaN(RawSkipCount) then
      RawSkipCount := 0
    else if not IsInfinite(RawSkipCount) then
      RawSkipCount := Trunc(RawSkipCount);
    RawActualSkipCount := Min(Max(RawSkipCount, 0.0), View.RawLen - RawActualStart);
  end
  else
    RawActualSkipCount := View.RawLen - RawActualStart;

  // Step 8: Let newLen be len + insertCount - actualSkipCount
  //         If newLen > 2^53 - 1, throw TypeError
  //         (ArrayCreate additionally throws RangeError if newLen > 2^32 - 1
  //          or > MaxInt — engine cannot address more than that.)
  InsertCount := AArgs.Length - 2;
  if InsertCount < 0 then
    InsertCount := 0;
  NewLen := View.RawLen + InsertCount - RawActualSkipCount;
  View.CheckSafeIntegerLen(NewLen);
  View.CheckArrayCreateLenValue(NewLen);

  // After the NewLen check we know any survivor index fits in Integer
  // (RawActualStart ≤ RawLen - RawActualSkipCount ≤ NewLen + InsertCount
  // ≤ MaxInt + InsertCount), but RawActualSkipCount itself can still exceed
  // MaxInt — e.g. toSpliced(0, 2^40-100) on a {length: 2^40} receiver gives
  // RawActualSkipCount ≈ 2^40 while NewLen = 100.  Truncating without the
  // guard would wrap the Integer.  Refuse the operation: the engine has no
  // sparse-iteration path for the [actualStart, actualStart+skipCount) read
  // loop below.
  if RawActualSkipCount > MaxInt then
    ThrowRangeError(
      'Array splice skip count exceeds engine maximum (MaxInt)',
      'use a smaller skipCount');
  ActualStartIndex := Integer(Trunc(RawActualStart));
  DeleteCount := Integer(Trunc(RawActualSkipCount));

  // Step 9: Let A be ArrayCreate(newLen)
  ResultArray := TGocciaArrayValue.Create;

  // Step 11: Copy elements before actualStart.  ActualStartIndex ≤ NewLen ≤
  // MaxInt, so the dense Integer loop covers the full leading range.  When
  // RawLen > MaxInt only the trailing range can extend past MaxInt; the
  // leading copy stays Integer-bounded by construction.
  for I := 0 to ActualStartIndex - 1 do
    ResultArray.Elements.Add(View.Get(I));

  // Step 12: Insert new items
  for I := 2 to AArgs.Length - 1 do
    ResultArray.Elements.Add(AArgs.GetElement(I));

  // Step 13: Copy elements after actualStart + actualSkipCount.  Source
  // range is [ActualStartIndex + DeleteCount, RawLen); destination side is
  // bounded by NewLen ≤ MaxInt (CheckArrayCreateLenValue above).  When
  // RawLen exceeds MaxInt the dense Integer loop saturates at View.Len =
  // MaxInt and silently drops every source index past it; switch to sparse
  // enumeration so high-index source elements are picked up.  toSpliced does
  // not preserve holes — every result position is a data property — so
  // pre-fill the tail with undefined and overwrite present indices.
  NewLenInt := Integer(Trunc(NewLen));
  TailLen := NewLenInt - ActualStartIndex - InsertCount;
  if View.NeedsSparsePath then
  begin
    DestBase := ResultArray.Elements.Count;
    for I := 0 to TailLen - 1 do
      ResultArray.Elements.Add(TGocciaUndefinedLiteralValue.UndefinedValue);
    StartKey := Int64(ActualStartIndex) + Int64(DeleteCount);
    EndKey := StartKey + Int64(TailLen);
    Sparse := CollectSparseIndicesInRange(View.Obj, StartKey, EndKey);
    // FPC rejects Int64 as a classic for-loop counter on 32-bit targets;
    // the enumerator form `for K in TArray<Int64>` compiles everywhere.
    for K in Sparse do
      ResultArray.Elements[DestBase + Integer(K - StartKey)] := View.Get64(K);
  end
  else
  begin
    for I := ActualStartIndex + DeleteCount to View.Len - 1 do
      ResultArray.Elements.Add(View.Get(I));
  end;

  // Step 14: Return A
  Result := ResultArray;
end;

function TGocciaArrayValue.ToStringLiteral: TGocciaStringLiteralValue;
var
  I: Integer;
  SB: TStringBuffer;
begin
  if FElements.Count = 0 then
  begin
    Result := TGocciaStringLiteralValue.Create('');
    Exit;
  end;

  SB := TStringBuffer.Create;
  for I := 0 to FElements.Count - 1 do
  begin
    if I > 0 then
      SB.AppendChar(',');
    if not IsArrayHole(FElements[I]) then
      SB.Append(FElements[I].ToStringLiteral.Value);
  end;
  Result := TGocciaStringLiteralValue.Create(SB.ToString);
end;

function TGocciaArrayValue.ToBooleanLiteral: TGocciaBooleanLiteralValue;
begin
  Result := TGocciaBooleanLiteralValue.TrueValue;
end;

function TGocciaArrayValue.TypeName: string;
begin
  Result := 'object';
end;

function TGocciaArrayValue.Includes(const AValue: TGocciaValue; AFromIndex: Integer = 0): Boolean;
var
  I: Integer;
begin
  if AFromIndex < 0 then
    AFromIndex := FElements.Count + AFromIndex;

  if AFromIndex < 0 then
    AFromIndex := 0;

  for I := AFromIndex to FElements.Count - 1 do
  begin
    if IsArrayHole(FElements[I]) then
    begin
      if AValue is TGocciaUndefinedLiteralValue then
      begin
        Result := True;
        Exit;
      end;
      Continue;
    end;

    // ECMAScript specifies SameValueZero for includes
    if IsSameValueZero(FElements[I], AValue) then
    begin
      Result := True;
      Exit;
    end;
  end;

  Result := False;
end;

function TGocciaArrayValue.GetProperty(const AName: string): TGocciaValue;
begin
  Result := GetPropertyWithContext(AName, Self);
end;

function TGocciaArrayValue.GetPropertyWithContext(const AName: string; const AThisContext: TGocciaValue): TGocciaValue;
var
  Index: Int64;
begin
  if AName = PROP_LENGTH then
  begin
    if FLength < FElements.Count then
      FLength := FElements.Count;
    Result := TGocciaNumberLiteralValue.Create(FLength);
    Exit;
  end;

  // Check if property name is a numeric index
  if TryParseArrayElementIndex(AName, Index) then
  begin
    if CanStoreDenseElementIndex(Index, FElements.Count) and
       (Index < FElements.Count) and
       not IsArrayHole(FElements[Integer(Index)]) then
    begin
      Result := FElements[Integer(Index)];
      Exit;
    end;
    // Hole or out-of-range: fall through to prototype per ES [[Get]]
    Result := inherited GetPropertyWithContext(AName, AThisContext);
    Exit;
  end
  else
  begin
    // Fall back to regular object property lookup
    Result := inherited GetPropertyWithContext(AName, AThisContext);
  end;
end;

procedure TGocciaArrayValue.SetProperty(const AName: string; const AValue: TGocciaValue);
var
  Accessor: TGocciaPropertyDescriptorAccessor;
  Args: TGocciaArgumentsCollection;
  Descriptor: TGocciaPropertyDescriptor;
  Index: Int64;
  Desc: TGocciaPropertyDescriptorData;
begin
  // Check if property name is a numeric index
  if TryParseArrayElementIndex(AName, Index) then
  begin
    Descriptor := GetOwnPropertyDescriptor(AName);
    if Descriptor is TGocciaPropertyDescriptorAccessor then
    begin
      Accessor := TGocciaPropertyDescriptorAccessor(Descriptor);
      if Assigned(Accessor.Setter) and Accessor.Setter.IsCallable then
      begin
        Args := TGocciaArgumentsCollection.Create([AValue]);
        try
          InvokeCallable(Accessor.Setter, Args, Self);
        finally
          Args.Free;
        end;
        Exit;
      end;
      ThrowTypeError(Format(SErrorSetPropertyOnlyGetter, [AName, ToStringTag]),
        SSuggestPropertyHasOnlyGetter);
    end;

    Descriptor := inherited GetOwnPropertyDescriptor(AName);
    if Assigned(Descriptor) then
    begin
      inherited AssignProperty(AName, AValue);
      if Index + 1 > FLength then
        FLength := Index + 1;
      Exit;
    end;

    if FFrozen and CanStoreDenseElementIndex(Index, FElements.Count) and
       (Index < FElements.Count) and
       not IsArrayHole(FElements[Integer(Index)]) then
      ThrowTypeError(Format(SErrorCannotAssignReadOnly, [AName]),
        SSuggestCannotDeleteNonConfigurable);

    if (Index >= FLength) and not FLengthWritable then
      ThrowTypeError(Format(SErrorCannotRedefineNonConfigurable, [AName]),
        SSuggestCannotDeleteNonConfigurable);

    if (not Extensible) and
       not (CanStoreDenseElementIndex(Index, FElements.Count) and
            (Index < FElements.Count) and
            not IsArrayHole(FElements[Integer(Index)])) then
      ThrowTypeError(Format(SErrorCannotAddPropertyNotExtensible, [AName]),
        SSuggestObjectNotExtensible);

    if CanStoreDenseElementIndex(Index, FElements.Count) then
    begin
      // Expand array if necessary
      ExtendElementsWithHoles(FElements, Int64(Index) + 1);

      // Set the element
      FElements[Integer(Index)] := AValue;
    end
    else
      inherited DefineProperty(AName, TGocciaPropertyDescriptorData.Create(AValue,
        [pfEnumerable, pfConfigurable, pfWritable]));

    if Index + 1 > FLength then
      FLength := Index + 1;
  end
  else if AName = PROP_LENGTH then
  begin
    if not FLengthWritable then
      ThrowTypeError(Format(SErrorCannotAssignReadOnly, [AName]),
        SSuggestCannotDeleteNonConfigurable);

    // §10.4.2.1 → OrdinarySet → OrdinarySetWithOwnDescriptor step 3e:
    // own writable data property ⇒ [[DefineOwnProperty]](P, {[[Value]]: V})
    Desc := TGocciaPropertyDescriptorData.CreatePartial(AValue, [],
      [pdfValue]);
    try
      DefineProperty(PROP_LENGTH, Desc);
    except
      Desc.Free;
      raise;
    end;
  end
  else
  begin
    inherited AssignProperty(AName, AValue);
  end;
end;

function TGocciaArrayValue.HasOwnProperty(const AName: string): Boolean;
var
  Index: Int64;
begin
  if TryParseArrayElementIndex(AName, Index) and
     CanStoreDenseElementIndex(Index, FElements.Count) and
     (Index < FElements.Count) and
     not IsArrayHole(FElements[Integer(Index)]) then
    Result := True
  else if AName = PROP_LENGTH then
    Result := True
  else
    Result := inherited HasOwnProperty(AName);
end;

function TGocciaArrayValue.DeleteProperty(const AName: string): Boolean;
var
  Index: Int64;
begin
  if AName = PROP_LENGTH then
    Exit(False);

  if TryParseArrayElementIndex(AName, Index) and
     CanStoreDenseElementIndex(Index, FElements.Count) and
     (Index < FElements.Count) and
     not IsArrayHole(FElements[Integer(Index)]) then
  begin
    if FSealed or FFrozen then
      Exit(False);
    FElements[Integer(Index)] := TGocciaHoleValue.HoleValue;
    Exit(True);
  end;

  Result := inherited DeleteProperty(AName);
end;

function TGocciaArrayValue.GetOwnPropertyKeys: TArray<string>;
var
  Keys: TArray<string>;
  NumericKeys: TList<Int64>;
  SeenNumericKeys: TDictionary<Int64, Boolean>;
  OtherKeys: TList<string>;
  Count, I: Integer;
  Key: string;
  Index: Int64;
begin
  Keys := inherited GetOwnPropertyKeys;
  NumericKeys := TList<Int64>.Create;
  SeenNumericKeys := TDictionary<Int64, Boolean>.Create;
  OtherKeys := TList<string>.Create;
  try
    for I := 0 to FElements.Count - 1 do
      if not IsArrayHole(FElements[I]) then
      begin
        NumericKeys.Add(I);
        SeenNumericKeys.Add(I, True);
      end;

    for Key in Keys do
    begin
      if TryParseArrayElementIndex(Key, Index) then
      begin
        if not SeenNumericKeys.ContainsKey(Index) then
        begin
          NumericKeys.Add(Index);
          SeenNumericKeys.Add(Index, True);
        end;
      end
      else
        OtherKeys.Add(Key);
    end;

    NumericKeys.Sort(TComparer<Int64>.Construct(CompareInt64));
    if OtherKeys.IndexOf(PROP_LENGTH) < 0 then
      OtherKeys.Insert(0, PROP_LENGTH);

    SetLength(Result, NumericKeys.Count + OtherKeys.Count);
    Count := 0;
    for I := 0 to NumericKeys.Count - 1 do
    begin
      Result[Count] := IntToStr(NumericKeys[I]);
      Inc(Count);
    end;
    for I := 0 to OtherKeys.Count - 1 do
    begin
      Result[Count] := OtherKeys[I];
      Inc(Count);
    end;
  finally
    OtherKeys.Free;
    SeenNumericKeys.Free;
    NumericKeys.Free;
  end;
end;

function TGocciaArrayValue.GetAllPropertyNames: TArray<string>;
begin
  Result := GetOwnPropertyKeys;
end;

function TGocciaArrayValue.GetOwnPropertyDescriptor(const AName: string): TGocciaPropertyDescriptor;
var
  Index: Int64;
  Descriptor: TGocciaPropertyDescriptor;
begin
  if TryParseArrayElementIndex(AName, Index) then
  begin
    Descriptor := inherited GetOwnPropertyDescriptor(AName);
    if Assigned(Descriptor) then
      Exit(Descriptor);
  end;

  if TryParseArrayElementIndex(AName, Index) and
     CanStoreDenseElementIndex(Index, FElements.Count) and
     (Index < FElements.Count) and
     not IsArrayHole(FElements[Integer(Index)]) then
  begin
    if FFrozen then
      Result := TGocciaPropertyDescriptorData.Create(
        FElements[Integer(Index)], [pfEnumerable])
    else if FSealed then
      Result := TGocciaPropertyDescriptorData.Create(
        FElements[Integer(Index)], [pfEnumerable, pfWritable])
    else
      Result := TGocciaPropertyDescriptorData.Create(
        FElements[Integer(Index)], [pfEnumerable, pfConfigurable, pfWritable]);
  end
  else if AName = PROP_LENGTH then
  begin
    if FLength < FElements.Count then
      FLength := FElements.Count;
    if FLengthWritable then
      Result := TGocciaPropertyDescriptorData.Create(
        TGocciaNumberLiteralValue.Create(FLength), [pfWritable])
    else
      Result := TGocciaPropertyDescriptorData.Create(
        TGocciaNumberLiteralValue.Create(FLength), []);
  end
  else
    Result := inherited GetOwnPropertyDescriptor(AName);
end;

// ES2026 §10.4.2.1 ArrayDefineOwnProperty(A, P, Desc)
// Mirrors the throwing variant of TGocciaObjectValue.DefineProperty: does NOT
// take ownership of ADescriptor on failure — the caller (e.g., the
// Object.defineProperty wrapper) catches the exception and frees the descriptor
// itself. Routing through the boolean TryDefineProperty would cause a
// double-free here because that variant frees on validation failure and the
// outer caller would free again from its `except` clause.
procedure TGocciaArrayValue.DefineProperty(const AName: string; const ADescriptor: TGocciaPropertyDescriptor);
var
  Index: Int64;
begin
  if AName = PROP_LENGTH then
  begin
    // §10.4.2.4 ArraySetLength(A, Desc)
    if not TryApplyArrayLengthDescriptor(Self, ADescriptor) then
      ThrowTypeError(Format(SErrorCannotRedefineNonConfigurable, [AName]),
        SSuggestCannotDeleteNonConfigurable);
    ADescriptor.Free;
    Exit;
  end;

  if TryParseArrayElementIndex(AName, Index) then
  begin
    if (Index >= FLength) and not FLengthWritable then
      ThrowTypeError(Format(SErrorCannotRedefineNonConfigurable, [AName]),
        SSuggestCannotDeleteNonConfigurable);

    if CanStoreDenseElementIndex(Index, FElements.Count) and
       (Index < FElements.Count) and
       (FElements[Integer(Index)] <> TGocciaHoleValue.HoleValue) then
    begin
      if ADescriptor.Fields = [] then
      begin
        ADescriptor.Free;
        Exit;
      end;
      if CanKeepDenseArrayElement(ADescriptor) then
      begin
        if ADescriptor.HasValue then
          FElements[Integer(Index)] := TGocciaPropertyDescriptorData(ADescriptor).Value;
        ADescriptor.Free;
        Exit;
      end;
      MaterializeDenseArrayElement(Self, AName, Integer(Index));
    end;

    if FProperties.ContainsKey(AName) then
    begin
      inherited DefineProperty(AName, ADescriptor);
      if Index + 1 > FLength then
        FLength := Index + 1;
      Exit;
    end;

    if not Extensible then
      ThrowTypeError(Format(SErrorCannotAddPropertyNotExtensible, [AName]),
        SSuggestObjectNotExtensible);

    if CanStoreDenseElementIndex(Index, FElements.Count) and
       CanCreateDenseArrayElement(ADescriptor) then
    begin
      ExtendElementsWithHoles(FElements, Int64(Index) + 1);
      FElements[Integer(Index)] := TGocciaPropertyDescriptorData(ADescriptor).Value;
      if Index + 1 > FLength then
        FLength := Index + 1;
      ADescriptor.Free;
      Exit;
    end;

    // Non-dense descriptors must be stored in the ordinary property map so
    // missing/defaulted attributes keep their spec values.
    inherited DefineProperty(AName, ADescriptor);
    if CanStoreDenseElementIndex(Index, FElements.Count) then
      ExtendElementsWithHoles(FElements, Int64(Index) + 1);
    if Index + 1 > FLength then
      FLength := Index + 1;
    if CanStoreDenseElementIndex(Index, FElements.Count) and
       not IsArrayHole(FElements[Integer(Index)]) then
      FElements[Integer(Index)] := TGocciaHoleValue.HoleValue;
    Exit;
  end;

  inherited DefineProperty(AName, ADescriptor);
end;

// ES2026 §10.4.2.1 ArrayDefineOwnProperty — boolean variant
function TGocciaArrayValue.TryDefineProperty(const AName: string; const ADescriptor: TGocciaPropertyDescriptor): Boolean;
var
  Index: Int64;
begin
  if AName = PROP_LENGTH then
  begin
    // §10.4.2.4 ArraySetLength(A, Desc)
    try
      Result := TryApplyArrayLengthDescriptor(Self, ADescriptor);
    finally
      ADescriptor.Free;
    end;
    Exit;
  end;

  // Numeric index — update FElements directly
  if TryParseArrayElementIndex(AName, Index) then
  begin
    if (Index >= FLength) and not FLengthWritable then
    begin
      ADescriptor.Free;
      Exit(False);
    end;

    if CanStoreDenseElementIndex(Index, FElements.Count) and
       (Index < FElements.Count) and
       (FElements[Integer(Index)] <> TGocciaHoleValue.HoleValue) then
    begin
      if ADescriptor.Fields = [] then
      begin
        ADescriptor.Free;
        Exit(True);
      end;
      if CanKeepDenseArrayElement(ADescriptor) then
      begin
        if ADescriptor.HasValue then
          FElements[Integer(Index)] := TGocciaPropertyDescriptorData(ADescriptor).Value;
        ADescriptor.Free;
        Exit(True);
      end;
      MaterializeDenseArrayElement(Self, AName, Integer(Index));
    end;

    if FProperties.ContainsKey(AName) then
    begin
      Result := inherited TryDefineProperty(AName, ADescriptor);
      if Result and (Index + 1 > FLength) then
        FLength := Index + 1;
      Exit;
    end;

    if not Extensible then
    begin
      ADescriptor.Free;
      Exit(False);
    end;

    if CanStoreDenseElementIndex(Index, FElements.Count) and
       CanCreateDenseArrayElement(ADescriptor) then
    begin
      ExtendElementsWithHoles(FElements, Int64(Index) + 1);
      FElements[Integer(Index)] := TGocciaPropertyDescriptorData(ADescriptor).Value;
      if Index + 1 > FLength then
        FLength := Index + 1;
      ADescriptor.Free;
      Exit(True);
    end;

    // Store non-dense descriptors on the ordinary property map. This preserves
    // omitted fields as false/undefined for new array-index properties and
    // keeps non-default attributes visible to subsequent [[GetOwnProperty]].
    Result := inherited TryDefineProperty(AName, ADescriptor);
    if Result then
    begin
      if CanStoreDenseElementIndex(Index, FElements.Count) then
        ExtendElementsWithHoles(FElements, Int64(Index) + 1);
      if Index + 1 > FLength then
        FLength := Index + 1;
      if CanStoreDenseElementIndex(Index, FElements.Count) and
         not IsArrayHole(FElements[Integer(Index)]) then
        FElements[Integer(Index)] := TGocciaHoleValue.HoleValue;
    end;
    Exit;
  end;

  // Non-index, non-length — delegate to inherited
  Result := inherited TryDefineProperty(AName, ADescriptor);
end;

// ES2026 §23.1.3.29 Array.prototype.sort(comparefn)
function TGocciaArrayValue.ArraySort(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  View: TArrayLikeView;
  TempArr: TGocciaArrayValue;
  CustomSortFunction: TGocciaValue;
  CallArgs: TGocciaArgumentsCollection;
  I: Integer;
  ReceiverRoot, TempRoot: TGocciaTempRoot;
begin
  // Step 1: If comparefn is not undefined and IsCallable(comparefn) is false,
  // throw a TypeError. Per spec §23.1.3.29 this runs BEFORE LengthOfArrayLike.
  if AArgs.Length > 0 then
  begin
    CustomSortFunction := AArgs.GetElement(0);
    if (not (CustomSortFunction is TGocciaUndefinedLiteralValue)) and
       (not CustomSortFunction.IsCallable) then
      ThrowTypeError(SErrorCustomSortMustBeFunction, SSuggestCallbackRequired);
  end
  else
    CustomSortFunction := TGocciaUndefinedLiteralValue.UndefinedValue;

  View.Init(AThisValue);
  InitializeTempRoot(ReceiverRoot);
  InitializeTempRoot(TempRoot);
  AddTempRootIfNeeded(ReceiverRoot, View.Obj);
  try

    // Collect only present elements via View for prototype-aware access
    TempArr := TGocciaArrayValue.Create;
    AddTempRootIfNeeded(TempRoot, TempArr);
    try
      for I := 0 to View.Len - 1 do
      begin
        if View.HasIndex(I) then
          TempArr.Elements.Add(View.Get(I));
      end;

      if not (CustomSortFunction is TGocciaUndefinedLiteralValue) then
      begin
        CallArgs := TGocciaArgumentsCollection.Create([nil, nil]);
        try
          if TempArr.Elements.Count > 1 then
            StableSortElements(TempArr.Elements, CustomSortFunction, CallArgs, AThisValue);
        finally
          CallArgs.Free;
        end;
      end else if TempArr.Elements.Count > 1 then
        StableSortElementsDefault(TempArr.Elements);

      // Write sorted elements back to front indices
      for I := 0 to TempArr.Elements.Count - 1 do
        View.Put(I, TempArr.Elements[I]);
      // Delete trailing indices (holes moved to end)
      for I := TempArr.Elements.Count to View.Len - 1 do
        View.DeleteIndex(I);
    finally
      RemoveTempRootIfNeeded(TempRoot);
    end;
  finally
    RemoveTempRootIfNeeded(ReceiverRoot);
  end;

  Result := View.Obj;
end;

// ES2026 §23.1.3.32 Array.prototype.splice(start, deleteCount, ...items)
function TGocciaArrayValue.ArraySplice(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  View: TArrayLikeView;
  Removed: TGocciaObjectValue;
  ActualStart, DeleteCount, ItemCount, NewLen: Integer;
  I, From, Target: Integer;
  RawStart, RawDeleteCount, RawActualStart, RawActualDeleteCount, RawNewLen: Double;
  Shift, RemovedRangeStart, TargetSrcKey, K: Int64;
  SourceSparse, DestSparse, TrailingSparse, RemovedSparse: TArray<Int64>;
  SrcIdx: Integer;
begin
  // Step 1: Let O be ToObject(this value)
  View.Init(AThisValue);

  // Step 5: If start is not present
  if AArgs.Length < 1 then
  begin
    if Assigned(View.SpeciesArr) then
      Removed := ArraySpeciesCreate(View.Obj, 0)
    else
      Removed := TGocciaArrayValue.Create;
    if not (Assigned(View.Arr) and View.Arr.FLengthWritable) then
      View.Obj.AssignProperty(PROP_LENGTH,
        TGocciaNumberLiteralValue.Create(View.RawLen));
    Result := Removed;
    Exit;
  end;

  // Step 3: Let relativeStart be ToIntegerOrInfinity(start).  Computed in
  // Double against RawLen so out-of-range and negative starts clamp against
  // the spec length, not the truncated View.Len, on receivers whose length
  // exceeds MaxInt (matches ArraySlice / ArrayToSpliced).
  RawStart := AArgs.GetElement(0).ToNumberLiteral.Value;
  if IsNaN(RawStart) then
    RawStart := 0
  else if not IsInfinite(RawStart) then
    RawStart := Trunc(RawStart);

  // Step 4: If relativeStart < 0, let actualStart be max(len + relativeStart, 0); else min(relativeStart, len)
  // The `0.0` literal forces Math.Max to bind to its Double overload (see
  // ArraySlice for the Single-narrowing rationale).
  if RawStart < 0 then
    RawActualStart := Max(View.RawLen + RawStart, 0.0)
  else
    RawActualStart := Min(RawStart, View.RawLen);

  // Steps 5-9: Determine actualDeleteCount
  if AArgs.Length > 1 then
  begin
    RawDeleteCount := AArgs.GetElement(1).ToNumberLiteral.Value;
    if IsNaN(RawDeleteCount) then
      RawDeleteCount := 0
    else if not IsInfinite(RawDeleteCount) then
      RawDeleteCount := Trunc(RawDeleteCount);
    RawActualDeleteCount := Min(Max(RawDeleteCount, 0.0), View.RawLen - RawActualStart);
  end
  else
    RawActualDeleteCount := View.RawLen - RawActualStart;

  ItemCount := AArgs.Length - 2;
  if ItemCount < 0 then
    ItemCount := 0;

  // Step 9a: If len + itemCount - actualDeleteCount > 2^53 - 1, throw TypeError.
  // Also reject when the post-mutation length would exceed MaxInt — SetLen
  // takes Integer, so we cannot address indices past it.
  RawNewLen := View.RawLen + ItemCount - RawActualDeleteCount;
  View.CheckSafeIntegerLen(RawNewLen);
  View.CheckArrayCreateLenValue(RawNewLen);

  // RawActualStart is bounded by NewLen + DeleteCount - ItemCount ≤ MaxInt
  // after CheckArrayCreateLenValue.  RawActualDeleteCount is not — e.g.
  // splice(0, 2^40-100) on a {length: 2^40} receiver gives a 2^40 delete
  // count with NewLen = 100.  Truncating without the guard would wrap.
  // Refuse the operation: the in-place shift loop below is Integer-bounded.
  if RawActualDeleteCount > MaxInt then
    ThrowRangeError(
      'Array splice delete count exceeds engine maximum (MaxInt)',
      'use a smaller deleteCount');
  ActualStart := Integer(Trunc(RawActualStart));
  DeleteCount := Integer(Trunc(RawActualDeleteCount));

  // Step 10: Let A be ArraySpeciesCreate(O, actualDeleteCount)
  if Assigned(View.SpeciesArr) then
    Removed := ArraySpeciesCreate(View.Obj, DeleteCount)
  else
    Removed := TGocciaArrayValue.Create;

  // Compute NewLen from the spec-relative RawNewLen so a receiver with
  // RawLen > MaxInt (whose truncated View.Len lost the high bits) still
  // produces the spec-mandated length when the post-mutation length fits
  // MaxInt.  Both ItemCount > DeleteCount (growth) and the equal case are
  // unreachable when RawLen > MaxInt because either branch keeps RawNewLen
  // ≥ RawLen, which CheckArrayCreateLenValue already rejected — only the
  // shrink path needs sparse handling.
  NewLen := Integer(Trunc(RawNewLen));

  if View.NeedsSparsePath then
  begin
    // Sparse path: receiver is a generic object with RawLen > MaxInt.  The
    // dense Integer-indexed shift and trailing-delete loops below saturate
    // at View.Len = MaxInt and silently drop high-index source properties
    // and orphan high-index trailing properties.  Walk the actually-present
    // indices via CollectSparseIndicesInRange + Get64/HasIndex64, mapping
    // writes to dest positions that always fit Integer (NewLen ≤ MaxInt).

    // Step 11 (sparse): Collect removed elements.  ActualStart + DeleteCount
    // can exceed MaxInt here (DeleteCount may equal MaxInt and ActualStart
    // is bounded only by NewLen ≤ MaxInt), so use Int64 arithmetic for the
    // source range.  Removed slot index K - RemovedRangeStart fits Integer
    // because DeleteCount ≤ MaxInt.
    RemovedRangeStart := Int64(ActualStart);
    RemovedSparse := CollectSparseIndicesInRange(View.Obj,
      RemovedRangeStart, RemovedRangeStart + Int64(DeleteCount));
    // FPC rejects Int64 as a classic for-loop counter on 32-bit targets;
    // the enumerator form `for K in TArray<Int64>` compiles everywhere.
    for K in RemovedSparse do
      ArrayCreateDataProperty(Removed, Integer(K - RemovedRangeStart),
        View.Get64(K));

    if ItemCount < DeleteCount then
    begin
      // Shift left: source [ActualStart + DeleteCount, RawLen) → dest
      // [ActualStart + ItemCount, NewLen).  Walk source-present keys to
      // write values, then walk originally-present dest keys whose source
      // counterpart is absent to delete them.  Source keys are read in
      // ascending order; since shift > 0, no read aliases a prior write.
      Shift := Int64(DeleteCount) - Int64(ItemCount);
      SourceSparse := CollectSparseIndicesInRange(View.Obj,
        Int64(ActualStart) + Int64(DeleteCount), View.Len64);
      DestSparse := CollectSparseIndicesInRange(View.Obj,
        Int64(ActualStart) + Int64(ItemCount), Int64(NewLen));
      for K in SourceSparse do
        View.Obj.AssignProperty(IntToStr(K - Shift), View.Get64(K));
      // Walk both sorted arrays in parallel for O(n+m) "is K's source
      // counterpart present?" checks.
      SrcIdx := 0;
      for K in DestSparse do
      begin
        TargetSrcKey := K + Shift;
        while (SrcIdx < Length(SourceSparse)) and
              (SourceSparse[SrcIdx] < TargetSrcKey) do
          Inc(SrcIdx);
        if (SrcIdx >= Length(SourceSparse)) or
           (SourceSparse[SrcIdx] <> TargetSrcKey) then
          View.Obj.DeleteProperty(IntToStr(K));
      end;

      // Delete properties in [NewLen, RawLen) — the original trailing range
      // including any indices past MaxInt.  The dense View.DeleteIndex loop
      // below would only reach MaxInt - 1 because View.Len is truncated.
      TrailingSparse := CollectSparseIndicesInRange(View.Obj,
        Int64(NewLen), View.Len64);
      for K in TrailingSparse do
        View.Obj.DeleteProperty(IntToStr(K));
    end;

    // Insert new items (Integer-bounded: ActualStart + ItemCount ≤ NewLen).
    for I := 0 to ItemCount - 1 do
      View.Put(ActualStart + I, AArgs.GetElement(I + 2));

    // Set the spec-relative length on the generic object.  View.SetLen on
    // the array path is unreachable here (NeedsSparsePath requires Arr nil).
    View.Obj.AssignProperty('length', TGocciaNumberLiteralValue.Create(NewLen));
  end
  else
  begin
    // Step 11: Collect removed elements (preserve sparsity per ES spec §23.1.3.32 step 11)
    for I := 0 to DeleteCount - 1 do
    begin
      if View.HasIndex(ActualStart + I) then
        ArrayCreateDataProperty(Removed, I, View.Get(ActualStart + I));
      // else: absent → leave hole in Removed (sparse)
    end;

    // Shift elements via View methods for prototype-aware semantics.
    if ItemCount < DeleteCount then
    begin
      // Shift elements left
      for I := ActualStart to View.Len - DeleteCount - 1 do
      begin
        From := I + DeleteCount;
        Target := I + ItemCount;
        if View.HasIndex(From) then
          View.Put(Target, View.Get(From))
        else
          View.DeleteIndex(Target);
      end;
      // Delete trailing properties
      for I := NewLen to View.Len - 1 do
        View.DeleteIndex(I);
    end
    else if ItemCount > DeleteCount then
    begin
      // Shift elements right
      for I := View.Len - DeleteCount - 1 downto ActualStart do
      begin
        From := I + DeleteCount;
        Target := I + ItemCount;
        if View.HasIndex(From) then
          View.Put(Target, View.Get(From))
        else
          View.DeleteIndex(Target);
      end;
    end;
    // Insert new items
    for I := 0 to ItemCount - 1 do
      View.Put(ActualStart + I, AArgs.GetElement(I + 2));
    View.SetLen(NewLen);
  end;

  // Ensure removed array has correct length (preserve trailing holes)
  Removed.SetProperty(PROP_LENGTH, TGocciaNumberLiteralValue.Create(DeleteCount));

  // Step 17: Return A
  Result := Removed;
end;

// ES2026 §23.1.3.25 Array.prototype.shift()
function TGocciaArrayValue.ArrayShift(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  View: TArrayLikeView;
  I: Integer;
begin
  // Step 1: Let O be ToObject(this value)
  View.Init(AThisValue);

  // Step 3: If len = 0, set length to 0 and return undefined
  if View.Len = 0 then
  begin
    View.SetLen(0);
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
    Exit;
  end;

  // Step 4: Let first be Get(O, "0")
  Result := View.Get(0);

  // Steps 5-7: shift elements down via View for prototype-aware semantics
  for I := 1 to View.Len - 1 do
  begin
    if View.HasIndex(I) then
      View.Put(I - 1, View.Get(I))
    else
      View.DeleteIndex(I - 1);
  end;
  View.DeleteIndex(View.Len - 1);
  View.SetLen(View.Len - 1);
end;

// ES2026 §23.1.3.37 Array.prototype.unshift(...items)
function TGocciaArrayValue.ArrayUnshift(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  View: TArrayLikeView;
  ArgCount, NewLen, I: Integer;
begin
  // Step 1: Let O be ToObject(this value)
  View.Init(AThisValue);
  ArgCount := AArgs.Length;

  // ES2026 §23.1.3.37 step 4: skip shifting when no arguments provided.
  // Step 5 still requires Set(O, "length", F(len), true); for native arrays
  // length is intrinsic, for generic receivers we write through Obj so
  // accessor/writability semantics fire.  Use RawLen so receivers with
  // length > MaxInt aren't truncated back to View.Len.
  if ArgCount = 0 then
  begin
    if Assigned(View.Arr) and View.Arr.FLengthWritable then
    begin
      Result := TGocciaNumberLiteralValue.Create(View.RawLen);
      Exit;
    end;
    View.Obj.AssignProperty(PROP_LENGTH,
      TGocciaNumberLiteralValue.Create(View.RawLen));
    Result := TGocciaNumberLiteralValue.Create(View.RawLen);
    Exit;
  end;

  // Step 4.a: If len + argCount > 2^53 - 1, throw TypeError
  View.CheckSafeIntegerLen(View.RawLen + ArgCount);

  // Shift existing elements up by argCount via View for prototype-aware semantics
  NewLen := View.Len + ArgCount;
  for I := View.Len - 1 downto 0 do
  begin
    if View.HasIndex(I) then
      View.Obj.AssignProperty(IntToStr(I + ArgCount), View.Get(I))
    else
      View.DeleteIndex(I + ArgCount);
  end;
  // Set items at front
  for I := 0 to ArgCount - 1 do
    View.Obj.AssignProperty(IntToStr(I), AArgs.GetElement(I));
  if (not Assigned(View.Arr)) or (View.Arr.FLength <> NewLen) then
    View.SetLen(NewLen);

  // Step 7: Return len + argCount
  Result := TGocciaNumberLiteralValue.Create(NewLen);
end;

// ES2026 §23.1.3.7 Array.prototype.fill(value [, start [, end]])
function TGocciaArrayValue.ArrayFill(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  View: TArrayLikeView;
  FillValue: TGocciaValue;
  StartIdx, EndIdx, I: Integer;
  Start64, End64, I64: Int64;
begin
  // Step 1: Let O be ToObject(this value)
  View.Init(AThisValue);

  // Step 3: Let value (default undefined)
  if AArgs.Length < 1 then
    FillValue := TGocciaUndefinedLiteralValue.UndefinedValue
  else
    FillValue := AArgs.GetElement(0);

  if View.NeedsSparsePath then
  begin
    Start64 := NormalizeRelativeIndex64(ToInteger64FromArgs(AArgs, 1), View.Len64);
    if (AArgs.Length > 2) and
       not (AArgs.GetElement(2) is TGocciaUndefinedLiteralValue) then
      End64 := NormalizeRelativeIndex64(ToInteger64FromArgs(AArgs, 2), View.Len64)
    else
      End64 := View.Len64;

    if (View.Obj is TGocciaStringObjectValue) and (Start64 < End64) then
      ThrowTypeError(Format(SErrorCannotAssignReadOnly, [IntToStr(Start64)]),
        SSuggestCannotDeleteNonConfigurable);

    I64 := Start64;
    while I64 < End64 do
    begin
      View.Obj.AssignProperty(IntToStr(I64), FillValue);
      Inc(I64);
    end;

    Result := View.Obj;
    Exit;
  end;

  // Step 4: Let relativeStart be ToIntegerOrInfinity(start)
  StartIdx := ToIntegerFromArgs(AArgs, 1);

  // Step 6: If end is undefined, let relativeEnd be len; else ToIntegerOrInfinity(end)
  if (AArgs.Length > 2) and
     not (AArgs.GetElement(2) is TGocciaUndefinedLiteralValue) then
    EndIdx := ToIntegerFromArgs(AArgs, 2)
  else
    EndIdx := View.Len;

  // Step 5: If relativeStart < 0, let k be max(len + relativeStart, 0); else min(relativeStart, len)
  StartIdx := NormalizeRelativeIndex(StartIdx, View.Len);
  // Step 7: If relativeEnd < 0, let final be max(len + relativeEnd, 0); else min(relativeEnd, len)
  EndIdx := NormalizeRelativeIndex(EndIdx, View.Len);

  if (View.Obj is TGocciaStringObjectValue) and (StartIdx < EndIdx) then
    ThrowTypeError(Format(SErrorCannotAssignReadOnly, [IntToStr(StartIdx)]),
      SSuggestCannotDeleteNonConfigurable);

  // Step 8: Repeat, while k < final — Set(O, ToString(k), value)
  for I := StartIdx to EndIdx - 1 do
    View.Put(I, FillValue);

  // Step 9: Return O
  Result := View.Obj;
end;

// ES2026 §23.1.3.1 Array.prototype.at(index)
function TGocciaArrayValue.ArrayAt(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  View: TArrayLikeView;
  Index64, Len64Val: Int64;
begin
  // Step 1: Let O be ToObject(this value)
  View.Init(AThisValue);

  // Step 3: Let relativeIndex be ToIntegerOrInfinity(index)
  // A missing index coerces to 0, not undefined. 64-bit math so generic
  // array-likes with length beyond MaxInt index correctly (Len64/Get64).
  Index64 := ToInteger64FromArgs(AArgs, 0);
  Len64Val := View.Len64;

  // Step 4: If relativeIndex >= 0, let k be relativeIndex; else len + relativeIndex
  if Index64 < 0 then
    Index64 := Len64Val + Index64;

  // Step 5: If k < 0 or k >= len, return undefined
  if (Index64 < 0) or (Index64 >= Len64Val) then
    Result := TGocciaUndefinedLiteralValue.UndefinedValue
  else
    // Step 6: Return Get(O, ToString(k))
    Result := View.Get64(Index64);
end;

// ES2026 §23.1.3.36 Array.prototype.values()
function TGocciaArrayValue.ArrayValues(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  // Step 1: Let O be ToObject(this value)
  // Step 2: Return CreateArrayIterator(O, value)
  Result := TGocciaArrayIteratorValue.Create(ToObject(AThisValue), akValues);
end;

// ES2026 §23.1.3.17 Array.prototype.keys()
function TGocciaArrayValue.ArrayKeys(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  // Step 1: Let O be ToObject(this value)
  // Step 2: Return CreateArrayIterator(O, key)
  Result := TGocciaArrayIteratorValue.Create(ToObject(AThisValue), akKeys);
end;

// ES2026 §23.1.3.5 Array.prototype.entries()
function TGocciaArrayValue.ArrayEntries(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  // Step 1: Let O be ToObject(this value)
  // Step 2: Return CreateArrayIterator(O, key+value)
  Result := TGocciaArrayIteratorValue.Create(ToObject(AThisValue), akEntries);
end;

// ES2026 §23.1.3.39 Array.prototype[@@iterator]()
function TGocciaArrayValue.ArraySymbolIterator(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  // Step 1: Return the result of calling Array.prototype.values
  Result := TGocciaArrayIteratorValue.Create(ToObject(AThisValue), akValues);
end;

initialization
  GArrayPrototypeSlot := RegisterRealmSlot('Array.prototype');
  GArrayMethodHostSlot := RegisterRealmSlot('Array.prototype.methodHost');

end.
