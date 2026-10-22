unit Goccia.Values.SharedArrayBufferValue;

{$I Goccia.inc}

interface

uses
  SysUtils,

  Goccia.Arguments.Collection,
  Goccia.ObjectModel,
  Goccia.Realm,
  Goccia.SharedPrototype,
  Goccia.Values.ClassValue,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  { A SharedArrayBuffer keeps one data block from construction to collection,
    as every agent that holds it must read and write the same memory. A
    growable buffer's block is allocated at [[ArrayBufferMaxByteLength]] bytes
    (ES2026 §25.2.2.1 AllocateSharedArrayBuffer steps 6-7); grow only raises
    FByteLength, the bytes the buffer exposes, and never moves the block.
    Script code never reaches past FByteLength, but native code given an FFI
    pointer can, so grow zeroes the bytes it exposes before it exposes them. }
  TGocciaSharedArrayBufferValue = class(TGocciaInstanceValue)
  private
    const NO_MAX_BYTE_LENGTH = -1;
  private
    FData: TBytes;
    // [[ArrayBufferByteLengthData]]: read by every agent through
    // AtomicLoadInt32, raised only by grow's compare-and-exchange.
    FByteLength: Integer;
    FMaxByteLength: Integer;
    FHasPendingAllocation: Boolean;
    FPendingByteLength: Double;
    FPendingMaxByteLength: Double;
    FChargedBytes: Int64;
    procedure AllocateBlock(const AByteLength, AMaxByteLength: Integer);

    function GetByteLength: Integer;
    function GetObservableMaxByteLength: Integer;

    procedure InitializePrototype;
  public
    constructor Create(const AByteLength: Integer = 0); overload;
    constructor Create(const AByteLength: Integer; const AMaxByteLength: Integer); overload;
    constructor Create(const AClass: TGocciaClassValue); overload;
    destructor Destroy; override;

    function GetProperty(const AName: string): TGocciaValue; override;
    function GetPropertyWithContext(const AName: string; const AThisContext: TGocciaValue): TGocciaValue; override;
    function ToStringTag: string; override;

    procedure InitializeNativeFromArguments(const AArguments: TGocciaArgumentsCollection); override;
    procedure FinalizeNativeFromArguments(const AArguments: TGocciaArgumentsCollection); override;
    procedure TraceReferences; override;

    class procedure ExposePrototype(const AConstructor: TGocciaValue);
    class function GetSharedPrototypeForRealm(
      const ARealm: TGocciaRealm): TGocciaObjectValue; static;

    { The whole data block, which never changes once the buffer is
      constructed. For a growable buffer it is MaxByteLength bytes long, and
      only its first ByteLength bytes are the buffer's contents: measure with
      ByteLength, never with Length(Data). }
    property Data: TBytes read FData;
    property MaxByteLength: Integer read FMaxByteLength;
  published
    property ByteLength: Integer read GetByteLength;
    function SharedArrayBufferSlice(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function SharedArrayBufferGrow(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function SharedArrayBufferByteLengthGetter(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function SharedArrayBufferMaxByteLengthGetter(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function SharedArrayBufferGrowableGetter(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
  end;

implementation

uses
  Math,

  CriticalSections,

  Goccia.Constants.ConstructorNames,
  Goccia.Constants.NumericLimits,
  Goccia.Constants.PropertyNames,
  Goccia.Error.Messages,
  Goccia.Error.Suggestions,
  Goccia.GarbageCollector,
  Goccia.Values.ErrorHelper,
  Goccia.Values.FunctionBase,
  Goccia.Values.NativeFunction,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.SymbolValue;

var
  GSharedArrayBufferSharedSlot: TGocciaRealmOwnedSlotId;
  // Serializes grows, so the bytes one grow zeroes are never bytes another
  // grow has already exposed.
  GSharedArrayBufferGrowLock: TGocciaCriticalSection;

function GetSharedArrayBufferShared: TGocciaSharedPrototype; {$IFDEF FPC}inline;{$ENDIF}
begin
  if (CurrentRealm <> nil) then
    Result := TGocciaSharedPrototype(CurrentRealm.GetOwnedSlot(GSharedArrayBufferSharedSlot))
  else
    Result := nil;
end;

function ToSharedArrayBufferIndex(const AValue: TGocciaValue): Integer;
var
  IntegerIndex: Double;
  Num: TGocciaNumberLiteralValue;
begin
  if (AValue = nil) or (AValue is TGocciaUndefinedLiteralValue) then
    Exit(0);

  Num := AValue.ToNumberLiteral;
  if Num.IsNaN then
    IntegerIndex := 0
  else if Num.IsInfinite then
  begin
    ThrowRangeError(SErrorInvalidSharedArrayBufferLength, SSuggestArrayLengthRange);
    Exit(0);
  end
  else
    IntegerIndex := Trunc(Num.Value);

  if (IntegerIndex < 0) or (IntegerIndex > MAX_SAFE_INTEGER_F) or
     (IntegerIndex > High(Integer)) then
    ThrowRangeError(SErrorInvalidSharedArrayBufferLength, SSuggestArrayLengthRange);

  Result := Trunc(IntegerIndex);
end;

function ToSharedArrayBufferConstructorIndex(const AValue: TGocciaValue): Double;
var
  IntegerIndex: Double;
  Num: TGocciaNumberLiteralValue;
begin
  if (AValue = nil) or (AValue is TGocciaUndefinedLiteralValue) then
    Exit(0);

  Num := AValue.ToNumberLiteral;
  if Num.IsNaN then
    Exit(0);
  if Num.IsInfinite then
    ThrowRangeError(SErrorInvalidSharedArrayBufferLength, SSuggestArrayLengthRange);

  IntegerIndex := Trunc(Num.Value);
  if (IntegerIndex < 0) or (IntegerIndex > MAX_SAFE_INTEGER_F) then
    ThrowRangeError(SErrorInvalidSharedArrayBufferLength, SSuggestArrayLengthRange);

  Result := IntegerIndex;
end;

function ToSharedArrayBufferSliceIndex(const AValue: TGocciaValue;
  const ALength: Integer): Integer;
var
  IntegerIndex: Double;
  Num: TGocciaNumberLiteralValue;
begin
  Num := AValue.ToNumberLiteral;
  if Num.IsNaN or Num.IsNegativeInfinity then
    Exit(0);
  if Num.IsInfinity then
    Exit(ALength);

  IntegerIndex := Trunc(Num.Value);
  if IntegerIndex < 0 then
  begin
    if IntegerIndex <= -ALength then
      Exit(0);
    Exit(Max(ALength + Trunc(IntegerIndex), 0));
  end;

  if IntegerIndex >= ALength then
    Exit(ALength);
  Result := Trunc(IntegerIndex);
end;

function SharedArrayBufferSpeciesConstructor(
  const ABuffer: TGocciaSharedArrayBufferValue): TGocciaValue;
var
  ConstructorValue, SpeciesValue: TGocciaValue;
begin
  // ES2026 §7.3.22 SpeciesConstructor(O, defaultConstructor)
  ConstructorValue := ABuffer.GetProperty(PROP_CONSTRUCTOR);
  if ConstructorValue is TGocciaUndefinedLiteralValue then
    Exit(TGocciaUndefinedLiteralValue.UndefinedValue);
  if not (ConstructorValue is TGocciaObjectValue) then
    ThrowTypeError(SErrorSpeciesNotConstructor, SSuggestSpeciesConstructor);

  SpeciesValue := TGocciaObjectValue(ConstructorValue).GetSymbolProperty(
    TGocciaSymbolValue.WellKnownSpecies);
  if (SpeciesValue is TGocciaUndefinedLiteralValue) or
     (SpeciesValue is TGocciaNullLiteralValue) then
    Exit(TGocciaUndefinedLiteralValue.UndefinedValue);
  if not SpeciesValue.IsConstructable then
    ThrowTypeError(SErrorSpeciesNotConstructor, SSuggestSpeciesConstructor);
  Result := SpeciesValue;
end;

// ES2026 §25.1.3.2 ArrayBufferByteLength(arrayBuffer, order) for a
// SharedArrayBuffer: a growable buffer's length is read from
// [[ArrayBufferByteLengthData]], which another agent's grow may raise.
function TGocciaSharedArrayBufferValue.GetByteLength: Integer;
begin
  Result := AtomicLoadInt32(FByteLength);
end;

// ES2026 §25.2.5.5 get SharedArrayBuffer.prototype.maxByteLength, steps 4-5:
// the byte length for a fixed-length buffer, [[ArrayBufferMaxByteLength]]
// otherwise.
function TGocciaSharedArrayBufferValue.GetObservableMaxByteLength: Integer;
begin
  if FMaxByteLength >= 0 then
    Result := FMaxByteLength
  else
    Result := GetByteLength;
end;

{ Allocates the buffer's data block: AByteLength bytes for a fixed-length
  buffer, AMaxByteLength bytes for a growable one (AMaxByteLength >= 0). The
  whole block is charged against the memory limit, because it is all
  allocated. Runs only while the buffer is constructed, before any view or
  agent can hold the block; afterwards FData never changes. }
procedure TGocciaSharedArrayBufferValue.AllocateBlock(
  const AByteLength, AMaxByteLength: Integer);
var
  AllocLength: Integer;
  Delta: Int64;
  GC: TGarbageCollector;
begin
  // ES2026 §25.2.2.1 AllocateSharedArrayBuffer steps 6-7: a growable buffer's
  // block holds maxByteLength bytes.
  if AMaxByteLength >= 0 then
    AllocLength := AMaxByteLength
  else
    AllocLength := AByteLength;
  Delta := AllocLength - FChargedBytes;
  GC := TGarbageCollector.Instance;
  if Assigned(GC) and (Delta > 0) then
  begin
    if not GC.TryReserveExternalBytes(Delta, Self) then
      ThrowRangeError(SErrorMemoryLimitExceeded,
        SSuggestMemoryLimitExceeded);
  end;
  try
    // A new, zero-filled block: the constructor's placeholder block is
    // empty, so nothing is copied.
    FData := nil;
    SetLength(FData, AllocLength);
  except
    if Assigned(GC) and (Delta > 0) then
      GC.ReleaseExternalBytes(Delta);
    raise;
  end;
  if Assigned(GC) and (Delta < 0) then
    GC.ReleaseExternalBytes(-Delta);
  FChargedBytes := AllocLength;
  FByteLength := AByteLength;
  FMaxByteLength := AMaxByteLength;
end;

constructor TGocciaSharedArrayBufferValue.Create(const AByteLength: Integer);
var
  Shared: TGocciaSharedPrototype;
begin
  inherited Create(nil);
  FMaxByteLength := NO_MAX_BYTE_LENGTH;
  FHasPendingAllocation := False;
  FPendingByteLength := 0;
  FPendingMaxByteLength := NO_MAX_BYTE_LENGTH;
  FChargedBytes := 0;
  AllocateBlock(AByteLength, NO_MAX_BYTE_LENGTH);
  InitializePrototype;
  Shared := GetSharedArrayBufferShared;
  if Assigned(Shared) then
    FPrototype := Shared.Prototype;
end;

constructor TGocciaSharedArrayBufferValue.Create(const AByteLength: Integer; const AMaxByteLength: Integer);
var
  Shared: TGocciaSharedPrototype;
begin
  inherited Create(nil);
  FHasPendingAllocation := False;
  FPendingByteLength := 0;
  FPendingMaxByteLength := NO_MAX_BYTE_LENGTH;
  FMaxByteLength := NO_MAX_BYTE_LENGTH;
  FChargedBytes := 0;
  if (AMaxByteLength >= 0) and (AByteLength > AMaxByteLength) then
    ThrowRangeError(SErrorInvalidSharedArrayBufferLength, SSuggestArrayLengthRange);
  if AMaxByteLength >= 0 then
    AllocateBlock(AByteLength, AMaxByteLength)
  else
    AllocateBlock(AByteLength, NO_MAX_BYTE_LENGTH);
  InitializePrototype;
  Shared := GetSharedArrayBufferShared;
  if Assigned(Shared) then
    FPrototype := Shared.Prototype;
end;

constructor TGocciaSharedArrayBufferValue.Create(const AClass: TGocciaClassValue);
var
  Shared: TGocciaSharedPrototype;
begin
  inherited Create(AClass);
  FMaxByteLength := NO_MAX_BYTE_LENGTH;
  FHasPendingAllocation := False;
  FPendingByteLength := 0;
  FPendingMaxByteLength := NO_MAX_BYTE_LENGTH;
  FChargedBytes := 0;
  AllocateBlock(0, NO_MAX_BYTE_LENGTH);
  InitializePrototype;
  Shared := GetSharedArrayBufferShared;
  if not Assigned(AClass) and Assigned(Shared) then
    FPrototype := Shared.Prototype;
end;

procedure TGocciaSharedArrayBufferValue.InitializePrototype;
var
  Members: TGocciaMemberCollection;
  Shared: TGocciaSharedPrototype;
  PrototypeMembers: TArray<TGocciaMemberDefinition>;
begin
  if (CurrentRealm = nil) then Exit;
  if (GetSharedArrayBufferShared <> nil) then Exit;

  Shared := TGocciaSharedPrototype.Create(Self);
  CurrentRealm.SetOwnedSlot(GSharedArrayBufferSharedSlot, Shared);
  Members := TGocciaMemberCollection.Create;
  try
    Members.AddMethod(SharedArrayBufferSlice, 2, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddMethod(SharedArrayBufferGrow, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddAccessor(PROP_BYTE_LENGTH, SharedArrayBufferByteLengthGetter, nil, [pfConfigurable]);
    Members.AddAccessor(PROP_MAX_BYTE_LENGTH, SharedArrayBufferMaxByteLengthGetter, nil, [pfConfigurable]);
    Members.AddAccessor(PROP_GROWABLE, SharedArrayBufferGrowableGetter, nil, [pfConfigurable]);
    Members.AddSymbolDataProperty(
      TGocciaSymbolValue.WellKnownToStringTag,
      TGocciaStringLiteralValue.Create(CONSTRUCTOR_SHARED_ARRAY_BUFFER),
      [pfConfigurable]);
    PrototypeMembers := Members.ToDefinitions;
  finally
    Members.Free;
  end;
  RegisterMemberDefinitions(Shared.Prototype, PrototypeMembers);
  MarkIntrinsicGetter(Shared.Prototype, PROP_BYTE_LENGTH,
    nikSharedArrayBufferByteLength);
  MarkIntrinsicGetter(Shared.Prototype, PROP_MAX_BYTE_LENGTH,
    nikSharedArrayBufferMaxByteLength);
  MarkIntrinsicGetter(Shared.Prototype, PROP_GROWABLE,
    nikSharedArrayBufferGrowable);
end;

class procedure TGocciaSharedArrayBufferValue.ExposePrototype(const AConstructor: TGocciaValue);
var
  Shared: TGocciaSharedPrototype;
begin
  Shared := GetSharedArrayBufferShared;
  if not Assigned(Shared) then
  begin
    TGocciaSharedArrayBufferValue.Create(0);
    Shared := GetSharedArrayBufferShared;
  end;
  if Assigned(Shared) then
    ExposeSharedPrototypeOnConstructor(Shared, AConstructor);
end;

class function TGocciaSharedArrayBufferValue.GetSharedPrototypeForRealm(
  const ARealm: TGocciaRealm): TGocciaObjectValue;
var
  Shared: TGocciaSharedPrototype;
begin
  Result := nil;
  if not Assigned(ARealm) then
    Exit;
  Shared := TGocciaSharedPrototype(ARealm.GetOwnedSlot(GSharedArrayBufferSharedSlot));
  if Assigned(Shared) then
    Result := Shared.Prototype;
end;

// ES2026 §25.2.3.1 SharedArrayBuffer(length [, options])
procedure TGocciaSharedArrayBufferValue.InitializeNativeFromArguments(const AArguments: TGocciaArgumentsCollection);
var
  Len: Double;
  MaxByteLengthValue: TGocciaValue;
  OptionsArg: TGocciaValue;
  RequestedMaxByteLength: Double;
begin
  if AArguments.Length = 0 then
    Len := 0
  else
    Len := ToSharedArrayBufferConstructorIndex(AArguments.GetElement(0));

  RequestedMaxByteLength := NO_MAX_BYTE_LENGTH;
  if AArguments.Length > 1 then
  begin
    OptionsArg := AArguments.GetElement(1);
    if not (OptionsArg is TGocciaUndefinedLiteralValue) and
       not OptionsArg.IsPrimitive then
    begin
      MaxByteLengthValue := OptionsArg.GetProperty(PROP_MAX_BYTE_LENGTH);
      if Assigned(MaxByteLengthValue) and
         not (MaxByteLengthValue is TGocciaUndefinedLiteralValue) then
        RequestedMaxByteLength := ToSharedArrayBufferConstructorIndex(MaxByteLengthValue);
    end;
  end;

  if RequestedMaxByteLength >= 0 then
  begin
    if Len > RequestedMaxByteLength then
      ThrowRangeError(SErrorInvalidSharedArrayBufferLength, SSuggestArrayLengthRange);
  end
  else
    RequestedMaxByteLength := NO_MAX_BYTE_LENGTH;

  FHasPendingAllocation := True;
  FPendingByteLength := Len;
  FPendingMaxByteLength := RequestedMaxByteLength;
end;

procedure TGocciaSharedArrayBufferValue.FinalizeNativeFromArguments(const AArguments: TGocciaArgumentsCollection);
var
  Len: Integer;
begin
  if not FHasPendingAllocation then
    Exit;

  if (FPendingByteLength > High(Integer)) or
     ((FPendingMaxByteLength >= 0) and (FPendingMaxByteLength > High(Integer))) then
    ThrowRangeError(SErrorInvalidSharedArrayBufferLength, SSuggestArrayLengthRange);

  Len := Trunc(FPendingByteLength);
  if FPendingMaxByteLength >= 0 then
    AllocateBlock(Len, Trunc(FPendingMaxByteLength))
  else
    AllocateBlock(Len, NO_MAX_BYTE_LENGTH);
  FHasPendingAllocation := False;
end;

procedure TGocciaSharedArrayBufferValue.TraceReferences;
begin
  inherited;
end;

destructor TGocciaSharedArrayBufferValue.Destroy;
var
  GC: TGarbageCollector;
begin
  GC := TGarbageCollector.Instance;
  if Assigned(GC) and (FChargedBytes > 0) then
    GC.ReleaseExternalBytes(FChargedBytes);
  FChargedBytes := 0;
  inherited;
end;

function TGocciaSharedArrayBufferValue.GetProperty(const AName: string): TGocciaValue;
begin
  Result := GetPropertyWithContext(AName, Self);
end;

// ES2026 §10.1.8.1 OrdinaryGet. byteLength, maxByteLength and growable are
// accessors on SharedArrayBuffer.prototype (§25.2.5) and nothing more, so a
// null prototype, a replaced one, an own property and a getter a subclass or a
// program defines are all found by the ordinary lookup. When this buffer is
// the receiver and the lookup reaches one of the built-in getters through
// plain objects, its result is computed here from the slots instead of through
// a call: the getter reads only its receiver's slots.
function TGocciaSharedArrayBufferValue.GetPropertyWithContext(const AName: string; const AThisContext: TGocciaValue): TGocciaValue;
var
  Kind: TGocciaNativeIntrinsicKind;
begin
  if (AThisContext = Self) and
     ResolvePropertyWithoutCall(Self, AName, Result, Kind) then
    case Kind of
      nikNone:
        Exit;
      nikSharedArrayBufferByteLength:
        Exit(TGocciaNumberLiteralValue.Create(GetByteLength));
      nikSharedArrayBufferMaxByteLength:
        Exit(TGocciaNumberLiteralValue.Create(GetObservableMaxByteLength));
      nikSharedArrayBufferGrowable:
        Exit(TGocciaBooleanLiteralValue.FromBoolean(FMaxByteLength >= 0));
    end;
  // Any other built-in getter (an ArrayBuffer or typed array one moved onto
  // this chain) is called, and rejects this receiver itself.
  Result := inherited GetPropertyWithContext(AName, AThisContext);
end;

function TGocciaSharedArrayBufferValue.ToStringTag: string;
begin
  Result := CONSTRUCTOR_SHARED_ARRAY_BUFFER;
end;

// ES2026 §25.2.5.1 get SharedArrayBuffer.prototype.byteLength
function TGocciaSharedArrayBufferValue.SharedArrayBufferByteLengthGetter(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  if not (AThisValue is TGocciaSharedArrayBufferValue) then
    ThrowTypeError(Format(SErrorRequiresSharedArrayBuffer,
      ['SharedArrayBuffer.prototype.byteLength']),
      SSuggestSharedArrayBufferThisType);
  Result := TGocciaNumberLiteralValue.Create(
    TGocciaSharedArrayBufferValue(AThisValue).GetByteLength);
end;

function TGocciaSharedArrayBufferValue.SharedArrayBufferMaxByteLengthGetter(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Buf: TGocciaSharedArrayBufferValue;
begin
  if not (AThisValue is TGocciaSharedArrayBufferValue) then
    ThrowTypeError(Format(SErrorRequiresSharedArrayBuffer,
      ['SharedArrayBuffer.prototype.maxByteLength']),
      SSuggestSharedArrayBufferThisType);
  Buf := TGocciaSharedArrayBufferValue(AThisValue);
  Result := TGocciaNumberLiteralValue.Create(Buf.GetObservableMaxByteLength);
end;

function TGocciaSharedArrayBufferValue.SharedArrayBufferGrowableGetter(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  if not (AThisValue is TGocciaSharedArrayBufferValue) then
    ThrowTypeError(Format(SErrorRequiresSharedArrayBuffer,
      ['SharedArrayBuffer.prototype.growable']),
      SSuggestSharedArrayBufferThisType);
  if TGocciaSharedArrayBufferValue(AThisValue).FMaxByteLength >= 0 then
    Result := TGocciaBooleanLiteralValue.TrueValue
  else
    Result := TGocciaBooleanLiteralValue.FalseValue;
end;

// ES2026 §25.2.5.3 SharedArrayBuffer.prototype.grow(newLength)
function TGocciaSharedArrayBufferValue.SharedArrayBufferGrow(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Buf: TGocciaSharedArrayBufferValue;
  NewByteLength: Integer;
  CurrentByteLength: Integer;
  ReadByteLength: Integer;
begin
  // Steps 1-3: RequireInternalSlot(O, [[ArrayBufferMaxByteLength]]) and
  // IsSharedArrayBuffer(O): only a growable SharedArrayBuffer, before the
  // argument is converted.
  if not (AThisValue is TGocciaSharedArrayBufferValue) then
    ThrowTypeError(Format(SErrorRequiresSharedArrayBuffer,
      ['SharedArrayBuffer.prototype.grow']),
      SSuggestSharedArrayBufferThisType);
  Buf := TGocciaSharedArrayBufferValue(AThisValue);
  if Buf.FMaxByteLength < 0 then
    ThrowTypeError('Cannot grow a fixed-length SharedArrayBuffer',
      SSuggestArrayLengthRange);

  // Step 4: ? ToIndex(newLength)
  if AArgs.Length = 0 then
    NewByteLength := 0
  else
    NewByteLength := ToSharedArrayBufferIndex(AArgs.GetElement(0));

  // Steps 10-12: a compare-and-exchange loop on
  // [[ArrayBufferByteLengthData]], so racing grows are totally ordered and
  // none is lost. The block already holds maxByteLength bytes (AllocateBlock),
  // so a grow allocates nothing and moves nothing (the step 12.g NOTE): the
  // bytes every agent wrote before it stay where they are. Grows also hold
  // GSharedArrayBufferGrowLock, so the bytes a grow zeroes are not yet
  // visible to any agent; script code never writes past the length, and
  // zeroing clears whatever native code wrote there.
  CriticalSectionEnter(GSharedArrayBufferGrowLock);
  try
    // Step 10: read the current length, seq-cst.
    CurrentByteLength := Buf.GetByteLength;
    repeat
      // Step 12.c: growing to the current length does nothing.
      if NewByteLength = CurrentByteLength then
        Break;
      // Step 12.d: a SharedArrayBuffer never shrinks, nor passes its maximum.
      if (NewByteLength < CurrentByteLength) or
         (NewByteLength > Buf.FMaxByteLength) then
        ThrowRangeError(SErrorInvalidSharedArrayBufferLength,
          SSuggestArrayLengthRange);
      FillChar(Buf.FData[CurrentByteLength],
        NewByteLength - CurrentByteLength, 0);
      // Steps 12.h-j: publish the new length unless the length changed
      // first; then retry against the length that was read.
      ReadByteLength := AtomicCompareExchangeInt32(Buf.FByteLength,
        NewByteLength, CurrentByteLength);
      if ReadByteLength = CurrentByteLength then
        Break;
      CurrentByteLength := ReadByteLength;
    until False;
  finally
    CriticalSectionLeave(GSharedArrayBufferGrowLock);
  end;
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

// ES2026 §25.2.5.6 SharedArrayBuffer.prototype.slice(start, end)
function TGocciaSharedArrayBufferValue.SharedArrayBufferSlice(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Buf: TGocciaSharedArrayBufferValue;
  Len, First, Final, NewLen: Integer;
  SpeciesConstructor: TGocciaValue;
  ConstructedValue: TGocciaValue;
  ConstructorArgs: TGocciaArgumentsCollection;
  NewBuf: TGocciaSharedArrayBufferValue;
begin
  if not (AThisValue is TGocciaSharedArrayBufferValue) then
    ThrowTypeError(Format(SErrorRequiresSharedArrayBuffer,
      ['SharedArrayBuffer.prototype.slice']),
      SSuggestSharedArrayBufferThisType);

  Buf := TGocciaSharedArrayBufferValue(AThisValue);
  Len := Buf.GetByteLength;

  if AArgs.Length = 0 then
    First := 0
  else
    First := ToSharedArrayBufferSliceIndex(AArgs.GetElement(0), Len);

  // ES2026 §25.2.5.6 step 15: If end is undefined, let relativeEnd be len
  if (AArgs.Length > 1) and not (AArgs.GetElement(1) is TGocciaUndefinedLiteralValue) then
    Final := ToSharedArrayBufferSliceIndex(AArgs.GetElement(1), Len)
  else
    Final := Len;

  NewLen := Max(Final - First, 0);

  SpeciesConstructor := SharedArrayBufferSpeciesConstructor(Buf);
  if SpeciesConstructor is TGocciaUndefinedLiteralValue then
    NewBuf := TGocciaSharedArrayBufferValue.Create(NewLen)
  else
  begin
    ConstructorArgs := TGocciaArgumentsCollection.Create(
      [TGocciaNumberLiteralValue.Create(NewLen)]);
    try
      ConstructedValue := ConstructValue(SpeciesConstructor, ConstructorArgs,
        SpeciesConstructor);
    finally
      ConstructorArgs.Free;
    end;
    if not (ConstructedValue is TGocciaSharedArrayBufferValue) then
      ThrowTypeError('SharedArrayBuffer species constructor did not return a SharedArrayBuffer',
        SSuggestSharedArrayBufferThisType);
    NewBuf := TGocciaSharedArrayBufferValue(ConstructedValue);
    if NewBuf = Buf then
      ThrowTypeError(SErrorSharedArrayBufferSpeciesReturnedThis, SSuggestSpeciesConstructor);
    if NewBuf.GetByteLength < NewLen then
      ThrowTypeError('SharedArrayBuffer species constructor returned a buffer that is too small',
        SSuggestSharedArrayBufferThisType);
  end;

  // ES2026 §25.2.5.6 step 11: If new.[[ArrayBufferData]] is O.[[ArrayBufferData]], throw TypeError.
  // Zero-length Pascal dynamic arrays may both have nil storage; that does not
  // represent the same ECMAScript data block.
  if (NewLen > 0) and (Pointer(NewBuf.FData) = Pointer(Buf.FData)) then
    ThrowTypeError(SErrorSharedArrayBufferSpeciesReturnedThis, SSuggestSpeciesConstructor);

  if NewLen > 0 then
    Move(Buf.FData[First], NewBuf.FData[0], NewLen);

  Result := NewBuf;
end;

initialization
  GSharedArrayBufferSharedSlot := RegisterRealmOwnedSlot('SharedArrayBuffer.shared');
  CriticalSectionInit(GSharedArrayBufferGrowLock);

finalization
  CriticalSectionDone(GSharedArrayBufferGrowLock);

end.
