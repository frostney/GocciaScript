unit Goccia.Values.FFILibrary;

{$I Goccia.inc}

interface

uses
  Goccia.Arguments.Collection,
  Goccia.FFI.LibraryGuard,
  Goccia.ObjectModel,
  Goccia.SharedPrototype,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  TGocciaFFILibraryValue = class(TGocciaObjectValue)
  private
    FLibraryGuard: TGocciaFFILibraryGuard;

    constructor CreatePrototypeHost;
    procedure InitializePrototype;
  published
    function Bind(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function Symbol(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function Close(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function PathGetter(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ClosedGetter(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
  public
    constructor Create(const ALibraryGuard: TGocciaFFILibraryGuard);
    destructor Destroy; override;

    function GetProperty(const AName: string): TGocciaValue; override;
    function GetPropertyWithContext(const AName: string; const AThisContext: TGocciaValue): TGocciaValue; override;
    function ToStringTag: string; override;
    procedure MarkReferences; override;

    class procedure ExposePrototype(const ATarget: TGocciaObjectValue);

    property LibraryGuard: TGocciaFFILibraryGuard read FLibraryGuard;
  end;

implementation

uses
  SysUtils,

  Goccia.Error.Messages,
  Goccia.Error.Suggestions,
  Goccia.FFI.ABI,
  Goccia.FFI.Call,
  Goccia.FFI.CallbackSlots,
  Goccia.FFI.Types,
  Goccia.FFI.UTF8String,
  Goccia.GarbageCollector,
  Goccia.Realm,
  Goccia.Utils,
  Goccia.Values.ArrayBufferValue,
  Goccia.Values.ArrayValue,
  Goccia.Values.ErrorHelper,
  Goccia.Values.FFICallback,
  Goccia.Values.FFIPointer,
  Goccia.Values.FFIType,
  Goccia.Values.NativeFunction,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.SharedArrayBufferValue,
  Goccia.Values.SymbolValue,
  Goccia.Values.TypedArrayValue;

var
  GFFILibrarySharedSlot: TGocciaRealmOwnedSlotId;

function GetFFILibraryShared: TGocciaSharedPrototype; {$IFDEF FPC}inline;{$ENDIF}
begin
  if (CurrentRealm <> nil) then
    Result := TGocciaSharedPrototype(CurrentRealm.GetOwnedSlot(GFFILibrarySharedSlot))
  else
    Result := nil;
end;

const
  FFI_LIBRARY_TAG = 'FFILibrary';

  // A call keeps its marshalled arguments and its native result in the
  // caller's stack frame when they fit in this many bytes.
  FFI_INLINE_ARGUMENT_BYTES = 256;
  FFI_INLINE_RESULT_BYTES = 64;

  PROP_FFI_PATH   = 'path';
  PROP_FFI_CLOSED = 'closed';

// ==========================================================================
// TGocciaFFIBoundFunctionValue — captures a symbol + signature
// ==========================================================================

type
  TGocciaFFIBoundFunctionValue = class(TGocciaNativeFunctionValue)
  private
    FSymbol: Pointer;
    FSignature: TGocciaFFICompiledSignature;
    FName: string;
    FLibraryGuard: TGocciaFFILibraryGuard;
    FVariadic: Boolean;
    // Whether calls take InvokeDirect: a fixed signature whose arguments all
    // marshal without per-call bookkeeping (no strings, no callbacks) and
    // whose arguments and result fit the inline buffers.
    FDirect: Boolean;

    function Invoke(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function InvokeDirect(
      const AArgs: TGocciaArgumentsCollection): TGocciaValue;
    function InvokeGeneral(
      const AArgs: TGocciaArgumentsCollection): TGocciaValue;
    // Failure paths, kept out of the invoke routines so that formatting their
    // messages costs a call that succeeds nothing.
    procedure ThrowLibraryClosed;
    procedure ThrowArgumentCount(const AActualCount: Integer);
  public
    constructor Create(const ASymbol: Pointer;
      const ASignature: TGocciaFFICompiledSignature; const AName: string;
      const ALibraryGuard: TGocciaFFILibraryGuard;
      const AVariadic: Boolean);
    destructor Destroy; override;
  end;

constructor TGocciaFFIBoundFunctionValue.Create(const ASymbol: Pointer;
  const ASignature: TGocciaFFICompiledSignature; const AName: string;
  const ALibraryGuard: TGocciaFFILibraryGuard;
  const AVariadic: Boolean);
begin
  inherited CreateWithoutPrototype(Invoke, AName, ASignature.ArgumentCount);
  FSymbol := ASymbol;
  FSignature := ASignature;
  FName := AName;
  FVariadic := AVariadic;
  FDirect := not AVariadic and
    not ASignature.HasStringArguments and
    not ASignature.HasCallbackArguments and
    (ASignature.ArgumentDataSize <= FFI_INLINE_ARGUMENT_BYTES) and
    (ASignature.ReturnPlanPointer^.TypeDescriptor.Size <=
      FFI_INLINE_RESULT_BYTES);
  ALibraryGuard.RetainDependent;
  FLibraryGuard := ALibraryGuard;
end;

destructor TGocciaFFIBoundFunctionValue.Destroy;
begin
  FSignature.Free;
  if Assigned(FLibraryGuard) then
    FLibraryGuard.ReleaseDependent;
  inherited;
end;

{ The failure helpers in this unit each hold their message in a local: a
  routine with a managed local is never inlined, and inlined back into its
  caller a helper would return the temporary strings, and the exception frame
  guarding them, that it exists to keep off the successful path. }

procedure TGocciaFFIBoundFunctionValue.ThrowLibraryClosed;
var
  Message: string;
begin
  Message := Format(SErrorFFICallLibraryClosed, [FName]);
  ThrowTypeError(Message, SSuggestFFIUsage);
end;

procedure TGocciaFFIBoundFunctionValue.ThrowArgumentCount(
  const AActualCount: Integer);
var
  Message: string;
begin
  Message := Format(SErrorFFIFuncArgCount,
    [FName, FSignature.ArgumentCount, AActualCount]);
  ThrowTypeError(Message, SSuggestFFIUsage);
end;

function PointerFromFFIValue(const AValue: TGocciaValue;
  const AArgumentIndex: Integer): Pointer;
begin
  if AValue is TGocciaFFIPointerValue then
    Result := TGocciaFFIPointerValue(AValue).Address
  else if AValue is TGocciaFFICallbackValue then
  begin
    TGocciaFFICallbackValue(AValue).EnsureOpen;
    Result := Pointer(TGocciaFFICallbackValue(AValue).Pointer);
  end
  else if AValue is TGocciaArrayBufferValue then
  begin
    if Length(TGocciaArrayBufferValue(AValue).Data) = 0 then
      Result := nil
    else
      Result := @TGocciaArrayBufferValue(AValue).Data[0];
  end
  else if AValue is TGocciaSharedArrayBufferValue then
  begin
    if Length(TGocciaSharedArrayBufferValue(AValue).Data) = 0 then
      Result := nil
    else
      Result := @TGocciaSharedArrayBufferValue(AValue).Data[0];
  end
  else if AValue is TGocciaTypedArrayValue then
  begin
    if Length(TGocciaTypedArrayValue(AValue).BufferData) = 0 then
      Result := nil
    else
      Result := @TGocciaTypedArrayValue(AValue).BufferData[
        TGocciaTypedArrayValue(AValue).ByteOffset];
  end
  else if AValue is TGocciaFFIAggregateValue then
    Result := TGocciaFFIAggregateValue(AValue).DataPointer
  else if AValue is TGocciaNullLiteralValue then
    Result := nil
  else
    ThrowTypeError(Format(SErrorFFIArgMustBeBufferOrNull,
      [AArgumentIndex]), SSuggestFFIUsage);
end;

{ Marshalling runs once per argument of every native call, so its failure
  paths live in these helpers: a message read where it is raised would leave
  the caller holding temporary strings, and with them an exception frame, on
  every call that does not fail. }

procedure ThrowFFIAggregateArgumentValue;
var
  Message: string;
begin
  Message := SErrorFFIAggregateArgumentValue;
  ThrowTypeError(Message, SSuggestFFIUsage);
end;

procedure ThrowFFIAggregateArgumentType;
var
  Message: string;
begin
  Message := SErrorFFIAggregateArgumentType;
  ThrowTypeError(Message, SSuggestFFIUsage);
end;

procedure ThrowFFIVoidNotValidArgument;
var
  Message: string;
begin
  Message := SErrorFFIVoidNotValidArg;
  ThrowTypeError(Message, SSuggestFFIUsage);
end;

{ Writes AValue's native representation, AType.Size bytes, at ADestination,
  which the caller has zeroed, for the argument types that need nothing kept
  for the duration of the call: aggregates, numbers, booleans and pointers.
  The rest (strings and callbacks) go through MarshalFFIValue. }
procedure MarshalFFIPlainValue(const AType: TGocciaFFITypeDescriptor;
  const AValue: TGocciaValue; const AArgumentIndex: Integer;
  const ADestination: PByte);
var
  Aggregate: TGocciaFFIAggregateValue;
  PointerValue: Pointer;
  Signed8: ShortInt;
  Unsigned8: Byte;
  Signed16: SmallInt;
  Unsigned16: Word;
  Signed32: LongInt;
  Unsigned32: LongWord;
  Signed64: Int64;
  Unsigned64: UInt64;
  Float32: Single;
  Float64: Double;
begin
  if AType.IsAggregate then
  begin
    if not (AValue is TGocciaFFIAggregateValue) then
      ThrowFFIAggregateArgumentValue;
    Aggregate := TGocciaFFIAggregateValue(AValue);
    if Aggregate.Descriptor <> AType then
      ThrowFFIAggregateArgumentType;
    Aggregate.CopyTo(ADestination);
    Exit;
  end;

  case AType.ScalarType of
    fftVoid:
      ThrowFFIVoidNotValidArgument;
    fftBool:
      if AValue.ToBooleanLiteral.Value then ADestination^ := 1;
    fftI8:
    begin
      Signed8 := ShortInt(ToInt32Value(AValue));
      Move(Signed8, ADestination^, SizeOf(Signed8));
    end;
    fftU8:
    begin
      Unsigned8 := Byte(ToUint32Value(AValue));
      Move(Unsigned8, ADestination^, SizeOf(Unsigned8));
    end;
    fftI16:
    begin
      Signed16 := SmallInt(ToInt32Value(AValue));
      Move(Signed16, ADestination^, SizeOf(Signed16));
    end;
    fftU16:
    begin
      Unsigned16 := Word(ToUint32Value(AValue));
      Move(Unsigned16, ADestination^, SizeOf(Unsigned16));
    end;
    fftI32:
    begin
      Signed32 := ToInt32Value(AValue);
      Move(Signed32, ADestination^, SizeOf(Signed32));
    end;
    fftU32:
    begin
      Unsigned32 := ToUint32Value(AValue);
      Move(Unsigned32, ADestination^, SizeOf(Unsigned32));
    end;
    fftI64:
    begin
      Signed64 := ToInt64Value(AValue);
      Move(Signed64, ADestination^, SizeOf(Signed64));
    end;
    fftU64:
    begin
      Unsigned64 := UInt64(ToInt64Value(AValue));
      Move(Unsigned64, ADestination^, SizeOf(Unsigned64));
    end;
    fftF32:
    begin
      Float32 := AValue.ToNumberLiteral.Value;
      Move(Float32, ADestination^, SizeOf(Float32));
    end;
    fftF64:
    begin
      Float64 := AValue.ToNumberLiteral.Value;
      Move(Float64, ADestination^, SizeOf(Float64));
    end;
    fftPointer:
    begin
      PointerValue := PointerFromFFIValue(AValue, AArgumentIndex);
      Move(PointerValue, ADestination^, SizeOf(Pointer));
    end;
  end;
end;

{ Whether an argument of AType marshals through MarshalFFIPlainValue. }
function IsPlainFFIArgumentType(
  const AType: TGocciaFFITypeDescriptor): Boolean;
begin
  Result := AType.IsAggregate or
    ((AType.Kind = ftkScalar) and (AType.ScalarType <> fftUTF8String));
end;

{ Marshals any argument. ATemporaryString receives the encoded bytes of a
  string argument, which must outlive the call; ACallback and
  ATemporaryCallback report the handle a callback-typed argument resolved to
  and whether this call created it. }
procedure MarshalFFIValue(const AType: TGocciaFFITypeDescriptor;
  const AValue: TGocciaValue; const AArgumentIndex: Integer;
  const ADestination: PByte; var ATemporaryString: TBytes;
  out ACallback: TGocciaFFICallbackValue;
  out ATemporaryCallback: Boolean);
var
  PointerValue: Pointer;
begin
  ACallback := nil;
  ATemporaryCallback := False;
  if IsPlainFFIArgumentType(AType) then
  begin
    MarshalFFIPlainValue(AType, AValue, AArgumentIndex, ADestination);
    Exit;
  end;
  if AType.Kind = ftkNullable then
  begin
    if AValue is TGocciaNullLiteralValue then
      PointerValue := nil
    else
    begin
      if not (AValue is TGocciaStringLiteralValue) or
         not TryEncodeFFIUTF8String(
           TGocciaStringLiteralValue(AValue).Value, ATemporaryString) then
        ThrowTypeError(SErrorFFINullableUTF8StringArgument,
          SSuggestFFIUsage);
      PointerValue := @ATemporaryString[0];
    end;
    Move(PointerValue, ADestination^, SizeOf(Pointer));
    Exit;
  end;
  if AType.Kind = ftkCallback then
  begin
    if AValue is TGocciaFFICallbackValue then
      ACallback := TGocciaFFICallbackValue(AValue)
    else if Assigned(AValue) and AValue.IsCallable then
    begin
      ACallback := TGocciaFFICallbackValue.Create(AType, AValue);
      ATemporaryCallback := True;
    end
    else
      ThrowTypeError(SErrorFFICallbackArgumentValue,
        SSuggestFFIUsage);
    if ACallback.Descriptor <> AType then
      ThrowTypeError(SErrorFFICallbackArgumentType,
        SSuggestFFIUsage);
    ACallback.EnsureOpen;
    PointerValue := Pointer(ACallback.Pointer);
    Move(PointerValue, ADestination^, SizeOf(Pointer));
    Exit;
  end;

  // fftUTF8String: the one scalar that is not plain.
  if not TryEncodeFFIUTF8String(AValue.ToStringLiteral.Value,
     ATemporaryString) then
    ThrowTypeError(SErrorFFIUTF8StringArgument, SSuggestFFIUsage);
  PointerValue := @ATemporaryString[0];
  Move(PointerValue, ADestination^, SizeOf(Pointer));
end;

{ The return types whose value is an object the call has to build or guard: an
  aggregate, a pointer, a callback pointer or a string. }
function UnmarshalFFIReferenceValue(const AType: TGocciaFFITypeDescriptor;
  const AData: PByte; const ALibraryGuard: TGocciaFFILibraryGuard): TGocciaValue;
var
  Aggregate: TGocciaFFIAggregateValue;
  PointerValue: Pointer;
  Text: string;
begin
  if AType.IsAggregate then
  begin
    Aggregate := TGocciaFFIAggregateValue.Create(AType);
    if AType.Size > 0 then
    begin
      Aggregate.CopyFrom(AData);
      Aggregate.AttachLibraryPointerFields(ALibraryGuard);
    end;
    Exit(Aggregate);
  end;
  if (AType.Kind = ftkCallback) or (AType.ScalarType = fftPointer) then
  begin
    PointerValue := nil;
    Move(AData^, PointerValue, SizeOf(Pointer));
    if Assigned(ALibraryGuard) and ALibraryGuard.IsClosed then
      ThrowTypeError(SErrorFFIPointerLibraryClosed, SSuggestFFIUsage);
    Exit(TGocciaFFIPointerValue.Create(PointerValue, ALibraryGuard));
  end;
  if AType.ScalarType = fftUTF8String then
  begin
    PointerValue := nil;
    Move(AData^, PointerValue, SizeOf(Pointer));
    if not Assigned(PointerValue) then
      Exit(TGocciaNullLiteralValue.NullValue);
    if not TryDecodeFFIUTF8String(PAnsiChar(PointerValue), Text) then
      ThrowTypeError(SErrorFFIUTF8StringResult, SSuggestFFIUsage);
    Exit(TGocciaStringLiteralValue.Create(Text));
  end;
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

{ Builds the JavaScript value for a native result of AType held at AData. The
  void, boolean and numeric returns are handled here, free of managed
  temporaries; the rest go through UnmarshalFFIReferenceValue. }
function UnmarshalFFIValue(const AType: TGocciaFFITypeDescriptor;
  const AData: PByte; const ALibraryGuard: TGocciaFFILibraryGuard): TGocciaValue;
var
  Signed8: ShortInt;
  Unsigned8: Byte;
  Signed16: SmallInt;
  Unsigned16: Word;
  Signed32: LongInt;
  Unsigned32: LongWord;
  Signed64: Int64;
  Unsigned64: UInt64;
  Float32: Single;
  Float64: Double;
begin
  if AType.Kind <> ftkScalar then
    Exit(UnmarshalFFIReferenceValue(AType, AData, ALibraryGuard));
  case AType.ScalarType of
    fftVoid:
      Result := TGocciaUndefinedLiteralValue.UndefinedValue;
    fftBool:
      if AData^ <> 0 then
        Result := TGocciaBooleanLiteralValue.TrueValue
      else
        Result := TGocciaBooleanLiteralValue.FalseValue;
    fftI8:
    begin Move(AData^, Signed8, SizeOf(Signed8)); Result := TGocciaNumberLiteralValue.Create(Signed8); end;
    fftU8:
    begin Move(AData^, Unsigned8, SizeOf(Unsigned8)); Result := TGocciaNumberLiteralValue.Create(Unsigned8); end;
    fftI16:
    begin Move(AData^, Signed16, SizeOf(Signed16)); Result := TGocciaNumberLiteralValue.Create(Signed16); end;
    fftU16:
    begin Move(AData^, Unsigned16, SizeOf(Unsigned16)); Result := TGocciaNumberLiteralValue.Create(Unsigned16); end;
    fftI32:
    begin Move(AData^, Signed32, SizeOf(Signed32)); Result := TGocciaNumberLiteralValue.Create(Signed32); end;
    fftU32:
    begin Move(AData^, Unsigned32, SizeOf(Unsigned32)); Result := TGocciaNumberLiteralValue.Create(Unsigned32); end;
    fftI64:
    begin Move(AData^, Signed64, SizeOf(Signed64)); Result := TGocciaNumberLiteralValue.Create(Signed64); end;
    fftU64:
    begin Move(AData^, Unsigned64, SizeOf(Unsigned64)); Result := TGocciaNumberLiteralValue.Create(Unsigned64); end;
    fftF32:
    begin Move(AData^, Float32, SizeOf(Float32)); Result := TGocciaNumberLiteralValue.Create(Float32); end;
    fftF64:
    begin Move(AData^, Float64, SizeOf(Float64)); Result := TGocciaNumberLiteralValue.Create(Float64); end;
  else
    Result := UnmarshalFFIReferenceValue(AType, AData, ALibraryGuard);
  end;
end;

function PromoteVariadicType(
  const AType: TGocciaFFITypeDescriptor): TGocciaFFITypeDescriptor;
begin
  if AType.Kind = ftkScalar then
    case AType.ScalarType of
      fftBool, fftI8, fftI16, fftU8, fftU16:
        Exit(TGocciaFFITypeDescriptor.CreateScalar(fftI32));
      fftF32:
        Exit(TGocciaFFITypeDescriptor.CreateScalar(fftF64));
    end;
  Result := AType;
  Result.AddReference;
end;

procedure ThrowFFICallbackForeignThread;
var
  Message: string;
begin
  Message := SErrorFFICallbackForeignThread;
  ThrowTypeError(Message, SSuggestFFIUsage);
end;

function TGocciaFFIBoundFunctionValue.Invoke(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  if FDirect then
    Result := InvokeDirect(AArgs)
  else
    Result := InvokeGeneral(AArgs);
end;

{ The common call: see FDirect. Everything it needs lives in this frame, so it
  runs without managed locals and with the one exception frame the call
  context requires. InvokeGeneral is the same protocol with the bookkeeping
  the other signatures need. }
function TGocciaFFIBoundFunctionValue.InvokeDirect(
  const AArgs: TGocciaArgumentsCollection): TGocciaValue;
var
  ArgumentData: array[0..FFI_INLINE_ARGUMENT_BYTES - 1] of Byte;
  ResultStorage: array[0..FFI_INLINE_RESULT_BYTES + FFI_BUFFER_ALIGNMENT - 1] of Byte;
  ResultData: PByte;
  CallContext: TGocciaFFICallContext;
  I: Integer;
begin
  if FLibraryGuard.IsClosed then
    ThrowLibraryClosed;
  if AArgs.Length < FSignature.ArgumentCount then
    ThrowArgumentCount(AArgs.Length);

  if FSignature.ArgumentDataSize > 0 then
    FillChar(ArgumentData[0], FSignature.ArgumentDataSize, 0);
  for I := 0 to FSignature.ArgumentCount - 1 do
    MarshalFFIPlainValue(FSignature.ArgumentTypeAt(I), AArgs.GetElement(I), I,
      @ArgumentData[FSignature.ArgumentDataOffset(I)]);
  // Marshalling can run JavaScript (a coercion), which can close the library.
  if FLibraryGuard.IsClosed then
    ThrowLibraryClosed;

  ResultData := FFIAlignBuffer(@ResultStorage[0]);
  BeginFFICallContext(CallContext);
  try
    FFIInvokeCompiled(FSymbol, FSignature, @ArgumentData[0], ResultData);
  finally
    FinishFFICallContext(CallContext);
  end;
  if ConsumeFFICallbackThreadViolationsForCurrentThread then
    ThrowFFICallbackForeignThread;
  Result := UnmarshalFFIValue(FSignature.ReturnPlanPointer^.TypeDescriptor,
    ResultData, FLibraryGuard);
end;

function TGocciaFFIBoundFunctionValue.InvokeGeneral(
  const AArgs: TGocciaArgumentsCollection): TGocciaValue;
var
  // The marshalled arguments, one after another (see ArgumentDataOffset), and
  // the native result.
  ArgumentData, ResultStorage: TBytes;
  TemporaryStrings: array of TBytes;
  Callbacks: array of TGocciaFFICallbackValue;
  TemporaryCallbacks: array of Boolean;
  CombinedTypes: array of TGocciaFFITypeDescriptor;
  Signature: TGocciaFFICompiledSignature;
  CallSignature: TGocciaFFICompiledSignature;
  VarArgs: TGocciaFFIVarArgsValue;
  ArgumentValue: TGocciaValue;
  CallContext: TGocciaFFICallContext;
  CallContextActive: Boolean;
  I, FixedCount, TotalCount: Integer;
begin
  if FLibraryGuard.IsClosed then
    ThrowLibraryClosed;
  FixedCount := FSignature.ArgumentCount;
  if AArgs.Length < FixedCount then
    ThrowArgumentCount(AArgs.Length);

  CallSignature := nil;
  VarArgs := nil;
  Signature := FSignature;
  TotalCount := FixedCount;
  if FVariadic then
  begin
    if (AArgs.Length <> FixedCount + 1) or
       not (AArgs.GetElement(FixedCount) is TGocciaFFIVarArgsValue) then
      ThrowTypeError(Format(SErrorFFIVariadicTailRequired, [FName]),
        SSuggestFFIUsage);
    VarArgs := TGocciaFFIVarArgsValue(AArgs.GetElement(FixedCount));
    TotalCount := FixedCount + VarArgs.Count;
    if TotalCount > MAX_FFI_ARGS then
      ThrowRangeError(Format(SErrorFFIVariadicArgumentLimit,
        [FName, MAX_FFI_ARGS]), SSuggestFFIUsage);
    SetLength(CombinedTypes, TotalCount);
    for I := 0 to FixedCount - 1 do
      CombinedTypes[I] := FSignature.ArgumentTypeAt(I);
    try
      for I := 0 to VarArgs.Count - 1 do
        CombinedTypes[FixedCount + I] :=
          PromoteVariadicType(VarArgs.TypeAt(I));
      try
        CallSignature := TGocciaFFICompiledSignature.Create(CurrentFFIABI,
          CombinedTypes, FSignature.ReturnPlanPointer^.TypeDescriptor,
          FixedCount);
      except
        on E: EArgumentOutOfRangeException do
          ThrowRangeError(SErrorFFICallLayoutLimit, SSuggestFFIUsage);
        on E: EArgumentException do
          ThrowTypeError(SErrorFFIInvalidCompiledSignature,
            SSuggestFFIUsage);
      end;
    finally
      for I := FixedCount to High(CombinedTypes) do
        if Assigned(CombinedTypes[I]) then
          CombinedTypes[I].ReleaseReference;
    end;
    Signature := CallSignature;
  end;

  // SetLength zero-fills. The slack keeps an empty buffer addressable and
  // leaves room to start the result on its alignment boundary.
  SetLength(ArgumentData, Signature.ArgumentDataSize + 1);
  SetLength(ResultStorage,
    Signature.ReturnPlanPointer^.TypeDescriptor.Size + FFI_BUFFER_ALIGNMENT);
  SetLength(TemporaryStrings, TotalCount);
  SetLength(Callbacks, TotalCount);
  SetLength(TemporaryCallbacks, TotalCount);
  CallContextActive := False;
  try
    for I := 0 to TotalCount - 1 do
    begin
      if I < FixedCount then
        ArgumentValue := AArgs.GetElement(I)
      else
        ArgumentValue := VarArgs.ValueAt(I - FixedCount);
      MarshalFFIValue(Signature.ArgumentTypeAt(I), ArgumentValue, I,
        @ArgumentData[Signature.ArgumentDataOffset(I)],
        TemporaryStrings[I], Callbacks[I], TemporaryCallbacks[I]);
    end;
    if FLibraryGuard.IsClosed then
      ThrowLibraryClosed;

    BeginFFICallContext(CallContext);
    CallContextActive := True;
    try
      try
        FFIInvokeCompiled(FSymbol, Signature, @ArgumentData[0],
          FFIAlignBuffer(@ResultStorage[0]));
      finally
        try
          FinishFFICallContext(CallContext);
        finally
          CallContextActive := False;
        end;
      end;
      if ConsumeFFICallbackThreadViolationsForCurrentThread then
        ThrowFFICallbackForeignThread;
      for I := 0 to High(Callbacks) do
        if Assigned(Callbacks[I]) then Callbacks[I].EnsureOpen;
      Result := UnmarshalFFIValue(
        Signature.ReturnPlanPointer^.TypeDescriptor,
        FFIAlignBuffer(@ResultStorage[0]), FLibraryGuard);
    finally
      if CallContextActive then CancelFFICallContext(CallContext);
    end;
  finally
    for I := 0 to High(Callbacks) do
      if TemporaryCallbacks[I] and Assigned(Callbacks[I]) then
        Callbacks[I].CloseForFFICallCleanup;
    CallSignature.Free;
  end;
end;

// ==========================================================================
// TGocciaFFILibraryValue
// ==========================================================================

constructor TGocciaFFILibraryValue.Create(
  const ALibraryGuard: TGocciaFFILibraryGuard);
var
  Shared: TGocciaSharedPrototype;
begin
  inherited Create;
  InitializePrototype;
  Shared := GetFFILibraryShared;
  if Assigned(Shared) then
    FPrototype := Shared.Prototype;
  FLibraryGuard := ALibraryGuard;
end;

constructor TGocciaFFILibraryValue.CreatePrototypeHost;
begin
  inherited Create;
end;

destructor TGocciaFFILibraryValue.Destroy;
begin
  if Assigned(FLibraryGuard) then
    FLibraryGuard.ReleaseOwner;
  inherited;
end;

procedure TGocciaFFILibraryValue.InitializePrototype;
var
  Members: TGocciaMemberCollection;
  MethodHost: TGocciaFFILibraryValue;
  Shared: TGocciaSharedPrototype;
  PrototypeMembers: TArray<TGocciaMemberDefinition>;
begin
  if (CurrentRealm = nil) then Exit;
  if (GetFFILibraryShared <> nil) then Exit;

  MethodHost := TGocciaFFILibraryValue.CreatePrototypeHost;
  Shared := TGocciaSharedPrototype.Create(MethodHost);
  CurrentRealm.SetOwnedSlot(GFFILibrarySharedSlot, Shared);
  Members := TGocciaMemberCollection.Create;
  try
    Members.AddNamedMethod('bind', MethodHost.Bind, 2, gmkPrototypeMethod,
      [gmfNoFunctionPrototype]);
    Members.AddNamedMethod('symbol', MethodHost.Symbol, 1,
      gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddNamedMethod('close', MethodHost.Close, 0,
      gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddAccessor(PROP_FFI_PATH, MethodHost.PathGetter, nil,
      [pfConfigurable]);
    Members.AddAccessor(PROP_FFI_CLOSED, MethodHost.ClosedGetter, nil,
      [pfConfigurable]);
    Members.AddSymbolDataProperty(
      TGocciaSymbolValue.WellKnownToStringTag,
      TGocciaStringLiteralValue.Create(FFI_LIBRARY_TAG),
      [pfConfigurable]);
    PrototypeMembers := Members.ToDefinitions;
  finally
    Members.Free;
  end;
  RegisterMemberDefinitions(Shared.Prototype, PrototypeMembers);
end;

class procedure TGocciaFFILibraryValue.ExposePrototype(const ATarget: TGocciaObjectValue);
begin
  // Prototype is initialized lazily on first Create; nothing to expose on a constructor
end;

function TGocciaFFILibraryValue.GetProperty(const AName: string): TGocciaValue;
begin
  Result := GetPropertyWithContext(AName, Self);
end;

function TGocciaFFILibraryValue.GetPropertyWithContext(const AName: string; const AThisContext: TGocciaValue): TGocciaValue;
begin
  if AName = PROP_FFI_PATH then
    Result := TGocciaStringLiteralValue.Create(FLibraryGuard.Path)
  else if AName = PROP_FFI_CLOSED then
  begin
    if FLibraryGuard.IsClosed then
      Result := TGocciaBooleanLiteralValue.TrueValue
    else
      Result := TGocciaBooleanLiteralValue.FalseValue;
  end
  else
    Result := inherited GetPropertyWithContext(AName, AThisContext);
end;

function TGocciaFFILibraryValue.ToStringTag: string;
begin
  Result := FFI_LIBRARY_TAG;
end;

procedure TGocciaFFILibraryValue.MarkReferences;
begin
  if GCMarked then Exit;
  inherited;
end;

// -- Prototype methods ------------------------------------------------------

function ParseSignatureFromArgs(const AArgs: TGocciaArgumentsCollection;
  const AFuncName: string; out AVariadic: Boolean): TGocciaFFICompiledSignature;
var
  SigObj: TGocciaObjectValue;
  ArgsField, ReturnsField, VariadicField: TGocciaValue;
  ArgsArray: TGocciaArrayValue;
  ArgumentTypes: array of TGocciaFFITypeDescriptor;
  ReturnType: TGocciaFFITypeDescriptor;
  I, J: Integer;
begin
  if AArgs.Length < 2 then
    ThrowTypeError(SErrorFFIBindRequiresNameAndSig, SSuggestFFIUsage);

  if not (AArgs.GetElement(1) is TGocciaObjectValue) then
    ThrowTypeError(SErrorFFIBindSigObject, SSuggestFFIUsage);

  SigObj := TGocciaObjectValue(AArgs.GetElement(1));
  AVariadic := False;
  ReturnType := nil;
  try
    ArgsField := SigObj.GetProperty('args');
    if ArgsField is TGocciaArrayValue then
    begin
      ArgsArray := TGocciaArrayValue(ArgsField);
      if ArgsArray.Elements.Count > MAX_FFI_ARGS then
        ThrowTypeError(Format(SErrorFFIMaxArguments, [MAX_FFI_ARGS]),
          SSuggestFFIUsage);
      SetLength(ArgumentTypes, ArgsArray.Elements.Count);
      for I := 0 to ArgsArray.Elements.Count - 1 do
        ArgumentTypes[I] := ParseFFITypeDescriptorValue(
          ArgsArray.Elements[I], ftpBoundArgument);
    end
    else if (ArgsField = nil) or
            (ArgsField is TGocciaUndefinedLiteralValue) then
      SetLength(ArgumentTypes, 0)
    else
      ThrowTypeError(SErrorFFISigArgsMustBeArray, SSuggestFFIUsage);

    ReturnsField := SigObj.GetProperty('returns');
    if (ReturnsField = nil) or
       (ReturnsField is TGocciaUndefinedLiteralValue) then
      ReturnType := TGocciaFFITypeDescriptor.CreateScalar(fftVoid)
    else
      ReturnType := ParseFFITypeDescriptorValue(ReturnsField,
        ftpBoundReturn);
    VariadicField := SigObj.GetProperty('variadic');
    if Assigned(VariadicField) and
       not (VariadicField is TGocciaUndefinedLiteralValue) then
    begin
      if not (VariadicField is TGocciaBooleanLiteralValue) then
        ThrowTypeError(SErrorFFIVariadicFlagBoolean, SSuggestFFIUsage);
      AVariadic := TGocciaBooleanLiteralValue(VariadicField).Value;
    end;
    try
      Result := TGocciaFFICompiledSignature.Create(CurrentFFIABI,
        ArgumentTypes, ReturnType);
    except
      on E: EArgumentOutOfRangeException do
        ThrowRangeError(SErrorFFICallLayoutLimit, SSuggestFFIUsage);
      on E: EArgumentException do
        ThrowTypeError(SErrorFFIInvalidCompiledSignature,
          SSuggestFFIUsage);
    end;
  finally
    if Assigned(ReturnType) then ReturnType.ReleaseReference;
    for J := 0 to High(ArgumentTypes) do
      if Assigned(ArgumentTypes[J]) then
        ArgumentTypes[J].ReleaseReference;
  end;
end;

function TGocciaFFILibraryValue.Bind(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Lib: TGocciaFFILibraryValue;
  FuncName: string;
  Sig: TGocciaFFICompiledSignature;
  SymbolPtr: Pointer;
  Variadic: Boolean;
begin
  if not (AThisValue is TGocciaFFILibraryValue) then
    ThrowTypeError(SErrorFFIBindRequiresLibrary, SSuggestFFILibraryOpen);
  Lib := TGocciaFFILibraryValue(AThisValue);

  if Lib.FLibraryGuard.IsClosed then
    ThrowTypeError(SErrorFFIBindLibraryClosed, SSuggestFFILibraryOpen);

  FuncName := AArgs.GetElement(0).ToStringLiteral.Value;
  Sig := ParseSignatureFromArgs(AArgs, FuncName, Variadic);

  try
    try
      SymbolPtr := Lib.FLibraryGuard.FindSymbol(FuncName);
    except
      on E: Exception do
        ThrowTypeError(E.Message, SSuggestFFIUsage);
    end;
    Result := TGocciaFFIBoundFunctionValue.Create(SymbolPtr, Sig, FuncName,
      Lib.FLibraryGuard, Variadic);
  except
    Sig.Free;
    raise;
  end;
end;

function TGocciaFFILibraryValue.Symbol(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Lib: TGocciaFFILibraryValue;
  SymName: string;
  SymPtr: Pointer;
begin
  if not (AThisValue is TGocciaFFILibraryValue) then
    ThrowTypeError(SErrorFFISymbolRequiresLibrary, SSuggestFFILibraryOpen);
  Lib := TGocciaFFILibraryValue(AThisValue);

  if Lib.FLibraryGuard.IsClosed then
    ThrowTypeError(SErrorFFISymbolLibraryClosed, SSuggestFFILibraryOpen);

  if AArgs.Length < 1 then
    ThrowTypeError(SErrorFFISymbolRequiresName, SSuggestFFIUsage);

  SymName := AArgs.GetElement(0).ToStringLiteral.Value;
  try
    SymPtr := Lib.FLibraryGuard.FindSymbol(SymName);
  except
    on E: Exception do
      ThrowTypeError(E.Message, SSuggestFFIUsage);
  end;
  Result := TGocciaFFIPointerValue.Create(Pointer(SymPtr),
    Lib.FLibraryGuard);
end;

function TGocciaFFILibraryValue.Close(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Lib: TGocciaFFILibraryValue;
begin
  if not (AThisValue is TGocciaFFILibraryValue) then
    ThrowTypeError(SErrorFFICloseRequiresLibrary, SSuggestFFILibraryOpen);
  Lib := TGocciaFFILibraryValue(AThisValue);
  Lib.FLibraryGuard.Close;
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaFFILibraryValue.PathGetter(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  if not (AThisValue is TGocciaFFILibraryValue) then
    ThrowTypeError(SErrorFFIPathRequiresLibrary, SSuggestFFILibraryOpen);
  Result := TGocciaStringLiteralValue.Create(
    TGocciaFFILibraryValue(AThisValue).FLibraryGuard.Path);
end;

function TGocciaFFILibraryValue.ClosedGetter(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  if not (AThisValue is TGocciaFFILibraryValue) then
    ThrowTypeError(SErrorFFIClosedRequiresLibrary, SSuggestFFILibraryOpen);
  if TGocciaFFILibraryValue(AThisValue).FLibraryGuard.IsClosed then
    Result := TGocciaBooleanLiteralValue.TrueValue
  else
    Result := TGocciaBooleanLiteralValue.FalseValue;
end;

initialization
  GFFILibrarySharedSlot := RegisterRealmOwnedSlot('FFILibrary.shared');

end.
