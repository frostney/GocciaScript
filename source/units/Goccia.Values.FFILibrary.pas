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

function GetFFILibraryShared: TGocciaSharedPrototype;
{$IFDEF FPC}inline;{$ENDIF}
begin
  if (CurrentRealm <> nil) then
    Result := TGocciaSharedPrototype(CurrentRealm.GetOwnedSlot(GFFILibrarySharedSlot))
  else
    Result := nil;
end;

const
  FFI_LIBRARY_TAG = 'FFILibrary';

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

    function Invoke(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
  public
    constructor Create(const ASymbol: Pointer;
      const ASignature: TGocciaFFICompiledSignature; const AName: string;
      const ALibraryGuard: TGocciaFFILibraryGuard);
    destructor Destroy; override;
  end;

constructor TGocciaFFIBoundFunctionValue.Create(const ASymbol: Pointer;
  const ASignature: TGocciaFFICompiledSignature; const AName: string;
  const ALibraryGuard: TGocciaFFILibraryGuard);
begin
  inherited CreateWithoutPrototype(Invoke, AName, ASignature.ArgumentCount);
  FSymbol := ASymbol;
  FSignature := ASignature;
  FName := AName;
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

function MarshalFFIValue(const AType: TGocciaFFITypeDescriptor;
  const AValue: TGocciaValue; const AArgumentIndex: Integer;
  var ATemporaryString: TBytes;
  out ACallback: TGocciaFFICallbackValue;
  out ATemporaryCallback: Boolean): TBytes;
var
  Aggregate: TGocciaFFIAggregateValue;
  BufferData: TBytes;
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
  ACallback := nil;
  ATemporaryCallback := False;
  SetLength(Result, AType.Size);
  if Length(Result) > 0 then
    FillChar(Result[0], Length(Result), 0);
  if AType.IsAggregate then
  begin
    if not (AValue is TGocciaFFIAggregateValue) then
      ThrowTypeError(SErrorFFIAggregateArgumentValue,
        SSuggestFFIUsage);
    Aggregate := TGocciaFFIAggregateValue(AValue);
    if Aggregate.Descriptor <> AType then
      ThrowTypeError(SErrorFFIAggregateArgumentType,
        SSuggestFFIUsage);
    Aggregate.EnsureBackingStore;
    BufferData := Aggregate.Buffer.Data;
    if AType.Size > 0 then
      Move(BufferData[Aggregate.ByteOffset], Result[0], AType.Size);
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
    Move(PointerValue, Result[0], SizeOf(Pointer));
    Exit;
  end;

  case AType.ScalarType of
    fftVoid:
      ThrowTypeError(SErrorFFIVoidNotValidArg, SSuggestFFIUsage);
    fftBool:
      if AValue.ToBooleanLiteral.Value then Result[0] := 1;
    fftI8:
    begin
      Signed8 := ShortInt(ToInt32Value(AValue));
      Move(Signed8, Result[0], SizeOf(Signed8));
    end;
    fftU8:
    begin
      Unsigned8 := Byte(ToUint32Value(AValue));
      Move(Unsigned8, Result[0], SizeOf(Unsigned8));
    end;
    fftI16:
    begin
      Signed16 := SmallInt(ToInt32Value(AValue));
      Move(Signed16, Result[0], SizeOf(Signed16));
    end;
    fftU16:
    begin
      Unsigned16 := Word(ToUint32Value(AValue));
      Move(Unsigned16, Result[0], SizeOf(Unsigned16));
    end;
    fftI32:
    begin
      Signed32 := ToInt32Value(AValue);
      Move(Signed32, Result[0], SizeOf(Signed32));
    end;
    fftU32:
    begin
      Unsigned32 := ToUint32Value(AValue);
      Move(Unsigned32, Result[0], SizeOf(Unsigned32));
    end;
    fftI64:
    begin
      Signed64 := ToInt64Value(AValue);
      Move(Signed64, Result[0], SizeOf(Signed64));
    end;
    fftU64:
    begin
      Unsigned64 := UInt64(ToInt64Value(AValue));
      Move(Unsigned64, Result[0], SizeOf(Unsigned64));
    end;
    fftF32:
    begin
      Float32 := AValue.ToNumberLiteral.Value;
      Move(Float32, Result[0], SizeOf(Float32));
    end;
    fftF64:
    begin
      Float64 := AValue.ToNumberLiteral.Value;
      Move(Float64, Result[0], SizeOf(Float64));
    end;
    fftPointer:
    begin
      PointerValue := PointerFromFFIValue(AValue, AArgumentIndex);
      Move(PointerValue, Result[0], SizeOf(Pointer));
    end;
    fftUTF8String:
    begin
      if not TryEncodeFFIUTF8String(AValue.ToStringLiteral.Value,
         ATemporaryString) then
        ThrowTypeError(SErrorFFIUTF8StringArgument, SSuggestFFIUsage);
      PointerValue := @ATemporaryString[0];
      Move(PointerValue, Result[0], SizeOf(Pointer));
    end;
  end;
end;

function UnmarshalFFIValue(const AType: TGocciaFFITypeDescriptor;
  const AData: TBytes; const ALibraryGuard: TGocciaFFILibraryGuard): TGocciaValue;
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
  Text: string;
begin
  if AType.IsAggregate then
  begin
    Aggregate := TGocciaFFIAggregateValue.Create(AType);
    if AType.Size > 0 then
    begin
      Aggregate.CopyFrom(@AData[0]);
      Aggregate.AttachLibraryPointerFields(ALibraryGuard);
    end;
    Exit(Aggregate);
  end;
  if AType.Kind = ftkCallback then
  begin
    PointerValue := nil;
    Move(AData[0], PointerValue, SizeOf(Pointer));
    if Assigned(ALibraryGuard) and ALibraryGuard.IsClosed then
      ThrowTypeError(SErrorFFIPointerLibraryClosed, SSuggestFFIUsage);
    Exit(TGocciaFFIPointerValue.Create(PointerValue, ALibraryGuard));
  end;
  case AType.ScalarType of
    fftVoid:
      Result := TGocciaUndefinedLiteralValue.UndefinedValue;
    fftBool:
      if AData[0] <> 0 then
        Result := TGocciaBooleanLiteralValue.TrueValue
      else
        Result := TGocciaBooleanLiteralValue.FalseValue;
    fftI8:
    begin Move(AData[0], Signed8, SizeOf(Signed8)); Result := TGocciaNumberLiteralValue.Create(Signed8); end;
    fftU8:
    begin Move(AData[0], Unsigned8, SizeOf(Unsigned8)); Result := TGocciaNumberLiteralValue.Create(Unsigned8); end;
    fftI16:
    begin Move(AData[0], Signed16, SizeOf(Signed16)); Result := TGocciaNumberLiteralValue.Create(Signed16); end;
    fftU16:
    begin Move(AData[0], Unsigned16, SizeOf(Unsigned16)); Result := TGocciaNumberLiteralValue.Create(Unsigned16); end;
    fftI32:
    begin Move(AData[0], Signed32, SizeOf(Signed32)); Result := TGocciaNumberLiteralValue.Create(Signed32); end;
    fftU32:
    begin Move(AData[0], Unsigned32, SizeOf(Unsigned32)); Result := TGocciaNumberLiteralValue.Create(Unsigned32); end;
    fftI64:
    begin Move(AData[0], Signed64, SizeOf(Signed64)); Result := TGocciaNumberLiteralValue.Create(Signed64); end;
    fftU64:
    begin Move(AData[0], Unsigned64, SizeOf(Unsigned64)); Result := TGocciaNumberLiteralValue.Create(Unsigned64); end;
    fftF32:
    begin Move(AData[0], Float32, SizeOf(Float32)); Result := TGocciaNumberLiteralValue.Create(Float32); end;
    fftF64:
    begin Move(AData[0], Float64, SizeOf(Float64)); Result := TGocciaNumberLiteralValue.Create(Float64); end;
    fftPointer:
    begin
      PointerValue := nil;
      Move(AData[0], PointerValue, SizeOf(Pointer));
      if Assigned(ALibraryGuard) and ALibraryGuard.IsClosed then
        ThrowTypeError(SErrorFFIPointerLibraryClosed, SSuggestFFIUsage);
      Result := TGocciaFFIPointerValue.Create(PointerValue, ALibraryGuard);
    end;
    fftUTF8String:
    begin
      PointerValue := nil;
      Move(AData[0], PointerValue, SizeOf(Pointer));
      if Assigned(PointerValue) then
      begin
        if not TryDecodeFFIUTF8String(PAnsiChar(PointerValue), Text) then
          ThrowTypeError(SErrorFFIUTF8StringResult, SSuggestFFIUsage);
        Result := TGocciaStringLiteralValue.Create(Text);
      end
      else
        Result := TGocciaNullLiteralValue.NullValue;
    end;
  else
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  end;
end;

function TGocciaFFIBoundFunctionValue.Invoke(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  NativeArguments: array of TBytes;
  NativeResult: TBytes;
  TemporaryStrings: array of TBytes;
  Callbacks: array of TGocciaFFICallbackValue;
  TemporaryCallbacks: array of Boolean;
  CallContext: TGocciaFFICallContext;
  CallContextActive: Boolean;
  I: Integer;
begin
  if FLibraryGuard.IsClosed then
    ThrowTypeError(Format(SErrorFFICallLibraryClosed, [FName]),
      SSuggestFFIUsage);
  if AArgs.Length < FSignature.ArgumentCount then
    ThrowTypeError(Format(SErrorFFIFuncArgCount,
      [FName, FSignature.ArgumentCount, AArgs.Length]), SSuggestFFIUsage);

  SetLength(NativeArguments, FSignature.ArgumentCount);
  SetLength(TemporaryStrings, FSignature.ArgumentCount);
  SetLength(Callbacks, FSignature.ArgumentCount);
  SetLength(TemporaryCallbacks, FSignature.ArgumentCount);
  try
    for I := 0 to FSignature.ArgumentCount - 1 do
      NativeArguments[I] := MarshalFFIValue(
        FSignature.Arguments[I].TypeDescriptor, AArgs.GetElement(I), I,
        TemporaryStrings[I], Callbacks[I], TemporaryCallbacks[I]);
    if FLibraryGuard.IsClosed then
      ThrowTypeError(Format(SErrorFFICallLibraryClosed, [FName]),
        SSuggestFFIUsage);

    BeginFFICallContext(CallContext);
    CallContextActive := True;
    try
      try
        FFIInvokeCompiled(FSymbol, FSignature, NativeArguments, NativeResult);
      finally
        try
          FinishFFICallContext(CallContext);
        finally
          CallContextActive := False;
        end;
      end;
      if ConsumeFFICallbackThreadViolationsForCurrentThread then
        ThrowTypeError(SErrorFFICallbackForeignThread,
          SSuggestFFIUsage);
      for I := 0 to High(Callbacks) do
        if Assigned(Callbacks[I]) then Callbacks[I].EnsureOpen;
      Result := UnmarshalFFIValue(FSignature.ReturnPlan.TypeDescriptor,
        NativeResult, FLibraryGuard);
    finally
      if CallContextActive then CancelFFICallContext(CallContext);
    end;
  finally
    for I := 0 to High(Callbacks) do
      if TemporaryCallbacks[I] and Assigned(Callbacks[I]) then
        Callbacks[I].CloseForFFICallCleanup;
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
  const AFuncName: string): TGocciaFFICompiledSignature;
var
  SigObj: TGocciaObjectValue;
  ArgsField, ReturnsField: TGocciaValue;
  ArgsArray: TGocciaArrayValue;
  ArgumentTypes: array of TGocciaFFITypeDescriptor;
  ReturnType: TGocciaFFITypeDescriptor;
  I, J: Integer;
  HasF32, HasOtherArgumentType: Boolean;
begin
  if AArgs.Length < 2 then
    ThrowTypeError(SErrorFFIBindRequiresNameAndSig, SSuggestFFIUsage);

  if not (AArgs.GetElement(1) is TGocciaObjectValue) then
    ThrowTypeError(SErrorFFIBindSigObject, SSuggestFFIUsage);

  SigObj := TGocciaObjectValue(AArgs.GetElement(1));
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
          ArgsArray.Elements[I], False);
      HasF32 := False;
      HasOtherArgumentType := False;
      for I := 0 to High(ArgumentTypes) do
        if (ArgumentTypes[I].Kind = ftkScalar) and
           (ArgumentTypes[I].ScalarType = fftF32) then
          HasF32 := True
        else
          HasOtherArgumentType := True;
      if HasF32 and HasOtherArgumentType then
        ThrowTypeError(SErrorFFIMixedF32Arguments, SSuggestFFIUsage);
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
      ReturnType := ParseFFITypeDescriptorValue(ReturnsField, True);
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
begin
  if not (AThisValue is TGocciaFFILibraryValue) then
    ThrowTypeError(SErrorFFIBindRequiresLibrary, SSuggestFFILibraryOpen);
  Lib := TGocciaFFILibraryValue(AThisValue);

  if Lib.FLibraryGuard.IsClosed then
    ThrowTypeError(SErrorFFIBindLibraryClosed, SSuggestFFILibraryOpen);

  FuncName := AArgs.GetElement(0).ToStringLiteral.Value;
  Sig := ParseSignatureFromArgs(AArgs, FuncName);

  try
    try
      SymbolPtr := Lib.FLibraryGuard.FindSymbol(FuncName);
    except
      on E: Exception do
        ThrowTypeError(E.Message, SSuggestFFIUsage);
    end;
    Result := TGocciaFFIBoundFunctionValue.Create(SymbolPtr, Sig, FuncName,
      Lib.FLibraryGuard);
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
