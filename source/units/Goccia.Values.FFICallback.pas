unit Goccia.Values.FFICallback;

{$I Goccia.inc}

interface

uses
  SysUtils,

  Goccia.Arguments.Collection,
  Goccia.FFI.ABI,
  Goccia.FFI.CallbackSlots,
  Goccia.FFI.Types,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  PGocciaFFICallContext = ^TGocciaFFICallContext;
  TGocciaFFICallContext = record
    Previous: PGocciaFFICallContext;
    PendingException: Exception;
    Failed: Boolean;
  end;

  TGocciaFFICallbackValue = class(TGocciaObjectValue)
  private
    FDescriptor: TGocciaFFITypeDescriptor;
    FSignature: TGocciaFFICompiledSignature;
    FCallable: TGocciaValue;
    FCloseFunction: TGocciaValue;
    FSlot: Integer;
    FCodePointer: CodePointer;
    FClosed: Boolean;
    FPinned: Boolean;
    FPendingException: Exception;
    procedure ReleaseNativeSlot;
    function CloseMethod(const AArguments: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    procedure InvokeFromNative(var AState: TGocciaFFICallbackMachineState);
    procedure RaisePendingFailure;
  public
    constructor Create(const ADescriptor: TGocciaFFITypeDescriptor;
      const ACallable: TGocciaValue);
    destructor Destroy; override;
    procedure Close;
    procedure CloseForFFICallCleanup;
    procedure EnsureOpen;
    function GetProperty(const AName: string): TGocciaValue; override;
    function GetPropertyWithContext(const AName: string;
      const AThisContext: TGocciaValue): TGocciaValue; override;
    function ToStringTag: string; override;
    procedure MarkReferences; override;
    property Descriptor: TGocciaFFITypeDescriptor read FDescriptor;
    property CodePointer: CodePointer read FCodePointer;
    property Closed: Boolean read FClosed;
  end;

procedure BeginFFICallContext(var AContext: TGocciaFFICallContext);
procedure CancelFFICallContext(var AContext: TGocciaFFICallContext);
procedure FinishFFICallContext(var AContext: TGocciaFFICallContext);

implementation

uses
  Goccia.Constants.PropertyNames,
  Goccia.Error.Messages,
  Goccia.Error.Suggestions,
  Goccia.FFI.LibraryGuard,
  Goccia.GarbageCollector,
  Goccia.Utils,
  Goccia.Values.Error,
  Goccia.Values.ErrorHelper,
  Goccia.Values.FFIPointer,
  Goccia.Values.FFIType,
  Goccia.Values.NativeFunction;

const
  FFI_CALLBACK_TAG = 'FFICallback';
  PROP_CLOSE = 'close';
  PROP_CLOSED = 'closed';

threadvar
  GCurrentFFICallContext: PGocciaFFICallContext;

procedure BeginFFICallContext(var AContext: TGocciaFFICallContext);
begin
  AContext.Previous := GCurrentFFICallContext;
  AContext.PendingException := nil;
  AContext.Failed := False;
  GCurrentFFICallContext := @AContext;
end;

procedure CancelFFICallContext(var AContext: TGocciaFFICallContext);
var
  PendingException: Exception;
begin
  GCurrentFFICallContext := AContext.Previous;
  PendingException := AContext.PendingException;
  AContext.PendingException := nil;
  PendingException.Free;
end;

procedure FinishFFICallContext(var AContext: TGocciaFFICallContext);
var
  PendingException: Exception;
begin
  GCurrentFFICallContext := AContext.Previous;
  PendingException := AContext.PendingException;
  AContext.PendingException := nil;
  if Assigned(PendingException) then
    raise PendingException;
end;

procedure CopyFromCallbackPlacement(
  const AState: TGocciaFFICallbackMachineState;
  const APlacement: TGocciaFFIPlacement; var ADestination: TBytes;
  const ADestinationOffset: Integer);
begin
  case APlacement.Kind of
    fpkGPR:
      {$IFDEF CPU64}
      Move(AState.GPR[APlacement.RegisterIndex],
        ADestination[ADestinationOffset], APlacement.Size)
      {$ELSE}
      raise EInvalidOpException.Create('i386 callback argument has GPR placement')
      {$ENDIF};
    fpkFPR:
      {$IFDEF CPU64}
      Move(AState.FPR[APlacement.RegisterIndex],
        ADestination[ADestinationOffset], APlacement.Size)
      {$ELSE}
      raise EInvalidOpException.Create('i386 callback argument has FPR placement')
      {$ENDIF};
    fpkStack:
      Move(PByte(AState.StackData)[APlacement.StackOffset],
        ADestination[ADestinationOffset], APlacement.Size);
  end;
end;

procedure CopyToCallbackPlacement(var AState: TGocciaFFICallbackMachineState;
  const APlacement: TGocciaFFIPlacement; const ASource: TBytes;
  const AType: TGocciaFFITypeDescriptor; const AABI: TGocciaFFIABI);
var
  Signed8: ShortInt;
  Signed16: SmallInt;
  Signed32: LongInt;
begin
  case APlacement.Kind of
    fpkGPR:
      begin
        {$IFDEF CPU64}
        if (AABI = fabiDarwinARM64) and
           (AType.Kind = ftkScalar) then
        begin
          case AType.ScalarType of
            fftI8:
            begin
              Move(ASource[APlacement.ValueOffset], Signed8,
                SizeOf(Signed8));
              Signed32 := Signed8;
              AState.RetGPR[APlacement.RegisterIndex] := 0;
              Move(Signed32, AState.RetGPR[APlacement.RegisterIndex],
                SizeOf(Signed32));
              Exit;
            end;
            fftI16:
            begin
              Move(ASource[APlacement.ValueOffset], Signed16,
                SizeOf(Signed16));
              Signed32 := Signed16;
              AState.RetGPR[APlacement.RegisterIndex] := 0;
              Move(Signed32, AState.RetGPR[APlacement.RegisterIndex],
                SizeOf(Signed32));
              Exit;
            end;
          end;
        end;
        {$ENDIF}
        AState.RetGPR[APlacement.RegisterIndex] := 0;
        Move(ASource[APlacement.ValueOffset],
          AState.RetGPR[APlacement.RegisterIndex], APlacement.Size);
      end;
    fpkFPR:
      {$IFDEF CPU64}
      Move(ASource[APlacement.ValueOffset],
        AState.RetFPR[APlacement.RegisterIndex], APlacement.Size)
      {$ELSE}
      begin
        Move(ASource[APlacement.ValueOffset], AState.RetFPR,
          APlacement.Size);
        AState.ReturnFloatSize := APlacement.Size;
      end
      {$ENDIF};
    fpkStack:
      raise EInvalidOpException.Create('FFI callback result has stack placement');
  end;
end;

function UnmarshalCallbackValue(const AType: TGocciaFFITypeDescriptor;
  const AData: TBytes): TGocciaValue;
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
  Unsigned64: QWord;
  Float32: Single;
  Float64: Double;
begin
  if AType.IsAggregate then
  begin
    Aggregate := TGocciaFFIAggregateValue.Create(AType);
    if AType.Size > 0 then Aggregate.CopyFrom(@AData[0]);
    Exit(Aggregate);
  end;
  if AType.Kind = ftkCallback then
  begin
    PointerValue := nil;
    Move(AData[0], PointerValue, SizeOf(Pointer));
    Exit(TGocciaFFIPointerValue.Create(PointerValue));
  end;
  case AType.ScalarType of
    fftVoid: Result := TGocciaUndefinedLiteralValue.UndefinedValue;
    fftBool:
      if AData[0] <> 0 then Result := TGocciaBooleanLiteralValue.TrueValue
      else Result := TGocciaBooleanLiteralValue.FalseValue;
    fftI8: begin Move(AData[0], Signed8, 1); Result := TGocciaNumberLiteralValue.Create(Signed8); end;
    fftU8: begin Move(AData[0], Unsigned8, 1); Result := TGocciaNumberLiteralValue.Create(Unsigned8); end;
    fftI16: begin Move(AData[0], Signed16, 2); Result := TGocciaNumberLiteralValue.Create(Signed16); end;
    fftU16: begin Move(AData[0], Unsigned16, 2); Result := TGocciaNumberLiteralValue.Create(Unsigned16); end;
    fftI32: begin Move(AData[0], Signed32, 4); Result := TGocciaNumberLiteralValue.Create(Signed32); end;
    fftU32: begin Move(AData[0], Unsigned32, 4); Result := TGocciaNumberLiteralValue.Create(Unsigned32); end;
    fftI64: begin Move(AData[0], Signed64, 8); Result := TGocciaNumberLiteralValue.Create(Signed64); end;
    fftU64: begin Move(AData[0], Unsigned64, 8); Result := TGocciaNumberLiteralValue.Create(Unsigned64); end;
    fftF32: begin Move(AData[0], Float32, 4); Result := TGocciaNumberLiteralValue.Create(Float32); end;
    fftF64: begin Move(AData[0], Float64, 8); Result := TGocciaNumberLiteralValue.Create(Float64); end;
    fftPointer:
    begin
      PointerValue := nil;
      Move(AData[0], PointerValue, SizeOf(Pointer));
      Result := TGocciaFFIPointerValue.Create(PointerValue);
    end;
    fftCString:
    begin
      PointerValue := nil;
      Move(AData[0], PointerValue, SizeOf(Pointer));
      if Assigned(PointerValue) then
        Result := TGocciaStringLiteralValue.Create(string(PAnsiChar(PointerValue)))
      else
        Result := TGocciaNullLiteralValue.NullValue;
    end;
  else
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  end;
end;

function MarshalCallbackResult(const AType: TGocciaFFITypeDescriptor;
  const AValue: TGocciaValue): TBytes;
var
  Aggregate: TGocciaFFIAggregateValue;
  BufferData: TBytes;
  PointerValue: Pointer;
  IntegerValue: Int64;
  UnsignedValue: QWord;
  Float32: Single;
  Float64: Double;
begin
  SetLength(Result, AType.Size);
  if Length(Result) > 0 then FillChar(Result[0], Length(Result), 0);
  if AType.IsAggregate then
  begin
    if not (AValue is TGocciaFFIAggregateValue) or
       (TGocciaFFIAggregateValue(AValue).Descriptor <> AType) then
      ThrowTypeError(SErrorFFICallbackAggregateReturn,
        SSuggestFFIUsage);
    Aggregate := TGocciaFFIAggregateValue(AValue);
    Aggregate.EnsureBackingStore;
    BufferData := Aggregate.Buffer.Data;
    Move(BufferData[Aggregate.ByteOffset], Result[0], AType.Size);
    Exit;
  end;
  if AType.Kind = ftkCallback then
  begin
    if not (AValue is TGocciaFFICallbackValue) then
      ThrowTypeError(SErrorFFICallbackHandleReturn,
        SSuggestFFIUsage);
    if TGocciaFFICallbackValue(AValue).Descriptor <> AType then
      ThrowTypeError(SErrorFFICallbackReturnType, SSuggestFFIUsage);
    TGocciaFFICallbackValue(AValue).EnsureOpen;
    PointerValue := TGocciaFFICallbackValue(AValue).CodePointer;
    Move(PointerValue, Result[0], SizeOf(Pointer));
    Exit;
  end;
  case AType.ScalarType of
    fftVoid: Exit;
    fftBool:
      if AValue.ToBooleanLiteral.Value then Result[0] := 1;
    fftI8:
    begin IntegerValue := ToInt32Value(AValue); Move(IntegerValue, Result[0], 1); end;
    fftU8:
    begin UnsignedValue := ToUint32Value(AValue); Move(UnsignedValue, Result[0], 1); end;
    fftI16:
    begin IntegerValue := ToInt32Value(AValue); Move(IntegerValue, Result[0], 2); end;
    fftU16:
    begin UnsignedValue := ToUint32Value(AValue); Move(UnsignedValue, Result[0], 2); end;
    fftI32:
    begin IntegerValue := ToInt32Value(AValue); Move(IntegerValue, Result[0], 4); end;
    fftU32:
    begin UnsignedValue := ToUint32Value(AValue); Move(UnsignedValue, Result[0], 4); end;
    fftI64:
    begin IntegerValue := ToInt64Value(AValue); Move(IntegerValue, Result[0], 8); end;
    fftU64:
    begin UnsignedValue := QWord(ToInt64Value(AValue)); Move(UnsignedValue, Result[0], 8); end;
    fftF32:
    begin Float32 := AValue.ToNumberLiteral.Value; Move(Float32, Result[0], 4); end;
    fftF64:
    begin Float64 := AValue.ToNumberLiteral.Value; Move(Float64, Result[0], 8); end;
    fftPointer:
    begin
      if AValue is TGocciaFFIPointerValue then
        PointerValue := TGocciaFFIPointerValue(AValue).Address
      else if AValue is TGocciaNullLiteralValue then
        PointerValue := nil
      else
        ThrowTypeError(SErrorFFICallbackPointerReturn,
          SSuggestFFIUsage);
      Move(PointerValue, Result[0], SizeOf(Pointer));
    end;
    fftCString:
      ThrowTypeError(SErrorFFICallbackCStringReturn, SSuggestFFIUsage);
  end;
end;

constructor TGocciaFFICallbackValue.Create(
  const ADescriptor: TGocciaFFITypeDescriptor;
  const ACallable: TGocciaValue);
var
  ArgumentTypes: array of TGocciaFFITypeDescriptor;
  HiddenResultLocation: TGocciaFFIHiddenResultLocation;
  HiddenResultSize: PtrUInt;
  ReturnFloatSize: PtrUInt;
  I: Integer;
  CloseFunction: TGocciaNativeFunctionValue;
begin
  if not Assigned(ADescriptor) or (ADescriptor.Kind <> ftkCallback) or
     not Assigned(ACallable) or not ACallable.IsCallable then
    ThrowTypeError(SErrorFFICallbackRequiresCallable,
      SSuggestFFIUsage);
  inherited Create(TGocciaObjectValue.SharedObjectPrototype);
  FSlot := -1;
  FDescriptor := ADescriptor;
  FDescriptor.AddReference;
  FCallable := ACallable;
  SetLength(ArgumentTypes, ADescriptor.CallbackArgumentCount);
  for I := 0 to High(ArgumentTypes) do
    ArgumentTypes[I] := ADescriptor.CallbackArgumentAt(I);
  try
    FSignature := TGocciaFFICompiledSignature.Create(CurrentFFIABI,
      ArgumentTypes, ADescriptor.CallbackReturn);
  except
    on E: EArgumentOutOfRangeException do
      ThrowRangeError(SErrorFFICallLayoutLimit, SSuggestFFIUsage);
    on E: EArgumentException do
      ThrowTypeError(SErrorFFIInvalidCompiledSignature,
        SSuggestFFIUsage);
  end;
  HiddenResultLocation := fhrNone;
  HiddenResultSize := 0;
  ReturnFloatSize := 0;
  if FSignature.ReturnPlan.UsesHiddenPointer then
  begin
    HiddenResultSize := FSignature.ReturnPlan.TypeDescriptor.Size;
    case FSignature.ABI of
      fabiSysVX64, fabiWin64:
        HiddenResultLocation := fhrGPR0;
      fabiAAPCS64, fabiDarwinARM64:
        HiddenResultLocation := fhrDedicated;
      fabiI386Win:
        HiddenResultLocation := fhrStack0;
    end;
  end;
  {$IFNDEF CPU64}
  if (Length(FSignature.ReturnPlan.Placements) > 0) and
     (FSignature.ReturnPlan.Placements[0].Kind = fpkFPR) then
    ReturnFloatSize := FSignature.ReturnPlan.Placements[0].Size;
  {$ENDIF}
  try
    FSlot := AllocateFFICallbackSlot(Self, HiddenResultLocation,
      HiddenResultSize, ReturnFloatSize, FCodePointer);
  except
    on E: EOutOfMemory do
      ThrowRangeError(Format(SErrorFFICallbackSlotLimit,
        [MAX_FFI_CALLBACK_SLOTS]), SSuggestFFIUsage);
  end;
  if Assigned(TGarbageCollector.Instance) then
  begin
    TGarbageCollector.Instance.PinObject(Self);
    FPinned := True;
  end;
  CloseFunction := TGocciaNativeFunctionValue.CreateWithoutPrototype(
    CloseMethod, PROP_CLOSE, 0);
  CloseFunction.CapturedRoot := Self;
  FCloseFunction := CloseFunction;
end;

destructor TGocciaFFICallbackValue.Destroy;
begin
  if FSlot >= 0 then ReleaseFFICallbackSlot(FSlot);
  if FPinned and Assigned(TGarbageCollector.Instance) then
    TGarbageCollector.Instance.UnpinObject(Self);
  if Assigned(FPendingException) then
  begin
    FPendingException.Free;
    FPendingException := nil;
  end;
  FSignature.Free;
  if Assigned(FDescriptor) then FDescriptor.ReleaseReference;
  inherited;
end;

procedure TGocciaFFICallbackValue.ReleaseNativeSlot;
begin
  if not FClosed then
  begin
    FClosed := True;
    if FSlot >= 0 then
    begin
      ReleaseFFICallbackSlot(FSlot);
      FSlot := -1;
    end;
    if FPinned and Assigned(TGarbageCollector.Instance) then
    begin
      TGarbageCollector.Instance.UnpinObject(Self);
      FPinned := False;
    end;
  end;
end;

procedure TGocciaFFICallbackValue.Close;
var
  PendingException: Exception;
  ForeignThreadViolation: Boolean;
begin
  PendingException := FPendingException;
  FPendingException := nil;
  ForeignThreadViolation := (FSlot >= 0) and
    ConsumeFFICallbackThreadViolation(FSlot);
  ReleaseNativeSlot;
  if Assigned(PendingException) then
    raise PendingException;
  if ForeignThreadViolation then
    ThrowTypeError(SErrorFFICallbackForeignThread,
      SSuggestFFIUsage);
end;

procedure TGocciaFFICallbackValue.CloseForFFICallCleanup;
begin
  if FSlot >= 0 then
    ConsumeFFICallbackThreadViolation(FSlot);
  if Assigned(FPendingException) then
  begin
    FPendingException.Free;
    FPendingException := nil;
  end;
  ReleaseNativeSlot;
end;

function TGocciaFFICallbackValue.CloseMethod(
  const AArguments: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  if not (AThisValue is TGocciaFFICallbackValue) then
    ThrowTypeError(SErrorFFICallbackCloseReceiver,
      SSuggestFFIUsage);
  TGocciaFFICallbackValue(AThisValue).Close;
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

procedure TGocciaFFICallbackValue.RaisePendingFailure;
var
  PendingException: Exception;
begin
  PendingException := FPendingException;
  FPendingException := nil;
  if Assigned(PendingException) then raise PendingException;
  if (FSlot >= 0) and ConsumeFFICallbackThreadViolation(FSlot) then
    ThrowTypeError(SErrorFFICallbackForeignThread,
      SSuggestFFIUsage);
end;

procedure TGocciaFFICallbackValue.EnsureOpen;
begin
  RaisePendingFailure;
  if FClosed then
    ThrowTypeError(SErrorFFICallbackClosed, SSuggestFFIUsage);
end;

function CallbackHiddenResultPointer(
  const AABI: TGocciaFFIABI;
  const AState: TGocciaFFICallbackMachineState): Pointer;
begin
  Result := nil;
  case AABI of
    fabiSysVX64, fabiWin64:
      {$IFDEF CPU64}
      Result := Pointer(PtrUInt(AState.GPR[0]))
      {$ENDIF};
    fabiAAPCS64, fabiDarwinARM64:
      {$IFDEF CPU64}
      Result := AState.HiddenResult
      {$ENDIF};
    fabiI386Win:
      if Assigned(AState.StackData) then
        Move(PByte(AState.StackData)[0], Result, SizeOf(Pointer));
  end;
end;

procedure MirrorCallbackHiddenResultPointer(
  const AABI: TGocciaFFIABI; const APointer: Pointer;
  var AState: TGocciaFFICallbackMachineState);
begin
  if AABI in [fabiSysVX64, fabiWin64, fabiI386Win] then
    AState.RetGPR[0] := PtrUInt(APointer);
end;

procedure TGocciaFFICallbackValue.InvokeFromNative(
  var AState: TGocciaFFICallbackMachineState);
var
  ArgumentPlan: TGocciaFFIArgumentPlan;
  ReturnPlan: TGocciaFFIReturnPlan;
  ArgumentData, ResultData: TBytes;
  PointerData: TBytes;
  Arguments: TGocciaArgumentsCollection;
  HiddenResultPointer, ArgumentPointer: Pointer;
  ResultValue: TGocciaValue;
  I, J: Integer;
begin
  Arguments := nil;
  HiddenResultPointer := nil;
  try
    ReturnPlan := FSignature.ReturnPlan;
    {$IFNDEF CPU64}
    if (Length(ReturnPlan.Placements) > 0) and
       (ReturnPlan.Placements[0].Kind = fpkFPR) then
      AState.ReturnFloatSize := ReturnPlan.Placements[0].Size;
    {$ENDIF}
    if ReturnPlan.UsesHiddenPointer then
    begin
      HiddenResultPointer := CallbackHiddenResultPointer(FSignature.ABI,
        AState);
      if not Assigned(HiddenResultPointer) then
        raise EInvalidOpException.Create(
          'FFI callback hidden return has no storage');
      if ReturnPlan.TypeDescriptor.Size > 0 then
        FillChar(HiddenResultPointer^, ReturnPlan.TypeDescriptor.Size, 0);
      MirrorCallbackHiddenResultPointer(FSignature.ABI, HiddenResultPointer,
        AState);
    end;

    if Assigned(GCurrentFFICallContext) and
       GCurrentFFICallContext^.Failed then
      Exit;
    Arguments := TGocciaArgumentsCollection.CreateWithCapacity(
      FSignature.ArgumentCount);
    try
      for I := 0 to FSignature.ArgumentCount - 1 do
      begin
        ArgumentPlan := FSignature.Arguments[I];
        SetLength(ArgumentData, ArgumentPlan.TypeDescriptor.Size);
        if Length(ArgumentData) > 0 then
          FillChar(ArgumentData[0], Length(ArgumentData), 0);
        if ArgumentPlan.Indirect then
        begin
          ArgumentPointer := nil;
          SetLength(PointerData, SizeOf(Pointer));
          CopyFromCallbackPlacement(AState, ArgumentPlan.Placements[0],
            PointerData, 0);
          Move(PointerData[0], ArgumentPointer, SizeOf(Pointer));
          if Assigned(ArgumentPointer) and (Length(ArgumentData) > 0) then
            Move(ArgumentPointer^, ArgumentData[0], Length(ArgumentData));
        end
        else
          for J := 0 to High(ArgumentPlan.Placements) do
            CopyFromCallbackPlacement(AState, ArgumentPlan.Placements[J],
              ArgumentData, ArgumentPlan.Placements[J].ValueOffset);
        Arguments.Add(UnmarshalCallbackValue(ArgumentPlan.TypeDescriptor,
          ArgumentData));
      end;
      ResultValue := InvokeCallable(FCallable, Arguments,
        TGocciaUndefinedLiteralValue.UndefinedValue);
      ResultData := MarshalCallbackResult(ReturnPlan.TypeDescriptor,
        ResultValue);
      if ReturnPlan.UsesHiddenPointer then
      begin
        if (Length(ResultData) > 0) and Assigned(HiddenResultPointer) then
          Move(ResultData[0], HiddenResultPointer^, Length(ResultData));
      end
      else
        for I := 0 to High(ReturnPlan.Placements) do
          CopyToCallbackPlacement(AState, ReturnPlan.Placements[I],
            ResultData, ReturnPlan.TypeDescriptor, FSignature.ABI);
    finally
      Arguments.Free;
      Arguments := nil;
    end;
  except
    on E: Exception do
    begin
      if Assigned(GCurrentFFICallContext) then
      begin
        if not GCurrentFFICallContext^.Failed then
        begin
          GCurrentFFICallContext^.PendingException :=
            Exception(AcquireExceptionObject);
          GCurrentFFICallContext^.Failed := True;
        end
      end
      else if not Assigned(FPendingException) then
        FPendingException := Exception(AcquireExceptionObject);
    end;
  end;
end;

function TGocciaFFICallbackValue.GetProperty(const AName: string): TGocciaValue;
begin
  Result := GetPropertyWithContext(AName, Self);
end;

function TGocciaFFICallbackValue.GetPropertyWithContext(const AName: string;
  const AThisContext: TGocciaValue): TGocciaValue;
begin
  if AName = PROP_CLOSE then
    Result := FCloseFunction
  else if AName = PROP_CLOSED then
  begin
    if FClosed then Result := TGocciaBooleanLiteralValue.TrueValue
    else Result := TGocciaBooleanLiteralValue.FalseValue;
  end
  else if AName = PROP_FFI_ADDRESS then
  begin
    EnsureOpen;
    Result := TGocciaNumberLiteralValue.Create(PtrUInt(FCodePointer));
  end
  else
    Result := inherited GetPropertyWithContext(AName, AThisContext);
end;

function TGocciaFFICallbackValue.ToStringTag: string;
begin
  Result := FFI_CALLBACK_TAG;
end;

procedure TGocciaFFICallbackValue.MarkReferences;
begin
  if GCMarked then Exit;
  inherited;
  if Assigned(FCallable) then FCallable.MarkReferences;
  if Assigned(FCloseFunction) then FCloseFunction.MarkReferences;
  if FPendingException is TGocciaThrowValue then
    TGocciaThrowValue(FPendingException).Value.MarkReferences;
end;

procedure DispatchCallbackHook(const AContext: Pointer;
  var AState: TGocciaFFICallbackMachineState);
begin
  TGocciaFFICallbackValue(AContext).InvokeFromNative(AState);
end;

initialization
  SetFFICallbackDispatchHook(@DispatchCallbackHook);

finalization
  SetFFICallbackDispatchHook(nil);

end.
