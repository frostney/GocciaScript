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
  TGocciaSharedArrayBufferValue = class(TGocciaInstanceValue)
  private
    const NO_MAX_BYTE_LENGTH = -1;
  private
    FData: TBytes;
    FMaxByteLength: Integer;
    FHasPendingAllocation: Boolean;
    FPendingByteLength: Double;
    FPendingMaxByteLength: Double;

    function GetByteLength: Integer;

    procedure InitializePrototype;
  public
    constructor Create(const AByteLength: Integer = 0); overload;
    constructor Create(const AByteLength: Integer; const AMaxByteLength: Integer); overload;
    constructor Create(const AClass: TGocciaClassValue); overload;

    function GetProperty(const AName: string): TGocciaValue; override;
    function GetPropertyWithContext(const AName: string; const AThisContext: TGocciaValue): TGocciaValue; override;
    function ToStringTag: string; override;
    function BuiltinTagFallback: Boolean; override;

    procedure InitializeNativeFromArguments(const AArguments: TGocciaArgumentsCollection); override;
    procedure FinalizeNativeFromArguments(const AArguments: TGocciaArgumentsCollection); override;
    procedure MarkReferences; override;

    class procedure ExposePrototype(const AConstructor: TGocciaValue);
    class function GetSharedPrototypeForRealm(
      const ARealm: TGocciaRealm): TGocciaObjectValue; static;

    property Data: TBytes read FData write FData;
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

  Goccia.Constants.ConstructorNames,
  Goccia.Constants.NumericLimits,
  Goccia.Constants.PropertyNames,
  Goccia.Error.Messages,
  Goccia.Error.Suggestions,
  Goccia.Values.ErrorHelper,
  Goccia.Values.FunctionBase,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.SymbolValue;

var
  GSharedArrayBufferSharedSlot: TGocciaRealmOwnedSlotId;

function GetSharedArrayBufferShared: TGocciaSharedPrototype; inline;
begin
  if Assigned(CurrentRealm) then
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

function TGocciaSharedArrayBufferValue.GetByteLength: Integer;
begin
  Result := Length(FData);
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
  SetLength(FData, AByteLength);
  if AByteLength > 0 then
    FillChar(FData[0], AByteLength, 0);
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
  if AMaxByteLength >= 0 then
  begin
    if AByteLength > AMaxByteLength then
      ThrowRangeError(SErrorInvalidSharedArrayBufferLength, SSuggestArrayLengthRange);
    FMaxByteLength := AMaxByteLength;
  end
  else
    FMaxByteLength := NO_MAX_BYTE_LENGTH;
  SetLength(FData, AByteLength);
  if AByteLength > 0 then
    FillChar(FData[0], AByteLength, 0);
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
  SetLength(FData, 0);
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
  if not Assigned(CurrentRealm) then Exit;
  if Assigned(GetSharedArrayBufferShared) then Exit;

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
    FMaxByteLength := Trunc(FPendingMaxByteLength)
  else
    FMaxByteLength := NO_MAX_BYTE_LENGTH;

  SetLength(FData, Len);
  if Len > 0 then
    FillChar(FData[0], Len, 0);
  FHasPendingAllocation := False;
end;

procedure TGocciaSharedArrayBufferValue.MarkReferences;
begin
  if GCMarked then Exit;
  inherited;
end;

function TGocciaSharedArrayBufferValue.GetProperty(const AName: string): TGocciaValue;
begin
  Result := GetPropertyWithContext(AName, Self);
end;

function TGocciaSharedArrayBufferValue.GetPropertyWithContext(const AName: string; const AThisContext: TGocciaValue): TGocciaValue;
begin
  if ((AName = PROP_BYTE_LENGTH) or
      (AName = PROP_MAX_BYTE_LENGTH) or
      (AName = PROP_GROWABLE)) and
     ((AThisContext <> Self) or HasOwnProperty(AName) or
      (Assigned(Prototype) and Prototype.HasProperty(AName))) then
    Exit(inherited GetPropertyWithContext(AName, AThisContext));

  if (AName = PROP_MAX_BYTE_LENGTH) and HasOwnProperty(AName) then
    Exit(inherited GetPropertyWithContext(AName, AThisContext));

  if AName = PROP_BYTE_LENGTH then
    Result := TGocciaNumberLiteralValue.Create(Length(FData))
  else if AName = PROP_MAX_BYTE_LENGTH then
  begin
    if FMaxByteLength >= 0 then
      Result := TGocciaNumberLiteralValue.Create(FMaxByteLength)
    else
      Result := TGocciaNumberLiteralValue.Create(Length(FData));
  end
  else if AName = PROP_GROWABLE then
  begin
    if FMaxByteLength >= 0 then
      Result := TGocciaBooleanLiteralValue.TrueValue
    else
      Result := TGocciaBooleanLiteralValue.FalseValue;
  end
  else
    Result := inherited GetPropertyWithContext(AName, AThisContext);
end;

function TGocciaSharedArrayBufferValue.ToStringTag: string;
begin
  Result := CONSTRUCTOR_SHARED_ARRAY_BUFFER;
end;

function TGocciaSharedArrayBufferValue.BuiltinTagFallback: Boolean;
begin
  Result := True;
end;

// ES2026 §25.2.5.1 get SharedArrayBuffer.prototype.byteLength
function TGocciaSharedArrayBufferValue.SharedArrayBufferByteLengthGetter(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  if not (AThisValue is TGocciaSharedArrayBufferValue) then
    ThrowTypeError(Format(SErrorRequiresSharedArrayBuffer,
      ['SharedArrayBuffer.prototype.byteLength']),
      SSuggestSharedArrayBufferThisType);
  Result := TGocciaNumberLiteralValue.Create(Length(TGocciaSharedArrayBufferValue(AThisValue).FData));
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
  if Buf.FMaxByteLength >= 0 then
    Result := TGocciaNumberLiteralValue.Create(Buf.FMaxByteLength)
  else
    Result := TGocciaNumberLiteralValue.Create(Length(Buf.FData));
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

function TGocciaSharedArrayBufferValue.SharedArrayBufferGrow(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Buf: TGocciaSharedArrayBufferValue;
  CopyLength: Integer;
  NewByteLength: Integer;
  NewData: TBytes;
  OldByteLength: Integer;
begin
  if not (AThisValue is TGocciaSharedArrayBufferValue) then
    ThrowTypeError(Format(SErrorRequiresSharedArrayBuffer,
      ['SharedArrayBuffer.prototype.grow']),
      SSuggestSharedArrayBufferThisType);

  Buf := TGocciaSharedArrayBufferValue(AThisValue);
  if AArgs.Length = 0 then
    NewByteLength := 0
  else
    NewByteLength := ToSharedArrayBufferIndex(AArgs.GetElement(0));

  if Buf.FMaxByteLength < 0 then
    ThrowTypeError('Cannot grow a fixed-length SharedArrayBuffer',
      SSuggestArrayLengthRange);

  OldByteLength := Length(Buf.FData);
  if NewByteLength < OldByteLength then
    ThrowRangeError(SErrorInvalidSharedArrayBufferLength, SSuggestArrayLengthRange);
  if NewByteLength > Buf.FMaxByteLength then
    ThrowRangeError(SErrorInvalidSharedArrayBufferLength, SSuggestArrayLengthRange);

  CopyLength := Min(OldByteLength, NewByteLength);
  SetLength(NewData, NewByteLength);
  if NewByteLength > 0 then
    FillChar(NewData[0], NewByteLength, 0);
  if CopyLength > 0 then
    Move(Buf.FData[0], NewData[0], CopyLength);
  Buf.FData := NewData;
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
  Len := Length(Buf.FData);

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
    if Length(NewBuf.FData) < NewLen then
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

end.
