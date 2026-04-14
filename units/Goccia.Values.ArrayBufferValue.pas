unit Goccia.Values.ArrayBufferValue;

{$I Goccia.inc}

interface

uses
  SysUtils,

  Goccia.Arguments.Collection,
  Goccia.ObjectModel,
  Goccia.SharedPrototype,
  Goccia.Values.ClassValue,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  TGocciaArrayBufferValue = class(TGocciaInstanceValue)
  private
    const NO_MAX_BYTE_LENGTH = -1;
  private
    FData: TBytes;
    FDetached: Boolean;
    FMaxByteLength: Integer;

    function GetByteLength: Integer;

    procedure InitializePrototype;
  public
    constructor Create(const AByteLength: Integer = 0); overload;
    constructor Create(const AByteLength: Integer; const AMaxByteLength: Integer); overload;
    constructor Create(const AClass: TGocciaClassValue); overload;

    function GetProperty(const AName: string): TGocciaValue; override;
    function GetPropertyWithContext(const AName: string; const AThisContext: TGocciaValue): TGocciaValue; override;
    function ToStringTag: string; override;

    procedure InitializeNativeFromArguments(const AArguments: TGocciaArgumentsCollection); override;
    procedure MarkReferences; override;
    procedure Detach;

    class procedure ExposePrototype(const AConstructor: TGocciaValue);

    property Data: TBytes read FData write FData;
    property Detached: Boolean read FDetached;
    property MaxByteLength: Integer read FMaxByteLength;
  published
    property ByteLength: Integer read GetByteLength;
    function ArrayBufferSlice(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ArrayBufferResize(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ArrayBufferTransfer(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ArrayBufferTransferToFixedLength(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ArrayBufferByteLengthGetter(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ArrayBufferMaxByteLengthGetter(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ArrayBufferResizableGetter(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ArrayBufferDetachedGetter(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
  end;

implementation

uses
  Math,

  Goccia.Constants.ConstructorNames,
  Goccia.Constants.PropertyNames,
  Goccia.Values.ErrorHelper,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.SymbolValue;

threadvar
  FShared: TGocciaSharedPrototype;
  FPrototypeMembers: TArray<TGocciaMemberDefinition>;

function RequireArrayBuffer(const AThisValue: TGocciaValue; const AMethodName: string): TGocciaArrayBufferValue;
begin
  if not (AThisValue is TGocciaArrayBufferValue) then
    ThrowTypeError(AMethodName + ' requires an ArrayBuffer');
  Result := TGocciaArrayBufferValue(AThisValue);
end;

// ES2026 §6.2.4.2 ToIndex(value)
function ToIndex(const AValue: TGocciaValue): Integer;
const
  MAX_ECMA_INDEX = 9007199254740991.0;
var
  Num: TGocciaNumberLiteralValue;
  IntegerIndex: Double;
begin
  if (AValue = nil) or (AValue is TGocciaUndefinedLiteralValue) then
  begin
    Result := 0;
    Exit;
  end;

  Num := AValue.ToNumberLiteral;
  if Num.IsNaN then
    IntegerIndex := 0
  else if Num.IsInfinite then
  begin
    ThrowRangeError('Invalid array buffer length');
    Exit(0);
  end
  else
    IntegerIndex := Trunc(Num.Value);

  if (IntegerIndex < 0) or (IntegerIndex > MAX_ECMA_INDEX) or
     (IntegerIndex > High(Integer)) then
    ThrowRangeError('Invalid array buffer length');

  Result := Trunc(IntegerIndex);
end;

function TGocciaArrayBufferValue.GetByteLength: Integer;
begin
  if FDetached then
    Result := 0
  else
    Result := Length(FData);
end;

constructor TGocciaArrayBufferValue.Create(const AByteLength: Integer);
begin
  inherited Create(nil);
  FDetached := False;
  FMaxByteLength := NO_MAX_BYTE_LENGTH;
  SetLength(FData, AByteLength);
  if AByteLength > 0 then
    FillChar(FData[0], AByteLength, 0);
  InitializePrototype;
  if Assigned(FShared) then
    FPrototype := FShared.Prototype;
end;

constructor TGocciaArrayBufferValue.Create(const AByteLength: Integer; const AMaxByteLength: Integer);
begin
  inherited Create(nil);
  FDetached := False;
  if AMaxByteLength >= 0 then
  begin
    if AByteLength > AMaxByteLength then
      ThrowRangeError('ArrayBuffer byte length must not exceed maxByteLength');
    FMaxByteLength := AMaxByteLength;
  end
  else
    FMaxByteLength := NO_MAX_BYTE_LENGTH;
  SetLength(FData, AByteLength);
  if AByteLength > 0 then
    FillChar(FData[0], AByteLength, 0);
  InitializePrototype;
  if Assigned(FShared) then
    FPrototype := FShared.Prototype;
end;

constructor TGocciaArrayBufferValue.Create(const AClass: TGocciaClassValue);
begin
  inherited Create(AClass);
  FDetached := False;
  FMaxByteLength := NO_MAX_BYTE_LENGTH;
  SetLength(FData, 0);
  InitializePrototype;
  if not Assigned(AClass) and Assigned(FShared) then
    FPrototype := FShared.Prototype;
end;

procedure TGocciaArrayBufferValue.InitializePrototype;
var
  Members: TGocciaMemberCollection;
begin
  if Assigned(FShared) then Exit;

  FShared := TGocciaSharedPrototype.Create(Self);
  if Length(FPrototypeMembers) = 0 then
  begin
    Members := TGocciaMemberCollection.Create;
    try
      Members.AddMethod(ArrayBufferSlice, 2, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(ArrayBufferResize, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(ArrayBufferTransfer, 0, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(ArrayBufferTransferToFixedLength, 0, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddAccessor(PROP_BYTE_LENGTH, ArrayBufferByteLengthGetter, nil, [pfConfigurable]);
      Members.AddAccessor(PROP_MAX_BYTE_LENGTH, ArrayBufferMaxByteLengthGetter, nil, [pfConfigurable]);
      Members.AddAccessor(PROP_RESIZABLE, ArrayBufferResizableGetter, nil, [pfConfigurable]);
      Members.AddAccessor(PROP_DETACHED, ArrayBufferDetachedGetter, nil, [pfConfigurable]);
      Members.AddSymbolDataProperty(
        TGocciaSymbolValue.WellKnownToStringTag,
        TGocciaStringLiteralValue.Create(CONSTRUCTOR_ARRAY_BUFFER),
        [pfConfigurable]);
      FPrototypeMembers := Members.ToDefinitions;
    finally
      Members.Free;
    end;
  end;
  RegisterMemberDefinitions(FShared.Prototype, FPrototypeMembers);
end;

class procedure TGocciaArrayBufferValue.ExposePrototype(const AConstructor: TGocciaValue);
begin
  if not Assigned(FShared) then
    TGocciaArrayBufferValue.Create(0);
  ExposeSharedPrototypeOnConstructor(FShared, AConstructor);
end;

// ES2026 §25.1.4.1 ArrayBuffer(length [, options])
procedure TGocciaArrayBufferValue.InitializeNativeFromArguments(const AArguments: TGocciaArgumentsCollection);
var
  Len: Integer;
  OptionsArg, MaxByteLengthValue: TGocciaValue;
  RequestedMaxByteLength: Integer;
begin
  // ES2026 §6.2.4.2 ToIndex(value)
  if AArguments.Length = 0 then
    Len := 0
  else
    Len := ToIndex(AArguments.GetElement(0));

  // ES2026 §25.1.4.1 step 3: GetArrayBufferMaxByteLengthOption(options)
  RequestedMaxByteLength := NO_MAX_BYTE_LENGTH;
  if AArguments.Length > 1 then
  begin
    OptionsArg := AArguments.GetElement(1);
    // ES2026 §25.1.3.7 GetArrayBufferMaxByteLengthOption step 2:
    // If options is not an Object, return empty
    if not (OptionsArg is TGocciaUndefinedLiteralValue) and
       not OptionsArg.IsPrimitive then
    begin
      MaxByteLengthValue := OptionsArg.GetProperty(PROP_MAX_BYTE_LENGTH);
      if Assigned(MaxByteLengthValue) and not (MaxByteLengthValue is TGocciaUndefinedLiteralValue) then
        RequestedMaxByteLength := ToIndex(MaxByteLengthValue);
    end;
  end;

  // ES2026 §25.1.2.1 AllocateArrayBuffer step 4: If maxByteLength is present
  if RequestedMaxByteLength >= 0 then
  begin
    if Len > RequestedMaxByteLength then
      ThrowRangeError('ArrayBuffer byte length must not exceed maxByteLength');
    FMaxByteLength := RequestedMaxByteLength;
  end
  else
    FMaxByteLength := NO_MAX_BYTE_LENGTH;

  SetLength(FData, Len);
  if Len > 0 then
    FillChar(FData[0], Len, 0);
end;

procedure TGocciaArrayBufferValue.Detach;
begin
  FDetached := True;
  SetLength(FData, 0);
end;

procedure TGocciaArrayBufferValue.MarkReferences;
begin
  if GCMarked then Exit;
  inherited;
end;

function TGocciaArrayBufferValue.GetProperty(const AName: string): TGocciaValue;
begin
  Result := GetPropertyWithContext(AName, Self);
end;

function TGocciaArrayBufferValue.GetPropertyWithContext(const AName: string; const AThisContext: TGocciaValue): TGocciaValue;
begin
  if AName = PROP_BYTE_LENGTH then
  begin
    if FDetached then
      Result := TGocciaNumberLiteralValue.ZeroValue
    else
      Result := TGocciaNumberLiteralValue.Create(Length(FData));
  end
  else if AName = PROP_MAX_BYTE_LENGTH then
  begin
    if FDetached then
      Result := TGocciaNumberLiteralValue.ZeroValue
    else if FMaxByteLength >= 0 then
      Result := TGocciaNumberLiteralValue.Create(FMaxByteLength)
    else
      Result := TGocciaNumberLiteralValue.Create(Length(FData));
  end
  else if AName = PROP_RESIZABLE then
  begin
    if FMaxByteLength >= 0 then
      Result := TGocciaBooleanLiteralValue.TrueValue
    else
      Result := TGocciaBooleanLiteralValue.FalseValue;
  end
  else if AName = PROP_DETACHED then
  begin
    if FDetached then
      Result := TGocciaBooleanLiteralValue.TrueValue
    else
      Result := TGocciaBooleanLiteralValue.FalseValue;
  end
  else
    Result := inherited GetPropertyWithContext(AName, AThisContext);
end;

function TGocciaArrayBufferValue.ToStringTag: string;
begin
  Result := CONSTRUCTOR_ARRAY_BUFFER;
end;

// ES2026 §25.1.6.1 get ArrayBuffer.prototype.byteLength
function TGocciaArrayBufferValue.ArrayBufferByteLengthGetter(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Buf: TGocciaArrayBufferValue;
begin
  Buf := RequireArrayBuffer(AThisValue, 'ArrayBuffer.prototype.byteLength');
  // ES2026 §25.1.6.1 step 4: If IsDetachedBuffer(O) is true, return +0
  if Buf.FDetached then
    Result := TGocciaNumberLiteralValue.ZeroValue
  else
    Result := TGocciaNumberLiteralValue.Create(Length(Buf.FData));
end;

// ES2026 §25.1.6.2 get ArrayBuffer.prototype.detached
function TGocciaArrayBufferValue.ArrayBufferDetachedGetter(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Buf: TGocciaArrayBufferValue;
begin
  Buf := RequireArrayBuffer(AThisValue, 'ArrayBuffer.prototype.detached');
  // ES2026 §25.1.6.2 step 4: Return IsDetachedBuffer(O)
  if Buf.FDetached then
    Result := TGocciaBooleanLiteralValue.TrueValue
  else
    Result := TGocciaBooleanLiteralValue.FalseValue;
end;

// ES2026 §25.1.6.3 get ArrayBuffer.prototype.maxByteLength
function TGocciaArrayBufferValue.ArrayBufferMaxByteLengthGetter(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Buf: TGocciaArrayBufferValue;
begin
  Buf := RequireArrayBuffer(AThisValue, 'ArrayBuffer.prototype.maxByteLength');
  // ES2026 §25.1.6.3 step 4: If IsDetachedBuffer(O) is true, return +0
  if Buf.FDetached then
    Result := TGocciaNumberLiteralValue.ZeroValue
  // ES2026 §25.1.6.3 step 5: If IsFixedLengthArrayBuffer(O), return byteLength
  else if Buf.FMaxByteLength < 0 then
    Result := TGocciaNumberLiteralValue.Create(Length(Buf.FData))
  // ES2026 §25.1.6.3 step 6: Return maxByteLength
  else
    Result := TGocciaNumberLiteralValue.Create(Buf.FMaxByteLength);
end;

// ES2026 §25.1.6.4 get ArrayBuffer.prototype.resizable
function TGocciaArrayBufferValue.ArrayBufferResizableGetter(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Buf: TGocciaArrayBufferValue;
begin
  Buf := RequireArrayBuffer(AThisValue, 'ArrayBuffer.prototype.resizable');
  // ES2026 §25.1.6.4 step 4-5: Return !IsFixedLengthArrayBuffer(O)
  if Buf.FMaxByteLength >= 0 then
    Result := TGocciaBooleanLiteralValue.TrueValue
  else
    Result := TGocciaBooleanLiteralValue.FalseValue;
end;

// ES2026 §25.1.6.5 ArrayBuffer.prototype.resize(newLength)
function TGocciaArrayBufferValue.ArrayBufferResize(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Buf: TGocciaArrayBufferValue;
  NewByteLength, OldByteLength, CopyLength: Integer;
  NewData: TBytes;
begin
  Buf := RequireArrayBuffer(AThisValue, 'ArrayBuffer.prototype.resize');

  // ES2026 §25.1.6.5 step 4: If IsDetachedBuffer(O), throw TypeError
  if Buf.FDetached then
    ThrowTypeError('Cannot resize a detached ArrayBuffer');

  // ES2026 §25.1.6.5 step 5: If IsFixedLengthArrayBuffer(O), throw TypeError
  if Buf.FMaxByteLength < 0 then
    ThrowTypeError('Cannot resize a fixed-length ArrayBuffer');

  // ES2026 §25.1.6.5 step 6: Let newByteLength be ToIndex(newLength)
  if AArgs.Length > 0 then
    NewByteLength := ToIndex(AArgs.GetElement(0))
  else
    NewByteLength := 0;

  // ES2026 §25.1.6.5 step 7: If newByteLength > maxByteLength, throw RangeError
  if NewByteLength > Buf.FMaxByteLength then
    ThrowRangeError('ArrayBuffer resize exceeds maxByteLength');

  // ES2026 §25.1.6.5 steps 10-16: Create new data block and copy
  OldByteLength := Length(Buf.FData);
  CopyLength := Min(NewByteLength, OldByteLength);

  SetLength(NewData, NewByteLength);
  if NewByteLength > 0 then
    FillChar(NewData[0], NewByteLength, 0);
  if CopyLength > 0 then
    Move(Buf.FData[0], NewData[0], CopyLength);

  Buf.FData := NewData;
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

// ES2026 §25.1.2.4 ArrayBufferCopyAndDetach(O, newLength, preserveResizability)
function ArrayBufferCopyAndDetach(const ABuf: TGocciaArrayBufferValue;
  const ANewLength: Integer; const APreserveResizability: Boolean): TGocciaArrayBufferValue;
var
  NewMaxByteLength, CopyLength: Integer;
begin
  // ES2026 §25.1.2.4 steps 5-6: Determine new maxByteLength
  // Preserve the original maxByteLength; AllocateArrayBuffer throws RangeError
  // if newByteLength > newMaxByteLength
  if APreserveResizability and (ABuf.FMaxByteLength >= 0) then
    NewMaxByteLength := ABuf.FMaxByteLength
  else
    NewMaxByteLength := ABuf.NO_MAX_BYTE_LENGTH;

  // ES2026 §25.1.2.4 step 8: Allocate new ArrayBuffer
  if NewMaxByteLength >= 0 then
    Result := TGocciaArrayBufferValue.Create(ANewLength, NewMaxByteLength)
  else
    Result := TGocciaArrayBufferValue.Create(ANewLength);

  // ES2026 §25.1.2.4 step 9: copyLength = min(newLength, oldByteLength)
  CopyLength := Min(ANewLength, Length(ABuf.FData));
  // ES2026 §25.1.2.4 step 12: CopyDataBlockBytes
  if CopyLength > 0 then
    Move(ABuf.FData[0], Result.FData[0], CopyLength);

  // ES2026 §25.1.2.4 step 14: DetachArrayBuffer(O)
  ABuf.Detach;
end;

// ES2026 §25.1.6.6 ArrayBuffer.prototype.transfer([newLength])
function TGocciaArrayBufferValue.ArrayBufferTransfer(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Buf: TGocciaArrayBufferValue;
  NewByteLength: Integer;
begin
  Buf := RequireArrayBuffer(AThisValue, 'ArrayBuffer.prototype.transfer');

  // ES2026 §25.1.6.6 step 2: If IsDetachedBuffer(O), throw TypeError
  if Buf.FDetached then
    ThrowTypeError('Cannot transfer a detached ArrayBuffer');

  // ES2026 §25.1.6.6 step 1: newLength defaults to current byteLength
  if (AArgs.Length = 0) or (AArgs.GetElement(0) is TGocciaUndefinedLiteralValue) then
    NewByteLength := Length(Buf.FData)
  else
    NewByteLength := ToIndex(AArgs.GetElement(0));

  // ES2026 §25.1.6.6 step 3: ArrayBufferCopyAndDetach(O, newLength, PRESERVE-RESIZABILITY)
  Result := ArrayBufferCopyAndDetach(Buf, NewByteLength, True);
end;

// ES2026 §25.1.6.7 ArrayBuffer.prototype.transferToFixedLength([newLength])
function TGocciaArrayBufferValue.ArrayBufferTransferToFixedLength(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Buf: TGocciaArrayBufferValue;
  NewByteLength: Integer;
begin
  Buf := RequireArrayBuffer(AThisValue, 'ArrayBuffer.prototype.transferToFixedLength');

  // ES2026 §25.1.6.7 step 2: If IsDetachedBuffer(O), throw TypeError
  if Buf.FDetached then
    ThrowTypeError('Cannot transfer a detached ArrayBuffer');

  // ES2026 §25.1.6.7 step 1: newLength defaults to current byteLength
  if (AArgs.Length = 0) or (AArgs.GetElement(0) is TGocciaUndefinedLiteralValue) then
    NewByteLength := Length(Buf.FData)
  else
    NewByteLength := ToIndex(AArgs.GetElement(0));

  // ES2026 §25.1.6.7 step 3: ArrayBufferCopyAndDetach(O, newLength, FIXED-LENGTH)
  Result := ArrayBufferCopyAndDetach(Buf, NewByteLength, False);
end;

// ES2026 §25.1.6.8 ArrayBuffer.prototype.slice(start, end)
function TGocciaArrayBufferValue.ArrayBufferSlice(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Buf: TGocciaArrayBufferValue;
  Len, First, Final, NewLen: Integer;
  StartNum, EndNum: TGocciaNumberLiteralValue;
  NewBuf: TGocciaArrayBufferValue;
begin
  Buf := RequireArrayBuffer(AThisValue, 'ArrayBuffer.prototype.slice');

  // ES2026 §25.1.6.8 step 4: If IsDetachedBuffer(O), throw TypeError
  if Buf.FDetached then
    ThrowTypeError('Cannot slice a detached ArrayBuffer');

  Len := Length(Buf.FData);

  // Step 7: Let relativeStart be ToIntegerOrInfinity(start)
  if AArgs.Length > 0 then
  begin
    StartNum := AArgs.GetElement(0).ToNumberLiteral;
    if StartNum.IsNaN then
      First := 0
    else
      First := Trunc(StartNum.Value);
  end
  else
    First := 0;

  // Step 8-10: Clamp start
  if First < 0 then
    First := Max(Len + First, 0)
  else
    First := Min(First, Len);

  // ES2026 §25.1.6.8 step 11: If end is undefined, let relativeEnd be len
  if (AArgs.Length > 1) and not (AArgs.GetElement(1) is TGocciaUndefinedLiteralValue) then
  begin
    EndNum := AArgs.GetElement(1).ToNumberLiteral;
    if EndNum.IsNaN then
      Final := 0
    else
      Final := Trunc(EndNum.Value);
  end
  else
    Final := Len;

  // Step 12-14: Clamp end
  if Final < 0 then
    Final := Max(Len + Final, 0)
  else
    Final := Min(Final, Len);

  // Step 15: Let newLen be max(final - first, 0)
  NewLen := Max(Final - First, 0);

  // Step 16-17: Create new ArrayBuffer and copy data
  NewBuf := TGocciaArrayBufferValue.Create(NewLen);
  if NewLen > 0 then
    Move(Buf.FData[First], NewBuf.FData[0], NewLen);

  Result := NewBuf;
end;

end.
