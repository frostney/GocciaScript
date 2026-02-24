unit Goccia.Values.ArrayBufferValue;

{$I Goccia.inc}

interface

uses
  SysUtils,

  Goccia.Arguments.Collection,
  Goccia.SharedPrototype,
  Goccia.Values.ClassValue,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  TGocciaArrayBufferValue = class(TGocciaInstanceValue)
  private
    class var FShared: TGocciaSharedPrototype;
  private
    FData: TBytes;

    function GetByteLength: Integer;

    function ArrayBufferSlice(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ArrayBufferByteLengthGetter(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;

    procedure InitializePrototype;
  public
    constructor Create(const AByteLength: Integer = 0); overload;
    constructor Create(const AClass: TGocciaClassValue); overload;

    function GetProperty(const AName: string): TGocciaValue; override;
    function ToStringTag: string; override;

    procedure InitializeNativeFromArguments(const AArguments: TGocciaArgumentsCollection); override;
    procedure MarkReferences; override;

    class procedure ExposePrototype(const AConstructor: TGocciaValue);

    property Data: TBytes read FData write FData;
    property ByteLength: Integer read GetByteLength;
  end;

implementation

uses
  Math,

  Goccia.Constants.ConstructorNames,
  Goccia.Constants.PropertyNames,
  Goccia.GarbageCollector,
  Goccia.Values.ErrorHelper,
  Goccia.Values.NativeFunction,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.SymbolValue;

function TGocciaArrayBufferValue.GetByteLength: Integer;
begin
  Result := Length(FData);
end;

constructor TGocciaArrayBufferValue.Create(const AByteLength: Integer);
begin
  inherited Create(nil);
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
  SetLength(FData, 0);
  InitializePrototype;
  if not Assigned(AClass) and Assigned(FShared) then
    FPrototype := FShared.Prototype;
end;

procedure TGocciaArrayBufferValue.InitializePrototype;
begin
  if Assigned(FShared) then Exit;

  FShared := TGocciaSharedPrototype.Create(Self);

  FShared.Prototype.RegisterNativeMethod(
    TGocciaNativeFunctionValue.CreateWithoutPrototype(ArrayBufferSlice, 'slice', 2));

  FShared.Prototype.DefineSymbolProperty(
    TGocciaSymbolValue.WellKnownToStringTag,
    TGocciaPropertyDescriptorData.Create(
      TGocciaStringLiteralValue.Create(CONSTRUCTOR_ARRAY_BUFFER),
      [pfConfigurable]
    )
  );

  FShared.Prototype.DefineProperty(PROP_BYTE_LENGTH,
    TGocciaPropertyDescriptorAccessor.Create(
      TGocciaNativeFunctionValue.CreateWithoutPrototype(ArrayBufferByteLengthGetter, 'get byteLength', 0),
      nil, [pfConfigurable]));
end;

class procedure TGocciaArrayBufferValue.ExposePrototype(const AConstructor: TGocciaValue);
begin
  if not Assigned(FShared) then
    TGocciaArrayBufferValue.Create(0);
  FShared.ExposeOnConstructor(AConstructor);
end;

// ES2026 §25.1.4.1 ArrayBuffer(length [, options])
procedure TGocciaArrayBufferValue.InitializeNativeFromArguments(const AArguments: TGocciaArgumentsCollection);
var
  LengthArg: TGocciaValue;
  Num: TGocciaNumberLiteralValue;
  IntegerIndex: Double;
  Len: Integer;
begin
  // ES2026 §6.2.4.2 ToIndex(value)
  // Step 1: If value is undefined, let integerIndex be 0
  if AArguments.Length = 0 then
  begin
    SetLength(FData, 0);
    Exit;
  end;

  LengthArg := AArguments.GetElement(0);
  if LengthArg is TGocciaUndefinedLiteralValue then
  begin
    SetLength(FData, 0);
    Exit;
  end;

  // Step 2: Let integerIndex be ToIntegerOrInfinity(value) (ES2026 §7.1.5)
  Num := LengthArg.ToNumberLiteral;
  if Num.IsNaN then
    IntegerIndex := 0
  else if Num.IsInfinite then
  begin
    // +/-Infinity is not in [0, 2^53-1]
    ThrowRangeError('Invalid array buffer length');
    Exit;
  end
  else
    IntegerIndex := Trunc(Num.Value);

  // Step 3: If integerIndex is not in [0, 2^53-1], throw RangeError
  if (IntegerIndex < 0) or (IntegerIndex > 9007199254740991) then
    ThrowRangeError('Invalid array buffer length');

  Len := Trunc(IntegerIndex);
  SetLength(FData, Len);
  if Len > 0 then
    FillChar(FData[0], Len, 0);
end;

procedure TGocciaArrayBufferValue.MarkReferences;
begin
  if GCMarked then Exit;
  inherited;
end;

function TGocciaArrayBufferValue.GetProperty(const AName: string): TGocciaValue;
begin
  if AName = PROP_BYTE_LENGTH then
    Result := TGocciaNumberLiteralValue.Create(Length(FData))
  else
    Result := inherited GetProperty(AName);
end;

function TGocciaArrayBufferValue.ToStringTag: string;
begin
  Result := CONSTRUCTOR_ARRAY_BUFFER;
end;

// ES2026 §25.1.6.1 get ArrayBuffer.prototype.byteLength
function TGocciaArrayBufferValue.ArrayBufferByteLengthGetter(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  if not (AThisValue is TGocciaArrayBufferValue) then
    ThrowTypeError('ArrayBuffer.prototype.byteLength requires an ArrayBuffer');
  Result := TGocciaNumberLiteralValue.Create(Length(TGocciaArrayBufferValue(AThisValue).FData));
end;

// ES2026 §25.1.6.3 ArrayBuffer.prototype.slice(start, end)
function TGocciaArrayBufferValue.ArrayBufferSlice(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Buf: TGocciaArrayBufferValue;
  Len, First, Final, NewLen: Integer;
  StartNum, EndNum: TGocciaNumberLiteralValue;
  NewBuf: TGocciaArrayBufferValue;
begin
  if not (AThisValue is TGocciaArrayBufferValue) then
    ThrowTypeError('ArrayBuffer.prototype.slice requires an ArrayBuffer');

  Buf := TGocciaArrayBufferValue(AThisValue);
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

  // ES2026 §25.1.6.3 step 11: If end is undefined, let relativeEnd be len
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
