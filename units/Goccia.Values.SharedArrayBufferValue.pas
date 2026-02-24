unit Goccia.Values.SharedArrayBufferValue;

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
  TGocciaSharedArrayBufferValue = class(TGocciaInstanceValue)
  private
    class var FShared: TGocciaSharedPrototype;
  private
    FData: TBytes;

    function GetByteLength: Integer;

    function SharedArrayBufferSlice(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function SharedArrayBufferByteLengthGetter(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;

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

function TGocciaSharedArrayBufferValue.GetByteLength: Integer;
begin
  Result := Length(FData);
end;

constructor TGocciaSharedArrayBufferValue.Create(const AByteLength: Integer);
begin
  inherited Create(nil);
  SetLength(FData, AByteLength);
  if AByteLength > 0 then
    FillChar(FData[0], AByteLength, 0);
  InitializePrototype;
  if Assigned(FShared) then
    FPrototype := FShared.Prototype;
end;

constructor TGocciaSharedArrayBufferValue.Create(const AClass: TGocciaClassValue);
begin
  inherited Create(AClass);
  SetLength(FData, 0);
  InitializePrototype;
  if not Assigned(AClass) and Assigned(FShared) then
    FPrototype := FShared.Prototype;
end;

procedure TGocciaSharedArrayBufferValue.InitializePrototype;
begin
  if Assigned(FShared) then Exit;

  FShared := TGocciaSharedPrototype.Create(Self);

  FShared.Prototype.RegisterNativeMethod(
    TGocciaNativeFunctionValue.CreateWithoutPrototype(SharedArrayBufferSlice, 'slice', 2));

  FShared.Prototype.DefineSymbolProperty(
    TGocciaSymbolValue.WellKnownToStringTag,
    TGocciaPropertyDescriptorData.Create(
      TGocciaStringLiteralValue.Create(CONSTRUCTOR_SHARED_ARRAY_BUFFER),
      [pfConfigurable]
    )
  );

  FShared.Prototype.DefineProperty(PROP_BYTE_LENGTH,
    TGocciaPropertyDescriptorAccessor.Create(
      TGocciaNativeFunctionValue.CreateWithoutPrototype(SharedArrayBufferByteLengthGetter, 'get byteLength', 0),
      nil, [pfConfigurable]));
end;

class procedure TGocciaSharedArrayBufferValue.ExposePrototype(const AConstructor: TGocciaValue);
begin
  if not Assigned(FShared) then
    TGocciaSharedArrayBufferValue.Create(0);
  FShared.ExposeOnConstructor(AConstructor);
end;

// ES2026 §25.2.3.1 SharedArrayBuffer(length [, options])
procedure TGocciaSharedArrayBufferValue.InitializeNativeFromArguments(const AArguments: TGocciaArgumentsCollection);
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
    ThrowRangeError('Invalid shared array buffer length');
    Exit;
  end
  else
    IntegerIndex := Trunc(Num.Value);

  // Step 3: If integerIndex is not in [0, 2^53-1], throw RangeError
  if (IntegerIndex < 0) or (IntegerIndex > 9007199254740991) then
    ThrowRangeError('Invalid shared array buffer length');

  Len := Trunc(IntegerIndex);
  SetLength(FData, Len);
  if Len > 0 then
    FillChar(FData[0], Len, 0);
end;

procedure TGocciaSharedArrayBufferValue.MarkReferences;
begin
  if GCMarked then Exit;
  inherited;
end;

function TGocciaSharedArrayBufferValue.GetProperty(const AName: string): TGocciaValue;
begin
  if AName = PROP_BYTE_LENGTH then
    Result := TGocciaNumberLiteralValue.Create(Length(FData))
  else
    Result := inherited GetProperty(AName);
end;

function TGocciaSharedArrayBufferValue.ToStringTag: string;
begin
  Result := CONSTRUCTOR_SHARED_ARRAY_BUFFER;
end;

// ES2026 §25.2.5.1 get SharedArrayBuffer.prototype.byteLength
function TGocciaSharedArrayBufferValue.SharedArrayBufferByteLengthGetter(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  if not (AThisValue is TGocciaSharedArrayBufferValue) then
    ThrowTypeError('SharedArrayBuffer.prototype.byteLength requires a SharedArrayBuffer');
  Result := TGocciaNumberLiteralValue.Create(Length(TGocciaSharedArrayBufferValue(AThisValue).FData));
end;

// ES2026 §25.2.5.6 SharedArrayBuffer.prototype.slice(start, end)
function TGocciaSharedArrayBufferValue.SharedArrayBufferSlice(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Buf: TGocciaSharedArrayBufferValue;
  Len, First, Final, NewLen: Integer;
  StartNum, EndNum: TGocciaNumberLiteralValue;
  NewBuf: TGocciaSharedArrayBufferValue;
begin
  if not (AThisValue is TGocciaSharedArrayBufferValue) then
    ThrowTypeError('SharedArrayBuffer.prototype.slice requires a SharedArrayBuffer');

  Buf := TGocciaSharedArrayBufferValue(AThisValue);
  Len := Length(Buf.FData);

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

  if First < 0 then
    First := Max(Len + First, 0)
  else
    First := Min(First, Len);

  // ES2026 §25.2.5.6 step 15: If end is undefined, let relativeEnd be len
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

  if Final < 0 then
    Final := Max(Len + Final, 0)
  else
    Final := Min(Final, Len);

  NewLen := Max(Final - First, 0);

  NewBuf := TGocciaSharedArrayBufferValue.Create(NewLen);

  // ES2026 §25.2.5.6 step 11: If new.[[ArrayBufferData]] is O.[[ArrayBufferData]], throw TypeError
  if Pointer(NewBuf.FData) = Pointer(Buf.FData) then
    ThrowTypeError('SharedArrayBuffer subclass returned this from species constructor');

  if NewLen > 0 then
    Move(Buf.FData[First], NewBuf.FData[0], NewLen);

  Result := NewBuf;
end;

end.
