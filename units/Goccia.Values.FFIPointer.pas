unit Goccia.Values.FFIPointer;

// Opaque pointer value for borrowed pointers returned by native libraries.
// User-owned memory uses ArrayBuffer + TypedArray views instead.

{$I Goccia.inc}

interface

uses
  Goccia.Arguments.Collection,
  Goccia.Constants.PropertyNames,
  Goccia.ObjectModel,
  Goccia.SharedPrototype,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  TGocciaFFIPointerValue = class(TGocciaObjectValue)
  private
    class var FShared: TGocciaSharedPrototype;
    class var FPrototypeMembers: array of TGocciaMemberDefinition;
    class var FNullPointer: TGocciaFFIPointerValue;
  private
    FAddress: Pointer;

    procedure InitializePrototype;
  published
    function IsNullGetter(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function AddressGetter(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
  public
    constructor Create(const AAddress: Pointer);

    function GetProperty(const AName: string): TGocciaValue; override;
    function ToStringTag: string; override;
    procedure MarkReferences; override;

    class function NullPointer: TGocciaFFIPointerValue;
    class procedure ExposePrototype(const ATarget: TGocciaObjectValue);

    property Address: Pointer read FAddress;
  end;

implementation

uses
  GarbageCollector.Generic,

  Goccia.Values.ErrorHelper,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.SymbolValue;

const
  FFI_POINTER_TAG = 'FFIPointer';

constructor TGocciaFFIPointerValue.Create(const AAddress: Pointer);
begin
  inherited Create;
  FAddress := AAddress;
  InitializePrototype;
  if Assigned(FShared) then
    FPrototype := FShared.Prototype;
end;

procedure TGocciaFFIPointerValue.InitializePrototype;
var
  Members: TGocciaMemberCollection;
begin
  if Assigned(FShared) then Exit;

  FShared := TGocciaSharedPrototype.Create(Self);
  if Length(FPrototypeMembers) = 0 then
  begin
    Members := TGocciaMemberCollection.Create;
    try
      Members.AddAccessor(PROP_IS_NULL, IsNullGetter, nil, [pfConfigurable]);
      Members.AddAccessor(PROP_FFI_ADDRESS, AddressGetter, nil, [pfConfigurable]);
      Members.AddSymbolDataProperty(
        TGocciaSymbolValue.WellKnownToStringTag,
        TGocciaStringLiteralValue.Create(FFI_POINTER_TAG),
        [pfConfigurable]);
      FPrototypeMembers := Members.ToDefinitions;
    finally
      Members.Free;
    end;
  end;
  RegisterMemberDefinitions(FShared.Prototype, FPrototypeMembers);
end;

class function TGocciaFFIPointerValue.NullPointer: TGocciaFFIPointerValue;
begin
  if not Assigned(FNullPointer) then
  begin
    FNullPointer := TGocciaFFIPointerValue.Create(nil);
    TGarbageCollector.Instance.PinObject(FNullPointer);
  end;
  Result := FNullPointer;
end;

class procedure TGocciaFFIPointerValue.ExposePrototype(const ATarget: TGocciaObjectValue);
begin
  if not Assigned(FShared) then
    TGocciaFFIPointerValue.Create(nil);
end;

function TGocciaFFIPointerValue.GetProperty(const AName: string): TGocciaValue;
begin
  if AName = PROP_IS_NULL then
  begin
    if Assigned(FAddress) then
      Result := TGocciaBooleanLiteralValue.FalseValue
    else
      Result := TGocciaBooleanLiteralValue.TrueValue;
  end
  else if AName = PROP_FFI_ADDRESS then
    Result := TGocciaNumberLiteralValue.Create(PtrUInt(FAddress) * 1.0)
  else
    Result := inherited GetProperty(AName);
end;

function TGocciaFFIPointerValue.ToStringTag: string;
begin
  Result := FFI_POINTER_TAG;
end;

procedure TGocciaFFIPointerValue.MarkReferences;
begin
  if GCMarked then Exit;
  inherited;
end;

// -- Accessor getters -------------------------------------------------------

function TGocciaFFIPointerValue.IsNullGetter(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  if not (AThisValue is TGocciaFFIPointerValue) then
    ThrowTypeError('FFIPointer.isNull requires an FFIPointer');
  if Assigned(TGocciaFFIPointerValue(AThisValue).FAddress) then
    Result := TGocciaBooleanLiteralValue.FalseValue
  else
    Result := TGocciaBooleanLiteralValue.TrueValue;
end;

function TGocciaFFIPointerValue.AddressGetter(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  if not (AThisValue is TGocciaFFIPointerValue) then
    ThrowTypeError('FFIPointer.address requires an FFIPointer');
  Result := TGocciaNumberLiteralValue.Create(
    PtrUInt(TGocciaFFIPointerValue(AThisValue).FAddress) * 1.0);
end;

end.
