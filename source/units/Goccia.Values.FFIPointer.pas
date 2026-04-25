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
    FAddress: Pointer;

    procedure InitializePrototype;
  published
    function IsNullGetter(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function AddressGetter(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
  public
    constructor Create(const AAddress: Pointer);

    function GetProperty(const AName: string): TGocciaValue; override;
    function GetPropertyWithContext(const AName: string; const AThisContext: TGocciaValue): TGocciaValue; override;
    function ToStringTag: string; override;
    procedure MarkReferences; override;

    class function NullPointer: TGocciaFFIPointerValue;
    class procedure ExposePrototype(const ATarget: TGocciaObjectValue);

    property Address: Pointer read FAddress;
  end;

implementation

uses
  SysUtils,

  Goccia.Error.Messages,
  Goccia.Error.Suggestions,
  Goccia.GarbageCollector,
  Goccia.Realm,
  Goccia.Values.ErrorHelper,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.SymbolValue;

var
  GFFIPointerSharedSlot: TGocciaRealmOwnedSlotId;
  GFFINullPointerSlot: TGocciaRealmSlotId;

threadvar
  FPrototypeMembers: TArray<TGocciaMemberDefinition>;

function GetFFIPointerShared: TGocciaSharedPrototype; inline;
begin
  if Assigned(CurrentRealm) then
    Result := TGocciaSharedPrototype(CurrentRealm.GetOwnedSlot(GFFIPointerSharedSlot))
  else
    Result := nil;
end;

const
  FFI_POINTER_TAG = 'FFIPointer';

constructor TGocciaFFIPointerValue.Create(const AAddress: Pointer);
var
  Shared: TGocciaSharedPrototype;
begin
  inherited Create;
  FAddress := AAddress;
  InitializePrototype;
  Shared := GetFFIPointerShared;
  if Assigned(Shared) then
    FPrototype := Shared.Prototype;
end;

procedure TGocciaFFIPointerValue.InitializePrototype;
var
  Members: TGocciaMemberCollection;
  Shared: TGocciaSharedPrototype;
begin
  if not Assigned(CurrentRealm) then Exit;
  if Assigned(GetFFIPointerShared) then Exit;

  Shared := TGocciaSharedPrototype.Create(Self);
  CurrentRealm.SetOwnedSlot(GFFIPointerSharedSlot, Shared);
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
  RegisterMemberDefinitions(Shared.Prototype, FPrototypeMembers);
end;

class function TGocciaFFIPointerValue.NullPointer: TGocciaFFIPointerValue;
var
  Cached: TGCManagedObject;
begin
  if not Assigned(CurrentRealm) then
  begin
    Result := nil;
    Exit;
  end;
  Cached := CurrentRealm.GetSlot(GFFINullPointerSlot);
  if Assigned(Cached) then
  begin
    Result := TGocciaFFIPointerValue(Cached);
    Exit;
  end;
  Result := TGocciaFFIPointerValue.Create(nil);
  CurrentRealm.SetSlot(GFFINullPointerSlot, Result);
end;

class procedure TGocciaFFIPointerValue.ExposePrototype(const ATarget: TGocciaObjectValue);
var
  Shared: TGocciaSharedPrototype;
begin
  Shared := GetFFIPointerShared;
  if not Assigned(Shared) then
  begin
    TGocciaFFIPointerValue.Create(nil);
    Shared := GetFFIPointerShared;
  end;
  if Assigned(Shared) then
    ExposeSharedPrototypeOnConstructor(Shared, ATarget);
end;

function TGocciaFFIPointerValue.GetProperty(const AName: string): TGocciaValue;
begin
  Result := GetPropertyWithContext(AName, Self);
end;

function TGocciaFFIPointerValue.GetPropertyWithContext(const AName: string; const AThisContext: TGocciaValue): TGocciaValue;
begin
  if AName = PROP_IS_NULL then
  begin
    if Assigned(FAddress) then
      Result := TGocciaBooleanLiteralValue.FalseValue
    else
      Result := TGocciaBooleanLiteralValue.TrueValue;
  end
  else if AName = PROP_FFI_ADDRESS then
    Result := TGocciaNumberLiteralValue.Create(PtrUInt(FAddress))
  else
    Result := inherited GetPropertyWithContext(AName, AThisContext);
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
    ThrowTypeError(Format(SErrorFFIPointerRequiresFFIPointer, [PROP_IS_NULL]), SSuggestFFIUsage);
  if Assigned(TGocciaFFIPointerValue(AThisValue).FAddress) then
    Result := TGocciaBooleanLiteralValue.FalseValue
  else
    Result := TGocciaBooleanLiteralValue.TrueValue;
end;

function TGocciaFFIPointerValue.AddressGetter(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  if not (AThisValue is TGocciaFFIPointerValue) then
    ThrowTypeError(Format(SErrorFFIPointerRequiresFFIPointer, [PROP_FFI_ADDRESS]), SSuggestFFIUsage);
  Result := TGocciaNumberLiteralValue.Create(
    PtrUInt(TGocciaFFIPointerValue(AThisValue).FAddress));
end;

initialization
  GFFIPointerSharedSlot := RegisterRealmOwnedSlot('FFIPointer.shared');
  GFFINullPointerSlot := RegisterRealmSlot('FFIPointer.nullPointer');

end.
