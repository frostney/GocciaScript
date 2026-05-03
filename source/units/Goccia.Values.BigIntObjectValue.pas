unit Goccia.Values.BigIntObjectValue;

{$I Goccia.inc}

interface

uses
  Goccia.Arguments.Collection,
  Goccia.ObjectModel,
  Goccia.Values.ClassValue,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  TGocciaBigIntObjectValue = class(TGocciaInstanceValue)
  private
    FPrimitive: TGocciaValue;
  public
    constructor Create(const APrimitive: TGocciaValue; const AClass: TGocciaClassValue = nil);
    procedure InitializePrototype;
    function GetProperty(const AName: string): TGocciaValue; override;
    function GetPropertyWithContext(const AName: string; const AThisContext: TGocciaValue): TGocciaValue; override;
    procedure MarkReferences; override;

    class function GetSharedPrototype: TGocciaObjectValue;

    property Primitive: TGocciaValue read FPrimitive;
  published
    function BigIntObjectValueOf(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function BigIntObjectToString(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
  end;

implementation

uses
  SysUtils,

  Goccia.Constants.PropertyNames,
  Goccia.Error.Messages,
  Goccia.Error.Suggestions,
  Goccia.GarbageCollector,
  Goccia.Realm,
  Goccia.Values.BigIntValue,
  Goccia.Values.ErrorHelper,
  Goccia.Values.NativeFunction,
  Goccia.Values.ObjectPropertyDescriptor;

// BigInt.prototype lives in a per-realm slot.  Method host and member
// definitions stay process-wide (immutable across realms).
var
  GBigIntPrototypeSlot: TGocciaRealmSlotId;

threadvar
  FPrototypeMethodHost: TGocciaBigIntObjectValue;
  FPrototypeMembers: TArray<TGocciaMemberDefinition>;

function GetSharedBigIntPrototype: TGocciaObjectValue; inline;
begin
  if Assigned(CurrentRealm) then
    Result := TGocciaObjectValue(CurrentRealm.GetSlot(GBigIntPrototypeSlot))
  else
    Result := nil;
end;

constructor TGocciaBigIntObjectValue.Create(const APrimitive: TGocciaValue; const AClass: TGocciaClassValue = nil);
var
  SharedPrototype: TGocciaObjectValue;
begin
  inherited Create(AClass);
  FPrimitive := APrimitive;
  InitializePrototype;
  SharedPrototype := GetSharedBigIntPrototype;
  if not Assigned(AClass) and Assigned(SharedPrototype) and
     (Self <> FPrototypeMethodHost) then
    FPrototype := SharedPrototype;
end;

procedure TGocciaBigIntObjectValue.InitializePrototype;
var
  Members: TGocciaMemberCollection;
  SharedPrototype: TGocciaObjectValue;
begin
  if not Assigned(CurrentRealm) then Exit;
  if Assigned(GetSharedBigIntPrototype) then Exit;

  SharedPrototype := TGocciaObjectValue.Create;
  CurrentRealm.SetSlot(GBigIntPrototypeSlot, SharedPrototype);
  FPrototypeMethodHost := Self;
  if Length(FPrototypeMembers) = 0 then
  begin
    Members := TGocciaMemberCollection.Create;
    try
      Members.AddMethod(BigIntObjectValueOf, 0);
      Members.AddMethod(BigIntObjectToString, 0);
      FPrototypeMembers := Members.ToDefinitions;
    finally
      Members.Free;
    end;
    FPrototypeMembers[0].ExposedName := PROP_VALUE_OF;
    FPrototypeMembers[1].ExposedName := PROP_TO_STRING;
  end;
  RegisterMemberDefinitions(SharedPrototype, FPrototypeMembers);

  // SharedPrototype pinned via realm slot; method host pinned directly
  // because it's a process-wide singleton.
  if Assigned(TGarbageCollector.Instance) then
    TGarbageCollector.Instance.PinObject(FPrototypeMethodHost);
end;

function TGocciaBigIntObjectValue.GetProperty(const AName: string): TGocciaValue;
begin
  Result := GetPropertyWithContext(AName, Self);
end;

function TGocciaBigIntObjectValue.GetPropertyWithContext(const AName: string; const AThisContext: TGocciaValue): TGocciaValue;
begin
  Result := inherited GetPropertyWithContext(AName, AThisContext);
  if not (Result is TGocciaUndefinedLiteralValue) then
    Exit;

  if Assigned(GetSharedBigIntPrototype) then
    Result := GetSharedBigIntPrototype.GetPropertyWithContext(AName, AThisContext);
end;

class function TGocciaBigIntObjectValue.GetSharedPrototype: TGocciaObjectValue;
begin
  if not Assigned(GetSharedBigIntPrototype) then
    TGocciaBigIntObjectValue.Create(TGocciaBigIntValue.BigIntZero);
  Result := GetSharedBigIntPrototype;
end;

function TGocciaBigIntObjectValue.BigIntObjectValueOf(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  if AThisValue is TGocciaBigIntObjectValue then
    Result := TGocciaBigIntObjectValue(AThisValue).Primitive
  else if AThisValue is TGocciaBigIntValue then
    Result := AThisValue
  else
    ThrowTypeError(Format(SErrorBigIntRequiresBigIntValue, ['BigInt.prototype.valueOf']),
      SSuggestBigIntRequiresBigIntValue);
end;

function TGocciaBigIntObjectValue.BigIntObjectToString(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Prim: TGocciaBigIntValue;
  Radix: Integer;
  RadixValue: TGocciaValue;
begin
  if AThisValue is TGocciaBigIntObjectValue then
    Prim := TGocciaBigIntValue(TGocciaBigIntObjectValue(AThisValue).Primitive)
  else if AThisValue is TGocciaBigIntValue then
    Prim := TGocciaBigIntValue(AThisValue)
  else
  begin
    ThrowTypeError(Format(SErrorBigIntRequiresBigIntValue, ['BigInt.prototype.toString']),
      SSuggestBigIntRequiresBigIntValue);
    Exit;
  end;

  Radix := 10;
  if AArgs.Length > 0 then
  begin
    RadixValue := AArgs.GetElement(0);
    if not (RadixValue is TGocciaUndefinedLiteralValue) then
    begin
      Radix := Trunc(RadixValue.ToNumberLiteral.Value);
      if (Radix < 2) or (Radix > 36) then
        ThrowRangeError(SErrorBigIntInvalidRadix, SSuggestBigIntInvalidRadix);
    end;
  end;

  Result := TGocciaStringLiteralValue.Create(Prim.Value.ToRadixString(Radix));
end;

procedure TGocciaBigIntObjectValue.MarkReferences;
begin
  if GCMarked then Exit;
  inherited;
  if Assigned(FPrimitive) then
    FPrimitive.MarkReferences;
end;

initialization
  // Registered separately from the JS-visible BigInt.prototype (which lives on
  // the primitive class TGocciaBigIntValue).  The wrapper prototype here is
  // used by the rare Object(1n) wrapping path; keeping its slot name distinct
  // makes diagnostic output unambiguous.
  GBigIntPrototypeSlot := RegisterRealmSlot('BigInt.wrapperPrototype');

end.
