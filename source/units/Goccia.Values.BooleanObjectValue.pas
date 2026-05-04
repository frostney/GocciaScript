unit Goccia.Values.BooleanObjectValue;

{$I Goccia.inc}

interface

uses
  Goccia.Arguments.Collection,
  Goccia.ObjectModel,
  Goccia.Values.ClassValue,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  TGocciaBooleanObjectValue = class(TGocciaInstanceValue)
  private
    FPrimitive: TGocciaBooleanLiteralValue;
  public
    constructor Create(const APrimitive: TGocciaBooleanLiteralValue; const AClass: TGocciaClassValue = nil);
    procedure InitializePrototype;
    function GetProperty(const AName: string): TGocciaValue; override;
    function GetPropertyWithContext(const AName: string; const AThisContext: TGocciaValue): TGocciaValue; override;
    procedure MarkReferences; override;

    class function GetSharedPrototype: TGocciaObjectValue;

    property Primitive: TGocciaBooleanLiteralValue read FPrimitive;
  published
    function BooleanValueOf(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function BooleanToString(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
  end;

implementation

uses
  Goccia.GarbageCollector,
  Goccia.Realm,
  Goccia.Values.NativeFunction;

// Boolean.prototype lives in a per-realm slot.  Method host and member
// definitions stay process-wide (immutable across realms).
var
  GBooleanPrototypeSlot: TGocciaRealmSlotId;

threadvar
  FPrototypeMethodHost: TGocciaBooleanObjectValue;
  FPrototypeMembers: TArray<TGocciaMemberDefinition>;

function GetSharedBooleanPrototype: TGocciaObjectValue; inline;
begin
  if Assigned(CurrentRealm) then
    Result := TGocciaObjectValue(CurrentRealm.GetSlot(GBooleanPrototypeSlot))
  else
    Result := nil;
end;

constructor TGocciaBooleanObjectValue.Create(const APrimitive: TGocciaBooleanLiteralValue; const AClass: TGocciaClassValue = nil);
var
  SharedPrototype: TGocciaObjectValue;
begin
  inherited Create(AClass);
  FPrimitive := APrimitive;
  InitializePrototype;
  SharedPrototype := GetSharedBooleanPrototype;
  if not Assigned(AClass) and Assigned(SharedPrototype) then
    FPrototype := SharedPrototype;
end;

procedure TGocciaBooleanObjectValue.InitializePrototype;
var
  Members: TGocciaMemberCollection;
  SharedPrototype: TGocciaObjectValue;
begin
  if not Assigned(CurrentRealm) then Exit;
  if Assigned(GetSharedBooleanPrototype) then Exit;

  SharedPrototype := TGocciaObjectValue.Create;
  CurrentRealm.SetSlot(GBooleanPrototypeSlot, SharedPrototype);
  FPrototypeMethodHost := Self;
  if Length(FPrototypeMembers) = 0 then
  begin
    Members := TGocciaMemberCollection.Create;
    try
      Members.AddMethod(BooleanValueOf, 0);
      Members.AddMethod(BooleanToString, 0);
      FPrototypeMembers := Members.ToDefinitions;
    finally
      Members.Free;
    end;
  end;
  RegisterMemberDefinitions(SharedPrototype, FPrototypeMembers);

  // SharedPrototype pinned via realm slot; method host pinned directly
  // because it's a process-wide singleton.
  if Assigned(TGarbageCollector.Instance) then
    TGarbageCollector.Instance.PinObject(FPrototypeMethodHost);
end;

function TGocciaBooleanObjectValue.GetProperty(const AName: string): TGocciaValue;
begin
  Result := GetPropertyWithContext(AName, Self);
end;

function TGocciaBooleanObjectValue.GetPropertyWithContext(const AName: string; const AThisContext: TGocciaValue): TGocciaValue;
begin
  Result := inherited GetPropertyWithContext(AName, AThisContext);
  if not (Result is TGocciaUndefinedLiteralValue) then
    Exit;

  if Assigned(GetSharedBooleanPrototype) then
    Result := GetSharedBooleanPrototype.GetPropertyWithContext(AName, AThisContext);
end;

class function TGocciaBooleanObjectValue.GetSharedPrototype: TGocciaObjectValue;
begin
  if not Assigned(GetSharedBooleanPrototype) then
    TGocciaBooleanObjectValue.Create(TGocciaBooleanLiteralValue.FalseValue);
  Result := GetSharedBooleanPrototype;
end;

function TGocciaBooleanObjectValue.BooleanValueOf(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  if AThisValue is TGocciaBooleanObjectValue then
    Result := TGocciaBooleanObjectValue(AThisValue).Primitive
  else if AThisValue is TGocciaBooleanLiteralValue then
    Result := AThisValue
  else
    Result := AThisValue.ToBooleanLiteral;
end;

function TGocciaBooleanObjectValue.BooleanToString(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Prim: TGocciaBooleanLiteralValue;
begin
  if AThisValue is TGocciaBooleanObjectValue then
    Prim := TGocciaBooleanObjectValue(AThisValue).Primitive
  else if AThisValue is TGocciaBooleanLiteralValue then
    Prim := TGocciaBooleanLiteralValue(AThisValue)
  else
    Prim := AThisValue.ToBooleanLiteral;

  if Prim.Value then
    Result := TGocciaStringLiteralValue.Create('true')
  else
    Result := TGocciaStringLiteralValue.Create('false');
end;

procedure TGocciaBooleanObjectValue.MarkReferences;
begin
  if GCMarked then Exit;
  inherited;
  if Assigned(FPrimitive) then
    FPrimitive.MarkReferences;
end;

initialization
  GBooleanPrototypeSlot := RegisterRealmSlot('Boolean.prototype');

end.
