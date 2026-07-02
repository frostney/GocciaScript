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

    function ExtractPrimitive(const AValue: TGocciaValue): TGocciaBooleanLiteralValue;
  public
    constructor Create(const APrimitive: TGocciaBooleanLiteralValue; const AClass: TGocciaClassValue = nil);
    procedure InitializePrototype;
    function ToStringTag: string; override;
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
  Goccia.Realm,
  Goccia.Values.ErrorHelper,
  Goccia.Values.ToObject;

// Boolean.prototype lives in a per-realm slot.  Its method host (Self in
// InitializePrototype) lives in its own per-realm slot too, so the realm pins it
// on SetSlot and unpins it on Destroy rather than keeping a per-thread pin for
// the process lifetime; the member definitions are rebuilt per realm because
// they bind to this realm's host. #892
var
  GBooleanPrototypeSlot: TGocciaRealmSlotId;
  GBooleanMethodHostSlot: TGocciaRealmSlotId;

function GetSharedBooleanPrototype: TGocciaObjectValue; inline;
begin
  if Assigned(CurrentRealm) then
    Result := TGocciaObjectValue(CurrentRealm.GetSlot(GBooleanPrototypeSlot))
  else
    Result := nil;
end;

// ES2026 §20.3.3.3.1 ThisBooleanValue(value)
function TGocciaBooleanObjectValue.ExtractPrimitive(const AValue: TGocciaValue): TGocciaBooleanLiteralValue;
begin
  RequireObjectCoercible(AValue);

  if AValue is TGocciaBooleanLiteralValue then
    Result := TGocciaBooleanLiteralValue(AValue)
  else if AValue is TGocciaBooleanObjectValue then
    Result := TGocciaBooleanObjectValue(AValue).Primitive
  else
    ThrowTypeError('Boolean.prototype method called on non-Boolean');
end;

constructor TGocciaBooleanObjectValue.Create(const APrimitive: TGocciaBooleanLiteralValue; const AClass: TGocciaClassValue = nil);
var
  SharedPrototype: TGocciaObjectValue;
begin
  inherited Create(AClass);
  FPrimitive := APrimitive;
  InitializePrototype;
  SharedPrototype := GetSharedBooleanPrototype;
  if not Assigned(AClass) and Assigned(SharedPrototype) and
     (SharedPrototype <> TGocciaObjectValue(Self)) then
    FPrototype := SharedPrototype;
end;

procedure TGocciaBooleanObjectValue.InitializePrototype;
var
  Members: TGocciaMemberCollection;
  SharedPrototype: TGocciaObjectValue;
  PrototypeMembers: TArray<TGocciaMemberDefinition>;
begin
  if not Assigned(CurrentRealm) then Exit;
  if Assigned(GetSharedBooleanPrototype) then Exit;

  SharedPrototype := Self;
  CurrentRealm.SetSlot(GBooleanPrototypeSlot, SharedPrototype);
  // The native methods below bind to this instance (Self); keep it alive for the
  // realm's lifetime in its own slot so the realm unpins it on Destroy. #892
  CurrentRealm.SetSlot(GBooleanMethodHostSlot, Self);
  Members := TGocciaMemberCollection.Create;
  try
    Members.AddMethod(BooleanValueOf, 0);
    Members.AddMethod(BooleanToString, 0);
    PrototypeMembers := Members.ToDefinitions;
  finally
    Members.Free;
  end;
  RegisterMemberDefinitions(SharedPrototype, PrototypeMembers);
end;

function TGocciaBooleanObjectValue.GetProperty(const AName: string): TGocciaValue;
begin
  Result := GetPropertyWithContext(AName, Self);
end;

function TGocciaBooleanObjectValue.ToStringTag: string;
begin
  Result := 'Boolean';
end;

function TGocciaBooleanObjectValue.GetPropertyWithContext(const AName: string; const AThisContext: TGocciaValue): TGocciaValue;
var
  SharedPrototype: TGocciaObjectValue;
begin
  Result := inherited GetPropertyWithContext(AName, AThisContext);
  if not (Result is TGocciaUndefinedLiteralValue) then
    Exit;

  SharedPrototype := GetSharedBooleanPrototype;
  if not Assigned(Prototype) and Assigned(SharedPrototype) and
     (SharedPrototype <> TGocciaObjectValue(Self)) then
    Result := SharedPrototype.GetPropertyWithContext(AName, AThisContext);
end;

class function TGocciaBooleanObjectValue.GetSharedPrototype: TGocciaObjectValue;
begin
  if not Assigned(GetSharedBooleanPrototype) then
    TGocciaBooleanObjectValue.Create(TGocciaBooleanLiteralValue.FalseValue);
  Result := GetSharedBooleanPrototype;
end;

function TGocciaBooleanObjectValue.BooleanValueOf(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := ExtractPrimitive(AThisValue);
end;

function TGocciaBooleanObjectValue.BooleanToString(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Prim: TGocciaBooleanLiteralValue;
begin
  Prim := ExtractPrimitive(AThisValue);

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
  GBooleanMethodHostSlot := RegisterRealmSlot('Boolean.prototype.methodHost');

end.
