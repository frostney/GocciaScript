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
  Goccia.Constants.PropertyNames,
  Goccia.GarbageCollector,
  Goccia.Values.BigIntValue,
  Goccia.Values.NativeFunction,
  Goccia.Values.ObjectPropertyDescriptor;

threadvar
  FSharedBigIntPrototype: TGocciaObjectValue;
  FPrototypeMethodHost: TGocciaBigIntObjectValue;
  FPrototypeMembers: TArray<TGocciaMemberDefinition>;

constructor TGocciaBigIntObjectValue.Create(const APrimitive: TGocciaValue; const AClass: TGocciaClassValue = nil);
begin
  inherited Create(AClass);
  FPrimitive := APrimitive;
  InitializePrototype;
  if not Assigned(AClass) and Assigned(FSharedBigIntPrototype) then
    FPrototype := FSharedBigIntPrototype;
end;

procedure TGocciaBigIntObjectValue.InitializePrototype;
var
  Members: TGocciaMemberCollection;
begin
  if Assigned(FSharedBigIntPrototype) then Exit;

  FSharedBigIntPrototype := TGocciaObjectValue.Create;
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
  RegisterMemberDefinitions(FSharedBigIntPrototype, FPrototypeMembers);

  if Assigned(TGarbageCollector.Instance) then
  begin
    TGarbageCollector.Instance.PinObject(FSharedBigIntPrototype);
    TGarbageCollector.Instance.PinObject(FPrototypeMethodHost);
  end;
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

  if Assigned(FSharedBigIntPrototype) then
    Result := FSharedBigIntPrototype.GetPropertyWithContext(AName, AThisContext);
end;

class function TGocciaBigIntObjectValue.GetSharedPrototype: TGocciaObjectValue;
begin
  if not Assigned(FSharedBigIntPrototype) then
    TGocciaBigIntObjectValue.Create(TGocciaBigIntValue.BigIntZero);
  Result := FSharedBigIntPrototype;
end;

function TGocciaBigIntObjectValue.BigIntObjectValueOf(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  if AThisValue is TGocciaBigIntObjectValue then
    Result := TGocciaBigIntObjectValue(AThisValue).Primitive
  else if AThisValue is TGocciaBigIntValue then
    Result := AThisValue
  else
    Result := AThisValue;
end;

function TGocciaBigIntObjectValue.BigIntObjectToString(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Prim: TGocciaValue;
begin
  if AThisValue is TGocciaBigIntObjectValue then
    Prim := TGocciaBigIntObjectValue(AThisValue).Primitive
  else if AThisValue is TGocciaBigIntValue then
    Prim := AThisValue
  else
    Prim := AThisValue;

  Result := Prim.ToStringLiteral;
end;

procedure TGocciaBigIntObjectValue.MarkReferences;
begin
  if GCMarked then Exit;
  inherited;
  if Assigned(FPrimitive) then
    FPrimitive.MarkReferences;
end;

end.
