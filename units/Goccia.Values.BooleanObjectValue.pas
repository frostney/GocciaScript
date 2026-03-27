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
    class var FSharedBooleanPrototype: TGocciaObjectValue;
    class var FPrototypeMethodHost: TGocciaBooleanObjectValue;
    class var FPrototypeMembers: array of TGocciaMemberDefinition;
  private
    FPrimitive: TGocciaBooleanLiteralValue;
  public
    constructor Create(const APrimitive: TGocciaBooleanLiteralValue; const AClass: TGocciaClassValue = nil);
    procedure InitializePrototype;
    function GetProperty(const AName: string): TGocciaValue; override;
    procedure MarkReferences; override;

    class function GetSharedPrototype: TGocciaObjectValue;

    property Primitive: TGocciaBooleanLiteralValue read FPrimitive;
  published
    function BooleanValueOf(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function BooleanToString(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
  end;

implementation

uses
  GarbageCollector.Generic,

  Goccia.Constants.PropertyNames,
  Goccia.Values.NativeFunction;

constructor TGocciaBooleanObjectValue.Create(const APrimitive: TGocciaBooleanLiteralValue; const AClass: TGocciaClassValue = nil);
begin
  inherited Create(AClass);
  FPrimitive := APrimitive;
  InitializePrototype;
  if not Assigned(AClass) and Assigned(FSharedBooleanPrototype) then
    FPrototype := FSharedBooleanPrototype;
end;

procedure TGocciaBooleanObjectValue.InitializePrototype;
var
  Members: TGocciaMemberCollection;
begin
  if Assigned(FSharedBooleanPrototype) then Exit;

  FSharedBooleanPrototype := TGocciaObjectValue.Create;
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
  RegisterMemberDefinitions(FSharedBooleanPrototype, FPrototypeMembers);

  if Assigned(TGarbageCollector.Instance) then
  begin
    TGarbageCollector.Instance.PinObject(FSharedBooleanPrototype);
    TGarbageCollector.Instance.PinObject(FPrototypeMethodHost);
  end;
end;

function TGocciaBooleanObjectValue.GetProperty(const AName: string): TGocciaValue;
begin
  Result := inherited GetProperty(AName);
  if not (Result is TGocciaUndefinedLiteralValue) then
    Exit;

  if Assigned(FSharedBooleanPrototype) then
    Result := FSharedBooleanPrototype.GetPropertyWithContext(AName, Self);
end;

class function TGocciaBooleanObjectValue.GetSharedPrototype: TGocciaObjectValue;
begin
  if not Assigned(FSharedBooleanPrototype) then
    TGocciaBooleanObjectValue.Create(TGocciaBooleanLiteralValue.FalseValue);
  Result := FSharedBooleanPrototype;
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

end.
