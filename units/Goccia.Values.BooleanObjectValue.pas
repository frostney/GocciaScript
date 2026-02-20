unit Goccia.Values.BooleanObjectValue;

{$I Goccia.inc}

interface

uses
  Goccia.Arguments.Collection,
  Goccia.Values.ClassValue,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  TGocciaBooleanObjectValue = class(TGocciaInstanceValue)
  private
    class var FSharedBooleanPrototype: TGocciaObjectValue;
    class var FPrototypeMethodHost: TGocciaBooleanObjectValue;
  private
    FPrimitive: TGocciaBooleanLiteralValue;

    function BooleanValueOf(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function BooleanToString(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
  public
    constructor Create(const APrimitive: TGocciaBooleanLiteralValue; const AClass: TGocciaClassValue = nil);
    procedure InitializePrototype;
    function GetProperty(const AName: string): TGocciaValue; override;
    procedure MarkReferences; override;

    class function GetSharedPrototype: TGocciaObjectValue;

    property Primitive: TGocciaBooleanLiteralValue read FPrimitive;
  end;

implementation

uses
  Goccia.GarbageCollector,
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
begin
  if Assigned(FSharedBooleanPrototype) then Exit;

  FSharedBooleanPrototype := TGocciaObjectValue.Create;
  FPrototypeMethodHost := Self;

  FSharedBooleanPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(BooleanValueOf, 'valueOf', 0));
  FSharedBooleanPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(BooleanToString, 'toString', 0));

  if Assigned(TGocciaGarbageCollector.Instance) then
  begin
    TGocciaGarbageCollector.Instance.PinValue(FSharedBooleanPrototype);
    TGocciaGarbageCollector.Instance.PinValue(FPrototypeMethodHost);
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