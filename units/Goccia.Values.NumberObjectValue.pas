unit Goccia.Values.NumberObjectValue;

{$I Goccia.inc}

interface

uses
  Goccia.Arguments.Collection,
  Goccia.Values.ClassValue,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  TGocciaNumberObjectValue = class(TGocciaInstanceValue)
  private
    FPrimitive: TGocciaNumberLiteralValue;

    class var FSharedNumberPrototype: TGocciaObjectValue;
    class var FPrototypeMethodHost: TGocciaNumberObjectValue;

    function ExtractPrimitive(const AValue: TGocciaValue): TGocciaNumberLiteralValue;
  public
    constructor Create(const APrimitive: TGocciaNumberLiteralValue; const AClass: TGocciaClassValue = nil);
    function GetProperty(const AName: string): TGocciaValue; override;
    procedure InitializePrototype;
    procedure MarkReferences; override;

    class function GetSharedPrototype: TGocciaObjectValue;

    property Primitive: TGocciaNumberLiteralValue read FPrimitive;

    // Number prototype methods
    function NumberToFixed(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function NumberToString(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function NumberValueOf(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function NumberToPrecision(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
  end;

implementation

uses
  SysUtils,

  Goccia.GarbageCollector,
  Goccia.Values.NativeFunction;

function TGocciaNumberObjectValue.ExtractPrimitive(const AValue: TGocciaValue): TGocciaNumberLiteralValue;
begin
  if AValue is TGocciaNumberLiteralValue then
    Result := TGocciaNumberLiteralValue(AValue)
  else if AValue is TGocciaNumberObjectValue then
    Result := TGocciaNumberObjectValue(AValue).Primitive
  else
    Result := AValue.ToNumberLiteral;
end;

constructor TGocciaNumberObjectValue.Create(const APrimitive: TGocciaNumberLiteralValue; const AClass: TGocciaClassValue = nil);
begin
  inherited Create(AClass);
  FPrimitive := APrimitive;
  InitializePrototype;
  if not Assigned(AClass) and Assigned(FSharedNumberPrototype) then
    FPrototype := FSharedNumberPrototype;
end;

function TGocciaNumberObjectValue.GetProperty(const AName: string): TGocciaValue;
begin
  Result := inherited GetProperty(AName);
  if not (Result is TGocciaUndefinedLiteralValue) then
    Exit;

  if Assigned(FSharedNumberPrototype) then
    Result := FSharedNumberPrototype.GetPropertyWithContext(AName, Self);
end;

procedure TGocciaNumberObjectValue.InitializePrototype;
begin
  if Assigned(FSharedNumberPrototype) then Exit;

  FSharedNumberPrototype := TGocciaObjectValue.Create;
  FPrototypeMethodHost := Self;

  FSharedNumberPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(NumberToFixed, 'toFixed', 1));
  FSharedNumberPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(NumberToString, 'toString', 1));
  FSharedNumberPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(NumberValueOf, 'valueOf', 0));
  FSharedNumberPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(NumberToPrecision, 'toPrecision', 1));

  if Assigned(TGocciaGarbageCollector.Instance) then
  begin
    TGocciaGarbageCollector.Instance.PinValue(FSharedNumberPrototype);
    TGocciaGarbageCollector.Instance.PinValue(FPrototypeMethodHost);
  end;
end;

class function TGocciaNumberObjectValue.GetSharedPrototype: TGocciaObjectValue;
begin
  if not Assigned(FSharedNumberPrototype) then
    TGocciaNumberObjectValue.Create(TGocciaNumberLiteralValue.ZeroValue);
  Result := FSharedNumberPrototype;
end;

procedure TGocciaNumberObjectValue.MarkReferences;
begin
  if GCMarked then Exit;
  inherited;
  if Assigned(FPrimitive) then
    FPrimitive.MarkReferences;
end;

function TGocciaNumberObjectValue.NumberToFixed(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Prim: TGocciaNumberLiteralValue;
  Digits: Integer;
begin
  Prim := ExtractPrimitive(AThisValue);

  // Special values return their string representation
  if Prim.IsNaN then
  begin
    Result := TGocciaStringLiteralValue.Create('NaN');
    Exit;
  end;
  if Prim.IsInfinity then
  begin
    Result := TGocciaStringLiteralValue.Create('Infinity');
    Exit;
  end;
  if Prim.IsNegativeInfinity then
  begin
    Result := TGocciaStringLiteralValue.Create('-Infinity');
    Exit;
  end;

  if AArgs.Length > 0 then
    Digits := Trunc(AArgs.GetElement(0).ToNumberLiteral.Value)
  else
    Digits := 0;

  if (Digits < 0) or (Digits > 100) then
  begin
    Result := TGocciaStringLiteralValue.Create('');
    Exit;
  end;

  Result := TGocciaStringLiteralValue.Create(FormatFloat('0.' + StringOfChar('0', Digits), Prim.Value));
end;

function TGocciaNumberObjectValue.NumberToString(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Prim: TGocciaNumberLiteralValue;
  Radix: Integer;
begin
  Prim := ExtractPrimitive(AThisValue);

  // Special values always return their default string
  if Prim.IsNaN or Prim.IsInfinity or Prim.IsNegativeInfinity or Prim.IsNegativeZero then
  begin
    Result := Prim.ToStringLiteral;
    Exit;
  end;

  if AArgs.Length > 0 then
  begin
    Radix := Trunc(AArgs.GetElement(0).ToNumberLiteral.Value);
    if (Radix < 2) or (Radix > 36) then
    begin
      Result := TGocciaStringLiteralValue.Create('');
      Exit;
    end;
    if Radix = 16 then
      Result := TGocciaStringLiteralValue.Create(LowerCase(IntToHex(Trunc(Prim.Value), 1)))
    else
      Result := Prim.ToStringLiteral;
  end
  else
    Result := Prim.ToStringLiteral;
end;

function TGocciaNumberObjectValue.NumberValueOf(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := ExtractPrimitive(AThisValue);
end;

function TGocciaNumberObjectValue.NumberToPrecision(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Prim: TGocciaNumberLiteralValue;
  Precision: Integer;
begin
  Prim := ExtractPrimitive(AThisValue);

  // Special values return their string representation
  if Prim.IsNaN or Prim.IsInfinity or Prim.IsNegativeInfinity then
  begin
    Result := Prim.ToStringLiteral;
    Exit;
  end;

  if AArgs.Length = 0 then
  begin
    Result := Prim.ToStringLiteral;
    Exit;
  end;

  Precision := Trunc(AArgs.GetElement(0).ToNumberLiteral.Value);

  if (Precision < 1) or (Precision > 100) then
  begin
    Result := TGocciaStringLiteralValue.Create('');
    Exit;
  end;

  Result := TGocciaStringLiteralValue.Create(FloatToStrF(Prim.Value, ffGeneral, Precision, 0));
end;

end.
