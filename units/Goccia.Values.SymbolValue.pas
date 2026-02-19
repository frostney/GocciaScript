unit Goccia.Values.SymbolValue;

{$I Goccia.inc}

interface

uses
  Goccia.Arguments.Collection,
  Goccia.Values.Primitives;

type
  TGocciaSymbolValue = class(TGocciaValue)
  private class var
    FSharedPrototype: TGocciaValue;
    FMethodHost: TGocciaSymbolValue;
    FWellKnownIterator: TGocciaSymbolValue;
  private
    FDescription: string;
    FId: Integer;

    function GetDescription(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function SymbolToString(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
  public
    constructor Create(const ADescription: string = '');
    procedure InitializePrototype;

    class function SharedPrototype: TGocciaValue;
    class function WellKnownIterator: TGocciaSymbolValue;

    function TypeName: string; override;
    function TypeOf: string; override;
    function GetProperty(const AName: string): TGocciaValue; override;

    function ToBooleanLiteral: TGocciaBooleanLiteralValue; override;
    function ToNumberLiteral: TGocciaNumberLiteralValue; override;
    function ToStringLiteral: TGocciaStringLiteralValue; override;

    property Description: string read FDescription;
    property Id: Integer read FId;
  end;

implementation

uses
  Goccia.GarbageCollector,
  Goccia.Values.Constants,
  Goccia.Values.ErrorHelper,
  Goccia.Values.NativeFunction,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.ObjectValue;

var
  GNextSymbolId: Integer = 0;

function TGocciaSymbolValue.SymbolToString(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  if not (AThisValue is TGocciaSymbolValue) then
    ThrowTypeError('Symbol.prototype.toString requires that ''this'' be a Symbol');
  Result := AThisValue.ToStringLiteral;
end;

function TGocciaSymbolValue.GetDescription(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  if not (AThisValue is TGocciaSymbolValue) then
    ThrowTypeError('Symbol.prototype.description requires that ''this'' be a Symbol');
  if TGocciaSymbolValue(AThisValue).FDescription <> '' then
    Result := TGocciaStringLiteralValue.Create(TGocciaSymbolValue(AThisValue).FDescription)
  else
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

procedure TGocciaSymbolValue.InitializePrototype;
var
  Proto: TGocciaObjectValue;
begin
  if Assigned(FSharedPrototype) then Exit;

  Proto := TGocciaObjectValue.Create;
  FSharedPrototype := Proto;
  FMethodHost := Self;

  Proto.DefineProperty('description', TGocciaPropertyDescriptorAccessor.Create(
    TGocciaNativeFunctionValue.CreateWithoutPrototype(GetDescription, 'description', 0), nil, [pfConfigurable]));

  Proto.RegisterNativeMethod(
    TGocciaNativeFunctionValue.CreateWithoutPrototype(SymbolToString, 'toString', 0));

  if Assigned(TGocciaGarbageCollector.Instance) then
  begin
    TGocciaGarbageCollector.Instance.PinValue(FSharedPrototype);
    TGocciaGarbageCollector.Instance.PinValue(FMethodHost);
  end;
end;

class function TGocciaSymbolValue.SharedPrototype: TGocciaValue;
begin
  Result := FSharedPrototype;
end;

class function TGocciaSymbolValue.WellKnownIterator: TGocciaSymbolValue;
begin
  if not Assigned(FWellKnownIterator) then
  begin
    FWellKnownIterator := TGocciaSymbolValue.Create('Symbol.iterator');
    if Assigned(TGocciaGarbageCollector.Instance) then
      TGocciaGarbageCollector.Instance.PinValue(FWellKnownIterator);
  end;
  Result := FWellKnownIterator;
end;

constructor TGocciaSymbolValue.Create(const ADescription: string);
begin
  inherited Create;
  FDescription := ADescription;
  FId := GNextSymbolId;
  Inc(GNextSymbolId);
end;

function TGocciaSymbolValue.TypeName: string;
begin
  Result := SYMBOL_TYPE_NAME;
end;

function TGocciaSymbolValue.TypeOf: string;
begin
  Result := SYMBOL_TYPE_NAME;
end;

function TGocciaSymbolValue.GetProperty(const AName: string): TGocciaValue;
begin
  if Assigned(FSharedPrototype) then
  begin
    Result := TGocciaObjectValue(FSharedPrototype).GetPropertyWithContext(AName, Self);
    if Assigned(Result) then
      Exit;
  end;
  Result := nil;
end;

function TGocciaSymbolValue.ToBooleanLiteral: TGocciaBooleanLiteralValue;
begin
  Result := TGocciaBooleanLiteralValue.TrueValue;
end;

function TGocciaSymbolValue.ToNumberLiteral: TGocciaNumberLiteralValue;
begin
  ThrowTypeError('Cannot convert a Symbol value to a number');
  Result := nil;
end;

function TGocciaSymbolValue.ToStringLiteral: TGocciaStringLiteralValue;
begin
  if FDescription <> '' then
    Result := TGocciaStringLiteralValue.Create('Symbol(' + FDescription + ')')
  else
    Result := TGocciaStringLiteralValue.Create('Symbol()');
end;

end.
