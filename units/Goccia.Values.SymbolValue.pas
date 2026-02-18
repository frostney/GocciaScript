unit Goccia.Values.SymbolValue;

{$I Goccia.inc}

interface

uses
  Goccia.Values.Primitives;

type
  TGocciaSymbolValue = class(TGocciaValue)
  private
    FDescription: string;
    FId: Integer;
  public
    constructor Create(const ADescription: string = '');

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
  Goccia.Values.Constants, Goccia.Values.ErrorHelper,
  Goccia.Values.NativeFunction, Goccia.Arguments.Collection,
  Goccia.GarbageCollector;

type
  TGocciaSymbolMethodHost = class
    function SymbolToString(Args: TGocciaArgumentsCollection;
      ThisValue: TGocciaValue): TGocciaValue;
  end;

var
  GNextSymbolId: Integer = 0;
  GSharedToString: TGocciaNativeFunctionValue = nil;
  GMethodHost: TGocciaSymbolMethodHost = nil;

function TGocciaSymbolMethodHost.SymbolToString(Args: TGocciaArgumentsCollection;
  ThisValue: TGocciaValue): TGocciaValue;
begin
  if not (ThisValue is TGocciaSymbolValue) then
    ThrowTypeError('Symbol.prototype.toString requires that ''this'' be a Symbol');
  Result := ThisValue.ToStringLiteral;
end;

procedure EnsureSymbolPrototype;
begin
  if Assigned(GSharedToString) then Exit;

  GMethodHost := TGocciaSymbolMethodHost.Create;
  GSharedToString := TGocciaNativeFunctionValue.CreateWithoutPrototype(
    GMethodHost.SymbolToString, 'toString', 0);

  if Assigned(TGocciaGC.Instance) then
    TGocciaGC.Instance.PinValue(GSharedToString);
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
  if AName = 'toString' then
  begin
    EnsureSymbolPrototype;
    Result := GSharedToString;
  end
  else if AName = 'description' then
  begin
    if FDescription <> '' then
      Result := TGocciaStringLiteralValue.Create(FDescription)
    else
      Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  end
  else
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

finalization
  GMethodHost.Free;

end.
