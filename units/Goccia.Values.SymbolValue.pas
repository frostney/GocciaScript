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

    function ToBooleanLiteral: TGocciaBooleanLiteralValue; override;
    function ToNumberLiteral: TGocciaNumberLiteralValue; override;
    function ToStringLiteral: TGocciaStringLiteralValue; override;

    property Description: string read FDescription;
    property Id: Integer read FId;
  end;

implementation

uses
  Goccia.Values.Constants;

var
  GNextSymbolId: Integer = 0;

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

function TGocciaSymbolValue.ToBooleanLiteral: TGocciaBooleanLiteralValue;
begin
  Result := TGocciaBooleanLiteralValue.TrueValue;
end;

function TGocciaSymbolValue.ToNumberLiteral: TGocciaNumberLiteralValue;
begin
  Result := TGocciaNumberLiteralValue.NaNValue;
end;

function TGocciaSymbolValue.ToStringLiteral: TGocciaStringLiteralValue;
begin
  if FDescription <> '' then
    Result := TGocciaStringLiteralValue.Create('Symbol(' + FDescription + ')')
  else
    Result := TGocciaStringLiteralValue.Create('Symbol()');
end;

end.
