unit Goccia.Values.SymbolObjectValue;

{$I Goccia.inc}

interface

uses
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  TGocciaSymbolObjectValue = class(TGocciaObjectValue)
  private
    FSymbolData: TGocciaValue;
  public
    constructor Create(const ASymbolData: TGocciaValue);
    procedure TraceReferences; override;

    property SymbolData: TGocciaValue read FSymbolData;
  end;

implementation

uses
  Goccia.GarbageCollector,
  Goccia.Realm,
  Goccia.Values.SymbolValue;

constructor TGocciaSymbolObjectValue.Create(const ASymbolData: TGocciaValue);
var
  Proto: TGocciaValue;
begin
  inherited Create;
  FSymbolData := ASymbolData;
  Proto := TGocciaSymbolValue.SharedPrototype;
  if Assigned(Proto) and (Proto is TGocciaObjectValue) then
    FPrototype := TGocciaObjectValue(Proto);
end;

procedure TGocciaSymbolObjectValue.TraceReferences;
begin
  inherited;
  if Assigned(FSymbolData) then
    FSymbolData.MarkReferences;
end;

end.
