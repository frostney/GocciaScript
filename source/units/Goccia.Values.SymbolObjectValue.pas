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
    function GetProperty(const AName: string): TGocciaValue; override;
    function GetPropertyWithContext(const AName: string; const AThisContext: TGocciaValue): TGocciaValue; override;
    procedure MarkReferences; override;

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

function TGocciaSymbolObjectValue.GetProperty(const AName: string): TGocciaValue;
begin
  Result := GetPropertyWithContext(AName, Self);
end;

function TGocciaSymbolObjectValue.GetPropertyWithContext(const AName: string; const AThisContext: TGocciaValue): TGocciaValue;
begin
  Result := inherited GetPropertyWithContext(AName, AThisContext);
end;

procedure TGocciaSymbolObjectValue.MarkReferences;
begin
  if GCMarked then Exit;
  inherited;
  if Assigned(FSymbolData) then
    FSymbolData.MarkReferences;
end;

end.
