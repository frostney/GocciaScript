unit Goccia.Values.ArrayValue;

{$I Goccia.inc}

interface

uses
  Goccia.Values.Base, Generics.Collections, Math;

type
  TGocciaArrayValue = class(TGocciaValue)
  private
    FElements: TObjectList<TGocciaValue>;
  public
    constructor Create;
    destructor Destroy; override;
    function ToString: string; override;
    function ToNumber: Double; override;
    function TypeName: string; override;
    property Elements: TObjectList<TGocciaValue> read FElements;
  end;

implementation

constructor TGocciaArrayValue.Create;
begin
  FElements := TObjectList<TGocciaValue>.Create(False);
end;

destructor TGocciaArrayValue.Destroy;
begin
  FElements.Free;
  inherited;
end;

function TGocciaArrayValue.ToString: string;
var
  I: Integer;
begin
  Result := '[';
  for I := 0 to FElements.Count - 1 do
  begin
    if I > 0 then
      Result := Result + ', ';
    Result := Result + FElements[I].ToString;
  end;
  Result := Result + ']';
end;

function TGocciaArrayValue.ToNumber: Double;
begin
  if FElements.Count = 0 then
    Result := NaN
  else
    Result := FElements[0].ToNumber;
end;

function TGocciaArrayValue.TypeName: string;
begin
  Result := 'object';
end;

end.