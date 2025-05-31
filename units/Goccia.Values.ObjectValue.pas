unit Goccia.Values.ObjectValue;

{$I Goccia.inc}

interface

uses
  Goccia.Values.Base, Generics.Collections, Goccia.Values.Undefined, Math;

type
  TGocciaObjectValue = class(TGocciaValue)
  private
    FProperties: TDictionary<string, TGocciaValue>;
    FPrototype: TGocciaObjectValue;
  public
    constructor Create;
    destructor Destroy; override;
    function ToString: string; override;
    function ToNumber: Double; override;
    function TypeName: string; override;
    procedure SetProperty(const AName: string; AValue: TGocciaValue);
    function GetProperty(const AName: string): TGocciaValue;
    function HasProperty(const AName: string): Boolean;
    function HasOwnProperty(const AName: string): Boolean;
    property Properties: TDictionary<string, TGocciaValue> read FProperties;
    property Prototype: TGocciaObjectValue read FPrototype write FPrototype;
  end;


implementation

constructor TGocciaObjectValue.Create;
begin
  FProperties := TDictionary<string, TGocciaValue>.Create;
  FPrototype := nil;
end;

destructor TGocciaObjectValue.Destroy;
begin
  FProperties.Free;
  inherited;
end;

function TGocciaObjectValue.ToString: string;
var
  Pair: TPair<string, TGocciaValue>;
  First: Boolean;
begin
  Result := '{';
  First := True;
  for Pair in FProperties do
  begin
    if not First then
      Result := Result + ', ';
    Result := Result + Pair.Key + ': ' + Pair.Value.ToString;
    First := False;
  end;
  Result := Result + '}';
end;

function TGocciaObjectValue.ToNumber: Double;
begin
  Result := NaN;
end;

function TGocciaObjectValue.TypeName: string;
begin
  Result := 'object';
end;

procedure TGocciaObjectValue.SetProperty(const AName: string; AValue: TGocciaValue);
begin
  FProperties.AddOrSetValue(AName, AValue);
end;

function TGocciaObjectValue.GetProperty(const AName: string): TGocciaValue;
begin
  if not FProperties.TryGetValue(AName, Result) then
  begin
    if Assigned(FPrototype) then
      Result := FPrototype.GetProperty(AName)
    else
      Result := TGocciaUndefinedValue.Create;
  end;
end;

function TGocciaObjectValue.HasProperty(const AName: string): Boolean;
begin
  Result := HasOwnProperty(AName);

  if not Result and Assigned(FPrototype) then
    Result := FPrototype.HasProperty(AName);
end;

function TGocciaObjectValue.HasOwnProperty(const AName: string): Boolean;
begin
  Result := FProperties.ContainsKey(AName);
end;

end.
