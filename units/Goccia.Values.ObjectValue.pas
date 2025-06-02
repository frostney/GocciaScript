unit Goccia.Values.ObjectValue;

{$I Goccia.inc}

interface

uses
  Goccia.Values.Base, Generics.Collections, Goccia.Values.UndefinedValue, Math, Goccia.Logger, SysUtils;

type
  // TGocciaProperty = class
  // private
  //   FName: string;
  //   FValue: TGocciaValue;
  //   FWritable: Boolean;
  //   FEnumerable: Boolean;
  //   FConfigurable: Boolean;
  // public
  //   constructor Create(const AName: string; AValue: TGocciaValue; AWritable: Boolean = True; AEnumerable: Boolean = True; AConfigurable: Boolean = True);

  //   property Name: string read FName;
  //   property Value: TGocciaValue read FValue;
  //   property Writable: Boolean read FWritable;
  //   property Enumerable: Boolean read FEnumerable;
  //   property Configurable: Boolean read FConfigurable;
  // end;

  TGocciaObjectValue = class;

  TComputedPropertyFunction = function(const AObject: TGocciaObjectValue): TGocciaValue of object;

  TGocciaObjectValue = class(TGocciaValue)
  protected
    FProperties: TDictionary<string, TGocciaValue>;
    FComputedProperties: TDictionary<string, TComputedPropertyFunction>;
    FPrototype: TGocciaObjectValue;
  public
    constructor Create;
    destructor Destroy; override;
    function ToDebugString: string;
    function ToString: string; override;
    function ToBoolean: Boolean; override;
    function ToNumber: Double; override;
    function TypeName: string; override;
    procedure SetProperty(const AName: string; AValue: TGocciaValue);
    procedure SetComputedProperty(const AName: string; AValue: TComputedPropertyFunction);
    function GetProperty(const AName: string): TGocciaValue;
    function HasProperty(const AName: string): Boolean;
    function HasOwnProperty(const AName: string): Boolean;
    procedure DeleteProperty(const AName: string);

    property Properties: TDictionary<string, TGocciaValue> read FProperties;
    property Prototype: TGocciaObjectValue read FPrototype write FPrototype;
  end;


implementation

// TODO: Should we allow the prototype to be set in the constructor?
constructor TGocciaObjectValue.Create;
begin
  FProperties := TDictionary<string, TGocciaValue>.Create;
  FComputedProperties := TDictionary<string, TComputedPropertyFunction>.Create;

  // TODO: Should this be TGocciaNullValue?
  // TODO: Should we set a default prototype?
  FPrototype := nil;
end;

destructor TGocciaObjectValue.Destroy;
begin
  FProperties.Free;
  FComputedProperties.Free;
  inherited;
end;

function TGocciaObjectValue.ToDebugString: string;
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

    if Pair.Value is TGocciaObjectValue then
      Result := Result + Pair.Key + ': ' + TGocciaObjectValue(Pair.Value).ToDebugString
    else
      Result := Result + Pair.Key + ': ' + Pair.Value.ToString;

    First := False;
  end;

  if Assigned(FPrototype) then
  begin
    if not First then
      Result := Result + ', ';

    Result := Result + '[[Prototype]]: ' + FPrototype.ToDebugString;
    First := False;
  end;

  Result := Result + '}';
end;

function TGocciaObjectValue.ToString: string;
begin
  Result := '[Object object]';
end;

function TGocciaObjectValue.ToBoolean: Boolean;
begin
  Result := True;
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

procedure TGocciaObjectValue.SetComputedProperty(const AName: string; AValue: TComputedPropertyFunction);
begin
  FComputedProperties.AddOrSetValue(AName, AValue);
end;

function TGocciaObjectValue.GetProperty(const AName: string): TGocciaValue;
begin
  // Technically, computed properties should be saved like regular properties. This is a worksaround unless
  // we implement getter and setter properties.

  TGocciaLogger.Debug('TGocciaObjectValue.GetProperty: Start');
  TGocciaLogger.Debug('  Name: %s', [AName]);
  TGocciaLogger.Debug('  FComputedProperties.ContainsKey(AName): %s', [BoolToStr(FComputedProperties.ContainsKey(AName))]);
  TGocciaLogger.Debug('  FProperties.ContainsKey(AName): %s', [BoolToStr(FProperties.ContainsKey(AName))]);
  if Assigned(FPrototype) then
    TGocciaLogger.Debug('  FPrototype: %s', [FPrototype.ToString])
  else
    TGocciaLogger.Debug('  FPrototype: not assigned');

  if FComputedProperties.ContainsKey(AName) then
  begin
    TGocciaLogger.Debug('TGocciaObjectValue.GetProperty: FComputedProperties.ContainsKey(AName)');
    Result := FComputedProperties[AName](Self);
    Exit;
  end
  else
  begin
    TGocciaLogger.Debug('TGocciaObjectValue.GetProperty: FProperties.ContainsKey(AName)');
    if FProperties.ContainsKey(AName) then
    begin
      Result := FProperties[AName];
      Exit;
    end else
    begin
      if Assigned(FPrototype) then
      begin
        TGocciaLogger.Debug('TGocciaObjectValue.GetProperty: FPrototype is assigned');
        Result := FPrototype.GetProperty(AName);
        Exit;
      end;

      TGocciaLogger.Debug('TGocciaObjectValue.GetProperty: FProperties.ContainsKey(AName) is false');
      Result := TGocciaUndefinedValue.Create;
    end;
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

procedure TGocciaObjectValue.DeleteProperty(const AName: string);
begin
  if FProperties.ContainsKey(AName) then
    FProperties.Remove(AName);

  if FComputedProperties.ContainsKey(AName) then
    FComputedProperties.Remove(AName);
end;

end.
