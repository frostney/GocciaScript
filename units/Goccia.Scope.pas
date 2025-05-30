unit Goccia.Scope;

{$I Goccia.inc}

interface

uses
  Goccia.Values.Base, Goccia.Values.Undefined, Goccia.Values.ObjectValue, Generics.Collections, Goccia.Error, Goccia.Interfaces, SysUtils, Goccia.Logger;

type
  // TODO: Add a way to access thisArg
  // TODO: Do we want to differentiate between global, local and function scopes?
  TGocciaScope = class
  private
    FValues: TDictionary<string, TGocciaValue>;
    FParent: TGocciaScope;
    FInterpreter: IGocciaInterpreter;
    FThisValue: TGocciaValue;
  public
    constructor Create(AInterpreter: IGocciaInterpreter; AParent: TGocciaScope = nil);
    destructor Destroy; override;
    function CreateChild: TGocciaScope;
    function GetValue(const AName: string): TGocciaValue;
    procedure Assign(const AName: string; AValue: TGocciaValue);
    function Contains(const AName: string): Boolean;
    property Parent: TGocciaScope read FParent;
    property Interpreter: IGocciaInterpreter read FInterpreter;
    procedure SetValue(const AName: string; AValue: TGocciaValue);
    property ThisValue: TGocciaValue read FThisValue write FThisValue;
    function GetThisProperty(const AName: string): TGocciaValue;
  end;


implementation

constructor TGocciaScope.Create(AInterpreter: IGocciaInterpreter; AParent: TGocciaScope = nil);
begin
  TGocciaLogger.Debug('Scope.Create: Creating new scope');
  TGocciaLogger.Debug('  Parent scope address: %d', [PtrUInt(AParent)]);

  FThisValue := TGocciaUndefinedValue.Create;
  FInterpreter := AInterpreter;
  FParent := AParent;
  FValues := TDictionary<string, TGocciaValue>.Create;
end;

destructor TGocciaScope.Destroy;
begin
  TGocciaLogger.Debug('Scope.Destroy: Destroying scope');
  TGocciaLogger.Debug('  Self address: %d', [PtrUInt(Self)]);
  TGocciaLogger.Debug('  Values dictionary address: %d', [PtrUInt(FValues)]);

  if Assigned(FValues) then
  begin
    FValues.Free;
    FValues := nil;
  end;

  inherited;
end;

function TGocciaScope.CreateChild: TGocciaScope;
begin
  Result := TGocciaScope.Create(FInterpreter, Self);
end;

function TGocciaScope.GetValue(const AName: string): TGocciaValue;
var
  Value: TGocciaValue;
begin
  TGocciaLogger.Debug('Scope.GetValue: Attempting to get value for name: %s', [AName]);
  TGocciaLogger.Debug('  Self address: %d', [PtrUInt(Self)]);
  TGocciaLogger.Debug('  Values dictionary address: %d', [PtrUInt(FValues)]);
  TGocciaLogger.Debug('  ThisValue: %s', [ThisValue.ToString]);

  if FValues.TryGetValue(AName, Value) then
  begin
    TGocciaLogger.Debug('  Found value of type: %s', [Value.ClassName]);
    Result := Value;
  end
  else if Assigned(FParent) then
  begin
    TGocciaLogger.Debug('  Value not found, checking enclosing scope');
    Result := FParent.GetValue(AName);
  end
  else
  begin
    TGocciaLogger.Debug('  Value not found and no enclosing scope');
    Result := TGocciaUndefinedValue.Create;
  end;
end;

function TGocciaScope.GetThisProperty(const AName: string): TGocciaValue;
begin
  if (ThisValue is TGocciaObjectValue) then
    Result := TGocciaObjectValue(ThisValue).GetProperty(AName)
  else
    Result := TGocciaUndefinedValue.Create;
end;

procedure TGocciaScope.Assign(const AName: string; AValue: TGocciaValue);
begin
  // TODO: Do we need this?
  if FValues.ContainsKey(AName) then
    FValues.AddOrSetValue(AName, AValue)
  else if Assigned(FParent) then
    FParent.Assign(AName, AValue)
  else
    raise TGocciaReferenceError.Create(Format('Undefined variable: %s', [AName]),
      0, 0, '', nil);
end;

function TGocciaScope.Contains(const AName: string): Boolean;
begin
  Result := FValues.ContainsKey(AName) or
    (Assigned(FParent) and FParent.Contains(AName));
end;

procedure TGocciaScope.SetValue(const AName: string; AValue: TGocciaValue);
begin
  TGocciaLogger.Debug('Scope.SetValue: Setting value for name: %s', [AName]);
  TGocciaLogger.Debug('  Value type: %s', [AValue.ClassName]);
  TGocciaLogger.Debug('  Self address: %d', [PtrUInt(Self)]);
  TGocciaLogger.Debug('  Values dictionary address: %d', [PtrUInt(FValues)]);

  FValues.AddOrSetValue(AName, AValue);
end;

end.
