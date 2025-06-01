unit Goccia.Scope;

{$I Goccia.inc}

interface

uses
  Goccia.Values.Base, Goccia.Values.Undefined, Goccia.Values.ObjectValue, Generics.Collections, Goccia.Error, Goccia.Interfaces, SysUtils, TypInfo, Goccia.Logger;

type
  TGocciaScopeKind = (skUnknown, skGlobal, skFunction, skBlock, skCustom);

  // TODO: Add a way to access thisArg
  // TODO: Do we want to differentiate between global, local and function scopes?
  TGocciaScope = class
  private
    FValues: TDictionary<string, TGocciaValue>;
    FParent: TGocciaScope;
    FThisValue: TGocciaValue;
    FScopeKind: TGocciaScopeKind;
    FCustomLabel: string;
  public
    constructor Create(AParent: TGocciaScope = nil; AScopeKind: TGocciaScopeKind = skUnknown; const ACustomLabel: string = '');
    destructor Destroy; override;
    function CreateChild(AScopeKind: TGocciaScopeKind = skUnknown; const ACustomLabel: string = ''): TGocciaScope;
    function GetValue(const AName: string): TGocciaValue;
    procedure Assign(const AName: string; AValue: TGocciaValue);
    function Contains(const AName: string): Boolean;
    property Parent: TGocciaScope read FParent;
    procedure SetValue(const AName: string; AValue: TGocciaValue);
    property ThisValue: TGocciaValue read FThisValue write FThisValue;
    function GetThisProperty(const AName: string): TGocciaValue;
    property ScopeKind: TGocciaScopeKind read FScopeKind;
    property CustomLabel: string read FCustomLabel;
  end;


implementation

constructor TGocciaScope.Create(AParent: TGocciaScope = nil; AScopeKind: TGocciaScopeKind = skUnknown; const ACustomLabel: string = '');
begin
  FScopeKind := AScopeKind;
  FCustomLabel := ACustomLabel;
  TGocciaLogger.Debug('Scope.Create: Creating new scope');
  TGocciaLogger.Debug('  Kind: %s', [GetEnumName(TypeInfo(TGocciaScopeKind), Ord(FScopeKind))]);
  if FCustomLabel <> '' then
    TGocciaLogger.Debug('  CustomLabel: %s', [FCustomLabel]);
  TGocciaLogger.Debug('  Parent scope address: %d', [PtrUInt(AParent)]);

  FThisValue := TGocciaUndefinedValue.Create;
  FParent := AParent;
  FValues := TDictionary<string, TGocciaValue>.Create;
end;

destructor TGocciaScope.Destroy;
var
  Value: TGocciaValue;
  Key: string;
begin
  TGocciaLogger.Debug('Scope.Destroy: Destroying scope');
  TGocciaLogger.Debug('  Kind: %s', [GetEnumName(TypeInfo(TGocciaScopeKind), Ord(FScopeKind))]);
  if FCustomLabel <> '' then
    TGocciaLogger.Debug('  CustomLabel: %s', [FCustomLabel]);
  TGocciaLogger.Debug('  Self address: %d', [PtrUInt(Self)]);
  TGocciaLogger.Debug('  Values dictionary address: %d', [PtrUInt(FValues)]);

  if Assigned(FValues) then
  begin
    // Log all values in the scope before destruction
    TGocciaLogger.Debug('  Values in scope:');
    for Key in FValues.Keys do
    begin
      if FValues.TryGetValue(Key, Value) then
        TGocciaLogger.Debug('    %s: %s', [Key, Value.ToString])
      else
        TGocciaLogger.Debug('    %s: <not found>', [Key]);
    end;

    // Only free values in child/local scopes
    if FScopeKind <> skGlobal then
    begin
      TGocciaLogger.Debug('  Freeing values in non-global scope');
      for Value in FValues.Values do
      begin
        if Assigned(Value) then
        begin
          TGocciaLogger.Debug('    Freeing value of type: %s', [Value.ClassName]);
          Value.Free;
        end;
      end;
    end
    else
    begin
      TGocciaLogger.Debug('  Skipping value freeing in global scope');
    end;

    FValues.Free;
    FValues := nil;
  end;

  if Assigned(FThisValue) then
  begin
    TGocciaLogger.Debug('  Freeing ThisValue of type: %s', [FThisValue.ClassName]);
    FThisValue.Free;
    FThisValue := nil;
  end;

  inherited;
end;

function TGocciaScope.CreateChild(AScopeKind: TGocciaScopeKind = skUnknown; const ACustomLabel: string = ''): TGocciaScope;
begin
  Result := TGocciaScope.Create(Self, AScopeKind, ACustomLabel);
end;

function TGocciaScope.GetValue(const AName: string): TGocciaValue;
var
  Value: TGocciaValue;
begin
  TGocciaLogger.Debug('Scope.GetValue: Attempting to get value for name: %s', [AName]);
  TGocciaLogger.Debug('  Kind: %s', [GetEnumName(TypeInfo(TGocciaScopeKind), Ord(FScopeKind))]);
  if FCustomLabel <> '' then
    TGocciaLogger.Debug('  CustomLabel: %s', [FCustomLabel]);
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
  TGocciaLogger.Debug('  Kind: %s', [GetEnumName(TypeInfo(TGocciaScopeKind), Ord(FScopeKind))]);
  if FCustomLabel <> '' then
    TGocciaLogger.Debug('  CustomLabel: %s', [FCustomLabel]);
  TGocciaLogger.Debug('  Value type: %s', [AValue.ClassName]);
  TGocciaLogger.Debug('  Self address: %d', [PtrUInt(Self)]);
  TGocciaLogger.Debug('  Values dictionary address: %d', [PtrUInt(FValues)]);

  FValues.AddOrSetValue(AName, AValue);
end;

end.
