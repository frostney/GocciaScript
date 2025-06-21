unit Goccia.Scope;

{$I Goccia.inc}

interface

uses
  Goccia.Values.Base, Goccia.Values.UndefinedValue, Goccia.Values.ObjectValue, Goccia.Error, Goccia.Logger, Generics.Collections, SysUtils, TypInfo, Goccia.Interfaces;

type
  TGocciaScopeKind = (skUnknown, skGlobal, skFunction, skBlock, skCustom);

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
    function GetValue(const AName: string): TGocciaValue; virtual;
    procedure Assign(const AName: string; AValue: TGocciaValue); virtual;
    function Contains(const AName: string): Boolean; inline;
    property Parent: TGocciaScope read FParent;
    procedure SetValue(const AName: string; AValue: TGocciaValue); virtual;
    property ThisValue: TGocciaValue read FThisValue write FThisValue;
    function GetThisProperty(const AName: string): TGocciaValue;
    property ScopeKind: TGocciaScopeKind read FScopeKind;
    property CustomLabel: string read FCustomLabel;
  end;

  // Specialized scope for try-catch blocks with proper assignment propagation
  TGocciaCatchScope = class(TGocciaScope)
  private
    FCatchParameter: string;  // Track the catch parameter name for proper shadowing
  public
    constructor Create(AParent: TGocciaScope; const ACatchParameter: string);
    procedure Assign(const AName: string; AValue: TGocciaValue); override;
    procedure SetValue(const AName: string; AValue: TGocciaValue); override;
  end;

  // Specialized scope for strict mode that returns nil for undefined variables
  TGocciaStrictScope = class(TGocciaScope)
  public
    constructor Create(AParent: TGocciaScope; const ACustomLabel: string);
    function GetValue(const AName: string): TGocciaValue; override;
  end;


implementation

constructor TGocciaScope.Create(AParent: TGocciaScope = nil; AScopeKind: TGocciaScopeKind = skUnknown; const ACustomLabel: string = '');
begin
  FScopeKind := AScopeKind;
  FCustomLabel := ACustomLabel;
  Logger.Debug('Scope.Create: Creating new scope');
  Logger.Debug('  Kind: %s', [GetEnumName(TypeInfo(TGocciaScopeKind), Ord(FScopeKind))]);
  if FCustomLabel <> '' then
    Logger.Debug('  CustomLabel: %s', [FCustomLabel]);
  Logger.Debug('  Parent scope address: %d', [PtrUInt(AParent)]);

  FThisValue := TGocciaUndefinedValue.Create;
  FParent := AParent;
  FValues := TDictionary<string, TGocciaValue>.Create;
end;

destructor TGocciaScope.Destroy;
var
  Value: TGocciaValue;
  Key: string;
begin
  Logger.Debug('Scope.Destroy: Destroying scope');
  Logger.Debug('  Kind: %s', [GetEnumName(TypeInfo(TGocciaScopeKind), Ord(FScopeKind))]);
  if FCustomLabel <> '' then
    Logger.Debug('  CustomLabel: %s', [FCustomLabel]);
  Logger.Debug('  Self address: %d', [PtrUInt(Self)]);
  Logger.Debug('  Values dictionary address: %d', [PtrUInt(FValues)]);

  if Assigned(FValues) then
  begin
    // Log all values in the scope before destruction
    Logger.Debug('  Values in scope:');
    for Key in FValues.Keys do
    begin
      if FValues.TryGetValue(Key, Value) then
        Logger.Debug('    %s: %s', [Key, Value.ToString])
      else
        Logger.Debug('    %s: <not found>', [Key]);
    end;

    FValues.Free;
  end;

  // Don't free FThisValue - the scope doesn't own it, it's just a reference
  // The actual owner should handle freeing it
  if Assigned(FThisValue) then
  begin
    Logger.Debug('  Not freeing ThisValue (scope doesn''t own it): %s', [FThisValue.ClassName]);
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
  if FValues.TryGetValue(AName, Value) then
    Result := Value
  else if Assigned(FParent) then
    Result := FParent.GetValue(AName)
  else
    Result := TGocciaUndefinedValue.Create;
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
  if FValues.ContainsKey(AName) then
    FValues.AddOrSetValue(AName, AValue)
  else if Assigned(FParent) then
    FParent.Assign(AName, AValue)
  else
    raise TGocciaReferenceError.Create(Format('Undefined variable: %s', [AName]),
      0, 0, '', nil);
end;

function TGocciaScope.Contains(const AName: string): Boolean; inline;
begin
  Result := FValues.ContainsKey(AName) or
    (Assigned(FParent) and FParent.Contains(AName));
end;

procedure TGocciaScope.SetValue(const AName: string; AValue: TGocciaValue);
begin
  Logger.Debug('Scope.SetValue: Setting value for name: %s', [AName]);
  Logger.Debug('  Kind: %s', [GetEnumName(TypeInfo(TGocciaScopeKind), Ord(FScopeKind))]);
  if FCustomLabel <> '' then
    Logger.Debug('  CustomLabel: %s', [FCustomLabel]);
  Logger.Debug('  Value type: %s', [AValue.ClassName]);
  Logger.Debug('  Self address: %d', [PtrUInt(Self)]);
  Logger.Debug('  Values dictionary address: %d', [PtrUInt(FValues)]);

  FValues.AddOrSetValue(AName, AValue);
end;

// TGocciaCatchScope implementation

constructor TGocciaCatchScope.Create(AParent: TGocciaScope; const ACatchParameter: string);
begin
  inherited Create(AParent, skBlock, 'CatchBlock');
  FCatchParameter := ACatchParameter;
end;

procedure TGocciaCatchScope.Assign(const AName: string; AValue: TGocciaValue);
begin
  // Surgical fix for catch parameter scopes: assignments to non-parameter variables
  // should propagate to parent scope, but catch parameters should stay for proper shadowing
  if (AName <> FCatchParameter) and (not FValues.ContainsKey(AName)) and Assigned(FParent) then
  begin
    // This is a catch parameter scope and the variable isn't the catch parameter.
    // Delegate directly to parent to ensure assignment propagation
    FParent.Assign(AName, AValue);
  end
  else
  begin
    // Either it's the catch parameter or it exists in current scope - use base behavior
    inherited Assign(AName, AValue);
  end;
end;

procedure TGocciaCatchScope.SetValue(const AName: string; AValue: TGocciaValue);
begin
  // Surgical fix: Also apply to SetValue for catch parameter scopes
  // Delegate to parent for non-parameter variables, but keep catch parameters for shadowing
  if (AName <> FCatchParameter) and (not FValues.ContainsKey(AName)) and Assigned(FParent) then
  begin
    // Check if the variable exists in parent scope - if so, assign there
    if FParent.Contains(AName) then
    begin
      FParent.Assign(AName, AValue);
      Exit;
    end;
  end;

  // Use base implementation
  inherited SetValue(AName, AValue);
end;

// TGocciaStrictScope implementation

constructor TGocciaStrictScope.Create(AParent: TGocciaScope; const ACustomLabel: string);
begin
  inherited Create(AParent, skFunction, ACustomLabel);
end;

function TGocciaStrictScope.GetValue(const AName: string): TGocciaValue;
var
  Value: TGocciaValue;
begin
  if FValues.TryGetValue(AName, Value) then
    Result := Value
  else if Assigned(FParent) then
  begin
    Result := FParent.GetValue(AName);
    // Convert undefined to nil in strict mode to trigger error handling
    if Result is TGocciaUndefinedValue then
      Result := nil;
  end
  else
    Result := nil; // Return nil for undefined variables in strict mode
end;

end.
