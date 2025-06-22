unit Goccia.Scope;

{$I Goccia.inc}

interface

uses
  Goccia.Values.Base, Goccia.Values.UndefinedValue, Goccia.Values.ObjectPropertyDescriptor, Goccia.Values.ObjectValue, Goccia.Error, Goccia.Logger, Generics.Collections, SysUtils, TypInfo, Goccia.Interfaces, Goccia.Token;

type
  TVariableFlags = set of (vfWritable, vfEnumerable, vfConfigurable, vfInitialized);

  TGocciaDeclarationType = (dtUnknown, dtLet, dtConst);

  TVariableDescriptor = record
    Value: TGocciaValue;
    Flags: TVariableFlags;
    DeclarationType: TGocciaDeclarationType; // dtLet, dtConst, dtImplicit

    function GetWritable: Boolean;
    function GetEnumerable: Boolean;
    function GetConfigurable: Boolean;
    function GetInitialized: Boolean;
  end;

  TGocciaScopeKind = (skUnknown, skGlobal, skFunction, skBlock, skCustom, skClass);

  TGocciaScope = class
  private
    FVariables: TDictionary<string, TVariableDescriptor>;
    FParent: TGocciaScope;
    FThisValue: TGocciaValue;
    FScopeKind: TGocciaScopeKind;
    FCustomLabel: string;
  public
    constructor Create(AParent: TGocciaScope = nil; AScopeKind: TGocciaScopeKind = skUnknown; const ACustomLabel: string = '');
    destructor Destroy; override;
    function CreateChild(AScopeKind: TGocciaScopeKind = skUnknown; const ACustomLabel: string = ''): TGocciaScope;

    // New Define/Assign pattern
    procedure DefineVariable(const AName: string; AValue: TGocciaValue; ADeclarationType: TGocciaDeclarationType);
    procedure AssignVariable(const AName: string; AValue: TGocciaValue); virtual;

    // Helper methods for token-based declarations
    procedure DefineFromToken(const AName: string; AValue: TGocciaValue; ATokenType: TGocciaTokenType);

    // Core methods
    function GetValue(const AName: string): TGocciaValue; virtual;
    function Contains(const AName: string): Boolean; inline;
    procedure DefineBuiltin(const AName: string; AValue: TGocciaValue); virtual;

    property Parent: TGocciaScope read FParent;
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
    procedure AssignVariable(const AName: string; AValue: TGocciaValue); override;
    procedure DefineBuiltin(const AName: string; AValue: TGocciaValue); override;
  end;

  TGocciaGlobalScope = class(TGocciaScope)
  private
    FGlobalObject: TGocciaObjectValue;
  public
    constructor Create;
    function GetValue(const AName: string): TGocciaValue;
  end;

  TGocciaClassScope = class(TGocciaScope)
  private
    FSuperClass: TGocciaObjectValue;
  public
    constructor Create(AParent: TGocciaScope; const AClassName: string);
    function GetSuperClass(): TGocciaObjectValue;
  end;



implementation

{ TVariableDescriptor }

function TVariableDescriptor.GetWritable: Boolean;
begin
  Result := vfWritable in Flags;
end;

function TVariableDescriptor.GetEnumerable: Boolean;
begin
  Result := vfEnumerable in Flags;
end;

function TVariableDescriptor.GetConfigurable: Boolean;
begin
  Result := vfConfigurable in Flags;
end;

function TVariableDescriptor.GetInitialized: Boolean;
begin
  Result := vfInitialized in Flags;
end;

{ TGocciaScope }

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
  FVariables := TDictionary<string, TVariableDescriptor>.Create;
end;

destructor TGocciaScope.Destroy;
var
  VarDescriptor: TVariableDescriptor;
  Key: string;
begin
  Logger.Debug('Scope.Destroy: Destroying scope');
  Logger.Debug('  Kind: %s', [GetEnumName(TypeInfo(TGocciaScopeKind), Ord(FScopeKind))]);
  if FCustomLabel <> '' then
    Logger.Debug('  CustomLabel: %s', [FCustomLabel]);
  Logger.Debug('  Self address: %d', [PtrUInt(Self)]);
  Logger.Debug('  Variables dictionary address: %d', [PtrUInt(FVariables)]);

  if Assigned(FVariables) then
  begin
    // Log all variables in the scope before destruction
    Logger.Debug('  Variables in scope:');
    for Key in FVariables.Keys do
    begin
      if FVariables.TryGetValue(Key, VarDescriptor) then
        Logger.Debug('    %s: %s', [Key, VarDescriptor.Value.ToString])
      else
        Logger.Debug('    %s: <not found>', [Key]);
    end;

    FVariables.Free;
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

procedure TGocciaScope.DefineVariable(const AName: string; AValue: TGocciaValue; ADeclarationType: TGocciaDeclarationType);
var
  Descriptor: TVariableDescriptor;
  ExistingDescriptor: TVariableDescriptor;
begin
  // Check for redeclaration errors
  if FVariables.TryGetValue(AName, ExistingDescriptor) then
  begin
    // Check redeclaration rules - let/const cannot be redeclared
    case ADeclarationType of
      dtLet, dtConst:
        begin
          // let/const cannot be redeclared
          raise TGocciaSyntaxError.Create(Format('Identifier ''%s'' has already been declared', [AName]), 0, 0, '', nil);
        end;
    end;
  end;

  // Set up descriptor based on declaration type
  Descriptor.Value := AValue;
  Descriptor.DeclarationType := ADeclarationType;

  case ADeclarationType of
    dtConst:
      begin
        // const: non-writable, enumerable, configurable, must be initialized
        Descriptor.Flags := [vfEnumerable, vfConfigurable, vfInitialized];
        if AValue is TGocciaUndefinedValue then
          raise TGocciaSyntaxError.Create('Missing initializer in const declaration', 0, 0, '', nil);
      end;
    dtLet:
      begin
        // let: writable, enumerable, configurable
        if AValue is TGocciaUndefinedValue then
          Descriptor.Flags := [vfWritable, vfEnumerable, vfConfigurable]
        else
          Descriptor.Flags := [vfWritable, vfEnumerable, vfConfigurable, vfInitialized];
      end;
  else
    // Default for unknown types (treat as let)
    Descriptor.Flags := [vfWritable, vfEnumerable, vfConfigurable, vfInitialized];
  end;

  FVariables.AddOrSetValue(AName, Descriptor);
end;

procedure TGocciaScope.DefineFromToken(const AName: string; AValue: TGocciaValue; ATokenType: TGocciaTokenType);
var
  DeclarationType: TGocciaDeclarationType;
begin
  case ATokenType of
    gttLet: DeclarationType := dtLet;
    gttConst: DeclarationType := dtConst;
  else
    DeclarationType := dtLet; // Default to let in strict mode
  end;

  DefineVariable(AName, AValue, DeclarationType);
end;

procedure TGocciaScope.AssignVariable(const AName: string; AValue: TGocciaValue);
var
  Descriptor: TVariableDescriptor;
begin
  // Try to find variable in current scope first
  if FVariables.TryGetValue(AName, Descriptor) then
  begin
    // Check if variable is initialized (temporal dead zone for let/const)
    if not Descriptor.GetInitialized and (Descriptor.DeclarationType in [dtLet, dtConst]) then
      raise TGocciaReferenceError.Create(Format('Cannot access ''%s'' before initialization', [AName]), 0, 0, '', nil);

    // Check if variable is writable
    if not Descriptor.GetWritable then
      raise TGocciaTypeError.Create(Format('Assignment to constant variable ''%s''', [AName]), 0, 0, '', nil);

    // Update the value and mark as initialized
    Descriptor.Value := AValue;
    Descriptor.Flags := Descriptor.Flags + [vfInitialized];
    FVariables.AddOrSetValue(AName, Descriptor);
    Exit;
  end;

  // Variable not found in current scope, try parent scope
  if Assigned(FParent) then
  begin
    FParent.AssignVariable(AName, AValue);
    Exit;
  end;

  // Variable not found in any scope - strict mode always throws
  raise TGocciaReferenceError.Create(Format('Undefined variable: %s', [AName]), 0, 0, '', nil);
end;

function TGocciaScope.GetValue(const AName: string): TGocciaValue;
var
  Descriptor: TVariableDescriptor;
begin
  if FVariables.TryGetValue(AName, Descriptor) then
  begin
    // Check temporal dead zone for let/const
    if not Descriptor.GetInitialized and (Descriptor.DeclarationType in [dtLet, dtConst]) then
      raise TGocciaReferenceError.Create(Format('Cannot access ''%s'' before initialization', [AName]), 0, 0, '', nil);
    Result := Descriptor.Value;
  end
  else if Assigned(FParent) then
    Result := FParent.GetValue(AName)
  else
  begin
    // Strict mode: undefined variables throw ReferenceError
    raise TGocciaReferenceError.Create(Format('Undefined variable: %s', [AName]), 0, 0, '', nil);
  end;
end;

function TGocciaScope.GetThisProperty(const AName: string): TGocciaValue;
begin
  if (ThisValue is TGocciaObjectValue) then
    Result := TGocciaObjectValue(ThisValue).GetProperty(AName)
  else
    Result := TGocciaUndefinedValue.Create;
end;



function TGocciaScope.Contains(const AName: string): Boolean; inline;
begin
  Result := FVariables.ContainsKey(AName) or
    (Assigned(FParent) and FParent.Contains(AName));
end;

procedure TGocciaScope.DefineBuiltin(const AName: string; AValue: TGocciaValue);
var
  Descriptor: TVariableDescriptor;
begin
  Logger.Debug('Scope.DefineBuiltin: Defining builtin for name: %s', [AName]);
  Logger.Debug('  Kind: %s', [GetEnumName(TypeInfo(TGocciaScopeKind), Ord(FScopeKind))]);
  if FCustomLabel <> '' then
    Logger.Debug('  CustomLabel: %s', [FCustomLabel]);
  Logger.Debug('  Value type: %s', [AValue.ClassName]);
  Logger.Debug('  Self address: %d', [PtrUInt(Self)]);
  Logger.Debug('  Variables dictionary address: %d', [PtrUInt(FVariables)]);

  // DefineBuiltin allows redefinition - used for built-ins and global initialization
  Descriptor.Value := AValue;
  Descriptor.DeclarationType := dtLet;
  Descriptor.Flags := [vfWritable, vfEnumerable, vfConfigurable, vfInitialized];
  FVariables.AddOrSetValue(AName, Descriptor);
end;

// TGocciaCatchScope implementation

constructor TGocciaCatchScope.Create(AParent: TGocciaScope; const ACatchParameter: string);
begin
  inherited Create(AParent, skBlock, 'CatchBlock');
  FCatchParameter := ACatchParameter;
end;

procedure TGocciaCatchScope.AssignVariable(const AName: string; AValue: TGocciaValue);
begin
  // Surgical fix for catch parameter scopes: assignments to non-parameter variables
  // should propagate to parent scope, but catch parameters should stay for proper shadowing
  if (AName <> FCatchParameter) and (not FVariables.ContainsKey(AName)) and Assigned(FParent) then
  begin
    // This is a catch parameter scope and the variable isn't the catch parameter.
    // Delegate directly to parent to ensure assignment propagation
    FParent.AssignVariable(AName, AValue);
  end
  else
  begin
    // Either it's the catch parameter or it exists in current scope - use base behavior
    inherited AssignVariable(AName, AValue);
  end;
end;

procedure TGocciaCatchScope.DefineBuiltin(const AName: string; AValue: TGocciaValue);
begin
  // Surgical fix: Also apply to DefineBuiltin for catch parameter scopes
  // Delegate to parent for non-parameter variables, but keep catch parameters for shadowing
  if (AName <> FCatchParameter) and (not FVariables.ContainsKey(AName)) and Assigned(FParent) then
  begin
    // Check if the variable exists in parent scope - if so, assign there
    if FParent.Contains(AName) then
    begin
      FParent.AssignVariable(AName, AValue);
      Exit;
    end;
  end;

  // Use base implementation
  inherited DefineBuiltin(AName, AValue);
end;

{ TGocciaGlobalScope }

constructor TGocciaGlobalScope.Create();
begin
  inherited Create(nil, skGlobal, 'GlobalScope');
  FGlobalObject := TGocciaObjectValue.Create();
  FGlobalObject.DefineProperty('globalThis', TGocciaPropertyDescriptorData.Create(FGlobalObject, []));
end;

function TGocciaGlobalScope.GetValue(const AName: string): TGocciaValue;
begin
  if FGlobalObject.HasProperty(AName) then
    Result := FGlobalObject.GetProperty(AName)
  else
    Result := inherited GetValue(AName);
end;

{ TGocciaClassScope }

constructor TGocciaClassScope.Create(AParent: TGocciaScope; const AClassName: string);
begin
  inherited Create(AParent, skClass, AClassName);
  // FSuperClass := AParent.GetSuperClass();
end;

function TGocciaClassScope.GetSuperClass(): TGocciaObjectValue;
begin
  Result := FSuperClass;
end;

end.
