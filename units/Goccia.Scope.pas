unit Goccia.Scope;

{$I Goccia.inc}

interface

uses
  Goccia.Values.Primitives, Goccia.Values.ObjectPropertyDescriptor, Goccia.Values.ObjectValue, Goccia.Error, Goccia.Error.ThrowErrorCallback, Generics.Collections, SysUtils, TypInfo, Goccia.Interfaces, Goccia.Token;

type
  TGocciaDeclarationType = (dtUnknown, dtLet, dtConst, dtParameter);

  TLexicalBinding = record
  private
    function IsWritable: Boolean; // computed from DeclarationType
    function CanAccess: Boolean; // computed from DeclarationType and Initialized
  public
    Value: TGocciaValue;
    DeclarationType: TGocciaDeclarationType;
    Initialized: Boolean;

    property Writable: Boolean read IsWritable;
    property IsAccessible: Boolean read CanAccess;
  end;

  TGocciaScopeKind = (skUnknown, skGlobal, skFunction, skBlock, skCustom, skClass);

  TGocciaScope = class
  private
    FLexicalBindings: TDictionary<string, TLexicalBinding>;
    FParent: TGocciaScope;
    FThisValue: TGocciaValue;
    FScopeKind: TGocciaScopeKind;
    FCustomLabel: string;
    FGCMarked: Boolean;
    FOnError: TGocciaThrowErrorCallback;
  public
    constructor Create(AParent: TGocciaScope = nil; AScopeKind: TGocciaScopeKind = skUnknown; const ACustomLabel: string = ''; ACapacity: Integer = 0);
    destructor Destroy; override;
    function CreateChild(AScopeKind: TGocciaScopeKind = skUnknown; const ACustomLabel: string = ''): TGocciaScope;

    // Garbage collection support
    procedure GCMarkReferences;
    property GCMarked: Boolean read FGCMarked write FGCMarked;

    // New Define/Assign pattern
    procedure DefineLexicalBinding(const AName: string; AValue: TGocciaValue; ADeclarationType: TGocciaDeclarationType; ALine: Integer = 0; AColumn: Integer = 0);
    procedure AssignLexicalBinding(const AName: string; AValue: TGocciaValue; ALine: Integer = 0; AColumn: Integer = 0); virtual;

    // Helper methods for token-based declarations
    procedure DefineFromToken(const AName: string; AValue: TGocciaValue; ATokenType: TGocciaTokenType);

    // Core methods
    function GetLexicalBinding(const AName: string; ALine: Integer = 0; AColumn: Integer = 0): TLexicalBinding;
    function GetValue(const AName: string): TGocciaValue;

    function ContainsOwnLexicalBinding(const AName: string): Boolean; inline;
    function Contains(const AName: string): Boolean; inline;

    // TODO: DefineBuiltin is a legacy method, should be removed and replaced with DefineLexicalBinding
    procedure DefineBuiltin(const AName: string; AValue: TGocciaValue); virtual;

    property Parent: TGocciaScope read FParent;
    property ThisValue: TGocciaValue read FThisValue write FThisValue;
    function GetThisProperty(const AName: string): TGocciaValue;
    property ScopeKind: TGocciaScopeKind read FScopeKind;
    property CustomLabel: string read FCustomLabel;
    property OnError: TGocciaThrowErrorCallback read FOnError write FOnError;
  end;

  // Specialized scope for try-catch blocks with proper assignment propagation
  TGocciaCatchScope = class(TGocciaScope)
  private
    FCatchParameter: string;  // Track the catch parameter name for proper shadowing
  public
    constructor Create(AParent: TGocciaScope; const ACatchParameter: string);
    procedure AssignLexicalBinding(const AName: string; AValue: TGocciaValue; ALine: Integer = 0; AColumn: Integer = 0); override;
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

uses Goccia.Values.ClassHelper, Goccia.GarbageCollector;

{ TLexicalBinding }

function TLexicalBinding.IsWritable: Boolean;
begin
  Result := DeclarationType in [dtLet, dtParameter];
end;

function TLexicalBinding.CanAccess: Boolean;
begin
  Result := Initialized or (DeclarationType = dtParameter); // Parameters have no TDZ
end;

{ TGocciaScope }

constructor TGocciaScope.Create(AParent: TGocciaScope = nil; AScopeKind: TGocciaScopeKind = skUnknown; const ACustomLabel: string = ''; ACapacity: Integer = 0);
begin
  FScopeKind := AScopeKind;
  FCustomLabel := ACustomLabel;
  FThisValue := TGocciaUndefinedLiteralValue.UndefinedValue;
  FParent := AParent;
  if ACapacity > 0 then
    FLexicalBindings := TDictionary<string, TLexicalBinding>.Create(ACapacity)
  else
    FLexicalBindings := TDictionary<string, TLexicalBinding>.Create;

  // Inherit interpreter context from parent scope
  if Assigned(AParent) then
    FOnError := AParent.FOnError;

  if Assigned(TGocciaGC.Instance) then
    TGocciaGC.Instance.RegisterScope(Self);
end;

destructor TGocciaScope.Destroy;
var
  LexicalBinding: TLexicalBinding;
  Key: string;
begin
  if Assigned(FLexicalBindings) then
    FLexicalBindings.Free;

  // Don't free FThisValue - the scope doesn't own it, it's just a reference
  // The actual owner should handle freeing it
  if Assigned(FThisValue) then
    FThisValue := nil;

  inherited;
end;

function TGocciaScope.CreateChild(AScopeKind: TGocciaScopeKind = skUnknown; const ACustomLabel: string = ''): TGocciaScope;
begin
  Result := TGocciaScope.Create(Self, AScopeKind, ACustomLabel);
end;

procedure TGocciaScope.DefineLexicalBinding(const AName: string; AValue: TGocciaValue; ADeclarationType: TGocciaDeclarationType; ALine: Integer = 0; AColumn: Integer = 0);
var
  LexicalBinding: TLexicalBinding;
  ExistingLexicalBinding: TLexicalBinding;
begin
  // Check for redeclaration errors
  if FLexicalBindings.TryGetValue(AName, ExistingLexicalBinding) then
  begin
    // Check redeclaration rules - let/const cannot be redeclared
    case ADeclarationType of
      dtLet, dtConst:
        begin
          // let/const cannot be redeclared
          raise TGocciaSyntaxError.Create(Format('Identifier ''%s'' has already been declared', [AName]), ALine, AColumn, '', nil);
        end;
    end;
  end;

  // Set up descriptor based on declaration type
  LexicalBinding.Value := AValue;
  LexicalBinding.DeclarationType := ADeclarationType;

  case ADeclarationType of
    dtConst:
      begin
        // const: non-writable, enumerable, configurable, must be initialized
        // Note: Parser already validates that const declarations have initializers
        // This includes cases like 'const x = undefined;' which are valid
        LexicalBinding.Initialized := True;
      end;
    dtLet:
      begin
        // let: writable, enumerable, configurable
        // Once a let declaration is processed, it's immediately initialized (no TDZ after declaration)
        LexicalBinding.Initialized := True;
      end;
  else
    // Default for unknown types (built-ins) - mark as initialized
    LexicalBinding.Initialized := True;
  end;

  FLexicalBindings.AddOrSetValue(AName, LexicalBinding);
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

  DefineLexicalBinding(AName, AValue, DeclarationType);
end;

procedure TGocciaScope.AssignLexicalBinding(const AName: string; AValue: TGocciaValue; ALine: Integer = 0; AColumn: Integer = 0);
var
  LexicalBinding: TLexicalBinding;
begin
  // Try to find variable in current scope first
  if FLexicalBindings.TryGetValue(AName, LexicalBinding) then
  begin
    // Check if variable is initialized (temporal dead zone for let/const)
    if not LexicalBinding.IsAccessible then
      raise TGocciaReferenceError.Create(Format('Cannot access ''%s'' before initialization', [AName]), ALine, AColumn, '', nil);

    // Check if variable is writable
    if not LexicalBinding.Writable then
      raise TGocciaTypeError.Create(Format('Assignment to constant variable ''%s''', [AName]), ALine, AColumn, '', nil);

    // Update the value and mark as initialized
    LexicalBinding.Value := AValue;
    LexicalBinding.Initialized := True;
    FLexicalBindings.AddOrSetValue(AName, LexicalBinding);
    Exit;
  end;

  // Variable not found in current scope, try parent scope
  if Assigned(FParent) then
  begin
    FParent.AssignLexicalBinding(AName, AValue, ALine, AColumn);
    Exit;
  end;

  // Variable not found in any scope - strict mode always throws
  raise TGocciaReferenceError.Create(Format('Undefined variable: %s', [AName]), ALine, AColumn, '', nil);
end;

function TGocciaScope.GetLexicalBinding(const AName: string; ALine: Integer = 0; AColumn: Integer = 0): TLexicalBinding;
var
  LexicalBinding: TLexicalBinding;
begin
  if FLexicalBindings.TryGetValue(AName, LexicalBinding) then
  begin
    // Check temporal dead zone for let/const
    if not LexicalBinding.IsAccessible then
      raise TGocciaReferenceError.Create(Format('Cannot access ''%s'' before initialization', [AName]), ALine, AColumn, '', nil);
    Result := LexicalBinding;
  end
  else if Assigned(FParent) then
    Result := FParent.GetLexicalBinding(AName, ALine, AColumn)
  else
  begin
    // Strict mode: undefined variables throw ReferenceError
    raise TGocciaReferenceError.Create(Format('Undefined variable: %s', [AName]), ALine, AColumn, '', nil);
  end;
end;

function TGocciaScope.GetValue(const AName: string): TGocciaValue;
begin
  Result := GetLexicalBinding(AName).Value;
end;

function TGocciaScope.GetThisProperty(const AName: string): TGocciaValue;
begin
  if (ThisValue is TGocciaObjectValue) then
    Result := TGocciaObjectValue(ThisValue).GetProperty(AName)
  else
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaScope.ContainsOwnLexicalBinding(const AName: string): Boolean; inline;
begin
  Result := FLexicalBindings.ContainsKey(AName);
end;

function TGocciaScope.Contains(const AName: string): Boolean; inline;
begin
  Result := ContainsOwnLexicalBinding(AName) or
    (Assigned(FParent) and FParent.Contains(AName));
end;

procedure TGocciaScope.DefineBuiltin(const AName: string; AValue: TGocciaValue);
var
  LexicalBinding: TLexicalBinding;
begin
  // DefineBuiltin allows redefinition - used for built-ins and global initialization
  LexicalBinding.Value := AValue;
  LexicalBinding.DeclarationType := dtLet;
  LexicalBinding.Initialized := True;
  FLexicalBindings.AddOrSetValue(AName, LexicalBinding);
end;

{ TGocciaScope - GC support }

procedure TGocciaScope.GCMarkReferences;
var
  Binding: TLexicalBinding;
begin
  if FGCMarked then Exit;
  FGCMarked := True;

  // Mark parent scope
  if Assigned(FParent) then
    FParent.GCMarkReferences;

  // Mark ThisValue
  if Assigned(FThisValue) then
    FThisValue.GCMarkReferences;

  // Mark all values in bindings
  for Binding in FLexicalBindings.Values do
  begin
    if Assigned(Binding.Value) then
      Binding.Value.GCMarkReferences;
  end;
end;

// TGocciaCatchScope implementation

constructor TGocciaCatchScope.Create(AParent: TGocciaScope; const ACatchParameter: string);
begin
  inherited Create(AParent, skBlock, 'CatchBlock');
  FCatchParameter := ACatchParameter;
end;

procedure TGocciaCatchScope.AssignLexicalBinding(const AName: string; AValue: TGocciaValue; ALine: Integer = 0; AColumn: Integer = 0);
begin
  // Surgical fix for catch parameter scopes: assignments to non-parameter variables
  // should propagate to parent scope, but catch parameters should stay for proper shadowing
  if (AName <> FCatchParameter) and (not FLexicalBindings.ContainsKey(AName)) and Assigned(FParent) then
  begin
    // This is a catch parameter scope and the variable isn't the catch parameter.
    // Delegate directly to parent to ensure assignment propagation
    FParent.AssignLexicalBinding(AName, AValue, ALine, AColumn);
  end
  else
  begin
    // Either it's the catch parameter or it exists in current scope - use base behavior
    inherited AssignLexicalBinding(AName, AValue, ALine, AColumn);
  end;
end;

procedure TGocciaCatchScope.DefineBuiltin(const AName: string; AValue: TGocciaValue);
begin
  // Surgical fix: Also apply to DefineBuiltin for catch parameter scopes
  // Delegate to parent for non-parameter variables, but keep catch parameters for shadowing
  if (AName <> FCatchParameter) and (not FLexicalBindings.ContainsKey(AName)) and Assigned(FParent) then
  begin
    // Check if the variable exists in parent scope - if so, assign there
    if FParent.Contains(AName) then
    begin
      FParent.AssignLexicalBinding(AName, AValue);
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
