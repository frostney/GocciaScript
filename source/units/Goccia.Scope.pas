unit Goccia.Scope;

{$I Goccia.inc}

interface

uses
  Generics.Collections,

  Goccia.Bytecode.Chunk,
  Goccia.Error.ThrowErrorCallback,
  Goccia.GarbageCollector,
  Goccia.Modules,
  Goccia.Scope.BindingMap,
  Goccia.Token,
  Goccia.Values.Primitives;

const
  dtLet = Goccia.Scope.BindingMap.dtLet;
  dtConst = Goccia.Scope.BindingMap.dtConst;
  dtParameter = Goccia.Scope.BindingMap.dtParameter;
  dtVar = Goccia.Scope.BindingMap.dtVar;

type
  TGocciaScopeKind = (skUnknown, skGlobal, skFunction, skBlock, skCustom, skClass, skModule);

  TGocciaScope = class(TGCManagedObject)
  private
    FLexicalBindings: TGocciaScopeBindingMap;
    FVarBindings: TGocciaScopeBindingMap;
    FParent: TGocciaScope;
    FThisValue: TGocciaValue;
    FScopeKind: TGocciaScopeKind;
    FCustomLabel: string;
    FOnError: TGocciaThrowErrorCallback;
    FLoadModule: TLoadModuleCallback;
    FStrictTypes: Boolean;
  protected
    function GetThisValue: TGocciaValue; virtual;
    function GetOwningClass: TGocciaValue; virtual;
    function GetSuperClass: TGocciaValue; virtual;
  public
    constructor Create(const AParent: TGocciaScope = nil; const AScopeKind: TGocciaScopeKind = skUnknown; const ACustomLabel: string = ''; const ACapacity: Integer = 0);
    destructor Destroy; override;
    function CreateChild(const AScopeKind: TGocciaScopeKind = skUnknown; const ACustomLabel: string = ''; const ACapacity: Integer = 0): TGocciaScope;

    procedure MarkReferences; override;

    // New Define/Assign pattern
    procedure DefineLexicalBinding(const AName: string; const AValue: TGocciaValue; const ADeclarationType: TGocciaDeclarationType; const ALine: Integer = 0; const AColumn: Integer = 0);
    procedure AssignBinding(const AName: string; const AValue: TGocciaValue; const ALine: Integer = 0; const AColumn: Integer = 0); virtual;
    procedure ForceUpdateBinding(const AName: string; const AValue: TGocciaValue);

    // Var binding support (separate map on function/module/global scopes)
    procedure DefineVariableBinding(const AName: string; const AValue: TGocciaValue;
      const AHasInitializer: Boolean);
    function ContainsOwnVarBinding(const AName: string): Boolean;

    // Strict-types enforcement: record the declared TGocciaLocalType for a
    // local binding so AssignBinding throws a TypeError on incompatible
    // assignments.  Looks up the binding by name on this scope only.
    procedure SetOwnBindingTypeHint(const AName: string;
      const ATypeHint: TGocciaLocalType);

    // Helper methods for token-based declarations
    procedure DefineFromToken(const AName: string; const AValue: TGocciaValue; const ATokenType: TGocciaTokenType);

    // Core methods
    function GetBinding(const AName: string; const ALine: Integer = 0; const AColumn: Integer = 0): TLexicalBinding;
    function GetValue(const AName: string): TGocciaValue; inline;

    function ResolveIdentifier(const AName: string): TGocciaValue; inline;
    function ContainsOwnLexicalBinding(const AName: string): Boolean; inline;
    function Contains(const AName: string): Boolean; inline;
    function GetOwnBindingNames: TGocciaStringArray; inline;

    // Walk the parent chain to find the nearest function/module/global scope (for var hoisting)
    function FindFunctionOrModuleScope: TGocciaScope;

    // Walk the parent chain to find the nearest owning class / superclass
    function FindOwningClass: TGocciaValue;
    function FindSuperClass: TGocciaValue;

    { Read StrictTypes from the root (global) scope so that
      TGocciaEngine.SetStrictTypes propagates uniformly to closures
      whose lexical scope was created before the setter ran.  Each
      scope's own FStrictTypes is still inherited from its parent at
      creation, but it is only the root's value that the engine setter
      keeps in sync. }
    function EffectiveStrictTypes: Boolean;

    property Parent: TGocciaScope read FParent;
    property ThisValue: TGocciaValue read FThisValue write FThisValue;
    property ScopeKind: TGocciaScopeKind read FScopeKind;
    property CustomLabel: string read FCustomLabel;
    property OnError: TGocciaThrowErrorCallback read FOnError write FOnError;
    property LoadModule: TLoadModuleCallback read FLoadModule write FLoadModule;
    { Strict-types enforcement flag.  Inherited from parent at scope
      creation so nested closures observe the same setting as the
      surrounding lexical scope.  For live engine state use
      EffectiveStrictTypes, which always reads the root scope. }
    property StrictTypes: Boolean read FStrictTypes write FStrictTypes;
  end;

  // Root scope with no parent -- used by the interpreter/engine
  TGocciaGlobalScope = class(TGocciaScope)
  protected
    function GetThisValue: TGocciaValue; override;
  public
    constructor Create;
  end;

  // Marker class for function call scopes
  TGocciaCallScope = class(TGocciaScope)
  protected
    function GetThisValue: TGocciaValue; override;
  public
    constructor Create(const AParent: TGocciaScope; const AFunctionName: string; const ACapacity: Integer = 0);
  end;

  // Function call scope for class methods -- carries super and owning class
  TGocciaMethodCallScope = class(TGocciaCallScope)
  private
    FSuperClass: TGocciaValue;
    FOwningClass: TGocciaValue;
  protected
    function GetOwningClass: TGocciaValue; override;
    function GetSuperClass: TGocciaValue; override;
  public
    constructor Create(const AParent: TGocciaScope; const AFunctionName: string;
      const ASuperClass, AOwningClass: TGocciaValue; const ACapacity: Integer = 0);
    procedure MarkReferences; override;
    property SuperClass: TGocciaValue read FSuperClass;
    property OwningClass: TGocciaValue read FOwningClass;
  end;

  // Scope for instance property initialization -- carries owning class
  TGocciaClassInitScope = class(TGocciaScope)
  private
    FOwningClass: TGocciaValue;
  protected
    function GetThisValue: TGocciaValue; override;
    function GetOwningClass: TGocciaValue; override;
  public
    constructor Create(const AParent: TGocciaScope; const AOwningClass: TGocciaValue);
    procedure MarkReferences; override;
    property OwningClass: TGocciaValue read FOwningClass;
  end;

  // Specialized scope for try-catch blocks with proper assignment propagation
  TGocciaCatchScope = class(TGocciaScope)
  private
    FCatchParameter: string;  // Track the catch parameter name for proper shadowing
  public
    constructor Create(const AParent: TGocciaScope; const ACatchParameter: string);
    procedure AssignBinding(const AName: string; const AValue: TGocciaValue; const ALine: Integer = 0; const AColumn: Integer = 0); override;
  end;

  TGocciaScopeList = TObjectList<TGocciaScope>;




implementation

uses
  SysUtils,

  Goccia.Error,
  Goccia.Error.Messages,
  Goccia.Error.Suggestions,
  Goccia.Keywords.Reserved,
  Goccia.Types.Enforcement;

{ TGocciaScope }

constructor TGocciaScope.Create(const AParent: TGocciaScope = nil; const AScopeKind: TGocciaScopeKind = skUnknown; const ACustomLabel: string = ''; const ACapacity: Integer = 0);
begin
  FScopeKind := AScopeKind;
  FCustomLabel := ACustomLabel;
  FThisValue := TGocciaUndefinedLiteralValue.UndefinedValue;
  FParent := AParent;
  FLexicalBindings := TGocciaScopeBindingMap.Create(ACapacity);

  if Assigned(AParent) then
  begin
    FOnError := AParent.FOnError;
    FLoadModule := AParent.FLoadModule;
    FStrictTypes := AParent.FStrictTypes;
  end;

  if Assigned(TGarbageCollector.Instance) then
    TGarbageCollector.Instance.RegisterObject(Self);
end;

destructor TGocciaScope.Destroy;
begin
  if Assigned(TGarbageCollector.Instance) then
    TGarbageCollector.Instance.UnregisterObject(Self);

  if Assigned(FLexicalBindings) then
    FLexicalBindings.Free;
  if Assigned(FVarBindings) then
    FVarBindings.Free;

  if Assigned(FThisValue) then
    FThisValue := nil;

  inherited;
end;

function TGocciaScope.CreateChild(const AScopeKind: TGocciaScopeKind = skUnknown; const ACustomLabel: string = ''; const ACapacity: Integer = 0): TGocciaScope;
begin
  Result := TGocciaScope.Create(Self, AScopeKind, ACustomLabel, ACapacity);
  // Inherit ThisValue so that block scopes can resolve `this` correctly
  Result.FThisValue := FThisValue;
end;

function TGocciaScope.GetThisValue: TGocciaValue;
begin
  Result := nil;
end;

function TGocciaScope.GetOwningClass: TGocciaValue;
begin
  Result := nil;
end;

function TGocciaScope.GetSuperClass: TGocciaValue;
begin
  Result := nil;
end;

function TGocciaScope.FindFunctionOrModuleScope: TGocciaScope;
begin
  Result := Self;
  while Assigned(Result) do
  begin
    if Result.ScopeKind in [skFunction, skGlobal, skModule] then
      Exit;
    Result := Result.FParent;
  end;
  // Fallback: return self if no function/module scope found
  Result := Self;
end;

function TGocciaScope.EffectiveStrictTypes: Boolean;
var
  Root: TGocciaScope;
begin
  Root := Self;
  while Assigned(Root.FParent) do
    Root := Root.FParent;
  Result := Root.FStrictTypes;
end;

function TGocciaScope.FindOwningClass: TGocciaValue;
var
  Current: TGocciaScope;
begin
  Current := Self;
  while Assigned(Current) do
  begin
    Result := Current.GetOwningClass;
    if Assigned(Result) then Exit;
    Current := Current.FParent;
  end;
  Result := nil;
end;

function TGocciaScope.FindSuperClass: TGocciaValue;
var
  Current: TGocciaScope;
begin
  Current := Self;
  while Assigned(Current) do
  begin
    Result := Current.GetSuperClass;
    if Assigned(Result) then Exit;
    Current := Current.FParent;
  end;
  Result := nil;
end;

procedure TGocciaScope.DefineLexicalBinding(const AName: string; const AValue: TGocciaValue; const ADeclarationType: TGocciaDeclarationType; const ALine: Integer = 0; const AColumn: Integer = 0);
var
  LexicalBinding: TLexicalBinding;
  ExistingLexicalBinding: TLexicalBinding;
  Error: TGocciaSyntaxError;
begin
  // Check for redeclaration errors
  if FLexicalBindings.TryGetValue(AName, ExistingLexicalBinding) then
  begin
    // Check redeclaration rules - let/const cannot be redeclared
    case ADeclarationType of
      dtLet, dtConst:
        begin
          Error := TGocciaSyntaxError.Create(Format(SErrorIdentifierAlreadyDeclared, [AName]), ALine, AColumn, '', nil);
          Error.Suggestion := SSuggestAlreadyDeclared;
          raise Error;
        end;
    end;
  end;

  // Set up descriptor based on declaration type
  LexicalBinding.Value := AValue;
  LexicalBinding.DeclarationType := ADeclarationType;

  // All declaration types are immediately initialized:
  // - dtConst: parser validates initializers; 'const x = undefined;' is valid
  // - dtLet: no TDZ after declaration statement is processed
  // - dtParameter: parameters have no TDZ
  LexicalBinding.Initialized := True;
  LexicalBinding.TypeHint := sltUntyped;

  FLexicalBindings.AddOrSetValue(AName, LexicalBinding);
end;

procedure TGocciaScope.DefineFromToken(const AName: string; const AValue: TGocciaValue; const ATokenType: TGocciaTokenType);
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

procedure TGocciaScope.ForceUpdateBinding(const AName: string; const AValue: TGocciaValue);
var
  LexicalBinding: TLexicalBinding;
begin
  if FLexicalBindings.TryGetValue(AName, LexicalBinding) then
  begin
    LexicalBinding.Value := AValue;
    LexicalBinding.Initialized := True;
    FLexicalBindings.AddOrSetValue(AName, LexicalBinding);
  end;
end;

procedure TGocciaScope.DefineVariableBinding(const AName: string; const AValue: TGocciaValue;
  const AHasInitializer: Boolean);
var
  TargetScope: TGocciaScope;
  Binding: TLexicalBinding;
begin
  TargetScope := FindFunctionOrModuleScope;
  // Lazily allocate the var bindings map
  if not Assigned(TargetScope.FVarBindings) then
    TargetScope.FVarBindings := TGocciaScopeBindingMap.Create;

  if TargetScope.FVarBindings.TryGetValue(AName, Binding) then
  begin
    // Redeclaration: only update if there's a real initializer
    if AHasInitializer then
    begin
      Binding.Value := AValue;
      TargetScope.FVarBindings.AddOrSetValue(AName, Binding);
    end;
  end
  else
  begin
    // First declaration: create the binding
    Binding.Value := AValue;
    Binding.DeclarationType := dtVar;
    Binding.Initialized := True;
    Binding.TypeHint := sltUntyped;
    TargetScope.FVarBindings.AddOrSetValue(AName, Binding);
  end;
end;

function TGocciaScope.ContainsOwnVarBinding(const AName: string): Boolean;
begin
  Result := Assigned(FVarBindings) and FVarBindings.ContainsKey(AName);
end;

procedure TGocciaScope.SetOwnBindingTypeHint(const AName: string;
  const ATypeHint: TGocciaLocalType);
var
  Binding: TLexicalBinding;
begin
  if FLexicalBindings.TryGetValue(AName, Binding) then
  begin
    Binding.TypeHint := ATypeHint;
    FLexicalBindings.AddOrSetValue(AName, Binding);
    Exit;
  end;
  if Assigned(FVarBindings) and FVarBindings.TryGetValue(AName, Binding) then
  begin
    Binding.TypeHint := ATypeHint;
    FVarBindings.AddOrSetValue(AName, Binding);
  end;
end;

procedure TGocciaScope.AssignBinding(const AName: string; const AValue: TGocciaValue; const ALine: Integer = 0; const AColumn: Integer = 0);
var
  LexicalBinding: TLexicalBinding;
begin
  // Try to find variable in current scope first
  if FLexicalBindings.TryGetValue(AName, LexicalBinding) then
  begin
    // Check if variable is initialized (temporal dead zone for let/const)
    if not LexicalBinding.IsAccessible then
      raise TGocciaReferenceError.Create(
        Format(SErrorCannotAccessBeforeInit, [AName]),
        ALine, AColumn, '', nil,
        SSuggestTemporalDeadZone);

    // Check if variable is writable
    if not LexicalBinding.Writable then
      raise TGocciaTypeError.Create(
        Format(SErrorAssignToConstant, [AName]),
        ALine, AColumn, '', nil,
        SSuggestUseLetNotConst);

    // Strict-types enforcement: when this binding has a recorded type
    // hint, throw a TypeError if the assigned value does not match.
    if LexicalBinding.TypeHint <> sltUntyped then
      EnforceStrictType(AValue, LexicalBinding.TypeHint);

    // Update the value and mark as initialized
    LexicalBinding.Value := AValue;
    LexicalBinding.Initialized := True;
    FLexicalBindings.AddOrSetValue(AName, LexicalBinding);
    Exit;
  end;

  // Check var bindings on this scope
  if Assigned(FVarBindings) and FVarBindings.TryGetValue(AName, LexicalBinding) then
  begin
    if LexicalBinding.TypeHint <> sltUntyped then
      EnforceStrictType(AValue, LexicalBinding.TypeHint);
    LexicalBinding.Value := AValue;
    FVarBindings.AddOrSetValue(AName, LexicalBinding);
    Exit;
  end;

  // Variable not found in current scope, try parent scope
  if Assigned(FParent) then
  begin
    FParent.AssignBinding(AName, AValue, ALine, AColumn);
    Exit;
  end;

  // Variable not found in any scope - strict mode always throws
  raise TGocciaReferenceError.Create(
    Format(SErrorUndefinedVariable, [AName]),
    ALine, AColumn, '', nil,
    SSuggestDeclareBeforeUse);
end;

function TGocciaScope.GetBinding(const AName: string; const ALine: Integer = 0; const AColumn: Integer = 0): TLexicalBinding;
var
  LexicalBinding: TLexicalBinding;
begin
  if FLexicalBindings.TryGetValue(AName, LexicalBinding) then
  begin
    if not LexicalBinding.IsAccessible then
      raise TGocciaReferenceError.Create(
        Format(SErrorCannotAccessBeforeInit, [AName]),
        ALine, AColumn, '', nil,
        SSuggestTemporalDeadZone);
    Result := LexicalBinding;
  end
  else if Assigned(FVarBindings) and FVarBindings.TryGetValue(AName, LexicalBinding) then
    Result := LexicalBinding
  else if Assigned(FParent) then
    Result := FParent.GetBinding(AName, ALine, AColumn)
  else
    raise TGocciaReferenceError.Create(
      Format(SErrorUndefinedVariable, [AName]),
      ALine, AColumn, '', nil,
      SSuggestDeclareBeforeUse);
end;

function TGocciaScope.GetValue(const AName: string): TGocciaValue;
begin
  Result := GetBinding(AName).Value;
end;

function TGocciaScope.ResolveIdentifier(const AName: string): TGocciaValue;
begin
  if AName = KEYWORD_THIS then
    Result := FThisValue
  else
    Result := GetValue(AName);
end;

function TGocciaScope.ContainsOwnLexicalBinding(const AName: string): Boolean; inline;
begin
  Result := FLexicalBindings.ContainsKey(AName);
end;

function TGocciaScope.Contains(const AName: string): Boolean; inline;
begin
  Result := ContainsOwnLexicalBinding(AName) or
    ContainsOwnVarBinding(AName) or
    (Assigned(FParent) and FParent.Contains(AName));
end;

function TGocciaScope.GetOwnBindingNames: TGocciaStringArray;
begin
  Result := FLexicalBindings.Keys;
end;

{ TGocciaScope - GC support }

procedure TGocciaScope.MarkReferences;
var
  Pair: TGocciaScopeBindingMap.TKeyValuePair;
begin
  if GCMarked then Exit;
  inherited;

  if Assigned(FParent) then
    FParent.MarkReferences;

  if Assigned(FThisValue) then
    FThisValue.MarkReferences;

  for Pair in FLexicalBindings do
    if Assigned(Pair.Value.Value) then
      Pair.Value.Value.MarkReferences;

  if Assigned(FVarBindings) then
    for Pair in FVarBindings do
      if Assigned(Pair.Value.Value) then
        Pair.Value.Value.MarkReferences;
end;

{ TGocciaGlobalScope }

constructor TGocciaGlobalScope.Create;
begin
  inherited Create(nil, skGlobal, 'GlobalScope');
  FThisValue := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaGlobalScope.GetThisValue: TGocciaValue;
begin
  Result := FThisValue;
end;

{ TGocciaCallScope }

constructor TGocciaCallScope.Create(const AParent: TGocciaScope; const AFunctionName: string; const ACapacity: Integer = 0);
begin
  inherited Create(AParent, skFunction, AFunctionName, ACapacity);
end;

function TGocciaCallScope.GetThisValue: TGocciaValue;
begin
  Result := FThisValue;
end;

{ TGocciaMethodCallScope }

constructor TGocciaMethodCallScope.Create(const AParent: TGocciaScope; const AFunctionName: string;
  const ASuperClass, AOwningClass: TGocciaValue; const ACapacity: Integer = 0);
begin
  inherited Create(AParent, AFunctionName, ACapacity);
  FSuperClass := ASuperClass;
  FOwningClass := AOwningClass;
end;

function TGocciaMethodCallScope.GetOwningClass: TGocciaValue;
begin
  Result := FOwningClass;
end;

function TGocciaMethodCallScope.GetSuperClass: TGocciaValue;
begin
  Result := FSuperClass;
end;

procedure TGocciaMethodCallScope.MarkReferences;
begin
  inherited;
  if Assigned(FSuperClass) then
    FSuperClass.MarkReferences;
  if Assigned(FOwningClass) then
    FOwningClass.MarkReferences;
end;

{ TGocciaClassInitScope }

constructor TGocciaClassInitScope.Create(const AParent: TGocciaScope; const AOwningClass: TGocciaValue);
begin
  inherited Create(AParent, skBlock, 'ClassInit');
  FOwningClass := AOwningClass;
end;

function TGocciaClassInitScope.GetThisValue: TGocciaValue;
begin
  Result := FThisValue;
end;

function TGocciaClassInitScope.GetOwningClass: TGocciaValue;
begin
  Result := FOwningClass;
end;

procedure TGocciaClassInitScope.MarkReferences;
begin
  inherited;
  if Assigned(FOwningClass) then
    FOwningClass.MarkReferences;
end;

{ TGocciaCatchScope }

constructor TGocciaCatchScope.Create(const AParent: TGocciaScope; const ACatchParameter: string);
begin
  inherited Create(AParent, skBlock, 'CatchBlock');
  FCatchParameter := ACatchParameter;
end;

procedure TGocciaCatchScope.AssignBinding(const AName: string; const AValue: TGocciaValue; const ALine: Integer = 0; const AColumn: Integer = 0);
begin
  // Surgical fix for catch parameter scopes: assignments to non-parameter variables
  // should propagate to parent scope, but catch parameters should stay for proper shadowing
  if (AName <> FCatchParameter) and (not FLexicalBindings.ContainsKey(AName)) and Assigned(FParent) then
  begin
    // This is a catch parameter scope and the variable isn't the catch parameter.
    // Delegate directly to parent to ensure assignment propagation
    FParent.AssignBinding(AName, AValue, ALine, AColumn);
  end
  else
  begin
    // Either it's the catch parameter or it exists in current scope - use base behavior
    inherited AssignBinding(AName, AValue, ALine, AColumn);
  end;
end;

end.
