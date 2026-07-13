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
  Goccia.Values.ObjectValue,
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
    FGlobalVarNames: TDictionary<string, Boolean>;
    FParent: TGocciaScope;
    FThisValue: TGocciaValue;
    FScopeKind: TGocciaScopeKind;
    FCustomLabel: string;
    FOnError: TGocciaThrowErrorCallback;
    FLoadModule: TLoadModuleCallback;
    FLoadModuleSource: TLoadModuleSourceCallback;
    FLoadDeferredModule: TLoadDeferredModuleCallback;
    FStrictTypes: Boolean;
    FNonStrictMode: Boolean;
    FArgumentsObjectEnabled: Boolean;
    FRejectArgumentsReferenceInDirectEval: Boolean;
    // Shared tail of TryGetBinding / TryGetBindingValueFillCache:
    // resolution after the own lexical map has already been consulted.
    function IsGlobalBuiltInObjectBinding(
      const ALexicalBinding: TLexicalBinding): Boolean;
    function CanSetGlobalObjectProperty(const AName: string): Boolean;
    function TryGetGlobalBuiltInObjectBinding(const AName: string;
      const ALexicalBinding: TLexicalBinding;
      out ABinding: TLexicalBinding): Boolean;
    function TryGetBindingSkipLexical(const AName: string;
      out ABinding: TLexicalBinding; const ALine: Integer = 0;
      const AColumn: Integer = 0): Boolean;
    procedure RegisterGlobalVarDeclaration(const AName: string);
    procedure RaiseBindingNotInitialized(const AName: string;
      const ALine, AColumn: Integer);
    procedure RaiseUndefinedVariable(const AName: string;
      const ALine, AColumn: Integer);
  protected
    function GetThisValue: TGocciaValue; virtual;
    function GetOwningClass: TGocciaValue; virtual;
    function GetSuperClass: TGocciaValue; virtual;
    function GetSuperConstructor: TGocciaValue; virtual;
    function GetNewTarget: TGocciaValue; virtual;
    function IsFunctionBoundary: Boolean; virtual;
  public
    constructor Create(const AParent: TGocciaScope = nil; const AScopeKind: TGocciaScopeKind = skUnknown; const ACustomLabel: string = ''; const ACapacity: Integer = 0);
    destructor Destroy; override;
    function CreateChild(const AScopeKind: TGocciaScopeKind = skUnknown; const ACustomLabel: string = ''; const ACapacity: Integer = 0): TGocciaScope;

    procedure MarkReferences; override;

    // New Define/Assign pattern
    procedure PredeclareLexicalBinding(const AName: string; const ADeclarationType: TGocciaDeclarationType; const ALine: Integer = 0; const AColumn: Integer = 0);
    procedure DefineLexicalBinding(const AName: string; const AValue: TGocciaValue; const ADeclarationType: TGocciaDeclarationType; const ABuiltIn: Boolean = False; const ALine: Integer = 0; const AColumn: Integer = 0);
    procedure AssignBinding(const AName: string; const AValue: TGocciaValue;
      const ALine: Integer = 0; const AColumn: Integer = 0;
      const ANonStrictMode: Boolean = False); virtual;
    procedure SetOwnMutableBinding(const AName: string;
      const AValue: TGocciaValue; const AStrict: Boolean);
    procedure ForceUpdateBinding(const AName: string; const AValue: TGocciaValue);
    procedure MarkGlobalObjectBackedBinding(const AName: string);
    function IsGlobalObjectBackedBinding(const AName: string): Boolean;

    // Var binding support (separate map on function/module/global scopes)
    procedure DefineVariableBinding(const AName: string; const AValue: TGocciaValue;
      const AHasInitializer: Boolean; const ACanDelete: Boolean = False);
    function ContainsOwnVarBinding(const AName: string): Boolean;
    function ContainsVarEnvironmentBinding(const AName: string): Boolean; virtual;
    function HasLexicalDeclaration(const AName: string): Boolean;
    function HasRestrictedGlobalProperty(const AName: string): Boolean;
    function CanDeclareGlobalVar(const AName: string): Boolean;
    function CanDeclareGlobalFunction(const AName: string): Boolean;
    procedure CreateGlobalVarBinding(const AName: string;
      const ACanDelete: Boolean);
    procedure CreateGlobalFunctionBinding(const AName: string;
      const AValue: TGocciaValue; const ACanDelete: Boolean);

    // Strict-types enforcement: record the declared TGocciaLocalType for a
    // local binding so AssignBinding throws a TypeError on incompatible
    // assignments.  Looks up the binding by name on this scope only.
    procedure SetOwnBindingTypeHint(const AName: string;
      const ATypeHint: TGocciaLocalType);

    // Helper methods for token-based declarations
    procedure DefineFromToken(const AName: string; const AValue: TGocciaValue; const ATokenType: TGocciaTokenType);

    // Core methods.  TryGetBinding is the single source of truth for read
    // resolution (TDZ still raises; False when unbound); GetBinding wraps it
    // with the unbound-name raise, and TryGetBindingValue extracts the value.
    function TryGetBinding(const AName: string; out ABinding: TLexicalBinding;
      const ALine: Integer = 0; const AColumn: Integer = 0): Boolean; virtual;
    // Own-scope read (own lexical, then own var) with no virtual dispatch
    // and no parent walk — for adapter scopes that must read their own
    // storage even when their TryGetBinding override resolves elsewhere
    // first (e.g. direct-eval scopes whose override consults with-object
    // bindings before the scope's own var environment).
    function TryGetOwnBinding(const AName: string;
      out ABinding: TLexicalBinding): Boolean;
    function GetBinding(const AName: string; const ALine: Integer = 0; const AColumn: Integer = 0): TLexicalBinding; virtual;
    function GetValue(const AName: string): TGocciaValue; virtual;
    function TryGetBindingValue(const AName: string;
      out AValue: TGocciaValue): Boolean;
    // Inline-cache fast path: re-read an own lexical binding by the entry
    // index obtained from TryGetBindingValueFillCache, validated against the
    // binding map's entry version.  TDZ still raises.  Returns False on any
    // stale cache so the caller can fall back to the named lookup.
    function TryGetLexicalValueAt(const AEntryIndex: Integer;
      const AVersion: Cardinal; out AValue: TGocciaValue): Boolean; inline;
    // Named lookup that reports the own-lexical entry index and map version
    // for inline caching.  AEntryIndex is -1 when the value was resolved
    // through the global this-object, var bindings, or the parent chain —
    // those resolutions are not cacheable.
    function TryGetBindingValueFillCache(const AName: string;
      out AEntryIndex: Integer; out AVersion: Cardinal;
      out AValue: TGocciaValue): Boolean;
    // Single source of truth for assignment resolution (TDZ, const, and
    // strict-type errors still raise); returns False when the name is
    // unbound anywhere on the chain and never creates a binding.
    // AssignBinding wraps it with the sloppy-global-create / raise outcome.
    function TryAssignExistingBinding(const AName: string;
      const AValue: TGocciaValue; const ANonStrictMode: Boolean = False;
      const ALine: Integer = 0; const AColumn: Integer = 0): Boolean; virtual;
    function DeleteBinding(const AName: string): Boolean; virtual;

    function ResolveIdentifier(const AName: string): TGocciaValue; virtual;
    procedure ResolveIdentifierReference(const AName: string;
      out AValue, AThisValue: TGocciaValue; const ALine: Integer = 0;
      const AColumn: Integer = 0); virtual;
    procedure ResolveAssignmentTarget(const AName: string;
      out AObjectBinding: TGocciaObjectValue; out AScopeBinding: TGocciaScope); virtual;
    function ContainsOwnLexicalBinding(const AName: string): Boolean; inline;
    function IsBuiltInBinding(const AName: string): Boolean;
    function Contains(const AName: string): Boolean; virtual;
    function GetOwnBindingNames: TGocciaStringArray; virtual;
    function GetOwnVarBindingNames: TGocciaStringArray; virtual;

    // Walk the parent chain to find the nearest function/module/global scope (for var hoisting)
    function FindFunctionOrModuleScope: TGocciaScope;

    // Walk the parent chain to find the nearest owning class / superclass
    function FindOwningClass: TGocciaValue;
    function FindOwningClassAfter(const AAfter: TGocciaValue): TGocciaValue;
    function FindSuperClass: TGocciaValue;
    function FindSuperConstructor: TGocciaValue;
    function FindNewTarget: TGocciaValue;
    function MarkSuperConstructorCalled: Boolean; virtual;

    { Read StrictTypes from the root (global) scope so that
      TGocciaEngine.SetStrictTypes propagates uniformly to closures
      whose lexical scope was created before the setter ran.  Each
      scope's own FStrictTypes is still inherited from its parent at
      creation, but it is only the root's value that the engine setter
      keeps in sync. }
    function EffectiveStrictTypes: Boolean;
    function EffectiveNonStrictMode: Boolean;
    function EffectiveArgumentsObjectEnabled: Boolean;

    property Parent: TGocciaScope read FParent;
    property ThisValue: TGocciaValue read FThisValue write FThisValue;
    property ScopeKind: TGocciaScopeKind read FScopeKind;
    property CustomLabel: string read FCustomLabel;
    property OnError: TGocciaThrowErrorCallback read FOnError write FOnError;
    property LoadModule: TLoadModuleCallback read FLoadModule write FLoadModule;
    property LoadModuleSource: TLoadModuleSourceCallback
      read FLoadModuleSource write FLoadModuleSource;
    property LoadDeferredModule: TLoadDeferredModuleCallback
      read FLoadDeferredModule write FLoadDeferredModule;
    { Strict-types enforcement flag.  Inherited from parent at scope
      creation so nested closures observe the same setting as the
      surrounding lexical scope.  For live engine state use
      EffectiveStrictTypes, which always reads the root scope. }
    property StrictTypes: Boolean read FStrictTypes write FStrictTypes;
    property NonStrictMode: Boolean read FNonStrictMode write FNonStrictMode;
    property ArgumentsObjectEnabled: Boolean
      read FArgumentsObjectEnabled write FArgumentsObjectEnabled;
    property RejectArgumentsReferenceInDirectEval: Boolean read FRejectArgumentsReferenceInDirectEval write FRejectArgumentsReferenceInDirectEval;
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
  private
    FNewTarget: TGocciaValue;
  protected
    function GetThisValue: TGocciaValue; override;
    function GetNewTarget: TGocciaValue; override;
    function IsFunctionBoundary: Boolean; override;
  public
    constructor Create(const AParent: TGocciaScope; const AFunctionName: string; const ACapacity: Integer = 0);
    procedure MarkReferences; override;
    property NewTarget: TGocciaValue read FNewTarget write FNewTarget;
  end;

  // ES2026 named function-expression environment. The function name is an
  // immutable binding, but not a strict binding, so sloppy assignment is a
  // no-op while strict assignment still throws.
  TGocciaFunctionNameScope = class(TGocciaScope)
  private
    FFunctionName: string;
  public
    constructor Create(const AParent: TGocciaScope;
      const AFunctionName: string);
    procedure DefineFunctionNameBinding(const AValue: TGocciaValue);
    function TryAssignExistingBinding(const AName: string;
      const AValue: TGocciaValue; const ANonStrictMode: Boolean = False;
      const ALine: Integer = 0; const AColumn: Integer = 0): Boolean; override;
  end;

  // Arrow function call scope -- transparent to Find* scope walks
  TGocciaArrowCallScope = class(TGocciaCallScope)
  protected
    function IsFunctionBoundary: Boolean; override;
  public
    constructor Create(const AParent: TGocciaScope; const AFunctionName: string; const ACapacity: Integer = 0);
  end;

  // Function call scope for class methods -- carries super and owning class
  TGocciaMethodCallScope = class(TGocciaCallScope)
  private
    FSuperClass: TGocciaValue;
    FOwningClass: TGocciaValue;
    FNewTarget: TGocciaValue;
    FSuperConstructorCalled: Boolean;
  protected
    function GetOwningClass: TGocciaValue; override;
    function GetSuperClass: TGocciaValue; override;
    function GetSuperConstructor: TGocciaValue; override;
    function GetNewTarget: TGocciaValue; override;
    function MarkSuperConstructorCalled: Boolean; override;
  public
    constructor Create(const AParent: TGocciaScope; const AFunctionName: string;
      const ASuperClass, AOwningClass: TGocciaValue; const ACapacity: Integer = 0);
    procedure MarkReferences; override;
    property SuperClass: TGocciaValue read FSuperClass;
    property OwningClass: TGocciaValue read FOwningClass;
    property NewTarget: TGocciaValue read FNewTarget write FNewTarget;
    property SuperConstructorCalled: Boolean
      read FSuperConstructorCalled write FSuperConstructorCalled;
  end;

  // Scope for instance property initialization -- carries owning class
  TGocciaClassInitScope = class(TGocciaScope)
  private
    FOwningClass: TGocciaValue;
  protected
    function GetThisValue: TGocciaValue; override;
    function GetOwningClass: TGocciaValue; override;
    function GetSuperClass: TGocciaValue; override;
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
    function TryAssignExistingBinding(const AName: string;
      const AValue: TGocciaValue; const ANonStrictMode: Boolean = False;
      const ALine: Integer = 0; const AColumn: Integer = 0): Boolean; override;
  end;

  TGocciaWithScope = class(TGocciaScope)
  private
    FBindingObject: TGocciaObjectValue;
    function HasObjectBinding(const AName: string): Boolean;
  public
    constructor Create(const AParent: TGocciaScope;
      const ABindingObject: TGocciaObjectValue);
    procedure MarkReferences; override;
    function TryGetBinding(const AName: string; out ABinding: TLexicalBinding;
      const ALine: Integer = 0; const AColumn: Integer = 0): Boolean; override;
    function TryAssignExistingBinding(const AName: string;
      const AValue: TGocciaValue; const ANonStrictMode: Boolean = False;
      const ALine: Integer = 0; const AColumn: Integer = 0): Boolean; override;
    function DeleteBinding(const AName: string): Boolean; override;
    procedure ResolveIdentifierReference(const AName: string;
      out AValue, AThisValue: TGocciaValue; const ALine: Integer = 0;
      const AColumn: Integer = 0); override;
    procedure ResolveAssignmentTarget(const AName: string;
      out AObjectBinding: TGocciaObjectValue; out AScopeBinding: TGocciaScope); override;
    function Contains(const AName: string): Boolean; override;
    property BindingObject: TGocciaObjectValue read FBindingObject;
  end;

  TGocciaScopeList = TObjectList<TGocciaScope>;




implementation

uses
  SysUtils,

  Goccia.Constants.PropertyNames,
  Goccia.Error,
  Goccia.Error.Messages,
  Goccia.Error.Suggestions,
  Goccia.Keywords.Reserved,
  Goccia.Types.Enforcement,
  Goccia.Values.ClassValue,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.SymbolValue;

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
    FLoadModuleSource := AParent.FLoadModuleSource;
    FLoadDeferredModule := AParent.FLoadDeferredModule;
    FStrictTypes := AParent.FStrictTypes;
    FNonStrictMode := AParent.FNonStrictMode;
    FArgumentsObjectEnabled := AParent.FArgumentsObjectEnabled;
    FRejectArgumentsReferenceInDirectEval :=
      AParent.FRejectArgumentsReferenceInDirectEval;
  end;

  if Assigned(TGarbageCollector.Instance) then
    TGarbageCollector.Instance.RegisterObject(Self);
end;

destructor TGocciaScope.Destroy;
begin
  if Assigned(FLexicalBindings) then
    FLexicalBindings.Free;
  if Assigned(FVarBindings) then
    FVarBindings.Free;
  if Assigned(FGlobalVarNames) then
    FGlobalVarNames.Free;

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

procedure TGocciaScope.RegisterGlobalVarDeclaration(const AName: string);
begin
  if FScopeKind <> skGlobal then
    Exit;
  if not Assigned(FGlobalVarNames) then
    FGlobalVarNames := TDictionary<string, Boolean>.Create;
  FGlobalVarNames.AddOrSetValue(AName, True);
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

function TGocciaScope.GetSuperConstructor: TGocciaValue;
begin
  Result := nil;
end;

function TGocciaScope.GetNewTarget: TGocciaValue;
begin
  Result := nil;
end;

function TGocciaScope.IsFunctionBoundary: Boolean;
begin
  Result := False;
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

function TGocciaScope.EffectiveNonStrictMode: Boolean;
var
  Current: TGocciaScope;
begin
  Current := Self;
  while Assigned(Current) do
  begin
    if Current.FScopeKind in [skFunction, skModule, skGlobal] then
      Exit(Current.FNonStrictMode);
    Current := Current.FParent;
  end;
  Result := False;
end;

function TGocciaScope.EffectiveArgumentsObjectEnabled: Boolean;
var
  Current: TGocciaScope;
begin
  Current := Self;
  while Assigned(Current) do
  begin
    if Current.FScopeKind in [skFunction, skModule, skGlobal] then
      Exit(Current.FArgumentsObjectEnabled);
    Current := Current.FParent;
  end;
  Result := False;
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
    if Current.IsFunctionBoundary then
      Exit(nil);
    Current := Current.FParent;
  end;
  Result := nil;
end;

function TGocciaScope.FindOwningClassAfter(const AAfter: TGocciaValue): TGocciaValue;
var
  Current: TGocciaScope;
  Candidate: TGocciaValue;
  FoundAfter: Boolean;
begin
  Current := Self;
  FoundAfter := not Assigned(AAfter);
  while Assigned(Current) do
  begin
    Candidate := Current.GetOwningClass;
    if Assigned(Candidate) then
    begin
      if FoundAfter and (Candidate <> AAfter) then
        Exit(Candidate);
      if Candidate = AAfter then
        FoundAfter := True;
    end;
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
    if Current.IsFunctionBoundary then
      Exit(nil);
    Current := Current.FParent;
  end;
  Result := nil;
end;

function TGocciaScope.FindSuperConstructor: TGocciaValue;
var
  Current: TGocciaScope;
begin
  Current := Self;
  while Assigned(Current) do
  begin
    Result := Current.GetSuperConstructor;
    if Assigned(Result) then Exit;
    if Current.IsFunctionBoundary then
      Exit(nil);
    Current := Current.FParent;
  end;
  Result := nil;
end;

function TGocciaScope.FindNewTarget: TGocciaValue;
var
  Current: TGocciaScope;
begin
  Current := Self;
  while Assigned(Current) do
  begin
    Result := Current.GetNewTarget;
    if Assigned(Result) then Exit;
    if Current.IsFunctionBoundary then
      Exit(nil);
    Current := Current.FParent;
  end;
  Result := nil;
end;

function TGocciaScope.MarkSuperConstructorCalled: Boolean;
begin
  Result := False;
end;

procedure TGocciaScope.PredeclareLexicalBinding(const AName: string;
  const ADeclarationType: TGocciaDeclarationType; const ALine: Integer = 0;
  const AColumn: Integer = 0);
var
  LexicalBinding: TLexicalBinding;
  ExistingBinding: TLexicalBinding;
  GlobalObject: TGocciaObjectValue;
  Descriptor: TGocciaPropertyDescriptor;
  Error: TGocciaSyntaxError;
begin
  if (FScopeKind = skGlobal) and ContainsOwnVarBinding(AName) then
  begin
    Error := TGocciaSyntaxError.Create(
      Format(SErrorIdentifierAlreadyDeclared, [AName]), ALine, AColumn, '',
      nil);
    Error.Suggestion := SSuggestAlreadyDeclared;
    raise Error;
  end;

  if HasRestrictedGlobalProperty(AName) then
  begin
    Error := TGocciaSyntaxError.Create(
      Format(SErrorIdentifierAlreadyDeclared, [AName]), ALine, AColumn, '',
      nil);
    Error.Suggestion := SSuggestAlreadyDeclared;
    raise Error;
  end;

  if FLexicalBindings.TryGetValue(AName, ExistingBinding) then
  begin
    if ExistingBinding.BuiltIn and (FScopeKind = skGlobal) and
       (FThisValue is TGocciaObjectValue) then
    begin
      GlobalObject := TGocciaObjectValue(FThisValue);
      Descriptor := GlobalObject.GetOwnPropertyDescriptor(AName);
      if (not Assigned(Descriptor)) or Descriptor.Configurable then
        FLexicalBindings.Remove(AName)
      else
      begin
        Error := TGocciaSyntaxError.Create(
          Format(SErrorIdentifierAlreadyDeclared, [AName]), ALine, AColumn,
          '', nil);
        Error.Suggestion := SSuggestAlreadyDeclared;
        raise Error;
      end;
    end
    else
    begin
      Error := TGocciaSyntaxError.Create(
        Format(SErrorIdentifierAlreadyDeclared, [AName]), ALine, AColumn, '',
        nil);
      Error.Suggestion := SSuggestAlreadyDeclared;
      raise Error;
    end;
  end;

  LexicalBinding.Value := TGocciaUndefinedLiteralValue.UndefinedValue;
  LexicalBinding.DeclarationType := ADeclarationType;
  LexicalBinding.Initialized := False;
  LexicalBinding.BuiltIn := False;
  LexicalBinding.GlobalObjectBacked := False;
  LexicalBinding.CanDelete := False;
  LexicalBinding.TypeHint := sltUntyped;
  FLexicalBindings.Add(AName, LexicalBinding);
end;

procedure TGocciaScope.DefineLexicalBinding(const AName: string; const AValue: TGocciaValue; const ADeclarationType: TGocciaDeclarationType; const ABuiltIn: Boolean = False; const ALine: Integer = 0; const AColumn: Integer = 0);
var
  LexicalBinding: TLexicalBinding;
  ExistingLexicalBinding: TLexicalBinding;
  Error: TGocciaSyntaxError;
begin
  // Check for redeclaration errors
  if FLexicalBindings.TryGetValue(AName, ExistingLexicalBinding) then
  begin
    if not ExistingLexicalBinding.Initialized then
    begin
      ExistingLexicalBinding.Value := AValue;
      ExistingLexicalBinding.DeclarationType := ADeclarationType;
      ExistingLexicalBinding.Initialized := True;
      ExistingLexicalBinding.BuiltIn := ABuiltIn;
      ExistingLexicalBinding.GlobalObjectBacked := False;
      FLexicalBindings.AddOrSetValue(AName, ExistingLexicalBinding);
      Exit;
    end;

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

  if (not ABuiltIn) and HasRestrictedGlobalProperty(AName) then
  begin
    Error := TGocciaSyntaxError.Create(
      Format(SErrorIdentifierAlreadyDeclared, [AName]), ALine, AColumn, '',
      nil);
    Error.Suggestion := SSuggestAlreadyDeclared;
    raise Error;
  end;

  // Set up descriptor based on declaration type
  LexicalBinding.Value := AValue;
  LexicalBinding.DeclarationType := ADeclarationType;

  // All declaration types are immediately initialized:
  // - dtConst: parser validates initializers; 'const x = undefined;' is valid
  // - dtLet: no TDZ after declaration statement is processed
  // - dtParameter: parameters have no TDZ
  LexicalBinding.Initialized := True;
  LexicalBinding.BuiltIn := ABuiltIn;
  LexicalBinding.GlobalObjectBacked := False;
  LexicalBinding.CanDelete := False;
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
    Exit;
  end;

  if Assigned(FVarBindings) and FVarBindings.TryGetValue(AName,
     LexicalBinding) then
  begin
    LexicalBinding.Value := AValue;
    LexicalBinding.Initialized := True;
    FVarBindings.AddOrSetValue(AName, LexicalBinding);
  end;
end;

procedure TGocciaScope.MarkGlobalObjectBackedBinding(const AName: string);
var
  LexicalBinding: TLexicalBinding;
begin
  if FLexicalBindings.TryGetValue(AName, LexicalBinding) then
  begin
    LexicalBinding.GlobalObjectBacked := True;
    FLexicalBindings.AddOrSetValue(AName, LexicalBinding);
    Exit;
  end;

  if Assigned(FVarBindings) and FVarBindings.TryGetValue(AName,
     LexicalBinding) then
  begin
    LexicalBinding.GlobalObjectBacked := True;
    FVarBindings.AddOrSetValue(AName, LexicalBinding);
  end;
end;

function TGocciaScope.IsGlobalObjectBackedBinding(const AName: string): Boolean;
var
  LexicalBinding: TLexicalBinding;
begin
  // Reads the binding's GlobalObjectBacked flag without resolving its value,
  // so callers can detect an already-mirrored global without triggering lazy
  // materialization (which a value read through TryGetBinding would force).
  if FLexicalBindings.TryGetValue(AName, LexicalBinding) then
    Exit(LexicalBinding.GlobalObjectBacked);

  if Assigned(FVarBindings) and FVarBindings.TryGetValue(AName,
     LexicalBinding) then
    Exit(LexicalBinding.GlobalObjectBacked);

  Result := False;
end;

procedure TGocciaScope.DefineVariableBinding(const AName: string; const AValue: TGocciaValue;
  const AHasInitializer: Boolean; const ACanDelete: Boolean = False);
var
  TargetScope: TGocciaScope;
  Binding: TLexicalBinding;
  ExistingBuiltIn: TLexicalBinding;
  EffectiveValue: TGocciaValue;
  GlobalObject: TGocciaObjectValue;
  Flags: TPropertyFlags;
begin
  TargetScope := FindFunctionOrModuleScope;
  EffectiveValue := AValue;

  // §16.1.7: var/function declarations may shadow built-in globals in
  // script source.  Only the global scope carries built-in bindings, so
  // skip the lookup for function/module-scoped vars (the common case).
  if (TargetScope.FScopeKind = skGlobal) and
     TargetScope.FLexicalBindings.TryGetValue(AName, ExistingBuiltIn) and
     ExistingBuiltIn.BuiltIn then
  begin
    if not AHasInitializer then
      EffectiveValue := ExistingBuiltIn.Value;
    TargetScope.FLexicalBindings.Remove(AName);
  end;

  if (TargetScope.FScopeKind = skGlobal) and
     (TargetScope.FThisValue is TGocciaObjectValue) then
  begin
    if not ACanDelete then
      TargetScope.RegisterGlobalVarDeclaration(AName);
    GlobalObject := TGocciaObjectValue(TargetScope.FThisValue);
    if GlobalObject.HasOwnProperty(AName) then
    begin
      if AHasInitializer then
        GlobalObject.AssignProperty(AName, EffectiveValue);
      Exit;
    end;
    Flags := [pfEnumerable, pfWritable];
    if ACanDelete then
      Include(Flags, pfConfigurable);
    GlobalObject.DefineProperty(AName,
      TGocciaPropertyDescriptorData.Create(EffectiveValue, Flags));
    Exit;
  end;

  // Lazily allocate the var bindings map
  if not Assigned(TargetScope.FVarBindings) then
    TargetScope.FVarBindings := TGocciaScopeBindingMap.Create;

  if TargetScope.FVarBindings.TryGetValue(AName, Binding) then
  begin
    // Redeclaration: only update if the source had a syntactic initializer.
    if AHasInitializer then
    begin
      Binding.Value := EffectiveValue;
      TargetScope.FVarBindings.AddOrSetValue(AName, Binding);
    end;
  end
  else
  begin
    // First declaration: create the binding
    Binding.Value := EffectiveValue;
    Binding.DeclarationType := dtVar;
    Binding.Initialized := True;
    Binding.BuiltIn := False;
    Binding.GlobalObjectBacked := False;
    Binding.CanDelete := ACanDelete;
    Binding.TypeHint := sltUntyped;
    TargetScope.FVarBindings.AddOrSetValue(AName, Binding);
  end;

end;

function TGocciaScope.ContainsOwnVarBinding(const AName: string): Boolean;
begin
  Result := (Assigned(FVarBindings) and FVarBindings.ContainsKey(AName)) or
    (Assigned(FGlobalVarNames) and FGlobalVarNames.ContainsKey(AName));
end;

function TGocciaScope.ContainsVarEnvironmentBinding(const AName: string): Boolean;
begin
  Result := ContainsOwnLexicalBinding(AName) or ContainsOwnVarBinding(AName);
end;

// ES2026 §9.1.1.4.15 HasLexicalDeclaration(N) — global-scope approximation.
function TGocciaScope.HasLexicalDeclaration(const AName: string): Boolean;
begin
  Result := ContainsOwnLexicalBinding(AName) and not IsBuiltInBinding(AName);
end;

// ES2026 §9.1.1.4.14 HasRestrictedGlobalProperty(N)
function TGocciaScope.HasRestrictedGlobalProperty(const AName: string): Boolean;
var
  Descriptor: TGocciaPropertyDescriptor;
  GlobalObject: TGocciaObjectValue;
begin
  if (FScopeKind <> skGlobal) or not (FThisValue is TGocciaObjectValue) then
    Exit(False);
  if (AName = PROP_GOCCIA) or (AName = PROP_GLOBAL_THIS) then
    Exit(False);

  GlobalObject := TGocciaObjectValue(FThisValue);
  Descriptor := GlobalObject.GetOwnPropertyDescriptor(AName);
  Result := Assigned(Descriptor) and not Descriptor.Configurable;
end;

// ES2026 §9.1.1.4.17 CanDeclareGlobalVar(N)
function TGocciaScope.CanDeclareGlobalVar(const AName: string): Boolean;
var
  GlobalObject: TGocciaObjectValue;
begin
  if (FScopeKind <> skGlobal) or not (FThisValue is TGocciaObjectValue) then
    Exit(True);

  GlobalObject := TGocciaObjectValue(FThisValue);
  Result := GlobalObject.HasOwnProperty(AName) or GlobalObject.Extensible;
end;

// ES2026 §9.1.1.4.16 CanDeclareGlobalFunction(N)
function TGocciaScope.CanDeclareGlobalFunction(const AName: string): Boolean;
var
  GlobalObject: TGocciaObjectValue;
  Descriptor: TGocciaPropertyDescriptor;
begin
  if (FScopeKind <> skGlobal) or not (FThisValue is TGocciaObjectValue) then
    Exit(True);

  GlobalObject := TGocciaObjectValue(FThisValue);
  Descriptor := GlobalObject.GetOwnPropertyDescriptor(AName);
  if not Assigned(Descriptor) then
    Exit(GlobalObject.Extensible);
  if Descriptor.Configurable then
    Exit(True);
  Result := IsDataDescriptor(Descriptor) and Descriptor.Writable and
    Descriptor.Enumerable;
end;

// ES2026 §9.1.1.4.18 CreateGlobalVarBinding(N, D)
procedure TGocciaScope.CreateGlobalVarBinding(const AName: string;
  const ACanDelete: Boolean);
var
  Flags: TPropertyFlags;
  GlobalObject: TGocciaObjectValue;
begin
  if (FScopeKind <> skGlobal) or not (FThisValue is TGocciaObjectValue) then
  begin
    DefineVariableBinding(AName, TGocciaUndefinedLiteralValue.UndefinedValue,
      False);
    Exit;
  end;

  if not ACanDelete then
    RegisterGlobalVarDeclaration(AName);
  GlobalObject := TGocciaObjectValue(FThisValue);
  if GlobalObject.HasOwnProperty(AName) then
    Exit;
  Flags := [pfEnumerable, pfWritable];
  if ACanDelete then
    Include(Flags, pfConfigurable);
  GlobalObject.DefineProperty(AName,
    TGocciaPropertyDescriptorData.Create(
      TGocciaUndefinedLiteralValue.UndefinedValue, Flags));
end;

// ES2026 §9.1.1.4.19 CreateGlobalFunctionBinding(N, V, D)
procedure TGocciaScope.CreateGlobalFunctionBinding(const AName: string;
  const AValue: TGocciaValue; const ACanDelete: Boolean);
var
  Descriptor: TGocciaPropertyDescriptor;
  Flags: TPropertyFlags;
  GlobalObject: TGocciaObjectValue;
begin
  if (FScopeKind <> skGlobal) or not (FThisValue is TGocciaObjectValue) then
  begin
    DefineVariableBinding(AName, AValue, True);
    Exit;
  end;

  if not ACanDelete then
    RegisterGlobalVarDeclaration(AName);
  GlobalObject := TGocciaObjectValue(FThisValue);
  Descriptor := GlobalObject.GetOwnPropertyDescriptor(AName);
  if Assigned(Descriptor) and not Descriptor.Configurable then
  begin
    GlobalObject.AssignProperty(AName, AValue);
    Exit;
  end;

  Flags := [pfEnumerable, pfWritable];
  if ACanDelete then
    Include(Flags, pfConfigurable);
  GlobalObject.DefineProperty(AName,
    TGocciaPropertyDescriptorData.Create(AValue, Flags));
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

procedure TGocciaScope.AssignBinding(const AName: string; const AValue: TGocciaValue; const ALine: Integer = 0; const AColumn: Integer = 0; const ANonStrictMode: Boolean = False);
var
  Root: TGocciaScope;
  GlobalObject: TGocciaObjectValue;
begin
  // Single source of truth for assignment semantics is the virtual
  // TryAssignExistingBinding; this wrapper only owns the unbound-name
  // outcome: sloppy assignments create a property on the root global
  // this-object, strict assignments raise.
  if TryAssignExistingBinding(AName, AValue, ANonStrictMode, ALine, AColumn) then
    Exit;

  Root := Self;
  while Assigned(Root.FParent) do
    Root := Root.FParent;
  if ANonStrictMode and (Root.FScopeKind = skGlobal) and
     (Root.FThisValue is TGocciaObjectValue) then
  begin
    GlobalObject := TGocciaObjectValue(Root.FThisValue);
    if ((AName = PROP_GOCCIA) or (AName = PROP_GLOBAL_THIS)) and
       GlobalObject.HasProperty(AName) then
      raise TGocciaTypeError.Create(
        Format(SErrorAssignToConstant, [AName]),
        ALine, AColumn, '', nil,
        SSuggestUseLetNotConst);
    GlobalObject.AssignPropertyWithReceiver(AName, AValue, GlobalObject);
    Exit;
  end;

  RaiseUndefinedVariable(AName, ALine, AColumn);
end;

// ES2026 §9.1.1.1.5 SetMutableBinding(N, V, S)
procedure TGocciaScope.SetOwnMutableBinding(const AName: string;
  const AValue: TGocciaValue; const AStrict: Boolean);
var
  Binding: TLexicalBinding;
begin
  if FLexicalBindings.TryGetValue(AName, Binding) then
  begin
    if not Binding.IsAccessible then
      RaiseBindingNotInitialized(AName, 0, 0);
    if not Binding.Writable then
    begin
      if AStrict then
        raise TGocciaTypeError.Create(
          Format(SErrorAssignToConstant, [AName]),
          0, 0, '', nil, SSuggestUseLetNotConst);
      Exit;
    end;
    if EffectiveStrictTypes and (Binding.TypeHint <> sltUntyped) then
      EnforceStrictType(AValue, Binding.TypeHint);
    Binding.Value := AValue;
    Binding.Initialized := True;
    FLexicalBindings.AddOrSetValue(AName, Binding);
    Exit;
  end;

  if Assigned(FVarBindings) and FVarBindings.TryGetValue(AName, Binding) then
  begin
    if EffectiveStrictTypes and (Binding.TypeHint <> sltUntyped) then
      EnforceStrictType(AValue, Binding.TypeHint);
    Binding.Value := AValue;
    FVarBindings.AddOrSetValue(AName, Binding);
    Exit;
  end;

  if AStrict then
    RaiseUndefinedVariable(AName, 0, 0);
  DefineVariableBinding(AName, AValue, True, True);
end;

function TGocciaScope.TryAssignExistingBinding(const AName: string;
  const AValue: TGocciaValue; const ANonStrictMode: Boolean = False;
  const ALine: Integer = 0; const AColumn: Integer = 0): Boolean;
var
  LexicalBinding: TLexicalBinding;
  StrictActive: Boolean;
  GlobalObject: TGocciaObjectValue;
begin
  // Assignment resolution order: own lexical (TDZ, const, and strict-type
  // errors still raise), global this-object property, own var binding, then
  // parent chain.  Unbound names return False; the caller decides whether
  // that is a ReferenceError or a sloppy global create (see AssignBinding).
  //
  // Type hints recorded on bindings persist for the lifetime of the
  // binding; the live --strict-types flag (read from the root scope)
  // gates whether they enforce.  This mirrors the function-entry
  // behaviour: when the embedder turns strict-types off after bindings
  // have been declared, those bindings stop enforcing rather than
  // continuing to throw based on stale state.
  StrictActive := EffectiveStrictTypes;

  if FLexicalBindings.TryGetValue(AName, LexicalBinding) then
  begin
    if not LexicalBinding.IsAccessible then
      RaiseBindingNotInitialized(AName, ALine, AColumn);
    if (AName = PROP_GOCCIA) or (AName = PROP_GLOBAL_THIS) then
      raise TGocciaTypeError.Create(
        Format(SErrorAssignToConstant, [AName]),
        ALine, AColumn, '', nil,
        SSuggestUseLetNotConst);
    if IsGlobalBuiltInObjectBinding(LexicalBinding) then
    begin
      GlobalObject := TGocciaObjectValue(FThisValue);
      if not GlobalObject.HasProperty(AName) then
        Exit(False);
      if CanSetGlobalObjectProperty(AName) then
      begin
        if ANonStrictMode then
          GlobalObject.AssignPropertyWithReceiver(AName, AValue, GlobalObject)
        else
          GlobalObject.AssignProperty(AName, AValue);
        LexicalBinding.Value := GlobalObject.GetProperty(AName);
        LexicalBinding.Initialized := True;
        FLexicalBindings.AddOrSetValue(AName, LexicalBinding);
        Exit(True);
      end;
    end;
    if not LexicalBinding.Writable then
      raise TGocciaTypeError.Create(
        Format(SErrorAssignToConstant, [AName]),
        ALine, AColumn, '', nil,
        SSuggestUseLetNotConst);
    if StrictActive and (LexicalBinding.TypeHint <> sltUntyped) then
      EnforceStrictType(AValue, LexicalBinding.TypeHint);
    LexicalBinding.Value := AValue;
    LexicalBinding.Initialized := True;
    FLexicalBindings.AddOrSetValue(AName, LexicalBinding);
    Exit(True);
  end;

  if (FScopeKind = skGlobal) and (FThisValue is TGocciaObjectValue) and
     TGocciaObjectValue(FThisValue).HasProperty(AName) then
  begin
    if (AName = PROP_GOCCIA) or (AName = PROP_GLOBAL_THIS) then
      raise TGocciaTypeError.Create(
        Format(SErrorAssignToConstant, [AName]),
        ALine, AColumn, '', nil,
        SSuggestUseLetNotConst);
    GlobalObject := TGocciaObjectValue(FThisValue);
    if ANonStrictMode then
      GlobalObject.AssignPropertyWithReceiver(AName, AValue, GlobalObject)
    else
      GlobalObject.AssignProperty(AName, AValue);
    Exit(True);
  end;

  if Assigned(FVarBindings) and FVarBindings.TryGetValue(AName, LexicalBinding) then
  begin
    if StrictActive and (LexicalBinding.TypeHint <> sltUntyped) then
      EnforceStrictType(AValue, LexicalBinding.TypeHint);
    LexicalBinding.Value := AValue;
    FVarBindings.AddOrSetValue(AName, LexicalBinding);
    Exit(True);
  end;

  if Assigned(FParent) then
    Exit(FParent.TryAssignExistingBinding(AName, AValue, ANonStrictMode,
      ALine, AColumn));

  Result := False;
end;

function TGocciaScope.GetBinding(const AName: string; const ALine: Integer = 0; const AColumn: Integer = 0): TLexicalBinding;
begin
  // Single source of truth for read resolution is the virtual
  // TryGetBinding; this wrapper only owns the unbound-name raise.
  if not TryGetBinding(AName, Result, ALine, AColumn) then
    RaiseUndefinedVariable(AName, ALine, AColumn);
end;

procedure TGocciaScope.RaiseBindingNotInitialized(const AName: string;
  const ALine, AColumn: Integer);
begin
  raise TGocciaReferenceError.Create(
    Format(SErrorCannotAccessBeforeInit, [AName]),
    ALine, AColumn, '', nil,
    SSuggestTemporalDeadZone);
end;

procedure TGocciaScope.RaiseUndefinedVariable(const AName: string;
  const ALine, AColumn: Integer);
begin
  raise TGocciaReferenceError.Create(
    Format(SErrorUndefinedVariable, [AName]),
    ALine, AColumn, '', nil,
    SSuggestDeclareBeforeUse);
end;

function TGocciaScope.GetValue(const AName: string): TGocciaValue;
begin
  Result := GetBinding(AName).Value;
end;

function TGocciaScope.TryGetBinding(const AName: string;
  out ABinding: TLexicalBinding; const ALine: Integer = 0;
  const AColumn: Integer = 0): Boolean;
begin
  // Resolution order: own lexical (with TDZ check), global this-object
  // property, own var binding, then parent chain.
  if FLexicalBindings.TryGetValue(AName, ABinding) then
  begin
    if not ABinding.IsAccessible then
      RaiseBindingNotInitialized(AName, ALine, AColumn);
    if IsGlobalBuiltInObjectBinding(ABinding) then
    begin
      if TryGetGlobalBuiltInObjectBinding(AName, ABinding, ABinding) then
        Exit(True);
      ABinding := Default(TLexicalBinding);
      Exit(False);
    end;
    Exit(True);
  end;
  Result := TryGetBindingSkipLexical(AName, ABinding, ALine, AColumn);
end;

function TGocciaScope.TryGetOwnBinding(const AName: string;
  out ABinding: TLexicalBinding): Boolean;
begin
  if FLexicalBindings.TryGetValue(AName, ABinding) then
  begin
    if not ABinding.IsAccessible then
      RaiseBindingNotInitialized(AName, 0, 0);
    Exit(True);
  end;
  if Assigned(FVarBindings) and FVarBindings.TryGetValue(AName, ABinding) then
    Exit(True);
  ABinding := Default(TLexicalBinding);
  Result := False;
end;

function TGocciaScope.TryGetBindingValue(const AName: string;
  out AValue: TGocciaValue): Boolean;
var
  LexicalBinding: TLexicalBinding;
begin
  Result := TryGetBinding(AName, LexicalBinding);
  if Result then
    AValue := LexicalBinding.Value
  else
    AValue := nil;
end;

function TGocciaScope.IsGlobalBuiltInObjectBinding(
  const ALexicalBinding: TLexicalBinding): Boolean;
begin
  Result := ALexicalBinding.BuiltIn and ALexicalBinding.GlobalObjectBacked and
    (FScopeKind = skGlobal) and (FThisValue is TGocciaObjectValue);
end;

function TGocciaScope.CanSetGlobalObjectProperty(const AName: string): Boolean;
var
  Descriptor: TGocciaPropertyDescriptor;
  GlobalObject: TGocciaObjectValue;
begin
  if (FScopeKind <> skGlobal) or not (FThisValue is TGocciaObjectValue) then
    Exit(False);

  GlobalObject := TGocciaObjectValue(FThisValue);
  Descriptor := GlobalObject.GetOwnPropertyDescriptor(AName);
  Result := Assigned(Descriptor) and IsDataDescriptor(Descriptor) and
    Descriptor.Writable;
end;

function TGocciaScope.TryGetGlobalBuiltInObjectBinding(const AName: string;
  const ALexicalBinding: TLexicalBinding;
  out ABinding: TLexicalBinding): Boolean;
var
  GlobalObject: TGocciaObjectValue;
begin
  if not IsGlobalBuiltInObjectBinding(ALexicalBinding) then
    Exit(False);

  GlobalObject := TGocciaObjectValue(FThisValue);
  if not GlobalObject.HasProperty(AName) then
    Exit(False);

  ABinding := ALexicalBinding;
  ABinding.Value := GlobalObject.GetProperty(AName);
  Result := True;
end;

function TGocciaScope.TryGetBindingSkipLexical(const AName: string;
  out ABinding: TLexicalBinding; const ALine: Integer = 0;
  const AColumn: Integer = 0): Boolean;
begin
  if (FScopeKind = skGlobal) and (FThisValue is TGocciaObjectValue) and
     TGocciaObjectValue(FThisValue).HasProperty(AName) then
  begin
    ABinding.Value := TGocciaObjectValue(FThisValue).GetProperty(AName);
    ABinding.DeclarationType := dtVar;
    ABinding.Initialized := True;
    ABinding.BuiltIn := False;
    ABinding.GlobalObjectBacked := False;
    ABinding.CanDelete := False;
    ABinding.TypeHint := sltUntyped;
    Exit(True);
  end;
  if Assigned(FVarBindings) and FVarBindings.TryGetValue(AName, ABinding) then
    Exit(True);
  if Assigned(FParent) then
    Exit(FParent.TryGetBinding(AName, ABinding, ALine, AColumn));
  ABinding := Default(TLexicalBinding);
  Result := False;
end;

function TGocciaScope.TryGetLexicalValueAt(const AEntryIndex: Integer;
  const AVersion: Cardinal; out AValue: TGocciaValue): Boolean;
var
  LexicalBinding: TLexicalBinding;
begin
  if (AVersion <> FLexicalBindings.EntryVersion) or
     not FLexicalBindings.TryGetValueAtEntry(AEntryIndex, LexicalBinding) then
  begin
    AValue := nil;
    Exit(False);
  end;
  if not LexicalBinding.IsAccessible then
    RaiseBindingNotInitialized(FLexicalBindings.KeyAtEntry(AEntryIndex), 0, 0);
  if IsGlobalBuiltInObjectBinding(LexicalBinding) then
  begin
    if TryGetGlobalBuiltInObjectBinding(FLexicalBindings.KeyAtEntry(AEntryIndex),
       LexicalBinding, LexicalBinding) then
    begin
      AValue := LexicalBinding.Value;
      Exit(True);
    end;
    AValue := nil;
    Exit(False);
  end;
  AValue := LexicalBinding.Value;
  Result := True;
end;

function TGocciaScope.TryGetBindingValueFillCache(const AName: string;
  out AEntryIndex: Integer; out AVersion: Cardinal;
  out AValue: TGocciaValue): Boolean;
var
  LexicalBinding: TLexicalBinding;
begin
  if FLexicalBindings.TryGetEntryIndex(AName, AEntryIndex) then
  begin
    AVersion := FLexicalBindings.EntryVersion;
    FLexicalBindings.TryGetValueAtEntry(AEntryIndex, LexicalBinding);
    if not LexicalBinding.IsAccessible then
      RaiseBindingNotInitialized(AName, 0, 0);
    if IsGlobalBuiltInObjectBinding(LexicalBinding) then
    begin
      if TryGetGlobalBuiltInObjectBinding(AName, LexicalBinding,
         LexicalBinding) then
      begin
        AValue := LexicalBinding.Value;
        Exit(True);
      end;
      AEntryIndex := -1;
      AVersion := 0;
      AValue := nil;
      Exit(False);
    end;
    AValue := LexicalBinding.Value;
    Exit(True);
  end;
  AEntryIndex := -1;
  AVersion := 0;
  Result := TryGetBindingSkipLexical(AName, LexicalBinding);
  if Result then
    AValue := LexicalBinding.Value
  else
    AValue := nil;
end;

function TGocciaScope.DeleteBinding(const AName: string): Boolean;
var
  Binding: TLexicalBinding;
  GlobalObject: TGocciaObjectValue;
begin
  if FLexicalBindings.TryGetValue(AName, Binding) then
  begin
    if (FScopeKind = skGlobal) and Binding.BuiltIn and
       (FThisValue is TGocciaObjectValue) then
    begin
      GlobalObject := TGocciaObjectValue(FThisValue);
      if GlobalObject.HasOwnProperty(AName) then
      begin
        Result := GlobalObject.DeleteProperty(AName);
        if Result then
          FLexicalBindings.Remove(AName);
        Exit;
      end;
    end;
    Exit(False);
  end;

  if Assigned(FVarBindings) and FVarBindings.TryGetValue(AName, Binding) then
  begin
    if not Binding.CanDelete then
      Exit(False);
    FVarBindings.Remove(AName);
    Exit(True);
  end;

  if (FScopeKind = skGlobal) and (FThisValue is TGocciaObjectValue) and
     TGocciaObjectValue(FThisValue).HasProperty(AName) then
    Exit(TGocciaObjectValue(FThisValue).DeleteProperty(AName));

  if Assigned(FParent) then
    Exit(FParent.DeleteBinding(AName));

  Result := True;
end;

function TGocciaScope.ResolveIdentifier(const AName: string): TGocciaValue;
begin
  if AName = KEYWORD_THIS then
    Result := FThisValue
  else
    Result := GetValue(AName);
end;

procedure TGocciaScope.ResolveIdentifierReference(const AName: string;
  out AValue, AThisValue: TGocciaValue; const ALine: Integer = 0;
  const AColumn: Integer = 0);
begin
  AThisValue := TGocciaUndefinedLiteralValue.UndefinedValue;

  if AName = KEYWORD_THIS then
  begin
    AValue := FThisValue;
    Exit;
  end;

  AValue := GetBinding(AName, ALine, AColumn).Value;
end;

procedure TGocciaScope.ResolveAssignmentTarget(const AName: string;
  out AObjectBinding: TGocciaObjectValue; out AScopeBinding: TGocciaScope);
begin
  AObjectBinding := nil;
  AScopeBinding := nil;
  if ContainsOwnLexicalBinding(AName) or ContainsOwnVarBinding(AName) or
     ((FScopeKind = skGlobal) and (FThisValue is TGocciaObjectValue) and
      TGocciaObjectValue(FThisValue).HasProperty(AName)) then
  begin
    AScopeBinding := Self;
    Exit;
  end;
  if Assigned(FParent) then
  begin
    FParent.ResolveAssignmentTarget(AName, AObjectBinding, AScopeBinding);
    Exit;
  end;
  AScopeBinding := Self;
end;

function TGocciaScope.ContainsOwnLexicalBinding(const AName: string): Boolean; inline;
begin
  Result := FLexicalBindings.ContainsKey(AName);
end;

function TGocciaScope.IsBuiltInBinding(const AName: string): Boolean;
var
  Binding: TLexicalBinding;
begin
  Result := FLexicalBindings.TryGetValue(AName, Binding) and Binding.BuiltIn;
end;

function TGocciaScope.Contains(const AName: string): Boolean;
begin
  Result := ContainsOwnLexicalBinding(AName) or
    ContainsOwnVarBinding(AName) or
    ((FScopeKind = skGlobal) and (FThisValue is TGocciaObjectValue) and
     TGocciaObjectValue(FThisValue).HasProperty(AName)) or
    (Assigned(FParent) and FParent.Contains(AName));
end;

function TGocciaScope.GetOwnBindingNames: TGocciaStringArray;
begin
  Result := FLexicalBindings.Keys;
end;

function TGocciaScope.GetOwnVarBindingNames: TGocciaStringArray;
begin
  if Assigned(FVarBindings) then
    Result := FVarBindings.Keys
  else
    SetLength(Result, 0);
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

function TGocciaCallScope.GetNewTarget: TGocciaValue;
begin
  Result := FNewTarget;
end;

function TGocciaCallScope.IsFunctionBoundary: Boolean;
begin
  Result := True;
end;

procedure TGocciaCallScope.MarkReferences;
begin
  inherited;
  if Assigned(FNewTarget) then
    FNewTarget.MarkReferences;
end;

{ TGocciaFunctionNameScope }

constructor TGocciaFunctionNameScope.Create(const AParent: TGocciaScope;
  const AFunctionName: string);
begin
  inherited Create(AParent, skBlock, 'FunctionName');
  FFunctionName := AFunctionName;
end;

procedure TGocciaFunctionNameScope.DefineFunctionNameBinding(
  const AValue: TGocciaValue);
begin
  DefineLexicalBinding(FFunctionName, AValue, dtConst);
end;

function TGocciaFunctionNameScope.TryAssignExistingBinding(const AName: string;
  const AValue: TGocciaValue; const ANonStrictMode: Boolean = False;
  const ALine: Integer = 0; const AColumn: Integer = 0): Boolean;
var
  LexicalBinding: TLexicalBinding;
begin
  if (AName = FFunctionName) and
     FLexicalBindings.TryGetValue(AName, LexicalBinding) then
  begin
    if not LexicalBinding.IsAccessible then
      RaiseBindingNotInitialized(AName, ALine, AColumn);
    if not LexicalBinding.Writable then
    begin
      if ANonStrictMode then
        Exit(True);
      raise TGocciaTypeError.Create(
        Format(SErrorAssignToConstant, [AName]),
        ALine, AColumn, '', nil,
        SSuggestUseLetNotConst);
    end;
  end;

  Result := inherited TryAssignExistingBinding(AName, AValue,
    ANonStrictMode, ALine, AColumn);
end;

{ TGocciaArrowCallScope }

constructor TGocciaArrowCallScope.Create(const AParent: TGocciaScope; const AFunctionName: string; const ACapacity: Integer = 0);
begin
  inherited Create(AParent, AFunctionName, ACapacity);
end;

function TGocciaArrowCallScope.IsFunctionBoundary: Boolean;
begin
  Result := False;
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

function TGocciaMethodCallScope.GetSuperConstructor: TGocciaValue;
begin
  if FOwningClass is TGocciaClassValue then
  begin
    Result := TGocciaClassValue(FOwningClass).GetConstructorPrototype;
    if Assigned(Result) then
      Exit;
  end;
  Result := FSuperClass;
end;

function TGocciaMethodCallScope.GetNewTarget: TGocciaValue;
begin
  Result := FNewTarget;
end;

function TGocciaMethodCallScope.MarkSuperConstructorCalled: Boolean;
begin
  if FSuperConstructorCalled then
    raise TGocciaReferenceError.Create(
      'Super constructor may only be called once',
      0, 0, '', nil);
  FSuperConstructorCalled := True;
  Result := True;
end;

procedure TGocciaMethodCallScope.MarkReferences;
begin
  inherited;
  if Assigned(FSuperClass) then
    FSuperClass.MarkReferences;
  if Assigned(FOwningClass) then
    FOwningClass.MarkReferences;
  if Assigned(FNewTarget) then
    FNewTarget.MarkReferences;
end;

{ TGocciaClassInitScope }

constructor TGocciaClassInitScope.Create(const AParent: TGocciaScope; const AOwningClass: TGocciaValue);
begin
  inherited Create(AParent, skBlock, 'ClassInit');
  FOwningClass := AOwningClass;
  RejectArgumentsReferenceInDirectEval := True;
end;

function TGocciaClassInitScope.GetThisValue: TGocciaValue;
begin
  Result := FThisValue;
end;

function TGocciaClassInitScope.GetOwningClass: TGocciaValue;
begin
  Result := FOwningClass;
end;

function TGocciaClassInitScope.GetSuperClass: TGocciaValue;
var
  ClassValue: TGocciaClassValue;
begin
  Result := nil;
  if not (FOwningClass is TGocciaClassValue) then
    Exit;

  ClassValue := TGocciaClassValue(FOwningClass);
  if Assigned(ClassValue.SuperClass) then
    Result := ClassValue.SuperClass
  else if Assigned(ClassValue.NativeSuperConstructor) then
    Result := ClassValue.NativeSuperConstructor;
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
  if Assigned(AParent) then
    ThisValue := AParent.ThisValue;
end;

function TGocciaCatchScope.TryAssignExistingBinding(const AName: string;
  const AValue: TGocciaValue; const ANonStrictMode: Boolean = False;
  const ALine: Integer = 0; const AColumn: Integer = 0): Boolean;
begin
  // Catch parameter shadowing: assignments to non-parameter names not bound
  // on this scope propagate straight to the parent; the catch parameter
  // itself (and anything declared here) uses base behavior.
  if (AName <> FCatchParameter) and
     (not FLexicalBindings.ContainsKey(AName)) and Assigned(FParent) then
    Result := FParent.TryAssignExistingBinding(AName, AValue, ANonStrictMode,
      ALine, AColumn)
  else
    Result := inherited TryAssignExistingBinding(AName, AValue, ANonStrictMode,
      ALine, AColumn);
end;

{ TGocciaWithScope }

constructor TGocciaWithScope.Create(const AParent: TGocciaScope;
  const ABindingObject: TGocciaObjectValue);
begin
  inherited Create(AParent, skBlock, 'WithScope');
  FBindingObject := ABindingObject;
  if Assigned(AParent) then
    ThisValue := AParent.ThisValue;
end;

function TGocciaWithScope.HasObjectBinding(const AName: string): Boolean;
var
  Unscopables: TGocciaValue;
  Blocked: TGocciaValue;
begin
  Result := Assigned(FBindingObject) and FBindingObject.HasProperty(AName);
  if not Result then
    Exit;

  Unscopables := FBindingObject.GetSymbolProperty(
    TGocciaSymbolValue.WellKnownUnscopables);
  if Unscopables is TGocciaObjectValue then
  begin
    Blocked := TGocciaObjectValue(Unscopables).GetProperty(AName);
    if Assigned(Blocked) and Blocked.ToBooleanLiteral.Value then
      Exit(False);
  end;
end;

procedure TGocciaWithScope.MarkReferences;
begin
  inherited;
  if Assigned(FBindingObject) then
    FBindingObject.MarkReferences;
end;

function TGocciaWithScope.TryGetBinding(const AName: string;
  out ABinding: TLexicalBinding; const ALine: Integer = 0;
  const AColumn: Integer = 0): Boolean;
begin
  // Object-environment lookups come first.
  if HasObjectBinding(AName) then
  begin
    ABinding.Value := FBindingObject.GetProperty(AName);
    ABinding.DeclarationType := dtVar;
    ABinding.Initialized := True;
    ABinding.BuiltIn := False;
    ABinding.GlobalObjectBacked := False;
    ABinding.CanDelete := False;
    ABinding.TypeHint := sltUntyped;
    Exit(True);
  end;
  Result := inherited TryGetBinding(AName, ABinding, ALine, AColumn);
end;

function TGocciaWithScope.TryAssignExistingBinding(const AName: string;
  const AValue: TGocciaValue; const ANonStrictMode: Boolean = False;
  const ALine: Integer = 0; const AColumn: Integer = 0): Boolean;
begin
  if HasObjectBinding(AName) then
  begin
    if ANonStrictMode then
      FBindingObject.AssignPropertyWithReceiver(AName, AValue, FBindingObject)
    else
      FBindingObject.AssignProperty(AName, AValue);
    Exit(True);
  end;
  Result := inherited TryAssignExistingBinding(AName, AValue, ANonStrictMode,
    ALine, AColumn);
end;

function TGocciaWithScope.DeleteBinding(const AName: string): Boolean;
begin
  if HasObjectBinding(AName) then
    Exit(FBindingObject.DeleteProperty(AName));

  Result := inherited DeleteBinding(AName);
end;

procedure TGocciaWithScope.ResolveIdentifierReference(const AName: string;
  out AValue, AThisValue: TGocciaValue; const ALine: Integer = 0;
  const AColumn: Integer = 0);
begin
  if HasObjectBinding(AName) then
  begin
    AValue := FBindingObject.GetProperty(AName);
    AThisValue := FBindingObject;
    Exit;
  end;

  inherited ResolveIdentifierReference(AName, AValue, AThisValue, ALine,
    AColumn);
end;

procedure TGocciaWithScope.ResolveAssignmentTarget(const AName: string;
  out AObjectBinding: TGocciaObjectValue; out AScopeBinding: TGocciaScope);
begin
  if HasObjectBinding(AName) then
  begin
    AObjectBinding := FBindingObject;
    AScopeBinding := nil;
    Exit;
  end;

  inherited ResolveAssignmentTarget(AName, AObjectBinding, AScopeBinding);
end;

function TGocciaWithScope.Contains(const AName: string): Boolean;
begin
  Result := HasObjectBinding(AName) or inherited Contains(AName);
end;

end.
