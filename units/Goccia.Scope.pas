unit Goccia.Scope;

{$I Goccia.inc}

interface

uses
  Generics.Collections,
  SysUtils,
  TypInfo,

  Goccia.Error,
  Goccia.Error.ThrowErrorCallback,
  Goccia.Interfaces,
  Goccia.Keywords,
  Goccia.Token,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  TGocciaDeclarationType = (dtLet, dtConst, dtParameter);

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

  TGocciaScopeKind = (skUnknown, skGlobal, skFunction, skBlock, skCustom, skClass, skModule);

  TGocciaScope = class
  private
    FLexicalBindings: TDictionary<string, TLexicalBinding>;
    FParent: TGocciaScope;
    FThisValue: TGocciaValue;
    FScopeKind: TGocciaScopeKind;
    FCustomLabel: string;
    FGCMarked: Boolean;
    FOnError: TGocciaThrowErrorCallback;
  protected
    function GetThisValue: TGocciaValue; virtual;
    function GetOwningClass: TGocciaValue; virtual;
    function GetSuperClass: TGocciaValue; virtual;
  public
    constructor Create(const AParent: TGocciaScope = nil; const AScopeKind: TGocciaScopeKind = skUnknown; const ACustomLabel: string = ''; const ACapacity: Integer = 0);
    destructor Destroy; override;
    function CreateChild(const AScopeKind: TGocciaScopeKind = skUnknown; const ACustomLabel: string = ''; const ACapacity: Integer = 0): TGocciaScope;

    // Garbage collection support
    procedure GCMarkReferences; virtual;
    property GCMarked: Boolean read FGCMarked write FGCMarked;

    // New Define/Assign pattern
    procedure DefineLexicalBinding(const AName: string; const AValue: TGocciaValue; const ADeclarationType: TGocciaDeclarationType; const ALine: Integer = 0; const AColumn: Integer = 0);
    procedure AssignLexicalBinding(const AName: string; const AValue: TGocciaValue; const ALine: Integer = 0; const AColumn: Integer = 0); virtual;

    // Helper methods for token-based declarations
    procedure DefineFromToken(const AName: string; const AValue: TGocciaValue; const ATokenType: TGocciaTokenType);

    // Core methods
    function GetLexicalBinding(const AName: string; const ALine: Integer = 0; const AColumn: Integer = 0): TLexicalBinding;
    function GetValue(const AName: string): TGocciaValue; inline;

    function ResolveIdentifier(const AName: string): TGocciaValue; inline;
    function ContainsOwnLexicalBinding(const AName: string): Boolean; inline;
    function Contains(const AName: string): Boolean; inline;

    // Walk the parent chain to find the nearest this / owning class / superclass
    function FindThisValue: TGocciaValue;
    function FindOwningClass: TGocciaValue;
    function FindSuperClass: TGocciaValue;

    property Parent: TGocciaScope read FParent;
    property ThisValue: TGocciaValue read FThisValue write FThisValue;
    function GetThisProperty(const AName: string): TGocciaValue;
    property ScopeKind: TGocciaScopeKind read FScopeKind;
    property CustomLabel: string read FCustomLabel;
    property OnError: TGocciaThrowErrorCallback read FOnError write FOnError;
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
    procedure GCMarkReferences; override;
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
    procedure GCMarkReferences; override;
    property OwningClass: TGocciaValue read FOwningClass;
  end;

  // Specialized scope for try-catch blocks with proper assignment propagation
  TGocciaCatchScope = class(TGocciaScope)
  private
    FCatchParameter: string;  // Track the catch parameter name for proper shadowing
  public
    constructor Create(const AParent: TGocciaScope; const ACatchParameter: string);
    procedure AssignLexicalBinding(const AName: string; const AValue: TGocciaValue; const ALine: Integer = 0; const AColumn: Integer = 0); override;
  end;




implementation

uses
  Goccia.GarbageCollector,
  Goccia.Values.ClassHelper;

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

constructor TGocciaScope.Create(const AParent: TGocciaScope = nil; const AScopeKind: TGocciaScopeKind = skUnknown; const ACustomLabel: string = ''; const ACapacity: Integer = 0);
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

function TGocciaScope.FindThisValue: TGocciaValue;
var
  Current: TGocciaScope;
begin
  Current := Self;
  while Assigned(Current) do
  begin
    Result := Current.GetThisValue;
    if Assigned(Result) then Exit;
    Current := Current.FParent;
  end;
  Result := nil;
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

  // All declaration types are immediately initialized:
  // - dtConst: parser validates initializers; 'const x = undefined;' is valid
  // - dtLet: no TDZ after declaration statement is processed
  // - dtParameter: parameters have no TDZ
  LexicalBinding.Initialized := True;

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

procedure TGocciaScope.AssignLexicalBinding(const AName: string; const AValue: TGocciaValue; const ALine: Integer = 0; const AColumn: Integer = 0);
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

function TGocciaScope.GetLexicalBinding(const AName: string; const ALine: Integer = 0; const AColumn: Integer = 0): TLexicalBinding;
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

function TGocciaScope.ResolveIdentifier(const AName: string): TGocciaValue;
begin
  if AName = KEYWORD_THIS then
    Result := FThisValue
  else
    Result := GetValue(AName);
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

procedure TGocciaMethodCallScope.GCMarkReferences;
begin
  inherited;
  if Assigned(FSuperClass) then
    FSuperClass.GCMarkReferences;
  if Assigned(FOwningClass) then
    FOwningClass.GCMarkReferences;
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

procedure TGocciaClassInitScope.GCMarkReferences;
begin
  inherited;
  if Assigned(FOwningClass) then
    FOwningClass.GCMarkReferences;
end;

{ TGocciaCatchScope }

constructor TGocciaCatchScope.Create(const AParent: TGocciaScope; const ACatchParameter: string);
begin
  inherited Create(AParent, skBlock, 'CatchBlock');
  FCatchParameter := ACatchParameter;
end;

procedure TGocciaCatchScope.AssignLexicalBinding(const AName: string; const AValue: TGocciaValue; const ALine: Integer = 0; const AColumn: Integer = 0);
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

end.
