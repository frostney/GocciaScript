// This unit defines TGocciaFunctionValue for the tree-walk interpreter's
// AST-backed functions. TGocciaArrowFunctionValue handles arrow functions
// (lexical this). TGocciaMethodValue handles class methods (with super/owning
// class).
//
// These values hold a function's parameters, body and closure and do the
// value-level part of a call. Running the body is the evaluator's job:
// Goccia.Evaluator.Calls registers the call paths, so this unit does not use
// the evaluator.
unit Goccia.Values.FunctionValue;

{$I Goccia.inc}

interface

uses
  Generics.Collections,

  Goccia.Arguments.Collection,
  Goccia.AST.Expressions,
  Goccia.AST.Node,
  Goccia.Scope,
  Goccia.Values.FunctionBase,
  Goccia.Values.Primitives;

type
  TGocciaFunctionValue = class;

  { Runs AFunction's body in ACallScope, which the caller created, rooted and
    published as the running function's context. }
  TGocciaFunctionBodyRunner = function(const AFunction: TGocciaFunctionValue;
    const ACallScope: TGocciaScope;
    const AArguments: TGocciaArgumentsCollection;
    const AThisValue: TGocciaValue): TGocciaValue;
  { Starts an async function and returns its promise. }
  TGocciaAsyncFunctionStarter = function(const AFunction: TGocciaFunctionValue;
    const AArguments: TGocciaArgumentsCollection;
    const AThisValue: TGocciaValue): TGocciaValue;

  TGocciaFunctionValue = class(TGocciaFunctionBase)
  protected
    FName: string;
    FParameters: TGocciaParameterArray;
    FBodyStatements: TObjectList<TGocciaASTNode>;
    FClosure: TGocciaScope;
    FSourceFilePath: string;
    FSourceLine: Integer;
    FSourceColumn: Integer;
    FTrackCoverage: Boolean;
    FSourceText: string;
    FHideNestedFunctionSourceText: Boolean;
    FIsExpressionBody: Boolean;
    FIsSimpleParams: Boolean;
    // Memoized arguments-object decision: 0 = not yet computed, 1 = may
    // reference (create the object), 2 = provably unused (elide). Reset by
    // SetSourceText so the lazy scan re-runs if the source span changes.
    FArgumentsObjectState: Byte;
    function GetFunctionLength: Integer; override;
    function GetFunctionName: string; override;
    function GetSourceText: string; override;
    procedure SetSourceText(const AValue: string);
    function ExecuteBody(const ACallScope: TGocciaScope; const AArguments: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function CallAsync(const AArguments: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function CallWithNewTarget(const AArguments: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue; const ANewTarget: TGocciaValue): TGocciaValue;
  public
    constructor Create(const AParameters: TGocciaParameterArray; const ABodyStatements: TObjectList<TGocciaASTNode>; const AClosure: TGocciaScope; const AName: string = '');
    destructor Destroy; override;

    function Call(const AArguments: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue; override;
    function ConstructWithReceiver(const AArguments: TGocciaArgumentsCollection;
      const AReceiver: TGocciaValue; const ANewTarget: TGocciaValue): TGocciaValue; override;
    procedure MarkReferences; override;
    procedure SetInferredName(const AName: string);
    procedure SetSourceLocation(const AFilePath: string;
      const ALine, AColumn: Integer; const ATrackCoverage: Boolean);

    { The parts of a call that depend on the kind of function. The
      interpreter's call paths drive them. }
    procedure BindThis(const ACallScope: TGocciaScope; const AThisValue: TGocciaValue); virtual;
    function CreateCallScope: TGocciaScope; virtual;
    function CreatesArgumentsObject: Boolean; virtual;
    procedure RecordCoverageCall;

    property Parameters: TGocciaParameterArray read FParameters;
    property BodyStatements: TObjectList<TGocciaASTNode> read FBodyStatements;
    property Closure: TGocciaScope read FClosure;
    property Name: string read FName write FName;
    { True when no parameter is a rest, a pattern or has a default. }
    property IsSimpleParams: Boolean read FIsSimpleParams;
    property TrackCoverage: Boolean read FTrackCoverage;
    property IsExpressionBody: Boolean read FIsExpressionBody write FIsExpressionBody;
    property HideNestedFunctionSourceText: Boolean read FHideNestedFunctionSourceText write FHideNestedFunctionSourceText;
    property SourceFilePath: string read FSourceFilePath write FSourceFilePath;
    property SourceLine: Integer read FSourceLine write FSourceLine;
    property SourceColumn: Integer read FSourceColumn write FSourceColumn;
    property SourceText: string read FSourceText write SetSourceText;
  end;

  TGocciaArrowFunctionValue = class(TGocciaFunctionValue)
  public
    function CreateCallScope: TGocciaScope; override;
    function CreatesArgumentsObject: Boolean; override;
    procedure BindThis(const ACallScope: TGocciaScope; const AThisValue: TGocciaValue); override;
  end;

  TGocciaMethodValue = class(TGocciaFunctionValue)
  private
    FSuperClass: TGocciaValue;
    FOwningClass: TGocciaValue; // TGocciaClassValue stored as TGocciaValue to avoid circular dependency
    FLastSuperConstructorCalled: Boolean;
  public
    function CreateCallScope: TGocciaScope; override;
    constructor Create(const AParameters: TGocciaParameterArray; const ABodyStatements: TObjectList<TGocciaASTNode>; const AClosure: TGocciaScope; const AName: string; const ASuperClass: TGocciaValue = nil);
    function CallWithThisValue(const AArguments: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue; out AFinalThisValue: TGocciaValue;
      const ANewTarget: TGocciaValue = nil): TGocciaValue;
    procedure MarkReferences; override;

    property SuperClass: TGocciaValue read FSuperClass write FSuperClass;
    property OwningClass: TGocciaValue read FOwningClass write FOwningClass;
    property LastSuperConstructorCalled: Boolean
      read FLastSuperConstructorCalled;
  end;

{ Registers how the interpreter runs an AST-backed function's body.
  Goccia.Evaluator.Calls does this when it is linked. }
procedure RegisterFunctionCallPaths(const ARunBody: TGocciaFunctionBodyRunner;
  const AStartAsync: TGocciaAsyncFunctionStarter);

implementation

uses
  SysUtils,

  Goccia.Coverage,
  Goccia.GarbageCollector,
  Goccia.Realm,
  Goccia.Values.ToObject;

var
  GRunFunctionBody: TGocciaFunctionBodyRunner;
  GStartAsyncFunction: TGocciaAsyncFunctionStarter;

procedure RegisterFunctionCallPaths(const ARunBody: TGocciaFunctionBodyRunner;
  const AStartAsync: TGocciaAsyncFunctionStarter);
begin
  GRunFunctionBody := ARunBody;
  GStartAsyncFunction := AStartAsync;
end;

procedure RaiseNoInterpreterCallPaths;
begin
  raise Exception.Create(
    'An AST-backed function was called, but no interpreter call paths are registered');
end;

{ TGocciaFunctionValue }

constructor TGocciaFunctionValue.Create(const AParameters: TGocciaParameterArray; const ABodyStatements: TObjectList<TGocciaASTNode>; const AClosure: TGocciaScope; const AName: string = '');
var
  I: Integer;
begin
  FParameters := AParameters;
  FBodyStatements := ABodyStatements;
  FClosure := AClosure;
  FName := AName;
  FSourceColumn := 0;
  FTrackCoverage := False;

  // Pre-compute whether all parameters are simple named params (no rest, no destructuring, no defaults)
  FIsSimpleParams := True;
  for I := 0 to Length(FParameters) - 1 do
  begin
    if FParameters[I].IsRest or FParameters[I].IsPattern or Assigned(FParameters[I].DefaultValue) then
    begin
      FIsSimpleParams := False;
      Break;
    end;
  end;

  inherited Create;
end;

destructor TGocciaFunctionValue.Destroy;
begin
  FBodyStatements.Free;
  inherited;
end;

procedure TGocciaFunctionValue.MarkReferences;
begin
  if GCMarked then Exit;
  inherited; // Marks self + object properties/prototype

  // Mark the closure scope
  if Assigned(FClosure) then
    FClosure.MarkReferences;
end;

procedure TGocciaFunctionValue.BindThis(const ACallScope: TGocciaScope; const AThisValue: TGocciaValue);
var
  Root: TGocciaScope;
begin
  if FStrictThis then
    ACallScope.ThisValue := AThisValue
  else if not Assigned(AThisValue) or
          (AThisValue is TGocciaUndefinedLiteralValue) or
          (AThisValue is TGocciaNullLiteralValue) then
  begin
    Root := FClosure;
    while Assigned(Root.Parent) do
      Root := Root.Parent;
    ACallScope.ThisValue := Root.ThisValue;
  end
  else
    ACallScope.ThisValue := CoerceNonStrictThis(AThisValue, nil);
end;

function TGocciaFunctionValue.CreateCallScope: TGocciaScope;
begin
  Result := TGocciaCallScope.Create(FClosure, FName, Length(FParameters) + 2);
end;

procedure TGocciaFunctionValue.SetSourceText(const AValue: string);
begin
  FSourceText := AValue;
  FArgumentsObjectState := 0;
end;

function TGocciaFunctionValue.CreatesArgumentsObject: Boolean;
begin
  // A non-arrow function only needs an implicit arguments object when its body
  // can reference one. Decide once from the source span (memoized) so the
  // ~per-call cost is a single byte test; see
  // FunctionSourceMayReferenceArgumentsObject for why the scan is sound.
  if FArgumentsObjectState = 0 then
  begin
    if FunctionSourceMayReferenceArgumentsObject(FSourceText) then
      FArgumentsObjectState := 1
    else
      FArgumentsObjectState := 2;
  end;
  Result := FArgumentsObjectState = 1;
end;

procedure TGocciaFunctionValue.RecordCoverageCall;
begin
  if FTrackCoverage and (TGocciaCoverageTracker.Instance <> nil) and
     TGocciaCoverageTracker.Instance.Enabled and (FSourceLine > 0) and
     (FSourceFilePath <> '') then
  begin
    TGocciaCoverageTracker.Instance.RecordLineHit(FSourceFilePath, FSourceLine);
    TGocciaCoverageTracker.Instance.RecordFunctionHit(FSourceFilePath,
      GetFunctionName, FSourceLine, FSourceColumn);
  end;
end;

function TGocciaFunctionValue.ExecuteBody(const ACallScope: TGocciaScope;
  const AArguments: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  if not Assigned(GRunFunctionBody) then
    RaiseNoInterpreterCallPaths;
  Result := GRunFunctionBody(Self, ACallScope, AArguments, AThisValue);
end;

function TGocciaFunctionValue.CallAsync(
  const AArguments: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  if not Assigned(GStartAsyncFunction) then
    RaiseNoInterpreterCallPaths;
  Result := GStartAsyncFunction(Self, AArguments, AThisValue);
end;

function TGocciaFunctionValue.CallWithNewTarget(
  const AArguments: TGocciaArgumentsCollection; const AThisValue: TGocciaValue;
  const ANewTarget: TGocciaValue): TGocciaValue;
var
  CallScope: TGocciaScope;
  GC: TGarbageCollector;
  SelfRooted: Boolean;
  CallScopeRooted: Boolean;
  FunctionContextPushed: Boolean;
begin
  GC := TGarbageCollector.Instance;
  SelfRooted := False;
  CallScopeRooted := False;
  FunctionContextPushed := False;
  CallScope := nil;
  if Assigned(GC) then
  begin
    GC.PushActiveRoot(Self);
    SelfRooted := True;
  end;
  try
    CallScope := CreateCallScope;
    if Assigned(ANewTarget) and (CallScope is TGocciaCallScope) then
      TGocciaCallScope(CallScope).NewTarget := ANewTarget;
    if Assigned(GC) then
    begin
      GC.PushActiveRoot(CallScope);
      CallScopeRooted := True;
    end;
    PushCurrentFunctionExecutionContext(CallScope, Self);
    FunctionContextPushed := True;
    Result := ExecuteBody(CallScope, AArguments, AThisValue);
  finally
    if FunctionContextPushed then
      PopCurrentFunctionExecutionContext;
    if Assigned(GC) then
    begin
      if CallScopeRooted then
        GC.PopActiveRoot;
      if SelfRooted then
        GC.PopActiveRoot;
    end;
  end;
end;

function TGocciaFunctionValue.Call(const AArguments: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := CallWithNewTarget(AArguments, AThisValue, nil);
end;

function TGocciaFunctionValue.ConstructWithReceiver(
  const AArguments: TGocciaArgumentsCollection; const AReceiver: TGocciaValue;
  const ANewTarget: TGocciaValue): TGocciaValue;
begin
  Result := CallWithNewTarget(AArguments, AReceiver, ANewTarget);
end;

function TGocciaFunctionValue.GetFunctionLength: Integer;
var
  I: Integer;
begin
  // ECMAScript: length is the number of formal parameters before the first
  // one with a default value or rest parameter
  Result := 0;
  for I := 0 to Length(FParameters) - 1 do
  begin
    if FParameters[I].IsRest then Break;
    if Assigned(FParameters[I].DefaultValue) then Break;
    Inc(Result);
  end;
end;

function TGocciaFunctionValue.GetFunctionName: string;
begin
  Result := FName;
end;

function TGocciaFunctionValue.GetSourceText: string;
begin
  Result := FSourceText;
end;

procedure TGocciaFunctionValue.SetInferredName(const AName: string);
begin
  if FName = '' then
    FName := AName;
  if FTrackCoverage and (TGocciaCoverageTracker.Instance <> nil) and
     TGocciaCoverageTracker.Instance.Enabled and
     (FSourceFilePath <> '') and (FSourceLine > 0) then
    TGocciaCoverageTracker.Instance.RegisterFunction(FSourceFilePath,
      GetFunctionName, FSourceLine, FSourceColumn);
end;

procedure TGocciaFunctionValue.SetSourceLocation(const AFilePath: string;
  const ALine, AColumn: Integer; const ATrackCoverage: Boolean);
begin
  FSourceFilePath := AFilePath;
  FSourceLine := ALine;
  FSourceColumn := AColumn;
  FTrackCoverage := ATrackCoverage;
  if FTrackCoverage and (TGocciaCoverageTracker.Instance <> nil) and
     TGocciaCoverageTracker.Instance.Enabled and
     (FSourceFilePath <> '') and (FSourceLine > 0) then
    TGocciaCoverageTracker.Instance.RegisterFunction(FSourceFilePath,
      GetFunctionName, FSourceLine, FSourceColumn);
end;

{ TGocciaArrowFunctionValue }

function TGocciaArrowFunctionValue.CreatesArgumentsObject: Boolean;
begin
  Result := False;
end;

function TGocciaArrowFunctionValue.CreateCallScope: TGocciaScope;
begin
  Result := TGocciaArrowCallScope.Create(FClosure, FName, Length(FParameters) + 2);
end;

procedure TGocciaArrowFunctionValue.BindThis(const ACallScope: TGocciaScope; const AThisValue: TGocciaValue);
begin
  ACallScope.ThisValue := FClosure.ThisValue;
end;

{ TGocciaMethodValue }

constructor TGocciaMethodValue.Create(const AParameters: TGocciaParameterArray; const ABodyStatements: TObjectList<TGocciaASTNode>; const AClosure: TGocciaScope; const AName: string; const ASuperClass: TGocciaValue = nil);
begin
  inherited Create(AParameters, ABodyStatements, AClosure, AName);
  FSuperClass := ASuperClass;
  FLastSuperConstructorCalled := False;
end;

function TGocciaMethodValue.CallWithThisValue(
  const AArguments: TGocciaArgumentsCollection; const AThisValue: TGocciaValue;
  out AFinalThisValue: TGocciaValue;
  const ANewTarget: TGocciaValue): TGocciaValue;
var
  CallScope: TGocciaScope;
  GC: TGarbageCollector;
  SelfRooted: Boolean;
  CallScopeRooted: Boolean;
  FunctionContextPushed: Boolean;
begin
  AFinalThisValue := AThisValue;
  GC := TGarbageCollector.Instance;
  SelfRooted := False;
  CallScopeRooted := False;
  FunctionContextPushed := False;
  CallScope := nil;
  if Assigned(GC) then
  begin
    GC.PushActiveRoot(Self);
    SelfRooted := True;
  end;
  try
    CallScope := CreateCallScope;
    if Assigned(ANewTarget) and (CallScope is TGocciaMethodCallScope) then
      TGocciaMethodCallScope(CallScope).NewTarget := ANewTarget;
    if Assigned(GC) then
    begin
      GC.PushActiveRoot(CallScope);
      CallScopeRooted := True;
    end;
    PushCurrentFunctionExecutionContext(CallScope, Self);
    FunctionContextPushed := True;
    if CallScope is TGocciaMethodCallScope then
      TGocciaMethodCallScope(CallScope).SuperConstructorCalled := False;
    Result := ExecuteBody(CallScope, AArguments, AThisValue);
    if (CallScope is TGocciaMethodCallScope) and
       Assigned(TGocciaMethodCallScope(CallScope).SuperClass) and
       not TGocciaMethodCallScope(CallScope).SuperConstructorCalled then
      AFinalThisValue := TGocciaUndefinedLiteralValue.UndefinedValue
    else
      AFinalThisValue := CallScope.ThisValue;
    if CallScope is TGocciaMethodCallScope then
      FLastSuperConstructorCalled :=
        TGocciaMethodCallScope(CallScope).SuperConstructorCalled
    else
      FLastSuperConstructorCalled := False;
  finally
    if FunctionContextPushed then
      PopCurrentFunctionExecutionContext;
    if Assigned(GC) then
    begin
      if CallScopeRooted then
        GC.PopActiveRoot;
      if SelfRooted then
        GC.PopActiveRoot;
    end;
  end;
end;

function TGocciaMethodValue.CreateCallScope: TGocciaScope;
begin
  Result := TGocciaMethodCallScope.Create(FClosure, FName, FSuperClass, FOwningClass, Length(FParameters) + 2);
end;

procedure TGocciaMethodValue.MarkReferences;
begin
  if GCMarked then Exit;
  inherited; // Marks self + object properties/prototype + closure

  // Mark method-specific references
  if Assigned(FSuperClass) then
    FSuperClass.MarkReferences;
  if Assigned(FOwningClass) then
    FOwningClass.MarkReferences;
end;

end.
