unit Goccia.ExecutionContext;

{$I Goccia.inc}

interface

uses
  Goccia.Realm,
  Goccia.Scope,
  Goccia.Values.Primitives;

type
  // ECMA-262 execution context slice used by runtime execution paths.
  // Spec fields that are not modelled yet stay nil/empty rather than being
  // inferred from process-global state.
  TGocciaExecutionContext = record
    Realm: TGocciaRealm;
    Scope: TGocciaScope;
    FunctionValue: TGocciaValue;
    ScriptOrModule: TObject;
    SourcePath: string;
  end;

  TGocciaExecutionContextStack = class
  public
    class procedure Push(const AContext: TGocciaExecutionContext); static;
    class function Pop: TGocciaExecutionContext; static;
    class procedure PushFunction(const AScope: TGocciaScope;
      const AFunctionValue: TGocciaValue); static;
    class procedure PopFunction; static;
    class function Running: TGocciaExecutionContext; static;
    class function HasRunning: Boolean; static;
    class function CurrentRealm: TGocciaRealm; static;
  end;

  TGocciaExecutionContextScope = class
  private
    FPopped: Boolean;
  public
    constructor Create(const AContext: TGocciaExecutionContext);
    destructor Destroy; override;
    procedure Pop;
  end;

function CreateExecutionContext(const ARealm: TGocciaRealm;
  const AScope: TGocciaScope; const ASourcePath: string;
  const AScriptOrModule: TObject = nil;
  const AFunctionValue: TGocciaValue = nil): TGocciaExecutionContext;

function RunningExecutionContext: TGocciaExecutionContext; inline;
function HasRunningExecutionContext: Boolean; inline;

implementation

uses
  SysUtils;

type
  TGocciaExecutionContextStackEntry = record
    Context: TGocciaExecutionContext;
    PreviousRealm: TGocciaRealm;
  end;

  TGocciaFunctionExecutionContextEntry = record
    Scope: TGocciaScope;
    FunctionValue: TGocciaValue;
  end;

threadvar
  GExecutionContextStack: array of TGocciaExecutionContextStackEntry;
  GExecutionContextStackCount: Integer;
  GFunctionExecutionContextStack: array of TGocciaFunctionExecutionContextEntry;
  GFunctionExecutionContextStackCount: Integer;

function CreateExecutionContext(const ARealm: TGocciaRealm;
  const AScope: TGocciaScope; const ASourcePath: string;
  const AScriptOrModule: TObject;
  const AFunctionValue: TGocciaValue): TGocciaExecutionContext;
begin
  Result.Realm := ARealm;
  Result.Scope := AScope;
  Result.FunctionValue := AFunctionValue;
  Result.ScriptOrModule := AScriptOrModule;
  Result.SourcePath := ASourcePath;
end;

function RunningExecutionContext: TGocciaExecutionContext;
begin
  Result := TGocciaExecutionContextStack.Running;
end;

function HasRunningExecutionContext: Boolean;
begin
  Result := TGocciaExecutionContextStack.HasRunning;
end;

{ TGocciaExecutionContextStack }

class procedure TGocciaExecutionContextStack.Push(
  const AContext: TGocciaExecutionContext);
begin
  if not Assigned(AContext.Realm) then
    raise Exception.Create('Execution context requires a realm.');

  if GExecutionContextStackCount >= Length(GExecutionContextStack) then
    SetLength(GExecutionContextStack, GExecutionContextStackCount * 2 + 8);

  GExecutionContextStack[GExecutionContextStackCount].Context := AContext;
  GExecutionContextStack[GExecutionContextStackCount].PreviousRealm :=
    Goccia.Realm.CurrentRealm;
  Inc(GExecutionContextStackCount);

  SetCurrentRealm(AContext.Realm);
end;

class function TGocciaExecutionContextStack.Pop: TGocciaExecutionContext;
var
  PreviousRealm: TGocciaRealm;
begin
  if GExecutionContextStackCount <= 0 then
    raise Exception.Create('Execution context stack underflow.');

  Dec(GExecutionContextStackCount);
  Result := GExecutionContextStack[GExecutionContextStackCount].Context;
  PreviousRealm := GExecutionContextStack[GExecutionContextStackCount].PreviousRealm;
  FillChar(GExecutionContextStack[GExecutionContextStackCount],
    SizeOf(GExecutionContextStack[GExecutionContextStackCount]), 0);
  SetCurrentRealm(PreviousRealm);
end;

class procedure TGocciaExecutionContextStack.PushFunction(
  const AScope: TGocciaScope; const AFunctionValue: TGocciaValue);
begin
  if GExecutionContextStackCount <= 0 then
    raise Exception.Create('Function execution context requires a running context.');
  if not Assigned(AFunctionValue) then
    raise Exception.Create('Function execution context requires a function value.');

  if GFunctionExecutionContextStackCount >= Length(GFunctionExecutionContextStack) then
    SetLength(GFunctionExecutionContextStack,
      GFunctionExecutionContextStackCount * 2 + 8);

  GFunctionExecutionContextStack[GFunctionExecutionContextStackCount].Scope :=
    AScope;
  GFunctionExecutionContextStack[GFunctionExecutionContextStackCount].FunctionValue :=
    AFunctionValue;
  Inc(GFunctionExecutionContextStackCount);
end;

class procedure TGocciaExecutionContextStack.PopFunction;
begin
  if GFunctionExecutionContextStackCount <= 0 then
    raise Exception.Create('Function execution context stack underflow.');

  Dec(GFunctionExecutionContextStackCount);
  FillChar(GFunctionExecutionContextStack[GFunctionExecutionContextStackCount],
    SizeOf(GFunctionExecutionContextStack[GFunctionExecutionContextStackCount]),
    0);
end;

class function TGocciaExecutionContextStack.Running: TGocciaExecutionContext;
begin
  if GExecutionContextStackCount > 0 then
    Result := GExecutionContextStack[GExecutionContextStackCount - 1].Context
  else
    FillChar(Result, SizeOf(Result), 0);

  if Goccia.Realm.HasCurrentFunctionExecutionContext then
  begin
    Result.Scope := TGocciaScope(
      Goccia.Realm.CurrentFunctionExecutionContextScope);
    Result.FunctionValue := TGocciaValue(
      Goccia.Realm.CurrentFunctionExecutionContextValue);
  end
  else if GFunctionExecutionContextStackCount > 0 then
  begin
    Result.Scope :=
      GFunctionExecutionContextStack[GFunctionExecutionContextStackCount - 1].Scope;
    Result.FunctionValue :=
      GFunctionExecutionContextStack[GFunctionExecutionContextStackCount - 1].FunctionValue;
  end;
end;

class function TGocciaExecutionContextStack.HasRunning: Boolean;
begin
  Result := GExecutionContextStackCount > 0;
end;

class function TGocciaExecutionContextStack.CurrentRealm: TGocciaRealm;
begin
  if GExecutionContextStackCount > 0 then
    Result := GExecutionContextStack[GExecutionContextStackCount - 1].Context.Realm
  else
    Result := Goccia.Realm.CurrentRealm;
end;

{ TGocciaExecutionContextScope }

constructor TGocciaExecutionContextScope.Create(
  const AContext: TGocciaExecutionContext);
begin
  inherited Create;
  FPopped := False;
  TGocciaExecutionContextStack.Push(AContext);
end;

destructor TGocciaExecutionContextScope.Destroy;
begin
  Pop;
  inherited;
end;

procedure TGocciaExecutionContextScope.Pop;
begin
  if FPopped then
    Exit;
  TGocciaExecutionContextStack.Pop;
  FPopped := True;
end;

finalization
  SetLength(GExecutionContextStack, 0);
  GExecutionContextStackCount := 0;
  SetLength(GFunctionExecutionContextStack, 0);
  GFunctionExecutionContextStackCount := 0;

end.
