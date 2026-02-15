unit Goccia.GC;

{$I Goccia.inc}

interface

uses
  Goccia.Values.Primitives, Goccia.Scope,
  Generics.Collections, SysUtils;

type
  TGocciaGC = class
  private class var
    FInstance: TGocciaGC;
  private
    FManagedValues: TList<TGocciaValue>;
    FManagedScopes: TList<TGocciaScope>;
    FRootScopes: TList<TGocciaScope>;
    FActiveScopeStack: TList<TGocciaScope>;
    FPinnedValues: TList<TGocciaValue>;
    FTempRoots: TList<TGocciaValue>;

    FAllocationsSinceLastGC: Integer;
    FGCThreshold: Integer;
    FEnabled: Boolean;
    FCollecting: Boolean;
    FTotalCollected: Int64;
    FTotalCollections: Integer;

    procedure MarkPhase;
    procedure SweepPhase;
    function GetManagedValueCount: Integer;
    function GetManagedScopeCount: Integer;
  public
    class function Instance: TGocciaGC;
    class procedure Initialize;
    class procedure Shutdown;

    constructor Create;
    destructor Destroy; override;

    procedure RegisterValue(Value: TGocciaValue);
    procedure RegisterScope(Scope: TGocciaScope);
    procedure PinValue(Value: TGocciaValue);
    procedure AddRoot(Scope: TGocciaScope);
    procedure RemoveRoot(Scope: TGocciaScope);
    procedure PushActiveScope(Scope: TGocciaScope);
    procedure PopActiveScope;
    procedure AddTempRoot(Value: TGocciaValue);
    procedure RemoveTempRoot(Value: TGocciaValue);

    procedure Collect;
    procedure CollectIfNeeded;

    property Enabled: Boolean read FEnabled write FEnabled;
    property Threshold: Integer read FGCThreshold write FGCThreshold;
    property TotalCollected: Int64 read FTotalCollected;
    property TotalCollections: Integer read FTotalCollections;
    property ManagedValueCount: Integer read GetManagedValueCount;
    property ManagedScopeCount: Integer read GetManagedScopeCount;
  end;

implementation

const
  DEFAULT_GC_THRESHOLD = 10000;

{ TGocciaGC }

class function TGocciaGC.Instance: TGocciaGC;
begin
  Result := FInstance;
end;

class procedure TGocciaGC.Initialize;
begin
  if not Assigned(FInstance) then
    FInstance := TGocciaGC.Create;
end;

class procedure TGocciaGC.Shutdown;
begin
  FreeAndNil(FInstance);
end;

constructor TGocciaGC.Create;
begin
  FManagedValues := TList<TGocciaValue>.Create;
  FManagedScopes := TList<TGocciaScope>.Create;
  FRootScopes := TList<TGocciaScope>.Create;
  FActiveScopeStack := TList<TGocciaScope>.Create;
  FPinnedValues := TList<TGocciaValue>.Create;
  FTempRoots := TList<TGocciaValue>.Create;
  FAllocationsSinceLastGC := 0;
  FGCThreshold := DEFAULT_GC_THRESHOLD;
  FEnabled := True;
  FCollecting := False;
  FTotalCollected := 0;
  FTotalCollections := 0;
end;

destructor TGocciaGC.Destroy;
begin
  // Don't free managed objects on shutdown - OS reclaims everything.
  // Freeing in arbitrary order could cause use-after-free in destructors.
  FManagedValues.Free;
  FManagedScopes.Free;
  FRootScopes.Free;
  FActiveScopeStack.Free;
  FPinnedValues.Free;
  FTempRoots.Free;
  inherited;
end;

procedure TGocciaGC.RegisterValue(Value: TGocciaValue);
begin
  FManagedValues.Add(Value);
  Inc(FAllocationsSinceLastGC);
end;

procedure TGocciaGC.RegisterScope(Scope: TGocciaScope);
begin
  FManagedScopes.Add(Scope);
  Inc(FAllocationsSinceLastGC);
end;

procedure TGocciaGC.PinValue(Value: TGocciaValue);
begin
  if not FPinnedValues.Contains(Value) then
    FPinnedValues.Add(Value);
end;

procedure TGocciaGC.AddRoot(Scope: TGocciaScope);
begin
  if not FRootScopes.Contains(Scope) then
    FRootScopes.Add(Scope);
end;

procedure TGocciaGC.RemoveRoot(Scope: TGocciaScope);
begin
  FRootScopes.Remove(Scope);
end;

procedure TGocciaGC.PushActiveScope(Scope: TGocciaScope);
begin
  FActiveScopeStack.Add(Scope);
end;

procedure TGocciaGC.PopActiveScope;
begin
  if FActiveScopeStack.Count > 0 then
    FActiveScopeStack.Delete(FActiveScopeStack.Count - 1);
end;

procedure TGocciaGC.AddTempRoot(Value: TGocciaValue);
begin
  if Assigned(Value) and (FTempRoots.IndexOf(Value) < 0) then
    FTempRoots.Add(Value);
end;

procedure TGocciaGC.RemoveTempRoot(Value: TGocciaValue);
begin
  FTempRoots.Remove(Value);
end;

procedure TGocciaGC.MarkPhase;
var
  I: Integer;
begin
  // Clear all marks
  for I := 0 to FManagedValues.Count - 1 do
    FManagedValues[I].GCMarked := False;
  for I := 0 to FManagedScopes.Count - 1 do
    FManagedScopes[I].GCMarked := False;

  // Mark pinned values (singletons - always reachable)
  for I := 0 to FPinnedValues.Count - 1 do
    FPinnedValues[I].GCMarkReferences;

  // Mark from root scopes (global scope etc.)
  for I := 0 to FRootScopes.Count - 1 do
    FRootScopes[I].GCMarkReferences;

  // Mark from active scope stack (currently executing functions)
  for I := 0 to FActiveScopeStack.Count - 1 do
    FActiveScopeStack[I].GCMarkReferences;

  // Mark temporary roots (values held by Pascal code outside the scope chain)
  for I := 0 to FTempRoots.Count - 1 do
    FTempRoots[I].GCMarkReferences;
end;

procedure TGocciaGC.SweepPhase;
var
  I, WriteIdx: Integer;
  Collected: Integer;
begin
  Collected := 0;

  // Sweep values: compact alive/permanent values to the front, free dead ones
  WriteIdx := 0;
  for I := 0 to FManagedValues.Count - 1 do
  begin
    if FManagedValues[I].GCMarked or FManagedValues[I].GCPermanent then
    begin
      FManagedValues[WriteIdx] := FManagedValues[I];
      Inc(WriteIdx);
    end
    else
    begin
      FManagedValues[I].Free;
      Inc(Collected);
    end;
  end;
  FManagedValues.Count := WriteIdx;

  // Sweep scopes: compact alive scopes to the front, free dead ones
  WriteIdx := 0;
  for I := 0 to FManagedScopes.Count - 1 do
  begin
    if FManagedScopes[I].GCMarked then
    begin
      FManagedScopes[WriteIdx] := FManagedScopes[I];
      Inc(WriteIdx);
    end
    else
    begin
      FManagedScopes[I].Free;
      Inc(Collected);
    end;
  end;
  FManagedScopes.Count := WriteIdx;

  FTotalCollected := FTotalCollected + Collected;
end;

procedure TGocciaGC.Collect;
begin
  if FCollecting then Exit;
  FCollecting := True;
  try
    MarkPhase;
    SweepPhase;
    FAllocationsSinceLastGC := 0;
    Inc(FTotalCollections);
  finally
    FCollecting := False;
  end;
end;

procedure TGocciaGC.CollectIfNeeded;
begin
  if FEnabled and (FAllocationsSinceLastGC >= FGCThreshold) and not FCollecting then
    Collect;
end;

function TGocciaGC.GetManagedValueCount: Integer;
begin
  Result := FManagedValues.Count;
end;

function TGocciaGC.GetManagedScopeCount: Integer;
begin
  Result := FManagedScopes.Count;
end;

end.
