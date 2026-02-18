unit Goccia.GarbageCollector;

{$I Goccia.inc}

interface

uses
  Generics.Collections,
  SysUtils,

  Goccia.Scope,
  Goccia.Values.Primitives;

type
  TGocciaGC = class
  private class var
    FInstance: TGocciaGC;
  private
    FManagedValues: TList<TGocciaValue>;
    FManagedScopes: TList<TGocciaScope>;
    FRootScopes: TDictionary<TGocciaScope, Boolean>;
    FActiveScopeStack: TList<TGocciaScope>;
    FPinnedValues: TDictionary<TGocciaValue, Boolean>;
    FTempRoots: TDictionary<TGocciaValue, Boolean>;

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

    procedure RegisterValue(const AValue: TGocciaValue);
    procedure UnregisterValue(const AValue: TGocciaValue);
    procedure RegisterScope(const AScope: TGocciaScope);
    procedure PinValue(const AValue: TGocciaValue);
    procedure AddRoot(const AScope: TGocciaScope);
    procedure RemoveRoot(const AScope: TGocciaScope);
    procedure PushActiveScope(const AScope: TGocciaScope);
    procedure PopActiveScope;
    procedure AddTempRoot(const AValue: TGocciaValue);
    procedure RemoveTempRoot(const AValue: TGocciaValue);

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
  FRootScopes := TDictionary<TGocciaScope, Boolean>.Create;
  FActiveScopeStack := TList<TGocciaScope>.Create;
  FPinnedValues := TDictionary<TGocciaValue, Boolean>.Create;
  FTempRoots := TDictionary<TGocciaValue, Boolean>.Create;
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

procedure TGocciaGC.RegisterValue(const AValue: TGocciaValue);
begin
  FManagedValues.Add(AValue);
  Inc(FAllocationsSinceLastGC);
end;

procedure TGocciaGC.UnregisterValue(const AValue: TGocciaValue);
begin
  FManagedValues.Remove(AValue);
end;

procedure TGocciaGC.RegisterScope(const AScope: TGocciaScope);
begin
  FManagedScopes.Add(AScope);
  Inc(FAllocationsSinceLastGC);
end;

procedure TGocciaGC.PinValue(const AValue: TGocciaValue);
begin
  if not FPinnedValues.ContainsKey(AValue) then
    FPinnedValues.Add(AValue, True);
end;

procedure TGocciaGC.AddRoot(const AScope: TGocciaScope);
begin
  if not FRootScopes.ContainsKey(AScope) then
    FRootScopes.Add(AScope, True);
end;

procedure TGocciaGC.RemoveRoot(const AScope: TGocciaScope);
begin
  FRootScopes.Remove(AScope);
end;

procedure TGocciaGC.PushActiveScope(const AScope: TGocciaScope);
begin
  FActiveScopeStack.Add(AScope);
end;

procedure TGocciaGC.PopActiveScope;
begin
  if FActiveScopeStack.Count > 0 then
    FActiveScopeStack.Delete(FActiveScopeStack.Count - 1);
end;

procedure TGocciaGC.AddTempRoot(const AValue: TGocciaValue);
begin
  if Assigned(AValue) and not FTempRoots.ContainsKey(AValue) then
    FTempRoots.Add(AValue, True);
end;

procedure TGocciaGC.RemoveTempRoot(const AValue: TGocciaValue);
begin
  FTempRoots.Remove(AValue);
end;

procedure TGocciaGC.MarkPhase;
var
  I: Integer;
  Value: TGocciaValue;
  Scope: TGocciaScope;
begin
  // Clear all marks
  for I := 0 to FManagedValues.Count - 1 do
    FManagedValues[I].GCMarked := False;
  for I := 0 to FManagedScopes.Count - 1 do
    FManagedScopes[I].GCMarked := False;

  // Mark pinned values (singletons - always reachable)
  for Value in FPinnedValues.Keys do
    Value.GCMarkReferences;

  // Mark from root scopes (global scope etc.)
  for Scope in FRootScopes.Keys do
    Scope.GCMarkReferences;

  // Mark from active scope stack (currently executing functions)
  for I := 0 to FActiveScopeStack.Count - 1 do
    FActiveScopeStack[I].GCMarkReferences;

  // Mark temporary roots (values held by Pascal code outside the scope chain)
  for Value in FTempRoots.Keys do
    Value.GCMarkReferences;
end;

procedure TGocciaGC.SweepPhase;
var
  I, WriteIdx: Integer;
  Collected: Integer;
begin
  Collected := 0;

  // Sweep values: compact alive values to the front, free dead ones
  WriteIdx := 0;
  for I := 0 to FManagedValues.Count - 1 do
  begin
    if FManagedValues[I].GCMarked then
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
