unit Goccia.GarbageCollector;

{$I Goccia.inc}

interface

uses
  Generics.Collections,

  Goccia.Scope,
  Goccia.Values.Primitives;

type
  TGocciaGarbageCollector = class
  private class var
    FInstance: TGocciaGarbageCollector;
  private
    FManagedValues: TGocciaValueList;
    FManagedScopes: TGocciaScopeList;
    FRootScopes: TDictionary<TGocciaScope, Boolean>;
    FActiveScopeStack: TGocciaScopeList;
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
    class function Instance: TGocciaGarbageCollector;
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
    function IsTempRoot(const AValue: TGocciaValue): Boolean;

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

uses
  SysUtils;

const
  DEFAULT_GC_THRESHOLD = 10000;

{ TGocciaGarbageCollector }

class function TGocciaGarbageCollector.Instance: TGocciaGarbageCollector;
begin
  Result := FInstance;
end;

class procedure TGocciaGarbageCollector.Initialize;
begin
  if not Assigned(FInstance) then
    FInstance := TGocciaGarbageCollector.Create;
end;

class procedure TGocciaGarbageCollector.Shutdown;
begin
  FreeAndNil(FInstance);
end;

constructor TGocciaGarbageCollector.Create;
begin
  FManagedValues := TGocciaValueList.Create(False);
  FManagedScopes := TGocciaScopeList.Create(False);
  FRootScopes := TDictionary<TGocciaScope, Boolean>.Create;
  FActiveScopeStack := TGocciaScopeList.Create(False);
  FPinnedValues := TDictionary<TGocciaValue, Boolean>.Create;
  FTempRoots := TDictionary<TGocciaValue, Boolean>.Create;
  FAllocationsSinceLastGC := 0;
  FGCThreshold := DEFAULT_GC_THRESHOLD;
  FEnabled := True;
  FCollecting := False;
  FTotalCollected := 0;
  FTotalCollections := 0;
end;

destructor TGocciaGarbageCollector.Destroy;
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

procedure TGocciaGarbageCollector.RegisterValue(const AValue: TGocciaValue);
begin
  FManagedValues.Add(AValue);
  Inc(FAllocationsSinceLastGC);
end;

procedure TGocciaGarbageCollector.UnregisterValue(const AValue: TGocciaValue);
begin
  FManagedValues.Remove(AValue);
end;

procedure TGocciaGarbageCollector.RegisterScope(const AScope: TGocciaScope);
begin
  FManagedScopes.Add(AScope);
  Inc(FAllocationsSinceLastGC);
end;

procedure TGocciaGarbageCollector.PinValue(const AValue: TGocciaValue);
begin
  if not FPinnedValues.ContainsKey(AValue) then
    FPinnedValues.Add(AValue, True);
end;

procedure TGocciaGarbageCollector.AddRoot(const AScope: TGocciaScope);
begin
  if not FRootScopes.ContainsKey(AScope) then
    FRootScopes.Add(AScope, True);
end;

procedure TGocciaGarbageCollector.RemoveRoot(const AScope: TGocciaScope);
begin
  FRootScopes.Remove(AScope);
end;

procedure TGocciaGarbageCollector.PushActiveScope(const AScope: TGocciaScope);
begin
  FActiveScopeStack.Add(AScope);
end;

procedure TGocciaGarbageCollector.PopActiveScope;
begin
  if FActiveScopeStack.Count > 0 then
    FActiveScopeStack.Delete(FActiveScopeStack.Count - 1);
end;

procedure TGocciaGarbageCollector.AddTempRoot(const AValue: TGocciaValue);
begin
  if Assigned(AValue) and not FTempRoots.ContainsKey(AValue) then
    FTempRoots.Add(AValue, True);
end;

procedure TGocciaGarbageCollector.RemoveTempRoot(const AValue: TGocciaValue);
begin
  FTempRoots.Remove(AValue);
end;

function TGocciaGarbageCollector.IsTempRoot(const AValue: TGocciaValue): Boolean;
begin
  Result := Assigned(AValue) and FTempRoots.ContainsKey(AValue);
end;

procedure TGocciaGarbageCollector.MarkPhase;
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
    Value.MarkReferences;

  // Mark from root scopes (global scope etc.)
  for Scope in FRootScopes.Keys do
    Scope.MarkReferences;

  // Mark from active scope stack (currently executing functions)
  for I := 0 to FActiveScopeStack.Count - 1 do
    FActiveScopeStack[I].MarkReferences;

  // Mark temporary roots (values held by Pascal code outside the scope chain)
  for Value in FTempRoots.Keys do
    Value.MarkReferences;
end;

procedure TGocciaGarbageCollector.SweepPhase;
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

procedure TGocciaGarbageCollector.Collect;
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

procedure TGocciaGarbageCollector.CollectIfNeeded;
begin
  if FEnabled and (FAllocationsSinceLastGC >= FGCThreshold) and not FCollecting then
    Collect;
end;

function TGocciaGarbageCollector.GetManagedValueCount: Integer;
begin
  Result := FManagedValues.Count;
end;

function TGocciaGarbageCollector.GetManagedScopeCount: Integer;
begin
  Result := FManagedScopes.Count;
end;

end.
