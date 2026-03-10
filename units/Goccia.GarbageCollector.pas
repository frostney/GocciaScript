unit Goccia.GarbageCollector;

{$I Goccia.inc}

interface

uses
  Generics.Collections,

  GarbageCollector.Generic,
  GarbageCollector.Managed,

  Goccia.Scope,
  Goccia.Values.Primitives;

type
  TGocciaGCExternalRootMarker = TGCRootMarker;

  TGocciaGarbageCollector = class(TGenericGarbageCollector)
  private
    FManagedScopes: TGocciaScopeList;
    FRootScopes: TDictionary<TGocciaScope, Boolean>;
    FActiveScopeStack: TGocciaScopeList;

    function GetManagedScopeCount: Integer;
  protected
    procedure MarkRoots; override;
  public
    class function Instance: TGocciaGarbageCollector; inline;
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

    procedure Collect; override;

    property ManagedScopeCount: Integer read GetManagedScopeCount;
  end;

implementation

uses
  SysUtils;

{ TGocciaGarbageCollector }

class function TGocciaGarbageCollector.Instance: TGocciaGarbageCollector;
begin
  Result := TGocciaGarbageCollector(FInstance);
end;

class procedure TGocciaGarbageCollector.Initialize;
begin
  if not Assigned(FInstance) then
    FInstance := TGocciaGarbageCollector.Create;
end;

class procedure TGocciaGarbageCollector.Shutdown;
begin
  inherited Shutdown;
end;

constructor TGocciaGarbageCollector.Create;
begin
  inherited Create;
  FManagedScopes := TGocciaScopeList.Create(False);
  FRootScopes := TDictionary<TGocciaScope, Boolean>.Create;
  FActiveScopeStack := TGocciaScopeList.Create(False);
end;

destructor TGocciaGarbageCollector.Destroy;
begin
  FManagedScopes.Free;
  FRootScopes.Free;
  FActiveScopeStack.Free;
  inherited;
end;

procedure TGocciaGarbageCollector.RegisterValue(const AValue: TGocciaValue);
begin
  RegisterObject(AValue);
end;

procedure TGocciaGarbageCollector.UnregisterValue(const AValue: TGocciaValue);
begin
  UnregisterObject(AValue);
end;

procedure TGocciaGarbageCollector.RegisterScope(const AScope: TGocciaScope);
begin
  FManagedScopes.Add(AScope);
end;

procedure TGocciaGarbageCollector.PinValue(const AValue: TGocciaValue);
begin
  PinObject(AValue);
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
  inherited AddTempRoot(AValue);
end;

procedure TGocciaGarbageCollector.RemoveTempRoot(const AValue: TGocciaValue);
begin
  inherited RemoveTempRoot(AValue);
end;

function TGocciaGarbageCollector.IsTempRoot(
  const AValue: TGocciaValue): Boolean;
begin
  Result := inherited IsTempRoot(AValue);
end;

procedure TGocciaGarbageCollector.MarkRoots;
var
  I: Integer;
  Scope: TGocciaScope;
begin
  inherited MarkRoots;

  for Scope in FRootScopes.Keys do
    Scope.MarkReferences;

  for I := 0 to FActiveScopeStack.Count - 1 do
    FActiveScopeStack[I].MarkReferences;
end;

procedure TGocciaGarbageCollector.Collect;
var
  I: Integer;
  WriteIdx: Integer;
begin
  if FCollecting then Exit;
  FCollecting := True;
  try
    for I := 0 to FManagedScopes.Count - 1 do
      FManagedScopes[I].GCMarked := False;

    ClearObjectMarks;
    MarkRoots;
    SweepObjects;

    WriteIdx := 0;
    for I := 0 to FManagedScopes.Count - 1 do
    begin
      if FManagedScopes[I].GCMarked then
      begin
        FManagedScopes[WriteIdx] := FManagedScopes[I];
        Inc(WriteIdx);
      end
      else
        FManagedScopes[I].Free;
    end;
    FManagedScopes.Count := WriteIdx;

    FAllocationsSinceLastGC := 0;
    Inc(FTotalCollections);
  finally
    FCollecting := False;
  end;
end;

function TGocciaGarbageCollector.GetManagedScopeCount: Integer;
begin
  Result := FManagedScopes.Count;
end;

end.
