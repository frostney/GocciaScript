unit Goccia.Builtins.Base;

{$I Goccia.inc}

interface

uses
  Goccia.Error.ThrowErrorCallback,
  Goccia.Scope,
  Goccia.Values.ObjectValue;

type
  { A live builtin's FBuiltinObject is a collector root, from the moment
    TGocciaBuiltin.Create makes it until the builtin is freed (issue #1655).
    A subclass constructor can run bytecode before it binds the object
    anywhere: reading a lazily loaded global such as Date loads its shim,
    which reaches the VM's memory-pressure checkpoint. Some builtins never
    bind FBuiltinObject in the global scope at all, yet the engine and runtime
    extensions read it later (static members, the goccia:test namespace).

    Every live builtin sits on its thread's intrusive list, which one root
    source per thread marks. Linking costs a few stores, where a root-set
    entry per builtin would cost a hash insert and removal each, on every
    engine boot. A subclass that keeps other values only in its own fields
    overrides MarkBuiltinReferences to mark them too. }
  TGocciaBuiltin = class
  private
    FPreviousLive: TGocciaBuiltin;
    FNextLive: TGocciaBuiltin;
    FLive: Boolean;
    procedure LinkLive;
    procedure UnlinkLive;
  protected
    FName: string;
    FScope: TGocciaScope;
    FBuiltinObject: TGocciaObjectValue;
    FThrowError: TGocciaThrowErrorCallback;
    procedure MarkBuiltinReferences; virtual;
  public
    constructor Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowErrorCallback);
    procedure AfterConstruction; override;
    destructor Destroy; override;

    property Name: string read FName;
    property BuiltinObject: TGocciaObjectValue read FBuiltinObject;
    property ThrowError: TGocciaThrowErrorCallback read FThrowError;
    // The next builtin on this thread's live list, for naming built-ins in a
    // heap snapshot (ADR 0148).
    property NextLive: TGocciaBuiltin read FNextLive;
  end;

// The newest live builtin on this thread, or nil.
function FirstLiveBuiltin: TGocciaBuiltin;

implementation

uses
  Goccia.GarbageCollector,
  Goccia.ThreadCleanupRegistry;

type
  { Holds the head of its thread's live-builtin list and marks every builtin
    on it. }
  TGocciaBuiltinRoots = class(TGCRootSource)
  private
    FHead: TGocciaBuiltin;
  public
    procedure MarkRootReferences; override;
  end;

threadvar
  GBuiltinRoots: TGocciaBuiltinRoots;

procedure TGocciaBuiltinRoots.MarkRootReferences;
var
  Builtin: TGocciaBuiltin;
begin
  Builtin := FHead;
  while Assigned(Builtin) do
  begin
    Builtin.MarkBuiltinReferences;
    Builtin := Builtin.FNextLive;
  end;
end;

{ The thread's root source, registered with its current collector. A root
  source registers with the collector current when it is created, and that
  collector's destructor drops it, so a thread whose collector was replaced
  gets a fresh one that takes over the list. One collector lives per thread
  at a time, so a source still registered is registered with the current
  one; only a source left without a collector reads the threadvar. }
function EnsureBuiltinRoots: TGocciaBuiltinRoots;
var
  Previous: TGocciaBuiltinRoots;
begin
  Result := GBuiltinRoots;
  if Assigned(Result) and (Assigned(Result.RegisteredCollector) or
     not Assigned(TGarbageCollector.Instance)) then
    Exit;
  Previous := Result;
  Result := TGocciaBuiltinRoots.Create;
  if Assigned(Previous) then
  begin
    Result.FHead := Previous.FHead;
    Previous.Free;
  end;
  GBuiltinRoots := Result;
end;

procedure ReleaseBuiltinRoots;
begin
  GBuiltinRoots.Free;
  GBuiltinRoots := nil;
end;

constructor TGocciaBuiltin.Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowErrorCallback);
begin
  FName := AName;
  FScope := AScope;
  if TGocciaObjectValue.SharedObjectPrototype = nil then
    TGocciaObjectValue.InitializeSharedPrototype;
  FBuiltinObject := TGocciaObjectValue.Create(TGocciaObjectValue.SharedObjectPrototype);
  // Before the subclass constructor runs, which may load a shim.
  LinkLive;
  FThrowError := AThrowError;
end;

procedure TGocciaBuiltin.AfterConstruction;
begin
  inherited;
  // A constructor that does not chain to Create links here.
  if not FLive then
    LinkLive;
end;

destructor TGocciaBuiltin.Destroy;
begin
  UnlinkLive;
  // FBuiltinObject is GC-managed when GC is active; only free manually otherwise
  if (TGarbageCollector.Instance = nil) then
    FBuiltinObject.Free;
  inherited;
end;

function FirstLiveBuiltin: TGocciaBuiltin;
begin
  if Assigned(GBuiltinRoots) then
    Result := GBuiltinRoots.FHead
  else
    Result := nil;
end;

procedure TGocciaBuiltin.LinkLive;
var
  Roots: TGocciaBuiltinRoots;
begin
  if FLive then
    Exit;
  Roots := EnsureBuiltinRoots;
  FPreviousLive := nil;
  FNextLive := Roots.FHead;
  if Assigned(FNextLive) then
    FNextLive.FPreviousLive := Self;
  Roots.FHead := Self;
  FLive := True;
end;

procedure TGocciaBuiltin.UnlinkLive;
var
  Roots: TGocciaBuiltinRoots;
begin
  if not FLive then
    Exit;
  if Assigned(FPreviousLive) then
    FPreviousLive.FNextLive := FNextLive
  else
  begin
    // The list head. The thread's cleanup may have released the list this
    // builtin was on, and a later one must not take over its tail.
    Roots := GBuiltinRoots;
    if Assigned(Roots) and (Roots.FHead = Self) then
      Roots.FHead := FNextLive;
  end;
  if Assigned(FNextLive) then
    FNextLive.FPreviousLive := FPreviousLive;
  FPreviousLive := nil;
  FNextLive := nil;
  FLive := False;
end;

procedure TGocciaBuiltin.MarkBuiltinReferences;
begin
  if Assigned(FBuiltinObject) then
    FBuiltinObject.MarkReferences;
end;

initialization
  RegisterThreadvarCleanup(@ReleaseBuiltinRoots);

end.
