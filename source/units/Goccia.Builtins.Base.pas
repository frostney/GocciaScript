unit Goccia.Builtins.Base;

{$I Goccia.inc}

interface

uses
  Goccia.Error.ThrowErrorCallback,
  Goccia.Scope,
  Goccia.Values.ObjectValue;

type
  { A builtin's FBuiltinObject is a collector root for the builtin's whole
    lifetime, from the moment TGocciaBuiltin.Create makes it (issue #1655).
    A subclass constructor can run bytecode before it binds the object
    anywhere: reading a lazily loaded global such as Date loads its shim,
    which reaches the VM's memory-pressure checkpoint. Some builtins never bind
    FBuiltinObject in the global scope at all, yet the engine and runtime
    extensions read it later (static members, the goccia:test namespace). }
  TGocciaBuiltin = class
  private
    FRootedObject: TGocciaObjectValue;
  protected
    FName: string;
    FScope: TGocciaScope;
    FBuiltinObject: TGocciaObjectValue;
    FThrowError: TGocciaThrowErrorCallback;
    { Roots FBuiltinObject, releasing the object rooted before if a
      constructor replaced it. Create and AfterConstruction call it; a
      constructor that replaces FBuiltinObject and then runs guest code or
      loads a shim calls it straight after the replacement. }
    procedure RootBuiltinObject;
  public
    constructor Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowErrorCallback);
    procedure AfterConstruction; override;
    destructor Destroy; override;

    property Name: string read FName;
    property BuiltinObject: TGocciaObjectValue read FBuiltinObject;
    property ThrowError: TGocciaThrowErrorCallback read FThrowError;
  end;

implementation

uses
  Goccia.GarbageCollector;

constructor TGocciaBuiltin.Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowErrorCallback);
begin
  FName := AName;
  FScope := AScope;
  if TGocciaObjectValue.SharedObjectPrototype = nil then
    TGocciaObjectValue.InitializeSharedPrototype;
  FBuiltinObject := TGocciaObjectValue.Create(TGocciaObjectValue.SharedObjectPrototype);
  RootBuiltinObject;
  FThrowError := AThrowError;
end;

procedure TGocciaBuiltin.RootBuiltinObject;
var
  GC: TGarbageCollector;
begin
  if FRootedObject = FBuiltinObject then
    Exit;
  GC := TGarbageCollector.Instance;
  if not Assigned(GC) then
    Exit;
  if Assigned(FRootedObject) then
    GC.RemoveRootObject(FRootedObject);
  FRootedObject := FBuiltinObject;
  if Assigned(FRootedObject) then
    GC.AddRootObject(FRootedObject);
end;

procedure TGocciaBuiltin.AfterConstruction;
begin
  inherited;
  // A constructor that does not chain to Create, or that replaces
  // FBuiltinObject, is rooted here.
  RootBuiltinObject;
end;

destructor TGocciaBuiltin.Destroy;
var
  GC: TGarbageCollector;
begin
  // FBuiltinObject is GC-managed when GC is active; only free manually otherwise
  GC := TGarbageCollector.Instance;
  if not Assigned(GC) then
    FBuiltinObject.Free
  else if Assigned(FRootedObject) then
    GC.RemoveRootObject(FRootedObject);
  FRootedObject := nil;
  inherited;
end;

end.
