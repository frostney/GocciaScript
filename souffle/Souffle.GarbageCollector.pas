unit Souffle.GarbageCollector;

{$I Souffle.inc}

interface

uses
  Generics.Collections,

  GarbageCollector.Generic,

  Souffle.Heap;

type
  TSouffleHeapObjectList = TObjectList<TSouffleHeapObject>;
  TSouffleGCRootMarker = TGCRootMarker;

  TSouffleGarbageCollector = class(TGenericGarbageCollector)
  private class var
    FInstance: TSouffleGarbageCollector;
  private
    function GetManagedObjectCount: Integer;
  public
    class function Instance: TSouffleGarbageCollector;
    class procedure Initialize;
    class procedure Shutdown;

    constructor Create;
    destructor Destroy; override;

    function AllocateObject(const AObject: TSouffleHeapObject): TSouffleHeapObject;
    procedure PinObject(const AObject: TSouffleHeapObject);
    procedure UnpinObject(const AObject: TSouffleHeapObject);
    procedure AddTempRoot(const AObject: TSouffleHeapObject);
    procedure RemoveTempRoot(const AObject: TSouffleHeapObject);

    property ManagedObjectCount: Integer read GetManagedObjectCount;
  end;

implementation

uses
  SysUtils;

{ TSouffleGarbageCollector }

class function TSouffleGarbageCollector.Instance: TSouffleGarbageCollector;
begin
  Result := FInstance;
end;

class procedure TSouffleGarbageCollector.Initialize;
begin
  if not Assigned(FInstance) then
    FInstance := TSouffleGarbageCollector.Create;
end;

class procedure TSouffleGarbageCollector.Shutdown;
begin
  FreeAndNil(FInstance);
end;

constructor TSouffleGarbageCollector.Create;
begin
  inherited Create;
end;

destructor TSouffleGarbageCollector.Destroy;
begin
  FreeAllManagedObjects;
  inherited;
end;

function TSouffleGarbageCollector.AllocateObject(
  const AObject: TSouffleHeapObject): TSouffleHeapObject;
begin
  RegisterObject(AObject);
  Result := AObject;
end;

procedure TSouffleGarbageCollector.PinObject(
  const AObject: TSouffleHeapObject);
begin
  inherited PinObject(AObject);
end;

procedure TSouffleGarbageCollector.UnpinObject(
  const AObject: TSouffleHeapObject);
begin
  inherited UnpinObject(AObject);
end;

procedure TSouffleGarbageCollector.AddTempRoot(
  const AObject: TSouffleHeapObject);
begin
  inherited AddTempRoot(AObject);
end;

procedure TSouffleGarbageCollector.RemoveTempRoot(
  const AObject: TSouffleHeapObject);
begin
  inherited RemoveTempRoot(AObject);
end;

function TSouffleGarbageCollector.GetManagedObjectCount: Integer;
begin
  Result := inherited ManagedObjectCount;
end;

end.
