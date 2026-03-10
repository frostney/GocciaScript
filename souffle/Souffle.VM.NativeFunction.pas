unit Souffle.VM.NativeFunction;

{$I Souffle.inc}

interface

uses
  Souffle.Heap,
  Souffle.Value;

type
  TSouffleNativeCallback = function(
    const AReceiver: TSouffleValue;
    const AArgs: PSouffleValue;
    const AArgCount: Integer): TSouffleValue;

  TSouffleMethodEntry = record
    Name: string;
    Arity: Integer;
    Callback: TSouffleNativeCallback;
  end;

  TSouffleMethodTable = array of TSouffleMethodEntry;

  TSouffleNativeFunction = class(TSouffleHeapObject)
  private
    FCallback: TSouffleNativeCallback;
    FName: string;
    FArity: Integer;
  public
    constructor Create(const AName: string; const AArity: Integer;
      const ACallback: TSouffleNativeCallback);

    function Invoke(const AReceiver: TSouffleValue;
      const AArgs: PSouffleValue;
      const AArgCount: Integer): TSouffleValue; inline;

    function DebugString: string; override;

    property Name: string read FName;
    property Arity: Integer read FArity;
  end;

function BuildDelegate(
  const AEntries: array of TSouffleMethodEntry): TSouffleHeapObject;

implementation

uses
  Souffle.Compound,
  GarbageCollector.Generic;

{ TSouffleNativeFunction }

constructor TSouffleNativeFunction.Create(const AName: string;
  const AArity: Integer; const ACallback: TSouffleNativeCallback);
begin
  inherited Create(SOUFFLE_HEAP_NATIVE_FUNCTION);
  FName := AName;
  FArity := AArity;
  FCallback := ACallback;
end;

function TSouffleNativeFunction.Invoke(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
begin
  Result := FCallback(AReceiver, AArgs, AArgCount);
end;

function TSouffleNativeFunction.DebugString: string;
begin
  Result := '<native:' + FName + '>';
end;

function BuildDelegate(
  const AEntries: array of TSouffleMethodEntry): TSouffleHeapObject;
var
  Rec: TSouffleRecord;
  Fn: TSouffleNativeFunction;
  GC: TGarbageCollector;
  I: Integer;
begin
  GC := TGarbageCollector.Instance;
  Rec := TSouffleRecord.Create(Length(AEntries));
  if Assigned(GC) then
    GC.AllocateObject(Rec);
  for I := 0 to High(AEntries) do
  begin
    Fn := TSouffleNativeFunction.Create(
      AEntries[I].Name, AEntries[I].Arity, AEntries[I].Callback);
    if Assigned(GC) then
      GC.AllocateObject(Fn);
    Rec.Put(AEntries[I].Name, SouffleReference(Fn));
  end;
  Result := Rec;
end;

end.
