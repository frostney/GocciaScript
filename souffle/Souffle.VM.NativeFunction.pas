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

  TSouffleNativeCallbackWithContext = function(
    const AReceiver: TSouffleValue;
    const AArgs: PSouffleValue;
    const AArgCount: Integer;
    const AContext: TSouffleValue): TSouffleValue;

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

  TSouffleNativeClosure = class(TSouffleHeapObject)
  private
    FCallback: TSouffleNativeCallbackWithContext;
    FContext: TSouffleValue;
    FName: string;
    FArity: Integer;
  public
    constructor Create(const AName: string; const AArity: Integer;
      const ACallback: TSouffleNativeCallbackWithContext;
      const AContext: TSouffleValue);

    function Invoke(const AReceiver: TSouffleValue;
      const AArgs: PSouffleValue;
      const AArgCount: Integer): TSouffleValue; inline;

    procedure MarkReferences; override;
    function DebugString: string; override;

    property Name: string read FName;
    property Arity: Integer read FArity;
    property Context: TSouffleValue read FContext;
  end;

function BuildDelegate(
  const AEntries: array of TSouffleMethodEntry): TSouffleHeapObject;

implementation

uses
  GarbageCollector.Generic,
  Souffle.Compound;

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

{ TSouffleNativeClosure }

constructor TSouffleNativeClosure.Create(const AName: string;
  const AArity: Integer; const ACallback: TSouffleNativeCallbackWithContext;
  const AContext: TSouffleValue);
begin
  inherited Create(SOUFFLE_HEAP_NATIVE_CLOSURE);
  FName := AName;
  FArity := AArity;
  FCallback := ACallback;
  FContext := AContext;
end;

function TSouffleNativeClosure.Invoke(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
begin
  Result := FCallback(AReceiver, AArgs, AArgCount, FContext);
end;

procedure TSouffleNativeClosure.MarkReferences;
begin
  if GCMarked then Exit;
  inherited;
  if SouffleIsReference(FContext) and Assigned(FContext.AsReference) and
     not FContext.AsReference.GCMarked then
    FContext.AsReference.MarkReferences;
end;

function TSouffleNativeClosure.DebugString: string;
begin
  Result := '<native-closure:' + FName + '>';
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
