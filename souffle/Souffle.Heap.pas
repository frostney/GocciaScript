unit Souffle.Heap;

{$I Souffle.inc}

interface

uses
  GarbageCollector.Managed;

type
  TSouffleHeapObject = class(TGCManagedObject)
  private
    FHeapKind: UInt8;
    FDelegate: TSouffleHeapObject;
  public
    constructor Create(const AHeapKind: UInt8);

    procedure MarkReferences; override;
    function DebugString: string; virtual;

    property HeapKind: UInt8 read FHeapKind;
    property Delegate: TSouffleHeapObject read FDelegate write FDelegate;
  end;

  TSouffleHeapString = class(TSouffleHeapObject)
  private
    FValue: string;
  public
    constructor Create(const AValue: string);
    function DebugString: string; override;
    property Value: string read FValue;
  end;

const
  SOUFFLE_HEAP_CLOSURE  = 1;
  SOUFFLE_HEAP_UPVALUE  = 2;
  SOUFFLE_HEAP_ARRAY    = 3;
  SOUFFLE_HEAP_RECORD   = 4;
  SOUFFLE_HEAP_NATIVE_FUNCTION = 5;
  SOUFFLE_HEAP_BLUEPRINT = 6;
  SOUFFLE_HEAP_STRING   = 7;
  SOUFFLE_HEAP_RUNTIME  = 128;

implementation

uses
  SysUtils;

{ TSouffleHeapObject }

constructor TSouffleHeapObject.Create(const AHeapKind: UInt8);
begin
  inherited Create;
  FHeapKind := AHeapKind;
  FDelegate := nil;
end;

procedure TSouffleHeapObject.MarkReferences;
begin
  inherited;
  if Assigned(FDelegate) and not FDelegate.GCMarked then
    FDelegate.MarkReferences;
end;

function TSouffleHeapObject.DebugString: string;
begin
  Result := '<heap:' + IntToStr(FHeapKind) + '>';
end;

{ TSouffleHeapString }

constructor TSouffleHeapString.Create(const AValue: string);
begin
  inherited Create(SOUFFLE_HEAP_STRING);
  FValue := AValue;
end;

function TSouffleHeapString.DebugString: string;
begin
  Result := FValue;
end;

end.
