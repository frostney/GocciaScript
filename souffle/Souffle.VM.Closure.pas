unit Souffle.VM.Closure;

{$I Souffle.inc}

interface

uses
  Souffle.Bytecode.Chunk,
  Souffle.Heap,
  Souffle.VM.Upvalue;

type
  TSouffleClosure = class(TSouffleHeapObject)
  private
    FPrototype: TSouffleFunctionPrototype;
    FUpvalues: array of TSouffleUpvalue;
    FUpvalueCount: Integer;
  public
    constructor Create(const APrototype: TSouffleFunctionPrototype);

    function GetUpvalue(const AIndex: Integer): TSouffleUpvalue; inline;
    procedure SetUpvalue(const AIndex: Integer; const AUpvalue: TSouffleUpvalue); inline;

    procedure MarkReferences; override;
    function DebugString: string; override;

    property Prototype: TSouffleFunctionPrototype read FPrototype;
    property UpvalueCount: Integer read FUpvalueCount;
  end;

implementation

{ TSouffleClosure }

constructor TSouffleClosure.Create(const APrototype: TSouffleFunctionPrototype);
begin
  inherited Create(SOUFFLE_HEAP_CLOSURE);
  FPrototype := APrototype;
  FUpvalueCount := APrototype.UpvalueCount;
  SetLength(FUpvalues, FUpvalueCount);
end;

function TSouffleClosure.GetUpvalue(const AIndex: Integer): TSouffleUpvalue;
begin
  Result := FUpvalues[AIndex];
end;

procedure TSouffleClosure.SetUpvalue(const AIndex: Integer;
  const AUpvalue: TSouffleUpvalue);
begin
  FUpvalues[AIndex] := AUpvalue;
end;

procedure TSouffleClosure.MarkReferences;
var
  I: Integer;
begin
  inherited;
  for I := 0 to FUpvalueCount - 1 do
    if Assigned(FUpvalues[I]) then
      FUpvalues[I].MarkReferences;
end;

function TSouffleClosure.DebugString: string;
begin
  Result := '<closure:' + FPrototype.Name + '>';
end;

end.
