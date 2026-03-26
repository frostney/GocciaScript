unit Souffle.Iterator;

{$I Souffle.inc}

interface

uses
  GarbageCollector.Generic,
  Souffle.Compound,
  Souffle.Heap,
  Souffle.Value;

const
  SOUFFLE_HEAP_ARRAY_ITERATOR  = 9;
  SOUFFLE_HEAP_STRING_ITERATOR = 10;

type
  TSouffleArrayIterationKind = (aikValues, aikKeys, aikEntries);

  TSouffleArrayIterator = class(TSouffleHeapObject)
  public
    IterArray: TSouffleArray;
    IterIndex: Integer;
    IterKind: TSouffleArrayIterationKind;

    constructor Create(const AArray: TSouffleArray;
      const AKind: TSouffleArrayIterationKind = aikValues);
    function Next(out ADone: Boolean): TSouffleValue;
    procedure MarkReferences; override;
    function DebugString: string; override;
  end;

  TSouffleStringIterator = class(TSouffleHeapObject)
  public
    IterValue: string;
    IterIndex: Integer;

    constructor Create(const AValue: string);
    function Next(out ADone: Boolean): TSouffleValue;
    function DebugString: string; override;
  end;

implementation

uses
  SysUtils;

{ TSouffleArrayIterator }

constructor TSouffleArrayIterator.Create(const AArray: TSouffleArray;
  const AKind: TSouffleArrayIterationKind = aikValues);
begin
  inherited Create(SOUFFLE_HEAP_ARRAY_ITERATOR);
  IterArray := AArray;
  IterIndex := 0;
  IterKind := AKind;
end;

procedure TSouffleArrayIterator.MarkReferences;
begin
  inherited;
  if Assigned(IterArray) and not IterArray.GCMarked then
    IterArray.MarkReferences;
end;

function TSouffleArrayIterator.Next(out ADone: Boolean): TSouffleValue;
var
  Entry: TSouffleArray;
begin
  if IterIndex >= IterArray.Count then
  begin
    ADone := True;
    Result := SouffleNil;
  end
  else
  begin
    ADone := False;
    case IterKind of
      aikKeys:
        Result := SouffleInteger(IterIndex);
      aikEntries:
      begin
        Entry := TSouffleArray.Create(2);
        Entry.Push(SouffleInteger(IterIndex));
        Entry.Push(IterArray.Get(IterIndex));
        if Assigned(TGarbageCollector.Instance) then
          TGarbageCollector.Instance.AllocateObject(Entry);
        Result := SouffleReference(Entry);
      end;
    else
      Result := IterArray.Get(IterIndex);
    end;
    Inc(IterIndex);
  end;
end;

function TSouffleArrayIterator.DebugString: string;
begin
  Result := '[ArrayIterator: ' + IntToStr(IterIndex) + ']';
end;

{ TSouffleStringIterator }

constructor TSouffleStringIterator.Create(const AValue: string);
begin
  inherited Create(SOUFFLE_HEAP_STRING_ITERATOR);
  IterValue := AValue;
  IterIndex := 1;
end;

function TSouffleStringIterator.Next(out ADone: Boolean): TSouffleValue;
var
  Ch: string;
begin
  if IterIndex > Length(IterValue) then
  begin
    ADone := True;
    Result := SouffleNil;
  end
  else
  begin
    ADone := False;
    Ch := IterValue[IterIndex];
    Result := SouffleString(Ch);
    Inc(IterIndex);
  end;
end;

function TSouffleStringIterator.DebugString: string;
begin
  Result := '[StringIterator: ' + IntToStr(IterIndex) + ']';
end;

end.
