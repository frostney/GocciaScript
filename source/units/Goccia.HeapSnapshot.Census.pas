unit Goccia.HeapSnapshot.Census;

{ Heap snapshot prototype: a live-class census.

  A baseline records every object the collector holds once the engine is
  ready and before any script code runs. At a pause point the census collects
  and counts, by Pascal class name, every surviving object that is not in the
  baseline: the classes a structured snapshot taken at that point would have
  to serialize. The result is written as one JSON line. }

{$I Goccia.inc}

interface

uses
  Classes,

  Goccia.GarbageCollector;

type
  TGocciaHeapBaseline = class
  private
    FObjects: TGCObjectSet;
  public
    constructor Create;
    destructor Destroy; override;
    // Collects, then records every surviving object.
    procedure Capture;
    function Contains(const AObject: TGCManagedObject): Boolean;
    property Objects: TGCObjectSet read FObjects;
  end;

  TGocciaHeapCensus = class
  private
    FCounts: TStringList;
    FTotal: Integer;
    FBaselineLive: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    // Collects, then counts every surviving object outside ABaseline.
    procedure Take(const ABaseline: TGocciaHeapBaseline);
    function ToJson(const AFile, APoint: string): string;
    property Counts: TStringList read FCounts;
    property Total: Integer read FTotal;
  end;

  TGocciaHeapCensusAwaitHook = procedure;

threadvar
  // Set by a census host. OP_AWAIT sets GHeapCensusAwaitPending when it
  // suspends an async body and the VM calls the hook at its next instruction
  // boundary (the memory-pressure checkpoint, where a collection is safe).
  GHeapCensusAwaitHook: TGocciaHeapCensusAwaitHook;
  GHeapCensusAwaitPending: Boolean;

// Appends one census line to <dir>/census-<thread>.jsonl when the
// GOCCIA_HEAP_CENSUS environment variable names a directory.
procedure AppendCensusLine(const ALine: string);
function CensusDirectory: string;

implementation

uses
  SysUtils;

function JsonEscape(const AText: string): string;
var
  I: Integer;
  C: Char;
begin
  Result := '';
  for I := 1 to Length(AText) do
  begin
    C := AText[I];
    case C of
      '"': Result := Result + '\"';
      '\': Result := Result + '\\';
      #0..#31: Result := Result + Format('\u%.4x', [Ord(C)]);
    else
      Result := Result + C;
    end;
  end;
end;

{ TGocciaHeapBaseline }

constructor TGocciaHeapBaseline.Create;
begin
  inherited Create;
  FObjects := TGCObjectSet.Create;
end;

destructor TGocciaHeapBaseline.Destroy;
begin
  FObjects.Free;
  inherited;
end;

procedure TGocciaHeapBaseline.Capture;
var
  GC: TGarbageCollector;
  I: Integer;
  Obj: TGCManagedObject;
begin
  FObjects.Clear;
  GC := TGarbageCollector.Instance;
  if not Assigned(GC) then
    Exit;
  GC.Collect;
  for I := 0 to GC.ManagedObjectCount - 1 do
  begin
    Obj := GC.ManagedObjects[I];
    if Assigned(Obj) then
      FObjects.Add(Obj, True);
  end;
end;

function TGocciaHeapBaseline.Contains(const AObject: TGCManagedObject): Boolean;
begin
  Result := FObjects.ContainsKey(AObject);
end;

{ TGocciaHeapCensus }

constructor TGocciaHeapCensus.Create;
begin
  inherited Create;
  FCounts := TStringList.Create;
  FCounts.Sorted := True;
  FCounts.Duplicates := dupError;
end;

destructor TGocciaHeapCensus.Destroy;
begin
  FCounts.Free;
  inherited;
end;

procedure TGocciaHeapCensus.Take(const ABaseline: TGocciaHeapBaseline);
var
  GC: TGarbageCollector;
  I, Index: Integer;
  Obj: TGCManagedObject;
  Name: string;
begin
  FCounts.Clear;
  FTotal := 0;
  FBaselineLive := 0;
  GC := TGarbageCollector.Instance;
  if not Assigned(GC) then
    Exit;
  GC.Collect;
  for I := 0 to GC.ManagedObjectCount - 1 do
  begin
    Obj := GC.ManagedObjects[I];
    if not Assigned(Obj) then
      Continue;
    if Assigned(ABaseline) and ABaseline.Contains(Obj) then
    begin
      Inc(FBaselineLive);
      Continue;
    end;
    Name := Obj.ClassName;
    Index := FCounts.IndexOf(Name);
    if Index < 0 then
      FCounts.AddObject(Name, TObject(PtrInt(1)))
    else
      FCounts.Objects[Index] := TObject(PtrInt(FCounts.Objects[Index]) + 1);
    Inc(FTotal);
  end;
end;

function TGocciaHeapCensus.ToJson(const AFile, APoint: string): string;
var
  I: Integer;
begin
  Result := '{"file":"' + JsonEscape(AFile) + '","point":"' +
    JsonEscape(APoint) + '","total":' + IntToStr(FTotal) +
    ',"baselineLive":' + IntToStr(FBaselineLive) + ',"classes":{';
  for I := 0 to FCounts.Count - 1 do
  begin
    if I > 0 then
      Result := Result + ',';
    Result := Result + '"' + JsonEscape(FCounts[I]) + '":' +
      IntToStr(PtrInt(FCounts.Objects[I]));
  end;
  Result := Result + '}}';
end;

function CensusDirectory: string;
begin
  Result := GetEnvironmentVariable('GOCCIA_HEAP_CENSUS');
end;

procedure AppendCensusLine(const ALine: string);
var
  Dir, Path: string;
  Stream: TFileStream;
  Bytes: UTF8String;
begin
  Dir := CensusDirectory;
  if Dir = '' then
    Exit;
  Path := IncludeTrailingPathDelimiter(Dir) + 'census-' +
    IntToStr(PtrUInt(GetCurrentThreadId)) + '.jsonl';
  if FileExists(Path) then
    Stream := TFileStream.Create(Path, fmOpenWrite or fmShareDenyNone)
  else
    Stream := TFileStream.Create(Path, fmCreate);
  try
    Stream.Seek(0, soEnd);
    Bytes := UTF8String(ALine) + #10;
    if Length(Bytes) > 0 then
      Stream.WriteBuffer(Bytes[1], Length(Bytes));
  finally
    Stream.Free;
  end;
end;

end.
