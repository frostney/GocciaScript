unit Goccia.Profiler;

{$I Goccia.inc}

interface

uses
  HashMap,
  OrderedStringMap;

const
  MAX_OPCODE_ORDINAL = 255;
  OPCODE_PAIR_SIZE = 256;
  INITIAL_FUNCTION_PROFILE_CAPACITY = 64;
  INITIAL_TIMING_STACK_CAPACITY = 32;

type
  TGocciaProfileModeItem = (pmOpcodes, pmFunctions, pmCompile);
  TGocciaProfileMode = set of TGocciaProfileModeItem;

  TGocciaFunctionProfile = record
    TemplateName: string;
    SourceFile: string;
    Line: Integer;
    CallCount: Int64;
    SelfTimeNanoseconds: Int64;
    // Inclusive time, counted once per outermost activation: a frame that
    // starts while the function already has one on the timing stack
    // (direct or mutual recursion, re-entry through a callback) adds
    // nothing here, since the outer frame's time already contains it.
    TotalTimeNanoseconds: Int64;
    Allocations: Int64;
    // Frames of this function on the timing stack right now.
    ActiveFrames: Integer;
  end;

  TGocciaProfilingStackEntry = record
    ProfileIndex: Integer;
    PathIndex: Integer;
    EntryTimestamp: Int64;
    ChildTimeAccumulated: Int64;
  end;

  TGocciaProfilePathKey = packed record
    Parent: Integer;
    Profile: Integer;
  end;

  TGocciaProfilePath = record
    Parent: Integer;
    Name: string;
    PathLength: SizeInt;
    SelfTime: Int64;
    NextSampled: Integer;
  end;

  TGocciaOpcodePairArray = array[0..OPCODE_PAIR_SIZE - 1,
    0..OPCODE_PAIR_SIZE - 1] of Int64;
  PGocciaOpcodePairArray = ^TGocciaOpcodePairArray;

type
  TGocciaProfiler = class
  private
    FMode: TGocciaProfileMode;
    FEnabled: Boolean;
    FOpcodeCount: array[0..MAX_OPCODE_ORDINAL] of Int64;
    FHasPrevOp: Boolean;
    FPrevOp: UInt8;
    FOpcodePairs: PGocciaOpcodePairArray;
    FScalarHits: Int64;
    FScalarMisses: Int64;
    FShapeDepthLimitEvents: Int64;
    FShapeTableSaturationEvents: Int64;
    FFunctionProfiles: array of TGocciaFunctionProfile;
    FFunctionProfileCount: Integer;
    FTimingStack: array of TGocciaProfilingStackEntry;
    FTimingStackCount: Integer;
    FCollapsedStacks: TOrderedStringMap<Int64>;
    FPaths: array of TGocciaProfilePath;
    FPathCount: Integer;
    FPathIndex: THashMap<TGocciaProfilePathKey, Integer>;
    FFirstSampledPath: Integer;
    FLastSampledPath: Integer;
    FCollapsedStacksDirty: Boolean;
    FExcludedNanoseconds: Int64;
    FDeterministic: Boolean;
    function InternPath(const AProfileIndex: Integer): Integer;
    function GetCollapsedStacks: TOrderedStringMap<Int64>;
  protected
    function ReadTimestamp: Int64; virtual;
  public
    class function Instance: TGocciaProfiler;
    class procedure Initialize;
    class procedure Shutdown;

    constructor Create;
    destructor Destroy; override;

    procedure ResetCounts;
    procedure BeginEntry;

    procedure RecordOpcode(const AOp: UInt8); {$IFDEF FPC}inline;{$ENDIF}
    procedure RecordScalarHit; {$IFDEF FPC}inline;{$ENDIF}
    procedure RecordScalarMiss; {$IFDEF FPC}inline;{$ENDIF}
    procedure RecordAllocation; {$IFDEF FPC}inline;{$ENDIF}
    // Shape system saturation events (cold paths; see Goccia.Values.Shape):
    // an object layout hitting the transition depth limit, and a realm
    // shape table reaching capacity. Both degrade caching silently, so the
    // profiler is the diagnostic surface for them.
    procedure RecordShapeDepthLimit; {$IFDEF FPC}inline;{$ENDIF}
    procedure RecordShapeTableSaturation; {$IFDEF FPC}inline;{$ENDIF}

    function RegisterTemplate(const AName, ASourceFile: string;
      const ALine: Integer): Integer;
    procedure PushFunction(const AProfileIndex: Integer;
      const ATimestamp: Int64); overload;
    procedure PopFunction(const AProfileIndex: Integer;
      const ATimestamp: Int64); overload;

    // Clock-owning entry points exclude registration and stack bookkeeping
    // from every active frame. Explicit timestamps remain useful to embedders
    // supplying an already adjusted clock and to exact accounting tests.
    procedure PushFunction(const AProfileIndex: Integer); overload;
    procedure PopFunction(const AProfileIndex: Integer); overload;

    function GetOpcodeCount(const AOp: UInt8): Int64; {$IFDEF FPC}inline;{$ENDIF}
    function GetOpcodePairCount(const APrev, ACur: UInt8): Int64; {$IFDEF FPC}inline;{$ENDIF}
    function GetFunctionProfile(const AIndex: Integer): TGocciaFunctionProfile; {$IFDEF FPC}inline;{$ENDIF}

    property Deterministic: Boolean read FDeterministic write FDeterministic;
    property Mode: TGocciaProfileMode read FMode write FMode;
    property Enabled: Boolean read FEnabled write FEnabled;
    property FunctionProfileCount: Integer read FFunctionProfileCount;
    property ScalarHits: Int64 read FScalarHits;
    property ScalarMisses: Int64 read FScalarMisses;
    property ShapeDepthLimitEvents: Int64 read FShapeDepthLimitEvents;
    property ShapeTableSaturationEvents: Int64 read FShapeTableSaturationEvents;
    property CollapsedStacks: TOrderedStringMap<Int64> read GetCollapsedStacks;
  end;

implementation

uses
  SysUtils,

  TimingUtils,

  Goccia.ThreadPolls;

{ TGocciaProfiler }

threadvar
  ProfilerThreadInstance: TGocciaProfiler;

class function TGocciaProfiler.Instance: TGocciaProfiler;
begin
  Result := ProfilerThreadInstance;
end;

class procedure TGocciaProfiler.Initialize;
begin
  if not Assigned(ProfilerThreadInstance) then
    ProfilerThreadInstance := TGocciaProfiler.Create;
end;

class procedure TGocciaProfiler.Shutdown;
begin
  FreeAndNil(ProfilerThreadInstance);
end;

constructor TGocciaProfiler.Create;
var
  I: Integer;
begin
  inherited Create;
  GThreadPolls.ProfilingAllocations := False;
  FEnabled := False;
  FMode := [];
  for I := 0 to MAX_OPCODE_ORDINAL do
    FOpcodeCount[I] := 0;
  FHasPrevOp := False;
  FPrevOp := 0;
  GetMem(FOpcodePairs, SizeOf(TGocciaOpcodePairArray));
  FillChar(FOpcodePairs^, SizeOf(TGocciaOpcodePairArray), 0);
  FScalarHits := 0;
  FScalarMisses := 0;
  FFunctionProfileCount := 0;
  SetLength(FFunctionProfiles, INITIAL_FUNCTION_PROFILE_CAPACITY);
  FTimingStackCount := 0;
  SetLength(FTimingStack, INITIAL_TIMING_STACK_CAPACITY);
  FCollapsedStacks := TOrderedStringMap<Int64>.Create;
  FPathIndex := THashMap<TGocciaProfilePathKey, Integer>.Create;
  FFirstSampledPath := -1;
  FLastSampledPath := -1;
end;

destructor TGocciaProfiler.Destroy;
begin
  FPathIndex.Free;
  FCollapsedStacks.Free;
  if Assigned(FOpcodePairs) then
    FreeMem(FOpcodePairs);
  inherited;
end;

procedure TGocciaProfiler.ResetCounts;
var
  I: Integer;
begin
  for I := 0 to MAX_OPCODE_ORDINAL do
    FOpcodeCount[I] := 0;
  FillChar(FOpcodePairs^, SizeOf(TGocciaOpcodePairArray), 0);
  FHasPrevOp := False;
  FPrevOp := 0;
  FScalarHits := 0;
  FScalarMisses := 0;
  FTimingStackCount := 0;
  FCollapsedStacks.Clear;
  FPathIndex.Clear;
  FPathCount := 0;
  SetLength(FPaths, 0);
  FFirstSampledPath := -1;
  FLastSampledPath := -1;
  FCollapsedStacksDirty := False;
  FExcludedNanoseconds := 0;

  for I := 0 to FFunctionProfileCount - 1 do
  begin
    FFunctionProfiles[I].CallCount := 0;
    FFunctionProfiles[I].SelfTimeNanoseconds := 0;
    FFunctionProfiles[I].TotalTimeNanoseconds := 0;
    FFunctionProfiles[I].Allocations := 0;
    // The timing stack was emptied above, so no frame is active any more.
    FFunctionProfiles[I].ActiveFrames := 0;
  end;
end;

procedure TGocciaProfiler.BeginEntry;
begin
  // Independent entry files have no opcode transition between them.
  FHasPrevOp := False;
end;

function TGocciaProfiler.ReadTimestamp: Int64;
begin
  Result := GetNanoseconds;
end;

procedure TGocciaProfiler.PushFunction(const AProfileIndex: Integer);
var
  Started: Int64;
begin
  Started := ReadTimestamp;
  PushFunction(AProfileIndex, Started - FExcludedNanoseconds);
  Inc(FExcludedNanoseconds, ReadTimestamp - Started);
end;

procedure TGocciaProfiler.PopFunction(const AProfileIndex: Integer);
var
  Started: Int64;
begin
  Started := ReadTimestamp;
  PopFunction(AProfileIndex, Started - FExcludedNanoseconds);
  Inc(FExcludedNanoseconds, ReadTimestamp - Started);
end;

procedure TGocciaProfiler.RecordOpcode(const AOp: UInt8);
begin
  Inc(FOpcodeCount[AOp]);
  if FHasPrevOp then
    Inc(FOpcodePairs^[FPrevOp, AOp]);
  FPrevOp := AOp;
  FHasPrevOp := True;
end;

procedure TGocciaProfiler.RecordScalarHit;
begin
  Inc(FScalarHits);
end;

procedure TGocciaProfiler.RecordShapeDepthLimit;
begin
  Inc(FShapeDepthLimitEvents);
end;

procedure TGocciaProfiler.RecordShapeTableSaturation;
begin
  Inc(FShapeTableSaturationEvents);
end;

procedure TGocciaProfiler.RecordScalarMiss;
begin
  Inc(FScalarMisses);
end;

procedure TGocciaProfiler.RecordAllocation;
begin
  if FTimingStackCount > 0 then
    Inc(FFunctionProfiles[
      FTimingStack[FTimingStackCount - 1].ProfileIndex].Allocations);
end;

function TGocciaProfiler.RegisterTemplate(const AName, ASourceFile: string;
  const ALine: Integer): Integer;
var
  Started: Int64;
begin
  Started := ReadTimestamp;
  if FFunctionProfileCount >= Length(FFunctionProfiles) then
    SetLength(FFunctionProfiles, FFunctionProfileCount * 2 + 16);
  Result := FFunctionProfileCount;
  FFunctionProfiles[Result].TemplateName := AName;
  FFunctionProfiles[Result].SourceFile := ASourceFile;
  FFunctionProfiles[Result].Line := ALine;
  FFunctionProfiles[Result].CallCount := 0;
  FFunctionProfiles[Result].SelfTimeNanoseconds := 0;
  FFunctionProfiles[Result].TotalTimeNanoseconds := 0;
  FFunctionProfiles[Result].Allocations := 0;
  FFunctionProfiles[Result].ActiveFrames := 0;
  Inc(FFunctionProfileCount);
  Inc(FExcludedNanoseconds, ReadTimestamp - Started);
end;

procedure TGocciaProfiler.PushFunction(const AProfileIndex: Integer;
  const ATimestamp: Int64);
begin
  if (AProfileIndex < 0) or (AProfileIndex >= FFunctionProfileCount) then Exit;
  if FTimingStackCount >= Length(FTimingStack) then
    SetLength(FTimingStack, FTimingStackCount * 2 + 16);
  FTimingStack[FTimingStackCount].PathIndex := InternPath(AProfileIndex);
  FTimingStack[FTimingStackCount].ProfileIndex := AProfileIndex;
  FTimingStack[FTimingStackCount].EntryTimestamp := ATimestamp;
  FTimingStack[FTimingStackCount].ChildTimeAccumulated := 0;
  Inc(FTimingStackCount);
  Inc(FFunctionProfiles[AProfileIndex].CallCount);
  Inc(FFunctionProfiles[AProfileIndex].ActiveFrames);
end;

procedure TGocciaProfiler.PopFunction(const AProfileIndex: Integer;
  const ATimestamp: Int64);
var
  Elapsed, SelfTime: Int64;
  PoppedIndex, Path: Integer;
begin
  if FTimingStackCount <= 0 then Exit;
  Dec(FTimingStackCount);
  PoppedIndex := FTimingStack[FTimingStackCount].ProfileIndex;
  if (PoppedIndex < 0) or (PoppedIndex >= FFunctionProfileCount) then Exit;
  Elapsed := ATimestamp - FTimingStack[FTimingStackCount].EntryTimestamp;
  SelfTime := Elapsed - FTimingStack[FTimingStackCount].ChildTimeAccumulated;
  Dec(FFunctionProfiles[PoppedIndex].ActiveFrames);
  if FFunctionProfiles[PoppedIndex].ActiveFrames = 0 then
    Inc(FFunctionProfiles[PoppedIndex].TotalTimeNanoseconds, Elapsed);
  Inc(FFunctionProfiles[PoppedIndex].SelfTimeNanoseconds, SelfTime);
  if FTimingStackCount > 0 then
    Inc(FTimingStack[FTimingStackCount - 1].ChildTimeAccumulated, Elapsed);

  // No path strings or ancestor walks on return. Sample order preserves
  // the existing collapsed export's first-positive-pop ordering.
  if SelfTime > 0 then
  begin
    Path := FTimingStack[FTimingStackCount].PathIndex;
    if FPaths[Path].SelfTime = 0 then
    begin
      if FLastSampledPath >= 0 then
        FPaths[FLastSampledPath].NextSampled := Path
      else
        FFirstSampledPath := Path;
      FLastSampledPath := Path;
    end;
    Inc(FPaths[Path].SelfTime, SelfTime);
    FCollapsedStacksDirty := True;
  end;
end;

function TGocciaProfiler.InternPath(const AProfileIndex: Integer): Integer;
var
  Key: TGocciaProfilePathKey;
begin
  Key.Parent := -1;
  if FTimingStackCount > 0 then
    Key.Parent := FTimingStack[FTimingStackCount - 1].PathIndex;
  Key.Profile := AProfileIndex;
  if FPathIndex.TryGetValue(Key, Result) then
    Exit;
  Result := FPathCount;
  if FPathCount = Length(FPaths) then
    SetLength(FPaths, FPathCount * 2 + INITIAL_TIMING_STACK_CAPACITY);
  FPaths[Result].Parent := Key.Parent;
  FPaths[Result].Name := FFunctionProfiles[AProfileIndex].TemplateName;
  if FPaths[Result].Name = '' then
    FPaths[Result].Name := '<anonymous>';
  FPaths[Result].PathLength := Length(FPaths[Result].Name);
  if Key.Parent >= 0 then
    Inc(FPaths[Result].PathLength, FPaths[Key.Parent].PathLength + 1);
  FPaths[Result].SelfTime := 0;
  FPaths[Result].NextSampled := -1;
  FPathIndex.Add(Key, Result);
  Inc(FPathCount);
end;

function TGocciaProfiler.GetCollapsedStacks: TOrderedStringMap<Int64>;
var
  Path, Node: Integer;
  Position, NameLength: SizeInt;
  StackPath: string;
  ExistingValue, Started: Int64;
begin
  Result := FCollapsedStacks;
  if not FCollapsedStacksDirty then
    Exit;
  Started := ReadTimestamp;
  FCollapsedStacks.Clear;
  Path := FFirstSampledPath;
  while Path >= 0 do
  begin
    // Allocate once and fill backwards. Output size, rather than repeated
    // prefix copying, determines the cold export's work.
    Position := FPaths[Path].PathLength;
    SetLength(StackPath, Position);
    Node := Path;
    while Node >= 0 do
    begin
      NameLength := Length(FPaths[Node].Name);
      Dec(Position, NameLength);
      Move(FPaths[Node].Name[1], StackPath[Position + 1], NameLength * SizeOf(Char));
      Node := FPaths[Node].Parent;
      if Node >= 0 then
      begin
        StackPath[Position] := ';';
        Dec(Position);
      end;
    end;
    // Different templates with the same displayed names still collapse.
    if not FCollapsedStacks.TryGetValue(StackPath, ExistingValue) then
      ExistingValue := 0;
    FCollapsedStacks.AddOrSetValue(StackPath,
      ExistingValue + FPaths[Path].SelfTime);
    Path := FPaths[Path].NextSampled;
  end;
  FCollapsedStacksDirty := False;
  Inc(FExcludedNanoseconds, ReadTimestamp - Started);
end;

function TGocciaProfiler.GetOpcodeCount(const AOp: UInt8): Int64;
begin
  Result := FOpcodeCount[AOp];
end;

function TGocciaProfiler.GetOpcodePairCount(const APrev, ACur: UInt8): Int64;
begin
  Result := FOpcodePairs^[APrev, ACur];
end;

function TGocciaProfiler.GetFunctionProfile(
  const AIndex: Integer): TGocciaFunctionProfile;
begin
  Result := FFunctionProfiles[AIndex];
  if FDeterministic then
  begin
    Result.SelfTimeNanoseconds := 0;
    Result.TotalTimeNanoseconds := 0;
  end;
end;

end.
