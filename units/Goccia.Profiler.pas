unit Goccia.Profiler;

{$I Goccia.inc}

interface

uses
  OrderedStringMap;

const
  MAX_OPCODE_ORDINAL = 255;
  OPCODE_PAIR_SIZE = 256;
  INITIAL_FUNCTION_PROFILE_CAPACITY = 64;
  INITIAL_TIMING_STACK_CAPACITY = 32;

type
  TGocciaProfileModeItem = (pmOpcodes, pmFunctions);
  TGocciaProfileMode = set of TGocciaProfileModeItem;

  TGocciaFunctionProfile = record
    TemplateName: string;
    SourceFile: string;
    Line: Integer;
    CallCount: Int64;
    SelfTimeNanoseconds: Int64;
    TotalTimeNanoseconds: Int64;
    Allocations: Int64;
  end;

  TGocciaProfilingStackEntry = record
    ProfileIndex: Integer;
    EntryTimestamp: Int64;
    ChildTimeAccumulated: Int64;
  end;

  TGocciaOpcodePairArray = array[0..OPCODE_PAIR_SIZE - 1,
    0..OPCODE_PAIR_SIZE - 1] of Int64;
  PGocciaOpcodePairArray = ^TGocciaOpcodePairArray;

var
  GProfilingAllocations: Boolean;

type
  TGocciaProfiler = class
  private class var
    FInstance: TGocciaProfiler;
  private
    FMode: TGocciaProfileMode;
    FEnabled: Boolean;
    FOpcodeCount: array[0..MAX_OPCODE_ORDINAL] of Int64;
    FHasPrevOp: Boolean;
    FPrevOp: UInt8;
    FOpcodePairs: PGocciaOpcodePairArray;
    FScalarHits: Int64;
    FScalarMisses: Int64;
    FFunctionProfiles: array of TGocciaFunctionProfile;
    FFunctionProfileCount: Integer;
    FTimingStack: array of TGocciaProfilingStackEntry;
    FTimingStackCount: Integer;
    FCollapsedStacks: TOrderedStringMap<Int64>;
  public
    class function Instance: TGocciaProfiler;
    class procedure Initialize;
    class procedure Shutdown;

    constructor Create;
    destructor Destroy; override;

    procedure RecordOpcode(const AOp: UInt8); inline;
    procedure RecordScalarHit; inline;
    procedure RecordScalarMiss; inline;
    procedure RecordAllocation; inline;

    function RegisterTemplate(const AName, ASourceFile: string;
      const ALine: Integer): Integer;
    procedure PushFunction(const AProfileIndex: Integer;
      const ATimestamp: Int64);
    procedure PopFunction(const AProfileIndex: Integer;
      const ATimestamp: Int64);

    function GetOpcodeCount(const AOp: UInt8): Int64; inline;
    function GetOpcodePairCount(const APrev, ACur: UInt8): Int64; inline;
    function GetFunctionProfile(const AIndex: Integer): TGocciaFunctionProfile; inline;

    property Mode: TGocciaProfileMode read FMode write FMode;
    property Enabled: Boolean read FEnabled write FEnabled;
    property FunctionProfileCount: Integer read FFunctionProfileCount;
    property ScalarHits: Int64 read FScalarHits;
    property ScalarMisses: Int64 read FScalarMisses;
    property CollapsedStacks: TOrderedStringMap<Int64> read FCollapsedStacks;
  end;

implementation

uses
  SysUtils;

{ TGocciaProfiler }

class function TGocciaProfiler.Instance: TGocciaProfiler;
begin
  Result := FInstance;
end;

class procedure TGocciaProfiler.Initialize;
begin
  if not Assigned(FInstance) then
    FInstance := TGocciaProfiler.Create;
end;

class procedure TGocciaProfiler.Shutdown;
begin
  FreeAndNil(FInstance);
end;

constructor TGocciaProfiler.Create;
var
  I: Integer;
begin
  inherited Create;
  GProfilingAllocations := False;
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
end;

destructor TGocciaProfiler.Destroy;
begin
  FCollapsedStacks.Free;
  if Assigned(FOpcodePairs) then
    FreeMem(FOpcodePairs);
  inherited;
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
begin
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
  Inc(FFunctionProfileCount);
end;

procedure TGocciaProfiler.PushFunction(const AProfileIndex: Integer;
  const ATimestamp: Int64);
begin
  if (AProfileIndex < 0) or (AProfileIndex >= FFunctionProfileCount) then Exit;
  if FTimingStackCount >= Length(FTimingStack) then
    SetLength(FTimingStack, FTimingStackCount * 2 + 16);
  FTimingStack[FTimingStackCount].ProfileIndex := AProfileIndex;
  FTimingStack[FTimingStackCount].EntryTimestamp := ATimestamp;
  FTimingStack[FTimingStackCount].ChildTimeAccumulated := 0;
  Inc(FTimingStackCount);
  Inc(FFunctionProfiles[AProfileIndex].CallCount);
end;

procedure TGocciaProfiler.PopFunction(const AProfileIndex: Integer;
  const ATimestamp: Int64);
var
  Elapsed, SelfTime: Int64;
  PoppedIndex: Integer;
  ExistingValue: Int64;
  StackPath: string;
  I: Integer;
begin
  if FTimingStackCount <= 0 then Exit;
  Dec(FTimingStackCount);
  PoppedIndex := FTimingStack[FTimingStackCount].ProfileIndex;
  if (PoppedIndex < 0) or (PoppedIndex >= FFunctionProfileCount) then Exit;
  Elapsed := ATimestamp - FTimingStack[FTimingStackCount].EntryTimestamp;
  SelfTime := Elapsed - FTimingStack[FTimingStackCount].ChildTimeAccumulated;
  Inc(FFunctionProfiles[PoppedIndex].TotalTimeNanoseconds, Elapsed);
  Inc(FFunctionProfiles[PoppedIndex].SelfTimeNanoseconds, SelfTime);
  if FTimingStackCount > 0 then
    Inc(FTimingStack[FTimingStackCount - 1].ChildTimeAccumulated, Elapsed);

  // Build collapsed stack path for flame graph export
  StackPath := '';
  for I := 0 to FTimingStackCount - 1 do
  begin
    if StackPath <> '' then
      StackPath := StackPath + ';';
    if FFunctionProfiles[FTimingStack[I].ProfileIndex].TemplateName <> '' then
      StackPath := StackPath + FFunctionProfiles[FTimingStack[I].ProfileIndex].TemplateName
    else
      StackPath := StackPath + '<anonymous>';
  end;
  if StackPath <> '' then
    StackPath := StackPath + ';';
  if FFunctionProfiles[PoppedIndex].TemplateName <> '' then
    StackPath := StackPath + FFunctionProfiles[PoppedIndex].TemplateName
  else
    StackPath := StackPath + '<anonymous>';

  // Accumulate self-time in nanoseconds (converted to microseconds at export)
  if SelfTime > 0 then
  begin
    if FCollapsedStacks.TryGetValue(StackPath, ExistingValue) then
      FCollapsedStacks.AddOrSetValue(StackPath, ExistingValue + SelfTime)
    else
      FCollapsedStacks.Add(StackPath, SelfTime);
  end;
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
end;

end.
