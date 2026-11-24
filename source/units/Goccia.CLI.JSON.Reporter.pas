unit Goccia.CLI.JSON.Reporter;

{$I Goccia.inc}

interface

uses
  SysUtils;

type
  TCLIJSONTiming = record
    LexTimeNanoseconds: Int64;
    ParseTimeNanoseconds: Int64;
    CompileTimeNanoseconds: Int64;
    ExecuteTimeNanoseconds: Int64;
    TotalTimeNanoseconds: Int64;
  end;

  TCLIJSONErrorInfo = record
    ErrorType: string;
    Message: string;
    FileName: string;
    Line: Integer;
    Column: Integer;
  end;

  TCLIJSONMemoryStats = record
    Enabled: Boolean;
    GCStartBytes: Int64;
    GCEndBytes: Int64;
    GCPeakBytes: Int64;
    GCLiveBytes: Int64;
    GCDeltaBytes: Int64;
    GCAllocatedDuringRunBytes: Int64;
    GCMaxBytes: Int64;
    GCStartObjectCount: Integer;
    GCEndObjectCount: Integer;
    GCCollections: Integer;
    GCCollectedObjects: Int64;
    HeapStartAllocatedBytes: Int64;
    HeapEndAllocatedBytes: Int64;
    HeapDeltaAllocatedBytes: Int64;
    HeapStartFreeBytes: Int64;
    HeapEndFreeBytes: Int64;
    HeapDeltaFreeBytes: Int64;
  end;

  TCLIJSONMemoryMeasurement = record
    Stats: TCLIJSONMemoryStats;
    StartTotalBytesAllocated: Int64;
    StartCollections: Integer;
    StartCollectedObjects: Int64;
  end;

function DefaultCLIJSONErrorInfo: TCLIJSONErrorInfo;
function DefaultCLIJSONMemoryStats: TCLIJSONMemoryStats;
function CombineCLIJSONMemoryStats(const ABase, AAdditional: TCLIJSONMemoryStats;
  const APreserveBaseHeap: Boolean): TCLIJSONMemoryStats;
function BuildCLIBuildJSON: string;
function BuildCLIOutputJSON(const AOutputText: string): string;
function BuildCLITimingJSON(const ATiming: TCLIJSONTiming): string;
function BuildCLIMemoryJSON(const AMemoryStats: TCLIJSONMemoryStats): string;
function BuildCLIWorkersJSON(const AUsed, AAvailable: Integer): string;
function BuildCLIErrorObjectJSON(const AErrorInfo: TCLIJSONErrorInfo): string;
function BuildCLIFileBaseJSON(const AFileName: string; const AOk: Boolean;
  const AOutputText, AStdoutText, AStderrText, AErrorJSON: string;
  const ATiming: TCLIJSONTiming; const AMemoryJSON: string;
  const ACompact: Boolean = False): string;
function BuildCLIReportJSON(const AOk: Boolean; const AOutputText,
  AStdoutText, AStderrText, AErrorJSON: string;
  const ATiming: TCLIJSONTiming;
  const AMemoryStats: TCLIJSONMemoryStats; const AWorkerCount,
  AAvailableWorkerCount: Integer; const AFilesJSON: string;
  const AExtraPropertiesJSON: string = '';
  const ACompact: Boolean = False): string;
{ A host-side exception as the report's error object (a guest value an engine
  threw is that engine's: Goccia.CLI.JSON.EngineReporter for the old one) }
function ExceptionToCLIJSONErrorInfo(const E: Exception): TCLIJSONErrorInfo;
function BuildCLIScriptFileSuccessJSON(const AFileName: string;
  const AResultJSON: string; const AOutputText, AStdoutText,
  AStderrText: string; const ATiming: TCLIJSONTiming;
  const AMemoryStats: TCLIJSONMemoryStats;
  const ACompact: Boolean = False): string;
function BuildCLIScriptFileErrorJSON(const AFileName, AOutputText,
  AStdoutText, AStderrText: string; const AErrorInfo: TCLIJSONErrorInfo;
  const ATiming: TCLIJSONTiming;
  const AMemoryStats: TCLIJSONMemoryStats;
  const ACompact: Boolean = False): string;
function BuildCLIScriptSuccessJSON(const AFileName, AResultJSON: string;
  const AOutputText, AStdoutText, AStderrText: string;
  const ATiming: TCLIJSONTiming;
  const AMemoryStats: TCLIJSONMemoryStats;
  const AWorkerCount, AAvailableWorkerCount: Integer;
  const ACompact: Boolean = False): string;
function BuildCLIScriptErrorJSON(const AFileName, AOutputText, AStdoutText,
  AStderrText: string; const AErrorInfo: TCLIJSONErrorInfo;
  const ATiming: TCLIJSONTiming;
  const AMemoryStats: TCLIJSONMemoryStats;
  const AWorkerCount, AAvailableWorkerCount: Integer;
  const ACompact: Boolean = False): string;

implementation

uses
  StrUtils,

  StringBuffer,

  Goccia.Error,
  Goccia.InstructionLimit,
  Goccia.JSON.Utils,
  Goccia.MemoryLimit,
  Goccia.Platform,
  Goccia.Timeout,
  Goccia.Version;

function SerializeNullableInteger(const AValue: Integer): string;
begin
  if AValue <= 0 then
    Result := 'null'
  else
    Result := IntToStr(AValue);
end;

function SerializeNullableString(const AValue: string): string;
begin
  if AValue = '' then
    Result := 'null'
  else
    Result := QuoteJSONString(AValue);
end;

function BuildCLIErrorObjectJSON(const AErrorInfo: TCLIJSONErrorInfo): string;
begin
  Result :=
    '{' +
      '"type":' + QuoteJSONString(AErrorInfo.ErrorType) + ',' +
      '"message":' + QuoteJSONString(AErrorInfo.Message) + ',' +
      '"line":' + SerializeNullableInteger(AErrorInfo.Line) + ',' +
      '"column":' + SerializeNullableInteger(AErrorInfo.Column) + ',' +
      '"fileName":' + SerializeNullableString(AErrorInfo.FileName) +
    '}';
end;

function BuildCLITimingJSON(const ATiming: TCLIJSONTiming): string;
begin
  Result :=
    '"timing":{' +
      '"lex_ns":' + IntToStr(ATiming.LexTimeNanoseconds) + ',' +
      '"parse_ns":' + IntToStr(ATiming.ParseTimeNanoseconds) + ',' +
      '"compile_ns":' + IntToStr(ATiming.CompileTimeNanoseconds) + ',' +
      '"exec_ns":' + IntToStr(ATiming.ExecuteTimeNanoseconds) + ',' +
      '"total_ns":' + IntToStr(ATiming.TotalTimeNanoseconds) +
    '}';
end;

function BuildCLIBuildJSON: string;
begin
  Result :=
    '"build":{' +
      '"version":' + QuoteJSONString(GetVersion) + ',' +
      '"date":' + QuoteJSONString(GetBuildDate) + ',' +
      '"commit":' + QuoteJSONString(GetCommit) + ',' +
      '"os":' + QuoteJSONString(GetBuildOS) + ',' +
      '"arch":' + QuoteJSONString(GetBuildArch) +
    '}';
end;

// One element per console line: every line a line feed ends, empty ones
// included, then the text after the last line feed when there is any.
function BuildCLIOutputJSON(const AOutputText: string): string;
var
  I, StartIndex: Integer;
  Line: string;
  Buffer: TStringBuffer;
  IsFirstLine: Boolean;
begin
  Buffer := TStringBuffer.Create(Length(AOutputText) + 16);
  Buffer.Append('"output":[');
  IsFirstLine := True;
  StartIndex := 1;
  for I := 1 to Length(AOutputText) do
    if AOutputText[I] = #10 then
    begin
      Line := Copy(AOutputText, StartIndex, I - StartIndex);
      if (Length(Line) > 0) and (Line[Length(Line)] = #13) then
        Delete(Line, Length(Line), 1);
      if not IsFirstLine then
        Buffer.Append(',');
      Buffer.Append(QuoteJSONString(Line));
      IsFirstLine := False;
      StartIndex := I + 1;
    end;

  if StartIndex <= Length(AOutputText) then
  begin
    if not IsFirstLine then
      Buffer.Append(',');
    Buffer.Append(QuoteJSONString(Copy(AOutputText, StartIndex,
      Length(AOutputText) - StartIndex + 1)));
  end;

  Buffer.Append(']');
  Result := Buffer.ToString;
end;

function BuildCLIMemoryJSON(const AMemoryStats: TCLIJSONMemoryStats): string;
var
  Buffer: TStringBuffer;
begin
  if not AMemoryStats.Enabled then
    Exit('"memory":null');

  Buffer := TStringBuffer.Create(512);
  Buffer.Append('"memory":{"gc":{');
  Buffer.Append('"liveBytes":');
  Buffer.Append(IntToStr(AMemoryStats.GCLiveBytes));
  Buffer.Append(',"startLiveBytes":');
  Buffer.Append(IntToStr(AMemoryStats.GCStartBytes));
  Buffer.Append(',"endLiveBytes":');
  Buffer.Append(IntToStr(AMemoryStats.GCEndBytes));
  Buffer.Append(',"peakLiveBytes":');
  Buffer.Append(IntToStr(AMemoryStats.GCPeakBytes));
  Buffer.Append(',"deltaLiveBytes":');
  Buffer.Append(IntToStr(AMemoryStats.GCDeltaBytes));
  Buffer.Append(',"allocatedDuringRunBytes":');
  Buffer.Append(IntToStr(AMemoryStats.GCAllocatedDuringRunBytes));
  Buffer.Append(',"limitBytes":');
  Buffer.Append(IntToStr(AMemoryStats.GCMaxBytes));
  Buffer.Append(',"startObjectCount":');
  Buffer.Append(IntToStr(AMemoryStats.GCStartObjectCount));
  Buffer.Append(',"endObjectCount":');
  Buffer.Append(IntToStr(AMemoryStats.GCEndObjectCount));
  Buffer.Append(',"collections":');
  Buffer.Append(IntToStr(AMemoryStats.GCCollections));
  Buffer.Append(',"collectedObjects":');
  Buffer.Append(IntToStr(AMemoryStats.GCCollectedObjects));
  Buffer.Append('},"heap":{');
  Buffer.Append('"startAllocatedBytes":');
  Buffer.Append(IntToStr(AMemoryStats.HeapStartAllocatedBytes));
  Buffer.Append(',"endAllocatedBytes":');
  Buffer.Append(IntToStr(AMemoryStats.HeapEndAllocatedBytes));
  Buffer.Append(',"deltaAllocatedBytes":');
  Buffer.Append(IntToStr(AMemoryStats.HeapDeltaAllocatedBytes));
  Buffer.Append(',"startFreeBytes":');
  Buffer.Append(IntToStr(AMemoryStats.HeapStartFreeBytes));
  Buffer.Append(',"endFreeBytes":');
  Buffer.Append(IntToStr(AMemoryStats.HeapEndFreeBytes));
  Buffer.Append(',"deltaFreeBytes":');
  Buffer.Append(IntToStr(AMemoryStats.HeapDeltaFreeBytes));
  Buffer.Append('}}');
  Result := Buffer.ToString;
end;

function BuildCLIWorkersJSON(const AUsed, AAvailable: Integer): string;
var
  Buffer: TStringBuffer;
begin
  Buffer := TStringBuffer.Create(64);
  Buffer.Append('"workers":{"used":');
  Buffer.Append(IntToStr(AUsed));
  Buffer.Append(',"available":');
  Buffer.Append(IntToStr(AAvailable));
  Buffer.Append(',"parallel":');
  Buffer.Append(StrUtils.IfThen(AUsed > 1, 'true', 'false'));
  Buffer.Append('}');
  Result := Buffer.ToString;
end;

function BuildCLIFileBaseJSON(const AFileName: string; const AOk: Boolean;
  const AOutputText, AStdoutText, AStderrText, AErrorJSON: string;
  const ATiming: TCLIJSONTiming; const AMemoryJSON: string;
  const ACompact: Boolean): string;
var
  Buffer: TStringBuffer;
begin
  Buffer := TStringBuffer.Create(256 + Length(AFileName) + Length(AOutputText) +
    Length(AStdoutText) + Length(AStderrText) + Length(AErrorJSON) +
    Length(AMemoryJSON));
  Buffer.Append('"fileName":');
  Buffer.Append(QuoteJSONString(AFileName));
  Buffer.Append(',"ok":');
  Buffer.Append(StrUtils.IfThen(AOk, 'true', 'false'));
  if not ACompact then
  begin
    Buffer.Append(',"stdout":');
    Buffer.Append(QuoteJSONString(AStdoutText));
    Buffer.Append(',"stderr":');
    Buffer.Append(QuoteJSONString(AStderrText));
  end;
  Buffer.Append(',');
  Buffer.Append(BuildCLIOutputJSON(AOutputText));
  Buffer.Append(',"error":');
  Buffer.Append(AErrorJSON);
  Buffer.Append(',');
  Buffer.Append(BuildCLITimingJSON(ATiming));
  if not ACompact then
  begin
    Buffer.Append(',');
    Buffer.Append(AMemoryJSON);
  end;
  Result := Buffer.ToString;
end;

function BuildCLIReportJSON(const AOk: Boolean; const AOutputText,
  AStdoutText, AStderrText, AErrorJSON: string;
  const ATiming: TCLIJSONTiming;
  const AMemoryStats: TCLIJSONMemoryStats; const AWorkerCount,
  AAvailableWorkerCount: Integer; const AFilesJSON: string;
  const AExtraPropertiesJSON: string;
  const ACompact: Boolean): string;
var
  ExtraProperties: string;
  Buffer: TStringBuffer;
begin
  ExtraProperties := AExtraPropertiesJSON;
  if (ExtraProperties <> '') and (ExtraProperties[Length(ExtraProperties)] <> ',') then
    ExtraProperties := ExtraProperties + ',';

  Buffer := TStringBuffer.Create(512 + Length(AOutputText) + Length(AStdoutText) +
    Length(AStderrText) + Length(AErrorJSON) + Length(AFilesJSON) +
    Length(ExtraProperties));
  Buffer.Append('{');
  if not ACompact then
  begin
    Buffer.Append(BuildCLIBuildJSON);
    Buffer.Append(',');
  end;
  Buffer.Append('"ok":');
  Buffer.Append(StrUtils.IfThen(AOk, 'true', 'false'));
  if not ACompact then
  begin
    Buffer.Append(',"stdout":');
    Buffer.Append(QuoteJSONString(AStdoutText));
    Buffer.Append(',"stderr":');
    Buffer.Append(QuoteJSONString(AStderrText));
  end;
  Buffer.Append(',');
  Buffer.Append(BuildCLIOutputJSON(AOutputText));
  Buffer.Append(',"error":');
  Buffer.Append(AErrorJSON);
  Buffer.Append(',');
  Buffer.Append(BuildCLITimingJSON(ATiming));
  if not ACompact then
  begin
    Buffer.Append(',');
    Buffer.Append(BuildCLIMemoryJSON(AMemoryStats));
  end;
  Buffer.Append(',');
  Buffer.Append(BuildCLIWorkersJSON(AWorkerCount, AAvailableWorkerCount));
  Buffer.Append(',');
  Buffer.Append(ExtraProperties);
  Buffer.Append('"files": [');
  Buffer.Append(AFilesJSON);
  Buffer.Append(']}');
  Result := Buffer.ToString;
end;

function DefaultCLIJSONErrorInfo: TCLIJSONErrorInfo;
begin
  Result.ErrorType := '';
  Result.Message := '';
  Result.FileName := '';
  Result.Line := -1;
  Result.Column := -1;
end;

function DefaultCLIJSONMemoryStats: TCLIJSONMemoryStats;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.Enabled := False;
end;

function CombineCLIJSONMemoryStats(const ABase,
  AAdditional: TCLIJSONMemoryStats; const APreserveBaseHeap: Boolean): TCLIJSONMemoryStats;
begin
  Result := ABase;
  if not AAdditional.Enabled then
    Exit;

  Result.Enabled := Result.Enabled or AAdditional.Enabled;
  Result.GCStartBytes := Result.GCStartBytes + AAdditional.GCStartBytes;
  Result.GCEndBytes := Result.GCEndBytes + AAdditional.GCEndBytes;
  Result.GCPeakBytes := Result.GCPeakBytes + AAdditional.GCPeakBytes;
  Result.GCLiveBytes := Result.GCLiveBytes + AAdditional.GCLiveBytes;
  Result.GCDeltaBytes := Result.GCDeltaBytes + AAdditional.GCDeltaBytes;
  Result.GCAllocatedDuringRunBytes := Result.GCAllocatedDuringRunBytes +
    AAdditional.GCAllocatedDuringRunBytes;
  if AAdditional.GCMaxBytes > Result.GCMaxBytes then
    Result.GCMaxBytes := AAdditional.GCMaxBytes;
  Result.GCStartObjectCount := Result.GCStartObjectCount +
    AAdditional.GCStartObjectCount;
  Result.GCEndObjectCount := Result.GCEndObjectCount +
    AAdditional.GCEndObjectCount;
  Result.GCCollections := Result.GCCollections + AAdditional.GCCollections;
  Result.GCCollectedObjects := Result.GCCollectedObjects +
    AAdditional.GCCollectedObjects;

  if APreserveBaseHeap then
    Exit;

  Result.HeapStartAllocatedBytes := Result.HeapStartAllocatedBytes +
    AAdditional.HeapStartAllocatedBytes;
  Result.HeapEndAllocatedBytes := Result.HeapEndAllocatedBytes +
    AAdditional.HeapEndAllocatedBytes;
  Result.HeapDeltaAllocatedBytes := Result.HeapDeltaAllocatedBytes +
    AAdditional.HeapDeltaAllocatedBytes;
  Result.HeapStartFreeBytes := Result.HeapStartFreeBytes +
    AAdditional.HeapStartFreeBytes;
  Result.HeapEndFreeBytes := Result.HeapEndFreeBytes +
    AAdditional.HeapEndFreeBytes;
  Result.HeapDeltaFreeBytes := Result.HeapDeltaFreeBytes +
    AAdditional.HeapDeltaFreeBytes;
end;

function ExceptionClassToErrorType(const E: Exception): string;
begin
  if E is TGocciaTimeoutError then
    Result := 'TimeoutError'
  { Ahead of its parent: a host's abort is a stop, not a spent budget. }
  else if E is TGocciaAbortError then
    Result := 'RunAbortedError'
  else if E is TGocciaInstructionLimitError then
    Result := 'InstructionLimitError'
  { Named alongside the other two limits: now that a refused allocation
    unwinds to the host instead of being caught by the script, this is the
    channel the host reads it on, and "the sandbox hit its ceiling" has to be
    distinguishable from an ordinary native failure. }
  else if E is TGocciaMemoryLimitError then
    Result := 'MemoryLimitError'
  else if E is TGocciaError then
    Result := ErrorDisplayName(TGocciaError(E))
  else
    Result := E.ClassName;
end;

function ExceptionToCLIJSONErrorInfo(const E: Exception): TCLIJSONErrorInfo;
var
  GocciaError: TGocciaError;
begin
  Result := DefaultCLIJSONErrorInfo;

  if E is TGocciaError then
  begin
    GocciaError := TGocciaError(E);
    Result.ErrorType := ExceptionClassToErrorType(E);
    Result.Message := E.Message;
    Result.FileName := GocciaError.FileName;
    Result.Line := GocciaError.Line;
    Result.Column := GocciaError.Column;
    Exit;
  end;

  Result.ErrorType := ExceptionClassToErrorType(E);
  Result.Message := E.Message;
end;

function BuildCLIScriptSuccessJSON(const AFileName, AResultJSON: string;
  const AOutputText, AStdoutText, AStderrText: string;
  const ATiming: TCLIJSONTiming;
  const AMemoryStats: TCLIJSONMemoryStats;
  const AWorkerCount, AAvailableWorkerCount: Integer;
  const ACompact: Boolean): string;
begin
  Result := BuildCLIReportJSON(True, AOutputText, AStdoutText, AStderrText,
    'null', ATiming, AMemoryStats, AWorkerCount, AAvailableWorkerCount,
    BuildCLIScriptFileSuccessJSON(AFileName, AResultJSON, AOutputText,
      AStdoutText, AStderrText, ATiming, AMemoryStats, ACompact),
    '', ACompact);
end;

function BuildCLIScriptErrorJSON(const AFileName, AOutputText, AStdoutText,
  AStderrText: string; const AErrorInfo: TCLIJSONErrorInfo;
  const ATiming: TCLIJSONTiming;
  const AMemoryStats: TCLIJSONMemoryStats;
  const AWorkerCount, AAvailableWorkerCount: Integer;
  const ACompact: Boolean): string;
begin
  Result := BuildCLIReportJSON(False, AOutputText, AStdoutText, AStderrText,
    BuildCLIErrorObjectJSON(AErrorInfo), ATiming, AMemoryStats, AWorkerCount,
    AAvailableWorkerCount, BuildCLIScriptFileErrorJSON(AFileName,
      AOutputText, AStdoutText, AStderrText, AErrorInfo, ATiming,
      AMemoryStats, ACompact),
    '', ACompact);
end;

function BuildCLIScriptFileSuccessJSON(const AFileName: string;
  const AResultJSON: string; const AOutputText, AStdoutText,
  AStderrText: string; const ATiming: TCLIJSONTiming;
  const AMemoryStats: TCLIJSONMemoryStats;
  const ACompact: Boolean): string;
var
  MemoryJSON: string;
begin
  if ACompact then
    MemoryJSON := ''
  else
    MemoryJSON := BuildCLIMemoryJSON(AMemoryStats);
  Result :=
    '{' +
      BuildCLIFileBaseJSON(AFileName, True, AOutputText, AStdoutText,
        AStderrText, 'null', ATiming, MemoryJSON, ACompact) + ',' +
      '"result":' + AResultJSON +
    '}';
end;

function BuildCLIScriptFileErrorJSON(const AFileName, AOutputText,
  AStdoutText, AStderrText: string; const AErrorInfo: TCLIJSONErrorInfo;
  const ATiming: TCLIJSONTiming;
  const AMemoryStats: TCLIJSONMemoryStats;
  const ACompact: Boolean): string;
var
  MemoryJSON: string;
begin
  if ACompact then
    MemoryJSON := ''
  else
    MemoryJSON := BuildCLIMemoryJSON(AMemoryStats);
  Result :=
    '{' +
      BuildCLIFileBaseJSON(AFileName, False, AOutputText, AStdoutText,
        AStderrText, BuildCLIErrorObjectJSON(AErrorInfo), ATiming,
        MemoryJSON, ACompact) + ',' +
      '"result":null' +
    '}';
end;

end.
