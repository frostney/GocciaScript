unit Goccia.Benchmark.Reporter;

{$I Goccia.inc}

interface

uses
  Classes,

  Goccia.CLI.JSON.Reporter;

type
  TBenchmarkEntry = record
    Suite: string;
    Name: string;
    OpsPerSec: Double;
    MeanMs: Double;
    Iterations: Int64;
    VariancePercentage: Double;
    SetupMs: Double;
    TeardownMs: Double;
    MinOpsPerSec: Double;
    MaxOpsPerSec: Double;
    SampleCount: Integer;
    MinSampleMs: Double;
    P25Ms: Double;
    MedianMs: Double;
    P75Ms: Double;
    P99Ms: Double;
    P999Ms: Double;
    MaxSampleMs: Double;
    SummaryScope: Integer;
    BoxplotScope: Integer;
    Error: string;
  end;

  TBenchmarkFileResult = record
    FileName: string;
    LexTimeNanoseconds: Int64;
    ParseTimeNanoseconds: Int64;
    CompileTimeNanoseconds: Int64;
    ExecuteTimeNanoseconds: Int64;
    Entries: array of TBenchmarkEntry;
    TotalBenchmarks: Integer;
    DurationNanoseconds: Int64;
    DeterministicProfile: Boolean;
  end;

  TBenchmarkReportFormat = (brfConsole, brfText, brfCSV, brfJSON,
    brfCompactJSON);

  TBenchmarkReporter = class
  private
    FFiles: array of TBenchmarkFileResult;
    FFileCount: Integer;
    FOutput: TStringList;
    FWallClockDurationNanoseconds: Int64;
    FJobCount: Integer;
    FMemoryStats: TCLIJSONMemoryStats;
    FRenderCompact: Boolean;

    procedure RenderConsole;
    procedure RenderText;
    procedure RenderCSV;
    procedure RenderJSON;
    procedure RenderScopedViews(const AFile: TBenchmarkFileResult;
      const AIndent: string);

    function GetFileResult(AIndex: Integer): TBenchmarkFileResult;
    function FormatOpsPerSec(const AOps: Double): string;
    function EscapeCSVField(const S: string): string;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddFileResult(const AResult: TBenchmarkFileResult);
    procedure Render(const AFormat: TBenchmarkReportFormat);
    procedure WriteToStream(const AStream: TStream);
    procedure WriteToFile(const AFileName: string);
    procedure WriteToStdOut;
    function HasFailures: Boolean;
    property Output: TStringList read FOutput;
    property FileCount: Integer read FFileCount;
    property Files[AIndex: Integer]: TBenchmarkFileResult read GetFileResult;
    property WallClockDurationNanoseconds: Int64 read FWallClockDurationNanoseconds write FWallClockDurationNanoseconds;
    property JobCount: Integer read FJobCount write FJobCount;
    property MemoryStats: TCLIJSONMemoryStats read FMemoryStats write FMemoryStats;
  end;

function ParseReportFormat(const S: string): TBenchmarkReportFormat;

implementation

uses
  Math,
  SysUtils,

  TimingUtils,

  Goccia.CSV,
  Goccia.JSON.Utils;

function IsPositiveFinite(const AValue: Double): Boolean;
begin
  Result := (AValue > 0) and not Math.IsNan(AValue) and
    not Math.IsInfinite(AValue);
end;

function IsValidBenchmarkEntry(const AEntry: TBenchmarkEntry;
  const ADeterministicProfile: Boolean): Boolean;
begin
  if ADeterministicProfile then
    Result := (AEntry.Error = '') and (AEntry.Iterations > 0)
  else
    Result := (AEntry.Error = '') and IsPositiveFinite(AEntry.OpsPerSec) and
      IsPositiveFinite(AEntry.MeanMs) and (AEntry.Iterations > 0);
end;

function FormatSampleTime(const AMilliseconds: Double): string;
var
  FormatSettings: TFormatSettings;
begin
  FormatSettings := DefaultFormatSettings;
  FormatSettings.DecimalSeparator := '.';
  if AMilliseconds < 0.001 then
    Result := SysUtils.Format('%.2f ns', [AMilliseconds * 1000000],
      FormatSettings)
  else if AMilliseconds < 1 then
    Result := SysUtils.Format('%.2f us', [AMilliseconds * 1000],
      FormatSettings)
  else
    Result := SysUtils.Format('%.2f ms', [AMilliseconds], FormatSettings);
end;

function ScaleSample(const AValue, AMinimum, AMaximum: Double;
  const AWidth: Integer): Integer;
begin
  if AMaximum <= AMinimum then
    Exit(0);
  Result := Round(((AValue - AMinimum) / (AMaximum - AMinimum)) *
    (AWidth - 1));
  if Result < 0 then
    Result := 0
  else if Result >= AWidth then
    Result := AWidth - 1;
end;

function BuildInterquartileBar(const AEntry: TBenchmarkEntry;
  const AMinimum, AMaximum: Double): string;
const
  BAR_WIDTH = 24;
var
  I, MinimumOffset, P25Offset, MedianOffset, P75Offset,
    P99Offset: Integer;
begin
  Result := StringOfChar(' ', BAR_WIDTH);
  MinimumOffset := ScaleSample(AEntry.MinSampleMs, AMinimum, AMaximum,
    BAR_WIDTH) + 1;
  P25Offset := ScaleSample(AEntry.P25Ms, AMinimum, AMaximum, BAR_WIDTH) + 1;
  MedianOffset := ScaleSample(AEntry.MedianMs, AMinimum, AMaximum,
    BAR_WIDTH) + 1;
  P75Offset := ScaleSample(AEntry.P75Ms, AMinimum, AMaximum, BAR_WIDTH) + 1;
  P99Offset := ScaleSample(AEntry.P99Ms, AMinimum, AMaximum, BAR_WIDTH) + 1;

  for I := MinimumOffset to P99Offset do
    Result[I] := '-';
  for I := P25Offset to P75Offset do
    Result[I] := '=';
  Result[MinimumOffset] := '|';
  Result[P99Offset] := '|';
  if P25Offset = P75Offset then
    Result[P25Offset] := '|'
  else
  begin
    Result[P25Offset] := '[';
    Result[P75Offset] := ']';
    Result[MedianOffset] := '|';
  end;
end;

function BenchmarkDisplayName(const AEntry: TBenchmarkEntry): string;
begin
  if AEntry.Suite = '' then
    Result := AEntry.Name
  else
    Result := AEntry.Suite + ' > ' + AEntry.Name;
end;

function FindFastestSummaryEntry(const AFile: TBenchmarkFileResult;
  const AScope: Integer): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to Length(AFile.Entries) - 1 do
    if (AFile.Entries[I].SummaryScope = AScope) and
      (AFile.Entries[I].Error = '') and (AFile.Entries[I].MedianMs > 0) and
      ((Result < 0) or
       (AFile.Entries[I].MedianMs < AFile.Entries[Result].MedianMs)) then
      Result := I;
end;

procedure CalculateRelativeComparison(const AEntry, AFastest: TBenchmarkEntry;
  const AIsBaseline: Boolean; out ARatio, ALowRatio, AHighRatio: Double;
  out AInconclusive: Boolean);
begin
  if AIsBaseline then
  begin
    ARatio := 1;
    ALowRatio := 1;
    AHighRatio := 1;
    AInconclusive := False;
    Exit;
  end;
  ARatio := AEntry.MedianMs / AFastest.MedianMs;
  if (AFastest.P75Ms > 0) and (AFastest.P25Ms > 0) then
  begin
    ALowRatio := AEntry.P25Ms / AFastest.P75Ms;
    AHighRatio := AEntry.P75Ms / AFastest.P25Ms;
  end
  else
  begin
    ALowRatio := ARatio;
    AHighRatio := ARatio;
  end;
  AInconclusive := (ALowRatio <= 1) and (AHighRatio >= 1);
end;

function FormatJSONNumber(const AValue: Double;
  const AFormatSettings: TFormatSettings): string;
begin
  if Math.IsNan(AValue) or Math.IsInfinite(AValue) then
    Exit('null');

  Result := SysUtils.Format('%.6f', [AValue], AFormatSettings);
end;

function FormatJSONPercentage(const AValue: Double;
  const AFormatSettings: TFormatSettings): string;
begin
  if Math.IsNan(AValue) or Math.IsInfinite(AValue) then
    Exit('null');

  Result := SysUtils.Format('%.4f', [AValue], AFormatSettings);
end;

function ParseReportFormat(const S: string): TBenchmarkReportFormat;
var
  Lower: string;
begin
  Lower := LowerCase(S);
  if Lower = 'text' then
    Result := brfText
  else if (Lower = 'csv') then
    Result := brfCSV
  else if (Lower = 'json') then
    Result := brfJSON
  else if (Lower = 'compact-json') then
    Result := brfCompactJSON
  else
    Result := brfConsole;
end;

{ TBenchmarkReporter }

constructor TBenchmarkReporter.Create;
begin
  inherited Create;
  FOutput := TStringList.Create;
  FFileCount := 0;
  FWallClockDurationNanoseconds := 0;
  FJobCount := 1;
  FMemoryStats := DefaultCLIJSONMemoryStats;
  FRenderCompact := False;
  SetLength(FFiles, 0);
end;

destructor TBenchmarkReporter.Destroy;
begin
  FOutput.Free;
  inherited Destroy;
end;

function TBenchmarkReporter.GetFileResult(AIndex: Integer): TBenchmarkFileResult;
begin
  Result := FFiles[AIndex];
end;

procedure TBenchmarkReporter.AddFileResult(const AResult: TBenchmarkFileResult);
begin
  Inc(FFileCount);
  SetLength(FFiles, FFileCount);
  FFiles[FFileCount - 1] := AResult;
end;

function TBenchmarkReporter.FormatOpsPerSec(const AOps: Double): string;
var
  IntOps: Int64;
  S: string;
  I, Len: Integer;
begin
  IntOps := Round(AOps);
  S := IntToStr(IntOps);
  Len := Length(S);

  Result := '';
  for I := 1 to Len do
  begin
    if (I > 1) and ((Len - I + 1) mod 3 = 0) then
      Result := Result + ',';
    Result := Result + S[I];
  end;
end;

function TBenchmarkReporter.EscapeCSVField(const S: string): string;
begin
  Result := TGocciaCSVStringifier.EscapeField(S, ',');
end;

procedure TBenchmarkReporter.RenderScopedViews(
  const AFile: TBenchmarkFileResult; const AIndent: string);
var
  Scope, MaximumBoxplotScope, MaximumSummaryScope, I, Count, Rank,
    CandidateIndex, FastestIndex: Integer;
  MinimumSample, MaximumSample, Ratio, LowRatio, HighRatio: Double;
  Inconclusive: Boolean;
  Printed: array of Boolean;
begin
  MaximumBoxplotScope := 0;
  MaximumSummaryScope := 0;
  for I := 0 to Length(AFile.Entries) - 1 do
  begin
    if AFile.Entries[I].BoxplotScope > MaximumBoxplotScope then
      MaximumBoxplotScope := AFile.Entries[I].BoxplotScope;
    if AFile.Entries[I].SummaryScope > MaximumSummaryScope then
      MaximumSummaryScope := AFile.Entries[I].SummaryScope;
  end;

  for Scope := 1 to MaximumBoxplotScope do
  begin
    Count := 0;
    MinimumSample := MaxDouble;
    MaximumSample := 0;
    for I := 0 to Length(AFile.Entries) - 1 do
      if (AFile.Entries[I].BoxplotScope = Scope) and
        (AFile.Entries[I].Error = '') and
        (AFile.Entries[I].SampleCount > 0) then
      begin
        Inc(Count);
        if AFile.Entries[I].MinSampleMs < MinimumSample then
          MinimumSample := AFile.Entries[I].MinSampleMs;
        if AFile.Entries[I].P99Ms > MaximumSample then
          MaximumSample := AFile.Entries[I].P99Ms;
      end;
    if Count = 0 then
      Continue;

    FOutput.Add('');
    FOutput.Add(AIndent + 'boxplot');
    for I := 0 to Length(AFile.Entries) - 1 do
      if (AFile.Entries[I].BoxplotScope = Scope) and
        (AFile.Entries[I].Error = '') and
        (AFile.Entries[I].SampleCount > 0) then
        FOutput.Add(SysUtils.Format('%s  %-30s %s',
          [AIndent, BenchmarkDisplayName(AFile.Entries[I]),
           BuildInterquartileBar(AFile.Entries[I], MinimumSample,
             MaximumSample)]));
    FOutput.Add(SysUtils.Format('%s  %s .. %s (p99)',
      [AIndent, FormatSampleTime(MinimumSample),
       FormatSampleTime(MaximumSample)]));
  end;

  for Scope := 1 to MaximumSummaryScope do
  begin
    Count := 0;
    for I := 0 to Length(AFile.Entries) - 1 do
      if (AFile.Entries[I].SummaryScope = Scope) and
        (AFile.Entries[I].Error = '') and
        (AFile.Entries[I].SampleCount > 0) then
        Inc(Count);
    if Count < 2 then
      Continue;

    FastestIndex := FindFastestSummaryEntry(AFile, Scope);
    if FastestIndex < 0 then
      Continue;
    SetLength(Printed, Length(AFile.Entries));
    FOutput.Add('');
    FOutput.Add(AIndent + 'summary');
    FOutput.Add(AIndent + '  fastest: ' +
      BenchmarkDisplayName(AFile.Entries[FastestIndex]));
    for Rank := 0 to Count - 1 do
    begin
      CandidateIndex := -1;
      for I := 0 to Length(AFile.Entries) - 1 do
        if not Printed[I] and (AFile.Entries[I].SummaryScope = Scope) and
          (AFile.Entries[I].Error = '') and
          (AFile.Entries[I].SampleCount > 0) and
          ((CandidateIndex < 0) or
           (AFile.Entries[I].MedianMs <
            AFile.Entries[CandidateIndex].MedianMs)) then
          CandidateIndex := I;
      if CandidateIndex < 0 then
        Break;
      Printed[CandidateIndex] := True;
      if CandidateIndex = FastestIndex then
        Continue;

      CalculateRelativeComparison(AFile.Entries[CandidateIndex],
        AFile.Entries[FastestIndex], False, Ratio, LowRatio, HighRatio,
        Inconclusive);
      if Inconclusive then
        FOutput.Add(SysUtils.Format(
          '%s  %6.2fx median (%.2fx..%.2fx IQR; inconclusive)  %s',
          [AIndent, Ratio, LowRatio, HighRatio,
           BenchmarkDisplayName(AFile.Entries[CandidateIndex])]))
      else
        FOutput.Add(SysUtils.Format(
          '%s  %6.2fx faster (%.2fx..%.2fx IQR)        %s',
          [AIndent, Ratio, LowRatio, HighRatio,
           BenchmarkDisplayName(AFile.Entries[CandidateIndex])]));
    end;
  end;
end;

procedure TBenchmarkReporter.Render(const AFormat: TBenchmarkReportFormat);
begin
  FOutput.Clear;
  FRenderCompact := AFormat = brfCompactJSON;
  case AFormat of
    brfConsole:     RenderConsole;
    brfText:        RenderText;
    brfCSV:         RenderCSV;
    brfJSON:        RenderJSON;
    brfCompactJSON: RenderJSON;
  end;
end;

procedure TBenchmarkReporter.RenderConsole;
var
  F, E: Integer;
  Entry: TBenchmarkEntry;
  CurrentSuite: string;
  TotalBenchmarks: Integer;
  TotalDurationNanoseconds: Int64;
  HasSetupTeardown: Boolean;
begin
  TotalBenchmarks := 0;
  TotalDurationNanoseconds := 0;

  for F := 0 to FFileCount - 1 do
  begin
    if FFileCount > 1 then
      FOutput.Add('Running benchmark: ' + FFiles[F].FileName);

    if FFiles[F].CompileTimeNanoseconds > 0 then
      FOutput.Add(SysUtils.Format('  Lex: %s | Parse: %s | Compile: %s | Execute: %s | Total: %s',
        [FormatDuration(FFiles[F].LexTimeNanoseconds), FormatDuration(FFiles[F].ParseTimeNanoseconds),
         FormatDuration(FFiles[F].CompileTimeNanoseconds), FormatDuration(FFiles[F].ExecuteTimeNanoseconds),
         FormatDuration(FFiles[F].LexTimeNanoseconds + FFiles[F].ParseTimeNanoseconds + FFiles[F].CompileTimeNanoseconds + FFiles[F].ExecuteTimeNanoseconds)]))
    else
      FOutput.Add(SysUtils.Format('  Lex: %s | Parse: %s | Execute: %s | Total: %s',
        [FormatDuration(FFiles[F].LexTimeNanoseconds), FormatDuration(FFiles[F].ParseTimeNanoseconds), FormatDuration(FFiles[F].ExecuteTimeNanoseconds),
         FormatDuration(FFiles[F].LexTimeNanoseconds + FFiles[F].ParseTimeNanoseconds + FFiles[F].ExecuteTimeNanoseconds)]));
    FOutput.Add('');

    CurrentSuite := '';
    for E := 0 to Length(FFiles[F].Entries) - 1 do
    begin
      Entry := FFiles[F].Entries[E];

      if Entry.Suite <> CurrentSuite then
      begin
        CurrentSuite := Entry.Suite;
        if CurrentSuite <> '' then
          FOutput.Add('  ' + CurrentSuite);
      end;

      if Entry.Error <> '' then
        FOutput.Add(SysUtils.Format('    %-30s  ERROR: %s', [Entry.Name, Entry.Error]))
      else if Entry.VariancePercentage > 0 then
        FOutput.Add(SysUtils.Format('    %-30s  %12s ops/sec  ±%5.2f%%  %10.4f ms/op  (%d iterations)',
          [Entry.Name, FormatOpsPerSec(Entry.OpsPerSec), Entry.VariancePercentage, Entry.MeanMs, Entry.Iterations]))
      else
        FOutput.Add(SysUtils.Format('    %-30s  %12s ops/sec  %10.4f ms/op  (%d iterations)',
          [Entry.Name, FormatOpsPerSec(Entry.OpsPerSec), Entry.MeanMs, Entry.Iterations]));

      if (Entry.Error = '') and (Entry.MinOpsPerSec > 0) and (Entry.MaxOpsPerSec > 0) then
        FOutput.Add(SysUtils.Format('    %-30s  range: %s .. %s ops/sec',
          ['', FormatOpsPerSec(Entry.MinOpsPerSec), FormatOpsPerSec(Entry.MaxOpsPerSec)]));

      if (Entry.Error = '') and (Entry.SampleCount > 0) then
      begin
        FOutput.Add(SysUtils.Format(
          '    %-30s  p75 %9s  p99 %9s  p999 %9s  (%d samples)',
          ['', FormatSampleTime(Entry.P75Ms), FormatSampleTime(Entry.P99Ms),
           FormatSampleTime(Entry.P999Ms), Entry.SampleCount]));
        FOutput.Add(SysUtils.Format('    %-30s  %s',
          ['', BuildInterquartileBar(Entry, Entry.MinSampleMs,
            Entry.P99Ms)]));
      end;

      HasSetupTeardown := (Entry.SetupMs > 0) or (Entry.TeardownMs > 0);
      if HasSetupTeardown and (Entry.Error = '') then
      begin
        if (Entry.SetupMs > 0) and (Entry.TeardownMs > 0) then
          FOutput.Add(SysUtils.Format('    %-30s  setup: %.4fms  teardown: %.4fms', ['', Entry.SetupMs, Entry.TeardownMs]))
        else if Entry.SetupMs > 0 then
          FOutput.Add(SysUtils.Format('    %-30s  setup: %.4fms', ['', Entry.SetupMs]))
        else
          FOutput.Add(SysUtils.Format('    %-30s  teardown: %.4fms', ['', Entry.TeardownMs]));
      end;
    end;

    RenderScopedViews(FFiles[F], '  ');

    TotalBenchmarks := TotalBenchmarks + FFiles[F].TotalBenchmarks;
    TotalDurationNanoseconds := TotalDurationNanoseconds + FFiles[F].DurationNanoseconds;

    if F < FFileCount - 1 then
      FOutput.Add('');
  end;

  if FWallClockDurationNanoseconds > 0 then
    TotalDurationNanoseconds := FWallClockDurationNanoseconds;

  FOutput.Add('');
  FOutput.Add('Benchmark Summary');
  FOutput.Add(SysUtils.Format('  Total benchmarks: %d', [TotalBenchmarks]));
  FOutput.Add(SysUtils.Format('  Total duration: %s', [FormatDuration(TotalDurationNanoseconds)]));
end;

procedure TBenchmarkReporter.RenderText;
var
  F, E: Integer;
  Entry: TBenchmarkEntry;
  TotalBenchmarks: Integer;
  TotalDurationNanoseconds: Int64;
  Line: string;
begin
  TotalBenchmarks := 0;
  TotalDurationNanoseconds := 0;

  for F := 0 to FFileCount - 1 do
  begin
    FOutput.Add(FFiles[F].FileName);

    for E := 0 to Length(FFiles[F].Entries) - 1 do
    begin
      Entry := FFiles[F].Entries[E];

      if Entry.Error <> '' then
        FOutput.Add(SysUtils.Format('%s > %s: ERROR: %s', [Entry.Suite, Entry.Name, Entry.Error]))
      else
      begin
        if Entry.VariancePercentage > 0 then
          Line := SysUtils.Format('%s > %s: %s ops/sec ±%.2f%% (%.4f ms/op, %d iterations, range %s..%s)',
            [Entry.Suite, Entry.Name, FormatOpsPerSec(Entry.OpsPerSec),
             Entry.VariancePercentage, Entry.MeanMs, Entry.Iterations,
             FormatOpsPerSec(Entry.MinOpsPerSec), FormatOpsPerSec(Entry.MaxOpsPerSec)])
        else
          Line := SysUtils.Format('%s > %s: %s ops/sec (%.4f ms/op, %d iterations)',
            [Entry.Suite, Entry.Name, FormatOpsPerSec(Entry.OpsPerSec),
             Entry.MeanMs, Entry.Iterations]);

        if Entry.SetupMs > 0 then
          Line := Line + SysUtils.Format(' setup=%.4fms', [Entry.SetupMs]);
        if Entry.TeardownMs > 0 then
          Line := Line + SysUtils.Format(' teardown=%.4fms', [Entry.TeardownMs]);
        if Entry.SampleCount > 0 then
          Line := Line + SysUtils.Format(
            ' p75=%s p99=%s p999=%s samples=%d %s',
            [FormatSampleTime(Entry.P75Ms), FormatSampleTime(Entry.P99Ms),
             FormatSampleTime(Entry.P999Ms), Entry.SampleCount,
             BuildInterquartileBar(Entry, Entry.MinSampleMs, Entry.P99Ms)]);

        FOutput.Add(Line);
      end;
    end;

    RenderScopedViews(FFiles[F], '');

    TotalBenchmarks := TotalBenchmarks + FFiles[F].TotalBenchmarks;
    TotalDurationNanoseconds := TotalDurationNanoseconds + FFiles[F].DurationNanoseconds;
  end;

  if FWallClockDurationNanoseconds > 0 then
    TotalDurationNanoseconds := FWallClockDurationNanoseconds;

  FOutput.Add('');
  FOutput.Add(SysUtils.Format('Total: %d benchmarks in %s',
    [TotalBenchmarks, FormatDuration(TotalDurationNanoseconds)]));
end;

procedure TBenchmarkReporter.RenderCSV;
var
  F, E, FastestIndex: Integer;
  Entry: TBenchmarkEntry;
  SummaryScope, BoxplotScope, RelativeFields: string;
  Ratio, LowRatio, HighRatio: Double;
  Inconclusive: Boolean;
  CSVFormatSettings: TFormatSettings;
begin
  CSVFormatSettings := DefaultFormatSettings;
  CSVFormatSettings.DecimalSeparator := '.';
  FOutput.Add('file,suite,name,ops_per_sec,variance_percentage,mean_ms,' +
    'iterations,setup_ms,teardown_ms,min_ops_per_sec,max_ops_per_sec,' +
    'sample_count,min_sample_ms,p25_ms,median_ms,p75_ms,p99_ms,p999_ms,' +
    'max_sample_ms,summary_scope,boxplot_scope,relative_median,' +
    'relative_low,relative_high,relative_inconclusive,error');

  for F := 0 to FFileCount - 1 do
  begin
    for E := 0 to Length(FFiles[F].Entries) - 1 do
    begin
      Entry := FFiles[F].Entries[E];

      if Entry.SummaryScope > 0 then
        SummaryScope := IntToStr(Entry.SummaryScope)
      else
        SummaryScope := '';
      if Entry.BoxplotScope > 0 then
        BoxplotScope := IntToStr(Entry.BoxplotScope)
      else
        BoxplotScope := '';

      if Entry.Error <> '' then
        FOutput.Add(EscapeCSVField(FFiles[F].FileName) + ',' +
          EscapeCSVField(Entry.Suite) + ',' + EscapeCSVField(Entry.Name) +
          StringOfChar(',', 17) + SummaryScope + ',' + BoxplotScope +
          StringOfChar(',', 5) + EscapeCSVField(Entry.Error))
      else
      begin
        RelativeFields := ',,,';
        if Entry.SummaryScope > 0 then
        begin
          FastestIndex := FindFastestSummaryEntry(FFiles[F],
            Entry.SummaryScope);
          if FastestIndex >= 0 then
          begin
            CalculateRelativeComparison(Entry,
              FFiles[F].Entries[FastestIndex], E = FastestIndex, Ratio,
              LowRatio, HighRatio, Inconclusive);
            RelativeFields := SysUtils.Format('%.6f,%.6f,%.6f,%s',
              [Ratio, LowRatio, HighRatio,
               LowerCase(BoolToStr(Inconclusive, True))], CSVFormatSettings);
          end;
        end;
        FOutput.Add(SysUtils.Format(
          '%s,%s,%s,%.6f,%.4f,%.6f,%d,%.6f,%.6f,%.6f,%.6f,' +
          '%d,%.6f,%.6f,%.6f,%.6f,%.6f,%.6f,%.6f,%s,%s,%s,',
          [EscapeCSVField(FFiles[F].FileName), EscapeCSVField(Entry.Suite),
           EscapeCSVField(Entry.Name), Entry.OpsPerSec, Entry.VariancePercentage,
           Entry.MeanMs, Entry.Iterations, Entry.SetupMs, Entry.TeardownMs,
           Entry.MinOpsPerSec, Entry.MaxOpsPerSec, Entry.SampleCount,
           Entry.MinSampleMs, Entry.P25Ms, Entry.MedianMs, Entry.P75Ms,
           Entry.P99Ms, Entry.P999Ms, Entry.MaxSampleMs, SummaryScope,
           BoxplotScope, RelativeFields], CSVFormatSettings));
      end;
    end;
  end;
end;

procedure TBenchmarkReporter.RenderJSON;
var
  F, E, FastestIndex: Integer;
  Entry: TBenchmarkEntry;
  TotalBenchmarks: Integer;
  TotalDurationNanoseconds: Int64;
  Timing: TCLIJSONTiming;
  FileTiming: TCLIJSONTiming;
  ErrorInfo: TCLIJSONErrorInfo;
  JSONFormatSettings: TFormatSettings;
  FileOk: Boolean;
  ValidFileBenchmarkCount: Integer;
  FilesJSON, FileJSON, BenchmarksJSON, BenchmarkJSON, ExtraJSON: string;
  FileErrorJSON, FileErrorMessage, SummaryScopeJSON, BoxplotScopeJSON,
    RelativeJSON: string;
  Ratio, LowRatio, HighRatio: Double;
  Inconclusive: Boolean;
begin
  TotalBenchmarks := 0;
  TotalDurationNanoseconds := 0;
  FilesJSON := '';
  JSONFormatSettings := DefaultFormatSettings;
  JSONFormatSettings.DecimalSeparator := '.';

  for F := 0 to FFileCount - 1 do
  begin
    FileTiming.LexTimeNanoseconds := FFiles[F].LexTimeNanoseconds;
    FileTiming.ParseTimeNanoseconds := FFiles[F].ParseTimeNanoseconds;
    FileTiming.CompileTimeNanoseconds := FFiles[F].CompileTimeNanoseconds;
    FileTiming.ExecuteTimeNanoseconds := FFiles[F].ExecuteTimeNanoseconds;
    FileTiming.TotalTimeNanoseconds := FFiles[F].LexTimeNanoseconds +
      FFiles[F].ParseTimeNanoseconds + FFiles[F].CompileTimeNanoseconds +
      FFiles[F].ExecuteTimeNanoseconds;

    BenchmarksJSON := '';
    ValidFileBenchmarkCount := 0;
    FileOk := (FFiles[F].TotalBenchmarks > 0) and
      (Length(FFiles[F].Entries) > 0);
    FileErrorMessage := '';
    if not FileOk then
      FileErrorMessage := 'Benchmark file produced no measurements';
    for E := 0 to Length(FFiles[F].Entries) - 1 do
    begin
      Entry := FFiles[F].Entries[E];

      if Entry.SummaryScope > 0 then
        SummaryScopeJSON := IntToStr(Entry.SummaryScope)
      else
        SummaryScopeJSON := 'null';
      if Entry.BoxplotScope > 0 then
        BoxplotScopeJSON := IntToStr(Entry.BoxplotScope)
      else
        BoxplotScopeJSON := 'null';
      RelativeJSON := 'null';
      if (Entry.Error = '') and (Entry.SummaryScope > 0) then
      begin
        FastestIndex := FindFastestSummaryEntry(FFiles[F],
          Entry.SummaryScope);
        if FastestIndex >= 0 then
        begin
          CalculateRelativeComparison(Entry,
            FFiles[F].Entries[FastestIndex], E = FastestIndex, Ratio,
            LowRatio, HighRatio, Inconclusive);
          RelativeJSON := SysUtils.Format(
            '{"median":%s,"low":%s,"high":%s,"inconclusive":%s}',
            [FormatJSONNumber(Ratio, JSONFormatSettings),
             FormatJSONNumber(LowRatio, JSONFormatSettings),
             FormatJSONNumber(HighRatio, JSONFormatSettings),
             LowerCase(BoolToStr(Inconclusive, True))]);
        end;
      end;

      if IsValidBenchmarkEntry(Entry, FFiles[F].DeterministicProfile) then
        Inc(ValidFileBenchmarkCount)
      else if FileOk then
      begin
        FileOk := False;
        if Entry.Error <> '' then
          FileErrorMessage := Entry.Error
        else
          FileErrorMessage := SysUtils.Format(
            'Benchmark "%s" produced no measurements',
            [Entry.Name]);
      end;

      if Entry.Error <> '' then
      begin
        BenchmarkJSON := SysUtils.Format(
          '{"suite":"%s","name":"%s","summaryScope":%s,' +
          '"boxplotScope":%s,"error":"%s"}',
          [EscapeJSONString(Entry.Suite), EscapeJSONString(Entry.Name),
           SummaryScopeJSON, BoxplotScopeJSON,
           EscapeJSONString(Entry.Error)])
      end
      else
        BenchmarkJSON := SysUtils.Format(
          '{"suite":"%s","name":"%s","opsPerSec":%s,' +
          '"variancePercentage":%s,"meanMs":%s,"iterations":%d,' +
          '"setupMs":%s,"teardownMs":%s,"minOpsPerSec":%s,' +
          '"maxOpsPerSec":%s,"sampleCount":%d,"minSampleMs":%s,' +
          '"p25Ms":%s,"medianMs":%s,"p75Ms":%s,"p99Ms":%s,' +
          '"p999Ms":%s,"maxSampleMs":%s,"summaryScope":%s,' +
          '"boxplotScope":%s,"relative":%s}',
          [EscapeJSONString(Entry.Suite), EscapeJSONString(Entry.Name),
           FormatJSONNumber(Entry.OpsPerSec, JSONFormatSettings),
           FormatJSONPercentage(Entry.VariancePercentage, JSONFormatSettings),
           FormatJSONNumber(Entry.MeanMs, JSONFormatSettings), Entry.Iterations,
           FormatJSONNumber(Entry.SetupMs, JSONFormatSettings),
           FormatJSONNumber(Entry.TeardownMs, JSONFormatSettings),
           FormatJSONNumber(Entry.MinOpsPerSec, JSONFormatSettings),
           FormatJSONNumber(Entry.MaxOpsPerSec, JSONFormatSettings),
           Entry.SampleCount,
           FormatJSONNumber(Entry.MinSampleMs, JSONFormatSettings),
           FormatJSONNumber(Entry.P25Ms, JSONFormatSettings),
           FormatJSONNumber(Entry.MedianMs, JSONFormatSettings),
           FormatJSONNumber(Entry.P75Ms, JSONFormatSettings),
           FormatJSONNumber(Entry.P99Ms, JSONFormatSettings),
           FormatJSONNumber(Entry.P999Ms, JSONFormatSettings),
           FormatJSONNumber(Entry.MaxSampleMs, JSONFormatSettings),
           SummaryScopeJSON, BoxplotScopeJSON, RelativeJSON]);

      if BenchmarksJSON <> '' then
        BenchmarksJSON := BenchmarksJSON + ',';
      BenchmarksJSON := BenchmarksJSON + BenchmarkJSON;
    end;

    if (ValidFileBenchmarkCount = 0) and FileOk then
    begin
      FileOk := False;
      FileErrorMessage := 'Benchmark file produced no valid measurements';
    end;

    if FileOk then
      FileErrorJSON := 'null'
    else
    begin
      ErrorInfo := DefaultCLIJSONErrorInfo;
      ErrorInfo.ErrorType := 'BenchmarkError';
      ErrorInfo.Message := FileErrorMessage;
      ErrorInfo.FileName := FFiles[F].FileName;
      FileErrorJSON := BuildCLIErrorObjectJSON(ErrorInfo);
    end;

    FileJSON :=
      '{' +
        BuildCLIFileBaseJSON(FFiles[F].FileName, FileOk, '', '', '',
          FileErrorJSON, FileTiming, '"memory":null', FRenderCompact) + ',' +
        SysUtils.Format('"lexTimeNanoseconds":%d,', [FFiles[F].LexTimeNanoseconds]) +
        SysUtils.Format('"parseTimeNanoseconds":%d,', [FFiles[F].ParseTimeNanoseconds]) +
        SysUtils.Format('"compileTimeNanoseconds":%d,', [FFiles[F].CompileTimeNanoseconds]) +
        SysUtils.Format('"executeTimeNanoseconds":%d,', [FFiles[F].ExecuteTimeNanoseconds]) +
        '"benchmarks":[' + BenchmarksJSON + ']' +
      '}';
    if FilesJSON <> '' then
      FilesJSON := FilesJSON + ',';
    FilesJSON := FilesJSON + FileJSON;

    TotalBenchmarks := TotalBenchmarks + FFiles[F].TotalBenchmarks;
    TotalDurationNanoseconds := TotalDurationNanoseconds + FFiles[F].DurationNanoseconds;
  end;

  if FWallClockDurationNanoseconds > 0 then
    TotalDurationNanoseconds := FWallClockDurationNanoseconds;

  Timing.LexTimeNanoseconds := 0;
  Timing.ParseTimeNanoseconds := 0;
  Timing.CompileTimeNanoseconds := 0;
  Timing.ExecuteTimeNanoseconds := 0;
  for F := 0 to FFileCount - 1 do
  begin
    Timing.LexTimeNanoseconds := Timing.LexTimeNanoseconds +
      FFiles[F].LexTimeNanoseconds;
    Timing.ParseTimeNanoseconds := Timing.ParseTimeNanoseconds +
      FFiles[F].ParseTimeNanoseconds;
    Timing.CompileTimeNanoseconds := Timing.CompileTimeNanoseconds +
      FFiles[F].CompileTimeNanoseconds;
    Timing.ExecuteTimeNanoseconds := Timing.ExecuteTimeNanoseconds +
      FFiles[F].ExecuteTimeNanoseconds;
  end;
  Timing.TotalTimeNanoseconds := Timing.LexTimeNanoseconds +
    Timing.ParseTimeNanoseconds + Timing.CompileTimeNanoseconds +
    Timing.ExecuteTimeNanoseconds;

  ExtraJSON :=
    SysUtils.Format('"jobCount":%d,', [FJobCount]) +
    SysUtils.Format('"totalBenchmarks":%d,', [TotalBenchmarks]) +
    SysUtils.Format('"totalDurationNanoseconds":%d', [TotalDurationNanoseconds]);
  FOutput.Text := BuildCLIReportJSON(not HasFailures, '', '', '', 'null',
    Timing, FMemoryStats, FJobCount, FJobCount, FilesJSON, ExtraJSON,
    FRenderCompact);
end;

procedure TBenchmarkReporter.WriteToStream(const AStream: TStream);
var
  S: string;
begin
  S := FOutput.Text;
  if Length(S) > 0 then
    AStream.WriteBuffer(S[1], Length(S));
end;

procedure TBenchmarkReporter.WriteToFile(const AFileName: string);
begin
  FOutput.SaveToFile(AFileName);
end;

procedure TBenchmarkReporter.WriteToStdOut;
var
  I: Integer;
begin
  for I := 0 to FOutput.Count - 1 do
    WriteLn(FOutput[I]);
end;

function TBenchmarkReporter.HasFailures: Boolean;
var
  F, E: Integer;
  Entry: TBenchmarkEntry;
  ValidBenchmarkCount: Integer;
begin
  if FFileCount = 0 then
    Exit(True);

  ValidBenchmarkCount := 0;
  for F := 0 to FFileCount - 1 do
  begin
    if Length(FFiles[F].Entries) = 0 then
      Exit(True);

    for E := 0 to Length(FFiles[F].Entries) - 1 do
    begin
      Entry := FFiles[F].Entries[E];
      if not IsValidBenchmarkEntry(Entry, FFiles[F].DeterministicProfile) then
        Exit(True);
      Inc(ValidBenchmarkCount);
    end;
  end;

  Result := ValidBenchmarkCount = 0;
end;

end.
