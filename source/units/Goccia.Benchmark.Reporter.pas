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

    function GetFileResult(AIndex: Integer): TBenchmarkFileResult;
    function FormatOpsPerSec(const AOps: Double): string;
    function EscapeCSVField(const S: string): string;
    function EscapeJSON(const S: string): string;
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

  Goccia.CSV;

function IsPositiveFinite(const AValue: Double): Boolean;
begin
  Result := (AValue > 0) and not Math.IsNan(AValue) and
    not Math.IsInfinite(AValue);
end;

function IsValidBenchmarkEntry(const AEntry: TBenchmarkEntry): Boolean;
begin
  Result := (AEntry.Error = '') and IsPositiveFinite(AEntry.OpsPerSec) and
    IsPositiveFinite(AEntry.MeanMs) and (AEntry.Iterations > 0);
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

function TBenchmarkReporter.EscapeJSON(const S: string): string;
begin
  Result := StringReplace(S, '\', '\\', [rfReplaceAll]);
  Result := StringReplace(Result, '"', '\"', [rfReplaceAll]);
  Result := StringReplace(Result, #10, '\n', [rfReplaceAll]);
  Result := StringReplace(Result, #13, '\r', [rfReplaceAll]);
  Result := StringReplace(Result, #9, '\t', [rfReplaceAll]);
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

        FOutput.Add(Line);
      end;
    end;

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
  F, E: Integer;
  Entry: TBenchmarkEntry;
begin
  FOutput.Add('file,suite,name,ops_per_sec,variance_percentage,mean_ms,iterations,setup_ms,teardown_ms,min_ops_per_sec,max_ops_per_sec,error');

  for F := 0 to FFileCount - 1 do
  begin
    for E := 0 to Length(FFiles[F].Entries) - 1 do
    begin
      Entry := FFiles[F].Entries[E];

      if Entry.Error <> '' then
        FOutput.Add(SysUtils.Format('%s,%s,%s,,,,,,,,,%s',
          [EscapeCSVField(FFiles[F].FileName), EscapeCSVField(Entry.Suite),
           EscapeCSVField(Entry.Name), EscapeCSVField(Entry.Error)]))
      else
        FOutput.Add(SysUtils.Format('%s,%s,%s,%.6f,%.4f,%.6f,%d,%.6f,%.6f,%.6f,%.6f,',
          [EscapeCSVField(FFiles[F].FileName), EscapeCSVField(Entry.Suite),
           EscapeCSVField(Entry.Name), Entry.OpsPerSec, Entry.VariancePercentage,
           Entry.MeanMs, Entry.Iterations, Entry.SetupMs, Entry.TeardownMs,
           Entry.MinOpsPerSec, Entry.MaxOpsPerSec]));
    end;
  end;
end;

procedure TBenchmarkReporter.RenderJSON;
var
  F, E: Integer;
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
  FileErrorJSON, FileErrorMessage: string;
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

      if IsValidBenchmarkEntry(Entry) then
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
          '{"suite":"%s","name":"%s","error":"%s"}',
          [EscapeJSON(Entry.Suite), EscapeJSON(Entry.Name),
           EscapeJSON(Entry.Error)])
      end
      else
        BenchmarkJSON := SysUtils.Format(
          '{"suite":"%s","name":"%s","opsPerSec":%.6f,' +
          '"variancePercentage":%.4f,"meanMs":%.6f,"iterations":%d,' +
          '"setupMs":%.6f,"teardownMs":%.6f,"minOpsPerSec":%.6f,' +
          '"maxOpsPerSec":%.6f}',
          [EscapeJSON(Entry.Suite), EscapeJSON(Entry.Name), Entry.OpsPerSec,
           Entry.VariancePercentage, Entry.MeanMs, Entry.Iterations,
           Entry.SetupMs, Entry.TeardownMs, Entry.MinOpsPerSec,
           Entry.MaxOpsPerSec], JSONFormatSettings);

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
      if not IsValidBenchmarkEntry(Entry) then
        Exit(True);
      Inc(ValidBenchmarkCount);
    end;
  end;

  Result := ValidBenchmarkCount = 0;
end;

end.
