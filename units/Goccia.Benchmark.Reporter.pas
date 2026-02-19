unit Goccia.Benchmark.Reporter;

{$I Goccia.inc}

interface

uses
  Classes;

type
  TBenchmarkEntry = record
    Suite: string;
    Name: string;
    OpsPerSec: Double;
    MeanMs: Double;
    Iterations: Int64;
    VariancePercentage: Double;
    Error: string;
  end;

  TBenchmarkFileResult = record
    FileName: string;
    LexTimeNanoseconds: Int64;
    ParseTimeNanoseconds: Int64;
    ExecuteTimeNanoseconds: Int64;
    Entries: array of TBenchmarkEntry;
    TotalBenchmarks: Integer;
    DurationNanoseconds: Int64;
  end;

  TBenchmarkReportFormat = (brfConsole, brfText, brfCSV, brfJSON);

  TBenchmarkReporter = class
  private
    FFiles: array of TBenchmarkFileResult;
    FFileCount: Integer;
    FOutput: TStringList;

    procedure RenderConsole;
    procedure RenderText;
    procedure RenderCSV;
    procedure RenderJSON;

    function FormatOpsPerSec(const AOps: Double): string;
    function EscapeCSV(const S: string): string;
    function EscapeJSON(const S: string): string;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddFileResult(const AResult: TBenchmarkFileResult);
    procedure Render(const AFormat: TBenchmarkReportFormat);
    procedure WriteToStream(const AStream: TStream);
    procedure WriteToFile(const AFileName: string);
    procedure WriteToStdOut;
    property Output: TStringList read FOutput;
  end;

function ParseReportFormat(const S: string): TBenchmarkReportFormat;

implementation

uses
  SysUtils,

  TimingUtils;

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
  else
    Result := brfConsole;
end;

{ TBenchmarkReporter }

constructor TBenchmarkReporter.Create;
begin
  inherited Create;
  FOutput := TStringList.Create;
  FFileCount := 0;
  SetLength(FFiles, 0);
end;

destructor TBenchmarkReporter.Destroy;
begin
  FOutput.Free;
  inherited Destroy;
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

function TBenchmarkReporter.EscapeCSV(const S: string): string;
begin
  if (Pos(',', S) > 0) or (Pos('"', S) > 0) or (Pos(#10, S) > 0) then
    Result := '"' + StringReplace(S, '"', '""', [rfReplaceAll]) + '"'
  else
    Result := S;
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
  case AFormat of
    brfConsole: RenderConsole;
    brfText:    RenderText;
    brfCSV:     RenderCSV;
    brfJSON:    RenderJSON;
  end;
end;

procedure TBenchmarkReporter.RenderConsole;
var
  F, E: Integer;
  Entry: TBenchmarkEntry;
  CurrentSuite: string;
  TotalBenchmarks: Integer;
  TotalDurationNanoseconds: Int64;
begin
  TotalBenchmarks := 0;
  TotalDurationNanoseconds := 0;

  for F := 0 to FFileCount - 1 do
  begin
    if FFileCount > 1 then
      FOutput.Add('Running benchmark: ' + FFiles[F].FileName);

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
    end;

    TotalBenchmarks := TotalBenchmarks + FFiles[F].TotalBenchmarks;
    TotalDurationNanoseconds := TotalDurationNanoseconds + FFiles[F].DurationNanoseconds;

    if F < FFileCount - 1 then
      FOutput.Add('');
  end;

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
      else if Entry.VariancePercentage > 0 then
        FOutput.Add(SysUtils.Format('%s > %s: %s ops/sec ±%.2f%% (%.4f ms/op, %d iterations)',
          [Entry.Suite, Entry.Name, FormatOpsPerSec(Entry.OpsPerSec),
           Entry.VariancePercentage, Entry.MeanMs, Entry.Iterations]))
      else
        FOutput.Add(SysUtils.Format('%s > %s: %s ops/sec (%.4f ms/op, %d iterations)',
          [Entry.Suite, Entry.Name, FormatOpsPerSec(Entry.OpsPerSec),
           Entry.MeanMs, Entry.Iterations]));
    end;

    TotalBenchmarks := TotalBenchmarks + FFiles[F].TotalBenchmarks;
    TotalDurationNanoseconds := TotalDurationNanoseconds + FFiles[F].DurationNanoseconds;
  end;

  FOutput.Add('');
  FOutput.Add(SysUtils.Format('Total: %d benchmarks in %s',
    [TotalBenchmarks, FormatDuration(TotalDurationNanoseconds)]));
end;

procedure TBenchmarkReporter.RenderCSV;
var
  F, E: Integer;
  Entry: TBenchmarkEntry;
begin
  FOutput.Add('file,suite,name,ops_per_sec,variance_percentage,mean_ms,iterations,error');

  for F := 0 to FFileCount - 1 do
  begin
    for E := 0 to Length(FFiles[F].Entries) - 1 do
    begin
      Entry := FFiles[F].Entries[E];

      if Entry.Error <> '' then
        FOutput.Add(SysUtils.Format('%s,%s,%s,,,,,%s',
          [EscapeCSV(FFiles[F].FileName), EscapeCSV(Entry.Suite),
           EscapeCSV(Entry.Name), EscapeCSV(Entry.Error)]))
      else
        FOutput.Add(SysUtils.Format('%s,%s,%s,%.6f,%.4f,%.6f,%d,',
          [EscapeCSV(FFiles[F].FileName), EscapeCSV(Entry.Suite),
           EscapeCSV(Entry.Name), Entry.OpsPerSec, Entry.VariancePercentage,
           Entry.MeanMs, Entry.Iterations]));
    end;
  end;
end;

procedure TBenchmarkReporter.RenderJSON;
var
  F, E: Integer;
  Entry: TBenchmarkEntry;
  TotalBenchmarks: Integer;
  TotalDurationNanoseconds: Int64;
  First: Boolean;
begin
  TotalBenchmarks := 0;
  TotalDurationNanoseconds := 0;

  FOutput.Add('{');
  FOutput.Add('  "files": [');

  for F := 0 to FFileCount - 1 do
  begin
    FOutput.Add('    {');
    FOutput.Add(SysUtils.Format('      "file": "%s",', [EscapeJSON(FFiles[F].FileName)]));
    FOutput.Add(SysUtils.Format('      "lexTimeNanoseconds": %d,', [FFiles[F].LexTimeNanoseconds]));
    FOutput.Add(SysUtils.Format('      "parseTimeNanoseconds": %d,', [FFiles[F].ParseTimeNanoseconds]));
    FOutput.Add(SysUtils.Format('      "executeTimeNanoseconds": %d,', [FFiles[F].ExecuteTimeNanoseconds]));
    FOutput.Add('      "benchmarks": [');

    First := True;
    for E := 0 to Length(FFiles[F].Entries) - 1 do
    begin
      Entry := FFiles[F].Entries[E];

      if not First then
        FOutput[FOutput.Count - 1] := FOutput[FOutput.Count - 1] + ',';
      First := False;

      FOutput.Add('        {');
      FOutput.Add(SysUtils.Format('          "suite": "%s",', [EscapeJSON(Entry.Suite)]));
      FOutput.Add(SysUtils.Format('          "name": "%s",', [EscapeJSON(Entry.Name)]));

      if Entry.Error <> '' then
      begin
        FOutput.Add(SysUtils.Format('          "error": "%s"', [EscapeJSON(Entry.Error)]));
      end
      else
      begin
        FOutput.Add(SysUtils.Format('          "opsPerSec": %.6f,', [Entry.OpsPerSec]));
        FOutput.Add(SysUtils.Format('          "variancePercentage": %.4f,', [Entry.VariancePercentage]));
        FOutput.Add(SysUtils.Format('          "meanMs": %.6f,', [Entry.MeanMs]));
        FOutput.Add(SysUtils.Format('          "iterations": %d', [Entry.Iterations]));
      end;

      FOutput.Add('        }');
    end;

    FOutput.Add('      ]');
    FOutput.Add('    }');

    TotalBenchmarks := TotalBenchmarks + FFiles[F].TotalBenchmarks;
    TotalDurationNanoseconds := TotalDurationNanoseconds + FFiles[F].DurationNanoseconds;

    if F < FFileCount - 1 then
      FOutput[FOutput.Count - 1] := FOutput[FOutput.Count - 1] + ',';
  end;

  FOutput.Add('  ],');
  FOutput.Add(SysUtils.Format('  "totalBenchmarks": %d,', [TotalBenchmarks]));
  FOutput.Add(SysUtils.Format('  "totalDurationNanoseconds": %d', [TotalDurationNanoseconds]));
  FOutput.Add('}');
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

end.
