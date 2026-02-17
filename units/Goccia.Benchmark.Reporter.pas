unit Goccia.Benchmark.Reporter;

{$I Goccia.inc}

interface

uses
  Classes, SysUtils, Generics.Collections;

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
    LexTimeMs: Int64;
    ParseTimeMs: Int64;
    ExecuteTimeMs: Int64;
    Entries: array of TBenchmarkEntry;
    TotalBenchmarks: Integer;
    DurationMs: Double;
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

    function FormatOpsPerSec(Ops: Double): string;
    function EscapeCSV(const S: string): string;
    function EscapeJSON(const S: string): string;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddFileResult(const AResult: TBenchmarkFileResult);
    procedure Render(Format: TBenchmarkReportFormat);
    procedure WriteToStream(Stream: TStream);
    procedure WriteToFile(const FileName: string);
    procedure WriteToStdOut;
    property Output: TStringList read FOutput;
  end;

function ParseReportFormat(const S: string): TBenchmarkReportFormat;

implementation

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

function TBenchmarkReporter.FormatOpsPerSec(Ops: Double): string;
var
  IntOps: Int64;
  S: string;
  I, Len: Integer;
begin
  IntOps := Round(Ops);
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

procedure TBenchmarkReporter.Render(Format: TBenchmarkReportFormat);
begin
  FOutput.Clear;
  case Format of
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
  TotalDuration: Double;
begin
  TotalBenchmarks := 0;
  TotalDuration := 0;

  for F := 0 to FFileCount - 1 do
  begin
    if FFileCount > 1 then
      FOutput.Add('Running benchmark: ' + FFiles[F].FileName);

    FOutput.Add(SysUtils.Format('  Lex: %dms | Parse: %dms | Execute: %dms',
      [FFiles[F].LexTimeMs, FFiles[F].ParseTimeMs, FFiles[F].ExecuteTimeMs]));
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
    TotalDuration := TotalDuration + FFiles[F].DurationMs;

    if F < FFileCount - 1 then
      FOutput.Add('');
  end;

  FOutput.Add('');
  FOutput.Add('Benchmark Summary');
  FOutput.Add(SysUtils.Format('  Total benchmarks: %d', [TotalBenchmarks]));
  if TotalDuration >= 1000 then
    FOutput.Add(SysUtils.Format('  Total duration: %.1fs', [TotalDuration / 1000]))
  else
    FOutput.Add(SysUtils.Format('  Total duration: %.0fms', [TotalDuration]));
end;

procedure TBenchmarkReporter.RenderText;
var
  F, E: Integer;
  Entry: TBenchmarkEntry;
  TotalBenchmarks: Integer;
  TotalDuration: Double;
begin
  TotalBenchmarks := 0;
  TotalDuration := 0;

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
    TotalDuration := TotalDuration + FFiles[F].DurationMs;
  end;

  FOutput.Add('');
  if TotalDuration >= 1000 then
    FOutput.Add(SysUtils.Format('Total: %d benchmarks in %.1fs',
      [TotalBenchmarks, TotalDuration / 1000]))
  else
    FOutput.Add(SysUtils.Format('Total: %d benchmarks in %.0fms',
      [TotalBenchmarks, TotalDuration]));
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
  TotalDuration: Double;
  First: Boolean;
begin
  TotalBenchmarks := 0;
  TotalDuration := 0;

  FOutput.Add('{');
  FOutput.Add('  "files": [');

  for F := 0 to FFileCount - 1 do
  begin
    FOutput.Add('    {');
    FOutput.Add(SysUtils.Format('      "file": "%s",', [EscapeJSON(FFiles[F].FileName)]));
    FOutput.Add(SysUtils.Format('      "lexTimeMs": %d,', [FFiles[F].LexTimeMs]));
    FOutput.Add(SysUtils.Format('      "parseTimeMs": %d,', [FFiles[F].ParseTimeMs]));
    FOutput.Add(SysUtils.Format('      "executeTimeMs": %d,', [FFiles[F].ExecuteTimeMs]));
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
    TotalDuration := TotalDuration + FFiles[F].DurationMs;

    if F < FFileCount - 1 then
      FOutput[FOutput.Count - 1] := FOutput[FOutput.Count - 1] + ',';
  end;

  FOutput.Add('  ],');
  FOutput.Add(SysUtils.Format('  "totalBenchmarks": %d,', [TotalBenchmarks]));
  FOutput.Add(SysUtils.Format('  "totalDurationMs": %.1f', [TotalDuration]));
  FOutput.Add('}');
end;

procedure TBenchmarkReporter.WriteToStream(Stream: TStream);
var
  S: string;
begin
  S := FOutput.Text;
  if Length(S) > 0 then
    Stream.WriteBuffer(S[1], Length(S));
end;

procedure TBenchmarkReporter.WriteToFile(const FileName: string);
begin
  FOutput.SaveToFile(FileName);
end;

procedure TBenchmarkReporter.WriteToStdOut;
var
  I: Integer;
begin
  for I := 0 to FOutput.Count - 1 do
    WriteLn(FOutput[I]);
end;

end.
