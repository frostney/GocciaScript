program BenchmarkRunner;

{$I Goccia.inc}

uses
  Classes, SysUtils, Generics.Collections, Goccia.Values.Primitives,
  Goccia.Values.ObjectValue, Goccia.Values.ArrayValue, Goccia.Engine,
  Goccia.Error, Goccia.Benchmark.Reporter,
  FileUtils in 'units/FileUtils.pas';

function CollectBenchmarkFile(const FileName: string;
  Reporter: TBenchmarkReporter): TGocciaObjectValue;
var
  Source: TStringList;
  BenchGlobals: TGocciaGlobalBuiltins;
  TimingResult: TGocciaTimingResult;
  ScriptResult: TGocciaObjectValue;
  ResultsArray: TGocciaArrayValue;
  SingleResult: TGocciaObjectValue;
  FileResult: TBenchmarkFileResult;
  Entry: TBenchmarkEntry;
  ErrorMsg: string;
  I, EntryCount: Integer;
begin
  BenchGlobals := TGocciaEngine.DefaultGlobals + [ggBenchmark];

  Source := TStringList.Create;
  try
    Source.LoadFromFile(FileName);
    Source.Add('runBenchmarks();');

    try
      TimingResult := TGocciaEngine.RunScriptWithTiming(Source, FileName, BenchGlobals);

      FileResult.FileName := FileName;
      FileResult.LexTimeMs := TimingResult.LexTimeMs;
      FileResult.ParseTimeMs := TimingResult.ParseTimeMs;
      FileResult.ExecuteTimeMs := TimingResult.ExecuteTimeMs;

      if TimingResult.Result is TGocciaObjectValue then
      begin
        ScriptResult := TGocciaObjectValue(TimingResult.Result);

        FileResult.TotalBenchmarks := Round(ScriptResult.GetProperty('totalBenchmarks').ToNumberLiteral.Value);
        FileResult.DurationMs := ScriptResult.GetProperty('duration').ToNumberLiteral.Value;

        if ScriptResult.GetProperty('results') is TGocciaArrayValue then
        begin
          ResultsArray := TGocciaArrayValue(ScriptResult.GetProperty('results'));
          EntryCount := ResultsArray.GetLength;
          SetLength(FileResult.Entries, EntryCount);

          for I := 0 to EntryCount - 1 do
          begin
            SingleResult := TGocciaObjectValue(ResultsArray.GetElement(I));

            Entry.Suite := SingleResult.GetProperty('suite').ToStringLiteral.Value;
            Entry.Name := SingleResult.GetProperty('name').ToStringLiteral.Value;

            ErrorMsg := SingleResult.GetProperty('error').ToStringLiteral.Value;
            if ErrorMsg <> 'undefined' then
            begin
              Entry.Error := ErrorMsg;
              Entry.OpsPerSec := 0;
              Entry.MeanMs := 0;
              Entry.Iterations := 0;
              Entry.VariancePercentage := 0;
            end
            else
            begin
              Entry.Error := '';
              Entry.OpsPerSec := SingleResult.GetProperty('opsPerSec').ToNumberLiteral.Value;
              Entry.MeanMs := SingleResult.GetProperty('meanMs').ToNumberLiteral.Value;
              Entry.Iterations := Round(SingleResult.GetProperty('iterations').ToNumberLiteral.Value);
              Entry.VariancePercentage := SingleResult.GetProperty('variancePercentage').ToNumberLiteral.Value;
            end;

            FileResult.Entries[I] := Entry;
          end;
        end;

        Reporter.AddFileResult(FileResult);
        Result := ScriptResult;
      end
      else
      begin
        FileResult.TotalBenchmarks := 0;
        FileResult.DurationMs := 0;
        SetLength(FileResult.Entries, 0);
        Reporter.AddFileResult(FileResult);

        Result := TGocciaObjectValue.Create;
        Result.AssignProperty('totalBenchmarks', TGocciaNumberLiteralValue.ZeroValue);
        Result.AssignProperty('duration', TGocciaNumberLiteralValue.ZeroValue);
      end;
    except
      on E: Exception do
      begin
        FileResult.FileName := FileName;
        FileResult.LexTimeMs := 0;
        FileResult.ParseTimeMs := 0;
        FileResult.ExecuteTimeMs := 0;
        FileResult.TotalBenchmarks := 0;
        FileResult.DurationMs := 0;
        SetLength(FileResult.Entries, 1);
        FileResult.Entries[0].Suite := '';
        FileResult.Entries[0].Name := '(fatal)';
        FileResult.Entries[0].Error := E.Message;
        Reporter.AddFileResult(FileResult);

        Result := TGocciaObjectValue.Create;
        Result.AssignProperty('totalBenchmarks', TGocciaNumberLiteralValue.ZeroValue);
        Result.AssignProperty('duration', TGocciaNumberLiteralValue.ZeroValue);
      end;
    end;
  finally
    Source.Free;
  end;
end;

procedure RunBenchmarks(const Path: string; Format: TBenchmarkReportFormat;
  const OutputFile: string);
var
  Files: TStringList;
  I: Integer;
  Reporter: TBenchmarkReporter;
begin
  Reporter := TBenchmarkReporter.Create;
  try
    if DirectoryExists(Path) then
    begin
      Files := FindAllFiles(Path, '.js');
      try
        for I := 0 to Files.Count - 1 do
          CollectBenchmarkFile(Files[I], Reporter);
      finally
        Files.Free;
      end;
    end
    else if FileExists(Path) then
      CollectBenchmarkFile(Path, Reporter)
    else
    begin
      WriteLn('Error: Path not found: ', Path);
      ExitCode := 1;
      Exit;
    end;

    Reporter.Render(Format);

    if OutputFile <> '' then
      Reporter.WriteToFile(OutputFile)
    else
      Reporter.WriteToStdOut;
  finally
    Reporter.Free;
  end;
end;

var
  BenchPath, OutputFile, FormatStr: string;
  Format: TBenchmarkReportFormat;
  I: Integer;

begin
  BenchPath := '';
  OutputFile := '';
  FormatStr := 'console';

  for I := 1 to ParamCount do
  begin
    if Copy(ParamStr(I), 1, 9) = '--format=' then
      FormatStr := Copy(ParamStr(I), 10, MaxInt)
    else if Copy(ParamStr(I), 1, 9) = '--output=' then
      OutputFile := Copy(ParamStr(I), 10, MaxInt)
    else if Copy(ParamStr(I), 1, 2) <> '--' then
      BenchPath := ParamStr(I);
  end;

  if BenchPath = '' then
  begin
    WriteLn('Usage: BenchmarkRunner <path> [--format=console|text|csv|json] [--output=file]');
    ExitCode := 1;
  end
  else
  begin
    Format := ParseReportFormat(FormatStr);
    RunBenchmarks(BenchPath, Format, OutputFile);
  end;
end.
