program BenchmarkRunner;

{$I Goccia.inc}

uses
  Classes,
  Generics.Collections,
  SysUtils,

  Goccia.Benchmark.Reporter,
  Goccia.Builtins.Benchmark,
  Goccia.Engine,
  Goccia.Error,
  Goccia.Values.ArrayValue,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives,

  FileUtils in 'units/FileUtils.pas';

type
  TBenchmarkProgress = class
    class procedure OnProgress(const ASuiteName, ABenchName: string; const AIndex, ATotal: Integer);
  end;

class procedure TBenchmarkProgress.OnProgress(const ASuiteName, ABenchName: string; const AIndex, ATotal: Integer);
begin
  if ASuiteName <> '' then
    WriteLn(SysUtils.Format('  [%d/%d] %s > %s', [AIndex, ATotal, ASuiteName, ABenchName]))
  else
    WriteLn(SysUtils.Format('  [%d/%d] %s', [AIndex, ATotal, ABenchName]));
end;

var
  GShowProgress: Boolean = True;

function CollectBenchmarkFile(const AFileName: string;
  const AReporter: TBenchmarkReporter): TGocciaObjectValue;
var
  Source: TStringList;
  Engine: TGocciaEngine;
  BenchGlobals: TGocciaGlobalBuiltins;
  EngineResult: TGocciaScriptResult;
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
    Source.LoadFromFile(AFileName);
    Source.Add('runBenchmarks();');

    try
      Engine := TGocciaEngine.Create(AFileName, Source, BenchGlobals);
      try
        if GShowProgress and Assigned(Engine.BuiltinBenchmark) then
          Engine.BuiltinBenchmark.OnProgress := TBenchmarkProgress.OnProgress;

        EngineResult := Engine.Execute;
      finally
        Engine.Free;
      end;

      FileResult.FileName := AFileName;
      FileResult.LexTimeNanoseconds := EngineResult.LexTimeNanoseconds;
      FileResult.ParseTimeNanoseconds := EngineResult.ParseTimeNanoseconds;
      FileResult.ExecuteTimeNanoseconds := EngineResult.ExecuteTimeNanoseconds;

      if EngineResult.Result is TGocciaObjectValue then
      begin
        ScriptResult := TGocciaObjectValue(EngineResult.Result);

        FileResult.TotalBenchmarks := Round(ScriptResult.GetProperty('totalBenchmarks').ToNumberLiteral.Value);
        FileResult.DurationNanoseconds := Round(ScriptResult.GetProperty('durationNanoseconds').ToNumberLiteral.Value);

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

        AReporter.AddFileResult(FileResult);
        Result := ScriptResult;
      end
      else
      begin
        FileResult.TotalBenchmarks := 0;
        FileResult.DurationNanoseconds := 0;
        SetLength(FileResult.Entries, 0);
        AReporter.AddFileResult(FileResult);

        Result := TGocciaObjectValue.Create;
        Result.AssignProperty('totalBenchmarks', TGocciaNumberLiteralValue.ZeroValue);
        Result.AssignProperty('duration', TGocciaNumberLiteralValue.ZeroValue);
      end;
    except
      on E: Exception do
      begin
        FileResult.FileName := AFileName;
        FileResult.LexTimeNanoseconds := 0;
        FileResult.ParseTimeNanoseconds := 0;
        FileResult.ExecuteTimeNanoseconds := 0;
        FileResult.TotalBenchmarks := 0;
        FileResult.DurationNanoseconds := 0;
        SetLength(FileResult.Entries, 1);
        FileResult.Entries[0].Suite := '';
        FileResult.Entries[0].Name := '(fatal)';
        FileResult.Entries[0].Error := E.Message;
        AReporter.AddFileResult(FileResult);

        Result := TGocciaObjectValue.Create;
        Result.AssignProperty('totalBenchmarks', TGocciaNumberLiteralValue.ZeroValue);
        Result.AssignProperty('duration', TGocciaNumberLiteralValue.ZeroValue);
      end;
    end;
  finally
    Source.Free;
  end;
end;

type
  TReportSpec = record
    Format: TBenchmarkReportFormat;
    OutputFile: string;
  end;

procedure RunBenchmarks(const APath: string; const AReports: array of TReportSpec);
var
  Files: TStringList;
  I, J: Integer;
  Reporter: TBenchmarkReporter;
begin
  Reporter := TBenchmarkReporter.Create;
  try
    if DirectoryExists(APath) then
    begin
      Files := FindAllFiles(APath);
      try
        for I := 0 to Files.Count - 1 do
        begin
          if GShowProgress then
            WriteLn(SysUtils.Format('[%d/%d] %s', [I + 1, Files.Count, ExtractFileName(Files[I])]));
          CollectBenchmarkFile(Files[I], Reporter);
        end;
        if GShowProgress and (Files.Count > 0) then
          WriteLn;
      finally
        Files.Free;
      end;
    end
    else if FileExists(APath) then
    begin
      if GShowProgress then
        WriteLn('[1/1] ', ExtractFileName(APath));
      CollectBenchmarkFile(APath, Reporter);
      if GShowProgress then
        WriteLn;
    end
    else
    begin
      WriteLn('Error: Path not found: ', APath);
      ExitCode := 1;
      Exit;
    end;

    for J := 0 to Length(AReports) - 1 do
    begin
      Reporter.Render(AReports[J].Format);
      if AReports[J].OutputFile <> '' then
        Reporter.WriteToFile(AReports[J].OutputFile)
      else
        Reporter.WriteToStdOut;
    end;
  finally
    Reporter.Free;
  end;
end;

var
  BenchPath: string;
  Reports: array of TReportSpec;
  ReportCount: Integer;
  I: Integer;
  OutputStr: string;

begin
  BenchPath := '';
  ReportCount := 0;
  GShowProgress := True;
  SetLength(Reports, 0);

  for I := 1 to ParamCount do
  begin
    if Copy(ParamStr(I), 1, 9) = '--format=' then
    begin
      Inc(ReportCount);
      SetLength(Reports, ReportCount);
      Reports[ReportCount - 1].Format := ParseReportFormat(Copy(ParamStr(I), 10, MaxInt));
      Reports[ReportCount - 1].OutputFile := '';
    end
    else if Copy(ParamStr(I), 1, 9) = '--output=' then
    begin
      OutputStr := Copy(ParamStr(I), 10, MaxInt);
      if ReportCount > 0 then
        Reports[ReportCount - 1].OutputFile := OutputStr
      else
      begin
        Inc(ReportCount);
        SetLength(Reports, ReportCount);
        Reports[ReportCount - 1].Format := brfConsole;
        Reports[ReportCount - 1].OutputFile := OutputStr;
      end;
    end
    else if ParamStr(I) = '--no-progress' then
      GShowProgress := False
    else if Copy(ParamStr(I), 1, 2) <> '--' then
      BenchPath := ParamStr(I);
  end;

  if ReportCount = 0 then
  begin
    SetLength(Reports, 1);
    Reports[0].Format := brfConsole;
    Reports[0].OutputFile := '';
  end;

  if BenchPath = '' then
  begin
    WriteLn('Usage: BenchmarkRunner <path> [--format=console|text|csv|json [--output=file]] ... [--no-progress]');
    ExitCode := 1;
  end
  else
    RunBenchmarks(BenchPath, Reports);
end.
