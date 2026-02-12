program BenchmarkRunner;

{$I Goccia.inc}

uses
  Classes, SysUtils, Generics.Collections, Goccia.Values.Primitives,
  Goccia.Values.ObjectValue, Goccia.Values.ArrayValue, Goccia.Engine,
  Goccia.Error, FileUtils in 'units/FileUtils.pas';

function FormatOpsPerSec(Ops: Double): string;
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

function RunBenchmarkFile(const FileName: string): TGocciaObjectValue;
var
  Source: TStringList;
  BenchGlobals: TGocciaGlobalBuiltins;
  TimingResult: TGocciaTimingResult;
  ScriptResult: TGocciaObjectValue;
  ResultsArray: TGocciaArrayValue;
  SingleResult: TGocciaObjectValue;
  CurrentSuite, BenchName, ErrorMsg: string;
  OpsPerSec, MeanMs: Double;
  Iterations: Int64;
  I: Integer;
begin
  BenchGlobals := TGocciaEngine.DefaultGlobals + [ggBenchmark];

  Source := TStringList.Create;
  try
    Source.LoadFromFile(FileName);
    Source.Add('runBenchmarks();');

    try
      TimingResult := TGocciaEngine.RunScriptWithTiming(Source, FileName, BenchGlobals);

      WriteLn(Format('  Lex: %dms | Parse: %dms | Execute: %dms',
        [TimingResult.LexTimeMs, TimingResult.ParseTimeMs, TimingResult.ExecuteTimeMs]));
      WriteLn('');

      if TimingResult.Result is TGocciaObjectValue then
      begin
        ScriptResult := TGocciaObjectValue(TimingResult.Result);

        if ScriptResult.GetProperty('results') is TGocciaArrayValue then
        begin
          ResultsArray := TGocciaArrayValue(ScriptResult.GetProperty('results'));
          CurrentSuite := '';

          for I := 0 to ResultsArray.GetLength - 1 do
          begin
            SingleResult := TGocciaObjectValue(ResultsArray.GetElement(I));

            // Print suite header if it changed
            if SingleResult.GetProperty('suite').ToStringLiteral.Value <> CurrentSuite then
            begin
              CurrentSuite := SingleResult.GetProperty('suite').ToStringLiteral.Value;
              if CurrentSuite <> '' then
                WriteLn('  ', CurrentSuite);
            end;

            BenchName := SingleResult.GetProperty('name').ToStringLiteral.Value;

            // Check for error
            ErrorMsg := SingleResult.GetProperty('error').ToStringLiteral.Value;
            if ErrorMsg <> 'undefined' then
            begin
              WriteLn(Format('    %-30s  ERROR: %s', [BenchName, ErrorMsg]));
            end
            else
            begin
              OpsPerSec := SingleResult.GetProperty('opsPerSec').ToNumberLiteral.Value;
              MeanMs := SingleResult.GetProperty('meanMs').ToNumberLiteral.Value;
              Iterations := Round(SingleResult.GetProperty('iterations').ToNumberLiteral.Value);

              WriteLn(Format('    %-30s  %12s ops/sec  %10.4f ms/op  (%d iterations)',
                [BenchName, FormatOpsPerSec(OpsPerSec), MeanMs, Iterations]));
            end;
          end;
        end;

        Result := ScriptResult;
      end
      else
      begin
        Result := TGocciaObjectValue.Create;
        Result.AssignProperty('totalBenchmarks', TGocciaNumberLiteralValue.ZeroValue);
        Result.AssignProperty('duration', TGocciaNumberLiteralValue.ZeroValue);
      end;
    except
      on E: Exception do
      begin
        WriteLn('  Fatal error: ', E.Message);
        Result := TGocciaObjectValue.Create;
        Result.AssignProperty('totalBenchmarks', TGocciaNumberLiteralValue.ZeroValue);
        Result.AssignProperty('duration', TGocciaNumberLiteralValue.ZeroValue);
      end;
    end;
  finally
    Source.Free;
  end;
end;

procedure RunBenchmarks(const Path: string);
var
  Files: TStringList;
  I: Integer;
  TotalBenchmarks: Integer;
  TotalDuration: Double;
  ScriptResult: TGocciaObjectValue;
begin
  TotalBenchmarks := 0;
  TotalDuration := 0;

  if DirectoryExists(Path) then
  begin
    Files := FindAllFiles(Path, '.js');
    try
      for I := 0 to Files.Count - 1 do
      begin
        WriteLn('Running benchmark: ', Files[I]);
        ScriptResult := RunBenchmarkFile(Files[I]);
        TotalBenchmarks := TotalBenchmarks + Round(ScriptResult.GetProperty('totalBenchmarks').ToNumberLiteral.Value);
        TotalDuration := TotalDuration + ScriptResult.GetProperty('duration').ToNumberLiteral.Value;
        WriteLn('');
      end;
    finally
      Files.Free;
    end;
  end
  else if FileExists(Path) then
  begin
    WriteLn('Running benchmark: ', Path);
    ScriptResult := RunBenchmarkFile(Path);
    TotalBenchmarks := TotalBenchmarks + Round(ScriptResult.GetProperty('totalBenchmarks').ToNumberLiteral.Value);
    TotalDuration := TotalDuration + ScriptResult.GetProperty('duration').ToNumberLiteral.Value;
    WriteLn('');
  end
  else
  begin
    WriteLn('Error: Path not found: ', Path);
    ExitCode := 1;
    Exit;
  end;

  WriteLn('Benchmark Summary');
  WriteLn(Format('  Total benchmarks: %d', [TotalBenchmarks]));
  if TotalDuration >= 1000 then
    WriteLn(Format('  Total duration: %.1fs', [TotalDuration / 1000]))
  else
    WriteLn(Format('  Total duration: %.0fms', [TotalDuration]));
end;

begin
  if ParamCount < 1 then
  begin
    WriteLn('Usage: BenchmarkRunner <filename.js>');
    WriteLn('or');
    WriteLn('Usage: BenchmarkRunner <directory>');
    ExitCode := 1;
  end
  else
    RunBenchmarks(ParamStr(1));
end.
