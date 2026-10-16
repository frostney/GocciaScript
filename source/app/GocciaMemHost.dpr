program GocciaMemHost;
{ bench host: GocciaMemHost <keep|seq|seqgc|parse|compile> <N> <file>
  keep: create N runtimes (bytecode, console only), execute file in each, keep all alive
  seq/seqgc: create/execute/free N times sequentially, print VmRSS every N/10
  parse: parse N times, discard AST, measure RSS (phase binary search)
  compile: parse+compile N times via engine, free module, measure RSS }
{$I Goccia.inc}
uses {$IFDEF UNIX}cthreads,{$ENDIF} Classes, SysUtils,
  Goccia.Runtime, Goccia.RuntimeExtensions.Console, Goccia.GarbageCollector,
  Goccia.Engine, Goccia.Executor, Goccia.Executor.Bytecode, Goccia.SourcePipeline, Goccia.AST.Node;
function RssKB: string; var F: TStringList; I: Integer;
begin F := TStringList.Create; Result := '?';
  try F.LoadFromFile('/proc/self/status');
    for I := 0 to F.Count-1 do if Pos('VmRSS', F[I]) = 1 then Result := Trim(Copy(F[I],7,99));
  finally F.Free; end; end;
var Mode: string; N, I: Integer; Src: TStringList; Rts: array of TGocciaRuntime; R: TGocciaRuntime; FileName: string;
  Opts: TGocciaSourcePipelineOptions; PipeResult: TGocciaSourcePipelineResult;
  Eng: TGocciaEngine; CompiledMod: TGocciaCompiledModule; ProbeStr: string;
begin
  Mode := ParamStr(1); N := StrToInt(ParamStr(2)); FileName := ParamStr(3);
  if Mode = 'keep' then begin
    SetLength(Rts, N);
    for I := 0 to N-1 do begin
      Src := TStringList.Create; Src.LoadFromFile(FileName);
      Rts[I] := TGocciaRuntime.Create(FileName, Src);
      Rts[I].Install(TGocciaConsoleRuntimeExtension.Create);
      Rts[I].Execute;
      Src.Free;
    end;
    WriteLn(StdErr, 'keep N=', N, ' VmRSS=', RssKB);
  end else if Mode = 'parse' then begin
    for I := 1 to N do begin
      Src := TStringList.Create; Src.LoadFromFile(FileName);
      Opts := Default(TGocciaSourcePipelineOptions);
      Opts.SourceType := stScript;
      PipeResult := TGocciaSourcePipeline.Parse(Src, FileName, Opts);
      PipeResult.ProgramNode.Free; PipeResult.Free; Src.Free;
      TGarbageCollector.Instance.Collect;
      if (N >= 10) and (I mod (N div 10) = 0) then WriteLn(StdErr, 'parse i=', I, ' VmRSS=', RssKB);
    end;
  end else if Mode = 'compile' then begin
    Src := TStringList.Create; Src.LoadFromFile(FileName);
    Eng := TGocciaEngine.Create(FileName, Src, TGocciaBytecodeExecutor.Create);
    Src.Free;
    for I := 1 to N do begin
      Src := TStringList.Create; Src.LoadFromFile(FileName);
      Opts := Default(TGocciaSourcePipelineOptions);
      Opts.SourceType := stScript;
      PipeResult := TGocciaSourcePipeline.Parse(Src, FileName, Opts);
      CompiledMod := Eng.CompileModule(PipeResult.ProgramNode);
      CompiledMod.Free; PipeResult.ProgramNode.Free; PipeResult.Free; Src.Free;
      TGarbageCollector.Instance.Collect;
      if (N >= 10) and (I mod (N div 10) = 0) then WriteLn(StdErr, 'compile i=', I, ' VmRSS=', RssKB);
    end;
    Eng.Free;
  end else if Mode = 'seqnocollect' then begin
    for I := 1 to N do begin
      Src := TStringList.Create; Src.LoadFromFile(FileName);
      R := TGocciaRuntime.Create(FileName, Src);
      R.Install(TGocciaConsoleRuntimeExtension.Create);
      R.Execute; Src.Free; R.Free;
      if (N >= 10) and (I mod (N div 10) = 0) then WriteLn(StdErr, 'seq i=', I, ' VmRSS=', RssKB);
    end;
  end else if Mode = 'parseonly' then begin
    for I := 1 to N do begin
      Src := TStringList.Create; Src.LoadFromFile(FileName);
      R := TGocciaRuntime.Create(FileName, Src);
      Src.Free; R.Free; TGarbageCollector.Instance.Collect;
      if (N >= 10) and (I mod (N div 10) = 0) then WriteLn(StdErr, 'seq i=', I, ' VmRSS=', RssKB);
    end;
  end else if Mode = 'compileonly' then begin
    for I := 1 to N do begin
      Src := TStringList.Create; Src.LoadFromFile(FileName);
      R := TGocciaRuntime.Create(FileName, Src);
      R.Install(TGocciaConsoleRuntimeExtension.Create);
      Src.Free; R.Free; TGarbageCollector.Instance.Collect;
      if (N >= 10) and (I mod (N div 10) = 0) then WriteLn(StdErr, 'seq i=', I, ' VmRSS=', RssKB);
    end;
  end else if Mode = 'seqgcdump' then begin
    for I := 1 to N do begin
      Src := TStringList.Create; Src.LoadFromFile(FileName);
      R := TGocciaRuntime.Create(FileName, Src);
      R.Install(TGocciaConsoleRuntimeExtension.Create);
      R.Execute; Src.Free; R.Free;
      WriteLn(StdErr, 'After Free i=', I);
      TGarbageCollector.Instance.DumpLargeStringRoots;
      TGarbageCollector.Instance.Collect;
      WriteLn(StdErr, 'After Collect i=', I, ' live_large_strings=',
        TGarbageCollector.Instance.CountLiveStringLiterals(100000));
      TGarbageCollector.Instance.DumpLargeStringRoots;
      if (N >= 10) and (I mod (N div 10) = 0) then WriteLn(StdErr, 'seq i=', I, ' VmRSS=', RssKB);
    end;
  end else if Mode = 'assert' then begin
    for I := 1 to N do begin
      Src := TStringList.Create; Src.LoadFromFile(FileName);
      R := TGocciaRuntime.Create(FileName, Src);
      R.Install(TGocciaConsoleRuntimeExtension.Create);
      WriteLn(StdErr, 'Before Execute: RetainedModules=', R.Engine.RetainedModuleCount);
      R.Execute;
      WriteLn(StdErr, 'After Execute: RetainedModules=', R.Engine.RetainedModuleCount);
      Src.Free; R.Free; TGarbageCollector.Instance.Collect;
      if TGarbageCollector.Instance.CountLiveStringLiterals(100000) > 0 then
      begin
        WriteLn(StdErr, 'FAIL iter ', I, ': ', TGarbageCollector.Instance.CountLiveStringLiterals(100000),
          ' large StringLiteralValue objects still live after Free+Collect');
        TGarbageCollector.Instance.DumpLargeStringRoots;
        Halt(1);
      end;
      if (N >= 10) and (I mod (N div 10) = 0) then WriteLn(StdErr, 'assert i=', I, ' VmRSS=', RssKB, ' OK');
    end;
  end else if Mode = 'probe' then begin
    for I := 1 to N do begin
      Src := TStringList.Create; Src.LoadFromFile(FileName);
      R := TGocciaRuntime.Create(FileName, Src);
      R.Install(TGocciaConsoleRuntimeExtension.Create);
      R.Execute; Src.Free; R.Free; TGarbageCollector.Instance.Collect;
      SetLength(ProbeStr, 110000); // Allocate large block to test if heap reuses freed space
      FillChar(ProbeStr[1], 110000, Byte('z'));
      ProbeStr := '';
      if (N >= 10) and (I mod (N div 10) = 0) then WriteLn(StdErr, 'probe i=', I, ' VmRSS=', RssKB);
    end;
  end else begin
    for I := 1 to N do begin
      Src := TStringList.Create; Src.LoadFromFile(FileName);
      R := TGocciaRuntime.Create(FileName, Src);
      R.Install(TGocciaConsoleRuntimeExtension.Create);
      R.Execute; Src.Free; R.Free; if Mode = 'seqgc' then TGarbageCollector.Instance.Collect;
      if (N >= 10) and (I mod (N div 10) = 0) then WriteLn(StdErr, 'seq i=', I, ' VmRSS=', RssKB);
    end;
  end;
end.
