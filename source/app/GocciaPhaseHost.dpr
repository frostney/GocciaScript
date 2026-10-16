program GocciaPhaseHost;
{ Phase binary search: GocciaPhaseHost <parse|compile|execute> <N> <file>
  parse: parse N times, discard AST, measure RSS
  compile: parse+compile N times via engine, free compiled module, measure RSS  
  execute: full execute+free N times, measure RSS }
{$I Goccia.inc}
uses {$IFDEF UNIX}cthreads,{$ENDIF} Classes, SysUtils,
  Goccia.Engine, Goccia.Executor.Bytecode, Goccia.Runtime, 
  Goccia.RuntimeExtensions.Console, Goccia.GarbageCollector,
  Goccia.SourcePipeline, Goccia.AST.Node;
function RssKB: string; var F: TStringList; I: Integer;
begin F := TStringList.Create; Result := '?';
  try F.LoadFromFile('/proc/self/status');
    for I := 0 to F.Count-1 do if Pos('VmRSS', F[I]) = 1 then Result := Trim(Copy(F[I],7,99));
  finally F.Free; end; end;
var Phase: string; N, I: Integer; Src: TStringList; FileName: string;
  Opts: TGocciaSourcePipelineOptions; PipeResult: TGocciaSourcePipelineResult;
  Eng: TGocciaEngine; Mod: TGocciaCompiledModule; R: TGocciaRuntime;
begin
  Phase := ParamStr(1); N := StrToInt(ParamStr(2)); FileName := ParamStr(3);
  if Phase = 'parse' then begin
    for I := 1 to N do begin
      Src := TStringList.Create; Src.LoadFromFile(FileName);
      Opts := Default(TGocciaSourcePipelineOptions);
      Opts.SourceType := stScript; Opts.Strict := True;
      PipeResult := TGocciaSourcePipeline.Parse(Src, FileName, Opts);
      PipeResult.ProgramNode.Free; PipeResult.Free; Src.Free;
      TGarbageCollector.Instance.Collect;
      if (N >= 10) and (I mod (N div 10) = 0) then WriteLn(StdErr, 'parse i=', I, ' VmRSS=', RssKB);
    end;
  end else if Phase = 'compile' then begin
    Eng := TGocciaEngine.Create(FileName, TGocciaBytecodeExecutor.Create);
    for I := 1 to N do begin
      Src := TStringList.Create; Src.LoadFromFile(FileName);
      Opts := Default(TGocciaSourcePipelineOptions);
      Opts.SourceType := stScript; Opts.Strict := True;
      PipeResult := TGocciaSourcePipeline.Parse(Src, FileName, Opts);
      Mod := Eng.CompileModule(PipeResult.ProgramNode);
      Mod.Free; PipeResult.ProgramNode.Free; PipeResult.Free; Src.Free;
      TGarbageCollector.Instance.Collect;
      if (N >= 10) and (I mod (N div 10) = 0) then WriteLn(StdErr, 'compile i=', I, ' VmRSS=', RssKB);
    end;
    Eng.Free;
  end else begin
    for I := 1 to N do begin
      Src := TStringList.Create; Src.LoadFromFile(FileName);
      R := TGocciaRuntime.Create(FileName, Src);
      R.Install(TGocciaConsoleRuntimeExtension.Create);
      R.Execute; Src.Free; R.Free;
      TGarbageCollector.Instance.Collect;
      if (N >= 10) and (I mod (N div 10) = 0) then WriteLn(StdErr, 'execute i=', I, ' VmRSS=', RssKB);
    end;
  end;
end.
