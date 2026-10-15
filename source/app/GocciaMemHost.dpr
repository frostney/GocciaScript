program GocciaMemHost;
{ bench host: GocciaMemHost <keep|seq> <N> <file>
  keep: create N runtimes (bytecode, console only), execute file in each, keep all alive
  seq : create/execute/free N times sequentially, print VmRSS every N/10 }
{$I Goccia.inc}
uses {$IFDEF UNIX}cthreads,{$ENDIF} Classes, SysUtils,
  Goccia.Runtime, Goccia.RuntimeExtensions.Console, Goccia.GarbageCollector,
  Goccia.Bytecode.Chunk;
function RssKB: string; var F: TStringList; I: Integer;
begin F := TStringList.Create; Result := '?';
  try F.LoadFromFile('/proc/self/status');
    for I := 0 to F.Count-1 do if Pos('VmRSS', F[I]) = 1 then Result := Trim(Copy(F[I],7,99));
  finally F.Free; end; end;
var Mode: string; N, I: Integer; Src: TStringList; Rts: array of TGocciaRuntime; R: TGocciaRuntime;
begin
  Mode := ParamStr(1); N := StrToInt(ParamStr(2));
  Src := TStringList.Create; Src.LoadFromFile(ParamStr(3));
  if Mode = 'keep' then begin
    SetLength(Rts, N);
    for I := 0 to N-1 do begin
      Rts[I] := TGocciaRuntime.Create(ParamStr(3), Src);
      Rts[I].Install(TGocciaConsoleRuntimeExtension.Create);
      Rts[I].Execute;
    end;
    WriteLn(StdErr, 'keep N=', N, ' VmRSS=', RssKB);
  end else begin
    for I := 1 to N do begin
      R := TGocciaRuntime.Create(ParamStr(3), Src);
      R.Install(TGocciaConsoleRuntimeExtension.Create);
      R.Execute; R.Free; 
      if Mode = 'seqgc' then 
      begin
        TGarbageCollector.Instance.Collect;
        CheckForTrackedStringLeaks;
      end;
      if (N >= 10) and (I mod (N div 10) = 0) then WriteLn(StdErr, 'seq i=', I, ' VmRSS=', RssKB);
    end;
  end;
end.
