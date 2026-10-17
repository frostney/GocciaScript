program LeakTest;

{$I Goccia.inc}

uses
  Classes,
  SysUtils,
  Goccia.Engine,
  Goccia.Executor.Bytecode,
  Goccia.GarbageCollector;

function GetVmRSS: Int64;
var
  F: TextFile;
  Line: string;
  ColonPos: Integer;
begin
  Result := 0;
  if not FileExists('/proc/self/status') then
    Exit;
  AssignFile(F, '/proc/self/status');
  Reset(F);
  try
    while not Eof(F) do
    begin
      ReadLn(F, Line);
      if Copy(Line, 1, 6) = 'VmRSS:' then
      begin
        Delete(Line, 1, 6);
        Line := Trim(Line);
        ColonPos := Pos(' ', Line);
        if ColonPos > 0 then
          Line := Copy(Line, 1, ColonPos - 1);
        Result := StrToInt64Def(Line, 0);
        Break;
      end;
    end;
  finally
    CloseFile(F);
  end;
end;

procedure RunTest(const AScript: string; const ALabel: string);
var
  I: Integer;
  Engine: TGocciaEngine;
  Executor: TGocciaBytecodeExecutor;
  Source: TStringList;
  RSS10, RSS50, RSS100: Int64;
begin
  WriteLn('Testing: ', ALabel);
  RSS10 := 0;
  RSS50 := 0;
  RSS100 := 0;
  
  for I := 1 to 100 do
  begin
    Source := TStringList.Create;
    try
      Source.Text := AScript;
      Executor := TGocciaBytecodeExecutor.Create;
      try
        Engine := TGocciaEngine.Create('<test>', Source, Executor);
        try
          Engine.Execute;
        finally
          Engine.Free;
        end;
      finally
        Executor.Free;
      end;
    finally
      Source.Free;
    end;
    
    TGarbageCollector.Instance.Collect;
    
    if I = 10 then
      RSS10 := GetVmRSS;
    if I = 50 then
      RSS50 := GetVmRSS;
    if I = 100 then
      RSS100 := GetVmRSS;
  end;
  
  WriteLn('  i=10:  ', RSS10, ' kB');
  WriteLn('  i=50:  ', RSS50, ' kB');
  WriteLn('  i=100: ', RSS100, ' kB');
  WriteLn('  Growth 10->100: ', RSS100 - RSS10, ' kB (', 
          (RSS100 - RSS10) div 90, ' kB/iter)');
  WriteLn;
end;

var
  LiteralScript, ControlScript: string;
begin
  WriteLn('String Literal Leak Test (Issue #1704)');
  WriteLn('=======================================');
  WriteLn;
  
  LiteralScript := 'const s = "' + StringOfChar('x', 100000) + '";';
  ControlScript := 'const s = "x".repeat(100000);';
  
  RunTest(LiteralScript, 'Literal: const s = "<100k x>"');
  RunTest(ControlScript, 'Control: const s = "x".repeat(100000)');
end.
