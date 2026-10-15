program MemLeakRepro;

{$I Goccia.inc}

uses
  Classes,
  SysUtils,
  Goccia.Engine,
  Goccia.Executor.Bytecode,
  Goccia.GarbageCollector;

function GetVmRSS: Int64;
var
  StatusFile: TextFile;
  Line: string;
  Parts: TStringArray;
begin
  Result := 0;
  if FileExists('/proc/self/status') then
  begin
    AssignFile(StatusFile, '/proc/self/status');
    Reset(StatusFile);
    try
      while not Eof(StatusFile) do
      begin
        ReadLn(StatusFile, Line);
        if Pos('VmRSS:', Line) = 1 then
        begin
          // Parse "VmRSS:    12345 kB"
          Delete(Line, 1, 6); // Remove "VmRSS:"
          Line := Trim(Line);
          // Extract first number
          Parts := Line.Split([' ', #9]);
          if Length(Parts) >= 1 then
            Result := StrToInt64Def(Parts[0], 0);
          Break;
        end;
      end;
    finally
      CloseFile(StatusFile);
    end;
  end;
end;

procedure RunTest(const AScript: string; const ALabel: string; AIterations: Integer);
var
  I: Integer;
  Engine: TGocciaEngine;
  Executor: TGocciaBytecodeExecutor;
  SourceLines: TStringList;
  VmRSS10, VmRSS50, VmRSS100: Int64;
begin
  WriteLn('Testing: ', ALabel);
  VmRSS10 := 0;
  VmRSS50 := 0;
  VmRSS100 := 0;
  
  for I := 1 to AIterations do
  begin
    SourceLines := TStringList.Create;
    try
      SourceLines.Text := AScript;
      Executor := TGocciaBytecodeExecutor.Create;
      try
        Engine := TGocciaEngine.Create('<test>', SourceLines, Executor);
        try
          Engine.Execute;
        finally
          Engine.Free;
        end;
      finally
        Executor.Free;
      end;
    finally
      SourceLines.Free;
    end;
    
    TGarbageCollector.Instance.Collect;
    
    if I = 10 then
      VmRSS10 := GetVmRSS;
    if I = 50 then
      VmRSS50 := GetVmRSS;
    if I = 100 then
      VmRSS100 := GetVmRSS;
  end;
  
  WriteLn('  VmRSS at i=10:  ', VmRSS10, ' kB');
  WriteLn('  VmRSS at i=50:  ', VmRSS50, ' kB');
  WriteLn('  VmRSS at i=100: ', VmRSS100, ' kB');
  WriteLn('  Growth 10->100: ', VmRSS100 - VmRSS10, ' kB (', 
          (VmRSS100 - VmRSS10) div 90, ' kB/iter)');
  WriteLn;
end;

var
  LiteralScript: string;
  RepeatScript: string;
begin
  WriteLn('String Literal Leak Repro');
  WriteLn('=========================');
  WriteLn;
  
  // Create a 100k character literal
  LiteralScript := 'const s = "' + StringOfChar('x', 100000) + '";';
  RepeatScript := 'const s = "x".repeat(100000);';
  
  RunTest(LiteralScript, 'Literal: const s = "<100k x>"', 100);
  RunTest(RepeatScript, 'Control: const s = "x".repeat(100000)', 100);
end.
