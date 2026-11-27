program EmbeddingInstructionCountsTest;

{$mode delphiunicode}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  SysUtils,
  Goccia.Embed;

procedure Check(ACondition: Boolean; const AMessage: UnicodeString);
begin
  if not ACondition then raise Exception.Create(AMessage);
end;

function MeasureEntry(const ASource: UnicodeString; ACap: Int64; AMeasure: Boolean): TGocciaRunResult;
var
  Engine: TGocciaEngine;
  ErrorText: UnicodeString;
begin
  Engine := TGocciaEngine.Create;
  try
    Check(Engine.PrepareHost(ErrorText), 'host setup failed: ' + ErrorText);
    Engine.MaxInstructions := ACap;
    Engine.MeasureInstructions := AMeasure;
    Engine.ResultForm := grfString;
    Result := Engine.Run('measure.js', ASource);
  finally
    Engine.Free;
  end;
end;

const
  Source = 'const cells = new Array(256); cells.fill(42); cells[13];';

var
  Unmeasured, Uncapped, Capped, Refused: TGocciaRunResult;
begin
  Unmeasured := MeasureEntry(Source, 0, False);
  Check(Unmeasured.Ok and (Unmeasured.ResultText = '42'), 'ordinary uncapped entry failed');
  Check(not Unmeasured.HasInstructionCount and (Unmeasured.InstructionCount = 0), 'unrequested count was reported');

  Uncapped := MeasureEntry(Source, 0, True);
  Capped := MeasureEntry(Source, 1000000, False);
  Check(Uncapped.Ok and (Uncapped.ResultText = '42') and Capped.Ok and (Capped.ResultText = '42'),
    'counting changed the result');
  Check(Uncapped.HasInstructionCount and Capped.HasInstructionCount, 'requested count was missing');
  Check((Uncapped.InstructionCount >= 256) and (Uncapped.InstructionCount = Capped.InstructionCount),
    'dispatch and native work charges differed between capped and uncapped execution');

  Refused := MeasureEntry(Source, 10, False);
  Check((Refused.Aborted = gaInstructionLimit) and Refused.HasInstructionCount and
    (Refused.InstructionCount = 11), 'exhaustion did not report the refused charge');
  WriteLn('Embedding instruction counts: passed');
end.
