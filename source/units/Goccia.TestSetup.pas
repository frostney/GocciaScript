unit Goccia.TestSetup;

{$I Goccia.inc}

interface

procedure RunGocciaTests;

implementation

uses
  TestingPascalLibrary,

  Goccia.FloatingPoint;

procedure RunGocciaTests;
var
  FloatingPointState: TGocciaFloatingPointState;
begin
  EnterGocciaFloatingPointScope(FloatingPointState);
  try
    TestRunnerProgram.Run;
  finally
    LeaveGocciaFloatingPointScope(FloatingPointState);
  end;
end;

end.
