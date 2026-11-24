program Goccia.Engine.Warnings.Test;

{ TGocciaEngine.SuppressWarnings reaches the module loader that prints the
  parse warnings of imported modules, so a host that mutes the entry's warnings
  mutes theirs too. The loader prints on standard output, so what a muted run
  prints is pinned by the CLI tests; this pins the setting that decides it. }

{$I Goccia.inc}

uses
  Classes,
  SysUtils,

  TestingPascalLibrary,

  Goccia.Engine,
  Goccia.TestSetup;

type
  TEngineWarningsTests = class(TTestSuite)
  private
    function CreateEngine(const ASource: TStringList): TGocciaEngine;
    procedure TestWarningsAreOnByDefault;
    procedure TestEngineSettingReachesTheLoader;
    procedure TestSettingCanBeLiftedAgain;
  public
    procedure SetupTests; override;
  end;

procedure TEngineWarningsTests.SetupTests;
begin
  Test('imported modules print their parse warnings by default',
    TestWarningsAreOnByDefault);
  Test('SuppressWarnings on the engine mutes the module loader',
    TestEngineSettingReachesTheLoader);
  Test('SuppressWarnings can be lifted again', TestSettingCanBeLiftedAgain);
end;

function TEngineWarningsTests.CreateEngine(
  const ASource: TStringList): TGocciaEngine;
begin
  Result := TGocciaEngine.Create('<warnings>', ASource);
end;

procedure TEngineWarningsTests.TestWarningsAreOnByDefault;
var
  Engine: TGocciaEngine;
  Source: TStringList;
begin
  Source := TStringList.Create;
  Engine := CreateEngine(Source);
  try
    Expect<Boolean>(Engine.SuppressWarnings).ToBe(False);
    Expect<Boolean>(Engine.ModuleLoader.SuppressWarnings).ToBe(False);
  finally
    Engine.Free;
    Source.Free;
  end;
end;

procedure TEngineWarningsTests.TestEngineSettingReachesTheLoader;
var
  Engine: TGocciaEngine;
  Source: TStringList;
begin
  Source := TStringList.Create;
  Engine := CreateEngine(Source);
  try
    Engine.SuppressWarnings := True;
    Expect<Boolean>(Engine.SuppressWarnings).ToBe(True);
    Expect<Boolean>(Engine.ModuleLoader.SuppressWarnings).ToBe(True);
  finally
    Engine.Free;
    Source.Free;
  end;
end;

procedure TEngineWarningsTests.TestSettingCanBeLiftedAgain;
var
  Engine: TGocciaEngine;
  Source: TStringList;
begin
  Source := TStringList.Create;
  Engine := CreateEngine(Source);
  try
    Engine.SuppressWarnings := True;
    Engine.SuppressWarnings := False;
    Expect<Boolean>(Engine.ModuleLoader.SuppressWarnings).ToBe(False);
  finally
    Engine.Free;
    Source.Free;
  end;
end;

begin
  TestRunnerProgram.AddSuite(TEngineWarningsTests.Create('Engine warnings'));
  RunGocciaTests;
  ExitCode := TestResultToExitCode;
end.
