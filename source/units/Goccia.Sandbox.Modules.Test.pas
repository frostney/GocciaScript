program Goccia.Sandbox.Modules.Test;

{$I Goccia.inc}

uses
  Classes,
  SysUtils,

  SandboxVirtualFileSystem,
  TestingPascalLibrary,

  Goccia.Sandbox.Modules,
  Goccia.TestSetup;

type
  TControlledClock = class(TSandboxFsClock)
  public
    TimeValue: TDateTime;
    function Now: TDateTime; override;
  end;

  TSandboxModuleTests = class(TTestSuite)
  private
    procedure TestBoundedReadRejectsBeforeAccess;
  public
    procedure SetupTests; override;
  end;

function TControlledClock.Now: TDateTime;
begin
  Result := TimeValue;
end;

procedure TSandboxModuleTests.SetupTests;
begin
  Test('Oversized module reads fail before accessing or copying sandbox bytes',
    TestBoundedReadRejectsBeforeAccess);
end;

procedure TSandboxModuleTests.TestBoundedReadRejectsBeforeAccess;
var
  Clock: TControlledClock;
  Fs: TSandboxVirtualFileSystem;
  Provider: TGocciaSandboxModuleContentProvider;
  Bytes, Loaded: TBytes;
  Rejected: Boolean;
begin
  Clock := TControlledClock.Create;
  try
    Clock.TimeValue := 1;
    Fs := TSandboxVirtualFileSystem.Create(1024, Clock);
    try
      SetLength(Bytes, 3);
      Bytes[0] := 0;
      Bytes[1] := 255;
      Bytes[2] := 42;
      Fs.WriteAllBytes('/library.gbc', Bytes);
      Provider := TGocciaSandboxModuleContentProvider.Create(Fs);
      try
        Clock.TimeValue := 2;
        Rejected := False;
        try
          Loaded := Provider.LoadContentBytesBounded('/library.gbc', 2);
        except
          on E: EStreamError do
            Rejected := True;
        end;
        Expect<Boolean>(Rejected).ToBe(True);
        { ReadAllBytes updates access time before allocating its copy. }
        Expect<TDateTime>(Fs.Stat('/library.gbc').AccessedAt).ToBe(1);
        Loaded := Provider.LoadContentBytesBounded('/library.gbc', 3);
        Expect<Integer>(Length(Loaded)).ToBe(3);
        Expect<Integer>(Loaded[0]).ToBe(0);
        Expect<Integer>(Loaded[1]).ToBe(255);
        Expect<Integer>(Loaded[2]).ToBe(42);
        Expect<TDateTime>(Fs.Stat('/library.gbc').AccessedAt).ToBe(2);
      finally
        Provider.Free;
      end;
    finally
      Fs.Free;
    end;
  finally
    Clock.Free;
  end;
end;

begin
  TestRunnerProgram.AddSuite(TSandboxModuleTests.Create('SandboxModules'));
  RunGocciaTests;
  ExitCode := TestResultToExitCode;
end.
