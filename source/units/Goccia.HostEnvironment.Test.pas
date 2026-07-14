program Goccia.HostEnvironment.Test;

{$I Goccia.inc}

uses
  TestingPascalLibrary,

  Goccia.HostEnvironment;

type
  TGocciaHostEnvironmentTests = class(TTestSuite)
  private
    procedure TestDeterministicProfileUsesFixedHostValues;
    procedure TestHostProvidersCanBeInjected;
    procedure TestSeededRandomUsesPortableSequence;
    procedure TestChildStreamsAreDistinctAndRepeatable;
  public
    procedure SetupTests; override;
  end;

procedure TGocciaHostEnvironmentTests.SetupTests;
begin
  Test('deterministic profile uses fixed host values',
    TestDeterministicProfileUsesFixedHostValues);
  Test('host providers can be injected', TestHostProvidersCanBeInjected);
  Test('seeded random uses portable sequence',
    TestSeededRandomUsesPortableSequence);
  Test('child streams are distinct and repeatable',
    TestChildStreamsAreDistinctAndRepeatable);
end;

procedure TGocciaHostEnvironmentTests.TestHostProvidersCanBeInjected;
var
  EnvironmentA, EnvironmentB: TGocciaHostEnvironment;
begin
  EnvironmentA := TGocciaHostEnvironment.Create;
  EnvironmentB := TGocciaHostEnvironment.Create;
  try
    EnvironmentA.Configure(TGocciaFixedHostClock.Create(123, 456,
      'Etc/Injected'), TGocciaSeededHostRandom.Create(42));
    EnvironmentB.Configure(TGocciaFixedHostClock.Create(123, 456,
      'Etc/Injected'), TGocciaSeededHostRandom.Create(42));

    Expect<Int64>(EnvironmentA.EpochNanoseconds).ToBe(123);
    Expect<Int64>(EnvironmentA.MonotonicNanoseconds).ToBe(456);
    Expect<string>(EnvironmentA.TimeZoneIdentifier).ToBe('Etc/Injected');
    Expect<Double>(EnvironmentA.RandomDouble).ToBe(
      EnvironmentB.RandomDouble);
  finally
    EnvironmentB.Free;
    EnvironmentA.Free;
  end;
end;

procedure TGocciaHostEnvironmentTests.TestDeterministicProfileUsesFixedHostValues;
var
  Environment: TGocciaHostEnvironment;
begin
  Environment := TGocciaHostEnvironment.Create;
  try
    Environment.UseDeterministicProfile;
    Expect<Int64>(Environment.EpochNanoseconds).ToBe(0);
    Expect<Int64>(Environment.MonotonicNanoseconds).ToBe(0);
    Expect<string>(Environment.TimeZoneIdentifier).ToBe('UTC');
  finally
    Environment.Free;
  end;
end;

procedure TGocciaHostEnvironmentTests.TestSeededRandomUsesPortableSequence;
var
  Environment: TGocciaHostEnvironment;
begin
  Environment := TGocciaHostEnvironment.Create;
  try
    Environment.UseDeterministicProfile;
    Expect<Double>(Environment.RandomDouble).ToBe(0.8833108082136426);
    Expect<Double>(Environment.RandomDouble).ToBe(0.43152799704850997);
    Expect<Double>(Environment.RandomDouble).ToBe(0.026433771592597743);
  finally
    Environment.Free;
  end;
end;

procedure TGocciaHostEnvironmentTests.TestChildStreamsAreDistinctAndRepeatable;
var
  ChildA, ChildB: TGocciaHostEnvironment;
  ParentA, ParentB: TGocciaHostEnvironment;
begin
  ParentA := TGocciaHostEnvironment.Create;
  ParentB := TGocciaHostEnvironment.Create;
  ChildA := TGocciaHostEnvironment.Create;
  ChildB := TGocciaHostEnvironment.Create;
  try
    ParentA.UseDeterministicProfile;
    ParentB.UseDeterministicProfile;
    ChildA.ConfigureAsChildOf(ParentA);
    ChildB.ConfigureAsChildOf(ParentB);

    Expect<Int64>(ChildA.EpochNanoseconds).ToBe(0);
    Expect<string>(ChildA.TimeZoneIdentifier).ToBe('UTC');
    Expect<Double>(ChildA.RandomDouble).ToBe(0.6524484863740322);
    Expect<Double>(ChildB.RandomDouble).ToBe(0.6524484863740322);
    Expect<Boolean>(ParentA.RandomDouble <> ChildA.RandomDouble).ToBe(True);
  finally
    ChildB.Free;
    ChildA.Free;
    ParentB.Free;
    ParentA.Free;
  end;
end;

begin
  TestRunnerProgram.AddSuite(
    TGocciaHostEnvironmentTests.Create('Host Environment'));
  TestRunnerProgram.Run;
  ExitCode := TestResultToExitCode;
end.
