program Goccia.ModuleResolver.Test;

{$I Goccia.inc}

uses
  SysUtils,

  ModuleResolver,
  TestRunner,

  Goccia.TestSetup;

type
  TTestModuleResolver = class(TModuleResolver)
  public
    function ExposedApplyAliases(const AModulePath: string): string;
  end;

  TModuleResolverTests = class(TTestSuite)
  private
    function CreateResolver: TTestModuleResolver;

    procedure TestHasAliasMatchesExactAlias;
    procedure TestHasAliasMatchesSegmentChild;
    procedure TestHasAliasRejectsPartialSegmentMatch;
    procedure TestApplyAliasesLeavesPartialSegmentUnchanged;
    procedure TestApplyAliasesUsesLongestBoundaryMatchedAlias;
  public
    procedure SetupTests; override;
  end;

function TTestModuleResolver.ExposedApplyAliases(const AModulePath: string): string;
begin
  Result := ApplyAliases(AModulePath);
end;

procedure TModuleResolverTests.SetupTests;
begin
  Test('HasAlias matches exact alias', TestHasAliasMatchesExactAlias);
  Test('HasAlias matches child path on segment boundary', TestHasAliasMatchesSegmentChild);
  Test('HasAlias rejects partial segment match', TestHasAliasRejectsPartialSegmentMatch);
  Test('ApplyAliases leaves partial segment unchanged', TestApplyAliasesLeavesPartialSegmentUnchanged);
  Test('ApplyAliases uses longest boundary-matched alias', TestApplyAliasesUsesLongestBoundaryMatchedAlias);
end;

function TModuleResolverTests.CreateResolver: TTestModuleResolver;
begin
  Result := TTestModuleResolver.Create(GetCurrentDir);
end;

procedure TModuleResolverTests.TestHasAliasMatchesExactAlias;
var
  Resolver: TTestModuleResolver;
begin
  Resolver := CreateResolver;
  try
    Resolver.AddAlias('@lib', 'src/lib');

    Expect<Boolean>(Resolver.HasAlias('@lib')).ToBe(True);
  finally
    Resolver.Free;
  end;
end;

procedure TModuleResolverTests.TestHasAliasMatchesSegmentChild;
var
  Resolver: TTestModuleResolver;
begin
  Resolver := CreateResolver;
  try
    Resolver.AddAlias('@lib', 'src/lib');

    Expect<Boolean>(Resolver.HasAlias('@lib/utils/math')).ToBe(True);
  finally
    Resolver.Free;
  end;
end;

procedure TModuleResolverTests.TestHasAliasRejectsPartialSegmentMatch;
var
  Resolver: TTestModuleResolver;
begin
  Resolver := CreateResolver;
  try
    Resolver.AddAlias('@lib', 'src/lib');

    Expect<Boolean>(Resolver.HasAlias('@library/utils')).ToBe(False);
  finally
    Resolver.Free;
  end;
end;

procedure TModuleResolverTests.TestApplyAliasesLeavesPartialSegmentUnchanged;
var
  Resolver: TTestModuleResolver;
begin
  Resolver := CreateResolver;
  try
    Resolver.AddAlias('@lib', 'src/lib');

    Expect<string>(Resolver.ExposedApplyAliases('@library/utils')).ToBe('@library/utils');
  finally
    Resolver.Free;
  end;
end;

procedure TModuleResolverTests.TestApplyAliasesUsesLongestBoundaryMatchedAlias;
var
  Resolver: TTestModuleResolver;
begin
  Resolver := CreateResolver;
  try
    Resolver.AddAlias('@lib', 'src/lib');
    Resolver.AddAlias('@lib/components', 'src/ui/components');

    Expect<string>(Resolver.ExposedApplyAliases('@lib/components/Button')).ToBe(
      Resolver.BaseDirectory + 'src/ui/components/Button');
  finally
    Resolver.Free;
  end;
end;

begin
  TestRunnerProgram.AddSuite(TModuleResolverTests.Create('ModuleResolver'));
  TestRunnerProgram.Run;

  ExitCode := TestResultToExitCode;
end.
