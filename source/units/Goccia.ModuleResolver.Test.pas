program Goccia.ModuleResolver.Test;

{$I Goccia.inc}

uses
  SysUtils,

  TestingPascalLibrary,

  Goccia.ModuleResolver,
  Goccia.TestSetup;

type
  TTestModuleResolver = class(TModuleResolver)
  public
    function ExposedApplyAliases(const AModulePath,
      AImportingFilePath: string): string;
  end;

  TModuleResolverTests = class(TTestSuite)
  private
    function CreateResolver: TTestModuleResolver;

    procedure TestHasAliasMatchesExactAlias;
    procedure TestHasAliasRejectsChildPathForExactAlias;
    procedure TestHasAliasMatchesPrefixAlias;
    procedure TestApplyAliasesLeavesUnmatchedSpecifierUnchanged;
    procedure TestApplyAliasesUsesLongestPrefixAlias;
    procedure TestApplyAliasesMatchesNormalizedRelativeSpecifier;
  public
    procedure SetupTests; override;
  end;

function TTestModuleResolver.ExposedApplyAliases(const AModulePath,
  AImportingFilePath: string): string;
begin
  Result := ApplyAliases(AModulePath, AImportingFilePath);
end;

procedure TModuleResolverTests.SetupTests;
begin
  Test('HasAlias matches exact alias', TestHasAliasMatchesExactAlias);
  Test('HasAlias rejects child path for exact alias', TestHasAliasRejectsChildPathForExactAlias);
  Test('HasAlias matches prefix alias', TestHasAliasMatchesPrefixAlias);
  Test('ApplyAliases leaves unmatched specifier unchanged', TestApplyAliasesLeavesUnmatchedSpecifierUnchanged);
  Test('ApplyAliases uses longest prefix alias', TestApplyAliasesUsesLongestPrefixAlias);
  Test('ApplyAliases matches normalized relative specifier', TestApplyAliasesMatchesNormalizedRelativeSpecifier);
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
    Resolver.AddAlias('lodash', 'vendor/lodash/index.js');

    Expect<Boolean>(Resolver.HasAlias('lodash')).ToBe(True);
  finally
    Resolver.Free;
  end;
end;

procedure TModuleResolverTests.TestHasAliasRejectsChildPathForExactAlias;
var
  Resolver: TTestModuleResolver;
begin
  Resolver := CreateResolver;
  try
    Resolver.AddAlias('lodash', 'vendor/lodash/index.js');

    Expect<Boolean>(Resolver.HasAlias('lodash/fp')).ToBe(False);
  finally
    Resolver.Free;
  end;
end;

procedure TModuleResolverTests.TestHasAliasMatchesPrefixAlias;
var
  Resolver: TTestModuleResolver;
begin
  Resolver := CreateResolver;
  try
    Resolver.AddAlias('@lib/', 'src/lib/');

    Expect<Boolean>(Resolver.HasAlias('@lib/utils')).ToBe(True);
  finally
    Resolver.Free;
  end;
end;

procedure TModuleResolverTests.TestApplyAliasesLeavesUnmatchedSpecifierUnchanged;
var
  Resolver: TTestModuleResolver;
begin
  Resolver := CreateResolver;
  try
    Resolver.AddAlias('lodash', 'vendor/lodash/index.js');

    Expect<string>(Resolver.ExposedApplyAliases('lodash/fp', '')).ToBe('lodash/fp');
  finally
    Resolver.Free;
  end;
end;

procedure TModuleResolverTests.TestApplyAliasesUsesLongestPrefixAlias;
var
  Resolver: TTestModuleResolver;
begin
  Resolver := CreateResolver;
  try
    Resolver.AddAlias('@/', 'src/');
    Resolver.AddAlias('@/components/', 'src/ui/components/');

    Expect<string>(Resolver.ExposedApplyAliases('@/components/Button', '')).ToBe(
      Resolver.BaseDirectory + 'src/ui/components/Button');
  finally
    Resolver.Free;
  end;
end;

procedure TModuleResolverTests.TestApplyAliasesMatchesNormalizedRelativeSpecifier;
var
  ImportingFilePath, ProjectDirectory: string;
  Resolver: TTestModuleResolver;
begin
  Resolver := CreateResolver;
  try
    ProjectDirectory := IncludeTrailingPathDelimiter(GetCurrentDir);
    ImportingFilePath := ProjectDirectory + 'src' + PathDelim + 'app' +
      PathDelim + 'main.js';
    Resolver.AddAlias(
      ProjectDirectory + 'src' + PathDelim + 'shared' + PathDelim + 'math.js',
      ProjectDirectory + 'vendor' + PathDelim + 'math.js');

    Expect<string>(Resolver.ExposedApplyAliases('../shared/math.js',
      ImportingFilePath)).ToBe(ProjectDirectory + 'vendor' + PathDelim +
      'math.js');
  finally
    Resolver.Free;
  end;
end;

begin
  TestRunnerProgram.AddSuite(TModuleResolverTests.Create('ModuleResolver'));
  TestRunnerProgram.Run;

  ExitCode := TestResultToExitCode;
end.
