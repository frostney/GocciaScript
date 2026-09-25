program Goccia.ModuleResolver.Test;

{$I Goccia.inc}

uses
  SysUtils,

  FileUtils,
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
    procedure TestResolveFailureForRelativeSpecifierHidesHostPath;
    procedure TestResolveFailureForAliasHidesHostPath;
  public
    procedure SetupTests; override;
  end;

const
  MISSING_RELATIVE_SPECIFIER = './definitely-missing-probe.js';
  MISSING_ALIAS_PREFIX = '@missing/';
  MISSING_ALIAS_SPECIFIER = '@missing/definitely-missing-probe.js';
  MISSING_ALIAS_REPLACEMENT = 'vendor/missing/';

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
  Test('Resolve failure for relative specifier hides the host path', TestResolveFailureForRelativeSpecifierHidesHostPath);
  Test('Resolve failure for alias hides the host path', TestResolveFailureForAliasHidesHostPath);
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

procedure TModuleResolverTests.TestResolveFailureForRelativeSpecifierHidesHostPath;
var
  CandidatePath, FailureMessage: string;
  ImportingFilePath, ProjectDirectory: string;
  Raised: Boolean;
  Resolver: TTestModuleResolver;
begin
  Resolver := CreateResolver;
  try
    ProjectDirectory := IncludeTrailingPathDelimiter(GetCurrentDir);
    ImportingFilePath := ProjectDirectory + 'entry.js';
    Raised := False;
    CandidatePath := '';
    FailureMessage := '';

    try
      Resolver.Resolve(MISSING_RELATIVE_SPECIFIER, ImportingFilePath);
    except
      on E: EModuleNotFound do
      begin
        Raised := True;
        FailureMessage := E.Message;
        CandidatePath := E.ResolvedCandidatePath;
      end;
    end;

    Expect<Boolean>(Raised).ToBe(True);
    Expect<string>(FailureMessage).ToBe(
      Format(MODULE_NOT_FOUND_MESSAGE_FORMAT, [MISSING_RELATIVE_SPECIFIER]));
    if Pos(ProjectDirectory, FailureMessage) > 0 then
      Fail('Resolution failure message leaked the expanded host directory.');
    Expect<string>(CandidatePath).ToBe(ExpandHostFileName(ProjectDirectory +
      MISSING_RELATIVE_SPECIFIER));
  finally
    Resolver.Free;
  end;
end;

procedure TModuleResolverTests.TestResolveFailureForAliasHidesHostPath;
var
  CandidatePath, FailureMessage, ProjectDirectory: string;
  Raised: Boolean;
  Resolver: TTestModuleResolver;
begin
  Resolver := CreateResolver;
  try
    ProjectDirectory := IncludeTrailingPathDelimiter(GetCurrentDir);
    Resolver.AddAlias(MISSING_ALIAS_PREFIX, MISSING_ALIAS_REPLACEMENT);
    Raised := False;
    CandidatePath := '';
    FailureMessage := '';

    try
      Resolver.Resolve(MISSING_ALIAS_SPECIFIER, ProjectDirectory + 'entry.js');
    except
      on E: EModuleNotFound do
      begin
        Raised := True;
        FailureMessage := E.Message;
        CandidatePath := E.ResolvedCandidatePath;
      end;
    end;

    Expect<Boolean>(Raised).ToBe(True);
    Expect<string>(FailureMessage).ToBe(
      Format(MODULE_NOT_FOUND_MESSAGE_FORMAT, [MISSING_ALIAS_SPECIFIER]));
    if Pos(ProjectDirectory, FailureMessage) > 0 then
      Fail('Alias resolution failure message leaked the expanded host directory.');
    if Pos(MISSING_ALIAS_REPLACEMENT, FailureMessage) > 0 then
      Fail('Alias resolution failure message leaked the alias replacement.');
    { Build the expectation the way Resolve does — ExpandHostFileName over the
      alias replacement joined with '/' — so the assertion does not assume how
      the host rewrites separators. }
    Expect<string>(CandidatePath).ToBe(ExpandHostFileName(ProjectDirectory +
      MISSING_ALIAS_REPLACEMENT + 'definitely-missing-probe.js'));
  finally
    Resolver.Free;
  end;
end;

begin
  TestRunnerProgram.AddSuite(TModuleResolverTests.Create('ModuleResolver'));
  RunGocciaTests;

  ExitCode := TestResultToExitCode;
end.
