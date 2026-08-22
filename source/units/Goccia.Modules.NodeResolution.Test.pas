program Goccia.Modules.NodeResolution.Test;

{$I Goccia.inc}
{ This file contains non-ASCII source string literals (e.g. `café`); pin the
  source codepage so they decode as UTF-8 on every target rather than through
  the platform default. }
{$codepage utf8}

uses
  {$IFDEF UNIX}BaseUnix,{$ENDIF}
  Classes,
  SysUtils,

  FileUtils,
  TestingPascalLibrary,

  Goccia.Modules.NodeResolution,
  Goccia.TestSetup;

type
  TNodeResolutionTests = class(TTestSuite)
  private
    FTempDirectories: TStringList;

    function CreateTempDirectory: string;
    procedure DeleteDirectoryTree(const APath: string);
    procedure WriteFixtureFile(const APath, AText: string);
    function TryCreateSymlink(const ATarget, ALinkPath: string): Boolean;
    procedure TestSplitsPlainPackageName;
    procedure TestSplitsPackageSubpath;
    procedure TestSplitsScopedPackageName;
    procedure TestSplitsScopedPackageSubpath;
    procedure TestRejectsNonBareSpecifiers;
    procedure TestRejectsIncompleteScope;

    procedure TestReadsLegacyManifestFields;
    procedure TestManifestWithoutExportsReportsNoExportsField;
    procedure TestStringExportsResolvesMainEntryOnly;
    procedure TestConditionsAreTriedInDeclarationOrder;
    procedure TestUnsupportedConditionIsSkipped;
    procedure TestSubpathMapResolvesExactKey;
    procedure TestWildcardPatternSubstitutesStar;
    procedure TestExactKeyBeatsWildcardPattern;
    procedure TestLongestPatternBaseWins;
    procedure TestNullTargetBlocksSubpath;
    procedure TestArrayTargetFallsBackToFirstUsable;
    procedure TestUnlistedSubpathIsNotExported;

    procedure TestInvalidSegmentsAreDetected;
    procedure TestValidSegmentsAreAccepted;
    procedure TestExportsTargetMustBeRelative;
    procedure TestExportsTargetMayNotEscapeThePackage;
    procedure TestPatternStarValueMayNotEscapeThePackage;
    procedure TestLegacySubpathMayNotEscapeThePackage;
    procedure TestNodeModulesSegmentIsRejected;
    procedure TestContainmentAcceptsPathsInsideTheDirectory;
    procedure TestContainmentRejectsSiblingAndEscapedPaths;
    procedure TestPhysicalContainmentRejectsAnEscapingChildLink;
    procedure TestPhysicalContainmentAllowsAnInPackageLink;
    procedure TestPhysicalContainmentAllowsAStoreLinkedPackageRoot;
    procedure TestPhysicalContainmentRejectsAnEscapeFromALinkedRoot;

    procedure TestModuleFieldIsPreferredOverMain;
    procedure TestMainIsUsedWhenNoModuleField;
    procedure TestIndexIsTheLastResort;
    procedure TestSubpathIsLiteralWithoutExports;
    procedure TestExportsMapSuppressesLegacyFields;

    procedure TestRequireCallLooksLikeCommonJS;
    procedure TestModuleExportsLooksLikeCommonJS;
    procedure TestESModuleSourceDoesNotLookLikeCommonJS;
    procedure TestMixedSourceIsReadAsESModule;
    procedure TestMinifiedSideEffectImportIsReadAsESModule;
    procedure TestInertSourceIsNotCommonJS;
    procedure TestIdentifierEndingInRequireIsNotACall;

    procedure TestEsbuildBannerIsNotAnESModuleMarker;
    procedure TestStringLiteralMarkerIsNotAnESModuleMarker;
    procedure TestBlockCommentSpanningLinesIsStripped;
    procedure TestTemplateBodyIsStrippedButSubstitutionsAreNot;
    procedure TestCommentedESModuleStaysAnESModule;
    procedure TestRegExpLiteralDoesNotSwallowCode;
    procedure TestUnterminatedCommentFallsBackToTheRawScan;

    procedure TestNonASCIIIdentifierBeforeASlashDivides;
    procedure TestStringBeforeASlashDivides;
    procedure TestLineSeparatorEndsALineComment;
    procedure TestCarriageReturnEndsALineComment;
    procedure TestSubstitutionBracesAreTrackedByDepth;
    procedure TestNestedTemplateLiteralsAreTracked;
    procedure TestUnterminatedTemplateFallsBackToTheRawScan;
  public
    procedure BeforeAll; override;
    procedure AfterAll; override;
    procedure SetupTests; override;
  end;

procedure TNodeResolutionTests.BeforeAll;
begin
  inherited BeforeAll;
  FTempDirectories := TStringList.Create;
end;

procedure TNodeResolutionTests.AfterAll;
var
  I: Integer;
begin
  for I := 0 to FTempDirectories.Count - 1 do
    DeleteDirectoryTree(FTempDirectories[I]);
  FTempDirectories.Free;
  inherited AfterAll;
end;

function TNodeResolutionTests.CreateTempDirectory: string;
begin
  Result := IncludeTrailingPathDelimiter(GetTempDir(False)) +
    'goccia-node-containment-' + IntToStr(Random(MaxInt));
  { The name is deterministic — nothing seeds the generator — so a run that
    died before its teardown would otherwise poison every run after it: a
    symlink fixture cannot be created over one that is already there, and the
    test would fail on the leftover rather than on the code. }
  DeleteDirectoryTree(Result);
  ForceDirectories(Result);
  FTempDirectories.Add(Result);
end;

procedure TNodeResolutionTests.DeleteDirectoryTree(const APath: string);
var
  EntryPath: string;
  SearchRec: TSearchRec;
begin
  if not DirectoryExists(APath) then
    Exit;

  { faSymLink has to be asked for: without it FindFirst stats through a link
    and silently drops any that no longer resolves, which is exactly what the
    escape fixtures leave behind once their target is gone. }
  if FindFirst(IncludeTrailingPathDelimiter(APath) + '*',
    faAnyFile or faSymLink, SearchRec) = 0 then
  begin
    repeat
      if (SearchRec.Name = '.') or (SearchRec.Name = '..') then
        Continue;

      EntryPath := IncludeTrailingPathDelimiter(APath) + SearchRec.Name;
      { A symlink to a directory must be unlinked, not descended into and
        emptied — otherwise the teardown deletes whatever it points at. }
      if ((SearchRec.Attr and faDirectory) = faDirectory) and
         (not FileUtils.HostPathIsSymlink(EntryPath)) then
        DeleteDirectoryTree(EntryPath)
      else
        DeleteFile(EntryPath);
    until FindNext(SearchRec) <> 0;
    FindClose(SearchRec);
  end;

  RemoveDir(APath);
end;

procedure TNodeResolutionTests.WriteFixtureFile(const APath, AText: string);
begin
  ForceDirectories(ExtractFileDir(APath));
  FileUtils.WriteUTF8FileText(APath, AText);
end;

function TNodeResolutionTests.TryCreateSymlink(const ATarget,
  ALinkPath: string): Boolean;
begin
  {$IFDEF UNIX}
  Result := fpSymlink(PAnsiChar(AnsiString(ATarget)),
    PAnsiChar(AnsiString(ALinkPath))) = 0;
  {$ELSE}
  { Only the UNIX cases are registered; see SetupTests. }
  Result := False;
  {$ENDIF}
end;

procedure TNodeResolutionTests.SetupTests;
begin
  Test('SplitBareSpecifier splits a plain package name',
    TestSplitsPlainPackageName);
  Test('SplitBareSpecifier splits a package subpath',
    TestSplitsPackageSubpath);
  Test('SplitBareSpecifier splits a scoped package name',
    TestSplitsScopedPackageName);
  Test('SplitBareSpecifier splits a scoped package subpath',
    TestSplitsScopedPackageSubpath);
  Test('SplitBareSpecifier rejects non-bare specifiers',
    TestRejectsNonBareSpecifiers);
  Test('SplitBareSpecifier rejects a scope without a package',
    TestRejectsIncompleteScope);

  Test('Manifest parsing reads name, type, main, and module',
    TestReadsLegacyManifestFields);
  Test('Manifest without exports reports no exports field',
    TestManifestWithoutExportsReportsNoExportsField);
  Test('String exports resolves the main entry only',
    TestStringExportsResolvesMainEntryOnly);
  Test('Conditions are tried in declaration order',
    TestConditionsAreTriedInDeclarationOrder);
  Test('Unsupported condition is skipped', TestUnsupportedConditionIsSkipped);
  Test('Subpath map resolves an exact key', TestSubpathMapResolvesExactKey);
  Test('Wildcard pattern substitutes the star',
    TestWildcardPatternSubstitutesStar);
  Test('Exact key beats a matching wildcard pattern',
    TestExactKeyBeatsWildcardPattern);
  Test('Longest pattern base wins', TestLongestPatternBaseWins);
  Test('Null target blocks a subpath', TestNullTargetBlocksSubpath);
  Test('Array target falls back to the first usable entry',
    TestArrayTargetFallsBackToFirstUsable);
  Test('Unlisted subpath is not exported', TestUnlistedSubpathIsNotExported);

  Test('Invalid path segments are detected', TestInvalidSegmentsAreDetected);
  Test('Ordinary path segments are accepted', TestValidSegmentsAreAccepted);
  Test('An exports target must start with "./"',
    TestExportsTargetMustBeRelative);
  Test('An exports target may not escape the package',
    TestExportsTargetMayNotEscapeThePackage);
  Test('A pattern star value may not escape the package',
    TestPatternStarValueMayNotEscapeThePackage);
  Test('A legacy subpath may not escape the package',
    TestLegacySubpathMayNotEscapeThePackage);
  Test('A node_modules segment is rejected', TestNodeModulesSegmentIsRejected);
  Test('Containment accepts paths inside the directory',
    TestContainmentAcceptsPathsInsideTheDirectory);
  Test('Containment rejects sibling and escaped paths',
    TestContainmentRejectsSiblingAndEscapedPaths);
  { Real symlinks on disk. Creating one needs an API this build only has on
    UNIX, and on Windows it also needs a privilege an unelevated CI job does
    not hold, so the cases are registered as skipped there rather than
    reporting a pass they never ran. }
  {$IFDEF UNIX}
  Test('Physical containment rejects a package child linked outside it',
    TestPhysicalContainmentRejectsAnEscapingChildLink);
  Test('Physical containment allows a link inside the package',
    TestPhysicalContainmentAllowsAnInPackageLink);
  Test('Physical containment allows a store-linked package root',
    TestPhysicalContainmentAllowsAStoreLinkedPackageRoot);
  Test('Physical containment rejects an escape from a store-linked root',
    TestPhysicalContainmentRejectsAnEscapeFromALinkedRoot);
  {$ELSE}
  Skip('Physical containment rejects a package child linked outside it',
    TestPhysicalContainmentRejectsAnEscapingChildLink,
    'creating a symlink is not available on this platform');
  Skip('Physical containment allows a link inside the package',
    TestPhysicalContainmentAllowsAnInPackageLink,
    'creating a symlink is not available on this platform');
  Skip('Physical containment allows a store-linked package root',
    TestPhysicalContainmentAllowsAStoreLinkedPackageRoot,
    'creating a symlink is not available on this platform');
  Skip('Physical containment rejects an escape from a store-linked root',
    TestPhysicalContainmentRejectsAnEscapeFromALinkedRoot,
    'creating a symlink is not available on this platform');
  {$ENDIF}

  Test('module field is preferred over main', TestModuleFieldIsPreferredOverMain);
  Test('main is used when there is no module field',
    TestMainIsUsedWhenNoModuleField);
  Test('index is the last resort', TestIndexIsTheLastResort);
  Test('subpath is taken literally without an exports map',
    TestSubpathIsLiteralWithoutExports);
  Test('an exports map suppresses the legacy fields',
    TestExportsMapSuppressesLegacyFields);

  Test('a require call looks like CommonJS', TestRequireCallLooksLikeCommonJS);
  Test('module.exports looks like CommonJS',
    TestModuleExportsLooksLikeCommonJS);
  Test('ES module source does not look like CommonJS',
    TestESModuleSourceDoesNotLookLikeCommonJS);
  Test('source with both shapes is read as an ES module',
    TestMixedSourceIsReadAsESModule);
  Test('a minified space-free side-effect import is read as an ES module',
    TestMinifiedSideEffectImportIsReadAsESModule);
  Test('inert source is not CommonJS', TestInertSourceIsNotCommonJS);
  Test('an identifier ending in require is not a require call',
    TestIdentifierEndingInRequireIsNotACall);

  Test('an esbuild banner comment is not an ES module marker',
    TestEsbuildBannerIsNotAnESModuleMarker);
  Test('a marker inside a string literal does not count',
    TestStringLiteralMarkerIsNotAnESModuleMarker);
  Test('a block comment spanning lines is stripped',
    TestBlockCommentSpanningLinesIsStripped);
  Test('a template body is stripped but its substitutions are not',
    TestTemplateBodyIsStrippedButSubstitutionsAreNot);
  Test('a commented ES module stays an ES module',
    TestCommentedESModuleStaysAnESModule);
  Test('a regular expression literal does not swallow the code after it',
    TestRegExpLiteralDoesNotSwallowCode);
  Test('an unterminated comment falls back to the raw scan',
    TestUnterminatedCommentFallsBackToTheRawScan);

  Test('a slash after a non-ASCII identifier divides',
    TestNonASCIIIdentifierBeforeASlashDivides);
  Test('a slash after a string literal divides', TestStringBeforeASlashDivides);
  Test('U+2028 ends a line comment', TestLineSeparatorEndsALineComment);
  Test('a carriage return ends a line comment',
    TestCarriageReturnEndsALineComment);
  Test('substitution braces are told apart by depth',
    TestSubstitutionBracesAreTrackedByDepth);
  Test('nested template literals are tracked',
    TestNestedTemplateLiteralsAreTracked);
  Test('an unterminated template falls back to the raw scan',
    TestUnterminatedTemplateFallsBackToTheRawScan);
end;

{ ── Specifier splitting ────────────────────────────────────── }

procedure TNodeResolutionTests.TestSplitsPlainPackageName;
var
  PackageName, Subpath: string;
begin
  Expect<Boolean>(SplitBareSpecifier('zod', PackageName, Subpath)).ToBe(True);
  Expect<string>(PackageName).ToBe('zod');
  Expect<string>(Subpath).ToBe(PACKAGE_MAIN_EXPORT_KEY);
end;

procedure TNodeResolutionTests.TestSplitsPackageSubpath;
var
  PackageName, Subpath: string;
begin
  Expect<Boolean>(SplitBareSpecifier('tldts/dist/index.js', PackageName,
    Subpath)).ToBe(True);
  Expect<string>(PackageName).ToBe('tldts');
  Expect<string>(Subpath).ToBe('./dist/index.js');
end;

procedure TNodeResolutionTests.TestSplitsScopedPackageName;
var
  PackageName, Subpath: string;
begin
  Expect<Boolean>(SplitBareSpecifier('@convex-dev/workpool', PackageName,
    Subpath)).ToBe(True);
  Expect<string>(PackageName).ToBe('@convex-dev/workpool');
  Expect<string>(Subpath).ToBe(PACKAGE_MAIN_EXPORT_KEY);
end;

procedure TNodeResolutionTests.TestSplitsScopedPackageSubpath;
var
  PackageName, Subpath: string;
begin
  Expect<Boolean>(SplitBareSpecifier('@convex-dev/workpool/test', PackageName,
    Subpath)).ToBe(True);
  Expect<string>(PackageName).ToBe('@convex-dev/workpool');
  Expect<string>(Subpath).ToBe('./test');
end;

procedure TNodeResolutionTests.TestRejectsNonBareSpecifiers;
var
  PackageName, Subpath: string;
begin
  Expect<Boolean>(SplitBareSpecifier('', PackageName, Subpath)).ToBe(False);
  Expect<Boolean>(SplitBareSpecifier('./local.js', PackageName,
    Subpath)).ToBe(False);
  Expect<Boolean>(SplitBareSpecifier('../up.js', PackageName,
    Subpath)).ToBe(False);
  Expect<Boolean>(SplitBareSpecifier('/abs.js', PackageName,
    Subpath)).ToBe(False);
  Expect<Boolean>(SplitBareSpecifier('#private', PackageName,
    Subpath)).ToBe(False);
  Expect<Boolean>(SplitBareSpecifier('node:fs', PackageName,
    Subpath)).ToBe(False);
  Expect<Boolean>(SplitBareSpecifier('https://example.test/m.js', PackageName,
    Subpath)).ToBe(False);
  { A trailing slash names no subpath. }
  Expect<Boolean>(SplitBareSpecifier('zod/', PackageName, Subpath)).ToBe(False);
end;

procedure TNodeResolutionTests.TestRejectsIncompleteScope;
var
  PackageName, Subpath: string;
begin
  Expect<Boolean>(SplitBareSpecifier('@convex-dev', PackageName,
    Subpath)).ToBe(False);
end;

{ ── Manifest parsing and the exports map ───────────────────── }

procedure TNodeResolutionTests.TestReadsLegacyManifestFields;
var
  Manifest: TGocciaPackageManifest;
begin
  Manifest := ParsePackageManifest('{"name":"pkg","version":"1.2.3",' +
    '"type":"module","main":"./main.js","module":"./esm.js",' +
    '"scripts":{"build":"tsc"}}');

  Expect<string>(Manifest.Name).ToBe('pkg');
  Expect<string>(Manifest.ModuleType).ToBe('module');
  Expect<Boolean>(Manifest.IsModuleType).ToBe(True);
  Expect<string>(Manifest.Main).ToBe('./main.js');
  Expect<string>(Manifest.ModuleField).ToBe('./esm.js');
  Expect<Boolean>(Manifest.HasExports).ToBe(False);
end;

procedure TNodeResolutionTests.TestManifestWithoutExportsReportsNoExportsField;
var
  Manifest: TGocciaPackageManifest;
  Target: string;
begin
  Manifest := ParsePackageManifest('{"name":"pkg"}');

  Expect<Boolean>(Manifest.ResolveExportsSubpath(PACKAGE_MAIN_EXPORT_KEY,
    Target) = eoNoExportsField).ToBe(True);
end;

procedure TNodeResolutionTests.TestStringExportsResolvesMainEntryOnly;
var
  Manifest: TGocciaPackageManifest;
  Target: string;
begin
  Manifest := ParsePackageManifest('{"exports":"./index.js"}');

  Expect<Boolean>(Manifest.HasExports).ToBe(True);
  Expect<Boolean>(Manifest.ResolveExportsSubpath(PACKAGE_MAIN_EXPORT_KEY,
    Target) = eoResolved).ToBe(True);
  Expect<string>(Target).ToBe('./index.js');
  Expect<Boolean>(Manifest.ResolveExportsSubpath('./sub',
    Target) = eoNotExported).ToBe(True);
end;

procedure TNodeResolutionTests.TestConditionsAreTriedInDeclarationOrder;
var
  Manifest: TGocciaPackageManifest;
  Target: string;
begin
  Manifest := ParsePackageManifest(
    '{"exports":{"import":"./esm.js","default":"./fallback.js"}}');

  Expect<Boolean>(Manifest.ResolveExportsSubpath(PACKAGE_MAIN_EXPORT_KEY,
    Target) = eoResolved).ToBe(True);
  Expect<string>(Target).ToBe('./esm.js');

  { Node's PACKAGE_TARGET_RESOLVE walks the condition keys in the order the
    manifest lists them and "default" matches unconditionally, so a manifest
    that writes "default" first genuinely selects it. Packages write it last
    for exactly that reason; matching Node here is what keeps a package that
    relies on the ordering from resolving differently under this engine. }
  Manifest := ParsePackageManifest(
    '{"exports":{"default":"./fallback.js","import":"./esm.js"}}');

  Expect<Boolean>(Manifest.ResolveExportsSubpath(PACKAGE_MAIN_EXPORT_KEY,
    Target) = eoResolved).ToBe(True);
  Expect<string>(Target).ToBe('./fallback.js');
end;

procedure TNodeResolutionTests.TestUnsupportedConditionIsSkipped;
var
  Manifest: TGocciaPackageManifest;
  Target: string;
begin
  { "require" and "node" are outside the supported condition set, so the only
    selectable target is the nested "import". }
  Manifest := ParsePackageManifest(
    '{"exports":{"require":"./cjs.js","node":{"import":"./node-esm.js"},' +
    '"import":"./esm.js"}}');

  Expect<Boolean>(Manifest.ResolveExportsSubpath(PACKAGE_MAIN_EXPORT_KEY,
    Target) = eoResolved).ToBe(True);
  Expect<string>(Target).ToBe('./esm.js');
end;

procedure TNodeResolutionTests.TestSubpathMapResolvesExactKey;
var
  Manifest: TGocciaPackageManifest;
  Target: string;
begin
  Manifest := ParsePackageManifest(
    '{"exports":{".":"./index.js","./test":{"import":"./src/test.ts"}}}');

  Expect<Boolean>(Manifest.ResolveExportsSubpath('./test',
    Target) = eoResolved).ToBe(True);
  Expect<string>(Target).ToBe('./src/test.ts');
end;

procedure TNodeResolutionTests.TestWildcardPatternSubstitutesStar;
var
  Manifest: TGocciaPackageManifest;
  Target: string;
begin
  Manifest := ParsePackageManifest('{"exports":{"./*":"./src/*.ts"}}');

  Expect<Boolean>(Manifest.ResolveExportsSubpath('./test',
    Target) = eoResolved).ToBe(True);
  Expect<string>(Target).ToBe('./src/test.ts');
end;

procedure TNodeResolutionTests.TestExactKeyBeatsWildcardPattern;
var
  Manifest: TGocciaPackageManifest;
  Target: string;
begin
  Manifest := ParsePackageManifest(
    '{"exports":{"./*":"./src/*.ts","./test":"./src/pinned.js"}}');

  Expect<Boolean>(Manifest.ResolveExportsSubpath('./test',
    Target) = eoResolved).ToBe(True);
  Expect<string>(Target).ToBe('./src/pinned.js');
end;

procedure TNodeResolutionTests.TestLongestPatternBaseWins;
var
  Manifest: TGocciaPackageManifest;
  Target: string;
begin
  Manifest := ParsePackageManifest(
    '{"exports":{"./*":"./src/*.js","./deep/*":"./lib/*.js"}}');

  Expect<Boolean>(Manifest.ResolveExportsSubpath('./deep/thing',
    Target) = eoResolved).ToBe(True);
  Expect<string>(Target).ToBe('./lib/thing.js');
end;

procedure TNodeResolutionTests.TestNullTargetBlocksSubpath;
var
  Manifest: TGocciaPackageManifest;
  Target: string;
begin
  Manifest := ParsePackageManifest(
    '{"exports":{".":"./index.js","./internal":null}}');

  Expect<Boolean>(Manifest.ResolveExportsSubpath('./internal',
    Target) = eoNotExported).ToBe(True);
end;

procedure TNodeResolutionTests.TestArrayTargetFallsBackToFirstUsable;
var
  Manifest: TGocciaPackageManifest;
  Target: string;
begin
  Manifest := ParsePackageManifest(
    '{"exports":{".":[{"require":"./cjs.js"},"./esm.js"]}}');

  Expect<Boolean>(Manifest.ResolveExportsSubpath(PACKAGE_MAIN_EXPORT_KEY,
    Target) = eoResolved).ToBe(True);
  Expect<string>(Target).ToBe('./esm.js');
end;

procedure TNodeResolutionTests.TestUnlistedSubpathIsNotExported;
var
  Manifest: TGocciaPackageManifest;
  Target: string;
begin
  Manifest := ParsePackageManifest('{"exports":{".":"./index.js"}}');

  Expect<Boolean>(Manifest.ResolveExportsSubpath('./hidden',
    Target) = eoNotExported).ToBe(True);
end;

{ ── PACKAGE_TARGET_RESOLVE validation ──────────────────────── }

procedure TNodeResolutionTests.TestInvalidSegmentsAreDetected;
begin
  Expect<Boolean>(HasInvalidPathSegment('..')).ToBe(True);
  Expect<Boolean>(HasInvalidPathSegment('.')).ToBe(True);
  Expect<Boolean>(HasInvalidPathSegment('../evil.js')).ToBe(True);
  Expect<Boolean>(HasInvalidPathSegment('src/../../evil.js')).ToBe(True);
  Expect<Boolean>(HasInvalidPathSegment('src/./evil.js')).ToBe(True);
  Expect<Boolean>(HasInvalidPathSegment('node_modules/other')).ToBe(True);
  Expect<Boolean>(HasInvalidPathSegment('src/node_modules')).ToBe(True);
  { A backslash counts as a separator too, so a Windows spelling cannot slip a
    segment past a check written for forward slashes. }
  Expect<Boolean>(HasInvalidPathSegment('src\..\evil.js')).ToBe(True);
end;

procedure TNodeResolutionTests.TestValidSegmentsAreAccepted;
begin
  Expect<Boolean>(HasInvalidPathSegment('')).ToBe(False);
  Expect<Boolean>(HasInvalidPathSegment('index.js')).ToBe(False);
  Expect<Boolean>(HasInvalidPathSegment('src/deep/index.js')).ToBe(False);
  { Only a whole segment counts: these merely contain the forbidden text. }
  Expect<Boolean>(HasInvalidPathSegment('..hidden/x.js')).ToBe(False);
  Expect<Boolean>(HasInvalidPathSegment('my_node_modules/x.js')).ToBe(False);
end;

procedure TNodeResolutionTests.TestExportsTargetMustBeRelative;
var
  Manifest: TGocciaPackageManifest;
  Target: string;
begin
  Expect<Boolean>(IsValidExportsTarget('./index.js')).ToBe(True);
  Expect<Boolean>(IsValidExportsTarget('index.js')).ToBe(False);
  Expect<Boolean>(IsValidExportsTarget('/abs/index.js')).ToBe(False);
  Expect<Boolean>(IsValidExportsTarget('../outside.js')).ToBe(False);

  Manifest := ParsePackageManifest('{"exports":"index.js"}');
  Expect<Boolean>(Manifest.ResolveExportsSubpath(PACKAGE_MAIN_EXPORT_KEY,
    Target) = eoNotExported).ToBe(True);
end;

procedure TNodeResolutionTests.TestExportsTargetMayNotEscapeThePackage;
var
  Manifest: TGocciaPackageManifest;
  Target: string;
begin
  { Reviewer repro 3: a package whose whole exports value points outside. }
  Manifest := ParsePackageManifest('{"exports":"../../outside.js"}');

  Expect<Boolean>(Manifest.ResolveExportsSubpath(PACKAGE_MAIN_EXPORT_KEY,
    Target) = eoNotExported).ToBe(True);
  Expect<Boolean>(ResolvePackageSubpath(Manifest, PACKAGE_MAIN_EXPORT_KEY,
    Target)).ToBe(False);

  { The same target reached through a condition object is refused too. }
  Manifest := ParsePackageManifest(
    '{"exports":{".":{"import":"./../../outside.js"}}}');
  Expect<Boolean>(Manifest.ResolveExportsSubpath(PACKAGE_MAIN_EXPORT_KEY,
    Target) = eoNotExported).ToBe(True);
end;

procedure TNodeResolutionTests.TestPatternStarValueMayNotEscapeThePackage;
var
  Manifest: TGocciaPackageManifest;
  Target: string;
begin
  { Reviewer repro 2: the star value is the attacker-controlled half of a
    pattern resolution. It is refused rather than matched against a later
    pattern, which is what Node's ERR_INVALID_MODULE_SPECIFIER does. }
  Manifest := ParsePackageManifest('{"exports":{"./sub/*":"./src/*.ts"}}');

  Expect<Boolean>(Manifest.ResolveExportsSubpath('./sub/widen',
    Target) = eoResolved).ToBe(True);
  Expect<string>(Target).ToBe('./src/widen.ts');

  Expect<Boolean>(Manifest.ResolveExportsSubpath('./sub/../../../../evil',
    Target) = eoInvalidTarget).ToBe(True);
  Expect<Boolean>(ResolvePackageSubpath(Manifest, './sub/../../../../evil',
    Target)).ToBe(False);
end;

procedure TNodeResolutionTests.TestLegacySubpathMayNotEscapeThePackage;
var
  Manifest: TGocciaPackageManifest;
  Target: string;
begin
  { Reviewer repro 1: no exports map, so the subpath is taken literally — the
    route that walked straight out of the package before this check. }
  Manifest := ParsePackageManifest('{"main":"./lib/entry.js"}');

  Expect<Boolean>(ResolvePackageSubpath(Manifest, './lib/util',
    Target)).ToBe(True);
  Expect<string>(Target).ToBe('./lib/util');

  Expect<Boolean>(ResolvePackageSubpath(Manifest, './../../../evil.js',
    Target)).ToBe(False);
end;

procedure TNodeResolutionTests.TestNodeModulesSegmentIsRejected;
var
  Manifest: TGocciaPackageManifest;
  Target: string;
begin
  { Node rejects a node_modules segment for the same reason as `..`: it
    reaches a different package's files through this package's name. }
  Manifest := ParsePackageManifest('{"main":"./index.js"}');
  Expect<Boolean>(ResolvePackageSubpath(Manifest, './node_modules/other',
    Target)).ToBe(False);

  Manifest := ParsePackageManifest('{"exports":{"./*":"./src/*.js"}}');
  Expect<Boolean>(Manifest.ResolveExportsSubpath('./node_modules/other',
    Target) = eoInvalidTarget).ToBe(True);
end;

procedure TNodeResolutionTests.TestContainmentAcceptsPathsInsideTheDirectory;
var
  PackageDirectory: string;
begin
  PackageDirectory := IncludeTrailingPathDelimiter(GetCurrentDir) + 'pkg';

  Expect<Boolean>(IsPathInsideDirectory(
    PackageDirectory + PathDelim + 'index.js', PackageDirectory)).ToBe(True);
  Expect<Boolean>(IsPathInsideDirectory(
    PackageDirectory + PathDelim + 'src' + PathDelim + 'deep.js',
    PackageDirectory)).ToBe(True);
  { A `..` that normalizes back inside is still inside. }
  Expect<Boolean>(IsPathInsideDirectory(
    PackageDirectory + PathDelim + 'src' + PathDelim + '..' + PathDelim +
    'index.js', PackageDirectory)).ToBe(True);
end;

procedure TNodeResolutionTests.TestContainmentRejectsSiblingAndEscapedPaths;
var
  ParentDirectory, PackageDirectory: string;
begin
  ParentDirectory := IncludeTrailingPathDelimiter(GetCurrentDir);
  PackageDirectory := ParentDirectory + 'pkg';

  Expect<Boolean>(IsPathInsideDirectory(ParentDirectory + 'evil.js',
    PackageDirectory)).ToBe(False);
  Expect<Boolean>(IsPathInsideDirectory(
    PackageDirectory + PathDelim + '..' + PathDelim + 'evil.js',
    PackageDirectory)).ToBe(False);
  { A sibling whose name merely starts with the package directory's name must
    not pass a prefix comparison. }
  Expect<Boolean>(IsPathInsideDirectory(ParentDirectory + 'pkg-evil' +
    PathDelim + 'x.js', PackageDirectory)).ToBe(False);
  Expect<Boolean>(IsPathInsideDirectory('', PackageDirectory)).ToBe(False);
end;

{ ── Physical containment ───────────────────────────────────── }

procedure TNodeResolutionTests.TestPhysicalContainmentRejectsAnEscapingChildLink;
var
  Candidate, PackageDirectory, Root: string;
begin
  { The shape a package would ship to escape itself: a link whose name sits
    inside the package and whose target does not. The lexical check passes it,
    which is exactly why the physical one exists. }
  Root := IncludeTrailingPathDelimiter(CreateTempDirectory);
  PackageDirectory := Root + 'pkg';
  WriteFixtureFile(Root + 'outside.js', 'export const secret = 1;');
  ForceDirectories(PackageDirectory + PathDelim + 'linked');
  Candidate := PackageDirectory + PathDelim + 'linked' + PathDelim + 'out.js';

  Expect<Boolean>(TryCreateSymlink('..' + PathDelim + '..' + PathDelim +
    'outside.js', Candidate)).ToBe(True);

  Expect<Boolean>(IsPathInsideDirectory(Candidate,
    PackageDirectory)).ToBe(True);
  Expect<Boolean>(IsPathPhysicallyInsideDirectory(Candidate,
    PackageDirectory)).ToBe(False);
end;

procedure TNodeResolutionTests.TestPhysicalContainmentAllowsAnInPackageLink;
var
  Candidate, PackageDirectory, Root: string;
begin
  { A package is free to link to its own files, and plenty do. Only leaving the
    package is refused. }
  Root := IncludeTrailingPathDelimiter(CreateTempDirectory);
  PackageDirectory := Root + 'pkg';
  WriteFixtureFile(PackageDirectory + PathDelim + 'src' + PathDelim +
    'real.js', 'export const value = 1;');
  Candidate := PackageDirectory + PathDelim + 'alias.js';

  Expect<Boolean>(TryCreateSymlink('src' + PathDelim + 'real.js',
    Candidate)).ToBe(True);

  Expect<Boolean>(IsPathPhysicallyInsideDirectory(Candidate,
    PackageDirectory)).ToBe(True);
end;

procedure TNodeResolutionTests.TestPhysicalContainmentAllowsAStoreLinkedPackageRoot;
var
  Candidate, PackageDirectory, Root, StorePackage: string;
begin
  { pnpm's layout: node_modules/<pkg> is itself a link into a
    content-addressed store, and the package's real files live there.
    Canonicalizing the candidate alone would put every one of them outside
    their own package; canonicalizing the root too moves the comparison into
    the store, where they belong. }
  Root := IncludeTrailingPathDelimiter(CreateTempDirectory);
  StorePackage := Root + 'store' + PathDelim + 'pkg@1.0.0' + PathDelim +
    'node_modules' + PathDelim + 'pkg';
  WriteFixtureFile(StorePackage + PathDelim + 'index.js',
    'export const value = 1;');
  WriteFixtureFile(StorePackage + PathDelim + 'lib' + PathDelim + 'deep.js',
    'export const deep = 1;');
  ForceDirectories(Root + 'node_modules');
  PackageDirectory := Root + 'node_modules' + PathDelim + 'pkg';

  Expect<Boolean>(TryCreateSymlink(StorePackage, PackageDirectory)).ToBe(True);

  Candidate := PackageDirectory + PathDelim + 'index.js';
  Expect<Boolean>(IsPathPhysicallyInsideDirectory(Candidate,
    PackageDirectory)).ToBe(True);

  Candidate := PackageDirectory + PathDelim + 'lib' + PathDelim + 'deep.js';
  Expect<Boolean>(IsPathPhysicallyInsideDirectory(Candidate,
    PackageDirectory)).ToBe(True);
end;

procedure TNodeResolutionTests.TestPhysicalContainmentRejectsAnEscapeFromALinkedRoot;
var
  Candidate, PackageDirectory, Root, StorePackage: string;
begin
  { The two links composed: a store-linked package root whose store copy ships
    an escaping child. Following only one of them would let this through. }
  Root := IncludeTrailingPathDelimiter(CreateTempDirectory);
  StorePackage := Root + 'store' + PathDelim + 'pkg@1.0.0' + PathDelim +
    'node_modules' + PathDelim + 'pkg';
  WriteFixtureFile(StorePackage + PathDelim + 'index.js',
    'export const value = 1;');
  WriteFixtureFile(Root + 'store' + PathDelim + 'other.js',
    'export const secret = 1;');
  ForceDirectories(Root + 'node_modules');
  PackageDirectory := Root + 'node_modules' + PathDelim + 'pkg';

  Expect<Boolean>(TryCreateSymlink(StorePackage, PackageDirectory)).ToBe(True);
  Expect<Boolean>(TryCreateSymlink('..' + PathDelim + '..' + PathDelim + '..' +
    PathDelim + 'other.js', StorePackage + PathDelim + 'escape.js')).ToBe(True);

  Candidate := PackageDirectory + PathDelim + 'escape.js';
  Expect<Boolean>(IsPathInsideDirectory(Candidate,
    PackageDirectory)).ToBe(True);
  Expect<Boolean>(IsPathPhysicallyInsideDirectory(Candidate,
    PackageDirectory)).ToBe(False);
end;

{ ── Target selection ───────────────────────────────────────── }

procedure TNodeResolutionTests.TestModuleFieldIsPreferredOverMain;
var
  Manifest: TGocciaPackageManifest;
  Target: string;
begin
  Manifest := ParsePackageManifest(
    '{"main":"./main.cjs","module":"./esm.js"}');

  Expect<Boolean>(ResolvePackageSubpath(Manifest, PACKAGE_MAIN_EXPORT_KEY,
    Target)).ToBe(True);
  Expect<string>(Target).ToBe('./esm.js');
end;

procedure TNodeResolutionTests.TestMainIsUsedWhenNoModuleField;
var
  Manifest: TGocciaPackageManifest;
  Target: string;
begin
  Manifest := ParsePackageManifest('{"main":"./lib/entry.js"}');

  Expect<Boolean>(ResolvePackageSubpath(Manifest, PACKAGE_MAIN_EXPORT_KEY,
    Target)).ToBe(True);
  Expect<string>(Target).ToBe('./lib/entry.js');
end;

procedure TNodeResolutionTests.TestIndexIsTheLastResort;
var
  Manifest: TGocciaPackageManifest;
  Target: string;
begin
  Manifest := ParsePackageManifest('{"name":"pkg"}');

  Expect<Boolean>(ResolvePackageSubpath(Manifest, PACKAGE_MAIN_EXPORT_KEY,
    Target)).ToBe(True);
  Expect<string>(Target).ToBe('./index');
end;

procedure TNodeResolutionTests.TestSubpathIsLiteralWithoutExports;
var
  Manifest: TGocciaPackageManifest;
  Target: string;
begin
  Manifest := ParsePackageManifest('{"main":"./index.js"}');

  Expect<Boolean>(ResolvePackageSubpath(Manifest, './dist/util',
    Target)).ToBe(True);
  Expect<string>(Target).ToBe('./dist/util');
end;

procedure TNodeResolutionTests.TestExportsMapSuppressesLegacyFields;
var
  Manifest: TGocciaPackageManifest;
  Target: string;
begin
  Manifest := ParsePackageManifest(
    '{"main":"./main.js","module":"./esm.js","exports":{".":"./exported.js"}}');

  Expect<Boolean>(ResolvePackageSubpath(Manifest, PACKAGE_MAIN_EXPORT_KEY,
    Target)).ToBe(True);
  Expect<string>(Target).ToBe('./exported.js');
  Expect<Boolean>(ResolvePackageSubpath(Manifest, './anything',
    Target)).ToBe(False);
end;

{ ── CommonJS heuristic ─────────────────────────────────────── }

procedure TNodeResolutionTests.TestRequireCallLooksLikeCommonJS;
begin
  Expect<Boolean>(LooksLikeCommonJSSource(
    'const os = require("node:os");' + sLineBreak +
    'module.exports = { os };')).ToBe(True);
end;

procedure TNodeResolutionTests.TestModuleExportsLooksLikeCommonJS;
begin
  Expect<Boolean>(LooksLikeCommonJSSource(
    'module.exports = { value: 1 };')).ToBe(True);
  Expect<Boolean>(LooksLikeCommonJSSource(
    'exports.value = 1;')).ToBe(True);
end;

procedure TNodeResolutionTests.TestESModuleSourceDoesNotLookLikeCommonJS;
begin
  Expect<Boolean>(LooksLikeCommonJSSource(
    'import { a } from "./a.js";' + sLineBreak +
    'export const b = a;')).ToBe(False);
end;

procedure TNodeResolutionTests.TestMixedSourceIsReadAsESModule;
begin
  { Interop shims that call require from an ES module are still ES modules —
    the refusal is for files with no ES module surface at all. }
  Expect<Boolean>(LooksLikeCommonJSSource(
    'const legacy = require("./legacy.js");' + sLineBreak +
    'export const value = legacy;')).ToBe(False);
end;

procedure TNodeResolutionTests.TestMinifiedSideEffectImportIsReadAsESModule;
begin
  { A minifier emits a space-free side-effect import (`import"./a.js";`). The
    scan strips the string literal to a placeholder before matching, so the
    keyword follower set must accept the placeholder or this ES module — whose
    only ES marker is that import — is misclassified as CommonJS and refused. }
  Expect<Boolean>(LooksLikeCommonJSSource(
    'import"./polyfill.js";const x=require("y");')).ToBe(False);
end;

procedure TNodeResolutionTests.TestInertSourceIsNotCommonJS;
begin
  Expect<Boolean>(LooksLikeCommonJSSource('')).ToBe(False);
  Expect<Boolean>(LooksLikeCommonJSSource(
    'const value = 1;')).ToBe(False);
end;

procedure TNodeResolutionTests.TestIdentifierEndingInRequireIsNotACall;
begin
  Expect<Boolean>(LooksLikeCommonJSSource(
    'const value = createRequire(import.meta.url);')).ToBe(False);
end;

{ ── Comment and literal stripping ──────────────────────────── }

procedure TNodeResolutionTests.TestEsbuildBannerIsNotAnESModuleMarker;
begin
  { The banner every esbuild __toCommonJS bundle carries. Its bare "export" and
    "import" words used to make the bundle pass as an ES module, so it was
    loaded instead of refused and failed at its first require. }
  Expect<Boolean>(LooksLikeCommonJSSource(
    'var __toCommonJS = (mod) => __copyProps(__defProp({}, "__esModule", ' +
      '{ value: true }), mod);' + sLineBreak +
    'var index_exports = {};' + sLineBreak +
    'module.exports = __toCommonJS(index_exports);' + sLineBreak +
    '// Annotate the CommonJS export names for ESM import in node:' +
      sLineBreak +
    '0 && (module.exports = {' + sLineBreak +
    '  getToken' + sLineBreak +
    '});')).ToBe(True);
end;

procedure TNodeResolutionTests.TestStringLiteralMarkerIsNotAnESModuleMarker;
begin
  Expect<Boolean>(LooksLikeCommonJSSource(
    'const hint = "run: import x from ''y''";' + sLineBreak +
    'module.exports = { hint };')).ToBe(True);
end;

procedure TNodeResolutionTests.TestBlockCommentSpanningLinesIsStripped;
begin
  Expect<Boolean>(LooksLikeCommonJSSource(
    '/*' + sLineBreak +
    ' * Historic note: this file used to read' + sLineBreak +
    ' *   export const value = 1;' + sLineBreak +
    ' */' + sLineBreak +
    'module.exports = { value: 1 };')).ToBe(True);
end;

procedure TNodeResolutionTests.TestTemplateBodyIsStrippedButSubstitutionsAreNot;
begin
  Expect<Boolean>(LooksLikeCommonJSSource(
    'module.exports = {' + sLineBreak +
    '  banner: `' + sLineBreak +
    '    import { x } from "y";' + sLineBreak +
    '    export const z = x;' + sLineBreak +
    '  `,' + sLineBreak +
    '};')).ToBe(True);
  { A substitution holds real code, so what is inside it still counts. }
  Expect<Boolean>(LooksLikeCommonJSSource(
    'const label = `name: ${exports.name}`;')).ToBe(True);
end;

procedure TNodeResolutionTests.TestCommentedESModuleStaysAnESModule;
begin
  { The strip may not eat code: the require before the comments and the export
    after them are both real, and a file carrying both is an ES module. }
  Expect<Boolean>(LooksLikeCommonJSSource(
    'const legacy = require("./legacy.cjs");' + sLineBreak +
    '/* interop shim for the legacy build */' + sLineBreak +
    '// exports.legacy = legacy;' + sLineBreak +
    'export const value = legacy;')).ToBe(False);
end;

procedure TNodeResolutionTests.TestRegExpLiteralDoesNotSwallowCode;
begin
  { The quotes and the escaped slash pair inside the literal must not be read
    as a string or a comment; the export after it decides the file. }
  Expect<Boolean>(LooksLikeCommonJSSource(
    'const legacy = require("./legacy.cjs");' + sLineBreak +
    'const pattern = /["'']\/\//g;' + sLineBreak +
    'export const value = legacy.replace(pattern, "");')).ToBe(False);
end;

procedure TNodeResolutionTests.TestUnterminatedCommentFallsBackToTheRawScan;
begin
  { A source the scan cannot finish is classified on its raw text, so the words
    inside the unclosed comment count again. The file is loaded rather than
    refused, which is the safe direction of the asymmetry. }
  Expect<Boolean>(LooksLikeCommonJSSource(
    'module.exports = { value: 1 };' + sLineBreak +
    '/* export * from "./value.js";')).ToBe(False);
end;

procedure TNodeResolutionTests.TestNonASCIIIdentifierBeforeASlashDivides;
begin
  { `café` is a value, so the slash after it divides. Reading it as an operand
    position would open a regular expression that closes on the next slash on
    the line, taking the export with it and refusing a genuine ES module. }
  Expect<Boolean>(LooksLikeCommonJSSource(
    'const café = 4; const x = café/2; export const v = 1; ' +
    'const y = 6/3; module.exports = x;')).ToBe(False);
end;

procedure TNodeResolutionTests.TestStringBeforeASlashDivides;
begin
  { A stripped literal leaves a value behind, so the slash after it divides for
    the same reason. On a placeholder the scan could see past, the literal
    opened here would run to the slash in `1/2` and swallow the export. }
  Expect<Boolean>(LooksLikeCommonJSSource(
    'var a = "p"/2;export { a };var b = 1/2;module.exports = b;')).ToBe(False);
end;

procedure TNodeResolutionTests.TestLineSeparatorEndsALineComment;
begin
  { U+2028 is an ECMAScript line terminator, so the banner comment ends there
    and the code on the next line is still code. }
  Expect<Boolean>(LooksLikeCommonJSSource(
    '// Annotate the CommonJS export names for ESM import in node:' +
    #$2028 + 'module.exports = { value: 1 };')).ToBe(True);
end;

procedure TNodeResolutionTests.TestCarriageReturnEndsALineComment;
begin
  Expect<Boolean>(LooksLikeCommonJSSource(
    '// Annotate the CommonJS export names for ESM import in node:' +
    #13#10 + 'module.exports = { value: 1 };')).ToBe(True);
end;

procedure TNodeResolutionTests.TestSubstitutionBracesAreTrackedByDepth;
begin
  { The closing brace of the object literal is not the one that ends the
    substitution. Counting templates instead of brace depth ends it early, and
    the rest of the substitution — the only CommonJS marker here — is read as
    template text and stripped. }
  Expect<Boolean>(LooksLikeCommonJSSource(
    'const label = `${ format({ width: 2 }, exports.name) } ready`;'))
    .ToBe(True);
end;

procedure TNodeResolutionTests.TestNestedTemplateLiteralsAreTracked;
begin
  { A substitution inside a substitution is still code. }
  Expect<Boolean>(LooksLikeCommonJSSource(
    'const label = `outer ${ wrap(`inner ${ exports.name }`) } done`;'))
    .ToBe(True);
  { The body of the nested template is text, so the marker in it is not one. }
  Expect<Boolean>(LooksLikeCommonJSSource(
    'module.exports = `a ${ wrap(`export const b = 1;`) } c`;')).ToBe(True);
end;

procedure TNodeResolutionTests.TestUnterminatedTemplateFallsBackToTheRawScan;
begin
  Expect<Boolean>(LooksLikeCommonJSSource(
    'module.exports = { value: 1 };' + sLineBreak +
    'const banner = `export * from "./value.js";')).ToBe(False);
end;

begin
  TestRunnerProgram.AddSuite(TNodeResolutionTests.Create('NodeResolution'));
  RunGocciaTests;

  ExitCode := TestResultToExitCode;
end.
