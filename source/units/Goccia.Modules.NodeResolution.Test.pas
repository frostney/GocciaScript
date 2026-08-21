program Goccia.Modules.NodeResolution.Test;

{$I Goccia.inc}

uses
  SysUtils,

  TestingPascalLibrary,

  Goccia.Modules.NodeResolution,
  Goccia.TestSetup;

type
  TNodeResolutionTests = class(TTestSuite)
  private
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

    procedure TestModuleFieldIsPreferredOverMain;
    procedure TestMainIsUsedWhenNoModuleField;
    procedure TestIndexIsTheLastResort;
    procedure TestSubpathIsLiteralWithoutExports;
    procedure TestExportsMapSuppressesLegacyFields;

    procedure TestRequireCallLooksLikeCommonJS;
    procedure TestModuleExportsLooksLikeCommonJS;
    procedure TestESModuleSourceDoesNotLookLikeCommonJS;
    procedure TestMixedSourceIsReadAsESModule;
    procedure TestInertSourceIsNotCommonJS;
    procedure TestIdentifierEndingInRequireIsNotACall;

    procedure TestEsbuildBannerIsNotAnESModuleMarker;
    procedure TestStringLiteralMarkerIsNotAnESModuleMarker;
    procedure TestBlockCommentSpanningLinesIsStripped;
    procedure TestTemplateBodyIsStrippedButSubstitutionsAreNot;
    procedure TestCommentedESModuleStaysAnESModule;
    procedure TestRegExpLiteralDoesNotSwallowCode;
    procedure TestUnterminatedCommentFallsBackToTheRawScan;
  public
    procedure SetupTests; override;
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

begin
  TestRunnerProgram.AddSuite(TNodeResolutionTests.Create('NodeResolution'));
  RunGocciaTests;

  ExitCode := TestResultToExitCode;
end.
