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

begin
  TestRunnerProgram.AddSuite(TNodeResolutionTests.Create('NodeResolution'));
  RunGocciaTests;

  ExitCode := TestResultToExitCode;
end.
