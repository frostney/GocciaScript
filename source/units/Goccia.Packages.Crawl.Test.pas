program Goccia.Packages.Crawl.Test;

{$I Goccia.inc}

uses
  Classes,
  SysUtils,

  TestingPascalLibrary,
  TextEncoding,

  Goccia.Packages.Crawl,
  Goccia.TestSetup;

type
  TCrawlTests = class(TTestSuite)
  private
    FFiles: TStringList;
    FRequests: TStringList;
    function Fetch(const APath: string; out ABytes: TBytes): Boolean;
    function Describe(const AReferences: TGocciaLiteralReferences): string;
    function Crawl(const AEntries: array of string): string;
    function CrawlError(const AEntries: array of string): string;
    procedure TestExtractsStaticAndReexports;
    procedure TestExtractsDynamicImportsAndAttributes;
    procedure TestExtractsAssets;
    procedure TestIgnoresNonLiteralAndLookalikes;
    procedure TestTypeScriptAndJSX;
    procedure TestJoinsRelativePaths;
    procedure TestCrawlFollowsModulesDataAndAssets;
    procedure TestCrawlProbesExtensionsAndIndex;
    procedure TestCrawlRefusesNonRelativeImports;
    procedure TestCrawlRefusesEscapesAndConfigFiles;
    procedure TestCrawlReportsMissingFiles;
    procedure TestRepeatedImportsAreFetchedOnce;
    procedure TestRequestsAreCapped;
    procedure TestAssetsAndDataAreNotParsed;
    procedure TestKnownFilesSettleCandidates;
    procedure TestAModuleFirstSeenAsAnAssetIsFollowed;
  protected
    procedure BeforeEach; override;
    procedure AfterEach; override;
  public
    procedure SetupTests; override;
  end;

procedure TCrawlTests.SetupTests;
begin
  Test('Extracts static imports and re-exports', TestExtractsStaticAndReexports);
  Test('Extracts dynamic imports and attribute imports',
    TestExtractsDynamicImportsAndAttributes);
  Test('Extracts new URL and import.meta.resolve assets', TestExtractsAssets);
  Test('Ignores computed specifiers and lookalikes',
    TestIgnoresNonLiteralAndLookalikes);
  Test('Reads TypeScript and JSX sources', TestTypeScriptAndJSX);
  Test('Joins relative paths inside the repository', TestJoinsRelativePaths);
  Test('Follows modules, data, and assets',
    TestCrawlFollowsModulesDataAndAssets);
  Test('Probes extensions and index files', TestCrawlProbesExtensionsAndIndex);
  Test('Refuses bare, absolute, URL, and github: imports',
    TestCrawlRefusesNonRelativeImports);
  Test('Refuses escapes and goccia.* files',
    TestCrawlRefusesEscapesAndConfigFiles);
  Test('Reports a missing file', TestCrawlReportsMissingFiles);
  Test('Repeated imports are fetched once, and misses asked once',
    TestRepeatedImportsAreFetchedOnce);
  Test('A crawl stops at its request cap', TestRequestsAreCapped);
  Test('Assets and data files are pinned but never parsed',
    TestAssetsAndDataAreNotParsed);
  Test('A known file set settles candidates without requests',
    TestKnownFilesSettleCandidates);
  Test('A file first reached as an asset is followed when imported',
    TestAModuleFirstSeenAsAnAssetIsFollowed);
end;

procedure TCrawlTests.BeforeEach;
begin
  inherited BeforeEach;
  FFiles := TStringList.Create;
  FRequests := TStringList.Create;
end;

procedure TCrawlTests.AfterEach;
begin
  FRequests.Free;
  FFiles.Free;
  inherited AfterEach;
end;

function TCrawlTests.Fetch(const APath: string; out ABytes: TBytes): Boolean;
var
  ErrorOffset, Index: Integer;
begin
  FRequests.Add(APath);
  Index := FFiles.IndexOfName(APath);
  Result := Index >= 0;
  if Result then
    TryEncodeUTF8(FFiles.ValueFromIndex[Index], ABytes, ErrorOffset)
  else
    ABytes := nil;
end;

function TCrawlTests.Describe(
  const AReferences: TGocciaLiteralReferences): string;
const
  KIND_NAMES: array[TGocciaReferenceKind] of string = ('module', 'data',
    'asset');
var
  I: Integer;
begin
  Result := '';
  for I := 0 to High(AReferences) do
  begin
    if Result <> '' then
      Result := Result + ' ';
    Result := Result + KIND_NAMES[AReferences[I].Kind] + ':' +
      AReferences[I].Specifier;
  end;
end;

function TCrawlTests.Crawl(const AEntries: array of string): string;
var
  Crawler: TGocciaPackageCrawl;
  I: Integer;
begin
  Crawler := TGocciaPackageCrawl.Create('github:o/r@v1', Fetch);
  try
    for I := Low(AEntries) to High(AEntries) do
      Crawler.AddModuleEntry(AEntries[I]);
    Crawler.Run;
    Result := '';
    for I := 0 to Crawler.Files.Count - 1 do
    begin
      if Result <> '' then
        Result := Result + ' ';
      Result := Result + Crawler.Files[I].Path;
    end;
  finally
    Crawler.Free;
  end;
end;

function TCrawlTests.CrawlError(const AEntries: array of string): string;
begin
  Result := '';
  try
    Crawl(AEntries);
  except
    on E: EGocciaCrawlError do
      Result := E.Message;
  end;
end;

procedure TCrawlTests.TestExtractsStaticAndReexports;
begin
  Expect<string>(Describe(ExtractLiteralReferences(
    'import a from "./a.js";' + sLineBreak +
    'import { b, c as d } from "./b.js";' + sLineBreak +
    'import * as e from "./e.js";' + sLineBreak +
    'import f, { g } from "./f.js";' + sLineBreak +
    'import "./side.js";' + sLineBreak +
    'export * from "./star.js";' + sLineBreak +
    'export * as ns from "./ns.js";' + sLineBreak +
    'export { h } from "./h.js";' + sLineBreak +
    'export { i };' + sLineBreak +
    'const i = 1;', 'mod.js'))).ToBe(
    'module:./a.js module:./b.js module:./e.js module:./f.js ' +
    'module:./side.js module:./star.js module:./ns.js module:./h.js');
end;

procedure TCrawlTests.TestExtractsDynamicImportsAndAttributes;
begin
  Expect<string>(Describe(ExtractLiteralReferences(
    'import data from "./d.json" with { type: "json" };' + sLineBreak +
    'import text from "./t.txt" with { "type": "text" };' + sLineBreak +
    'export { default as raw } from "./r.bin" with { type: "bytes" };' +
    sLineBreak +
    'const m = await import("./dyn.js");' + sLineBreak +
    'const j = await import("./dyn.json", { with: { type: "json" } });',
    'mod.js'))).ToBe(
    'data:./d.json data:./t.txt data:./r.bin module:./dyn.js data:./dyn.json');
end;

procedure TCrawlTests.TestExtractsAssets;
begin
  Expect<string>(Describe(ExtractLiteralReferences(
    'const lib = new URL("../native/lib.so", import.meta.url);' + sLineBreak +
    'const other = import.meta.resolve("./helper.wasm");' + sLineBreak +
    'const remote = new URL("https://example.com/x", import.meta.url);',
    'mod.js'))).ToBe(
    'asset:../native/lib.so asset:./helper.wasm asset:https://example.com/x');
end;

procedure TCrawlTests.TestIgnoresNonLiteralAndLookalikes;
begin
  Expect<string>(Describe(ExtractLiteralReferences(
    'const name = "./x.js";' + sLineBreak +
    'await import(name);' + sLineBreak +
    'await import("./a" + ".js");' + sLineBreak +
    'const s = ''import "./no.js"'';' + sLineBreak +
    '// import "./comment.js";' + sLineBreak +
    '/* export * from "./block.js"; */' + sLineBreak +
    'const t = `import "./template.js"`;' + sLineBreak +
    'const r = /import "\.\/regex.js"/;' + sLineBreak +
    'const o = { import: 1 }; o.import;' + sLineBreak +
    'new URL("./computed", base);',
    'mod.js'))).ToBe('');
end;

procedure TCrawlTests.TestTypeScriptAndJSX;
begin
  Expect<string>(Describe(ExtractLiteralReferences(
    'import type { T } from "./types.ts";' + sLineBreak +
    'import { v } from "./v.ts";' + sLineBreak +
    'export const f = (x: number): T => x as unknown as T;',
    'mod.ts'))).ToBe('module:./v.ts');
  Expect<string>(Describe(ExtractLiteralReferences(
    'import { C } from "./c.jsx";' + sLineBreak +
    'export const e = <C name="x" />;',
    'mod.jsx'))).ToBe('module:./c.jsx');
end;

procedure TCrawlTests.TestJoinsRelativePaths;
var
  Path: string;
begin
  Expect<Boolean>(TryJoinPackagePath('a/b/c.ts', './d.ts', Path)).ToBe(True);
  Expect<string>(Path).ToBe('a/b/d.ts');
  Expect<Boolean>(TryJoinPackagePath('a/b/c.ts', '../../d.ts', Path))
    .ToBe(True);
  Expect<string>(Path).ToBe('d.ts');
  Expect<Boolean>(TryJoinPackagePath('a/b/c.ts', '../x/./y//z.ts', Path))
    .ToBe(True);
  Expect<string>(Path).ToBe('a/x/y/z.ts');
  Expect<Boolean>(TryJoinPackagePath('c.ts', './dir/', Path)).ToBe(True);
  Expect<string>(Path).ToBe('dir/');
  Expect<Boolean>(TryJoinPackagePath('a/c.ts', '../../d.ts', Path))
    .ToBe(False);
end;

procedure TCrawlTests.TestCrawlFollowsModulesDataAndAssets;
begin
  FFiles.Values['bindings/raylib.ts'] :=
    'import { s } from "./lib/structs.ts";' + sLineBreak +
    'import data from "../vendor/raylib.json" with { type: "json" };' +
    sLineBreak +
    'export const lib = new URL("../native/linux/libraylib.so", ' +
    'import.meta.url);' + sLineBreak +
    'export const win = new URL("../native/windows/raylib.dll", ' +
    'import.meta.url);';
  FFiles.Values['bindings/lib/structs.ts'] :=
    'import { a } from "../raylib.ts"; export const s = 1;';
  FFiles.Values['vendor/raylib.json'] := '{"x": "import \"./no.js\""}';
  FFiles.Values['native/linux/libraylib.so'] := 'elf';
  FFiles.Values['native/windows/raylib.dll'] := 'pe';
  FFiles.Values['unreached.ts'] := 'export {};';
  Expect<string>(Crawl(['bindings/raylib.ts'])).ToBe(
    'bindings/lib/structs.ts bindings/raylib.ts native/linux/libraylib.so ' +
    'native/windows/raylib.dll vendor/raylib.json');
end;

procedure TCrawlTests.TestCrawlProbesExtensionsAndIndex;
begin
  FFiles.Values['index.js'] := 'import "./util"; import "./dir";';
  FFiles.Values['util.ts'] := 'export {};';
  FFiles.Values['dir/index.js'] := 'export {};';
  Expect<string>(Crawl([''])).ToBe('dir/index.js index.js util.ts');
  { The candidates were asked for in the resolver's order. }
  Expect<Boolean>(FRequests.IndexOf('util') <
    FRequests.IndexOf('util.js')).ToBe(True);
  Expect<Boolean>(FRequests.IndexOf('util.js') <
    FRequests.IndexOf('util.ts')).ToBe(True);
end;

procedure TCrawlTests.TestCrawlRefusesNonRelativeImports;
const
  SPECIFIERS: array[0..4] of string = ('lodash', '/etc/x.js',
    'https://example.com/x.js', 'github:o/other@v1/x.ts', 'file:///x.js');
var
  I: Integer;
begin
  for I := Low(SPECIFIERS) to High(SPECIFIERS) do
  begin
    FFiles.Clear;
    FFiles.Values['m.js'] := 'import "' + SPECIFIERS[I] + '";';
    Expect<Boolean>(Pos('a package may import only its own files',
      CrawlError(['m.js'])) > 0).ToBe(True);
  end;
end;

procedure TCrawlTests.TestCrawlRefusesEscapesAndConfigFiles;
begin
  FFiles.Values['m.js'] := 'import "../outside.js";';
  Expect<Boolean>(Pos('leaves the repository', CrawlError(['m.js'])) > 0)
    .ToBe(True);
  FFiles.Clear;
  FFiles.Values['m.js'] :=
    'import c from "./goccia.json" with { type: "json" };';
  FFiles.Values['goccia.json'] := '{}';
  Expect<Boolean>(Pos('goccia.* file', CrawlError(['m.js'])) > 0).ToBe(True);
end;

procedure TCrawlTests.TestCrawlReportsMissingFiles;
begin
  FFiles.Values['m.js'] := 'import "./missing.js";';
  Expect<Boolean>(Pos('names "./missing.js", which does not exist',
    CrawlError(['m.js'])) > 0).ToBe(True);
  Expect<Boolean>(Pos('has no file for "absent.ts"',
    CrawlError(['absent.ts'])) > 0).ToBe(True);
end;

procedure TCrawlTests.TestRepeatedImportsAreFetchedOnce;
var
  Source: string;
  I: Integer;
begin
  Source := '';
  for I := 1 to 200 do
    Source := Source + 'import "./d";' + sLineBreak;
  FFiles.Values['bindings/amp.js'] := Source;
  FFiles.Values['bindings/other.js'] := Source;
  FFiles.Values['bindings/d/index.js'] := 'export {};';
  Expect<string>(Crawl(['bindings/amp.js', 'bindings/other.js'])).ToBe(
    'bindings/amp.js bindings/d/index.js bindings/other.js');
  { The two entries, then ./d once: its path, its nine extensions, and
    index.js, whatever number of modules import it. }
  Expect<Integer>(FRequests.Count).ToBe(13);
end;

procedure TCrawlTests.TestRequestsAreCapped;
var
  Source: string;
  I: Integer;
begin
  { Every module is found only at its last candidate, index.md. }
  Source := '';
  for I := 1 to 600 do
  begin
    Source := Source + 'import "./m' + IntToStr(I) + '";' + sLineBreak;
    FFiles.Values['bindings/m' + IntToStr(I) + '/index.md'] := '# m';
  end;
  FFiles.Values['bindings/many.js'] := Source;
  Expect<Boolean>(Pos('needs more than 10000 requests',
    CrawlError(['bindings/many.js'])) > 0).ToBe(True);
  Expect<Integer>(FRequests.Count).ToBe(MAX_CRAWL_REQUESTS);
end;

procedure TCrawlTests.TestAssetsAndDataAreNotParsed;
begin
  FFiles.Values['m.js'] :=
    'export const u = new URL("./w.js", import.meta.url);' + sLineBreak +
    'import d from "./d.js" with { type: "text" };';
  FFiles.Values['w.js'] := 'import "lodash";';
  FFiles.Values['d.js'] := 'import "../escape.js";';
  Expect<string>(Crawl(['m.js'])).ToBe('d.js m.js w.js');
end;

procedure TCrawlTests.TestKnownFilesSettleCandidates;
var
  Crawler: TGocciaPackageCrawl;
  Refused: Boolean;
begin
  FFiles.Values['index.js'] := 'import "./util";';
  FFiles.Values['util'] := 'a file named like the stem';
  FFiles.Values['util.ts'] := 'export {};';
  Crawler := TGocciaPackageCrawl.Create('github:o/r@v1', Fetch);
  try
    { As a run resolves: the pinned util.ts, though util exists too. }
    Crawler.SetKnownFiles(['index.js', 'util.ts']);
    Crawler.AddModuleEntry('index.js');
    Crawler.Run;
    Expect<Integer>(Crawler.Files.Count).ToBe(2);
    Expect<string>(Crawler.Files[1].Path).ToBe('util.ts');
    Expect<Integer>(FRequests.Count).ToBe(2);
  finally
    Crawler.Free;
  end;
  Crawler := TGocciaPackageCrawl.Create('github:o/r@v1', Fetch);
  try
    Crawler.SetKnownFiles(['index.js']);
    Crawler.AddModuleEntry('index.js');
    Refused := False;
    try
      Crawler.Run;
    except
      on EGocciaCrawlNeedsFetch do
        Refused := True;
    end;
    Expect<Boolean>(Refused).ToBe(True);
  finally
    Crawler.Free;
  end;
end;

procedure TCrawlTests.TestAModuleFirstSeenAsAnAssetIsFollowed;
begin
  FFiles.Values['amp.js'] :=
    'export const u = new URL("./h.js", import.meta.url);' + sLineBreak +
    'import { d } from "./h.js";';
  FFiles.Values['h.js'] := 'export { d } from "./dep.js";';
  FFiles.Values['dep.js'] := 'export const d = 1;';
  Expect<string>(Crawl(['amp.js'])).ToBe('amp.js dep.js h.js');
  { Data first, then a module import, works the same way. }
  FFiles.Clear;
  FFiles.Values['m.js'] :=
    'import t from "./h.js" with { type: "text" };' + sLineBreak +
    'import "./h.js";';
  FFiles.Values['h.js'] := 'export { d } from "./dep.js";';
  FFiles.Values['dep.js'] := 'export const d = 1;';
  Expect<string>(Crawl(['m.js'])).ToBe('dep.js h.js m.js');
end;

begin
  TestRunnerProgram.AddSuite(TCrawlTests.Create('Provider package crawl'));
  RunGocciaTests;

  ExitCode := TestResultToExitCode;
end.
