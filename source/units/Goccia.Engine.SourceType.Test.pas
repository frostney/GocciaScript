program Goccia.Engine.SourceType.Test;

{ ADR 0140: an entry whose goal is not named is a Module when a top-level
  statement is an import or export declaration, and a Script otherwise. The
  engine's SourceType is how an embedder names the goal instead. }

{$I Goccia.inc}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes,
  SysUtils,

  TestingPascalLibrary,

  Goccia.Engine,
  Goccia.Error,
  Goccia.SourcePipeline,
  Goccia.TestSetup,
  Goccia.Values.Primitives;

type
  TEngineSourceTypeTests = class(TTestSuite)
  private
    function ParsedSourceType(const ASource: string;
      const ASourceType: TGocciaSourceType;
      const ACompatibility: TGocciaCompatibilityFlags = []): TGocciaSourceType;
    function ParseError(const ASource: string;
      const ASourceType: TGocciaSourceType;
      const ACompatibility: TGocciaCompatibilityFlags = []): string;
    function RunEntry(const AFileName, ASource: string; const AOverride: Boolean;
      const ASourceType: TGocciaSourceType;
      out ASourceTypeAfterRun: TGocciaSourceType): string;
    procedure TestImportDeclarationIsModule;
    procedure TestExportDeclarationIsModule;
    procedure TestNoModuleDeclarationIsScript;
    procedure TestDynamicImportAloneIsScript;
    procedure TestImportMetaAloneIsAScriptError;
    procedure TestImportMetaBeforeAnExportIsModule;
    procedure TestWithInADetectedModuleIsAStrictModeError;
    procedure TestWithInADetectedSloppyModuleIsAStrictModeError;
    procedure TestExplicitScriptKeepsModuleDeclarations;
    procedure TestDefaultEntrySourceType;
    procedure TestDetectedModuleEntryRunsAsModule;
    procedure TestDetectedScriptEntryRunsAsScript;
    procedure TestEmbedderChoosesScript;
    procedure TestDuplicateImportedBindingIsAnEarlyError;
  public
    procedure SetupTests; override;
  end;

procedure TEngineSourceTypeTests.SetupTests;
begin
  Test('A top-level import declaration makes a detected source a Module',
    TestImportDeclarationIsModule);
  Test('A top-level export declaration makes a detected source a Module',
    TestExportDeclarationIsModule);
  Test('A detected source without import or export stays a Script',
    TestNoModuleDeclarationIsScript);
  Test('A dynamic import() alone does not make a source a Module',
    TestDynamicImportAloneIsScript);
  Test('import.meta alone is a SyntaxError in a detected Script',
    TestImportMetaAloneIsAScriptError);
  Test('import.meta ahead of a later export is allowed in a detected Module',
    TestImportMetaBeforeAnExportIsModule);
  Test('A with statement in a detected Module is a strict-mode SyntaxError',
    TestWithInADetectedModuleIsAStrictModeError);
  Test('--compat-non-strict-mode does not make a detected Module sloppy',
    TestWithInADetectedSloppyModuleIsAStrictModeError);
  Test('An explicit Script keeps accepting import and export',
    TestExplicitScriptKeepsModuleDeclarations);
  Test('An engine detects a .js entry and takes .mjs as a Module',
    TestDefaultEntrySourceType);
  Test('Two imports binding one name are an early error of a Module',
    TestDuplicateImportedBindingIsAnEarlyError);
  Test('A detected Module entry has an undefined this',
    TestDetectedModuleEntryRunsAsModule);
  Test('A detected Script entry keeps its globals',
    TestDetectedScriptEntryRunsAsScript);
  Test('An embedder that sets stScript runs module syntax as a Script',
    TestEmbedderChoosesScript);
end;

function TEngineSourceTypeTests.ParsedSourceType(const ASource: string;
  const ASourceType: TGocciaSourceType;
  const ACompatibility: TGocciaCompatibilityFlags): TGocciaSourceType;
var
  Options: TGocciaSourcePipelineOptions;
  Parsed: TGocciaSourcePipelineResult;
  Source: TStringList;
begin
  Source := TStringList.Create;
  try
    Source.Text := ASource;
    Options := TGocciaSourcePipeline.DefaultOptions;
    Options.SourceType := ASourceType;
    Options.Compatibility := ACompatibility;
    Parsed := TGocciaSourcePipeline.Parse(Source, 'entry.js', Options);
    try
      Result := Parsed.SourceType;
    finally
      Parsed.Free;
    end;
  finally
    Source.Free;
  end;
end;

function TEngineSourceTypeTests.ParseError(const ASource: string;
  const ASourceType: TGocciaSourceType;
  const ACompatibility: TGocciaCompatibilityFlags): string;
begin
  Result := '';
  try
    ParsedSourceType(ASource, ASourceType, ACompatibility);
  except
    on E: TGocciaSyntaxError do
      Result := E.Message;
  end;
end;

function TEngineSourceTypeTests.RunEntry(const AFileName, ASource: string; const AOverride: Boolean;
  const ASourceType: TGocciaSourceType;
  out ASourceTypeAfterRun: TGocciaSourceType): string;
var
  Engine: TGocciaEngine;
  Source: TStringList;
begin
  Source := TStringList.Create;
  try
    Source.Text := ASource;
    Engine := TGocciaEngine.Create(AFileName, Source);
    try
      if AOverride then
        Engine.SourceType := ASourceType;
      Result := Engine.Execute.Result.ToStringLiteral.Value;
      ASourceTypeAfterRun := Engine.SourceType;
    finally
      Engine.Free;
    end;
  finally
    Source.Free;
  end;
end;

procedure TEngineSourceTypeTests.TestImportDeclarationIsModule;
begin
  Expect<Boolean>(ParsedSourceType('import "./dependency.js";',
    stDetect) = stModule).ToBe(True);
  Expect<Boolean>(ParsedSourceType(
    'const before = 1;' + LineEnding + 'import { x } from "./dependency.js";',
    stDetect) = stModule).ToBe(True);
end;

procedure TEngineSourceTypeTests.TestExportDeclarationIsModule;
begin
  Expect<Boolean>(ParsedSourceType('export const answer = 42;',
    stDetect) = stModule).ToBe(True);
  Expect<Boolean>(ParsedSourceType('export {};', stDetect) = stModule)
    .ToBe(True);
end;

procedure TEngineSourceTypeTests.TestNoModuleDeclarationIsScript;
begin
  Expect<Boolean>(ParsedSourceType('const answer = 42;', stDetect) =
    stScript).ToBe(True);
  Expect<Boolean>(ParsedSourceType('', stDetect) = stScript).ToBe(True);
end;

procedure TEngineSourceTypeTests.TestDynamicImportAloneIsScript;
begin
  Expect<Boolean>(ParsedSourceType(
    'const loaded = import("./dependency.js");', stDetect) = stScript)
    .ToBe(True);
  Expect<Boolean>(ParsedSourceType(
    'import("./dependency.js").then(() => 1);', stDetect) = stScript)
    .ToBe(True);
end;

procedure TEngineSourceTypeTests.TestImportMetaAloneIsAScriptError;
begin
  Expect<string>(ParseError('const url = import.meta.url;', stDetect))
    .ToBe('import.meta is only allowed in module code');
end;

procedure TEngineSourceTypeTests.TestImportMetaBeforeAnExportIsModule;
begin
  Expect<Boolean>(ParsedSourceType(
    'const url = import.meta.url;' + LineEnding + 'export { url };',
    stDetect) = stModule).ToBe(True);
end;

procedure TEngineSourceTypeTests.TestWithInADetectedModuleIsAStrictModeError;
begin
  Expect<string>(ParseError(
    'with ({}) {}' + LineEnding + 'export {};', stDetect))
    .ToBe('''with'' statements are not allowed in strict mode');
  Expect<string>(ParseError('with ({}) {}', stDetect))
    .ToBe('''with'' statements require --compat-non-strict-mode');
end;

procedure TEngineSourceTypeTests.TestWithInADetectedSloppyModuleIsAStrictModeError;
begin
  Expect<string>(ParseError(
    'with ({}) {}' + LineEnding + 'export {};', stDetect, [cfNonStrictMode]))
    .ToBe('''with'' statements are not allowed in strict mode');
  Expect<Boolean>(ParsedSourceType('with ({}) {}', stDetect,
    [cfNonStrictMode]) = stScript).ToBe(True);
end;

procedure TEngineSourceTypeTests.TestExplicitScriptKeepsModuleDeclarations;
begin
  Expect<Boolean>(ParsedSourceType('export const answer = 42;',
    stScript) = stScript).ToBe(True);
  Expect<Boolean>(ParsedSourceType('const answer = 42;', stModule) =
    stModule).ToBe(True);
end;

procedure TEngineSourceTypeTests.TestDefaultEntrySourceType;
var
  Engine: TGocciaEngine;
  Source: TStringList;
begin
  Source := TStringList.Create;
  try
    Engine := TGocciaEngine.Create('entry.js', Source);
    try
      Expect<Boolean>(Engine.SourceType = stDetect).ToBe(True);
    finally
      Engine.Free;
    end;
    Engine := TGocciaEngine.Create('entry.mjs', Source);
    try
      Expect<Boolean>(Engine.SourceType = stModule).ToBe(True);
    finally
      Engine.Free;
    end;
  finally
    Source.Free;
  end;
end;

procedure TEngineSourceTypeTests.TestDuplicateImportedBindingIsAnEarlyError;
begin
  Expect<string>(ParseError(
    'import { n } from "./a.js";' + LineEnding + 'const n = 1;', stModule))
    .ToBe('Identifier ''n'' has already been declared');
  Expect<string>(ParseError(
    'import { n } from "./a.js";' + LineEnding +
    'import { m as n } from "./b.js";', stModule))
    .ToBe('Identifier ''n'' has already been declared');
  Expect<string>(ParseError(
    'import * as ns from "./a.js";' + LineEnding + 'export const ns = 1;',
    stDetect)).ToBe('Identifier ''ns'' has already been declared');
  { One declaration binding a name twice (ImportedBinding in
    ImportsList, or the default binding and a named one). }
  Expect<string>(ParseError(
    'import { a as x, b as x } from "./a.js";', stModule))
    .ToBe('Identifier ''x'' has already been declared');
  Expect<string>(ParseError(
    'import x, { a as x } from "./a.js";', stDetect))
    .ToBe('Identifier ''x'' has already been declared');
  Expect<string>(ParseError(
    'import x, * as x from "./a.js";', stModule))
    .ToBe('Identifier ''x'' has already been declared');
  { A Script has no such early error; its declarations meet at run time. }
  Expect<string>(ParseError(
    'import { n } from "./a.js";' + LineEnding + 'const n = 1;', stScript))
    .ToBe('');
end;

procedure TEngineSourceTypeTests.TestDetectedModuleEntryRunsAsModule;
var
  After: TGocciaSourceType;
begin
  Expect<string>(RunEntry('entry.js',
    'export const answer = 42;' + LineEnding +
    'String(this) + ":" + typeof import.meta.url;',
    False, stDetect, After)).ToBe('undefined:string');
  { The setting is kept: a later Execute settles its own source. }
  Expect<Boolean>(After = stDetect).ToBe(True);
end;

procedure TEngineSourceTypeTests.TestDetectedScriptEntryRunsAsScript;
var
  After: TGocciaSourceType;
begin
  Expect<string>(RunEntry('entry.js',
    'const scriptGlobal = 1;' + LineEnding +
    'String(this === globalThis) + ":" + typeof scriptGlobal;',
    False, stDetect, After)).ToBe('true:number');
  Expect<Boolean>(After = stDetect).ToBe(True);
end;

procedure TEngineSourceTypeTests.TestEmbedderChoosesScript;
var
  After: TGocciaSourceType;
begin
  Expect<string>(RunEntry('entry.js',
    'export const answer = 42;' + LineEnding +
    'String(this === globalThis);',
    True, stScript, After)).ToBe('true');
  Expect<Boolean>(After = stScript).ToBe(True);
end;

begin
  TestRunnerProgram.AddSuite(TEngineSourceTypeTests.Create(
    'Engine SourceType detection'));
  RunGocciaTests;
  ExitCode := TestResultToExitCode;
end.
