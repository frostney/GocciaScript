program ModuleProviderTest;

{ Modules an embedder supplies on first import (TGocciaEngine.DefineModuleProvider): a source answer and a bytecode
  answer link and keep one namespace; a module never imported never asks its provider; a bytecode file compiled under
  another profile is refused; a failed answer is asked again by a later import; registration follows the native
  modules' rules; and registrations end with the engine. Prints "Module providers: passed". }

{$mode delphiunicode}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, SysUtils,
  Goccia.Core,
  Goccia.Embed,
  Goccia.HostAPI,
  Goccia.Instructions,
  Goccia.Modules,
  Goccia.VM;

type
  TProvider = class
  public
    Calls, NeverCalls, FailCalls, CycleCalls, DamagedCalls: Integer;
    Source: UnicodeString;
    Bytecode: TBytes;
    { Nested: the engine and a manifest module it defines from inside its provider, during a run }
    Engine: TGocciaEngine;
    NestedManifest, GlobalsPath, SetupError: UnicodeString;
    function Provide(const ASpecifier: UnicodeString; var AAnswer: TGocciaModuleProviderAnswer): Boolean;
    function Never(const ASpecifier: UnicodeString; var AAnswer: TGocciaModuleProviderAnswer): Boolean;
    function Fail(const ASpecifier: UnicodeString; var AAnswer: TGocciaModuleProviderAnswer): Boolean;
    function Silent(const ASpecifier: UnicodeString; var AAnswer: TGocciaModuleProviderAnswer): Boolean;
    function ProvideDamaged(const ASpecifier: UnicodeString; var AAnswer: TGocciaModuleProviderAnswer): Boolean;
    function Cyclic(const ASpecifier: UnicodeString; var AAnswer: TGocciaModuleProviderAnswer): Boolean;
    function Nested(const ASpecifier: UnicodeString; var AAnswer: TGocciaModuleProviderAnswer): Boolean;
    function Transitive(const ASpecifier: UnicodeString; var AAnswer: TGocciaModuleProviderAnswer): Boolean;
    function InjectsGlobals(const ASpecifier: UnicodeString; var AAnswer: TGocciaModuleProviderAnswer): Boolean;
  end;

procedure Check(ACondition: Boolean; const AMessage: UnicodeString);
begin
  if not ACondition then raise Exception.Create(AMessage);
end;

{ host:source answers with source text, host:bytecode and host:other with the bytecode file in Bytecode }
function TProvider.Provide(const ASpecifier: UnicodeString; var AAnswer: TGocciaModuleProviderAnswer): Boolean;
begin
  Inc(Calls);
  if (ASpecifier = 'host:bytecode') or (ASpecifier = 'host:other') then
  begin
    AAnswer.Kind := mpaBytecode;
    AAnswer.Bytecode := Bytecode;
  end
  else
  begin
    AAnswer.Kind := mpaSource;
    AAnswer.Source := Source;
  end;
  Result := True;
end;

function TProvider.Never(const ASpecifier: UnicodeString; var AAnswer: TGocciaModuleProviderAnswer): Boolean;
begin
  Inc(NeverCalls);
  AAnswer.Kind := mpaSource;
  AAnswer.Source := 'export const unused = true;';
  Result := True;
end;

{ fails with an error of its own, then answers }
function TProvider.Fail(const ASpecifier: UnicodeString; var AAnswer: TGocciaModuleProviderAnswer): Boolean;
begin
  Inc(FailCalls);
  if FailCalls = 1 then
  begin
    ThrowTypeError('the library is not installed');
    Exit(False);
  end;
  AAnswer.Kind := mpaSource;
  AAnswer.Source := 'export const installed = "yes";';
  Result := True;
end;

{ defines a manifest module synchronously (a setup call inside a run), then answers with Bytecode }
function TProvider.Nested(const ASpecifier: UnicodeString; var AAnswer: TGocciaModuleProviderAnswer): Boolean;
var
  Err: UnicodeString;
begin
  if not Engine.DefineModuleManifestVirtualModules(NestedManifest, Err) then
    raise Exception.Create('the nested manifest failed: ' + string(Err));
  AAnswer.Kind := mpaBytecode;
  AAnswer.Bytecode := Bytecode;
  Result := True;
end;

{ evaluates a module that imports the module whose import it is supplying, whose imports are still loading }
function TProvider.Transitive(const ASpecifier: UnicodeString; var AAnswer: TGocciaModuleProviderAnswer): Boolean;
var
  Namespace: TValue;
begin
  if EvaluateModuleNamespace('host:loop', Namespace) then
    raise Exception.Create('a graph still loading its imports was linked');
  Result := False;
end;

{ tries InjectModuleGlobals inside the run, then answers }
function TProvider.InjectsGlobals(const ASpecifier: UnicodeString; var AAnswer: TGocciaModuleProviderAnswer): Boolean;
var
  Err: UnicodeString;
begin
  Err := '';
  if Engine.InjectModuleGlobals(GlobalsPath, Err) then
    SetupError := 'accepted'
  else
    SetupError := Err;
  AAnswer.Kind := mpaSource;
  AAnswer.Source := 'export const ready = true;';
  Result := True;
end;

function TProvider.ProvideDamaged(const ASpecifier: UnicodeString; var AAnswer: TGocciaModuleProviderAnswer): Boolean;
begin
  Inc(DamagedCalls);
  AAnswer.Kind := mpaBytecode;
  AAnswer.Bytecode := Bytecode;
  Result := True;
end;

{ loads a module that imports this same one while the provider runs: the first call sees that load refused and
  fails with it; later calls answer }
function TProvider.Cyclic(const ASpecifier: UnicodeString; var AAnswer: TGocciaModuleProviderAnswer): Boolean;
var
  Namespace: TValue;
begin
  Inc(CycleCalls);
  if CycleCalls = 1 then
  begin
    if EvaluateModuleNamespace('host:cycle-dependency', Namespace) then
      raise Exception.Create('a recursive load reached the module provider again');
    Exit(False);
  end;
  AAnswer.Kind := mpaSource;
  AAnswer.Source := 'export const answer = 42;';
  Result := True;
end;

function TProvider.Silent(const ASpecifier: UnicodeString; var AAnswer: TGocciaModuleProviderAnswer): Boolean;
begin
  Result := False;
end;

{ the bytecode file of ASource compiled as a module, for the address ASpecifier, by an engine of its own (one engine
  per thread at a time: before the engine under test exists); AOtherProfile flips one compatibility flag }
function CompileModule(const ASpecifier: string; const ASource: UnicodeString; AOtherProfile: Boolean): TBytes;
var
  Engine: TGocciaEngine;
  R: TGocciaRunResult;
  Profile: TLangProfile;
begin
  Engine := TGocciaEngine.Create;
  try
    Engine.SourceKind := gskModule;
    if AOtherProfile then
    begin
      Profile := Engine.Profile;
      Profile.WhileLoop := not Profile.WhileLoop;
      Engine.Profile := Profile;
    end;
    Check(Engine.CompileArtifact(ASpecifier, ASource, Result, R), 'compile failed: ' + R.ErrorMessage);
  finally
    Engine.Free;
  end;
end;

procedure CheckProviders;
var
  Engine: TGocciaEngine;
  Provider: TProvider;
  ErrorText: UnicodeString;
  RunResult: TGocciaRunResult;
  OtherBytecode, Saved: TBytes;
begin
  Provider := TProvider.Create;
  Provider.Source := 'import { twice } from "host:bytecode"; export const answer = twice(21);';
  Provider.Bytecode := CompileModule('host:bytecode', 'export const twice = (n) => n * 2;', False);
  OtherBytecode := CompileModule('host:other', 'globalThis.otherRan = true; export const x = 1;', True);
  Engine := TGocciaEngine.Create;
  try
    Engine.ResultForm := grfString;
    Engine.SourceKind := gskModule;

    // registration follows the native modules' rules
    Check(not Engine.DefineModuleProvider('/file.mjs', Provider.Provide, ErrorText) and
      (Pos('virtual address', ErrorText) > 0), 'a file name was accepted: ' + ErrorText);
    Check(not Engine.DefineModuleProvider('goccia:json5', Provider.Provide, ErrorText) and
      (Pos('goccia:', ErrorText) > 0), 'the runtime''s namespace was accepted: ' + ErrorText);
    Check(not Engine.DefineModuleProvider('host:none', nil, ErrorText) and (Pos('required', ErrorText) > 0),
      'a missing provider was accepted: ' + ErrorText);
    Check(Engine.DefineModuleProvider('host:source', Provider.Provide, ErrorText), ErrorText);
    Check(Engine.DefineModuleProvider('host:bytecode', Provider.Provide, ErrorText), ErrorText);
    Check(Engine.DefineModuleProvider('host:never', Provider.Never, ErrorText), ErrorText);
    Check(not Engine.DefineModuleProvider('host:source', Provider.Provide, ErrorText) and
      (Pos('already registered', ErrorText) > 0), 'a duplicate was accepted: ' + ErrorText);
    Check(not Engine.DefineInlineVirtualModule('host:source=export const value = 1;', '', ErrorText),
      'a source module replaced a provided module');
    Check(Engine.DefineInlineVirtualModule('host:inline=export const value = 1;', '', ErrorText), ErrorText);
    Check(not Engine.DefineModuleProvider('host:inline', Provider.Provide, ErrorText),
      'a provided module replaced a source module');

    // a source answer importing a bytecode answer: both link, one namespace each, each provider asked once
    RunResult := Engine.Run('/first.mjs',
      'import * as lib from "host:source"; import * as bc from "host:bytecode"; globalThis.firstLib = lib; ' +
      'globalThis.firstBc = bc; lib.answer + ":" + bc.twice(5);');
    Check(RunResult.Ok and (RunResult.ResultText = '42:10'),
      'the provided modules did not link: ' + RunResult.ResultText + RunResult.ErrorMessage);
    Check(Provider.Calls = 2, Format('the providers were asked %d times for two modules', [Provider.Calls]));
    RunResult := Engine.Run('/second.mjs',
      'import * as lib from "host:source"; const bc = await import("host:bytecode"); ' +
      '(globalThis.firstLib === lib) + ":" + (globalThis.firstBc === bc);');
    Check(RunResult.Ok and (RunResult.ResultText = 'true:true') and (Provider.Calls = 2),
      'a provided module''s namespace changed or its provider was asked again: ' + RunResult.ResultText +
      RunResult.ErrorMessage);
    Check(Provider.NeverCalls = 0, 'a module no script imported asked its provider');

    // no source phase and no typed lookup: the provider is not asked
    RunResult := Engine.Run('/source-phase.mjs', 'import source original from "host:never"; original;');
    Check(not RunResult.Ok and (RunResult.ErrorName = 'SyntaxError') and (Provider.NeverCalls = 0) and
      (Pos('Module source is not available for "host:never"', RunResult.ErrorText) > 0),
      'a source phase import asked a provider: ' + RunResult.ErrorText);
    RunResult := Engine.Run('/typed.mjs', 'import value from "host:never" with { type: "text" }; value;');
    Check(not RunResult.Ok and (Provider.NeverCalls = 0), 'a typed import asked a provider');

    // a failed answer fails the import with its error and is asked again by a later import
    Check(Engine.DefineModuleProvider('host:fails', Provider.Fail, ErrorText), ErrorText);
    RunResult := Engine.Run('/fails.mjs',
      'const first = await import("host:fails").then(() => "loaded", e => e instanceof TypeError && e.message); ' +
      'const second = await import("host:fails").then(m => m.installed, e => "failed again"); first + ":" + second;');
    Check(RunResult.Ok and (RunResult.ResultText = 'the library is not installed:yes') and (Provider.FailCalls = 2),
      'a failed answer was not reported or not asked again: ' + RunResult.ResultText + RunResult.ErrorMessage);
    Check(Engine.DefineModuleProvider('host:silent', Provider.Silent, ErrorText), ErrorText);
    RunResult := Engine.Run('/silent.mjs',
      'await import("host:silent").then(() => "loaded", e => e instanceof TypeError && e.message);');
    Check(RunResult.Ok and (Pos('The module provider for host:silent failed without an error', RunResult.ResultText) > 0),
      'a silent failure was not reported: ' + RunResult.ResultText + RunResult.ErrorMessage);

    // a load that reaches the provider's module while the provider runs is refused; a later import loads it once
    Check(Engine.DefineModuleProvider('host:cycle', Provider.Cyclic, ErrorText), ErrorText);
    Check(Engine.DefineInlineVirtualModule(
      'host:cycle-dependency=import { answer } from "host:cycle"; export const value = answer;', '', ErrorText),
      ErrorText);
    RunResult := Engine.Run('/cycle.mjs',
      'await import("host:cycle").then(() => "loaded", e => e instanceof TypeError && e.message);');
    Check(RunResult.Ok and (Pos('host:cycle is still being supplied', RunResult.ResultText) > 0) and
      (Provider.CycleCalls = 1), 'a recursive load was not refused: ' + RunResult.ResultText + RunResult.ErrorMessage);
    RunResult := Engine.Run('/cycle-retry.mjs',
      'import * as first from "host:cycle"; const second = await import("host:cycle"); (first === second) + ":" + first.answer;');
    Check(RunResult.Ok and (RunResult.ResultText = 'true:42') and (Provider.CycleCalls = 2),
      'a retried provider did not publish one module: ' + RunResult.ResultText + RunResult.ErrorMessage);

    // with module sources on, a provided module still has no source phase, even when the same entry evaluates it
    Engine.JSModuleSources := True;
    RunResult := Engine.Run('/mixed.mjs',
      'import source original from "host:never"; import { unused } from "host:never"; unused;');
    Check(not RunResult.Ok and (RunResult.ErrorName = 'SyntaxError'),
      'a provided module had a source phase: ' + RunResult.ErrorName + ': ' + RunResult.ErrorText);
    Engine.JSModuleSources := False;

    // an answer past the bytecode file bound is refused before it is copied
    Saved := Provider.Bytecode;
    SetLength(Provider.Bytecode, 64 * 1024 * 1024 + 1);
    Check(Engine.DefineModuleProvider('host:huge', Provider.ProvideDamaged, ErrorText), ErrorText);
    Provider.DamagedCalls := 0;
    RunResult := Engine.Run('/huge.mjs', 'const a = await import("host:huge").then(() => "loaded", e => e.message); ' +
      'const b = await import("host:huge").then(() => "loaded", e => e.message); a === b ? a : a + " / " + b;');
    Check(RunResult.Ok and (Pos('Cannot load bytecode module ''host:huge''', RunResult.ResultText) > 0) and
      (Pos('byte limit', RunResult.ResultText) > 0) and (Provider.DamagedCalls = 1),
      Format('an oversized answer was not refused once and kept (%d calls): ', [Provider.DamagedCalls]) +
      RunResult.ResultText);
    Provider.Bytecode := Saved;

    // a bytecode file compiled under another profile is refused before any of it runs, never compiled from source
    Provider.Bytecode := OtherBytecode;
    Check(Engine.DefineModuleProvider('host:other', Provider.Provide, ErrorText), ErrorText);
    RunResult := Engine.Run('/other.mjs',
      'const r = await import("host:other").then(() => "loaded", e => e.message); ' +
      'r + ":" + (globalThis.otherRan === true);');
    Check(RunResult.Ok and (Pos('Cannot load bytecode module ''host:other''', RunResult.ResultText) > 0) and
      (Pos('Bytecode compile profile does not match the engine', RunResult.ResultText) > 0) and
      (Pos(':false', RunResult.ResultText) > 0) and (Provider.Calls = 3),
      'a bytecode file of another profile was not refused: ' + RunResult.ResultText + RunResult.ErrorMessage);
    // damaged bytes are refused the same way
    SetLength(Provider.Bytecode, 8);
    Check(Engine.DefineModuleProvider('host:damaged', Provider.ProvideDamaged, ErrorText), ErrorText);
    RunResult := Engine.Run('/damaged.mjs',
      'await import("host:damaged").then(() => "loaded", e => e.message);');
    Check(RunResult.Ok and (Pos('Cannot load bytecode module ''host:damaged''', RunResult.ResultText) > 0),
      'damaged bytecode was not refused: ' + RunResult.ResultText + RunResult.ErrorMessage);
  finally
    Engine.Free;
    Provider.Free;
  end;
  // registrations end with their engine
  Engine := TGocciaEngine.Create;
  try
    Engine.SourceKind := gskModule;
    RunResult := Engine.Run('/next.mjs', 'import "host:source";');
    Check(not RunResult.Ok, 'a provided module survived engine teardown');
  finally
    Engine.Free;
  end;
end;

{ a provider registered for a 256 KiB module holds nothing in the heap until an import asks it; the same source
  defined inline holds at least its text there (the measurement's own check) }
procedure CheckHeapCost;
const
  BIG = 256 * 1024;
var
  Engine: TGocciaEngine;
  Provider: TProvider;
  ErrorText: UnicodeString;
  Before, Provided, Inline: Int64;
begin
  Provider := TProvider.Create;
  Provider.Source := 'export const text = "' + StringOfChar('x', BIG) + '";';
  Engine := TGocciaEngine.Create;
  try
    Check(Engine.PrepareHost(ErrorText), ErrorText);
    CollectMinor;
    CollectMajor;
    Before := HeapUsedBytes;
    Check(Engine.DefineModuleProvider('host:big', Provider.Provide, ErrorText), ErrorText);
    CollectMinor;
    CollectMajor;
    Provided := HeapUsedBytes - Before;
    Check(Engine.DefineInlineVirtualModule('host:big-inline=' + Provider.Source, '', ErrorText), ErrorText);
    CollectMinor;
    CollectMajor;
    Inline := HeapUsedBytes - Before - Provided;
    Check(Inline >= BIG, Format('an inline 256 KiB module held only %d bytes: the measurement sees nothing', [Inline]));
    Check(Provided < 4096, Format('a provider registration held %d bytes of the heap', [Provided]));
    Check(Provider.Calls = 0, 'a registration asked its provider');
  finally
    Engine.Free;
    Provider.Free;
  end;
end;

{ a globals module the engine imports during setup, before any run, checks a provided bytecode file's profile too }
procedure CheckSetupImport;
var
  Engine: TGocciaEngine;
  Provider: TProvider;
  ErrorText: UnicodeString;
  RunResult: TGocciaRunResult;
  Path: string;
  Lines: TStringList;
begin
  Provider := TProvider.Create;
  Provider.Bytecode := CompileModule('host:other', 'globalThis.otherRan = true; export const x = 1;', True);
  Path := IncludeTrailingPathDelimiter(ExtractFileDir(ExpandFileName(ParamStr(0)))) + 'provider-setup-globals.mjs';
  Lines := TStringList.Create;
  Engine := TGocciaEngine.Create;
  try
    Lines.Text := 'import { x } from "host:other"; export const fromSetup = x;';
    Lines.SaveToFile(Path);
    Engine.ResultForm := grfString;
    Check(Engine.DefineModuleProvider('host:other', Provider.Provide, ErrorText), ErrorText);
    Check(not Engine.InjectModuleGlobals(UnicodeString(Path), ErrorText) and
      (Pos('Bytecode compile profile does not match the engine', ErrorText) > 0),
      'a setup import took another profile''s bytecode: ' + ErrorText);
    RunResult := Engine.Run('/after-setup.js', 'globalThis.otherRan === true;');
    Check(RunResult.Ok and (RunResult.ResultText = 'false'), 'another profile''s bytecode ran during setup');
  finally
    Engine.Free;
    Provider.Free;
    Lines.Free;
    DeleteFile(Path);
  end;
end;

{ a config's JavaScript manifest imports a provided module during setup: another profile's file is refused there too }
procedure CheckConfigSetupImport;
var
  Engine: TGocciaEngine;
  Provider: TProvider;
  ErrorText: UnicodeString;
  RunResult: TGocciaRunResult;
  Dir, Manifest, Config: string;
  Lines: TStringList;
begin
  Provider := TProvider.Create;
  Provider.Bytecode := CompileModule('host:other', 'globalThis.otherRan = true; export const x = 1;', True);
  Dir := IncludeTrailingPathDelimiter(ExtractFileDir(ExpandFileName(ParamStr(0))));
  Manifest := Dir + 'provider-config-manifest.mjs';
  Config := Dir + 'provider-config.json';
  Lines := TStringList.Create;
  Engine := TGocciaEngine.Create;
  try
    Lines.Text := 'import { x } from "host:other"; export default { "host:from-config": { content: "export default 1;" } };';
    Lines.SaveToFile(Manifest);
    Lines.Text := '{ "modules": "./provider-config-manifest.mjs" }';
    Lines.SaveToFile(Config);
    Engine.ResultForm := grfString;
    Check(Engine.DefineModuleProvider('host:other', Provider.Provide, ErrorText), ErrorText);
    Check(not Engine.DefineConfigVirtualModules(UnicodeString(Config), ErrorText) and
      (Pos('Bytecode compile profile does not match the engine', ErrorText) > 0),
      'a config''s manifest took another profile''s bytecode: ' + ErrorText);
    RunResult := Engine.Run('/after-config.js', 'globalThis.otherRan === true;');
    Check(RunResult.Ok and (RunResult.ResultText = 'false'), 'another profile''s bytecode ran during config setup');
  finally
    Engine.Free;
    Provider.Free;
    Lines.Free;
    DeleteFile(Manifest);
    DeleteFile(Config);
  end;
end;

{ a provider that makes a setup call during a run (DefineModuleManifestVirtualModules) leaves the run's check in
  force: its answer of another profile's file is still refused }
procedure CheckNestedSetup;
var
  Engine: TGocciaEngine;
  Provider: TProvider;
  ErrorText: UnicodeString;
  RunResult: TGocciaRunResult;
  Manifest: string;
  Lines: TStringList;
begin
  Provider := TProvider.Create;
  Provider.Bytecode := CompileModule('host:nested', 'globalThis.otherRan = true; export const x = 1;', True);
  Manifest := IncludeTrailingPathDelimiter(ExtractFileDir(ExpandFileName(ParamStr(0)))) + 'provider-nested-manifest.mjs';
  Lines := TStringList.Create;
  Engine := TGocciaEngine.Create;
  try
    Lines.Text := 'export default { "host:from-nested": { content: "export default 1;" } };';
    Lines.SaveToFile(Manifest);
    Provider.Engine := Engine;
    Provider.NestedManifest := UnicodeString(Manifest);
    Engine.ResultForm := grfString;
    Engine.SourceKind := gskModule;
    Check(Engine.DefineModuleProvider('host:nested', Provider.Nested, ErrorText), ErrorText);
    RunResult := Engine.Run('/nested.mjs',
      'const r = await import("host:nested").then(() => "loaded", e => e.message); r + ":" + (globalThis.otherRan === true);');
    Check(RunResult.Ok and (Pos('Bytecode compile profile does not match the engine', RunResult.ResultText) > 0) and
      (Pos(':false', RunResult.ResultText) > 0),
      'a setup call inside a provider dropped the run''s check: ' + RunResult.ResultText + RunResult.ErrorMessage);
  finally
    Engine.Free;
    Provider.Free;
    Lines.Free;
    DeleteFile(Manifest);
  end;
end;

{ a provider that evaluates a module importing the graph it is supplying a dependency of gets a load error, not a
  fault; an ordinary cycle still loads }
procedure CheckTransitiveLoad;
var
  Engine: TGocciaEngine;
  Provider: TProvider;
  ErrorText: UnicodeString;
  RunResult: TGocciaRunResult;
begin
  Provider := TProvider.Create;
  Engine := TGocciaEngine.Create;
  try
    Engine.ResultForm := grfString;
    Engine.SourceKind := gskModule;
    Check(Engine.DefineModuleProvider('host:dep', Provider.Transitive, ErrorText), ErrorText);
    Check(Engine.DefineInlineVirtualModule('host:parent=import "host:dep"; export const parent = 1;', '', ErrorText),
      ErrorText);
    Check(Engine.DefineInlineVirtualModule('host:loop=import { parent } from "host:parent"; export const loop = parent;',
      '', ErrorText), ErrorText);
    Check(Engine.DefineInlineVirtualModule('host:a=import { b } from "host:b"; export const a = 1; export const viaB = () => b;',
      '', ErrorText), ErrorText);
    Check(Engine.DefineInlineVirtualModule('host:b=import { a } from "host:a"; export const b = 2; export const viaA = () => a;',
      '', ErrorText), ErrorText);
    RunResult := Engine.Run('/transitive.mjs',
      'await import("host:parent").then(() => "loaded", e => e instanceof TypeError && e.message);');
    Check(RunResult.Ok and (Pos('is still loading its imports', RunResult.ResultText) > 0),
      'a graph still loading its imports was not refused: ' + RunResult.ResultText + RunResult.ErrorMessage);
    RunResult := Engine.Run('/cycle-ok.mjs', 'import { a, viaB } from "host:a"; import { b, viaA } from "host:b"; ' +
      'a + viaB() + b + viaA();');
    Check(RunResult.Ok and (RunResult.ResultText = '6'), 'an ordinary cycle did not load: ' + RunResult.ResultText +
      RunResult.ErrorMessage);
  finally
    Engine.Free;
    Provider.Free;
  end;
end;

{ InjectModuleGlobals inside a run (from a provider) is refused before it changes anything: the run's instruction
  cap still ends it }
procedure CheckGlobalsInsideRun;
var
  Engine: TGocciaEngine;
  Provider: TProvider;
  ErrorText: UnicodeString;
  RunResult: TGocciaRunResult;
  Path: string;
  Lines: TStringList;
begin
  Provider := TProvider.Create;
  Path := IncludeTrailingPathDelimiter(ExtractFileDir(ExpandFileName(ParamStr(0)))) + 'provider-run-globals.mjs';
  Lines := TStringList.Create;
  Engine := TGocciaEngine.Create;
  try
    Lines.Text := 'export const injected = true;';
    Lines.SaveToFile(Path);
    Provider.Engine := Engine;
    Provider.GlobalsPath := UnicodeString(Path);
    Engine.ResultForm := grfString;
    Engine.SourceKind := gskModule;
    Engine.MaxInstructions := 5000000;
    Check(Engine.DefineModuleProvider('host:injects', Provider.InjectsGlobals, ErrorText), ErrorText);
    RunResult := Engine.Run('/globals-inside.mjs', 'import { ready } from "host:injects"; ' +
      'const forever = { [Symbol.iterator]() { return { next: () => ({ value: 1, done: false }) }; } }; ' +
      'for (const x of forever) {} ready;');
    Check(Pos('cannot be called while the engine runs an entry', Provider.SetupError) > 0,
      'InjectModuleGlobals inside a run was not refused: ' + Provider.SetupError);
    Check(RunResult.Aborted = gaInstructionLimit, Format('the run lost its instruction cap (abort %d) %s',
      [Ord(RunResult.Aborted), RunResult.ErrorMessage]));
  finally
    Engine.Free;
    Provider.Free;
    Lines.Free;
    DeleteFile(Path);
  end;
end;

{ setup imports follow the engine's JSModuleSources, as its runs do, and leave the setting they found }
procedure CheckSetupModuleSources;
var
  Engine: TGocciaEngine;
  ErrorText: UnicodeString;
  RunResult: TGocciaRunResult;
  Path: string;
  Lines: TStringList;
begin
  Path := IncludeTrailingPathDelimiter(ExtractFileDir(ExpandFileName(ParamStr(0)))) + 'provider-source-globals.mjs';
  Lines := TStringList.Create;
  Engine := TGocciaEngine.Create;
  try
    Lines.Text := 'import source plain from "host:plain"; export const sourceKind = typeof plain;';
    Lines.SaveToFile(Path);
    Engine.ResultForm := grfString;
    Engine.JSModuleSources := True;
    Check(Engine.DefineInlineVirtualModule('host:plain=export const v = 1;', '', ErrorText), ErrorText);
    Check(Engine.InjectModuleGlobals(UnicodeString(Path), ErrorText),
      'a setup import did not follow JSModuleSources: ' + ErrorText);
    Check(not JSModuleSourcesEnabled, 'setup left module sources on');
    RunResult := Engine.Run('/kind.js', 'sourceKind;');
    Check(RunResult.Ok and (RunResult.ResultText = 'object'), 'the source phase value was not kept: ' +
      RunResult.ResultText + RunResult.ErrorMessage);
  finally
    Engine.Free;
    Lines.Free;
    DeleteFile(Path);
  end;
end;

{ the hash a bytecode record keeps is its file's: the same bytes in another engine give the same hash, other code
  under the same specifier another (a heap snapshot's restore compares them) }
function RecordHash(const ABytecode: TBytes): QWord;
var
  Engine: TGocciaEngine;
  Provider: TProvider;
  ErrorText, RecordPath: UnicodeString;
  RunResult: TGocciaRunResult;
  I: Integer;
  Tpl: PFuncTemplate;
  Hash: QWord;
begin
  Result := 0;
  Provider := TProvider.Create;
  Provider.Bytecode := ABytecode;
  Engine := TGocciaEngine.Create;
  try
    Engine.SourceKind := gskModule;
    Check(Engine.DefineModuleProvider('host:bytecode', Provider.Provide, ErrorText), ErrorText);
    RunResult := Engine.Run('/hash.mjs', 'import "host:bytecode";');
    Check(RunResult.Ok, 'the hashed module did not load: ' + RunResult.ErrorMessage);
    I := 0;
    while ModuleTemplateAt(I, RecordPath, Tpl, Hash) do
    begin
      if RecordPath = 'host:bytecode' then Result := Hash;
      Inc(I);
    end;
  finally
    Engine.Free;
    Provider.Free;
  end;
end;

procedure CheckRecordHash;
var
  First, Second: TBytes;
  H1, H2, H3: QWord;
begin
  First := CompileModule('host:bytecode', 'export const twice = (n) => n * 2;', False);
  Second := CompileModule('host:bytecode', 'export const twice = (n) => n + n;', False);
  H1 := RecordHash(First);
  H2 := RecordHash(Second);
  H3 := RecordHash(First);
  Check((H1 <> 0) and (H1 = H3) and (H1 <> H2),
    Format('bytecode record hashes do not follow the file: %u, %u, %u', [H1, H2, H3]));
end;

{ a Bare engine (no host modules of its own) takes provided modules too }
procedure CheckBare;
var
  Engine: TGocciaEngine;
  Provider: TProvider;
  ErrorText: UnicodeString;
  RunResult: TGocciaRunResult;
begin
  Provider := TProvider.Create;
  Provider.Source := 'export const answer = 42;';
  Engine := TGocciaEngine.Create;
  try
    Engine.Bare := True;
    Engine.ResultForm := grfString;
    Engine.SourceKind := gskModule;
    Check(Engine.DefineModuleProvider('host:source', Provider.Provide, ErrorText), ErrorText);
    RunResult := Engine.Run('/bare.mjs', 'import { answer } from "host:source"; answer;');
    Check(RunResult.Ok and (RunResult.ResultText = '42') and (Provider.Calls = 1),
      'a Bare engine did not load a provided module: ' + RunResult.ResultText + RunResult.ErrorMessage);
  finally
    Engine.Free;
    Provider.Free;
  end;
end;

begin
  CheckHeapCost;
  CheckBare;
  CheckSetupImport;
  CheckConfigSetupImport;
  CheckNestedSetup;
  CheckRecordHash;
  CheckTransitiveLoad;
  CheckGlobalsInsideRun;
  CheckSetupModuleSources;
  CheckProviders;
  WriteLn('Module providers: passed');
end.
