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
    Calls, NeverCalls, FailCalls, CycleCalls: Integer;
    Source: UnicodeString;
    Bytecode: TBytes;
    function Provide(const ASpecifier: UnicodeString; var AAnswer: TGocciaModuleProviderAnswer): Boolean;
    function Never(const ASpecifier: UnicodeString; var AAnswer: TGocciaModuleProviderAnswer): Boolean;
    function Fail(const ASpecifier: UnicodeString; var AAnswer: TGocciaModuleProviderAnswer): Boolean;
    function Silent(const ASpecifier: UnicodeString; var AAnswer: TGocciaModuleProviderAnswer): Boolean;
    function ProvideDamaged(const ASpecifier: UnicodeString; var AAnswer: TGocciaModuleProviderAnswer): Boolean;
    function Cyclic(const ASpecifier: UnicodeString; var AAnswer: TGocciaModuleProviderAnswer): Boolean;
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

function TProvider.ProvideDamaged(const ASpecifier: UnicodeString; var AAnswer: TGocciaModuleProviderAnswer): Boolean;
begin
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
  I: Integer;
  RecordPath: UnicodeString;
  Tpl: PFuncTemplate;
  Hash: QWord;
  Hashed: Boolean;
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

    // the bytecode record's hash is its file's (a heap snapshot's restore compares it with the code it reloads)
    I := 0;
    Hashed := False;
    while ModuleTemplateAt(I, RecordPath, Tpl, Hash) do
    begin
      if RecordPath = 'host:bytecode' then Hashed := Hash <> 0;
      Inc(I);
    end;
    Check(Hashed, 'the bytecode record has no hash of its file');

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
    RunResult := Engine.Run('/huge.mjs', 'await import("host:huge").then(() => "loaded", e => e.message);');
    Check(RunResult.Ok and (Pos('Cannot load bytecode module ''host:huge''', RunResult.ResultText) > 0) and
      (Pos('byte limit', RunResult.ResultText) > 0), 'an oversized answer was not refused: ' + RunResult.ResultText);
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
  CheckProviders;
  WriteLn('Module providers: passed');
end.
