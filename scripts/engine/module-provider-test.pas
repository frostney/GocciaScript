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
  Goccia.Modules,
  Goccia.VM;

type
  TProvider = class
  public
    Calls, NeverCalls, FailCalls: Integer;
    Source: UnicodeString;
    Bytecode: TBytes;
    function Provide(const ASpecifier: UnicodeString; var AAnswer: TGocciaModuleProviderAnswer): Boolean;
    function Never(const ASpecifier: UnicodeString; var AAnswer: TGocciaModuleProviderAnswer): Boolean;
    function Fail(const ASpecifier: UnicodeString; var AAnswer: TGocciaModuleProviderAnswer): Boolean;
    function Silent(const ASpecifier: UnicodeString; var AAnswer: TGocciaModuleProviderAnswer): Boolean;
    function ProvideDamaged(const ASpecifier: UnicodeString; var AAnswer: TGocciaModuleProviderAnswer): Boolean;
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
  OtherBytecode: TBytes;
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
  CheckProviders;
  WriteLn('Module providers: passed');
end.
