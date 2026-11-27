program NativeModuleTest;

{$mode delphiunicode}{$H+}

uses
  {$IFDEF UNIX}cthreads, BaseUnix,{$ENDIF}
  Classes, SysUtils,
  Goccia.Core,
  Goccia.Embed,
  Goccia.HostAPI,
  Goccia.Modules,
  Goccia.VM;

type
  TFactory = class
  public
    Calls, CycleCalls: Integer;
    function CreateExports(var AExports: TValue): Boolean;
    function InvalidExports(var AExports: TValue): Boolean;
    function MissingExports(var AExports: TValue): Boolean;
    function CyclicExports(var AExports: TValue): Boolean;
  end;

function Answer(AArgs: PValue; AArgc: Integer; var AResult: TValue): Boolean;
begin
  AResult := MkInt(42);
  Result := True;
end;

function TFactory.CreateExports(var AExports: TValue): Boolean;
var
  Base: Integer;
begin
  Inc(Calls);
  Base := TempTop;
  PushRoot(MkPtr(NewArray(0, 2)));
  try
    PushRoot(StrFromUnicode('answer'));
    PushRoot(NewNativeFunction(@Answer, 'answer', 0));
    CollectMinor;
    ArrayPush(ObjOf(RootAt(Base)), RootAt(Base + 1));
    ArrayPush(ObjOf(RootAt(Base)), RootAt(Base + 2));
    AExports := RootAt(Base);
  finally
    PopRoots(TempTop - Base);
  end;
  Result := True;
end;

function TFactory.InvalidExports(var AExports: TValue): Boolean;
begin
  PushRoot(MkPtr(NewArray(0, 2)));
  try
    ArrayPush(ObjOf(RootAt(TempTop - 1)), MkInt(1));
    ArrayPush(ObjOf(RootAt(TempTop - 1)), MkInt(42));
    AExports := RootAt(TempTop - 1);
  finally
    PopRoots(1);
  end;
  Result := True;
end;

function TFactory.MissingExports(var AExports: TValue): Boolean;
begin
  Result := True;
end;

{ Loads a dependency that imports this same module while the factory runs. The first two calls see that load
  refused and fail with it; later calls create the exports. }
function TFactory.CyclicExports(var AExports: TValue): Boolean;
var
  Namespace: TValue;
begin
  Inc(CycleCalls);
  if CycleCalls <= 2 then
  begin
    if EvaluateModuleNamespace('host:cycle-dependency', Namespace) then
      raise Exception.Create('a recursive load reached the native factory again');
    Exit(False);
  end;
  Result := CreateExports(AExports);
end;

procedure Check(ACondition: Boolean; const AMessage: UnicodeString);
begin
  if not ACondition then raise Exception.Create(AMessage);
end;

procedure CheckRecursiveFactory;
var
  Engine: TGocciaEngine;
  Factory: TFactory;
  ErrorText: UnicodeString;
  RunResult: TGocciaRunResult;
begin
  Factory := TFactory.Create;
  Engine := TGocciaEngine.Create;
  try
    Engine.ResultForm := grfString;
    Check(Engine.DefineNativeModule('host:cycle', Factory.CyclicExports, ErrorText), ErrorText);
    Check(Engine.DefineInlineVirtualModule(
      'host:cycle-dependency=import { answer } from "host:cycle"; export const value = answer;', '', ErrorText),
      ErrorText);
    RunResult := Engine.Run('cycle.mjs',
      'await import("host:cycle").then(() => "loaded", e => e instanceof TypeError && e.message);');
    Check(RunResult.Ok and (Pos('host:cycle is still being created', RunResult.ResultText) > 0) and
      (Factory.CycleCalls = 1), 'a recursive native load was not refused: ' + RunResult.ResultText + RunResult.ErrorMessage);
    RunResult := Engine.Run('cycle-again.mjs',
      'await import("host:cycle").then(() => "loaded", e => e instanceof TypeError && e.message);');
    Check(RunResult.Ok and (Pos('still being created', RunResult.ResultText) > 0) and (Factory.CycleCalls = 2),
      'a failed native factory was not run again: ' + RunResult.ResultText + RunResult.ErrorMessage);
    RunResult := Engine.Run('cycle-retry.mjs',
      'import * as first from "host:cycle"; const second = await import("host:cycle"); ' +
      '(first === second) + ":" + first.answer();');
    Check(RunResult.Ok and (RunResult.ResultText = 'true:42') and (Factory.CycleCalls = 3),
      'a retried native factory did not publish one module: ' + RunResult.ResultText + RunResult.ErrorMessage);
  finally
    Engine.Free;
    Factory.Free;
  end;
end;

procedure CheckRegistration(ABare: Boolean);
var
  Engine: TGocciaEngine;
  Factory: TFactory;
  ErrorText, Source: UnicodeString;
  RunResult: TGocciaRunResult;
begin
  Factory := TFactory.Create;
  Engine := TGocciaEngine.Create;
  try
    Engine.Bare := ABare;
    Engine.ResultForm := grfString;
    Engine.JSModuleSources := True;
    Check(not Engine.DefineNativeModule('/file.mjs', Factory.CreateExports, ErrorText), 'file name accepted');
    Check(not Engine.DefineNativeModule('host:typed'#1'text', Factory.CreateExports, ErrorText),
      'specifier contains the internal type separator');
    {$IFDEF WINDOWS}
    Check(not Engine.DefineNativeModule('\\?\C:\modules\file.mjs', Factory.CreateExports, ErrorText),
      'verbatim file path accepted as a virtual address');
    {$ENDIF}
    Check(Engine.DefineNativeModule('host:sample', Factory.CreateExports, ErrorText), ErrorText);
    Check(not Engine.DefineNativeModule('host:sample', Factory.CreateExports, ErrorText), 'duplicate accepted');
    if not ABare then
      Check(not Engine.DefineNativeModule('goccia:json5', Factory.CreateExports, ErrorText), 'built-in was replaced');
    Check(Engine.DefineInlineVirtualModule('host:source=export const value = 1;', '', ErrorText), ErrorText);
    Check(not Engine.DefineNativeModule('host:source', Factory.CreateExports, ErrorText),
      'source module was replaced by a native module');
    Check(not Engine.DefineInlineVirtualModule('host:sample=export const value = 1;', '', ErrorText),
      'native module was replaced by a source module');
    Check(Pos('conflicts with a registered host module', ErrorText) > 0, ErrorText);
    RunResult := Engine.Run('typed-native.mjs', 'import value from "host:sample" with { type: "text" }; value;');
    Check(not RunResult.Ok and (Factory.Calls = 0), 'a typed import ran a native factory');
    RunResult := Engine.Run('static-source.mjs', 'import source original from "host:sample"; original;');
    Check(not RunResult.Ok and (RunResult.ErrorName = 'SyntaxError') and (Factory.Calls = 0) and
      (Pos('no source phase representation', RunResult.ErrorText) > 0), 'a static source import ran a native factory');
    RunResult := Engine.Run('dynamic-source.mjs',
      'await import.source("host:sample").then(() => false, e => e instanceof SyntaxError);');
    Check(RunResult.Ok and (RunResult.ResultText = 'true') and (Factory.Calls = 0), 'a dynamic source import ran a native factory');
    if not ABare then
    begin
      RunResult := Engine.Run('typed-source.mjs', 'import source from "host:source" with { type: "text" }; source;');
      Check(RunResult.Ok and (RunResult.ResultText = 'export const value = 1;'),
        'typed source lookup changed: ' + RunResult.ErrorMessage);
    end;
    Source := 'import * as first from "host:sample"; globalThis.firstNative = first;';
    if not ABare then Source += 'import {parse} from "goccia:json5"; parse("{ok:true}");';
    RunResult := Engine.Run('module.mjs', Source + 'first.answer();');
    Check(RunResult.Ok and (RunResult.ResultText = '42'), 'native module did not link: ' + RunResult.ErrorMessage);
    RunResult := Engine.Run('second.mjs',
      'import * as second from "host:sample"; globalThis.firstNative === second;');
    Check(RunResult.Ok and (RunResult.ResultText = 'true') and (Factory.Calls = 1),
      'native module identity changed between entries');
    Check(Engine.DefineNativeModule('host:mixed', Factory.CreateExports, ErrorText), ErrorText);
    RunResult := Engine.Run('mixed-source.mjs',
      'import source original from "host:mixed"; import { answer } from "host:mixed"; answer;');
    Check(not RunResult.Ok and (RunResult.ErrorName = 'SyntaxError') and (Factory.Calls = 2),
      'a mixed request lost its evaluation lookup');
    Check(Engine.DefineNativeModule('host:no-output', Factory.MissingExports, ErrorText), ErrorText);
    RunResult := Engine.Run('missing-exports.mjs',
      'await import("host:no-output").then(() => false, e => e instanceof TypeError);');
    Check(RunResult.Ok and (RunResult.ResultText = 'true'), 'a missing factory result was not refused');
    Check(Engine.DefineNativeModule('host:invalid', Factory.InvalidExports, ErrorText), ErrorText);
    RunResult := Engine.Run('invalid.mjs',
      'const first = await import("host:invalid").then(() => false, e => e instanceof TypeError);' +
      'const second = await import("host:invalid").then(() => false, e => e instanceof TypeError); first && second;');
    Check(RunResult.Ok and (RunResult.ResultText = 'true'), 'invalid exports left a published module');
  finally
    Engine.Free;
    Factory.Free;
  end;
  Engine := TGocciaEngine.Create;
  try
    RunResult := Engine.Run('next.mjs', 'import "host:sample";');
    Check(not RunResult.Ok, 'a native module survived engine teardown');
  finally
    Engine.Free;
  end;
end;

procedure CheckOptionalHostConflicts;
var
  Engine: TGocciaEngine;
  Factory: TFactory;
  ErrorText: UnicodeString;
begin
  Factory := TFactory.Create;
  Engine := TGocciaEngine.Create;
  try
    Engine.TestMode := True;
    Check(Engine.DefineNativeModule('goccia:test', Factory.CreateExports, ErrorText), ErrorText);
    Check(not InstallTestingAPI(ErrorText) and (Pos('goccia:test', ErrorText) > 0), ErrorText);
    Check(Engine.DefineNativeModule('goccia:microbench', Factory.CreateExports, ErrorText), ErrorText);
    Check(not InstallBenchmarkAPI(ErrorText) and (Pos('goccia:microbench', ErrorText) > 0), ErrorText);
    Check(not Engine.DefineInlineVirtualModule('goccia:microbench=export const value = 1;', '', ErrorText) and
      (Pos('conflicts with a registered host module', ErrorText) > 0), ErrorText);
    Check(Factory.Calls = 0, 'rejecting a conflicting registration ran the factory');
  finally
    Engine.Free;
    Factory.Free;
  end;
end;

{$IFDEF UNIX}
procedure CheckBareFileIdentity;
var
  Engine: TGocciaEngine;
  Factory: TFactory;
  Lines: TStringList;
  Directory, SourcePath, AliasPath: string;
  ErrorText: UnicodeString;
  RunResult: TGocciaRunResult;
begin
  Directory := IncludeTrailingPathDelimiter(ExtractFileDir(ExpandFileName(ParamStr(0))));
  SourcePath := Directory + 'native-test-source.mjs';
  AliasPath := Directory + 'native-test-alias.txt';
  Lines := TStringList.Create;
  Factory := TFactory.Create;
  Engine := TGocciaEngine.Create;
  try
    Lines.Text := 'export const answer = 42;';
    Lines.SaveToFile(SourcePath);
    Check(fpSymlink(PAnsiChar(UTF8Encode(SourcePath)), PAnsiChar(UTF8Encode(AliasPath))) = 0, 'could not create test alias');
    Engine.Bare := True;
    Engine.SourceKind := gskModule;
    Engine.ResultForm := grfString;
    Check(Engine.DefineNativeModule('host:unrelated', Factory.CreateExports, ErrorText), ErrorText);
    RunResult := Engine.Run(SourcePath, Lines.Text);
    Check(RunResult.Ok, 'bare source failed: ' + RunResult.ErrorMessage);
    RunResult := Engine.Run(Directory + 'native-test-entry.mjs',
      'import { answer } from "./native-test-alias.txt"; answer;');
    Check(RunResult.Ok and (RunResult.ResultText = '42') and (Factory.Calls = 0),
      'native registration changed bare file identity: ' + RunResult.ErrorMessage);
  finally
    Engine.Free;
    Factory.Free;
    Lines.Free;
    DeleteFile(AliasPath);
    DeleteFile(SourcePath);
  end;
end;
{$ENDIF}

begin
  {$IFDEF UNIX}CheckBareFileIdentity;{$ENDIF}
  CheckOptionalHostConflicts;
  CheckRecursiveFactory;
  CheckRegistration(False);
  CheckRegistration(True);
  WriteLn('Native module factories: passed');
end.
