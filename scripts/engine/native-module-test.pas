program NativeModuleTest;

{$mode delphiunicode}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  SysUtils,
  Goccia.Core,
  Goccia.Embed,
  Goccia.HostAPI,
  Goccia.VM;

type
  TFactory = class
  public
    Calls: Integer;
    function CreateExports(var AExports: TValue): Boolean;
    function InvalidExports(var AExports: TValue): Boolean;
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

procedure Check(ACondition: Boolean; const AMessage: UnicodeString);
begin
  if not ACondition then raise Exception.Create(AMessage);
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
    Check(not Engine.DefineNativeModule('/file.mjs', Factory.CreateExports, ErrorText), 'file name accepted');
    Check(not Engine.DefineNativeModule('host:typed'#1'text', Factory.CreateExports, ErrorText),
      'specifier contains the internal type separator');
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

begin
  CheckOptionalHostConflicts;
  CheckRegistration(False);
  CheckRegistration(True);
  WriteLn('Native module factories: passed');
end.
