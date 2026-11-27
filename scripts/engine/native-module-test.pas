program NativeModuleTest;

{$mode delphiunicode}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  SysUtils,
  Goccia.Core,
  Goccia.Embed,
  Goccia.VM;

type
  TFactory = class
  public
    Calls: Integer;
    function CreateExports(var AExports: TValue): Boolean;
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
    ArrayPush(ObjOf(RootAt(Base)), RootAt(Base + 1));
    ArrayPush(ObjOf(RootAt(Base)), RootAt(Base + 2));
    AExports := RootAt(Base);
  finally
    PopRoots(TempTop - Base);
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
    Check(Engine.DefineNativeModule('host:sample', Factory.CreateExports, ErrorText), ErrorText);
    Check(not Engine.DefineNativeModule('host:sample', Factory.CreateExports, ErrorText), 'duplicate accepted');
    if not ABare then
    begin
      Check(not Engine.DefineNativeModule('goccia:json5', Factory.CreateExports, ErrorText), 'built-in was replaced');
      Check(Engine.DefineInlineVirtualModule('host:source=export const value = 1;', '', ErrorText), ErrorText);
      Check(not Engine.DefineNativeModule('host:source', Factory.CreateExports, ErrorText),
        'source module was replaced by a native module');
      Check(not Engine.DefineInlineVirtualModule('host:sample=export const value = 1;', '', ErrorText),
        'native module was replaced by a source module');
      Check(Pos('conflicts with a registered host module', ErrorText) > 0, ErrorText);
    end;
    Check(Factory.Calls = 0, 'factory ran before import');
    Source := 'import * as first from "host:sample"; import * as second from "host:sample";';
    if not ABare then Source += 'import {parse} from "goccia:json5"; parse("{ok:true}");';
    RunResult := Engine.Run('module.mjs', Source + '(first === second) ? first.answer() : 0;');
    Check(RunResult.Ok and (RunResult.ResultText = '42'), 'native module did not link: ' + RunResult.ErrorMessage);
    Check(Factory.Calls = 1, 'native module identity changed between imports');
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

begin
  CheckRegistration(False);
  CheckRegistration(True);
  WriteLn('Native module factories: passed');
end.
