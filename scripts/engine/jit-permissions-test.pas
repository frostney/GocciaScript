program JitPermissionsTest;

{$mode delphiunicode}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes,
  SysUtils,
  Goccia.Core,
  Goccia.Embed,
  Goccia.Instructions,
  Goccia.ThreadCleanupRegistry,
  Goccia.VM;

procedure Check(ACondition: Boolean; const AMessage: UnicodeString);
begin
  if not ACondition then raise Exception.Create(AMessage);
end;

function MappingPermissions(AAddress: PtrUInt): UnicodeString;
var
  Maps: TextFile;
  Line: UnicodeString;
  Dash, Space: Integer;
  FirstAddress, EndAddress: QWord;
begin
  Result := '';
  AssignFile(Maps, '/proc/self/maps');
  Reset(Maps);
  try
    while not Eof(Maps) do
    begin
      ReadLn(Maps, Line);
      Dash := Pos('-', Line);
      Space := Pos(' ', Line);
      if (Dash = 0) or (Space < Dash) then Continue;
      FirstAddress := StrToQWord('$' + Copy(Line, 1, Dash - 1));
      EndAddress := StrToQWord('$' + Copy(Line, Dash + 1, Space - Dash - 1));
      if (FirstAddress <= AAddress) and (AAddress < EndAddress) then
        Exit(Copy(Line, Space + 1, 4));
    end;
  finally
    CloseFile(Maps);
  end;
end;

procedure CheckEngine;
var
  Engine: TGocciaEngine;
  Profile: TLangProfile;
  RunResult: TGocciaRunResult;
  Closure: TValue;
  Template: PFuncTemplate;
  CodeAddress: PtrUInt;
  Permissions: UnicodeString;
begin
  Engine := TGocciaEngine.Create;
  try
    Profile := Engine.Profile;
    Profile.ForLoop := True;
    Engine.Profile := Profile;
    Engine.ResultForm := grfString;
    RunResult := Engine.Run('jit-permissions.js',
      'globalThis.hot = (n) => n + 3;' +
      'let sum = 0; for (let i = 0; i < 5000; i++) sum += hot(i); sum;');
    Check(RunResult.Ok and (RunResult.ResultText = '12512500'),
      'JIT fixture did not finish: ' + RunResult.ErrorMessage);
    Check(GetV(GlobalObj, AtomOf('hot'), Closure) and IsObject(Closure) and
      (ObjOf(Closure)^.OKind = okFunction), 'missing fixture bytecode closure');
    Template := PFuncTemplate(ObjOf(Closure)^.Native);
    CodeAddress := PtrUInt(Template^.Jit);
    Check(CodeAddress <> 0, 'fixture was not compiled');
    Permissions := MappingPermissions(CodeAddress);
    Check(Permissions = 'r-xp', 'compiled code permissions are ' + Permissions);
  finally
    Engine.Free;
  end;
  Check(MappingPermissions(CodeAddress) = '', 'compiled code survived engine teardown');
end;

type
  { a thread that takes over the JIT, compiles code and ends without freeing its engine, as a host that skips
    teardown does: its thread cleanup unmaps the code }
  TLeakingEngineThread = class(TThread)
  public
    CodeAddress: PtrUInt;
    Failure: UnicodeString;
  protected
    procedure Execute; override;
  end;

procedure TLeakingEngineThread.Execute;
var
  Engine: TGocciaEngine;
  Profile: TLangProfile;
  RunResult: TGocciaRunResult;
  Closure: TValue;
begin
  try
    JitTakeThread;
    Engine := TGocciaEngine.Create;
    Profile := Engine.Profile;
    Profile.ForLoop := True;
    Engine.Profile := Profile;
    Engine.ResultForm := grfString;
    RunResult := Engine.Run('jit-thread.js',
      'globalThis.hot = (n) => n + 3;' +
      'let sum = 0; for (let i = 0; i < 5000; i++) sum += hot(i); sum;');
    if not RunResult.Ok then Failure := 'thread fixture did not finish: ' + RunResult.ErrorMessage
    else if GetV(GlobalObj, AtomOf('hot'), Closure) and IsObject(Closure) and (ObjOf(Closure)^.OKind = okFunction) then
      CodeAddress := PtrUInt(PFuncTemplate(ObjOf(Closure)^.Native)^.Jit);
    if (Failure = '') and (CodeAddress = 0) then Failure := 'thread fixture was not compiled';
    if (Failure = '') and (MappingPermissions(CodeAddress) <> 'r-xp') then
      Failure := 'thread code permissions are ' + MappingPermissions(CodeAddress);
    { the engine is never freed }
    RunThreadvarCleanups;
  except
    on E: Exception do Failure := UnicodeString(E.Message);
  end;
end;

procedure CheckThreadCleanup;
var Thread: TLeakingEngineThread;
begin
  Thread := TLeakingEngineThread.Create(True);
  try
    Thread.Start;
    Thread.WaitFor;
    Check(Thread.Failure = '', Thread.Failure);
    Check(MappingPermissions(Thread.CodeAddress) = '', 'compiled code survived the thread''s cleanup');
  finally
    Thread.Free;
  end;
  JitTakeThread;
end;

begin
  CheckEngine;
  CheckEngine;
  CheckThreadCleanup;
  CheckEngine;
  WriteLn('JIT permissions: passed');
end.
