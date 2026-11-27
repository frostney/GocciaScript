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

{ the permissions of the mapping that holds AAddress, '' when none does }
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

{ runs the hot loop on AEngine and returns the hot function's compiled code, checked read-execute }
function RunFixture(AEngine: TGocciaEngine; const AFileName: string): PtrUInt;
var
  Profile: TLangProfile;
  RunResult: TGocciaRunResult;
  Closure: TValue;
  Permissions: UnicodeString;
begin
  Profile := AEngine.Profile;
  Profile.ForLoop := True;
  AEngine.Profile := Profile;
  AEngine.ResultForm := grfString;
  RunResult := AEngine.Run(AFileName,
    'globalThis.hot = (n) => n + 3;' +
    'let sum = 0; for (let i = 0; i < 5000; i++) sum += hot(i); sum;');
  Check(RunResult.Ok and (RunResult.ResultText = '12512500'),
    'JIT fixture did not finish: ' + RunResult.ResultText + RunResult.ErrorMessage);
  Check(GetV(GlobalObj, AtomOf('hot'), Closure) and IsObject(Closure) and
    (ObjOf(Closure)^.OKind = okFunction), 'missing fixture bytecode closure');
  Result := PtrUInt(PFuncTemplate(ObjOf(Closure)^.Native)^.Jit);
  Check(Result <> 0, 'fixture was not compiled');
  Permissions := MappingPermissions(Result);
  Check(Permissions = 'r-xp', 'compiled code permissions are ' + Permissions);
end;

procedure CheckEngine;
var
  Engine: TGocciaEngine;
  CodeAddress: PtrUInt;
begin
  Engine := TGocciaEngine.Create;
  try
    CodeAddress := RunFixture(Engine, 'jit-permissions.js');
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
begin
  try
    JitTakeThread;
    { the engine is never freed }
    CodeAddress := RunFixture(TGocciaEngine.Create, 'jit-thread.js');
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
