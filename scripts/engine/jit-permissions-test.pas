program JitPermissionsTest;

{$mode delphiunicode}{$H+}

uses
  cthreads,
  SysUtils,
  Goccia.Core,
  Goccia.Embed,
  Goccia.Instructions,
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
      'let sum = 0; for (let i = 0; i < 100; i++) sum += hot(i); sum;');
    Check(RunResult.Ok and (RunResult.ResultText = '5250'),
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

begin
  CheckEngine;
  CheckEngine;
  WriteLn('JIT permissions: passed');
end.
