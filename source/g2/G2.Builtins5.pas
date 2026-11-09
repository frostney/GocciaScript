unit G2.Builtins5;
{ Array.fromAsync, Object.groupBy / Map.groupBy, Map and WeakMap getOrInsert / getOrInsertComputed, written in JS
  (src/prelude/builtins5.js, turned into G2.Builtins5.inc by t/lang/gen_prelude_inc.py) and compiled once into
  the prelude snapshot. }
{$mode delphiunicode}{$H+}

interface

procedure InitBuiltins5;

implementation

uses
  SysUtils, G2.Core, G2.Bytecode, G2.VM, G2.Snapshot, G2.RegExpV;

{$I G2.Builtins5.inc}

procedure InitBuiltins5;
var T: PFuncTemplate; Err: UnicodeString; Clo, Fn, R: TValue; CA: array[0..1] of TValue; Ok: Boolean;
begin
  if not CompilePrelude('<builtins5>', Builtins5Src, T, Err) then
    raise Exception.Create('builtins5 prelude: ' + string(Err));
  if not MakeScriptClosure(T, Clo) then raise Exception.Create('builtins5 prelude closure');
  if not CallValue(Clo, VUndefined, nil, 0, Fn) then raise Exception.Create('builtins5 prelude run');
  PushRoot(Fn);
  PushRoot(NewNativeFunction(MarkIntrinsicClass, 'markIntrinsic', 1));
  CA[0] := GlobalObj;
  CA[1] := RootAt(TempTop - 1);
  Ok := CallValue(RootAt(TempTop - 2), VUndefined, @CA[0], 2, R);
  PopRoots(1);
  if not Ok then
  begin
    PopRoots(1);
    if ToStringV(Pending, R) and IsString(R) then
      raise Exception.Create('builtins5 prelude call: ' + string(StrToUnicode(StrOf(R))));
    raise Exception.Create('builtins5 prelude call');
  end;
  PopRoots(1);
  InitRegExpPrelude;
end;

end.
