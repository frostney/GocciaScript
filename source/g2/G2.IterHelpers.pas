unit G2.IterHelpers;
{ 27.1 Iterator: the Iterator constructor and statics, %Iterator.prototype% helpers and their prototypes, written in
  JS (src/prelude/iterhelpers.js, turned into G2.IterHelpers.inc by t/lang/gen_prelude_inc.py) and compiled once
  into the prelude snapshot like G2.Builtins4's prelude. }
{$mode delphiunicode}{$H+}

interface

procedure InitIterHelpers;

implementation

uses
  SysUtils, G2.Core, G2.Bytecode, G2.VM, G2.Snapshot;

{$I G2.IterHelpers.inc}

procedure InitIterHelpers;
var T: PFuncTemplate; Err: UnicodeString; Clo, Fn, R: TValue;
begin
  if not CompilePrelude('<iterhelpers>', IterHelpersSrc, T, Err) then
    raise Exception.Create('iterator helpers prelude: ' + string(Err));
  if not MakeScriptClosure(T, Clo) then raise Exception.Create('iterator helpers prelude closure');
  if not CallValue(Clo, VUndefined, nil, 0, Fn) then raise Exception.Create('iterator helpers prelude run');
  PushRoot(Fn);
  if not CallValue(RootAt(TempTop - 1), VUndefined, @GlobalObj, 1, R) then
  begin
    PopRoots(1);
    if ToStringV(Pending, R) and IsString(R) then
      raise Exception.Create('iterator helpers prelude call: ' + string(StrToUnicode(StrOf(R))));
    raise Exception.Create('iterator helpers prelude call');
  end;
  PopRoots(1);
end;

end.
