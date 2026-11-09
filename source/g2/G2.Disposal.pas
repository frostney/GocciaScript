unit G2.Disposal;
{ Explicit resource management: SuppressedError, DisposableStack, AsyncDisposableStack, %AsyncIteratorPrototype%
  [@@asyncDispose] and the helpers the compiler calls for using / await using blocks, written in JS
  (src/prelude/disposal.js -> G2.Disposal.inc, t/lang/gen_prelude_inc.py) and snapshotted. The helpers live on the
  global object under internal keys (G2.Core UsingKey): no script can name them. }
{$mode delphiunicode}{$H+}

interface

procedure InitDisposal;

implementation

uses
  SysUtils,

  G2.Bytecode,
  G2.Core,
  G2.Snapshot,
  G2.VM;

{$I G2.Disposal.inc}

function IsRetComp_Native(AArgs: PValue; AArgc: Integer; var ARes: TValue): Boolean;
begin
  ARes := MkBool((AArgc >= 1) and IsRetCompV(AArgs[1]));
  Result := True;
end;

procedure InitDisposal;
var T: PFuncTemplate; Err: UnicodeString; Clo, Fn, R, H: TValue; CA: array[0..1] of TValue; I: Integer;
const
  Names: array[0..2] of UnicodeString = ('add', 'dispose', 'disposeAsync');
begin
  if not CompilePrelude('<disposal>', DisposalSrc, T, Err) then raise Exception.Create('disposal prelude: ' + string(Err));
  if not MakeScriptClosure(T, Clo) then raise Exception.Create('disposal prelude closure');
  if not CallValue(Clo, VUndefined, nil, 0, Fn) then raise Exception.Create('disposal prelude run');
  PushRoot(Fn);
  CA[0] := GlobalObj;
  CA[1] := NewNativeFunction(IsRetComp_Native, 'isRetComp', 1);
  CA[0] := GlobalObj;
  if not CallValue(RootAt(TempTop - 1), VUndefined, @CA[0], 2, R) then
  begin
    PopRoots(1);
    if ToStringV(Pending, R) and IsString(R) then
      raise Exception.Create('disposal prelude call: ' + string(StrToUnicode(StrOf(R))));
    raise Exception.Create('disposal prelude call');
  end;
  PopRoots(1);
  PushRoot(R);
  for I := 0 to 2 do
  begin
    GetV(RootAt(TempTop - 1), AtomOf(Names[I]), H);
    DefineOwn(GlobalObj, UsingKey(I), H, 0);
  end;
  PopRoots(1);
end;

end.
