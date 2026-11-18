unit G2.Builtins5;
{ Array.fromAsync, Object.groupBy / Map.groupBy, Map and WeakMap getOrInsert / getOrInsertComputed, written in JS
  (src/prelude/builtins5.js, turned into G2.Builtins5.inc by t/lang/gen_prelude_inc.py) and compiled once into
  the prelude snapshot. }
{$mode delphiunicode}{$H+}
{$MINFPCONSTPREC 64}

interface

procedure InitBuiltins5;

implementation

uses
  SysUtils, G2.Core, G2.Bytecode, G2.VM, G2.Snapshot, G2.RegExpV, G2.Weak;

{$I G2.Builtins5.inc}

{ argument I (1-based; Args[0] is this) }
function ArgAt(Args: PValue; Argc, I: Integer): TValue; inline;
begin
  if I <= Argc then Result := PValue(PByte(Args) + I * SizeOf(TValue))^ else Result := VUndefined;
end;

{ privGet(o, privateName), privSet(o, privateName, v), privHas(o, privateName): a private element's read, write and
  presence test (as o.#x, o.#x = v and #x in o), for the decorator runtime's context.access }
function Priv_Get(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
begin
  Result := PrivateGet(ArgAt(Args, Argc, 1), PStr(PtrOf(ArgAt(Args, Argc, 2))), Res);
end;

function Priv_Set(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
begin
  Res := VUndefined;
  Result := PrivateSet(ArgAt(Args, Argc, 1), PStr(PtrOf(ArgAt(Args, Argc, 2))), ArgAt(Args, Argc, 3));
end;

function Priv_Has(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var O: TValue;
begin
  O := ArgAt(Args, Argc, 1);
  if not IsObject(O) then
    Exit(ThrowTypeError('Cannot use ''in'' operator to search for ''' +
      StrToUnicode(PStr(PtrOf(ArgAt(Args, Argc, 2)))) + ''' in ' + ValueDisplayText(O)));
  Res := MkBool(ShapeLookup(ObjOf(O)^.Shape, PStr(PtrOf(ArgAt(Args, Argc, 2)))) >= 0);
  Result := True;
end;

procedure InitBuiltins5;
var T: PFuncTemplate; Err: UnicodeString; Clo, Fn, R: TValue; CA: array[0..8] of TValue; Ok: Boolean; I: Integer;
begin
  if not CompilePrelude('<builtins5>', Builtins5Src, T, Err) then
    raise Exception.Create('builtins5 prelude: ' + string(Err));
  if not MakeScriptClosure(T, Clo) then raise Exception.Create('builtins5 prelude closure');
  if not CallValue(Clo, VUndefined, nil, 0, Fn) then raise Exception.Create('builtins5 prelude run');
  PushRoot(Fn);
  PushRoot(NewNativeFunction(MarkIntrinsicClass, 'markIntrinsic', 1));
  PushRoot(NewNativeFunction(Priv_Get, 'privGet', 2));
  PushRoot(NewNativeFunction(Priv_Set, 'privSet', 3));
  PushRoot(NewNativeFunction(Priv_Has, 'privHas', 2));
  { G2.Weak: WeakRef and FinalizationRegistry }
  PushRoot(NewNativeFunction(WeakBoxNative, 'weakBox', 2));
  PushRoot(NewNativeFunction(WeakGetNative, 'weakGet', 1));
  PushRoot(NewNativeFunction(WeakKeepNative, 'weakKeep', 1));
  PushRoot(NewNativeFunction(WeakSetCleanupNative, 'weakSetCleanup', 1));
  CA[0] := GlobalObj;
  for I := 1 to 8 do CA[I] := RootAt(TempTop - 9 + I);
  Ok := CallValue(RootAt(TempTop - 9), VUndefined, @CA[0], 9, R);
  PopRoots(8);
  if not Ok then
  begin
    PopRoots(1);
    if ToStringV(Pending, R) and IsString(R) then
      raise Exception.Create('builtins5 prelude call: ' + string(StrToUnicode(StrOf(R))));
    raise Exception.Create('builtins5 prelude call');
  end;
  PopRoots(1);
  { the prelude's internals: opIntrinsic's helpers, an array in INTRINSIC_* order }
  PushRoot(R);
  for I := 0 to INTRINSIC_COUNT - 1 do
  begin
    AddRoot(@Intrinsics[I]);
    if not GetV(RootAt(TempTop - 1), AtomOf(UnicodeString(IntToStr(I))), Intrinsics[I]) or not IsCallable(Intrinsics[I]) then
      raise Exception.Create('builtins5 prelude internals');
  end;
  PopRoots(1);
  InitRegExpPrelude;
end;

end.
