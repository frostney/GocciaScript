unit G2.Weak;

{$mode objfpc}{$H+}

{ Weak references for WeakRef and FinalizationRegistry (ECMA-262 9.9-9.13, 26.1, 26.2).

  A weak box is an internal object (no prototype, not extensible) whose target is kept in its Native field, which the
  collector does not trace. Every box is listed here; after each collection's marking the hook clears the target of
  a box whose target died, or updates it when the target moved out of the nursery. A box made for a
  FinalizationRegistry cell marks the cleanup as pending when its target dies; the event loop then runs the
  registries' cleanup one job at a time (RunWeakCleanup).

  [[KeptAlive]] (9.10 AddToKeptObjects): the targets a WeakRef constructor or deref returned stay alive until the
  current job ends; ClearKeptObjects empties the list when the next job starts. }

interface

uses
  G2.Core;

{ the natives the builtins5 prelude receives: newBox(target, forCleanup), get(box), keep(target),
  setCleanup(fn) }
function WeakBoxNative(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
function WeakGetNative(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
function WeakKeepNative(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
function WeakSetCleanupNative(Args: PValue; Argc: Integer; var Res: TValue): Boolean;

{ 9.10 ClearKeptObjects: at the start of each job }
procedure ClearKeptObjects;
{ a FinalizationRegistry target died since the last cleanup job }
function WeakCleanupPending: Boolean;
{ one cleanup job (the registries' callback for one dead cell); False when it threw (Pending holds the error) }
function RunWeakCleanup: Boolean;

implementation

uses
  G2.VM;

const
  WEAK_MAGIC = $57454100;   { 'WEA' and the box kind in the low byte }
  BOX_REF = 0;
  BOX_CELL = 1;

type
  PWeakState = ^TWeakState;
  TWeakState = record
    Boxes: array of Pointer;
    Count: Integer;
    Kept: TValue;           { a JS array: [[KeptAlive]] }
    CleanupFns: TValue;     { a JS array: each realm's cleanup function }
    CleanupPending: Boolean;
  end;

threadvar
  WeakStateVar: PWeakState;

{ the agent's state (one threadvar read in a statement of its own: FPC 3.3.1+4 can drop the %fs prefix of a second
  one in an expression) }
function WeakStateNow: PWeakState; noinline;
begin
  Result := WeakStateVar;
end;

{ after marking (and evacuation): a box that died is dropped, a dead target cleared, a moved one followed }
procedure WeakRefHook;
var S: PWeakState; I, N: Integer; B, T: Pointer;
begin
  S := WeakStateNow;
  if (S = nil) or (S^.Count = 0) then Exit;
  N := 0;
  for I := 0 to S^.Count - 1 do
  begin
    B := S^.Boxes[I];
    if not GCSurvives(B) then Continue;
    T := PObj(B)^.Native;
    if T <> nil then
    begin
      if GCSurvives(T) then PObj(B)^.Native := T
      else
      begin
        PObj(B)^.Native := nil;
        if (PObj(B)^.Pad and $FF) = BOX_CELL then S^.CleanupPending := True;
      end;
    end;
    S^.Boxes[N] := B;
    Inc(N);
  end;
  S^.Count := N;
end;

function EnsureState: PWeakState;
begin
  Result := WeakStateNow;
  if Result <> nil then Exit;
  New(Result);
  Result^.Count := 0;
  SetLength(Result^.Boxes, 64);
  Result^.Kept := VUndefined;
  Result^.CleanupFns := VUndefined;
  Result^.CleanupPending := False;
  WeakStateVar := Result;
  AddRoot(@Result^.Kept);
  AddRoot(@Result^.CleanupFns);
  GCWeakRefHook := @WeakRefHook;
end;

function IsBox(V: TValue): Boolean;
begin
  Result := IsObject(V) and (ObjOf(V)^.OKind = okPlain) and ((ObjOf(V)^.Pad and $FFFFFF00) = WEAK_MAGIC);
end;

{ newBox(target, forCleanup): a box holding target weakly (an object or a symbol: the caller checked) }
function WeakBoxNative(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var S: PWeakState; O: PObj; Kind: UInt32;
begin
  S := EnsureState;
  if (Argc < 1) or not IsPtr(Args[1]) then Exit(ThrowTypeError('weak box: the target must be an object or a symbol'));
  if (Argc >= 2) and (Args[2] = VTrue) then Kind := BOX_CELL else Kind := BOX_REF;
  O := NewPlainObject(VNull, 0);
  O^.OFlags := O^.OFlags or 8;   { not extensible }
  O^.Pad := WEAK_MAGIC or Kind;
  O^.Native := PtrOf(Args[1]);   { Args is on the value stack: current after the allocation }
  if S^.Count >= Length(S^.Boxes) then SetLength(S^.Boxes, S^.Count * 2);
  S^.Boxes[S^.Count] := O;
  Inc(S^.Count);
  Res := MkPtr(O);
  Result := True;
end;

{ get(box): its target, or undefined once the target was collected }
function WeakGetNative(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
begin
  if (Argc < 1) or not IsBox(Args[1]) then Exit(ThrowTypeError('weak box expected'));
  if ObjOf(Args[1])^.Native = nil then Res := VUndefined else Res := MkPtr(ObjOf(Args[1])^.Native);
  Result := True;
end;

{ keep(target): 9.10 AddToKeptObjects }
function WeakKeepNative(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var S: PWeakState;
begin
  S := EnsureState;
  Res := VUndefined;
  if (Argc < 1) or not IsPtr(Args[1]) then Exit(True);
  if not IsObject(S^.Kept) then S^.Kept := MkPtr(NewArray(0, 8));
  ArrayPush(ObjOf(S^.Kept), Args[1]);
  Result := True;
end;

{ setCleanup(fn): a realm's cleanup function: runs one cleanup job, true when more may be waiting }
function WeakSetCleanupNative(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var S: PWeakState;
begin
  S := EnsureState;
  Res := VUndefined;
  if (Argc < 1) or not IsCallable(Args[1]) then Exit(True);
  if not IsObject(S^.CleanupFns) then S^.CleanupFns := MkPtr(NewArray(0, 2));
  ArrayPush(ObjOf(S^.CleanupFns), Args[1]);
  Result := True;
end;

procedure ClearKeptObjects;
var S: PWeakState;
begin
  S := WeakStateNow;
  if (S <> nil) and IsObject(S^.Kept) and (ArrayLength(ObjOf(S^.Kept)) > 0) then ArraySetLength(ObjOf(S^.Kept), 0);
end;

function WeakCleanupPending: Boolean;
var S: PWeakState;
begin
  S := WeakStateNow;
  Result := (S <> nil) and S^.CleanupPending;
end;

function RunWeakCleanup: Boolean;
var S: PWeakState; I: Integer; R, F: TValue; More: Boolean;
begin
  Result := True;
  S := WeakStateNow;
  if (S = nil) or not S^.CleanupPending then Exit;
  S^.CleanupPending := False;
  if not IsObject(S^.CleanupFns) then Exit;
  More := False;
  for I := 0 to Integer(ArrayLength(ObjOf(S^.CleanupFns))) - 1 do
  begin
    F := ObjOf(S^.CleanupFns)^.Elements^.Data[I];
    if not CallValue(F, VUndefined, nil, 0, R) then
    begin
      S^.CleanupPending := True;   { the other cells still wait }
      Exit(False);
    end;
    if R = VTrue then
    begin
      More := True;
      Break;   { one job at a time: microtasks run before the next }
    end;
  end;
  if More then S^.CleanupPending := True;
end;

end.
