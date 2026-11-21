unit G2.Weak;

{$mode objfpc}{$H+}
{$I G2.Checks.inc}

{ Weak references for WeakRef and FinalizationRegistry (ECMA-262 9.9-9.13, 26.1, 26.2).

  A weak box is an internal object (no prototype, not extensible) whose target is kept in its Native field, which the
  collector does not trace. Every box is listed here; after each collection's marking the hook clears the target of
  a box whose target died, or updates it when the target moved out of the nursery. A box made for a
  FinalizationRegistry cell marks the cleanup as pending when its target dies; the event loop then runs the
  registries' cleanup one job at a time (RunWeakCleanup).

  [[KeptAlive]] (9.10 AddToKeptObjects): the targets a WeakRef constructor or deref returned stay alive until the
  current job ends; ClearKeptObjects empties the list when the next job starts.

  WeakMap and WeakSet keep their entries in an ephemeron table outside the heap, owned by a box: an entry's value
  is reachable only while its key is (24.3). In a collection, after marking drains, EphemeronHook visits the value
  of every entry whose key is alive in a live table, and flags the keys (and table owners) not reached yet
  (cfEphKey); marking notes a flagged cell when it reaches it, and the next call visits what waited on it, so a
  chain of keys holding the next key is followed in linear time. Then WeakRefHook drops the entries whose key died.
  A minor collection only looks at the entries made or changed with a young key or value since the last one (the
  table's Young list): every other entry holds old cells, which a minor collection neither moves nor frees. }

interface

uses
  G2.Core;

{ the natives the builtins5 prelude receives: newBox(target, forCleanup), get(box), keep(target),
  setCleanup(fn) }
function WeakBoxNative(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
function WeakGetNative(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
function WeakKeepNative(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
function WeakSetCleanupNative(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
{ the ephemeron tables of WeakMap and WeakSet: tableNew(), tableGet(t, key), tableHas(t, key),
  tableSet(t, key, value), tableDelete(t, key) (the caller checked that key can be held weakly) }
function TableNewNative(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
function TableGetNative(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
function TableHasNative(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
function TableSetNative(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
function TableDeleteNative(Args: PValue; Argc: Integer; var Res: TValue): Boolean;

{ 9.10 ClearKeptObjects: at the start of each job }
procedure ClearKeptObjects;
{ a FinalizationRegistry target died since the last cleanup job }
function WeakCleanupPending: Boolean;
{ whether a FinalizationRegistry cell still waits on its target (a collection could find it dead) }
function WeakCellsWaiting: Boolean;
{ one cleanup job (the registries' callback for one dead cell); False when it threw (Pending holds the error) }
function RunWeakCleanup: Boolean;

implementation

uses
  Generics.Collections, G2.VM;

const
  WEAK_MAGIC = $57454100;   { 'WEA' and the box kind in the low byte }
  BOX_REF = 0;
  BOX_CELL = 1;
  BOX_TABLE = 2;

type
  TPtrIntMap = specialize TDictionary<Pointer, Integer>;
  TEphEntry = record
    Key: Pointer;      { nil: a deleted entry }
    Value: TValue;
    Visited: Integer;  { the collection (GCEpochNow) that visited the value }
  end;
  PEphTable = ^TEphTable;
  TEphTable = record
    Box: Pointer;                          { the owner (BOX_TABLE): the table lives as long as it }
    Entries: array of TEphEntry;
    Count, Live: Integer;                  { entries used (deleted ones included), and live }
    Index: TPtrIntMap;  { key -> entry }
    Young: array of Integer;               { entries set with a young key or value since the last minor collection }
    YoungCount: Integer;
  end;
  TWatch = record
    Table: PEphTable;
    Idx: Integer;   { the entry waiting on the cell; -1: the table, waiting on its owner }
    Next: Integer;
  end;

  PWeakState = ^TWeakState;
  TWeakState = record
    Boxes: array of Pointer;
    Count: Integer;
    Kept: TValue;           { a JS array: [[KeptAlive]] }
    CleanupFns: TValue;     { a JS array: each realm's cleanup function }
    CleanupPending: Boolean;
    Tables: array of PEphTable;
    TableCount: Integer;
    EphEpoch: Integer;      { the collection the watches belong to }
    WatchHead: TPtrIntMap;
    Watches: array of TWatch;
    WatchCount: Integer;
    EphChanged: Boolean;
  end;

threadvar
  WeakStateVar: PWeakState;
  { the engine WeakStateVar belongs to (G2.Core FirstInEngine): a state of the thread's previous engine holds
    values of a heap that is gone }
  WeakStateGen: Integer;

{ the agent's state (one threadvar read in a statement of its own: FPC 3.3.1+4 can drop the %fs prefix of a second
  one in an expression) }
function WeakStateNow: PWeakState; noinline;
begin
  Result := WeakStateVar;
  if (Result <> nil) and not SameEngine(WeakStateGen) then Result := nil;
end;

{ ---- ephemerons ---- }

procedure Watch(S: PWeakState; P: Pointer; T: PEphTable; Idx: Integer);
var H: Integer;
begin
  PCell(P)^.Flags := PCell(P)^.Flags or cfEphKey;
  if S^.WatchCount >= Length(S^.Watches) then SetLength(S^.Watches, S^.WatchCount * 2 + 64);
  S^.Watches[S^.WatchCount].Table := T;
  S^.Watches[S^.WatchCount].Idx := Idx;
  if S^.WatchHead.TryGetValue(P, H) then S^.Watches[S^.WatchCount].Next := H
  else S^.Watches[S^.WatchCount].Next := -1;
  S^.WatchHead.AddOrSetValue(P, S^.WatchCount);
  Inc(S^.WatchCount);
end;

{ an entry of a live table: its value visited once its key is alive, else the key watched }
procedure ProcessEntry(S: PWeakState; T: PEphTable; I, Epoch: Integer);
var K: Pointer;
begin
  K := T^.Entries[I].Key;
  if (K = nil) or (T^.Entries[I].Visited = Epoch) then Exit;
  if GCSurvives(K) then
  begin
    T^.Entries[I].Visited := Epoch;
    GCVisitValue(T^.Entries[I].Value);
    S^.EphChanged := True;
  end
  else
    Watch(S, T^.Entries[I].Key, T, I);
end;

{ a table: its entries processed once its owner is alive, else the owner watched }
procedure ProcessTable(S: PWeakState; T: PEphTable; Epoch: Integer);
var B: Pointer; I: Integer;
begin
  B := T^.Box;
  if not GCSurvives(B) then
  begin
    Watch(S, T^.Box, T, -1);
    Exit;
  end;
  if GCInMinor then
  begin
    for I := 0 to T^.YoungCount - 1 do ProcessEntry(S, T, T^.Young[I], Epoch);
  end
  else
    for I := 0 to T^.Count - 1 do ProcessEntry(S, T, I, Epoch);
end;

function EphemeronHook: Boolean;
var S: PWeakState; Epoch, I, N, H: Integer; P: Pointer;
begin
  Result := False;
  S := WeakStateNow;
  if (S = nil) or (S^.TableCount = 0) then Exit;
  Epoch := GCEpochNow;
  S^.EphChanged := False;
  if S^.EphEpoch <> Epoch then
  begin
    { this collection's first call: every table }
    S^.EphEpoch := Epoch;
    S^.WatchHead.Clear;
    S^.WatchCount := 0;
    GCEphKeysClear;
    for I := 0 to S^.TableCount - 1 do ProcessTable(S, S^.Tables[I], Epoch);
  end
  else
  begin
    { what waited on the cells marking has reached since (visiting a value here can reach more: they join the
      list, and the loop takes them too) }
    I := 0;
    while I < GCEphKeyCount do
    begin
      P := GCEphKeyAt(I);
      Inc(I);
      if not S^.WatchHead.TryGetValue(P, H) then Continue;
      S^.WatchHead.Remove(P);
      while H >= 0 do
      begin
        if S^.Watches[H].Idx < 0 then ProcessTable(S, S^.Watches[H].Table, Epoch)
        else ProcessEntry(S, S^.Watches[H].Table, S^.Watches[H].Idx, Epoch);
        H := S^.Watches[H].Next;
      end;
    end;
    GCEphKeysClear;
  end;
  Result := S^.EphChanged;
end;

procedure DeleteEntry(T: PEphTable; I: Integer);
begin
  T^.Index.Remove(T^.Entries[I].Key);
  T^.Entries[I].Key := nil;
  T^.Entries[I].Value := VUndefined;
  Dec(T^.Live);
end;

procedure CompactTable(T: PEphTable);
var I, N: Integer;
begin
  N := 0;
  T^.Index.Clear;
  for I := 0 to T^.Count - 1 do
    if T^.Entries[I].Key <> nil then
    begin
      T^.Entries[N] := T^.Entries[I];
      T^.Index.Add(T^.Entries[N].Key, N);
      Inc(N);
    end;
  for I := N to T^.Count - 1 do T^.Entries[I].Value := VUndefined;
  T^.Count := N;
  T^.Live := N;
end;

procedure FreeTable(T: PEphTable);
begin
  T^.Index.Free;
  Dispose(T);
end;

{ after the fixpoint: a table whose owner died freed; in a live one, the entries whose key died dropped and the
  moved keys followed }
procedure SweepTables(S: PWeakState);
var I, J, N, Idx: Integer; T: PEphTable; B, K: Pointer;
begin
  N := 0;
  for I := 0 to S^.TableCount - 1 do
  begin
    T := S^.Tables[I];
    B := T^.Box;
    if not GCSurvives(B) then
    begin
      FreeTable(T);
      Continue;
    end;
    T^.Box := B;
    if GCInMinor then
    begin
      for J := 0 to T^.YoungCount - 1 do
      begin
        Idx := T^.Young[J];
        K := T^.Entries[Idx].Key;
        if K = nil then Continue;
        if GCSurvives(K) then
        begin
          if K <> T^.Entries[Idx].Key then
          begin
            T^.Index.Remove(T^.Entries[Idx].Key);
            T^.Entries[Idx].Key := K;
            T^.Index.AddOrSetValue(K, Idx);
          end;
        end
        else DeleteEntry(T, Idx);
      end;
      T^.YoungCount := 0;
    end
    else
    begin
      for J := 0 to T^.Count - 1 do
      begin
        K := T^.Entries[J].Key;
        if (K <> nil) and not GCSurvives(K) then DeleteEntry(T, J);
      end;
      if (T^.YoungCount = 0) and (T^.Count > 2 * T^.Live + 16) then CompactTable(T);
    end;
    S^.Tables[N] := T;
    Inc(N);
  end;
  S^.TableCount := N;
  S^.WatchHead.Clear;
  S^.WatchCount := 0;
end;

{ after marking (and evacuation): a box that died is dropped, a dead target cleared, a moved one followed }
procedure WeakRefHook;
var S: PWeakState; I, N: Integer; B, T: Pointer;
begin
  S := WeakStateNow;
  if S = nil then Exit;
  if S^.TableCount > 0 then SweepTables(S);
  if S^.Count = 0 then Exit;
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
  { the previous engine's state on this thread: its values and roots went with that engine }
  if WeakStateVar <> nil then
  begin
    WeakStateVar^.WatchHead.Free;
    Dispose(WeakStateVar);
    WeakStateVar := nil;
  end;
  FirstInEngine(WeakStateGen);
  New(Result);
  Result^.Count := 0;
  SetLength(Result^.Boxes, 64);
  Result^.Kept := VUndefined;
  Result^.CleanupFns := VUndefined;
  Result^.CleanupPending := False;
  Result^.TableCount := 0;
  Result^.EphEpoch := -1;
  Result^.WatchHead := TPtrIntMap.Create;
  Result^.WatchCount := 0;
  WeakStateVar := Result;
  AddRoot(@Result^.Kept);
  AddRoot(@Result^.CleanupFns);
  GCWeakRefHook := @WeakRefHook;
  GCEphemeronHook := @EphemeronHook;
end;

function IsBox(V: TValue): Boolean;
begin
  Result := IsObject(V) and (ObjOf(V)^.OKind = okPlain) and ((ObjOf(V)^.Pad and $FFFFFF00) = WEAK_MAGIC) and
    ((ObjOf(V)^.Pad and $FF) <> BOX_TABLE);
end;

{ the table of a table owner, nil for anything else }
function TableOf(V: TValue): PEphTable;
begin
  if IsObject(V) and (ObjOf(V)^.OKind = okPlain) and (ObjOf(V)^.Pad = WEAK_MAGIC or BOX_TABLE) then
    Result := PEphTable(ObjOf(V)^.Native)
  else
    Result := nil;
end;

function TableNewNative(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var S: PWeakState; O: PObj; T: PEphTable;
begin
  S := EnsureState;
  O := NewPlainObject(VNull, 0);
  O^.OFlags := O^.OFlags or 8;
  O^.Pad := WEAK_MAGIC or BOX_TABLE;
  New(T);
  T^.Box := O;
  T^.Count := 0;
  T^.Live := 0;
  T^.YoungCount := 0;
  T^.Index := TPtrIntMap.Create;
  O^.Native := T;
  if S^.TableCount >= Length(S^.Tables) then SetLength(S^.Tables, S^.TableCount * 2 + 16);
  S^.Tables[S^.TableCount] := T;
  Inc(S^.TableCount);
  Res := MkPtr(O);
  Result := True;
end;

function FindEntry(T: PEphTable; Key: TValue): Integer;
begin
  if not IsPtr(Key) or not T^.Index.TryGetValue(PtrOf(Key), Result) then Result := -1;
end;

function TableGetNative(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var T: PEphTable; I: Integer;
begin
  if Argc < 2 then Exit(ThrowTypeError('weak table expected'));
  T := TableOf(Args[1]);
  if T = nil then Exit(ThrowTypeError('weak table expected'));
  I := FindEntry(T, Args[2]);
  if I < 0 then Res := VUndefined else Res := T^.Entries[I].Value;
  Result := True;
end;

function TableHasNative(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var T: PEphTable;
begin
  if Argc < 2 then Exit(ThrowTypeError('weak table expected'));
  T := TableOf(Args[1]);
  if T = nil then Exit(ThrowTypeError('weak table expected'));
  Res := MkBool(FindEntry(T, Args[2]) >= 0);
  Result := True;
end;

function TableSetNative(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var T: PEphTable; I: Integer; K: Pointer; V: TValue;
begin
  if (Argc < 2) or not IsPtr(Args[2]) then Exit(ThrowTypeError('weak table expected'));
  T := TableOf(Args[1]);
  if T = nil then Exit(ThrowTypeError('weak table expected'));
  if Argc >= 3 then V := Args[3] else V := VUndefined;
  K := PtrOf(Args[2]);
  I := FindEntry(T, Args[2]);
  if I < 0 then
  begin
    if T^.Count >= Length(T^.Entries) then SetLength(T^.Entries, T^.Count * 2 + 8);
    I := T^.Count;
    Inc(T^.Count);
    Inc(T^.Live);
    T^.Entries[I].Key := K;
    T^.Entries[I].Visited := -1;
    T^.Index.AddOrSetValue(K, I);
  end;
  T^.Entries[I].Value := V;
  { a young key or value: the next minor collection looks at this entry }
  if GCIsYoung(K) or (IsPtr(V) and GCIsYoung(PtrOf(V))) then
  begin
    if T^.YoungCount >= Length(T^.Young) then SetLength(T^.Young, T^.YoungCount * 2 + 8);
    T^.Young[T^.YoungCount] := I;
    Inc(T^.YoungCount);
  end;
  Res := VUndefined;
  Result := True;
end;

function TableDeleteNative(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var T: PEphTable; I: Integer;
begin
  if Argc < 2 then Exit(ThrowTypeError('weak table expected'));
  T := TableOf(Args[1]);
  if T = nil then Exit(ThrowTypeError('weak table expected'));
  I := FindEntry(T, Args[2]);
  if I >= 0 then DeleteEntry(T, I);
  Res := MkBool(I >= 0);
  Result := True;
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

function WeakCellsWaiting: Boolean;
var S: PWeakState; I: Integer; B: Pointer;
begin
  Result := False;
  S := WeakStateNow;
  if S = nil then Exit;
  for I := 0 to S^.Count - 1 do
  begin
    B := S^.Boxes[I];
    if ((PObj(B)^.Pad and $FF) = BOX_CELL) and (PObj(B)^.Native <> nil) then Exit(True);
  end;
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
