unit G2.Promise;
{ Promise objects, their jobs and the microtask queue (ES2026 27.2), and the await machinery of async functions
  (27.7).

  A promise is an okPromise object: Aux1 the result, Aux2 its reactions (undefined, one reaction, or an array of
  them), Pad the state, OFlags bit 1 [[PromiseIsHandled]].
  A reaction is a value array [kind, onFulfilled, onRejected, capability]. The capability is undefined (none),
  a promise object (a capability of %Promise% itself, settled directly: no resolving functions are made), or a
  value array [promise, resolve, reject] from NewPromiseCapability of another constructor. An await reaction
  holds the suspended async function instead of handlers and resumes it.
  Jobs are quads [kind, a, b, c] in one ring buffer, so queueing a job allocates nothing. }
{$mode delphiunicode}{$H+}
{$MINFPCONSTPREC 64}
{$POINTERMATH ON}
{$Q-}{$R-}

interface

uses
  G2.Core;

procedure InitPromise;
function RunMicrotasks: Boolean;
{ a pending promise of this engine, and settling one from native code (G2.Atomics' waitAsync) }
function NewPendingPromise: TValue;
procedure ResolvePendingPromise(P, V: TValue);
function IsPromiseValue(V: TValue): Boolean;
procedure RejectPendingPromise(P, Reason: TValue);
{ 0 pending, 1 fulfilled, 2 rejected; the result (value or reason) once settled }
function PromiseStatus(P: TValue; var Result_: TValue): Integer;
{ a job that calls F with no arguments, after the current job (G2.Modules: import()'s loading) }
procedure EnqueueCallback(F: TValue);
{ 27.2.5.4.1 PerformPromiseThen(P, OnF, OnR, Cap) for a promise P of this engine; Cap a pending promise
  (G2.ShadowRealm's importValue) }
procedure PromiseThenNative(P, OnF, OnR, Cap: TValue);

implementation

uses
  SysUtils, Math, G2.Bytecode, G2.VM;

const
  okPromise = 20;
  psPending = 0; psFulfilled = 1; psRejected = 2;
  rkThen = 0; rkAwait = 1;
  jkReaction = 1; jkThenable = 2; jkCallback = 3;

type
  TArgs = PValue;

threadvar
  PromiseProto, PromiseCtor, ThenFn, Jobs, Scratch: TValue;
  JobHead, JobCount: Integer;
  AtomThen, AtomResolve: PStr;

function Arg(Args: TArgs; Argc, I: Integer): TValue; inline;
begin
  if I <= Argc then Result := Args[I] else Result := VUndefined;
end;

function IsPromise(V: TValue): Boolean; inline;
begin
  Result := IsObject(V) and (ObjOf(V)^.OKind = okPromise);
end;

function IsPromiseValue(V: TValue): Boolean;
begin
  Result := IsPromise(V);
end;

{ the exception a failed call left behind, cleared so execution goes on }
function TakePending: TValue;
begin
  Result := Pending;
  Pending := VUndefined;
end;

function VA(V: TValue): PValueArray; inline;
begin
  Result := PValueArray(PtrOf(V));
end;

function NewQuad(A, B, C, D: TValue): TValue;
var Q: PValueArray;
begin
  PushRoot(A); PushRoot(B); PushRoot(C); PushRoot(D);
  Q := NewValueArray(4);
  Q^.Len := 4;
  Q^.Data[0] := RootAt(TempTop - 4); Q^.Data[1] := RootAt(TempTop - 3);
  Q^.Data[2] := RootAt(TempTop - 2); Q^.Data[3] := RootAt(TempTop - 1);
  PopRoots(4);
  Result := MkPtr(Q);
end;

{ ---------------- jobs ---------------- }

procedure EnqueueJob(Kind: Integer; A, B, C: TValue);
var Q, N: PValueArray; Cap, I, J: Integer;
begin
  Q := VA(Jobs);
  Cap := Integer(Q^.Len) div 4;
  if JobCount = Cap then
  begin
    PushRoot(A); PushRoot(B); PushRoot(C);
    N := NewValueArray(Cap * 8);
    N^.Len := Cap * 8;
    Q := VA(Jobs);
    for I := 0 to JobCount - 1 do
    begin
      J := ((JobHead + I) mod Cap) * 4;
      N^.Data[I * 4] := Q^.Data[J]; N^.Data[I * 4 + 1] := Q^.Data[J + 1];
      N^.Data[I * 4 + 2] := Q^.Data[J + 2]; N^.Data[I * 4 + 3] := Q^.Data[J + 3];
    end;
    for I := JobCount * 4 to Cap * 8 - 1 do N^.Data[I] := VUndefined;
    if (N^.Hdr.Flags and cfOld) <> 0 then Remember(@N^.Hdr);
    Jobs := MkPtr(N);
    JobHead := 0;
    Cap := Cap * 2;
    C := RootAt(TempTop - 1); B := RootAt(TempTop - 2); A := RootAt(TempTop - 3);
    PopRoots(3);
    Q := VA(Jobs);
  end;
  J := ((JobHead + JobCount) mod Cap) * 4;
  ValueArrayStore(Q, J, MkInt(Kind));
  ValueArrayStore(Q, J + 1, A);
  ValueArrayStore(Q, J + 2, B);
  ValueArrayStore(Q, J + 3, C);
  Inc(JobCount);
end;

{ ---------------- promise core ---------------- }

function NewPromiseObj(Proto: TValue): TValue;
var O: PObj;
begin
  PushRoot(Proto);
  O := NewObject(RootShapeFor(RootAt(TempTop - 1)), 0, okPromise);
  O^.Aux1 := VUndefined;
  O^.Aux2 := VUndefined;
  O^.Pad := psPending;
  O^.OFlags := 0;
  PopRoots(1);
  Result := MkPtr(O);
end;

{ 27.2.1.4 FulfillPromise / 27.2.1.7 RejectPromise, then 27.2.1.8 TriggerPromiseReactions }
procedure Settle(P: TValue; State: Integer; V: TValue);
var O: PObj; R: TValue; I, N: Integer;
begin
  O := ObjOf(P);
  if O^.Pad <> psPending then Exit;
  O^.Pad := State;
  O^.Aux1 := V;
  WriteBarrier(@O^.Hdr, V);
  R := O^.Aux2;
  O^.Aux2 := VUndefined;
  if R = VUndefined then Exit;
  PushRoot(R); PushRoot(V);
  if IsObject(R) then
  begin
    N := ArrayLen(ObjOf(R));
    for I := 0 to N - 1 do
      EnqueueJob(jkReaction, ObjOf(RootAt(TempTop - 2))^.Elements^.Data[I], RootAt(TempTop - 1), MkInt(State));
  end
  else
    EnqueueJob(jkReaction, R, V, MkInt(State));
  PopRoots(2);
end;

procedure RejectPromise(P, Reason: TValue);
begin
  Settle(P, psRejected, Reason);
end;

{ 27.2.1.3.2 Promise Resolve Functions, steps 7-16 }
procedure ResolvePromise(P, V: TValue);
var T, E: TValue;
begin
  if V = P then
  begin
    PushRoot(P);
    E := MakeError(TypeErrorProto, 'Chaining cycle detected for promise');
    RejectPromise(RootAt(TempTop - 1), E);
    PopRoots(1);
    Exit;
  end;
  if not IsObject(V) then begin Settle(P, psFulfilled, V); Exit; end;
  PushRoot(P); PushRoot(V);
  if not GetV(V, AtomThen, T) then
  begin
    T := TakePending;
    RejectPromise(RootAt(TempTop - 2), T);
    PopRoots(2);
    Exit;
  end;
  if IsCallable(T) then EnqueueJob(jkThenable, RootAt(TempTop - 2), RootAt(TempTop - 1), T)
  else Settle(RootAt(TempTop - 2), psFulfilled, RootAt(TempTop - 1));
  PopRoots(2);
end;

{ ---------------- natives with data (Aux1 of the function object, read through NativeSelf) ---------------- }

function NewDataNative(Fn: TNativeFn; Length: Integer; Data: TValue): TValue;
begin
  PushRoot(Data);
  Result := NewNativeFunction(Fn, '', Length);
  ObjOf(Result)^.Aux1 := RootAt(TempTop - 1);
  WriteBarrier(@ObjOf(Result)^.Hdr, RootAt(TempTop - 1));
  PopRoots(1);
end;

function SelfData: TValue; inline;
begin
  Result := ObjOf(NativeSelf)^.Aux1;
end;

{ 27.2.1.3 CreateResolvingFunctions: data [promise, alreadyResolved] shared by both }
function Resolve_N(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var D: PValueArray;
begin
  D := VA(SelfData);
  Res := VUndefined;
  if D^.Data[1] = VTrue then Exit(True);
  D^.Data[1] := VTrue;
  ResolvePromise(D^.Data[0], Arg(Args, Argc, 1));
  Result := True;
end;

function Reject_N(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var D: PValueArray;
begin
  D := VA(SelfData);
  Res := VUndefined;
  if D^.Data[1] = VTrue then Exit(True);
  D^.Data[1] := VTrue;
  RejectPromise(D^.Data[0], Arg(Args, Argc, 1));
  Result := True;
end;

procedure MakeResolvingFunctions(P: TValue; var ResFn, RejFn: TValue);
var D: TValue;
begin
  D := NewQuad(P, VFalse, VUndefined, VUndefined);
  PushRoot(D);
  ResFn := NewDataNative(Resolve_N, 1, RootAt(TempTop - 1));
  PushRoot(ResFn);
  RejFn := NewDataNative(Reject_N, 1, RootAt(TempTop - 2));
  ResFn := RootAt(TempTop - 1);
  PopRoots(2);
end;

{ ---------------- capabilities ---------------- }

{ 27.2.1.5 GetCapabilitiesExecutor: data [resolve, reject] }
function CapExecutor_N(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var D: PValueArray;
begin
  D := VA(SelfData);
  if D^.Data[0] <> VUndefined then Exit(ThrowTypeError('Promise executor has already been invoked with non-undefined arguments'));
  if D^.Data[1] <> VUndefined then Exit(ThrowTypeError('Promise executor has already been invoked with non-undefined arguments'));
  ValueArrayStore(D, 0, Arg(Args, Argc, 1));
  ValueArrayStore(D, 1, Arg(Args, Argc, 2));
  Res := VUndefined;
  Result := True;
end;

{ 27.2.1.5 NewPromiseCapability(C): a promise object for C = %Promise% (settled directly), else
  [promise, resolve, reject] }
function NewPromiseCapability(C: TValue; var Cap: TValue): Boolean;
var D, Ex, P: TValue;
begin
  if C = PromiseCtor then begin Cap := NewPromiseObj(PromiseProto); Exit(True); end;
  if not IsConstructorV(C) then Exit(ThrowTypeError('Promise resolve or reject function is not callable'));
  PushRoot(C);
  D := NewQuad(VUndefined, VUndefined, VUndefined, VUndefined);
  PushRoot(D);
  Ex := NewDataNative(CapExecutor_N, 2, D);
  PushRoot(Ex);
  if not Construct(RootAt(TempTop - 3), @Ex, 1, RootAt(TempTop - 3), P) then begin PopRoots(3); Exit(False); end;
  D := RootAt(TempTop - 2);
  if not IsCallable(VA(D)^.Data[0]) then begin PopRoots(3); Exit(ThrowTypeError('Promise resolve function is not callable')); end;
  if not IsCallable(VA(D)^.Data[1]) then begin PopRoots(3); Exit(ThrowTypeError('Promise reject function is not callable')); end;
  PushRoot(P);
  Cap := NewQuad(RootAt(TempTop - 1), VA(RootAt(TempTop - 3))^.Data[0], VA(RootAt(TempTop - 3))^.Data[1], VUndefined);
  PopRoots(4);
  Result := True;
end;

function CapPromise(Cap: TValue): TValue; inline;
begin
  if IsObject(Cap) then Result := Cap else Result := VA(Cap)^.Data[0];
end;

{ resolve (Which 0) or reject (1) a capability; False if its function threw }
function CapSettle(Cap: TValue; Which: Integer; V: TValue): Boolean;
var R: TValue;
begin
  if Cap = VUndefined then Exit(True);
  if IsObject(Cap) then
  begin
    if Which = 0 then ResolvePromise(Cap, V) else RejectPromise(Cap, V);
    Exit(True);
  end;
  Result := CallValue(VA(Cap)^.Data[1 + Which], VUndefined, @V, 1, R);
end;

{ IfAbruptRejectPromise: the pending exception rejects the capability; Res its promise }
function RejectWithPending(Cap: TValue; var Res: TValue): Boolean;
var E: TValue;
begin
  E := TakePending;
  PushRoot(Cap);
  if not CapSettle(Cap, 1, E) then begin PopRoots(1); Exit(False); end;
  Res := CapPromise(RootAt(TempTop - 1));
  PopRoots(1);
  Result := True;
end;

{ 27.2.5.4.1 PerformPromiseThen }
procedure PerformThen(P: TValue; Kind: Integer; OnF, OnR, Cap: TValue);
var Rec, Old: TValue; O: PObj; A: PObj;
begin
  if not IsCallable(OnF) and (Kind = rkThen) then OnF := VUndefined;
  if not IsCallable(OnR) and (Kind = rkThen) then OnR := VUndefined;
  PushRoot(P);
  Rec := NewQuad(MkInt(Kind), OnF, OnR, Cap);
  PushRoot(Rec);
  O := ObjOf(RootAt(TempTop - 2));
  if O^.Pad = psPending then
  begin
    Old := O^.Aux2;
    if Old = VUndefined then
    begin
      O^.Aux2 := RootAt(TempTop - 1);
      WriteBarrier(@O^.Hdr, O^.Aux2);
    end
    else if IsObject(Old) then ArrayPush(ObjOf(Old), RootAt(TempTop - 1))
    else
    begin
      PushRoot(Old);
      A := NewArray(0, 4);
      ArrayPush(A, RootAt(TempTop - 1));
      ArrayPush(A, RootAt(TempTop - 2));
      PopRoots(1);
      O := ObjOf(RootAt(TempTop - 2));
      O^.Aux2 := MkPtr(A);
      WriteBarrier(@O^.Hdr, O^.Aux2);
    end;
  end
  else
    EnqueueJob(jkReaction, RootAt(TempTop - 1), O^.Aux1, MkInt(O^.Pad));
  ObjOf(RootAt(TempTop - 2))^.OFlags := ObjOf(RootAt(TempTop - 2))^.OFlags or 1;
  PopRoots(2);
end;

procedure PromiseThenNative(P, OnF, OnR, Cap: TValue);
begin
  PerformThen(P, rkThen, OnF, OnR, Cap);
end;

{ 7.3.22 SpeciesConstructor(O, %Promise%) }
function SpeciesCtor(O: TValue; var C: TValue): Boolean;
var Ctor, S: TValue;
begin
  if not GetV(O, AtomConstructor, Ctor) then Exit(False);
  if Ctor = VUndefined then begin C := PromiseCtor; Exit(True); end;
  if not IsObject(Ctor) then Exit(ThrowTypeError('object.constructor is not an object'));
  if not GetElemV(Ctor, SymSpecies, S) then Exit(False);
  if IsNullish(S) then begin C := PromiseCtor; Exit(True); end;
  if not IsConstructorV(S) then Exit(ThrowTypeError('object.constructor[Symbol.species] is not a constructor'));
  C := S;
  Result := True;
end;

{ 27.2.4.7.1 PromiseResolve(C, x) }
function PromiseResolveC(C, X: TValue; var Res: TValue): Boolean;
var Ctor, Cap: TValue;
begin
  if IsPromise(X) then
  begin
    PushRoot(C); PushRoot(X);
    if not GetV(X, AtomConstructor, Ctor) then begin PopRoots(2); Exit(False); end;
    X := RootAt(TempTop - 1); C := RootAt(TempTop - 2);
    PopRoots(2);
    if Ctor = C then begin Res := X; Exit(True); end;
  end;
  PushRoot(X);
  if not NewPromiseCapability(C, Cap) then begin PopRoots(1); Exit(False); end;
  PushRoot(Cap);
  if not CapSettle(Cap, 0, RootAt(TempTop - 2)) then begin PopRoots(2); Exit(False); end;
  Res := CapPromise(RootAt(TempTop - 1));
  PopRoots(2);
  Result := True;
end;

{ ---------------- async functions (27.7.5) ---------------- }

{ resumes the async function G with V (Mode 0) or throws V into it (Mode 1), until its next await or its end }
procedure AsyncStep(G, V: TValue; Mode: Integer);
var T: PFuncTemplate; P, Res, Aw: TValue; Done, Ok: Boolean;
begin
  T := PFuncTemplate(ObjOf(ObjOf(G)^.Aux1)^.Native);
  P := VA(ObjOf(G)^.Aux2)^.Data[T^.AsyncReg];
  PushRoot(G); PushRoot(P);
  Ok := GeneratorResume(G, V, Mode, Res, Done);
  if not Ok then RejectPromise(RootAt(TempTop - 1), TakePending)
  else if Done then ResolvePromise(RootAt(TempTop - 1), Res)
  else
  begin
    { 27.7.5.3 Await(value): PromiseResolve(%Promise%, value), then resume on its settlement }
    if PromiseResolveC(PromiseCtor, Res, Aw) then PerformThen(Aw, rkAwait, RootAt(TempTop - 2), VUndefined, VUndefined)
    else
    begin
      { an abrupt PromiseResolve throws into the async function at once }
      Res := TakePending;
      PopRoots(2);
      AsyncStep(G, Res, 1);
      Exit;
    end;
  end;
  PopRoots(2);
end;

{ the call of an async function: its frame is suspended in G; returns its promise }
function AsyncStart(G: TValue): TValue;
var T: PFuncTemplate; P: TValue;
begin
  PushRoot(G);
  P := NewPromiseObj(PromiseProto);
  G := RootAt(TempTop - 1);
  T := PFuncTemplate(ObjOf(ObjOf(G)^.Aux1)^.Native);
  ValueArrayStore(VA(ObjOf(G)^.Aux2), T^.AsyncReg, P);
  PushRoot(P);
  AsyncStep(RootAt(TempTop - 2), VUndefined, 0);
  Result := RootAt(TempTop - 1);
  PopRoots(2);
end;

{ ---------------- async generators (27.6) ----------------
  An async generator is an okAsyncGen object run by the generator machinery. Its request queue (a JS array of
  [kind, value, promise] quads; kind 0 next, 1 throw, 2 return) is an internal-key property. OFlags is the
  state: 0 suspended-start, 1 suspended-yield, 2 executing, 3 completed, 4 awaiting-return. }
const
  agStart = 0; agYield = 1; agExec = 2; agDone = 3; agAwaitReturn = 4;
  rkAwaitReturn = 2;      { AsyncGeneratorAwaitReturn: settles the front request }
  rkYieldReturn = 3;      { a return() resumption at a yield: the awaited value, then the return }

threadvar
  AtomAGQueue: PStr;

function AGQueue(G: TValue): PObj; inline;
begin
  Result := ObjOf(GetSlot(ObjOf(G), ShapeLookup(ObjOf(G)^.Shape, AtomAGQueue)));
end;

function NewIterResult(V: TValue; Done: Boolean): TValue;
var O: TValue;
begin
  PushRoot(V);
  O := MkPtr(NewPlainObject(ObjectProto, 4));
  PushRoot(O);
  DefineOwn(O, AtomValue, RootAt(TempTop - 2), pfDefault);
  DefineOwn(RootAt(TempTop - 1), AtomDone, MkBool(Done), pfDefault);
  Result := RootAt(TempTop - 1);
  PopRoots(2);
end;

procedure AsyncGenResume(G, V: TValue; Mode: Integer); forward;
procedure AsyncGenDrain(G: TValue); forward;

{ 27.6.3.4 AsyncGeneratorCompleteStep: the front request settles with a throw (Throwing) or an iterator result }
procedure AGCompleteStep(G: TValue; Throwing: Boolean; V: TValue; Done: Boolean);
var Q: PObj; Req, P: TValue; I, N: Integer;
begin
  Q := AGQueue(G);
  N := ArrayLen(Q);
  if N = 0 then Exit;
  Req := Q^.Elements^.Data[0];
  for I := 1 to N - 1 do Q^.Elements^.Data[I - 1] := Q^.Elements^.Data[I];
  ArraySetLength(Q, N - 1);
  P := VA(Req)^.Data[2];
  if Throwing then RejectPromise(P, V)
  else
  begin
    PushRoot(P);
    V := NewIterResult(V, Done);
    ResolvePromise(RootAt(TempTop - 1), V);
    PopRoots(1);
  end;
end;

function AGFront(G: TValue; var Req: TValue): Boolean;
var Q: PObj;
begin
  Q := AGQueue(G);
  Result := ArrayLen(Q) > 0;
  if Result then Req := Q^.Elements^.Data[0];
end;

{ 27.6.3.9 AsyncGeneratorAwaitReturn }
procedure AsyncGenAwaitReturn(G: TValue);
var Req, P: TValue;
begin
  if not AGFront(G, Req) then Exit;
  PushRoot(G);
  if not PromiseResolveC(PromiseCtor, VA(Req)^.Data[1], P) then
  begin
    ObjOf(RootAt(TempTop - 1))^.OFlags := agDone;
    AGCompleteStep(RootAt(TempTop - 1), True, TakePending, True);
    AsyncGenDrain(RootAt(TempTop - 1));
    PopRoots(1);
    Exit;
  end;
  PerformThen(P, rkAwaitReturn, RootAt(TempTop - 1), VUndefined, VUndefined);
  PopRoots(1);
end;

{ 27.6.3.10 AsyncGeneratorDrainQueue (the generator is completed) }
procedure AsyncGenDrain(G: TValue);
var Req: TValue; K: Integer;
begin
  PushRoot(G);
  while AGFront(RootAt(TempTop - 1), Req) do
  begin
    K := IntOf(VA(Req)^.Data[0]);
    if K = 2 then
    begin
      ObjOf(RootAt(TempTop - 1))^.OFlags := agAwaitReturn;
      AsyncGenAwaitReturn(RootAt(TempTop - 1));
      Break;
    end;
    if K = 1 then AGCompleteStep(RootAt(TempTop - 1), True, VA(Req)^.Data[1], True)
    else AGCompleteStep(RootAt(TempTop - 1), False, VUndefined, True);
  end;
  PopRoots(1);
end;

{ a request taken up when the generator is suspended at a yield (27.6.3.8 AsyncGeneratorYield steps 9-12 and
  27.6.3.7 AsyncGeneratorUnwrapYieldResumption): a return is awaited first }
procedure AGResumeWith(G, Req: TValue);
var K: Integer; P: TValue;
begin
  K := IntOf(VA(Req)^.Data[0]);
  if K <> 2 then begin AsyncGenResume(G, VA(Req)^.Data[1], K); Exit; end;
  PushRoot(G);
  ObjOf(G)^.OFlags := agExec;
  if PromiseResolveC(PromiseCtor, VA(Req)^.Data[1], P) then
    PerformThen(P, rkYieldReturn, RootAt(TempTop - 1), VUndefined, VUndefined)
  else
  begin
    ObjOf(RootAt(TempTop - 1))^.OFlags := agYield;
    AsyncGenResume(RootAt(TempTop - 1), TakePending, 1);
  end;
  PopRoots(1);
end;

{ runs the body from its suspension with V (Mode 0 next, 1 throw, 2 return) until it awaits, yields or ends }
procedure AsyncGenResume(G, V: TValue; Mode: Integer);
var Res, Aw, Req: TValue; Done, Ok: Boolean;
begin
  PushRoot(G);
  ObjOf(G)^.OFlags := agYield * Ord(ObjOf(G)^.OFlags <> agStart);   { GeneratorResume's states: 0 start, 1 suspended }
  Ok := GeneratorResume(G, V, Mode, Res, Done);
  G := RootAt(TempTop - 1);
  if not Ok then
  begin
    ObjOf(G)^.OFlags := agDone;
    AGCompleteStep(G, True, TakePending, True);
    AsyncGenDrain(RootAt(TempTop - 1));
  end
  else if Done then
  begin
    ObjOf(G)^.OFlags := agDone;
    AGCompleteStep(G, False, Res, True);
    AsyncGenDrain(RootAt(TempTop - 1));
  end
  else if SuspendedByAwait then
  begin
    ObjOf(G)^.OFlags := agExec;
    if PromiseResolveC(PromiseCtor, Res, Aw) then PerformThen(Aw, rkAwait, RootAt(TempTop - 1), VUndefined, VUndefined)
    else
    begin
      Res := TakePending;
      PopRoots(1);
      AsyncGenResume(G, Res, 1);
      Exit;
    end;
  end
  else
  begin
    { a yield: the front request gets an iterator result (done false); the next one, if any, resumes the body }
    ObjOf(G)^.OFlags := agYield;
    AGCompleteStep(G, False, Res, False);
    if AGFront(RootAt(TempTop - 1), Req) then AGResumeWith(RootAt(TempTop - 1), Req);
  end;
  PopRoots(1);
end;

procedure AGEnqueue(G: TValue; Kind: Integer; V, P: TValue);
var Req: TValue;
begin
  PushRoot(G);
  Req := NewQuad(MkInt(Kind), V, P, VUndefined);
  ArrayPush(AGQueue(RootAt(TempTop - 1)), Req);
  PopRoots(1);
end;

{ 27.6.1.2-27.6.1.4 AsyncGenerator.prototype.next / return / throw (NativeData 0 / 2 / 1) }
function AsyncGen_Method(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var G, P, V, E: TValue; Kind, State: Integer;
begin
  Kind := NativeData;
  G := Args[0];
  V := Arg(Args, Argc, 1);
  PushRoot(G); PushRoot(V);
  P := NewPromiseObj(PromiseProto);
  PushRoot(P);
  G := RootAt(TempTop - 3);
  if not IsObject(G) or (ObjOf(G)^.OKind <> okAsyncGen) then
  begin
    { 27.6.3.2 AsyncGeneratorValidate: a rejected promise, not an exception }
    E := MakeError(TypeErrorProto, 'AsyncGenerator method called on incompatible receiver');
    RejectPromise(RootAt(TempTop - 1), E);
    Res := RootAt(TempTop - 1);
    PopRoots(3);
    Exit(True);
  end;
  State := ObjOf(G)^.OFlags;
  if Kind = 1 then
  begin
    if State = agStart then begin ObjOf(G)^.OFlags := agDone; State := agDone; end;
    if State = agDone then
    begin
      RejectPromise(RootAt(TempTop - 1), RootAt(TempTop - 2));
      Res := RootAt(TempTop - 1);
      PopRoots(3);
      Exit(True);
    end;
  end
  else if (Kind = 0) and (State = agDone) then
  begin
    V := NewIterResult(VUndefined, True);
    ResolvePromise(RootAt(TempTop - 1), V);
    Res := RootAt(TempTop - 1);
    PopRoots(3);
    Exit(True);
  end;
  AGEnqueue(RootAt(TempTop - 3), Kind, RootAt(TempTop - 2), RootAt(TempTop - 1));
  G := RootAt(TempTop - 3);
  if Kind = 2 then
  begin
    if (State = agStart) or (State = agDone) then
    begin
      ObjOf(G)^.OFlags := agAwaitReturn;
      AsyncGenAwaitReturn(G);
    end
    else if State = agYield then
    begin
      AGFront(G, E);   { suspended at a yield, the queue held nothing else: this request is its front }
      AGResumeWith(G, E);
    end;
  end
  else if (State = agStart) or (State = agYield) then
    AsyncGenResume(G, RootAt(TempTop - 2), Kind);
  Res := RootAt(TempTop - 1);
  PopRoots(3);
  Result := True;
end;

{ the call of an async generator function: G is its suspended frame }
function AsyncGenStart(G: TValue): TValue;
var Q: TValue;
begin
  PushRoot(G);
  Q := MkPtr(NewArray(0, 2));
  G := RootAt(TempTop - 1);
  ObjOf(G)^.OKind := okAsyncGen;
  ObjOf(G)^.OFlags := agStart;
  AddProperty(ObjOf(G), AtomAGQueue, Q, 0);
  Result := RootAt(TempTop - 1);
  PopRoots(1);
end;

{ ---------------- CreateAsyncFromSyncIterator (27.1.6) ----------------
  An okAsyncFromSync object: Aux1 the sync iterator, Aux2 its next method. }
const
  okAsyncFromSync = 27;

function ASUnwrap_N(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
begin
  { onFulfilled of the continuation: data [done] }
  Res := NewIterResult(Arg(Args, Argc, 1), VA(SelfData)^.Data[0] = VTrue);
  Result := True;
end;

function ASCloseReject_N(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var It, E: TValue;
begin
  { onRejected that closes the sync iterator first: data [unused, iterator] }
  It := VA(SelfData)^.Data[1];
  E := Arg(Args, Argc, 1);
  PushRoot(E);
  Pending := E;
  IteratorCloseV(It, True);
  PopRoots(1);
  Result := False;
end;

{ 27.1.6.4 AsyncFromSyncIteratorContinuation }
function ASContinuation(Result_, Cap, It: TValue; CloseOnRejection: Boolean; var Res: TValue): Boolean;
var DoneV, V, W, OnF, OnR, D: TValue; Done: Boolean;
begin
  PushRoot(Cap); PushRoot(It); PushRoot(Result_);
  if not GetV(Result_, AtomDone, DoneV) then begin Result := RejectWithPending(RootAt(TempTop - 3), Res); PopRoots(3); Exit; end;
  Done := ToBool(DoneV);
  if not GetV(RootAt(TempTop - 1), AtomValue, V) then begin Result := RejectWithPending(RootAt(TempTop - 3), Res); PopRoots(3); Exit; end;
  if not PromiseResolveC(PromiseCtor, V, W) then
  begin
    if not Done and CloseOnRejection then
    begin
      V := Pending;
      IteratorCloseV(RootAt(TempTop - 2), True);
    end;
    Result := RejectWithPending(RootAt(TempTop - 3), Res);
    PopRoots(3);
    Exit;
  end;
  PushRoot(W);
  D := NewQuad(MkBool(Done), RootAt(TempTop - 3), VUndefined, VUndefined);
  PushRoot(D);
  OnF := NewDataNative(ASUnwrap_N, 1, RootAt(TempTop - 1));
  PushRoot(OnF);
  if Done or not CloseOnRejection then OnR := VUndefined
  else OnR := NewDataNative(ASCloseReject_N, 1, RootAt(TempTop - 2));
  { roots: Cap -6, It -5, Result_ -4, W -3, D -2, OnF -1 }
  PerformThen(RootAt(TempTop - 3), rkThen, RootAt(TempTop - 1), OnR, RootAt(TempTop - 6));
  Res := CapPromise(RootAt(TempTop - 6));
  PopRoots(6);
  Result := True;
end;

{ %AsyncFromSyncIteratorPrototype%.next / return / throw (NativeData 0 / 2 / 1) }
function ASMethod(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var O, Cap, It, M, R, A: TValue; Kind: Integer;
begin
  Kind := NativeData;
  O := Args[0];
  Cap := NewPromiseObj(PromiseProto);
  PushRoot(Cap);
  It := ObjOf(O)^.Aux1;
  PushRoot(It);
  A := Arg(Args, Argc, 1);
  case Kind of
    0:
      begin
        if Argc >= 1 then Result := CallValue(ObjOf(O)^.Aux2, It, @A, 1, R)
        else Result := CallValue(ObjOf(O)^.Aux2, It, nil, 0, R);
        if not Result then begin Result := RejectWithPending(RootAt(TempTop - 2), Res); PopRoots(2); Exit; end;
      end;
    2:
      begin
        if not GetV(It, AtomOf('return'), M) then begin Result := RejectWithPending(RootAt(TempTop - 2), Res); PopRoots(2); Exit; end;
        if IsNullish(M) then
        begin
          R := NewIterResult(A, True);
          ResolvePromise(RootAt(TempTop - 2), R);
          Res := RootAt(TempTop - 2);
          PopRoots(2);
          Exit(True);
        end;
        if Argc >= 1 then Result := CallValue(M, RootAt(TempTop - 1), @A, 1, R)
        else Result := CallValue(M, RootAt(TempTop - 1), nil, 0, R);
        if not Result then begin Result := RejectWithPending(RootAt(TempTop - 2), Res); PopRoots(2); Exit; end;
      end;
  else
    begin
      if not GetV(It, AtomOf('throw'), M) then begin Result := RejectWithPending(RootAt(TempTop - 2), Res); PopRoots(2); Exit; end;
      if IsNullish(M) then
      begin
        { no throw method: close the sync iterator, then a TypeError }
        if not IteratorCloseV(RootAt(TempTop - 1), False) then begin Result := RejectWithPending(RootAt(TempTop - 2), Res); PopRoots(2); Exit; end;
        ThrowTypeError('The iterator does not provide a ''throw'' method');
        Result := RejectWithPending(RootAt(TempTop - 2), Res);
        PopRoots(2);
        Exit;
      end;
      if not CallValue(M, RootAt(TempTop - 1), @A, 1, R) then begin Result := RejectWithPending(RootAt(TempTop - 2), Res); PopRoots(2); Exit; end;
    end;
  end;
  if not IsObject(R) then
  begin
    ThrowTypeError('iterator result is not an object');
    Result := RejectWithPending(RootAt(TempTop - 2), Res);
    PopRoots(2);
    Exit;
  end;
  Result := ASContinuation(R, RootAt(TempTop - 2), RootAt(TempTop - 1), Kind <> 2, Res);
  PopRoots(2);
end;

function AsyncFromSync(It, NextM: TValue; var Res: TValue): Boolean;
var O: PObj;
begin
  PushRoot(It); PushRoot(NextM);
  O := NewObject(RootShapeFor(AsyncFromSyncIterProto), 0, okAsyncFromSync);
  O^.Aux1 := RootAt(TempTop - 2);
  O^.Aux2 := RootAt(TempTop - 1);
  PopRoots(2);
  Res := MkPtr(O);
  Result := True;
end;

{ ---------------- running jobs ---------------- }

{ 27.2.2.1 NewPromiseReactionJob }
procedure RunReactionJob(Rec, V: TValue; State: Integer);
var H, R, Cap: TValue; Ok: Boolean;
begin
  case IntOf(VA(Rec)^.Data[0]) of
    rkAwait:
      begin
        if ObjOf(VA(Rec)^.Data[1])^.OKind = okAsyncGen then AsyncGenResume(VA(Rec)^.Data[1], V, Ord(State = psRejected))
        else AsyncStep(VA(Rec)^.Data[1], V, Ord(State = psRejected));
        Exit;
      end;
    rkAwaitReturn:
      begin
        H := VA(Rec)^.Data[1];
        PushRoot(H);
        ObjOf(H)^.OFlags := agDone;
        AGCompleteStep(H, State = psRejected, V, True);
        AsyncGenDrain(RootAt(TempTop - 1));
        PopRoots(1);
        Exit;
      end;
    rkYieldReturn:
      begin
        H := VA(Rec)^.Data[1];
        ObjOf(H)^.OFlags := agYield;
        if State = psRejected then AsyncGenResume(H, V, 1) else AsyncGenResume(H, V, 2);
        Exit;
      end;
  end;
  PushRoot(Rec); PushRoot(V);
  if State = psFulfilled then H := VA(Rec)^.Data[1] else H := VA(Rec)^.Data[2];
  if H = VUndefined then
  begin
    { the default handlers pass the value or the reason on }
    CapSettle(VA(RootAt(TempTop - 2))^.Data[3], Ord(State = psRejected), RootAt(TempTop - 1));
  end
  else
  begin
    Ok := CallValue(H, VUndefined, @V, 1, R);
    if not Ok then R := TakePending;
    Cap := VA(RootAt(TempTop - 2))^.Data[3];
    CapSettle(Cap, Ord(not Ok), R);
  end;
  if Pending <> VUndefined then Pending := VUndefined;   { a capability function that threw: nothing observes it }
  PopRoots(2);
end;

{ 27.2.2.2 NewPromiseResolveThenableJob }
procedure RunThenableJob(P, Thenable, Then_: TValue);
var Fns: array[0..1] of TValue; R, E: TValue;
begin
  PushRoot(Thenable); PushRoot(Then_);
  MakeResolvingFunctions(P, Fns[0], Fns[1]);
  PushRoot(Fns[0]); PushRoot(Fns[1]);
  Fns[0] := RootAt(TempTop - 2); Fns[1] := RootAt(TempTop - 1);
  if not CallValue(RootAt(TempTop - 3), RootAt(TempTop - 4), @Fns[0], 2, R) then
  begin
    E := TakePending;
    CallValue(RootAt(TempTop - 1), VUndefined, @E, 1, R);
  end;
  PopRoots(4);
end;

function RunMicrotasks: Boolean;
var Q: PValueArray; J, Kind: Integer; A, B, C, R: TValue;
begin
  Result := True;
  while JobCount > 0 do
  begin
    Q := VA(Jobs);
    J := JobHead * 4;
    Kind := IntOf(Q^.Data[J]);
    A := Q^.Data[J + 1]; B := Q^.Data[J + 2]; C := Q^.Data[J + 3];
    Q^.Data[J + 1] := VUndefined; Q^.Data[J + 2] := VUndefined; Q^.Data[J + 3] := VUndefined;
    JobHead := (JobHead + 1) mod (Integer(Q^.Len) div 4);
    Dec(JobCount);
    case Kind of
      jkReaction: RunReactionJob(A, B, IntOf(C));
      jkThenable: RunThenableJob(A, B, C);
      jkCallback:
        if not CallValue(A, VUndefined, nil, 0, R) then Exit(False);   { reported like an uncaught exception }
    end;
  end;
  JobHead := 0;
end;

{ ---------------- constructor and statics ---------------- }

function Promise_Ctor(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var P, Proto, ResFn, RejFn, R, E: TValue; Fns: array[0..1] of TValue;
begin
  if CurNewTarget = VUndefined then Exit(ThrowTypeError('Promise constructor cannot be invoked without ''new'''));
  if not IsCallable(Arg(Args, Argc, 1)) then Exit(ThrowTypeError('Promise resolver is not a function'));
  { OrdinaryCreateFromConstructor(newTarget, %Promise.prototype%) }
  if CurNewTarget = PromiseCtor then Proto := PromiseProto
  else if not ProtoFromNewTarget(PromiseProto, Proto) then Exit(False);
  P := NewPromiseObj(Proto);
  PushRoot(P);
  MakeResolvingFunctions(P, ResFn, RejFn);
  PushRoot(ResFn); PushRoot(RejFn);
  Fns[0] := RootAt(TempTop - 2); Fns[1] := RootAt(TempTop - 1);
  if not CallValue(Args[1], VUndefined, @Fns[0], 2, R) then
  begin
    E := TakePending;
    if not CallValue(RootAt(TempTop - 1), VUndefined, @E, 1, R) then begin PopRoots(3); Exit(False); end;
  end;
  Res := RootAt(TempTop - 3);
  PopRoots(3);
  Result := True;
end;

function Promise_Then(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var C, Cap: TValue;
begin
  if not IsPromise(Args[0]) then Exit(ThrowTypeError('Promise.prototype.then called on incompatible receiver'));
  if not SpeciesCtor(Args[0], C) then Exit(False);
  if not NewPromiseCapability(C, Cap) then Exit(False);
  PushRoot(Cap);
  PerformThen(Args[0], rkThen, Arg(Args, Argc, 1), Arg(Args, Argc, 2), RootAt(TempTop - 1));
  Res := CapPromise(RootAt(TempTop - 1));
  PopRoots(1);
  Result := True;
end;

{ 27.2.5.1 Promise.prototype.catch: Invoke(promise, "then", undefined, onRejected) }
function Promise_Catch(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var F: TValue; A: array[0..1] of TValue;
begin
  if not GetV(Args[0], AtomThen, F) then Exit(False);
  if not IsCallable(F) then Exit(ThrowTypeError('then is not a function'));
  A[0] := VUndefined; A[1] := Arg(Args, Argc, 1);
  Result := CallValue(F, Args[0], @A[0], 2, Res);
end;

{ 27.2.5.3 Promise.prototype.finally: data of the closures [onFinally, C] }
function ValueThunk_N(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
begin
  Res := SelfData;
  Result := True;
end;

function Thrower_N(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
begin
  Pending := SelfData;
  Result := False;
end;

function FinallyStep(Args: TArgs; Argc: Integer; var Res: TValue; Throwing: Boolean): Boolean;
var D, R, P, F, Thunk: TValue; A: TValue;
begin
  D := SelfData;
  PushRoot(D); PushRoot(Arg(Args, Argc, 1));
  if not CallValue(VA(D)^.Data[0], VUndefined, nil, 0, R) then begin PopRoots(2); Exit(False); end;
  if not PromiseResolveC(VA(RootAt(TempTop - 2))^.Data[1], R, P) then begin PopRoots(2); Exit(False); end;
  PushRoot(P);
  if Throwing then Thunk := NewDataNative(Thrower_N, 0, RootAt(TempTop - 2))
  else Thunk := NewDataNative(ValueThunk_N, 0, RootAt(TempTop - 2));
  PushRoot(Thunk);
  if not GetV(RootAt(TempTop - 2), AtomThen, F) then begin PopRoots(4); Exit(False); end;
  A := RootAt(TempTop - 1);
  Result := CallValue(F, RootAt(TempTop - 2), @A, 1, Res);
  PopRoots(4);
end;

function ThenFinally_N(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
begin
  Result := FinallyStep(Args, Argc, Res, False);
end;

function CatchFinally_N(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
begin
  Result := FinallyStep(Args, Argc, Res, True);
end;

function Promise_Finally(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var C, D, F, OnF: TValue; A: array[0..1] of TValue;
begin
  if not IsObject(Args[0]) then Exit(ThrowTypeError('Promise.prototype.finally called on a non-object'));
  if not SpeciesCtor(Args[0], C) then Exit(False);
  OnF := Arg(Args, Argc, 1);
  PushRoot(OnF); PushRoot(C);
  if not IsCallable(OnF) then
  begin
    A[0] := OnF; A[1] := OnF;
  end
  else
  begin
    D := NewQuad(OnF, C, VUndefined, VUndefined);
    PushRoot(D);
    A[0] := NewDataNative(ThenFinally_N, 1, RootAt(TempTop - 1));
    PushRoot(A[0]);
    A[1] := NewDataNative(CatchFinally_N, 1, RootAt(TempTop - 2));
    A[0] := RootAt(TempTop - 1);
    PopRoots(2);
  end;
  PushRoot(A[0]); PushRoot(A[1]);
  if not GetV(Args[0], AtomThen, F) then begin PopRoots(4); Exit(False); end;
  if not IsCallable(F) then begin PopRoots(4); Exit(ThrowTypeError('then is not a function')); end;
  A[0] := RootAt(TempTop - 2); A[1] := RootAt(TempTop - 1);
  Result := CallValue(F, Args[0], @A[0], 2, Res);
  PopRoots(4);
end;

function Promise_Resolve(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
begin
  if not IsObject(Args[0]) then Exit(ThrowTypeError('Promise.resolve called on a non-object'));
  Result := PromiseResolveC(Args[0], Arg(Args, Argc, 1), Res);
end;

function Promise_Reject(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var Cap: TValue;
begin
  if not NewPromiseCapability(Args[0], Cap) then Exit(False);
  PushRoot(Cap);
  if not CapSettle(Cap, 1, Arg(Args, Argc, 1)) then begin PopRoots(1); Exit(False); end;
  Res := CapPromise(RootAt(TempTop - 1));
  PopRoots(1);
  Result := True;
end;

{ 27.2.4.8 Promise.try }
function Promise_Try(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var Cap, R: TValue;
begin
  if not IsObject(Args[0]) then Exit(ThrowTypeError('Promise.try called on a non-object'));
  { the callback first; its value through PromiseResolve (a promise of this constructor comes back as is), its
    throw through a new capability's reject }
  if CallValue(Arg(Args, Argc, 1), VUndefined, @Args[2], Max(0, Argc - 1), R) then
    Exit(PromiseResolveC(Args[0], R, Res));
  PushRoot(TakePending);
  if not NewPromiseCapability(Args[0], Cap) then begin PopRoots(1); Exit(False); end;
  PushRoot(Cap);
  if not CapSettle(RootAt(TempTop - 1), 1, RootAt(TempTop - 2)) then begin PopRoots(2); Exit(False); end;
  Res := CapPromise(RootAt(TempTop - 1));
  PopRoots(2);
  Result := True;
end;

{ 27.2.4.9 Promise.withResolvers }
function Promise_WithResolvers(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var Cap, O, ResFn, RejFn: TValue;
begin
  if Args[0] = PromiseCtor then
  begin
    { the fast capability has no functions: make them }
    Cap := NewPromiseObj(PromiseProto);
    PushRoot(Cap);
    MakeResolvingFunctions(Cap, ResFn, RejFn);
    PushRoot(ResFn); PushRoot(RejFn);
    Cap := NewQuad(RootAt(TempTop - 3), RootAt(TempTop - 2), RootAt(TempTop - 1), VUndefined);
    PopRoots(3);
  end
  else if not NewPromiseCapability(Args[0], Cap) then Exit(False);
  PushRoot(Cap);
  O := MkPtr(NewPlainObject(ObjectProto, 4));
  PushRoot(O);
  DefineOwn(O, AtomOf('promise'), VA(RootAt(TempTop - 2))^.Data[0], pfDefault);
  DefineOwn(RootAt(TempTop - 1), AtomResolve, VA(RootAt(TempTop - 2))^.Data[1], pfDefault);
  DefineOwn(RootAt(TempTop - 1), AtomOf('reject'), VA(RootAt(TempTop - 2))^.Data[2], pfDefault);
  Res := RootAt(TempTop - 1);
  PopRoots(2);
  Result := True;
end;

function Promise_Species(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
begin
  Res := Args[0];
  Result := True;
end;

{ ---------------- combinators (27.2.4.1-27.2.4.5) ----------------
  State shared by one call's element functions: a value array [values array, remaining (int), capability,
  kind]. An element function's data: [state, index, alreadyCalled, unused]. }
const
  ckAll = 0; ckAllSettled = 1; ckAny = 2;

{ the shared intrinsic %AggregateError.prototype% (made by G2.Builtins4) }
function AggregateErrorProto: TValue;
var C, P: TValue;
begin
  Result := ErrorProto;
  if GetV(GlobalObj, AtomOf('AggregateError'), C) and IsObject(C) and GetV(C, AtomPrototype, P) and IsObject(P) then
    Result := P;
  Pending := VUndefined;
end;

function FinishCombinator(St: TValue): Boolean;
var E: TValue; O: PObj;
begin
  if IntOf(VA(St)^.Data[3]) = ckAny then
  begin
    { 27.2.4.3.1 step 8.c: a new AggregateError whose errors property holds the reasons }
    PushRoot(St);
    E := AggregateErrorProto;
    O := NewObject(RootShapeFor(E), 4, okError);
    PushRoot(MkPtr(O));
    DefineOwn(RootAt(TempTop - 1), AtomOf('errors'), VA(RootAt(TempTop - 2))^.Data[0], pfHidden);
    Result := CapSettle(VA(RootAt(TempTop - 2))^.Data[2], 1, RootAt(TempTop - 1));
    PopRoots(2);
    Exit;
  end;
  Result := CapSettle(VA(St)^.Data[2], 0, VA(St)^.Data[0]);
end;

{ a resolve (Settled 0) or reject (1) element function; data [state, index, alreadyCalled] }
function ElementFn(Args: TArgs; Argc: Integer; var Res: TValue; Settled: Integer): Boolean;
var D, St, V, O: TValue; Kind, N: Integer;
begin
  D := SelfData;
  Res := VUndefined;
  if VA(D)^.Data[2] = VTrue then Exit(True);
  VA(D)^.Data[2] := VTrue;
  St := VA(D)^.Data[0];
  Kind := IntOf(VA(St)^.Data[3]);
  PushRoot(D); PushRoot(St);
  V := Arg(Args, Argc, 1);
  if Kind = ckAllSettled then
  begin
    PushRoot(V);
    O := MkPtr(NewPlainObject(ObjectProto, 4));
    PushRoot(O);
    if Settled = 0 then
    begin
      DefineOwn(O, AtomOf('status'), MkPtr(AtomOf('fulfilled')), pfDefault);
      DefineOwn(RootAt(TempTop - 1), AtomValue, RootAt(TempTop - 2), pfDefault);
    end
    else
    begin
      DefineOwn(O, AtomOf('status'), MkPtr(AtomOf('rejected')), pfDefault);
      DefineOwn(RootAt(TempTop - 1), AtomOf('reason'), RootAt(TempTop - 2), pfDefault);
    end;
    V := RootAt(TempTop - 1);
    PopRoots(2);
  end;
  PushRoot(V);
  if not SetElemV(VA(RootAt(TempTop - 2))^.Data[0], MkInt(IntOf(VA(RootAt(TempTop - 3))^.Data[1])), V) then
  begin
    PopRoots(3);
    Exit(False);
  end;
  St := RootAt(TempTop - 2);
  N := IntOf(VA(St)^.Data[1]) - 1;
  VA(St)^.Data[1] := MkInt(N);
  Result := True;
  if N = 0 then Result := FinishCombinator(St);
  PopRoots(3);
end;

function AllResolve_N(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
begin
  Result := ElementFn(Args, Argc, Res, 0);
end;

function AllReject_N(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
begin
  Result := ElementFn(Args, Argc, Res, 1);
end;

{ 27.2.4.1-27.2.4.5: the loop over the iterable (Kind 3: race). The state shared by the element functions is
  [values or errors array, remaining count, capability, kind]. }
function Combinator(C, Iterable: TValue; Kind: Integer; var Res: TValue): Boolean;
var Cap, Resolve, It, V, NextP, OnF, OnR, D, F, R, ResFn, RejFn, St: TValue; Index: Integer; Done, IterDone: Boolean;
  A: array[0..1] of TValue;
begin
  if not NewPromiseCapability(C, Cap) then Exit(False);
  if IsObject(Cap) then
  begin
    { a %Promise% capability gets its resolving functions now: every element shares their state }
    PushRoot(Cap);
    MakeResolvingFunctions(Cap, ResFn, RejFn);
    PushRoot(ResFn); PushRoot(RejFn);
    Cap := NewQuad(RootAt(TempTop - 3), RootAt(TempTop - 2), RootAt(TempTop - 1), VUndefined);
    PopRoots(3);
  end;
  PushRoot(Cap);
  { GetPromiseResolve(C) }
  if not GetV(C, AtomResolve, Resolve) then
  begin
    Result := RejectWithPending(RootAt(TempTop - 1), Res);
    PopRoots(1);
    Exit;
  end;
  if not IsCallable(Resolve) then
  begin
    ThrowTypeError('Promise resolve is not a function');
    Result := RejectWithPending(RootAt(TempTop - 1), Res);
    PopRoots(1);
    Exit;
  end;
  PushRoot(Resolve);
  if not GetIterator(Iterable, It) then
  begin
    Result := RejectWithPending(RootAt(TempTop - 2), Res);
    PopRoots(2);
    Exit;
  end;
  PushRoot(It);
  St := MkPtr(NewArray(0, 4));
  St := NewQuad(St, MkInt(1), RootAt(TempTop - 3), MkInt(Kind));
  PushRoot(St);
  { roots: Cap -4, Resolve -3, It -2, St -1 }
  Index := 0;
  IterDone := False;
  while True do
  begin
    if not IteratorStep(RootAt(TempTop - 2), V, Done) then begin IterDone := True; Break; end;
    if Done then
    begin
      IterDone := True;
      if Kind <> 3 then
      begin
        St := RootAt(TempTop - 1);
        VA(St)^.Data[1] := MkInt(IntOf(VA(St)^.Data[1]) - 1);
        if IntOf(VA(St)^.Data[1]) = 0 then
          if not FinishCombinator(St) then Break;
      end;
      Res := CapPromise(RootAt(TempTop - 4));
      PopRoots(4);
      Exit(True);
    end;
    PushRoot(V);
    if Kind <> 3 then ArrayPush(ObjOf(VA(RootAt(TempTop - 2))^.Data[0]), VUndefined);
    A[0] := RootAt(TempTop - 1);
    if not CallValue(RootAt(TempTop - 4), C, @A[0], 1, NextP) then begin PopRoots(1); Break; end;
    PushRoot(NextP);
    { roots: Cap -6, Resolve -5, It -4, St -3, V -2, NextP -1 }
    Cap := RootAt(TempTop - 6);
    if Kind = 3 then
    begin
      OnF := VA(Cap)^.Data[1];
      OnR := VA(Cap)^.Data[2];
    end
    else
    begin
      D := NewQuad(RootAt(TempTop - 3), MkInt(Index), VFalse, VUndefined);
      PushRoot(D);
      case Kind of
        ckAll:
          begin
            OnF := NewDataNative(AllResolve_N, 1, RootAt(TempTop - 1));
            OnR := VA(RootAt(TempTop - 7))^.Data[2];
          end;
        ckAllSettled:
          begin
            OnF := NewDataNative(AllResolve_N, 1, RootAt(TempTop - 1));
            PushRoot(OnF);
            OnR := NewDataNative(AllReject_N, 1, RootAt(TempTop - 2));
            OnF := RootAt(TempTop - 1);
            PopRoots(1);
          end;
      else
        begin
          OnR := NewDataNative(AllReject_N, 1, RootAt(TempTop - 1));
          OnF := VA(RootAt(TempTop - 7))^.Data[1];
        end;
      end;
      PopRoots(1);
      St := RootAt(TempTop - 3);
      VA(St)^.Data[1] := MkInt(IntOf(VA(St)^.Data[1]) + 1);
    end;
    { Invoke(nextPromise, "then", onFulfilled, onRejected) }
    PushRoot(OnF); PushRoot(OnR);
    if not GetV(RootAt(TempTop - 3), AtomThen, F) then begin PopRoots(4); Break; end;
    A[0] := RootAt(TempTop - 2); A[1] := RootAt(TempTop - 1);
    if not CallValue(F, RootAt(TempTop - 3), @A[0], 2, R) then begin PopRoots(4); Break; end;
    PopRoots(4);
    Inc(Index);
  end;
  { abrupt: close the iterator unless it failed itself, then reject }
  if not IterDone then IteratorCloseV(RootAt(TempTop - 2), True);
  Result := RejectWithPending(RootAt(TempTop - 4), Res);
  PopRoots(4);
end;

function Promise_All(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
begin
  Result := Combinator(Args[0], Arg(Args, Argc, 1), ckAll, Res);
end;

function Promise_AllSettled(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
begin
  Result := Combinator(Args[0], Arg(Args, Argc, 1), ckAllSettled, Res);
end;

function Promise_Any(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
begin
  Result := Combinator(Args[0], Arg(Args, Argc, 1), ckAny, Res);
end;

function Promise_Race(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
begin
  Result := Combinator(Args[0], Arg(Args, Argc, 1), 3, Res);
end;

function QueueMicrotask(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
begin
  if not IsCallable(Arg(Args, Argc, 1)) then Exit(ThrowTypeError('queueMicrotask argument must be a function'));
  EnqueueJob(jkCallback, Args[1], VUndefined, VUndefined);
  Res := VUndefined;
  Result := True;
end;

procedure InitPromise;
var Q: PValueArray; I: Integer;
begin
  AddRoot(@PromiseProto); AddRoot(@PromiseCtor); AddRoot(@ThenFn); AddRoot(@Jobs); AddRoot(@Scratch);
  AtomThen := AtomOf('then');
  AtomResolve := AtomOf('resolve');
  RealmVals([@PromiseProto, @PromiseCtor, @ThenFn]);
  { the agent's job queue (9.5): all realms share it }
  if not RealmInit then
  begin
    Q := NewValueArray(256);
    Q^.Len := 256;
    for I := 0 to 255 do Q^.Data[I] := VUndefined;
    Jobs := MkPtr(Q);
    JobHead := 0; JobCount := 0;
  end;
  AsyncStartHook := @AsyncStart;
  AsyncGenStartHook := @AsyncGenStart;
  AsyncFromSyncHook := @AsyncFromSync;
  if not RealmInit then AtomAGQueue := NewKeyAtom('asyncGeneratorQueue', kfInternal);   { one per agent }
  DefineOwn(AsyncGeneratorProto, AtomNext, NewNativeFunctionData(AsyncGen_Method, 'next', 1, 0), pfHidden);
  DefineOwn(AsyncGeneratorProto, AtomOf('return'), NewNativeFunctionData(AsyncGen_Method, 'return', 1, 2), pfHidden);
  DefineOwn(AsyncGeneratorProto, AtomOf('throw'), NewNativeFunctionData(AsyncGen_Method, 'throw', 1, 1), pfHidden);
  DefineOwn(AsyncFromSyncIterProto, AtomNext, NewNativeFunctionData(ASMethod, 'next', 1, 0), pfHidden);
  DefineOwn(AsyncFromSyncIterProto, AtomOf('return'), NewNativeFunctionData(ASMethod, 'return', 1, 2), pfHidden);
  DefineOwn(AsyncFromSyncIterProto, AtomOf('throw'), NewNativeFunctionData(ASMethod, 'throw', 1, 1), pfHidden);

  PromiseProto := MkPtr(NewPlainObject(ObjectProto, 8));
  PromiseCtor := NewNativeFunction(Promise_Ctor, 'Promise', 1);
  DefineOwn(PromiseCtor, AtomPrototype, PromiseProto, 0);
  DefineOwn(PromiseProto, AtomConstructor, PromiseCtor, pfHidden);
  DefineOwn(GlobalObj, AtomOf('Promise'), PromiseCtor, pfHidden);
  DefineNative(PromiseProto, 'then', Promise_Then, 2);
  DefineNative(PromiseProto, 'catch', Promise_Catch, 1);
  DefineNative(PromiseProto, 'finally', Promise_Finally, 1);
  DefineOwn(PromiseProto, SymKey(SymToStringTagW), MkPtr(AtomOf('Promise')), pfConfigurable);
  DefineNative(PromiseCtor, 'resolve', Promise_Resolve, 1);
  DefineNative(PromiseCtor, 'reject', Promise_Reject, 1);
  DefineNative(PromiseCtor, 'all', Promise_All, 1);
  DefineNative(PromiseCtor, 'allSettled', Promise_AllSettled, 1);
  DefineNative(PromiseCtor, 'any', Promise_Any, 1);
  DefineNative(PromiseCtor, 'race', Promise_Race, 1);
  DefineNative(PromiseCtor, 'try', Promise_Try, 1);
  DefineNative(PromiseCtor, 'withResolvers', Promise_WithResolvers, 0);
  DefineNativeGetterKey(PromiseCtor, SymKey(SymSpecies), 'get [Symbol.species]', Promise_Species);
  DefineNative(GlobalObj, 'queueMicrotask', QueueMicrotask, 1);
end;

function NewPendingPromise: TValue;
begin
  Result := NewPromiseObj(PromiseProto);
end;

procedure RejectPendingPromise(P, Reason: TValue);
begin
  RejectPromise(P, Reason);
end;

function PromiseStatus(P: TValue; var Result_: TValue): Integer;
begin
  Result := ObjOf(P)^.Pad;
  Result_ := ObjOf(P)^.Aux1;
end;

procedure EnqueueCallback(F: TValue);
begin
  EnqueueJob(jkCallback, F, VUndefined, VUndefined);
end;

procedure ResolvePendingPromise(P, V: TValue);
begin
  ResolvePromise(P, V);
end;

end.
