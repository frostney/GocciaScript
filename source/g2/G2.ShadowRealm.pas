unit G2.ShadowRealm;
{ ShadowRealm (the TC39 proposal, https://tc39.es/proposal-shadowrealm/): a realm whose code shares no objects
  with the code that made it. evaluate runs a Script there and hands back primitives and wrapped functions
  (3.1 Wrapped Function Exotic Objects): a wrapped function calls its target in the target's realm, with its this
  and arguments wrapped for that realm, and wraps the result for the caller's. The errors of either side never
  cross: an abrupt completion on the other side is a TypeError of the caller's realm. }
{$mode delphiunicode}{$H+}
{$MINFPCONSTPREC 64}

interface

procedure InitShadowRealm;

implementation

uses
  SysUtils, Math, G2.Core, G2.Bytecode, G2.VM, G2.Eval, G2.ObjModel, G2.Promise;

threadvar
  ShadowRealmProto: TValue;
  RealmKey: PStr;   { the [[ShadowRealm]] internal slot: the realm's index }

function Arg(Args: PValue; Argc, I: Integer): TValue; inline;
begin
  if I <= Argc then Result := Args[I] else Result := VUndefined;
end;

{ ValidateShadowRealmObject: R the realm of the ShadowRealm object V }
function ThisRealm(V: TValue; var R: Integer): Boolean;
var S: Integer;
begin
  if IsObject(V) and (ObjOf(V)^.OKind = okPlain) then
  begin
    S := ShapeLookup(ObjOf(V)^.Shape, RealmKey);
    if S >= 0 then
    begin
      R := IntOf(GetSlot(ObjOf(V), S));
      Exit(True);
    end;
  end;
  Result := ThrowTypeError('ShadowRealm method called on an incompatible receiver');
end;

{ the TypeError of the running realm that replaces an abrupt completion of the other side }
function ThrowCrossing(const What: UnicodeString): Boolean;
begin
  Pending := VUndefined;
  Result := ThrowTypeError(What);
end;

function Wrapped_Call(Args: PValue; Argc: Integer; var Res: TValue): Boolean; forward;

{ CopyNameAndLength(F, Target): an abrupt completion is the caller's TypeError (WrappedFunctionCreate step 7) }
function CopyNameAndLength(F, Target: TValue): Boolean;
var D: TPropDesc; Found: Boolean; V: TValue; L: Double;
begin
  PushRoot(F); PushRoot(Target);
  Result := False;
  L := 0;
  if not GetOwnPropertyV(RootAt(TempTop - 1), AtomLength, D, Found) then begin PopRoots(2); Exit; end;
  if Found then
  begin
    if not GetV(RootAt(TempTop - 1), AtomLength, V) then begin PopRoots(2); Exit; end;
    if IsNumber(V) then
    begin
      L := NumOf(V);
      if IsNaN(L) then L := 0
      else if IsInfinite(L) then begin if L < 0 then L := 0; end
      else L := Max(Int(L), 0);
    end;
  end;
  DefineOwn(RootAt(TempTop - 2), AtomLength, MkNumber(L), pfConfigurable);
  if not GetV(RootAt(TempTop - 1), AtomName, V) then begin PopRoots(2); Exit; end;
  if not IsString(V) then V := MkPtr(AtomOf(''));
  DefineOwn(RootAt(TempTop - 2), AtomName, V, pfConfigurable);
  PopRoots(2);
  Result := True;
end;

{ 3.1.3 WrappedFunctionCreate(callerRealm R, Target): a function of realm R }
function WrappedFunctionCreate(R: Integer; Target: TValue; var Res: TValue): Boolean;
var R0: Integer; F: TValue;
begin
  PushRoot(Target);
  R0 := CurRealm;
  SwitchRealm(R);
  F := NewNativeFunction(Wrapped_Call, '', 0);
  SwitchRealm(R0);
  ObjOf(F)^.Aux1 := RootAt(TempTop - 1);
  WriteBarrier(@ObjOf(F)^.Hdr, RootAt(TempTop - 1));
  PushRoot(F);
  if not CopyNameAndLength(F, RootAt(TempTop - 2)) then
  begin
    PopRoots(2);
    Exit(ThrowCrossing('ShadowRealm: the wrapped function''s name or length threw'));
  end;
  Res := RootAt(TempTop - 1);
  PopRoots(2);
  Result := True;
end;

{ 3.1.4 GetWrappedValue(R, V): a primitive, or a callable object wrapped for realm R }
function WrapValue(R: Integer; V: TValue; var Res: TValue): Boolean;
begin
  if not IsObject(V) then
  begin
    Res := V;
    Exit(True);
  end;
  if not IsCallable(V) then Exit(ThrowTypeError('ShadowRealm: an object that is not callable cannot cross realms'));
  Result := WrappedFunctionCreate(R, V, Res);
end;

{ 3.1.1 [[Call]] of a wrapped function (its target in Aux1): it runs in its own realm, the caller's }
function Wrapped_Call(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var CallerRealm, TargetRealm, I, Base, SavedTop: Integer; V: TValue; Ok: Boolean;
begin
  CallerRealm := CurRealm;
  if not GetFunctionRealm(ObjOf(NativeSelf)^.Aux1, TargetRealm) then Exit(False);
  { the target, then this and the arguments wrapped for the target's realm, on the value stack (roots) }
  SavedTop := StackTop;
  Base := StackTop;
  if Base + Argc + 2 >= STACK_MAX then Exit(ThrowError('RangeError', 'Maximum call stack size exceeded'));
  VStack[Base] := ObjOf(NativeSelf)^.Aux1;
  for I := 0 to Argc do VStack[Base + 1 + I] := VUndefined;
  StackTop := Base + Argc + 2;
  for I := 0 to Argc do
  begin
    if not WrapValue(TargetRealm, Args[I], V) then
    begin
      StackTop := SavedTop;
      Exit(False);
    end;
    VStack[Base + 1 + I] := V;
  end;
  Ok := CallValue(VStack[Base], VStack[Base + 1], @VStack[Base + 2], Argc, V);
  StackTop := SavedTop;
  if not Ok then Exit(ThrowCrossing('ShadowRealm: the wrapped function threw'));
  Result := WrapValue(CallerRealm, V, Res);
end;

{ 3.2.1 ShadowRealm ( ) }
function SR_Ctor(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var P: TValue; R: Integer; O: PObj;
begin
  if CurNewTarget = VUndefined then Exit(ThrowTypeError('Constructor ShadowRealm requires ''new'''));
  if not ProtoFromNewTarget(ShadowRealmProto, P) then Exit(False);
  PushRoot(P);
  if not CreateRealm(R) then
  begin
    PopRoots(1);
    Exit(False);
  end;
  O := NewPlainObject(RootAt(TempTop - 1), 2);
  PopRoots(1);
  AddProperty(O, RealmKey, MkInt(R), pfHidden);
  Res := MkPtr(O);
  Result := True;
end;

{ 3.4.1 ShadowRealm.prototype.evaluate ( sourceText ): PerformShadowRealmEval }
function SR_Evaluate(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var EvalRealm, CallerRealm: Integer; Src, Clo, V: TValue; T: PFuncTemplate; Err: UnicodeString; Ok: Boolean;
begin
  if not ThisRealm(Args[0], EvalRealm) then Exit(False);
  Src := Arg(Args, Argc, 1);
  if not IsString(Src) then Exit(ThrowTypeError('ShadowRealm.prototype.evaluate requires a string'));
  CallerRealm := CurRealm;
  { compiled in the evaluation realm: a Script's global declarations are its global environment's }
  SwitchRealm(EvalRealm);
  if not ParseAndCompile(StrToUnicode(StrOf(Src)), '<ShadowRealm>', True, T, Err, 1) then
  begin
    SwitchRealm(CallerRealm);
    Exit(ThrowError('SyntaxError', Err));
  end;
  Ok := MakeScriptClosure(T, Clo) and CallValue(Clo, GlobalObj, nil, 0, V);
  SwitchRealm(CallerRealm);
  if not Ok then Exit(ThrowCrossing('ShadowRealm.prototype.evaluate: the evaluation threw'));
  Result := WrapValue(CallerRealm, V, Res);
end;

{ ExportGetter (ShadowRealmImportValue step 7): the export named by its data (Aux1) of the module namespace
  Args[1], wrapped for its own realm (the caller's) }
function SR_ExportGetter(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var NS, V: TValue; K: PStr; D: TPropDesc; Found: Boolean;
begin
  NS := Arg(Args, Argc, 1);
  if not IsObject(NS) then Exit(ThrowTypeError('ShadowRealm.prototype.importValue: no module namespace'));
  PushRoot(NS);
  K := AtomOf(StrToUnicode(StrOf(ObjOf(NativeSelf)^.Aux1)));
  if not GetOwnPropertyV(RootAt(TempTop - 1), K, D, Found) then begin PopRoots(1); Exit(False); end;
  if not Found then
  begin
    PopRoots(1);
    Exit(ThrowTypeError('ShadowRealm.prototype.importValue: the module has no export ''' + StrToUnicode(K) + ''''));
  end;
  Result := GetV(RootAt(TempTop - 1), K, V);
  PopRoots(1);
  if Result then Result := WrapValue(CurRealm, V, Res);
end;

{ 3.4.2 ShadowRealm.prototype.importValue ( specifier, exportName ): ShadowRealmImportValue }
function SR_ImportValue(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var EvalRealm, CallerRealm, Base: Integer; S, Inner, F: TValue; Referrer: UnicodeString;
begin
  if not ThisRealm(Args[0], EvalRealm) then Exit(False);
  if not ToStringV(Arg(Args, Argc, 1), S) then Exit(False);
  if not IsString(Arg(Args, Argc, 2)) then Exit(ThrowTypeError('ShadowRealm.prototype.importValue requires an export name'));
  CallerRealm := CurRealm;
  { the specifier resolves against the code that called importValue }
  if FrameTop > 0 then Referrer := Frames[FrameTop - 1].Tpl^.SourceName else Referrer := '';
  Base := TempTop;
  PushRoot(S);
  { the loading runs in the evaluation realm (import()'s job is a function of the realm that made it) }
  SwitchRealm(EvalRealm);
  ImportPhase := 0;
  if not ImportHook(Referrer, 0, RootAt(Base), VUndefined, Inner) then
  begin
    SwitchRealm(CallerRealm);
    PopRoots(1);
    Exit(False);
  end;
  SwitchRealm(CallerRealm);
  PushRoot(Inner);
  F := NewNativeFunction(SR_ExportGetter, '', 1);
  ObjOf(F)^.Aux1 := Args[2];
  WriteBarrier(@ObjOf(F)^.Hdr, Args[2]);
  PushRoot(F);
  PushRoot(NewPendingPromise);
  { rejections become the caller's TypeError: its %ThrowTypeError% }
  PromiseThenNative(RootAt(Base + 1), RootAt(Base + 2), PAccessor(PtrOf(ThrowerAcc))^.Getter, RootAt(Base + 3));
  Res := RootAt(Base + 3);
  PopRoots(4);
  Result := True;
end;

procedure InitShadowRealm;
begin
  AddRoot(@ShadowRealmProto);
  RealmVals([@ShadowRealmProto]);
  if not RealmInit then RealmKey := NewKeyAtom('%shadowRealm', kfInternal);   { one per agent }
  ShadowRealmProto := MkPtr(NewPlainObject(ObjectProto, 4));
  PushRoot(NewNativeFunction(SR_Ctor, 'ShadowRealm', 0));
  DefineOwn(RootAt(TempTop - 1), AtomPrototype, ShadowRealmProto, 0);
  DefineOwn(ShadowRealmProto, AtomConstructor, RootAt(TempTop - 1), pfHidden);
  DefineOwn(GlobalObj, AtomOf('ShadowRealm'), RootAt(TempTop - 1), pfHidden);
  PopRoots(1);
  DefineNative(ShadowRealmProto, 'evaluate', SR_Evaluate, 1);
  DefineNative(ShadowRealmProto, 'importValue', SR_ImportValue, 2);
  PushRoot(StrFromUnicode('ShadowRealm'));
  DefineOwn(ShadowRealmProto, SymKey(SymToStringTagW), RootAt(TempTop - 1), pfConfigurable);
  PopRoots(1);
end;

end.
