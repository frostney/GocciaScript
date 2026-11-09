unit G2.Builtins3;
{ Date, Symbol.for, WeakMap/WeakSet, property descriptors and Reflect: the rest of what the tooling bundles use. }
{$mode delphiunicode}{$H+}
{$MINFPCONSTPREC 64}
{$POINTERMATH ON}
{$Q-}{$R-}

interface

procedure InitBuiltins3;

implementation

uses
  SysUtils, Math, DateUtils, Linux, UnixType, G2.Core, G2.VM, G2.Builtins;

const
  okDate = 18;

type
  TArgs = PValue;

threadvar
  DateProto, WeakMapProto, WeakSetProto, Scratch, Scratch2, SymToStringTag: TValue;
  SymbolRegistry: TValue;   { plain object: atom -> symbol, so the GC sees the symbols }

function Arg(Args: TArgs; Argc, I: Integer): TValue; inline;
begin
  if I <= Argc then Result := Args[I] else Result := VUndefined;
end;

{ ---------------- Date (UTC; enough for timing and formatting) ---------------- }

function NowMs: Double;
var TS: TTimeSpec;
begin
  clock_gettime(CLOCK_REALTIME, @TS);
  Result := TS.tv_sec * 1000.0 + TS.tv_nsec div 1000000;
end;

function ThisTime(Args: TArgs; var T: Double): Boolean;
begin
  if not IsObject(Args[0]) or (ObjOf(Args[0])^.OKind <> okDate) then Exit(ThrowTypeError('this is not a Date object.'));
  T := NumOf(ObjOf(Args[0])^.Aux1);
  Result := True;
end;

function Date_Ctor(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var T, D: Double; O: PObj; S: TValue; Y, Mo, Dd, H, Mi, Se, Ms: Double; DT: TDateTime;
begin
  if CurNewTarget = VUndefined then
  begin
    Res := StrFromUnicode(UnicodeString(FormatDateTime('ddd mmm dd yyyy hh:nn:ss', Now)) + ' GMT+0000');
    Exit(True);
  end;
  if Argc = 0 then T := NowMs
  else if Argc = 1 then
  begin
    if IsObject(Args[1]) and (ObjOf(Args[1])^.OKind = okDate) then T := NumOf(ObjOf(Args[1])^.Aux1)
    else if IsString(Args[1]) then
    begin
      if TryISO8601ToDate(StrToUnicode(StrOf(Args[1])), DT, True) then T := DateTimeToUnix(DT, False) * 1000.0
      else T := NaN;
    end
    else
    begin
      if not ToNumberV(Args[1], D) then Exit(False);
      T := D;
    end;
  end
  else
  begin
    if not ToNumberV(Args[1], Y) then Exit(False);
    if not ToNumberV(Args[2], Mo) then Exit(False);
    Dd := 1; H := 0; Mi := 0; Se := 0; Ms := 0;
    if Argc >= 3 then if not ToNumberV(Args[3], Dd) then Exit(False);
    if Argc >= 4 then if not ToNumberV(Args[4], H) then Exit(False);
    if Argc >= 5 then if not ToNumberV(Args[5], Mi) then Exit(False);
    if Argc >= 6 then if not ToNumberV(Args[6], Se) then Exit(False);
    if Argc >= 7 then if not ToNumberV(Args[7], Ms) then Exit(False);
    try
      DT := EncodeDateTime(Trunc(Y), Trunc(Mo) + 1, Trunc(Dd), Trunc(H), Trunc(Mi), Trunc(Se), Trunc(Ms));
      T := DateTimeToUnix(DT, False) * 1000.0 + Trunc(Ms);
    except
      T := NaN;
    end;
  end;
  O := NewObject(RootShapeFor(DateProto), 0, okDate);
  O^.Aux1 := MkNumber(T);
  Res := MkPtr(O);
  Result := True;
end;

function Date_GetTime(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var T: Double;
begin
  if not ThisTime(Args, T) then Exit(False);
  Res := MkNumber(T);
  Result := True;
end;

function DatePart(Args: TArgs; var Res: TValue; Part: Integer): Boolean;
var T: Double; DT: TDateTime;
begin
  if not ThisTime(Args, T) then Exit(False);
  if IsNaN(T) then begin Res := MkDouble(NaN); Exit(True); end;
  DT := UnixToDateTime(Trunc(T / 1000.0), True) + (T - Trunc(T / 1000.0) * 1000.0) / 86400000.0;
  case Part of
    0: Res := MkInt(YearOf(DT));
    1: Res := MkInt(MonthOf(DT) - 1);
    2: Res := MkInt(DayOf(DT));
    3: Res := MkInt(HourOf(DT));
    4: Res := MkInt(MinuteOf(DT));
    5: Res := MkInt(SecondOf(DT));
    6: Res := MkInt(Trunc(T) mod 1000);
    7: Res := MkInt(DayOfWeek(DT) - 1);
    8: Res := StrFromUnicode(UnicodeString(FormatDateTime('yyyy-mm-dd"T"hh:nn:ss.zzz"Z"', DT)));
  else Res := MkInt(0);
  end;
  Result := True;
end;
function Date_GetFullYear(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := DatePart(Args, Res, 0); end;
function Date_GetMonth(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := DatePart(Args, Res, 1); end;
function Date_GetDate(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := DatePart(Args, Res, 2); end;
function Date_GetHours(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := DatePart(Args, Res, 3); end;
function Date_GetMinutes(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := DatePart(Args, Res, 4); end;
function Date_GetSeconds(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := DatePart(Args, Res, 5); end;
function Date_GetMilliseconds(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := DatePart(Args, Res, 6); end;
function Date_GetDay(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := DatePart(Args, Res, 7); end;
function Date_ToISOString(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := DatePart(Args, Res, 8); end;
function Date_GetTimezoneOffset(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := DatePart(Args, Res, 9); end;

function Date_Now(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
begin
  Res := MkNumber(NowMs);
  Result := True;
end;

{ ---------------- Symbol registry ---------------- }

function Symbol_For(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var S: TValue; K: PStr; I: Integer;
begin
  if not ToStringV(Arg(Args, Argc, 1), S) then Exit(False);
  K := Atomize(StrOf(S));
  I := GetOwnSlot(ObjOf(SymbolRegistry), K);
  if I >= 0 then begin Res := GetSlot(ObjOf(SymbolRegistry), I); Exit(True); end;
  S := NewSymbolValue(StrToUnicode(K));
  PushRoot(S);
  AddProperty(ObjOf(SymbolRegistry), K, S, pfDefault);
  Res := RootAt(TempTop - 1);
  PopRoots(1);
  Result := True;
end;

function Symbol_KeyFor(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var O: PObj; I: Integer;
begin
  if not (IsPtr(Arg(Args, Argc, 1)) and (PCell(PtrOf(Args[1]))^.Kind = ckSymbol)) then
    Exit(ThrowTypeError('Symbol.keyFor: argument is not a symbol'));
  Res := VUndefined;
  O := ObjOf(SymbolRegistry);
  for I := 0 to O^.Shape^.Count - 1 do
    if GetSlot(O, I) = Arg(Args, Argc, 1) then begin Res := MkPtr(O^.Shape^.Keys[I]); Break; end;
  Result := True;
end;

{ ---------------- WeakMap / WeakSet (held strongly: the spike has no ephemerons) ---------------- }

function WeakCtor(Args: TArgs; Argc: Integer; var Res: TValue; IsSet: Boolean): Boolean;
var MapCtor: TValue; Proto: TValue;
begin
  if CurNewTarget = VUndefined then Exit(ThrowTypeError('Constructor requires ''new'''));
  if IsSet then
  begin
    if not GetV(GlobalObj, AtomOf('Set'), MapCtor) then Exit(False);
    Proto := WeakSetProto;
  end
  else
  begin
    if not GetV(GlobalObj, AtomOf('Map'), MapCtor) then Exit(False);
    Proto := WeakMapProto;
  end;
  if not Construct(MapCtor, @Args[1], Argc, MapCtor, Res) then Exit(False);
  SetProto(ObjOf(Res), Proto);
  Result := True;
end;
function WeakMap_Ctor(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := WeakCtor(Args, Argc, Res, False); end;
function WeakSet_Ctor(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := WeakCtor(Args, Argc, Res, True); end;

{ ---------------- property descriptors ---------------- }

function DescriptorOf(O: TValue; K: PStr; var Res: TValue): Boolean;
var P: PObj; S: Integer; F: Byte; V: TValue; D: PObj; Acc: PAccessor; Idx: Int64;
begin
  Res := VUndefined;
  if not IsObject(O) then Exit(True);
  P := ObjOf(O);
  S := ShapeLookup(P^.Shape, K);
  if S < 0 then
  begin
    if (P^.Elements <> nil) and IndexFromString(K, Idx) and (Idx < P^.Elements^.Len) and (P^.Elements^.Data[Idx] <> VHole) then
    begin
      V := P^.Elements^.Data[Idx];
      F := pfDefault;
    end
    else if not OwnVirtual(P, K, V, F) then Exit(True);
  end
  else
  begin
    F := P^.Shape^.PFlags[S];
    V := GetSlot(P, S);
  end;
  PushRoot(V);
  D := NewPlainObject(ObjectProto, 4);
  PushRoot(MkPtr(D));
  if (F and pfAccessor) <> 0 then
  begin
    Acc := PAccessor(PtrOf(RootAt(TempTop - 2)));
    AddProperty(ObjOf(RootAt(TempTop - 1)), AtomOf('get'), Acc^.Getter, pfDefault);
    Acc := PAccessor(PtrOf(RootAt(TempTop - 2)));
    AddProperty(ObjOf(RootAt(TempTop - 1)), AtomOf('set'), Acc^.Setter, pfDefault);
  end
  else
  begin
    AddProperty(ObjOf(RootAt(TempTop - 1)), AtomValue, RootAt(TempTop - 2), pfDefault);
    AddProperty(ObjOf(RootAt(TempTop - 1)), AtomOf('writable'), MkBool((F and pfWritable) <> 0), pfDefault);
  end;
  AddProperty(ObjOf(RootAt(TempTop - 1)), AtomOf('enumerable'), MkBool((F and pfEnumerable) <> 0), pfDefault);
  AddProperty(ObjOf(RootAt(TempTop - 1)), AtomOf('configurable'), MkBool((F and pfConfigurable) <> 0), pfDefault);
  Res := RootAt(TempTop - 1);
  PopRoots(2);
  Result := True;
end;

function Object_GetOwnPropertyDescriptor(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var K: PStr;
begin
  if not ToPropKey(Arg(Args, Argc, 2), K) then Exit(False);
  Result := DescriptorOf(Arg(Args, Argc, 1), K, Res);
end;

function ObjProto_PropertyIsEnumerable(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var K: PStr; O: PObj; Idx: Int64; S: Integer;
begin
  if not ToPropKey(Arg(Args, Argc, 1), K) then Exit(False);
  Res := VFalse;
  if IsString(Args[0]) then
  begin
    Res := MkBool(IndexFromString(K, Idx) and (Idx < StrOf(Args[0])^.Len));
    Exit(True);
  end;
  if not IsObject(Args[0]) then Exit(True);
  O := ObjOf(Args[0]);
  if (O^.Elements <> nil) and IndexFromString(K, Idx) then
    Res := MkBool((Idx < O^.Elements^.Len) and (O^.Elements^.Data[Idx] <> VHole))
  else
  begin
    S := ShapeLookup(O^.Shape, K);
    Res := MkBool((S >= 0) and ((O^.Shape^.PFlags[S] and pfEnumerable) <> 0));
  end;
  Result := True;
end;

function Object_GetOwnPropertyDescriptors(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var Keys, D: TValue; I, N: Integer; K: PStr; R: PObj;
begin
  R := NewPlainObject(ObjectProto, 8);
  PushRoot(MkPtr(R));
  if IsObject(Arg(Args, Argc, 1)) then
  begin
    Keys := OwnKeys(ObjOf(Args[1]), False);
    PushRoot(Keys);
    N := ArrayLen(ObjOf(Keys));
    for I := 0 to N - 1 do
    begin
      K := Atomize(StrOf(ObjOf(RootAt(TempTop - 1))^.Elements^.Data[I]));
      if not DescriptorOf(Args[1], K, D) then begin PopRoots(2); Exit(False); end;
      DefineOwn(RootAt(TempTop - 2), K, D, pfDefault);
    end;
    PopRoots(1);
  end;
  Res := RootAt(TempTop - 1);
  PopRoots(1);
  Result := True;
end;

function Object_DefineProperties(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var Keys, D, DP: TValue; I, N: Integer; K: PStr; CallArgs: array[0..2] of TValue;
begin
  if not IsObject(Arg(Args, Argc, 1)) then Exit(ThrowTypeError('Object.defineProperties called on non-object'));
  if not IsObject(Arg(Args, Argc, 2)) then begin Res := Args[1]; Exit(True); end;
  if not GetV(GlobalObj, AtomOf('Object'), DP) then Exit(False);
  if not GetV(DP, AtomOf('defineProperty'), DP) then Exit(False);
  Scratch2 := DP;
  Keys := OwnKeys(ObjOf(Args[2]), True);
  PushRoot(Keys);
  N := ArrayLen(ObjOf(Keys));
  for I := 0 to N - 1 do
  begin
    K := Atomize(StrOf(ObjOf(RootAt(TempTop - 1))^.Elements^.Data[I]));
    if not GetV(Args[2], K, D) then begin PopRoots(1); Exit(False); end;
    CallArgs[0] := Args[1]; CallArgs[1] := MkPtr(K); CallArgs[2] := D;
    if not CallValue(Scratch2, VUndefined, @CallArgs[0], 3, DP) then begin PopRoots(1); Exit(False); end;
  end;
  PopRoots(1);
  Res := Args[1];
  Result := True;
end;

function IsSymbolKey(K: PStr): Boolean; inline;
begin
  Result := (K^.Hdr.Extra and kfSymbol) <> 0;
end;

{ the own symbol-keyed properties, in insertion order, as Symbol values }
function OwnSymbols(O: TValue; OnlyEnumerable: Boolean): TValue;
var A: PObj; I: Integer; S: PShape; V: TValue;
begin
  PushRoot(O);
  A := NewArray(0, 4);
  PushRoot(MkPtr(A));
  S := ObjOf(RootAt(TempTop - 2))^.Shape;
  for I := 0 to S^.Count - 1 do
    if IsSymbolKey(S^.Keys[I]) and (not OnlyEnumerable or ((S^.PFlags[I] and pfEnumerable) <> 0)) then
    begin
      V := KeyToValue(S^.Keys[I]);
      if not IsString(V) then ArrayPush(ObjOf(RootAt(TempTop - 1)), V);
      S := ObjOf(RootAt(TempTop - 2))^.Shape;
    end;
  Result := RootAt(TempTop - 1);
  PopRoots(2);
end;

function Object_GetOwnPropertySymbols(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
begin
  if not IsObject(Arg(Args, Argc, 1)) then begin Res := MkPtr(NewArray(0, 0)); Exit(True); end;
  Res := OwnSymbols(Args[1], False);
  Result := True;
end;

function Object_Assign3(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var I, J, N, Pass: Integer; Keys, V: TValue; K: PStr;
begin
  if not IsObject(Arg(Args, Argc, 1)) then Exit(ThrowTypeError('Cannot convert undefined or null to object'));
  for I := 2 to Argc do
  begin
    if not IsObject(Args[I]) then Continue;
    for Pass := 0 to 1 do
    begin
      if Pass = 0 then Keys := OwnKeys(ObjOf(Args[I]), True) else Keys := OwnSymbols(Args[I], True);
      PushRoot(Keys);
      N := ArrayLen(ObjOf(Keys));
      for J := 0 to N - 1 do
      begin
        if not ToPropKey(ObjOf(RootAt(TempTop - 1))^.Elements^.Data[J], K) then begin PopRoots(1); Exit(False); end;
        if not GetV(Args[I], K, V) then begin PopRoots(1); Exit(False); end;
        if not SetV(Args[1], K, V) then begin PopRoots(1); Exit(False); end;
      end;
      PopRoots(1);
    end;
  end;
  Res := Args[1];
  Result := True;
end;

{ SetIntegrityLevel: sealed drops configurable, frozen also writable (dense elements stay as they are) }
procedure IntegrityLevel(O: TValue; Frozen: Boolean);
var I: Integer; S: PShape; F: Byte;
begin
  if not IsObject(O) or (ObjOf(O)^.OKind = okProxy) then Exit;
  PushRoot(O);
  I := 0;
  while I < ObjOf(RootAt(TempTop - 1))^.Shape^.Count do
  begin
    S := ObjOf(RootAt(TempTop - 1))^.Shape;
    if (S^.Keys[I]^.Hdr.Extra and kfPrivate) <> 0 then begin Inc(I); Continue; end;   { private elements stay }
    F := S^.PFlags[I] and not pfConfigurable;
    if Frozen and ((F and pfAccessor) = 0) then F := F and not pfWritable;
    if F <> S^.PFlags[I] then ChangeFlags(ObjOf(RootAt(TempTop - 1)), S^.Keys[I], F);
    Inc(I);
  end;
  ObjOf(RootAt(TempTop - 1))^.OFlags := ObjOf(RootAt(TempTop - 1))^.OFlags or 8;
  PopRoots(1);
end;

function Object_Freeze3(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
begin
  IntegrityLevel(Arg(Args, Argc, 1), True);
  Res := Arg(Args, Argc, 1);
  Result := True;
end;

function Object_Seal3(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
begin
  IntegrityLevel(Arg(Args, Argc, 1), False);
  Res := Arg(Args, Argc, 1);
  Result := True;
end;

function Object_Is(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var A, B: TValue; X, Y: Double;
begin
  A := Arg(Args, Argc, 1); B := Arg(Args, Argc, 2);
  if IsNumber(A) and IsNumber(B) then
  begin
    X := NumOf(A); Y := NumOf(B);
    if IsNaN(X) or IsNaN(Y) then Res := MkBool(IsNaN(X) and IsNaN(Y))
    else Res := MkBool((X = Y) and ((X <> 0) or ((PQWord(@X)^ shr 63) = (PQWord(@Y)^ shr 63))));
  end
  else Res := MkBool(StrictEquals(A, B));
  Result := True;
end;

function IsProxy(V: TValue): Boolean; inline;
begin
  Result := IsObject(V) and (ObjOf(V)^.OKind = okProxy);
end;

{ [[IsExtensible]], through the trap for a proxy }
function IsExtensibleV(V: TValue; var Ext: Boolean): Boolean;
var R: TValue; Had: Boolean;
begin
  while IsProxy(V) do
  begin
    if not ProxyInvoke(V, 'isExtensible', VUndefined, 0, R, Had) then Exit(False);
    if Had then begin Ext := ToBool(R); Exit(True); end;
    V := ObjOf(V)^.Aux1;
  end;
  Ext := IsObject(V) and ((ObjOf(V)^.OFlags and 8) = 0);
  Result := True;
end;

{ TestIntegrityLevel: not extensible, and every own property non-configurable (and, frozen, non-writable) }
function TestIntegrity(V: TValue; Frozen: Boolean; var Res: TValue): Boolean;
var Ext: Boolean; O: PObj; I: Integer; F: Byte; Keys: TValue;
begin
  if not IsObject(V) then begin Res := VTrue; Exit(True); end;
  if not IsExtensibleV(V, Ext) then Exit(False);
  if Ext then begin Res := VFalse; Exit(True); end;
  if IsProxy(V) then
  begin
    { the keys come through the trap; their descriptors are not consulted here }
    if not ProxyOwnKeys(V, Keys) then Exit(False);
    Res := VTrue;
    Exit(True);
  end;
  O := ObjOf(V);
  Res := VFalse;
  if O^.Elements <> nil then
    for I := 0 to O^.Elements^.Len - 1 do
      if O^.Elements^.Data[I] <> VHole then Exit(True);
  for I := 0 to O^.Shape^.Count - 1 do
  begin
    F := O^.Shape^.PFlags[I];
    if (F and pfConfigurable) <> 0 then Exit(True);
    if Frozen and ((F and pfAccessor) = 0) and ((F and pfWritable) <> 0) then Exit(True);
  end;
  Res := VTrue;
  Result := True;
end;

function Object_IsFrozen(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
begin
  Result := TestIntegrity(Arg(Args, Argc, 1), True, Res);
end;

function Object_IsSealed(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
begin
  Result := TestIntegrity(Arg(Args, Argc, 1), False, Res);
end;

function Object_PreventExtensions3(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var V, R: TValue; Had: Boolean;
begin
  V := Arg(Args, Argc, 1);
  Res := V;
  while IsProxy(V) do
  begin
    if not ProxyInvoke(V, 'preventExtensions', VUndefined, 0, R, Had) then Exit(False);
    if Had then Exit(True);
    V := ObjOf(V)^.Aux1;
  end;
  if IsObject(V) then ObjOf(V)^.OFlags := ObjOf(V)^.OFlags or 8;
  Result := True;
end;

function Object_IsExtensible(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var Ext: Boolean;
begin
  if not IsObject(Arg(Args, Argc, 1)) then begin Res := VFalse; Exit(True); end;
  if not IsExtensibleV(Args[1], Ext) then Exit(False);
  Res := MkBool(Ext);
  Result := True;
end;

{ ---------------- Proxy ---------------- }

threadvar
  RevokeKey: PStr;

function Proxy_Create(Target, Handler: TValue; var Res: TValue): Boolean;
var O: PObj;
begin
  if not IsObject(Target) or not IsObject(Handler) then
    Exit(ThrowTypeError('Cannot create proxy with a non-object as target or handler'));
  PushRoot(Target); PushRoot(Handler);
  O := NewObject(NewDictShape(VNull), 0, okProxy);
  O^.Aux1 := RootAt(TempTop - 2);
  O^.Aux2 := RootAt(TempTop - 1);
  if IsCallable(O^.Aux1) then O^.OFlags := 16;
  PopRoots(2);
  Res := MkPtr(O);
  Result := True;
end;

function Proxy_Ctor(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
begin
  if CurNewTarget = VUndefined then Exit(ThrowTypeError('Constructor Proxy requires ''new'''));
  Result := Proxy_Create(Arg(Args, Argc, 1), Arg(Args, Argc, 2), Res);
end;

{ 28.2.2.1.1 a revocation function: its revocable proxy under an internal key, cleared once revoked }
function Revoke_N(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var F: PObj; S: Integer; P: TValue;
begin
  F := ObjOf(NativeSelf);
  S := ShapeLookup(F^.Shape, RevokeKey);
  if S >= 0 then
  begin
    P := GetSlot(F, S);
    { the handler marks revocation; the target stays, for an operation that was already past
      ValidateNonRevokedProxy when a trap lookup revoked the proxy (it holds the target it read) }
    if IsProxy(P) then ObjOf(P)^.Aux2 := VNull;
    SetSlot(F, S, VNull);
  end;
  Res := VUndefined;
  Result := True;
end;

function Proxy_Revocable(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var P, Rv, O: TValue;
begin
  if not Proxy_Create(Arg(Args, Argc, 1), Arg(Args, Argc, 2), P) then Exit(False);
  PushRoot(P);
  Rv := NewNativeFunction(Revoke_N, '', 0);
  PushRoot(Rv);
  DefineOwn(RootAt(TempTop - 1), RevokeKey, RootAt(TempTop - 2), 0);
  O := MkPtr(NewPlainObject(ObjectProto, 2));
  PushRoot(O);
  DefineOwn(RootAt(TempTop - 1), AtomOf('proxy'), RootAt(TempTop - 3), pfDefault);
  DefineOwn(RootAt(TempTop - 1), AtomOf('revoke'), RootAt(TempTop - 2), pfDefault);
  Res := RootAt(TempTop - 1);
  PopRoots(3);
  Result := True;
end;

{ ---------------- Number predicates: no coercion, unlike the globals ---------------- }

function Number_IsNaN(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var V: TValue;
begin
  V := Arg(Args, Argc, 1);
  Res := MkBool(IsDouble(V) and IsNaN(DoubleOf(V)));
  Result := True;
end;

function Number_IsFinite(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var V: TValue;
begin
  V := Arg(Args, Argc, 1);
  Res := MkBool(IsInt(V) or (IsDouble(V) and not IsNaN(DoubleOf(V)) and not IsInfinite(DoubleOf(V))));
  Result := True;
end;

function Number_IsSafeInteger(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var V: TValue; D: Double;
begin
  V := Arg(Args, Argc, 1);
  if IsInt(V) then begin Res := VTrue; Exit(True); end;
  Res := VFalse;
  if IsDouble(V) then
  begin
    D := DoubleOf(V);
    if not IsNaN(D) and not IsInfinite(D) and (Frac(D) = 0) and (Abs(D) <= 9007199254740991.0) then Res := VTrue;
  end;
  Result := True;
end;

{ ---------------- Object.prototype.toString (ECMA-262 Object.prototype.toString) ---------------- }

function ObjProto_ToString3(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var V, T: TValue; Tag: UnicodeString;
begin
  V := Args[0];
  if V = VUndefined then begin Res := StrFromUnicode('[object Undefined]'); Exit(True); end;
  if V = VNull then begin Res := StrFromUnicode('[object Null]'); Exit(True); end;
  if IsNumber(V) then Tag := 'Number'
  else if IsString(V) then Tag := 'String'
  else if (V = VTrue) or (V = VFalse) then Tag := 'Boolean'
  else if IsObject(V) then
    case ObjOf(V)^.OKind of
      okArray: Tag := 'Array';
      okArguments, okArgumentsOrd: Tag := 'Arguments';
      okFunction, okNative, okBound: Tag := 'Function';
      okError: Tag := 'Error';
      okBoolean: Tag := 'Boolean';
      okNumber: Tag := 'Number';
      okStringObj: Tag := 'String';
      okDate: Tag := 'Date';
      17: Tag := 'RegExp';
    else Tag := 'Object';
    end
  else Tag := 'Object';
  { @@toStringTag, looked up on the value (primitives through their prototype) }
  if not GetElemV(V, SymToStringTag, T) then Exit(False);
  if IsString(T) then Tag := StrToUnicode(StrOf(T));
  Res := StrFromUnicode('[object ' + Tag + ']');
  Result := True;
end;

{ ---------------- Reflect ---------------- }

{ 7.3.20 CreateListFromArrayLike, into a new array (Res) }
function CreateListFromArrayLike(Obj: TValue; var Res: TValue): Boolean;
var LenV, V: TValue; D: Double; N, I: Int64; A: PObj;
begin
  if not IsObject(Obj) then Exit(ThrowTypeError('CreateListFromArrayLike called on non-object'));
  PushRoot(Obj);
  if not GetV(Obj, AtomLength, LenV) or not ToNumberV(LenV, D) then begin PopRoots(1); Exit(False); end;
  if IsNaN(D) or (D <= 0) then N := 0 else if D > 1E7 then begin PopRoots(1); Exit(ThrowError('RangeError', 'Too many arguments')); end
  else N := Trunc(D);
  A := NewArray(0, Max(4, N));
  PushRoot(MkPtr(A));
  for I := 0 to N - 1 do
  begin
    if not GetElemV(RootAt(TempTop - 2), MkInt(I), V) then begin PopRoots(2); Exit(False); end;
    ArrayPush(ObjOf(RootAt(TempTop - 1)), V);
  end;
  Res := RootAt(TempTop - 1);
  PopRoots(2);
  Result := True;
end;

{ 28.1.1 Reflect.apply }
function Reflect_Apply(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var L: TValue;
begin
  if not IsCallable(Arg(Args, Argc, 1)) then Exit(ThrowTypeError('Reflect.apply target is not a function'));
  if not CreateListFromArrayLike(Arg(Args, Argc, 3), L) then Exit(False);
  PushRoot(L);
  Result := CallValue(Args[1], Arg(Args, Argc, 2), @ObjOf(L)^.Elements^.Data[0], ArrayLen(ObjOf(L)), Res);
  PopRoots(1);
end;

{ 28.1.2 Reflect.construct }
function Reflect_Construct(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var L, NT: TValue;
begin
  if not IsConstructorV(Arg(Args, Argc, 1)) then Exit(ThrowTypeError('Reflect.construct target is not a constructor'));
  if Argc >= 3 then NT := Args[3] else NT := Args[1];
  if not IsConstructorV(NT) then Exit(ThrowTypeError('Reflect.construct newTarget is not a constructor'));
  PushRoot(NT);
  if not CreateListFromArrayLike(Arg(Args, Argc, 2), L) then begin PopRoots(1); Exit(False); end;
  PushRoot(L);
  Result := Construct(Args[1], @ObjOf(L)^.Elements^.Data[0], ArrayLen(ObjOf(L)), RootAt(TempTop - 2), Res);
  PopRoots(2);
end;

function Reflect_OwnKeys(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var Syms: TValue; I, N: Integer;
begin
  if not IsObject(Arg(Args, Argc, 1)) then Exit(ThrowTypeError('Reflect.ownKeys called on non-object'));
  Res := OwnKeys(ObjOf(Args[1]), False);
  if ObjOf(Args[1])^.OKind = okProxy then Exit(True);
  PushRoot(Res);
  Syms := OwnSymbols(Args[1], False);
  PushRoot(Syms);
  N := ArrayLen(ObjOf(Syms));
  for I := 0 to N - 1 do ArrayPush(ObjOf(RootAt(TempTop - 2)), ObjOf(RootAt(TempTop - 1))^.Elements^.Data[I]);
  Res := RootAt(TempTop - 2);
  PopRoots(2);
  Result := True;
end;

function Reflect_Has(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var K: PStr; Found: Boolean;
begin
  if not IsObject(Arg(Args, Argc, 1)) then Exit(ThrowTypeError('Reflect.has called on non-object'));
  if not ToPropKey(Arg(Args, Argc, 2), K) then Exit(False);
  if not HasPropertyV(Args[1], K, Found) then Exit(False);
  Res := MkBool(Found);
  Result := True;
end;

function Reflect_GetPrototypeOf(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var V: TValue; Had: Boolean;
begin
  V := Arg(Args, Argc, 1);
  while IsProxy(V) do
  begin
    if not ProxyInvoke(V, 'getPrototypeOf', VUndefined, 0, Res, Had) then Exit(False);
    if Had then Exit(True);
    V := ObjOf(V)^.Aux1;
  end;
  if IsObject(V) then Res := ProtoOf(ObjOf(V))
  else if IsString(V) then Res := StringProto
  else if IsNumber(V) then Res := NumberProto
  else if (V = VTrue) or (V = VFalse) then Res := BooleanProto
  else Res := VNull;
  Result := True;
end;

{ ---------------- setup ---------------- }

procedure InitBuiltins3;
var C, G: TValue; M: TValue;
  procedure N(O: TValue; const Name: UnicodeString; Fn: TNativeFn; Len: Integer);
  begin
    DefineNative(O, Name, Fn, Len);
  end;
  function Lookup(Obj: TValue; const Name: UnicodeString): TValue;
  begin
    if not IsObject(Obj) or not GetV(Obj, AtomOf(Name), Result) then Result := VUndefined;
  end;
  procedure SetTag(Proto: TValue; const Tag: UnicodeString);
  var Key: PStr;
  begin
    if not IsObject(Proto) then Exit;
    Scratch2 := Proto;
    PushRoot(StrFromUnicode(Tag));
    Key := PStr(PPointer(PByte(PtrOf(SymToStringTag)) + 16)^);
    DefineOwn(Scratch2, Key, RootAt(TempTop - 1), pfConfigurable);
    PopRoots(1);
  end;
begin
  AddRoot(@DateProto); AddRoot(@WeakMapProto); AddRoot(@WeakSetProto); AddRoot(@Scratch); AddRoot(@Scratch2);
  AddRoot(@SymbolRegistry);
  RealmVals([@DateProto, @WeakMapProto, @WeakSetProto]);
  { 20.4.2.2: the GlobalSymbolRegistry is shared by all realms }
  if not RealmInit then SymbolRegistry := MkPtr(NewPlainObject(VNull, 8));

  { Date replaces the Date.now-only object }
  DateProto := MkPtr(NewPlainObject(ObjectProto, 16));
  C := NewNativeFunction(Date_Ctor, 'Date', 7);
  Scratch := C;
  DefineOwn(Scratch, AtomPrototype, DateProto, 0);
  DefineOwn(DateProto, AtomConstructor, Scratch, pfHidden);
  DefineOwn(GlobalObj, AtomOf('Date'), Scratch, pfHidden);
  N(Scratch, 'now', Date_Now, 0);
  N(DateProto, 'getTime', Date_GetTime, 0);
  N(DateProto, 'valueOf', Date_GetTime, 0);
  N(DateProto, 'getFullYear', Date_GetFullYear, 0);
  N(DateProto, 'getUTCFullYear', Date_GetFullYear, 0);
  N(DateProto, 'getMonth', Date_GetMonth, 0);
  N(DateProto, 'getUTCMonth', Date_GetMonth, 0);
  N(DateProto, 'getDate', Date_GetDate, 0);
  N(DateProto, 'getUTCDate', Date_GetDate, 0);
  N(DateProto, 'getHours', Date_GetHours, 0);
  N(DateProto, 'getUTCHours', Date_GetHours, 0);
  N(DateProto, 'getMinutes', Date_GetMinutes, 0);
  N(DateProto, 'getUTCMinutes', Date_GetMinutes, 0);
  N(DateProto, 'getSeconds', Date_GetSeconds, 0);
  N(DateProto, 'getUTCSeconds', Date_GetSeconds, 0);
  N(DateProto, 'getMilliseconds', Date_GetMilliseconds, 0);
  N(DateProto, 'getDay', Date_GetDay, 0);
  N(DateProto, 'toISOString', Date_ToISOString, 0);
  N(DateProto, 'toJSON', Date_ToISOString, 0);
  N(DateProto, 'toString', Date_ToISOString, 0);
  N(DateProto, 'getTimezoneOffset', Date_GetTimezoneOffset, 0);

  { Symbol.for / keyFor }
  G := Lookup(GlobalObj, 'Symbol');
  Scratch := G;
  N(Scratch, 'for', Symbol_For, 1);
  N(Scratch, 'keyFor', Symbol_KeyFor, 1);

  { WeakMap / WeakSet on the Map and Set methods }
  M := Lookup(GlobalObj, 'Map');
  Scratch := Lookup(M, 'prototype');
  WeakMapProto := MkPtr(NewPlainObject(Scratch, 4));
  C := NewNativeFunction(WeakMap_Ctor, 'WeakMap', 0);
  Scratch := C;
  DefineOwn(Scratch, AtomPrototype, WeakMapProto, 0);
  DefineOwn(WeakMapProto, AtomConstructor, Scratch, pfHidden);
  DefineOwn(GlobalObj, AtomOf('WeakMap'), Scratch, pfHidden);
  M := Lookup(GlobalObj, 'Set');
  Scratch := Lookup(M, 'prototype');
  WeakSetProto := MkPtr(NewPlainObject(Scratch, 4));
  C := NewNativeFunction(WeakSet_Ctor, 'WeakSet', 0);
  Scratch := C;
  DefineOwn(Scratch, AtomPrototype, WeakSetProto, 0);
  DefineOwn(WeakSetProto, AtomConstructor, Scratch, pfHidden);
  DefineOwn(GlobalObj, AtomOf('WeakSet'), Scratch, pfHidden);

  { Object }
  Scratch := Lookup(GlobalObj, 'Object');
  N(Scratch, 'getOwnPropertyDescriptor', Object_GetOwnPropertyDescriptor, 2);
  N(Scratch, 'getOwnPropertyDescriptors', Object_GetOwnPropertyDescriptors, 1);
  N(Scratch, 'defineProperties', Object_DefineProperties, 2);
  N(Scratch, 'getOwnPropertySymbols', Object_GetOwnPropertySymbols, 1);
  N(Scratch, 'assign', Object_Assign3, 2);
  N(Scratch, 'freeze', Object_Freeze3, 1);
  N(Scratch, 'seal', Object_Seal3, 1);
  N(Scratch, 'is', Object_Is, 2);
  N(Scratch, 'isFrozen', Object_IsFrozen, 1);
  N(Scratch, 'getPrototypeOf', Reflect_GetPrototypeOf, 1);
  N(Scratch, 'isSealed', Object_IsSealed, 1);
  N(Scratch, 'preventExtensions', Object_PreventExtensions3, 1);
  N(Scratch, 'isExtensible', Object_IsExtensible, 1);

  N(ObjectProto, 'propertyIsEnumerable', ObjProto_PropertyIsEnumerable, 1);

  { Symbol.toStringTag and the prototypes that carry one }
  AddRoot(@SymToStringTag);
  SymToStringTag := SymToStringTagW;
  Scratch := Lookup(GlobalObj, 'Symbol');
  DefineOwn(Scratch, AtomOf('toStringTag'), SymToStringTag, 0);
  N(ObjectProto, 'toString', ObjProto_ToString3, 0);
  SetTag(Lookup(Lookup(GlobalObj, 'Map'), 'prototype'), 'Map');
  SetTag(Lookup(Lookup(GlobalObj, 'Set'), 'prototype'), 'Set');
  SetTag(WeakMapProto, 'WeakMap');
  SetTag(WeakSetProto, 'WeakSet');
  SetTag(Lookup(Lookup(GlobalObj, 'Symbol'), 'prototype'), 'Symbol');
  SetTag(Lookup(Lookup(GlobalObj, 'Promise'), 'prototype'), 'Promise');
  SetTag(Lookup(Lookup(GlobalObj, 'ArrayBuffer'), 'prototype'), 'ArrayBuffer');
  SetTag(Lookup(Lookup(GlobalObj, 'DataView'), 'prototype'), 'DataView');
  SetTag(Lookup(Lookup(GlobalObj, 'BigInt'), 'prototype'), 'BigInt');
  SetTag(Lookup(GlobalObj, 'Math'), 'Math');
  SetTag(Lookup(GlobalObj, 'JSON'), 'JSON');
  SetTag(GeneratorProto, 'Generator');

  Scratch := Lookup(GlobalObj, 'Number');
  N(Scratch, 'isNaN', Number_IsNaN, 1);
  N(Scratch, 'isFinite', Number_IsFinite, 1);
  N(Scratch, 'isSafeInteger', Number_IsSafeInteger, 1);

  { Proxy }
  if not RealmInit then RevokeKey := NewKeyAtom('%revocableProxy', kfInternal);   { internal keys: one per agent }
  Scratch := NewNativeFunction(Proxy_Ctor, 'Proxy', 2);
  ObjOf(Scratch)^.OFlags := ObjOf(Scratch)^.OFlags or 32;   { a constructor without a prototype property }
  DefineOwn(GlobalObj, AtomOf('Proxy'), Scratch, pfHidden);
  N(Scratch, 'revocable', Proxy_Revocable, 2);

  { Reflect }
  Scratch := MkPtr(NewPlainObject(ObjectProto, 8));
  DefineOwn(GlobalObj, AtomOf('Reflect'), Scratch, pfHidden);
  N(Scratch, 'apply', Reflect_Apply, 3);
  N(Scratch, 'construct', Reflect_Construct, 2);
  N(Scratch, 'ownKeys', Reflect_OwnKeys, 1);
  N(Scratch, 'has', Reflect_Has, 2);
  N(Scratch, 'getPrototypeOf', Reflect_GetPrototypeOf, 1);
end;

end.
