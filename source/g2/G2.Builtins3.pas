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
  DateUtils,
  Math,
  SysUtils,

  G2.Builtins,
  G2.Core,
  G2.VM,
  Linux,
  UnixType;

const
  okDate = 18;

type
  TArgs = PValue;

threadvar
  DateProto, WeakMapProto, WeakSetProto, Scratch, Scratch2, SymToStringTag: TValue;
  SymbolRegistry: TValue;   { plain object: atom -> symbol, so the GC sees the symbols }

function Arg(AArgs: TArgs; AArgc, I: Integer): TValue; inline;
begin
  if I <= AArgc then Result := AArgs[I] else Result := VUndefined;
end;

{ ---------------- Date (UTC; enough for timing and formatting) ---------------- }

function NowMs: Double;
var TS: TTimeSpec;
begin
  clock_gettime(CLOCK_REALTIME, @TS);
  Result := TS.tv_sec * 1000.0 + TS.tv_nsec div 1000000;
end;

function ThisTime(AArgs: TArgs; var T: Double): Boolean;
begin
  if not IsObject(AArgs[0]) or (ObjOf(AArgs[0])^.OKind <> okDate) then Exit(ThrowTypeError('this is not a Date object.'));
  T := NumOf(ObjOf(AArgs[0])^.Aux1);
  Result := True;
end;

function Date_Ctor(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var T, D: Double; O: PObj; S: TValue; Y, Mo, Dd, H, Mi, Se, Ms: Double; DT: TDateTime;
begin
  if CurNewTarget = VUndefined then
  begin
    ARes := StrFromUnicode(UnicodeString(FormatDateTime('ddd mmm dd yyyy hh:nn:ss', Now)) + ' GMT+0000');
    Exit(True);
  end;
  if AArgc = 0 then T := NowMs
  else if AArgc = 1 then
  begin
    if IsObject(AArgs[1]) and (ObjOf(AArgs[1])^.OKind = okDate) then T := NumOf(ObjOf(AArgs[1])^.Aux1)
    else if IsString(AArgs[1]) then
    begin
      if TryISO8601ToDate(StrToUnicode(StrOf(AArgs[1])), DT, True) then T := DateTimeToUnix(DT, False) * 1000.0
      else T := NaN;
    end
    else
    begin
      if not ToNumberV(AArgs[1], D) then Exit(False);
      T := D;
    end;
  end
  else
  begin
    if not ToNumberV(AArgs[1], Y) then Exit(False);
    if not ToNumberV(AArgs[2], Mo) then Exit(False);
    Dd := 1; H := 0; Mi := 0; Se := 0; Ms := 0;
    if AArgc >= 3 then if not ToNumberV(AArgs[3], Dd) then Exit(False);
    if AArgc >= 4 then if not ToNumberV(AArgs[4], H) then Exit(False);
    if AArgc >= 5 then if not ToNumberV(AArgs[5], Mi) then Exit(False);
    if AArgc >= 6 then if not ToNumberV(AArgs[6], Se) then Exit(False);
    if AArgc >= 7 then if not ToNumberV(AArgs[7], Ms) then Exit(False);
    try
      DT := EncodeDateTime(Trunc(Y), Trunc(Mo) + 1, Trunc(Dd), Trunc(H), Trunc(Mi), Trunc(Se), Trunc(Ms));
      T := DateTimeToUnix(DT, False) * 1000.0 + Trunc(Ms);
    except
      T := NaN;
    end;
  end;
  O := NewObject(RootShapeFor(DateProto), 0, okDate);
  O^.Aux1 := MkNumber(T);
  ARes := MkPtr(O);
  Result := True;
end;

function Date_GetTime(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var T: Double;
begin
  if not ThisTime(AArgs, T) then Exit(False);
  ARes := MkNumber(T);
  Result := True;
end;

function DatePart(AArgs: TArgs; var ARes: TValue; APart: Integer): Boolean;
var T: Double; DT: TDateTime;
begin
  if not ThisTime(AArgs, T) then Exit(False);
  if IsNaN(T) then begin ARes := MkDouble(NaN); Exit(True); end;
  DT := UnixToDateTime(Trunc(T / 1000.0), True) + (T - Trunc(T / 1000.0) * 1000.0) / 86400000.0;
  case APart of
    0: ARes := MkInt(YearOf(DT));
    1: ARes := MkInt(MonthOf(DT) - 1);
    2: ARes := MkInt(DayOf(DT));
    3: ARes := MkInt(HourOf(DT));
    4: ARes := MkInt(MinuteOf(DT));
    5: ARes := MkInt(SecondOf(DT));
    6: ARes := MkInt(Trunc(T) mod 1000);
    7: ARes := MkInt(DayOfWeek(DT) - 1);
    8: ARes := StrFromUnicode(UnicodeString(FormatDateTime('yyyy-mm-dd"T"hh:nn:ss.zzz"Z"', DT)));
  else ARes := MkInt(0);
  end;
  Result := True;
end;
function Date_GetFullYear(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean; begin Result := DatePart(AArgs, ARes, 0); end;
function Date_GetMonth(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean; begin Result := DatePart(AArgs, ARes, 1); end;
function Date_GetDate(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean; begin Result := DatePart(AArgs, ARes, 2); end;
function Date_GetHours(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean; begin Result := DatePart(AArgs, ARes, 3); end;
function Date_GetMinutes(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean; begin Result := DatePart(AArgs, ARes, 4); end;
function Date_GetSeconds(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean; begin Result := DatePart(AArgs, ARes, 5); end;
function Date_GetMilliseconds(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean; begin Result := DatePart(AArgs, ARes, 6); end;
function Date_GetDay(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean; begin Result := DatePart(AArgs, ARes, 7); end;
function Date_ToISOString(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean; begin Result := DatePart(AArgs, ARes, 8); end;
function Date_GetTimezoneOffset(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean; begin Result := DatePart(AArgs, ARes, 9); end;

function Date_Now(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
begin
  ARes := MkNumber(NowMs);
  Result := True;
end;

{ ---------------- Symbol registry ---------------- }

function Symbol_For(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var S: TValue; K: PStr; I: Integer;
begin
  if not ToStringV(Arg(AArgs, AArgc, 1), S) then Exit(False);
  K := Atomize(StrOf(S));
  I := GetOwnSlot(ObjOf(SymbolRegistry), K);
  if I >= 0 then begin ARes := GetSlot(ObjOf(SymbolRegistry), I); Exit(True); end;
  S := NewSymbolValue(StrToUnicode(K));
  PushRoot(S);
  AddProperty(ObjOf(SymbolRegistry), K, S, pfDefault);
  ARes := RootAt(TempTop - 1);
  PopRoots(1);
  Result := True;
end;

function Symbol_KeyFor(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var O: PObj; I: Integer;
begin
  if not (IsPtr(Arg(AArgs, AArgc, 1)) and (PCell(PtrOf(AArgs[1]))^.Kind = ckSymbol)) then
    Exit(ThrowTypeError('Symbol.keyFor: argument is not a symbol'));
  ARes := VUndefined;
  O := ObjOf(SymbolRegistry);
  for I := 0 to O^.Shape^.Count - 1 do
    if GetSlot(O, I) = Arg(AArgs, AArgc, 1) then begin ARes := MkPtr(O^.Shape^.Keys[I]); Break; end;
  Result := True;
end;

{ ---------------- WeakMap / WeakSet (held strongly: the spike has no ephemerons) ---------------- }

function WeakCtor(AArgs: TArgs; AArgc: Integer; var ARes: TValue; AIsSet: Boolean): Boolean;
var MapCtor: TValue; Proto: TValue;
begin
  if CurNewTarget = VUndefined then Exit(ThrowTypeError('Constructor requires ''new'''));
  if AIsSet then
  begin
    if not GetV(GlobalObj, AtomOf('Set'), MapCtor) then Exit(False);
    Proto := WeakSetProto;
  end
  else
  begin
    if not GetV(GlobalObj, AtomOf('Map'), MapCtor) then Exit(False);
    Proto := WeakMapProto;
  end;
  if not Construct(MapCtor, @AArgs[1], AArgc, MapCtor, ARes) then Exit(False);
  SetProto(ObjOf(ARes), Proto);
  Result := True;
end;
function WeakMap_Ctor(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean; begin Result := WeakCtor(AArgs, AArgc, ARes, False); end;
function WeakSet_Ctor(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean; begin Result := WeakCtor(AArgs, AArgc, ARes, True); end;

{ ---------------- property descriptors ---------------- }

function DescriptorOf(O: TValue; K: PStr; var ARes: TValue): Boolean;
var P: PObj; S: Integer; F: Byte; V: TValue; D: PObj; Acc: PAccessor; Idx: Int64;
begin
  ARes := VUndefined;
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
  ARes := RootAt(TempTop - 1);
  PopRoots(2);
  Result := True;
end;

function Object_GetOwnPropertyDescriptor(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var K: PStr;
begin
  if not ToPropKey(Arg(AArgs, AArgc, 2), K) then Exit(False);
  Result := DescriptorOf(Arg(AArgs, AArgc, 1), K, ARes);
end;

function ObjProto_PropertyIsEnumerable(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var K: PStr; O: PObj; Idx: Int64; S: Integer;
begin
  if not ToPropKey(Arg(AArgs, AArgc, 1), K) then Exit(False);
  ARes := VFalse;
  if IsString(AArgs[0]) then
  begin
    ARes := MkBool(IndexFromString(K, Idx) and (Idx < StrOf(AArgs[0])^.Len));
    Exit(True);
  end;
  if not IsObject(AArgs[0]) then Exit(True);
  O := ObjOf(AArgs[0]);
  if (O^.Elements <> nil) and IndexFromString(K, Idx) then
    ARes := MkBool((Idx < O^.Elements^.Len) and (O^.Elements^.Data[Idx] <> VHole))
  else
  begin
    S := ShapeLookup(O^.Shape, K);
    ARes := MkBool((S >= 0) and ((O^.Shape^.PFlags[S] and pfEnumerable) <> 0));
  end;
  Result := True;
end;

function Object_GetOwnPropertyDescriptors(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var Keys, D: TValue; I, N: Integer; K: PStr; R: PObj;
begin
  R := NewPlainObject(ObjectProto, 8);
  PushRoot(MkPtr(R));
  if IsObject(Arg(AArgs, AArgc, 1)) then
  begin
    Keys := OwnKeys(ObjOf(AArgs[1]), False);
    PushRoot(Keys);
    N := ArrayLen(ObjOf(Keys));
    for I := 0 to N - 1 do
    begin
      K := Atomize(StrOf(ObjOf(RootAt(TempTop - 1))^.Elements^.Data[I]));
      if not DescriptorOf(AArgs[1], K, D) then begin PopRoots(2); Exit(False); end;
      DefineOwn(RootAt(TempTop - 2), K, D, pfDefault);
    end;
    PopRoots(1);
  end;
  ARes := RootAt(TempTop - 1);
  PopRoots(1);
  Result := True;
end;

function Object_DefineProperties(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var Keys, D, DP: TValue; I, N: Integer; K: PStr; CallArgs: array[0..2] of TValue;
begin
  if not IsObject(Arg(AArgs, AArgc, 1)) then Exit(ThrowTypeError('Object.defineProperties called on non-object'));
  if not IsObject(Arg(AArgs, AArgc, 2)) then begin ARes := AArgs[1]; Exit(True); end;
  if not GetV(GlobalObj, AtomOf('Object'), DP) then Exit(False);
  if not GetV(DP, AtomOf('defineProperty'), DP) then Exit(False);
  Scratch2 := DP;
  Keys := OwnKeys(ObjOf(AArgs[2]), True);
  PushRoot(Keys);
  N := ArrayLen(ObjOf(Keys));
  for I := 0 to N - 1 do
  begin
    K := Atomize(StrOf(ObjOf(RootAt(TempTop - 1))^.Elements^.Data[I]));
    if not GetV(AArgs[2], K, D) then begin PopRoots(1); Exit(False); end;
    CallArgs[0] := AArgs[1]; CallArgs[1] := MkPtr(K); CallArgs[2] := D;
    if not CallValue(Scratch2, VUndefined, @CallArgs[0], 3, DP) then begin PopRoots(1); Exit(False); end;
  end;
  PopRoots(1);
  ARes := AArgs[1];
  Result := True;
end;

function IsSymbolKey(K: PStr): Boolean; inline;
begin
  Result := (K^.Hdr.Extra and kfSymbol) <> 0;
end;

{ the own symbol-keyed properties, in insertion order, as Symbol values }
function OwnSymbols(O: TValue; AOnlyEnumerable: Boolean): TValue;
var A: PObj; I: Integer; S: PShape; V: TValue;
begin
  PushRoot(O);
  A := NewArray(0, 4);
  PushRoot(MkPtr(A));
  S := ObjOf(RootAt(TempTop - 2))^.Shape;
  for I := 0 to S^.Count - 1 do
    if IsSymbolKey(S^.Keys[I]) and (not AOnlyEnumerable or ((S^.PFlags[I] and pfEnumerable) <> 0)) then
    begin
      V := KeyToValue(S^.Keys[I]);
      if not IsString(V) then ArrayPush(ObjOf(RootAt(TempTop - 1)), V);
      S := ObjOf(RootAt(TempTop - 2))^.Shape;
    end;
  Result := RootAt(TempTop - 1);
  PopRoots(2);
end;

function Object_GetOwnPropertySymbols(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
begin
  if not IsObject(Arg(AArgs, AArgc, 1)) then begin ARes := MkPtr(NewArray(0, 0)); Exit(True); end;
  ARes := OwnSymbols(AArgs[1], False);
  Result := True;
end;

function Object_Assign3(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var I, J, N, Pass: Integer; Keys, V: TValue; K: PStr;
begin
  if not IsObject(Arg(AArgs, AArgc, 1)) then Exit(ThrowTypeError('Cannot convert undefined or null to object'));
  for I := 2 to AArgc do
  begin
    if not IsObject(AArgs[I]) then Continue;
    for Pass := 0 to 1 do
    begin
      if Pass = 0 then Keys := OwnKeys(ObjOf(AArgs[I]), True) else Keys := OwnSymbols(AArgs[I], True);
      PushRoot(Keys);
      N := ArrayLen(ObjOf(Keys));
      for J := 0 to N - 1 do
      begin
        if not ToPropKey(ObjOf(RootAt(TempTop - 1))^.Elements^.Data[J], K) then begin PopRoots(1); Exit(False); end;
        if not GetV(AArgs[I], K, V) then begin PopRoots(1); Exit(False); end;
        if not SetV(AArgs[1], K, V) then begin PopRoots(1); Exit(False); end;
      end;
      PopRoots(1);
    end;
  end;
  ARes := AArgs[1];
  Result := True;
end;

{ SetIntegrityLevel: sealed drops configurable, frozen also writable (dense elements stay as they are) }
procedure IntegrityLevel(O: TValue; AFrozen: Boolean);
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
    if AFrozen and ((F and pfAccessor) = 0) then F := F and not pfWritable;
    if F <> S^.PFlags[I] then ChangeFlags(ObjOf(RootAt(TempTop - 1)), S^.Keys[I], F);
    Inc(I);
  end;
  ObjOf(RootAt(TempTop - 1))^.OFlags := ObjOf(RootAt(TempTop - 1))^.OFlags or 8;
  PopRoots(1);
end;

function Object_Freeze3(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
begin
  IntegrityLevel(Arg(AArgs, AArgc, 1), True);
  ARes := Arg(AArgs, AArgc, 1);
  Result := True;
end;

function Object_Seal3(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
begin
  IntegrityLevel(Arg(AArgs, AArgc, 1), False);
  ARes := Arg(AArgs, AArgc, 1);
  Result := True;
end;

function Object_Is(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var A, B: TValue; X, Y: Double;
begin
  A := Arg(AArgs, AArgc, 1); B := Arg(AArgs, AArgc, 2);
  if IsNumber(A) and IsNumber(B) then
  begin
    X := NumOf(A); Y := NumOf(B);
    if IsNaN(X) or IsNaN(Y) then ARes := MkBool(IsNaN(X) and IsNaN(Y))
    else ARes := MkBool((X = Y) and ((X <> 0) or ((PQWord(@X)^ shr 63) = (PQWord(@Y)^ shr 63))));
  end
  else ARes := MkBool(StrictEquals(A, B));
  Result := True;
end;

function IsProxy(V: TValue): Boolean; inline;
begin
  Result := IsObject(V) and (ObjOf(V)^.OKind = okProxy);
end;

{ [[IsExtensible]], through the trap for a proxy }
function IsExtensibleV(V: TValue; var AExt: Boolean): Boolean;
var R: TValue; Had: Boolean;
begin
  while IsProxy(V) do
  begin
    if not ProxyInvoke(V, 'isExtensible', VUndefined, 0, R, Had) then Exit(False);
    if Had then begin AExt := ToBool(R); Exit(True); end;
    V := ObjOf(V)^.Aux1;
  end;
  AExt := IsObject(V) and ((ObjOf(V)^.OFlags and 8) = 0);
  Result := True;
end;

{ TestIntegrityLevel: not extensible, and every own property non-configurable (and, frozen, non-writable) }
function TestIntegrity(V: TValue; AFrozen: Boolean; var ARes: TValue): Boolean;
var Ext: Boolean; O: PObj; I: Integer; F: Byte; Keys: TValue;
begin
  if not IsObject(V) then begin ARes := VTrue; Exit(True); end;
  if not IsExtensibleV(V, Ext) then Exit(False);
  if Ext then begin ARes := VFalse; Exit(True); end;
  if IsProxy(V) then
  begin
    { the keys come through the trap; their descriptors are not consulted here }
    if not ProxyOwnKeys(V, Keys) then Exit(False);
    ARes := VTrue;
    Exit(True);
  end;
  O := ObjOf(V);
  ARes := VFalse;
  if O^.Elements <> nil then
    for I := 0 to O^.Elements^.Len - 1 do
      if O^.Elements^.Data[I] <> VHole then Exit(True);
  for I := 0 to O^.Shape^.Count - 1 do
  begin
    F := O^.Shape^.PFlags[I];
    if (F and pfConfigurable) <> 0 then Exit(True);
    if AFrozen and ((F and pfAccessor) = 0) and ((F and pfWritable) <> 0) then Exit(True);
  end;
  ARes := VTrue;
  Result := True;
end;

function Object_IsFrozen(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
begin
  Result := TestIntegrity(Arg(AArgs, AArgc, 1), True, ARes);
end;

function Object_IsSealed(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
begin
  Result := TestIntegrity(Arg(AArgs, AArgc, 1), False, ARes);
end;

function Object_PreventExtensions3(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var V, R: TValue; Had: Boolean;
begin
  V := Arg(AArgs, AArgc, 1);
  ARes := V;
  while IsProxy(V) do
  begin
    if not ProxyInvoke(V, 'preventExtensions', VUndefined, 0, R, Had) then Exit(False);
    if Had then Exit(True);
    V := ObjOf(V)^.Aux1;
  end;
  if IsObject(V) then ObjOf(V)^.OFlags := ObjOf(V)^.OFlags or 8;
  Result := True;
end;

function Object_IsExtensible(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var Ext: Boolean;
begin
  if not IsObject(Arg(AArgs, AArgc, 1)) then begin ARes := VFalse; Exit(True); end;
  if not IsExtensibleV(AArgs[1], Ext) then Exit(False);
  ARes := MkBool(Ext);
  Result := True;
end;

{ ---------------- Proxy ---------------- }

threadvar
  RevokeKey: PStr;

function Proxy_Create(ATarget, AHandler: TValue; var ARes: TValue): Boolean;
var O: PObj;
begin
  if not IsObject(ATarget) or not IsObject(AHandler) then
    Exit(ThrowTypeError('Cannot create proxy with a non-object as target or handler'));
  PushRoot(ATarget); PushRoot(AHandler);
  O := NewObject(NewDictShape(VNull), 0, okProxy);
  O^.Aux1 := RootAt(TempTop - 2);
  O^.Aux2 := RootAt(TempTop - 1);
  if IsCallable(O^.Aux1) then O^.OFlags := 16;
  PopRoots(2);
  ARes := MkPtr(O);
  Result := True;
end;

function Proxy_Ctor(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
begin
  if CurNewTarget = VUndefined then Exit(ThrowTypeError('Constructor Proxy requires ''new'''));
  Result := Proxy_Create(Arg(AArgs, AArgc, 1), Arg(AArgs, AArgc, 2), ARes);
end;

{ 28.2.2.1.1 a revocation function: its revocable proxy under an internal key, cleared once revoked }
function Revoke_N(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var F: PObj; S: Integer; P: TValue;
begin
  F := ObjOf(NativeSelf);
  S := ShapeLookup(F^.Shape, RevokeKey);
  if S >= 0 then
  begin
    P := GetSlot(F, S);
    if IsProxy(P) then
    begin
      ObjOf(P)^.Aux1 := VNull;
      ObjOf(P)^.Aux2 := VNull;
    end;
    SetSlot(F, S, VNull);
  end;
  ARes := VUndefined;
  Result := True;
end;

function Proxy_Revocable(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var P, Rv, O: TValue;
begin
  if not Proxy_Create(Arg(AArgs, AArgc, 1), Arg(AArgs, AArgc, 2), P) then Exit(False);
  PushRoot(P);
  Rv := NewNativeFunction(Revoke_N, '', 0);
  PushRoot(Rv);
  DefineOwn(RootAt(TempTop - 1), RevokeKey, RootAt(TempTop - 2), 0);
  O := MkPtr(NewPlainObject(ObjectProto, 2));
  PushRoot(O);
  DefineOwn(RootAt(TempTop - 1), AtomOf('proxy'), RootAt(TempTop - 3), pfDefault);
  DefineOwn(RootAt(TempTop - 1), AtomOf('revoke'), RootAt(TempTop - 2), pfDefault);
  ARes := RootAt(TempTop - 1);
  PopRoots(3);
  Result := True;
end;

{ ---------------- Number predicates: no coercion, unlike the globals ---------------- }

function Number_IsNaN(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var V: TValue;
begin
  V := Arg(AArgs, AArgc, 1);
  ARes := MkBool(IsDouble(V) and IsNaN(DoubleOf(V)));
  Result := True;
end;

function Number_IsFinite(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var V: TValue;
begin
  V := Arg(AArgs, AArgc, 1);
  ARes := MkBool(IsInt(V) or (IsDouble(V) and not IsNaN(DoubleOf(V)) and not IsInfinite(DoubleOf(V))));
  Result := True;
end;

function Number_IsSafeInteger(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var V: TValue; D: Double;
begin
  V := Arg(AArgs, AArgc, 1);
  if IsInt(V) then begin ARes := VTrue; Exit(True); end;
  ARes := VFalse;
  if IsDouble(V) then
  begin
    D := DoubleOf(V);
    if not IsNaN(D) and not IsInfinite(D) and (Frac(D) = 0) and (Abs(D) <= 9007199254740991.0) then ARes := VTrue;
  end;
  Result := True;
end;

{ ---------------- Object.prototype.toString (ECMA-262 Object.prototype.toString) ---------------- }

function ObjProto_ToString3(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var V, T: TValue; Tag: UnicodeString;
begin
  V := AArgs[0];
  if V = VUndefined then begin ARes := StrFromUnicode('[object Undefined]'); Exit(True); end;
  if V = VNull then begin ARes := StrFromUnicode('[object Null]'); Exit(True); end;
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
  ARes := StrFromUnicode('[object ' + Tag + ']');
  Result := True;
end;

{ ---------------- Reflect ---------------- }

{ 7.3.20 CreateListFromArrayLike, into a new array (Res) }
function CreateListFromArrayLike(AObj: TValue; var ARes: TValue): Boolean;
var LenV, V: TValue; D: Double; N, I: Int64; A: PObj;
begin
  if not IsObject(AObj) then Exit(ThrowTypeError('CreateListFromArrayLike called on non-object'));
  PushRoot(AObj);
  if not GetV(AObj, AtomLength, LenV) or not ToNumberV(LenV, D) then begin PopRoots(1); Exit(False); end;
  if IsNaN(D) or (D <= 0) then N := 0 else if D > 1E7 then begin PopRoots(1); Exit(ThrowError('RangeError', 'Too many arguments')); end
  else N := Trunc(D);
  A := NewArray(0, Max(4, N));
  PushRoot(MkPtr(A));
  for I := 0 to N - 1 do
  begin
    if not GetElemV(RootAt(TempTop - 2), MkInt(I), V) then begin PopRoots(2); Exit(False); end;
    ArrayPush(ObjOf(RootAt(TempTop - 1)), V);
  end;
  ARes := RootAt(TempTop - 1);
  PopRoots(2);
  Result := True;
end;

{ 28.1.1 Reflect.apply }
function Reflect_Apply(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var L: TValue;
begin
  if not IsCallable(Arg(AArgs, AArgc, 1)) then Exit(ThrowTypeError('Reflect.apply target is not a function'));
  if not CreateListFromArrayLike(Arg(AArgs, AArgc, 3), L) then Exit(False);
  PushRoot(L);
  Result := CallValue(AArgs[1], Arg(AArgs, AArgc, 2), @ObjOf(L)^.Elements^.Data[0], ArrayLen(ObjOf(L)), ARes);
  PopRoots(1);
end;

{ 28.1.2 Reflect.construct }
function Reflect_Construct(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var L, NT: TValue;
begin
  if not IsConstructorV(Arg(AArgs, AArgc, 1)) then Exit(ThrowTypeError('Reflect.construct target is not a constructor'));
  if AArgc >= 3 then NT := AArgs[3] else NT := AArgs[1];
  if not IsConstructorV(NT) then Exit(ThrowTypeError('Reflect.construct newTarget is not a constructor'));
  PushRoot(NT);
  if not CreateListFromArrayLike(Arg(AArgs, AArgc, 2), L) then begin PopRoots(1); Exit(False); end;
  PushRoot(L);
  Result := Construct(AArgs[1], @ObjOf(L)^.Elements^.Data[0], ArrayLen(ObjOf(L)), RootAt(TempTop - 2), ARes);
  PopRoots(2);
end;

function Reflect_OwnKeys(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var Syms: TValue; I, N: Integer;
begin
  if not IsObject(Arg(AArgs, AArgc, 1)) then Exit(ThrowTypeError('Reflect.ownKeys called on non-object'));
  ARes := OwnKeys(ObjOf(AArgs[1]), False);
  if ObjOf(AArgs[1])^.OKind = okProxy then Exit(True);
  PushRoot(ARes);
  Syms := OwnSymbols(AArgs[1], False);
  PushRoot(Syms);
  N := ArrayLen(ObjOf(Syms));
  for I := 0 to N - 1 do ArrayPush(ObjOf(RootAt(TempTop - 2)), ObjOf(RootAt(TempTop - 1))^.Elements^.Data[I]);
  ARes := RootAt(TempTop - 2);
  PopRoots(2);
  Result := True;
end;

function Reflect_Has(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var K: PStr; Found: Boolean;
begin
  if not IsObject(Arg(AArgs, AArgc, 1)) then Exit(ThrowTypeError('Reflect.has called on non-object'));
  if not ToPropKey(Arg(AArgs, AArgc, 2), K) then Exit(False);
  if not HasPropertyV(AArgs[1], K, Found) then Exit(False);
  ARes := MkBool(Found);
  Result := True;
end;

function Reflect_GetPrototypeOf(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var V: TValue; Had: Boolean;
begin
  V := Arg(AArgs, AArgc, 1);
  while IsProxy(V) do
  begin
    if not ProxyInvoke(V, 'getPrototypeOf', VUndefined, 0, ARes, Had) then Exit(False);
    if Had then Exit(True);
    V := ObjOf(V)^.Aux1;
  end;
  if IsObject(V) then ARes := ProtoOf(ObjOf(V))
  else if IsString(V) then ARes := StringProto
  else if IsNumber(V) then ARes := NumberProto
  else if (V = VTrue) or (V = VFalse) then ARes := BooleanProto
  else ARes := VNull;
  Result := True;
end;

{ ---------------- setup ---------------- }

procedure InitBuiltins3;
var C, G: TValue; M: TValue;
  procedure N(O: TValue; const AName: UnicodeString; AFn: TNativeFn; ALen: Integer);
  begin
    DefineNative(O, AName, AFn, ALen);
  end;
  function Lookup(AObj: TValue; const AName: UnicodeString): TValue;
  begin
    if not IsObject(AObj) or not GetV(AObj, AtomOf(AName), Result) then Result := VUndefined;
  end;
  procedure SetTag(AProto: TValue; const ATag: UnicodeString);
  var Key: PStr;
  begin
    if not IsObject(AProto) then Exit;
    Scratch2 := AProto;
    PushRoot(StrFromUnicode(ATag));
    Key := PStr(PPointer(PByte(PtrOf(SymToStringTag)) + 16)^);
    DefineOwn(Scratch2, Key, RootAt(TempTop - 1), pfConfigurable);
    PopRoots(1);
  end;
begin
  AddRoot(@DateProto); AddRoot(@WeakMapProto); AddRoot(@WeakSetProto); AddRoot(@Scratch); AddRoot(@Scratch2);
  AddRoot(@SymbolRegistry);
  SymbolRegistry := MkPtr(NewPlainObject(VNull, 8));

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
  RevokeKey := NewKeyAtom('%revocableProxy', kfInternal);
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
