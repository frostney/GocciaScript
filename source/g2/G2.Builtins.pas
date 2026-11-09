unit G2.Builtins;
{ The built-in objects the benchmark corpora use. }
{$mode delphiunicode}{$H+}
{$MINFPCONSTPREC 64}
{$POINTERMATH ON}
{$Q-}{$R-}

interface

uses
  DateUtils,
  Math,
  SysUtils,

  G2.Bytecode,
  G2.Core,
  G2.VM;

procedure InitBuiltins;
function NewSymbolValue(const ADesc: UnicodeString): TValue;

threadvar
  { set by an embedder (G2.Engine) to take this thread's console output; nil writes to stdout }
  ConsoleSink: procedure(const Line: UnicodeString);

implementation

uses
  BigInteger,
  G2.BigIntV,
  G2.ObjModel,
  Linux,
  NumericText,
  UnixType;

type
  TArgs = PValue;

function Arg(AArgs: TArgs; AArgc, I: Integer): TValue; inline;
begin
  if I <= AArgc then Result := AArgs[I] else Result := VUndefined;
end;

function ThisObj(AArgs: TArgs): PObj; inline;
begin
  Result := ObjOf(AArgs[0]);
end;

function NewObjWithProto(AProto: TValue): TValue;
begin
  Result := MkPtr(NewPlainObject(AProto, 4));
end;

function StrArg(AArgs: TArgs; AArgc, I: Integer; var S: PStr): Boolean;
var V: TValue;
begin
  if not ToStringV(Arg(AArgs, AArgc, I), V) then Exit(False);
  S := StrOf(V);
  Result := True;
end;

function NumArg(AArgs: TArgs; AArgc, I: Integer; var D: Double): Boolean; inline;
var V: TValue;
begin
  V := Arg(AArgs, AArgc, I);
  if IsInt(V) then begin D := IntOf(V); Exit(True); end;
  if IsDouble(V) then begin D := DoubleOf(V); Exit(True); end;
  Result := ToNumberV(V, D);
end;

function IntArg(AArgs: TArgs; AArgc, I: Integer; ADefault: Int64; var R: Int64): Boolean;
var D: Double;
begin
  if (I > AArgc) or (AArgs[I] = VUndefined) then begin R := ADefault; Exit(True); end;
  if not NumArg(AArgs, AArgc, I, D) then Exit(False);
  if IsNaN(D) then R := 0
  else if D > 9e15 then R := High(Int32)
  else if D < -9e15 then R := Low(Int32)
  else R := Trunc(D);
  Result := True;
end;

function IsArrayValue(V: TValue): Boolean; inline;
begin
  Result := IsObject(V) and (ObjOf(V)^.OKind = okArray);
end;

{ ---------------- global functions ---------------- }

function G_ParseInt(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var S: PStr; T: UnicodeString; Radix: Int64; I, Sign, D: Integer; Acc: Double; Any: Boolean;
begin
  if not StrArg(AArgs, AArgc, 1, S) then Exit(False);
  if not IntArg(AArgs, AArgc, 2, 0, Radix) then Exit(False);
  T := Trim(StrToUnicode(S));
  Sign := 1; I := 1;
  if (I <= Length(T)) and ((T[I] = '-') or (T[I] = '+')) then begin if T[I] = '-' then Sign := -1; Inc(I); end;
  if Radix = 0 then
  begin
    Radix := 10;
    if (I + 1 <= Length(T)) and (T[I] = '0') and ((T[I + 1] = 'x') or (T[I + 1] = 'X')) then begin Radix := 16; Inc(I, 2); end;
  end
  else if (Radix = 16) and (I + 1 <= Length(T)) and (T[I] = '0') and ((T[I + 1] = 'x') or (T[I + 1] = 'X')) then Inc(I, 2);
  Acc := 0; Any := False;
  while I <= Length(T) do
  begin
    case T[I] of
      '0'..'9': D := Ord(T[I]) - 48;
      'a'..'z': D := Ord(T[I]) - 87;
      'A'..'Z': D := Ord(T[I]) - 55;
    else Break;
    end;
    if D >= Radix then Break;
    Acc := Acc * Radix + D;
    Any := True;
    Inc(I);
  end;
  if not Any then ARes := MkDouble(NaN) else ARes := MkNumber(Sign * Acc);
  Result := True;
end;

function G_ParseFloat(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var S: PStr; T: UnicodeString; I: Integer;
begin
  if not StrArg(AArgs, AArgc, 1, S) then Exit(False);
  T := Trim(StrToUnicode(S));
  I := 1;
  if (I <= Length(T)) and ((T[I] = '-') or (T[I] = '+')) then Inc(I);
  if Copy(T, I, 8) = 'Infinity' then begin T := Copy(T, 1, I + 7); end
  else
  begin
    while (I <= Length(T)) and (CharInSet(T[I], ['0'..'9'])) do Inc(I);
    if (I <= Length(T)) and (T[I] = '.') then begin Inc(I); while (I <= Length(T)) and CharInSet(T[I], ['0'..'9']) do Inc(I); end;
    if (I < Length(T)) and CharInSet(T[I], ['e', 'E']) and (CharInSet(T[I + 1], ['0'..'9', '+', '-'])) then
    begin
      Inc(I, 2);
      while (I <= Length(T)) and CharInSet(T[I], ['0'..'9']) do Inc(I);
    end;
    T := Copy(T, 1, I - 1);
  end;
  if (T = '') or (T = '-') or (T = '+') then ARes := MkDouble(NaN) else ARes := MkNumber(StringToNumber(T));
  Result := True;
end;

function G_IsNaN(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var D: Double;
begin
  if not NumArg(AArgs, AArgc, 1, D) then Exit(False);
  ARes := MkBool(IsNaN(D));
  Result := True;
end;

function G_IsFinite(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var D: Double;
begin
  if not NumArg(AArgs, AArgc, 1, D) then Exit(False);
  ARes := MkBool(not IsNaN(D) and not IsInfinite(D));
  Result := True;
end;

{ ---------------- console / performance ---------------- }

function Console_Log(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var I: Integer; S: TValue; Line: UnicodeString;
begin
  Line := '';
  for I := 1 to AArgc do
  begin
    if not ToStringV(AArgs[I], S) then Exit(False);
    if I > 1 then Line := Line + ' ';
    Line := Line + StrToUnicode(StrOf(S));
  end;
  if Assigned(ConsoleSink) then ConsoleSink(Line) else WriteLn(UTF8Encode(Line));
  ARes := VUndefined;
  Result := True;
end;

function Perf_Now(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var TS: TTimeSpec;
begin
  clock_gettime(CLOCK_MONOTONIC, @TS);
  ARes := MkDouble(TS.tv_sec * 1000.0 + TS.tv_nsec / 1e6);
  Result := True;
end;

function Date_Now(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
begin
  ARes := MkNumber(Trunc(DateTimeToUnix(Now) * 1000.0));
  Result := True;
end;

{ ---------------- Object ---------------- }

{ ToObject for a primitive: the wrapper keeps the primitive in Aux1 }
function WrapPrimitive(V: TValue): TValue;
var O: PObj; Proto: TValue; Kind: Byte;
begin
  Kind := okPlain;
  if IsString(V) then begin Proto := StringProto; Kind := okStringObj; end
  else if IsNumber(V) then begin Proto := NumberProto; Kind := okNumber; end
  else if (V = VTrue) or (V = VFalse) then begin Proto := BooleanProto; Kind := okBoolean; end
  else if IsPtr(V) and (PCell(PtrOf(V))^.Kind = ckSymbol) then begin Proto := SymbolProto; Kind := okSymbolObj; end
  else Proto := BigIntProto;
  PushRoot(V);
  O := NewObject(RootShapeFor(Proto), 0, Kind);
  O^.Aux1 := RootAt(TempTop - 1);
  PopRoots(1);
  Result := MkPtr(O);
end;

function Object_Ctor(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var V: TValue;
begin
  V := Arg(AArgs, AArgc, 1);
  if IsObject(V) then ARes := V
  else if IsNullish(V) then ARes := NewObjWithProto(ObjectProto)
  else ARes := WrapPrimitive(V);
  Result := True;
end;

function Object_Create(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var P, F, R: TValue; CallArgs: array[0..1] of TValue;
begin
  P := Arg(AArgs, AArgc, 1);
  if not (IsObject(P) or (P = VNull)) then Exit(ThrowTypeError('Object prototype may only be an Object or null'));
  ARes := NewObjWithProto(P);
  if Arg(AArgs, AArgc, 2) <> VUndefined then
  begin
    { the properties argument: Object.defineProperties(O, Properties) }
    PushRoot(ARes);
    if not GetV(GlobalObj, AtomOf('Object'), F) then begin PopRoots(1); Exit(False); end;
    if not GetV(F, AtomOf('defineProperties'), F) then begin PopRoots(1); Exit(False); end;
    CallArgs[0] := RootAt(TempTop - 1); CallArgs[1] := AArgs[2];
    if not CallValue(F, VUndefined, @CallArgs[0], 2, R) then begin PopRoots(1); Exit(False); end;
    ARes := RootAt(TempTop - 1);
    PopRoots(1);
  end;
  Result := True;
end;

function Object_Keys(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var V: TValue;
begin
  V := Arg(AArgs, AArgc, 1);
  if not IsObject(V) then begin ARes := MkPtr(NewArray(0, 0)); Exit(True); end;
  ARes := OwnKeys(ObjOf(V), True);
  Result := True;
end;

function Object_Values(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var V, Keys, X: TValue; I, N: Integer; A: PObj;
begin
  V := Arg(AArgs, AArgc, 1);
  if not IsObject(V) then begin ARes := MkPtr(NewArray(0, 0)); Exit(True); end;
  Keys := OwnKeys(ObjOf(V), True);
  PushRoot(Keys);
  N := ArrayLen(ObjOf(Keys));
  A := NewArray(0, N);
  PushRoot(MkPtr(A));
  for I := 0 to N - 1 do
  begin
    if not GetV(AArgs[1], Atomize(StrOf(ObjOf(RootAt(TempTop - 2))^.Elements^.Data[I])), X) then begin PopRoots(2); Exit(False); end;
    ArrayPush(ObjOf(RootAt(TempTop - 1)), X);
  end;
  ARes := RootAt(TempTop - 1);
  PopRoots(2);
  Result := True;
end;

function Object_Entries(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var V, Keys, X: TValue; I, N: Integer; A, P: PObj;
begin
  V := Arg(AArgs, AArgc, 1);
  if not IsObject(V) then begin ARes := MkPtr(NewArray(0, 0)); Exit(True); end;
  Keys := OwnKeys(ObjOf(V), True);
  PushRoot(Keys);
  N := ArrayLen(ObjOf(Keys));
  A := NewArray(0, N);
  PushRoot(MkPtr(A));
  for I := 0 to N - 1 do
  begin
    if not GetV(AArgs[1], Atomize(StrOf(ObjOf(RootAt(TempTop - 2))^.Elements^.Data[I])), X) then begin PopRoots(2); Exit(False); end;
    PushRoot(X);
    P := NewArray(2, 2);
    P^.Elements^.Data[0] := ObjOf(RootAt(TempTop - 3))^.Elements^.Data[I];
    P^.Elements^.Data[1] := RootAt(TempTop - 1);
    PopRoots(1);
    ArrayPush(ObjOf(RootAt(TempTop - 1)), MkPtr(P));
  end;
  ARes := RootAt(TempTop - 1);
  PopRoots(2);
  Result := True;
end;

function Object_Assign(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var I, J, N: Integer; Keys, V: TValue; K: PStr;
begin
  for I := 2 to AArgc do
  begin
    if not IsObject(AArgs[I]) then Continue;
    Keys := OwnKeys(ObjOf(AArgs[I]), True);
    PushRoot(Keys);
    N := ArrayLen(ObjOf(Keys));
    for J := 0 to N - 1 do
    begin
      K := Atomize(StrOf(ObjOf(RootAt(TempTop - 1))^.Elements^.Data[J]));
      if not GetV(AArgs[I], K, V) then begin PopRoots(1); Exit(False); end;
      if not SetV(AArgs[1], K, V) then begin PopRoots(1); Exit(False); end;
    end;
    PopRoots(1);
  end;
  ARes := AArgs[1];
  Result := True;
end;

function Object_Freeze(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
begin
  ARes := Arg(AArgs, AArgc, 1);
  if IsObject(ARes) then ObjOf(ARes)^.OFlags := ObjOf(ARes)^.OFlags or 8;
  Result := True;
end;

function Object_GetPrototypeOf(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var V: TValue;
begin
  V := Arg(AArgs, AArgc, 1);
  if IsObject(V) then ARes := ObjOf(V)^.Shape^.Proto
  else if IsString(V) then ARes := StringProto
  else if IsNumber(V) then ARes := NumberProto
  else ARes := VNull;
  Result := True;
end;

function Object_SetPrototypeOf(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
begin
  ARes := Arg(AArgs, AArgc, 1);
  if IsObject(ARes) then SetProto(ObjOf(ARes), Arg(AArgs, AArgc, 2));
  Result := True;
end;

function DescHas(D: TValue; AKey: PStr): Boolean;
var P: TValue;
begin
  P := D;
  while IsObject(P) do
  begin
    if ShapeLookup(ObjOf(P)^.Shape, AKey) >= 0 then Exit(True);
    P := ObjOf(P)^.Shape^.Proto;
  end;
  Result := False;
end;

{ ValidateAndApplyPropertyDescriptor without the non-configurable checks: absent fields keep the
  existing property's attributes and value }
function Object_DefineProperty(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var D, V, G, S, Tmp, OldV: TValue; K: PStr; Flags, OldFlags: Byte; Slot: Integer; Exists: Boolean;
  KEnum, KConf, KWrit, KValue, KGet, KSet: PStr;
  procedure Pick(AHas: Boolean; ABit: Byte; AVal: TValue);
  begin
    if AHas then
    begin
      if ToBool(AVal) then Flags := Flags or ABit;
    end
    else if Exists then Flags := Flags or (OldFlags and ABit);
  end;
begin
  if not IsObject(Arg(Args, Argc, 1)) then Exit(ThrowTypeError('Object.defineProperty called on non-object'));
  if not IsObject(Arg(Args, Argc, 3)) then Exit(ThrowTypeError('Property description must be an object'));
  if not ToPropKey(Arg(Args, Argc, 2), K) then Exit(False);
  KEnum := AtomOf('enumerable'); KConf := AtomOf('configurable'); KWrit := AtomOf('writable');
  KValue := AtomValue; KGet := AtomOf('get'); KSet := AtomOf('set');
  Slot := ShapeLookup(ObjOf(Args[1])^.Shape, K);
  Exists := Slot >= 0;
  OldFlags := 0; OldV := VUndefined;
  if Exists then
  begin
    OldFlags := ObjOf(Args[1])^.Shape^.PFlags[Slot];
    OldV := GetSlot(ObjOf(Args[1]), Slot);
  end;
  PushRoot(OldV);
  Flags := 0;
  D := Args[3];
  if not GetV(D, KEnum, Tmp) then begin PopRoots(1); Exit(False); end;
  Pick(DescHas(Args[3], KEnum), pfEnumerable, Tmp);
  if not GetV(Args[3], KConf, Tmp) then begin PopRoots(1); Exit(False); end;
  Pick(DescHas(Args[3], KConf), pfConfigurable, Tmp);
  if DescHas(Args[3], KGet) or DescHas(Args[3], KSet) then
  begin
    if not GetV(Args[3], KGet, G) then begin PopRoots(1); Exit(False); end;
    PushRoot(G);
    if not GetV(Args[3], KSet, S) then begin PopRoots(2); Exit(False); end;
    G := RootAt(TempTop - 1);
    OldV := RootAt(TempTop - 2);
    PopRoots(2);
    if Exists and ((OldFlags and pfAccessor) <> 0) then
    begin
      { a partial accessor descriptor keeps the other half }
      if not DescHas(Args[3], KGet) then G := PAccessor(PtrOf(OldV))^.Getter;
      if not DescHas(Args[3], KSet) then S := PAccessor(PtrOf(OldV))^.Setter;
    end;
    PushRoot(G); PushRoot(S);
    Tmp := MkPtr(NewAccessor(RootAt(TempTop - 2), RootAt(TempTop - 1)));
    PopRoots(2);
    DefineOwn(Args[1], K, Tmp, Flags or pfAccessor);
  end
  else if Exists and ((OldFlags and pfAccessor) <> 0) and not DescHas(Args[3], KValue) and not DescHas(Args[3], KWrit) then
  begin
    { a generic descriptor on an accessor changes only enumerable/configurable }
    DefineOwn(Args[1], K, RootAt(TempTop - 1), Flags or pfAccessor);
    PopRoots(1);
  end
  else
  begin
    if not GetV(Args[3], KWrit, Tmp) then begin PopRoots(1); Exit(False); end;
    if Exists and ((OldFlags and pfAccessor) <> 0) then Exists := False;   { accessor to data: fresh attributes }
    Pick(DescHas(Args[3], KWrit), pfWritable, Tmp);
    if DescHas(Args[3], KValue) then
    begin
      if not GetV(Args[3], KValue, V) then begin PopRoots(1); Exit(False); end;
    end
    else if Exists then V := RootAt(TempTop - 1)
    else V := VUndefined;
    PopRoots(1);
    if IsArrayValue(Args[1]) and ((K = AtomLength) or (ObjOf(Args[1])^.Elements <> nil)) and (Slot < 0) then
    begin
      if DescHas(Args[3], KValue) then
        if not SetV(Args[1], K, V) then Exit(False);
    end
    else
      DefineOwn(Args[1], K, V, Flags);
  end;
  Res := Args[1];
  Result := True;
end;

function Object_GetOwnPropertyNames(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var V: TValue;
begin
  V := Arg(AArgs, AArgc, 1);
  if not IsObject(V) then begin ARes := MkPtr(NewArray(0, 0)); Exit(True); end;
  ARes := OwnKeys(ObjOf(V), False);
  Result := True;
end;

function ObjProto_HasOwnProperty(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var K: PStr; O: PObj; Idx: Int64; V: TValue; F: Byte;
begin
  if not ToPropKey(Arg(AArgs, AArgc, 1), K) then Exit(False);
  if not IsObject(AArgs[0]) then begin ARes := VFalse; Exit(True); end;
  O := ObjOf(AArgs[0]);
  if (O^.Elements <> nil) and IndexFromString(K, Idx) then
    ARes := MkBool((Idx < O^.Elements^.Len) and (O^.Elements^.Data[Idx] <> VHole))
  else
    ARes := MkBool(ShapeLookup(O^.Shape, K) >= 0);
  if ARes = VFalse then
    if OwnVirtual(ObjOf(AArgs[0]), K, V, F) then ARes := VTrue;
  Result := True;
end;

function ObjProto_ToString(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var V: TValue;
begin
  V := AArgs[0];
  if V = VUndefined then ARes := StrFromUnicode('[object Undefined]')
  else if V = VNull then ARes := StrFromUnicode('[object Null]')
  else if IsArrayValue(V) then ARes := StrFromUnicode('[object Array]')
  else if IsCallable(V) then ARes := StrFromUnicode('[object Function]')
  else ARes := StrFromUnicode('[object Object]');
  Result := True;
end;

function ObjProto_ValueOf(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
begin
  ARes := AArgs[0];
  Result := True;
end;

function ObjProto_IsPrototypeOf(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var P: TValue;
begin
  ARes := VFalse;
  if not IsObject(Arg(AArgs, AArgc, 1)) then Exit(True);
  P := ObjOf(AArgs[1])^.Shape^.Proto;
  while IsObject(P) do
  begin
    if P = AArgs[0] then begin ARes := VTrue; Break; end;
    P := ObjOf(P)^.Shape^.Proto;
  end;
  Result := True;
end;

{ ---------------- Function ---------------- }

function FuncProto_Call(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
begin
  if AArgc >= 2 then Result := CallValue(AArgs[0], AArgs[1], @AArgs[2], AArgc - 1, ARes)
  else Result := CallValue(AArgs[0], Arg(AArgs, AArgc, 1), nil, 0, ARes);
end;

function MaterializeArrayLike(AArgs: TArgs): Boolean;
var LenV, V: TValue; D: Double; N, I: Integer; A: PObj;
begin
  if IsNullish(AArgs[0]) then Exit(ThrowTypeError('Array.prototype method called on null or undefined'));
  if IsString(AArgs[0]) then N := StrOf(AArgs[0])^.Len
  else
  begin
    if not GetV(AArgs[0], AtomLength, LenV) then Exit(False);
    if not ToNumberV(LenV, D) then Exit(False);
    if IsNaN(D) or (D < 0) then N := 0 else N := Trunc(Min(D, 100000000));
  end;
  A := NewArray(N, N);
  PushRoot(MkPtr(A));
  for I := 0 to N - 1 do
  begin
    if not GetElemV(AArgs[0], MkInt(I), V) then begin PopRoots(1); Exit(False); end;
    ValueArrayStore(ObjOf(RootAt(TempTop - 1))^.Elements, I, V);
  end;
  AArgs[0] := RootAt(TempTop - 1);
  PopRoots(1);
  Result := True;
end;

function FuncProto_Apply(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var L: TValue; A: PObj; N, I: Integer; Buf: array of TValue;
begin
  L := Arg(AArgs, AArgc, 2);
  if IsNullish(L) then Exit(CallValue(AArgs[0], Arg(AArgs, AArgc, 1), nil, 0, ARes));
  if not IsObject(L) then Exit(ThrowTypeError('CreateListFromArrayLike called on non-object'));
  if (ObjOf(L)^.Elements = nil) and (ObjOf(L)^.OKind <> okArray) then
  begin
    if not MaterializeArrayLike(@AArgs[2]) then Exit(False);
    L := AArgs[2];
  end;
  A := ObjOf(L);
  N := ArrayLen(A);
  SetLength(Buf, N + 1);
  for I := 0 to N - 1 do
  begin
    Buf[I] := A^.Elements^.Data[I];
    if Buf[I] = VHole then Buf[I] := VUndefined;
  end;
  { Buf is not a GC root: copy to the stack through CallValue, which copies before allocating }
  Result := CallValue(AArgs[0], Arg(AArgs, AArgc, 1), @Buf[0], N, ARes);
end;

function FuncProto_Empty(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
begin
  ARes := VUndefined;
  Result := True;
end;

{ [[GetOwnProperty]] existence, proxies through their trap (Object.prototype.hasOwnProperty) }
function HasOwnPropertyV(O: TValue; AKey: PStr; var AHas: Boolean): Boolean;
var A: array[0..1] of TValue; R, F: TValue;
begin
  if ObjOf(O)^.OKind = okProxy then
  begin
    A[0] := O; A[1] := MkPtr(AKey);
    if not GetV(ObjectProto, AtomOf('hasOwnProperty'), F) then Exit(False);
    if not CallValue(F, O, @A[1], 1, R) then Exit(False);
    AHas := ToBool(R);
    Exit(True);
  end;
  AHas := (ShapeLookup(ObjOf(O)^.Shape, AKey) >= 0);
  if not AHas then AHas := OwnVirtual(ObjOf(O), AKey, R, PByte(@A[0])^);
  Result := True;
end;

{ [[GetPrototypeOf]], proxies through their trap }
function GetPrototypeOfV(O: TValue; var P: TValue): Boolean;
var Buf: array[0..1] of TValue;
begin
  Buf[0] := VUndefined;
  Buf[1] := O;
  Result := Object_GetPrototypeOf(@Buf[0], 1, P);
end;

{ 20.2.3.2 Function.prototype.bind }
function FuncProto_Bind(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var B: PObj; VA: PValueArray; I, N: Integer; Proto, LenV, NameV: TValue; L, D: Double; Has: Boolean;
begin
  if not IsCallable(AArgs[0]) then Exit(ThrowTypeError('Bind must be called on a function'));
  { BoundFunctionCreate: the target's [[GetPrototypeOf]] }
  if ObjOf(AArgs[0])^.OKind = okProxy then
  begin
    if not GetPrototypeOfV(AArgs[0], Proto) then Exit(False);
  end
  else Proto := ObjOf(AArgs[0])^.Shape^.Proto;
  N := Max(AArgc, 1);
  PushRoot(Proto);
  VA := NewValueArray(N);
  VA^.Len := N;
  VA^.Data[0] := Arg(AArgs, AArgc, 1);
  for I := 2 to AArgc do VA^.Data[I - 1] := AArgs[I];
  PushRoot(MkPtr(VA));
  B := NewObject(FnShapeFor(RootAt(TempTop - 2)), 3, okBound);
  B^.Aux1 := AArgs[0];
  B^.Aux2 := RootAt(TempTop - 1);
  B^.Slots[0] := MkInt(0);
  B^.Slots[1] := MkPtr(AtomOf(''));
  PopRoots(2);
  PushRoot(MkPtr(B));
  { length: the target's own length less the bound arguments }
  L := 0;
  if not HasOwnPropertyV(AArgs[0], AtomLength, Has) then begin PopRoots(1); Exit(False); end;
  if Has then
  begin
    if not GetV(AArgs[0], AtomLength, LenV) then begin PopRoots(1); Exit(False); end;
    if IsNumber(LenV) then
    begin
      D := NumOf(LenV);
      if IsInfinite(D) then begin if D > 0 then L := D else L := 0; end
      else if not IsNaN(D) then L := Max(Trunc(D) - Max(AArgc - 1, 0), 0);
    end;
  end;
  ObjOf(RootAt(TempTop - 1))^.Slots[0] := MkNumber(L);
  if not GetV(AArgs[0], AtomName, NameV) then begin PopRoots(1); Exit(False); end;
  if IsString(NameV) then NameV := StrFromUnicode('bound ' + StrToUnicode(StrOf(NameV)))
  else NameV := MkPtr(AtomOf('bound '));
  ObjOf(RootAt(TempTop - 1))^.Slots[1] := NameV;
  WriteBarrier(@ObjOf(RootAt(TempTop - 1))^.Hdr, NameV);
  ARes := RootAt(TempTop - 1);
  PopRoots(1);
  Result := True;
end;

function FuncProto_ToString(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var O: PObj; T: PFuncTemplate; Nm: UnicodeString;
begin
  if not IsCallable(AArgs[0]) then Exit(ThrowTypeError('Function.prototype.toString requires that ''this'' be a Function'));
  O := ObjOf(AArgs[0]);
  Nm := '';
  if O^.OKind = okFunction then
  begin
    T := PFuncTemplate(O^.Native);
    if not T^.SourceRange.IsEmpty then
    begin
      ARes := StrFromUnicode(UTF8Decode(T^.SourceRange.Get));
      Exit(True);
    end;
    if T^.Name <> nil then Nm := StrToUnicode(T^.Name);
  end
  else if (O^.OKind = okNative) and IsString(O^.Aux1) then Nm := StrToUnicode(StrOf(O^.Aux1));
  ARes := StrFromUnicode('function ' + Nm + '() { [native code] }');
  Result := True;
end;

{ ---------------- Array ---------------- }

function Array_Ctor(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var A: PObj; D: Double; I: Integer;
begin
  if (AArgc = 1) and IsNumber(AArgs[1]) then
  begin
    D := NumOf(AArgs[1]);
    if (D < 0) or (D > 4294967295.0) or (Frac(D) <> 0) then Exit(ThrowError('RangeError', 'Invalid array length'));
    A := NewArray(Trunc(D), Trunc(D));
  end
  else
  begin
    A := NewArray(AArgc, AArgc);
    for I := 1 to AArgc do A^.Elements^.Data[I - 1] := AArgs[I];
  end;
  ARes := MkPtr(A);
  Result := True;
end;

function Array_IsArray(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
begin
  ARes := MkBool(IsArrayValue(Arg(AArgs, AArgc, 1)));
  Result := True;
end;

function Array_Of(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var A: PObj; I: Integer;
begin
  A := NewArray(AArgc, AArgc);
  for I := 1 to AArgc do A^.Elements^.Data[I - 1] := AArgs[I];
  ARes := MkPtr(A);
  Result := True;
end;

function Array_From(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var A: PObj; Src, It, V, F, R: TValue; Done: Boolean; N, I: Integer; LenV: TValue; D: Double;
begin
  A := NewArray(0, 8);
  PushRoot(MkPtr(A));
  Src := Arg(AArgs, AArgc, 1);   { read after the allocation: a collection there moves young values }
  if IsObject(Src) and (ObjOf(Src)^.OKind <> okArray) and (ObjOf(Src)^.Elements = nil) and
     not IsCallable(ObjOf(Src)^.Shape^.Proto) then
  begin
    { array-like or iterable object }
    if not GetIterator(Src, It) then
    begin
      Pending := VUndefined;
      if not GetV(AArgs[1], AtomLength, LenV) then begin PopRoots(1); Exit(False); end;
      if not ToNumberV(LenV, D) then begin PopRoots(1); Exit(False); end;
      N := Trunc(D);
      for I := 0 to N - 1 do
      begin
        if not GetElemV(AArgs[1], MkInt(I), V) then begin PopRoots(1); Exit(False); end;
        ArrayPush(ObjOf(RootAt(TempTop - 1)), V);
      end;
      ARes := RootAt(TempTop - 1);
      PopRoots(1);
      Exit(True);
    end;
  end
  else if not GetIterator(Src, It) then begin PopRoots(1); Exit(False); end;
  PushRoot(It);
  I := 0;
  while True do
  begin
    if not IteratorStep(RootAt(TempTop - 1), V, Done) then begin PopRoots(2); Exit(False); end;
    if Done then Break;
    if IsCallable(AArgs[2]) and (AArgc >= 2) then
    begin
      PushRoot(V); PushRoot(MkInt(I));
      if not CallValue(AArgs[2], VUndefined, @TempRoots[TempTop - 2], 2, R) then begin PopRoots(4); Exit(False); end;
      PopRoots(2);
      V := R;
    end;
    ArrayPush(ObjOf(RootAt(TempTop - 2)), V);
    Inc(I);
  end;
  ARes := RootAt(TempTop - 2);
  PopRoots(2);
  Result := True;
end;

function ThisArray(AArgs: TArgs; var A: PObj): Boolean; inline;
begin
  if not IsObject(AArgs[0]) or ((ObjOf(AArgs[0])^.Elements = nil) and (ObjOf(AArgs[0])^.OKind <> okArray)
    and (ObjOf(AArgs[0])^.OKind <> okArguments)) then
    if not MaterializeArrayLike(AArgs) then Exit(False);
  A := ObjOf(AArgs[0]);
  Result := True;
end;

function ArrProto_Push(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var A: PObj; I: Integer; O, LenV: TValue; L: Double;
begin
  if not (IsObject(AArgs[0]) and (ObjOf(AArgs[0])^.OKind = okArray) and ((ObjOf(AArgs[0])^.OFlags and 64) = 0)) then
  begin
    { 23.1.3.23 on any other receiver: ToObject, LengthOfArrayLike, Set(O, k, v, true) and Set(O, "length", true) }
    if not ToObjectV(AArgs[0], O) then Exit(False);
    PushRoot(O);
    if not GetV(O, AtomLength, LenV) or not ToNumberV(LenV, L) then begin PopRoots(1); Exit(False); end;
    if IsNaN(L) or (L < 0) then L := 0 else L := Int(Min(L, 9007199254740991.0));
    if L + AArgc > 9007199254740991.0 then begin PopRoots(1); Exit(ThrowTypeError('Pushing would exceed the maximum array length')); end;
    for I := 1 to AArgc do
      if not SetElemVT(RootAt(TempTop - 1), MkNumber(L + I - 1), AArgs[I]) then begin PopRoots(1); Exit(False); end;
    ARes := MkNumber(L + AArgc);
    Result := SetVT(RootAt(TempTop - 1), AtomLength, ARes);
    PopRoots(1);
    Exit;
  end;
  if not ThisArray(AArgs, A) then Exit(False);
  for I := 1 to AArgc do ArrayPush(ObjOf(AArgs[0]), AArgs[I]);
  ARes := MkInt(ArrayLen(ObjOf(AArgs[0])));
  Result := True;
end;

function ArrProto_Pop(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var A: PObj; E: PValueArray;
begin
  if not ThisArray(AArgs, A) then Exit(False);
  E := A^.Elements;
  if E^.Len = 0 then begin ARes := VUndefined; Exit(True); end;
  Dec(E^.Len);
  ARes := E^.Data[E^.Len];
  if ARes = VHole then ARes := VUndefined;
  E^.Data[E^.Len] := VUndefined;
  Result := True;
end;

function ArrProto_Shift(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var A: PObj; E: PValueArray;
begin
  if not ThisArray(AArgs, A) then Exit(False);
  E := A^.Elements;
  if E^.Len = 0 then begin ARes := VUndefined; Exit(True); end;
  ARes := E^.Data[0];
  if ARes = VHole then ARes := VUndefined;
  Move(E^.Data[1], E^.Data[0], (E^.Len - 1) * SizeOf(TValue));
  Dec(E^.Len);
  E^.Data[E^.Len] := VUndefined;
  Result := True;
end;

function ArrProto_Unshift(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var A: PObj; E: PValueArray; I, N: Integer;
begin
  if not ThisArray(AArgs, A) then Exit(False);
  N := A^.Elements^.Len;
  EnsureElementsCap(A, N + AArgc);
  A := ObjOf(AArgs[0]);
  E := A^.Elements;
  Move(E^.Data[0], E^.Data[AArgc], N * SizeOf(TValue));
  for I := 1 to AArgc do
  begin
    E^.Data[I - 1] := AArgs[I];
    WriteBarrier(@E^.Hdr, AArgs[I]);
  end;
  E^.Len := N + AArgc;
  ARes := MkInt(E^.Len);
  Result := True;
end;

function RelIndex(V: Int64; ALen: Integer): Integer; inline;
begin
  if V < 0 then begin V := V + ALen; if V < 0 then V := 0; end
  else if V > ALen then V := ALen;
  Result := V;
end;

function ArrProto_Slice(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var A, R: PObj; Len, S, E_, I: Integer; X, Y: Int64;
begin
  if not ThisArray(AArgs, A) then Exit(False);
  Len := ArrayLen(A);
  if not IntArg(AArgs, AArgc, 1, 0, X) then Exit(False);
  if not IntArg(AArgs, AArgc, 2, Len, Y) then Exit(False);
  S := RelIndex(X, Len); E_ := RelIndex(Y, Len);
  if E_ < S then E_ := S;
  R := NewArray(E_ - S, E_ - S);
  A := ObjOf(AArgs[0]);
  for I := S to E_ - 1 do R^.Elements^.Data[I - S] := A^.Elements^.Data[I];
  ARes := MkPtr(R);
  Result := True;
end;

function ArrProto_Splice(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var A, R: PObj; Len, S, DelCount, I, Ins, NewLen: Integer; X, Y: Int64; E: PValueArray;
begin
  if not ThisArray(AArgs, A) then Exit(False);
  Len := ArrayLen(A);
  if not IntArg(AArgs, AArgc, 1, 0, X) then Exit(False);
  S := RelIndex(X, Len);
  if AArgc < 2 then DelCount := Len - S
  else
  begin
    if not IntArg(AArgs, AArgc, 2, 0, Y) then Exit(False);
    DelCount := Max(0, Min(Y, Len - S));
  end;
  Ins := Max(0, AArgc - 2);
  R := NewArray(DelCount, DelCount);
  A := ObjOf(AArgs[0]);
  for I := 0 to DelCount - 1 do R^.Elements^.Data[I] := A^.Elements^.Data[S + I];
  PushRoot(MkPtr(R));
  NewLen := Len - DelCount + Ins;
  EnsureElementsCap(A, NewLen);
  A := ObjOf(AArgs[0]);
  E := A^.Elements;
  Move(E^.Data[S + DelCount], E^.Data[S + Ins], (Len - S - DelCount) * SizeOf(TValue));
  for I := 0 to Ins - 1 do
  begin
    E^.Data[S + I] := AArgs[3 + I];
    WriteBarrier(@E^.Hdr, AArgs[3 + I]);
  end;
  for I := NewLen to Len - 1 do E^.Data[I] := VUndefined;
  E^.Len := NewLen;
  ARes := RootAt(TempTop - 1);
  PopRoots(1);
  Result := True;
end;

function ArrProto_Concat(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var R: PObj; I, J: Integer; V: TValue;
begin
  R := NewArray(0, 8);
  PushRoot(MkPtr(R));
  for I := 0 to AArgc do
  begin
    V := AArgs[I];
    if IsArrayValue(V) then
    begin
      for J := 0 to ArrayLen(ObjOf(AArgs[I])) - 1 do
        ArrayPush(ObjOf(RootAt(TempTop - 1)), ObjOf(AArgs[I])^.Elements^.Data[J]);
    end
    else ArrayPush(ObjOf(RootAt(TempTop - 1)), V);
  end;
  ARes := RootAt(TempTop - 1);
  PopRoots(1);
  Result := True;
end;

function ArrProto_Join(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var A: PObj; Sep: UnicodeString; S: PStr; I, N: Integer; V, SV: TValue; B: TUnicodeStringBuilder;
  Parts: array of UnicodeString; Total: Integer; Out_: PStr; P: Integer;
begin
  if not ThisArray(AArgs, A) then Exit(False);
  if (AArgc >= 1) and (AArgs[1] <> VUndefined) then
  begin
    if not StrArg(AArgs, AArgc, 1, S) then Exit(False);
    Sep := StrToUnicode(S);
  end
  else Sep := ',';
  N := ArrayLen(ObjOf(AArgs[0]));
  SetLength(Parts, N);
  for I := 0 to N - 1 do
  begin
    V := ObjOf(AArgs[0])^.Elements^.Data[I];
    if IsNullish(V) or (V = VHole) then Parts[I] := ''
    else
    begin
      if not ToStringV(V, SV) then Exit(False);
      Parts[I] := StrToUnicode(StrOf(SV));
    end;
  end;
  ARes := StrFromUnicode(UnicodeString(''.Join(Sep, Parts)));
  Result := True;
end;

function ArrProto_ToString(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
begin
  if not IsArrayValue(AArgs[0]) then begin ARes := StrFromUnicode('[object Object]'); Exit(True); end;
  Result := ArrProto_Join(AArgs, 0, ARes);
end;

function ArrProto_Reverse(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var A: PObj; I, N: Integer; T: TValue;
begin
  if not ThisArray(AArgs, A) then Exit(False);
  N := ArrayLen(A);
  for I := 0 to N div 2 - 1 do
  begin
    T := A^.Elements^.Data[I];
    A^.Elements^.Data[I] := A^.Elements^.Data[N - 1 - I];
    A^.Elements^.Data[N - 1 - I] := T;
  end;
  ARes := AArgs[0];
  Result := True;
end;

function ArrProto_IndexOf(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var A: PObj; I, N: Integer; X: Int64; V: TValue;
begin
  if not ThisArray(AArgs, A) then Exit(False);
  N := ArrayLen(A);
  if not IntArg(AArgs, AArgc, 2, 0, X) then Exit(False);
  A := ObjOf(AArgs[0]);
  V := Arg(AArgs, AArgc, 1);
  for I := RelIndex(X, N) to N - 1 do
    if StrictEquals(A^.Elements^.Data[I], V) then begin ARes := MkInt(I); Exit(True); end;
  ARes := MkInt(-1);
  Result := True;
end;

function ArrProto_LastIndexOf(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var A: PObj; I, N, Start: Integer; V: TValue; X: Int64;
begin
  if not ThisArray(AArgs, A) then Exit(False);
  N := ArrayLen(A);
  { fromIndex: absent means the last element; negative counts from the end }
  if AArgc >= 2 then
  begin
    if not IntArg(AArgs, AArgc, 2, N - 1, X) then Exit(False);
    A := ObjOf(AArgs[0]);
    if X < 0 then X := X + N;
    if X > N - 1 then X := N - 1;
    Start := X;
  end
  else Start := N - 1;
  V := Arg(AArgs, AArgc, 1);
  for I := Start downto 0 do
    if StrictEquals(A^.Elements^.Data[I], V) then begin ARes := MkInt(I); Exit(True); end;
  ARes := MkInt(-1);
  Result := True;
end;

function ArrProto_Includes(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var A: PObj; I, N: Integer; V, E: TValue; X: Int64;
begin
  if not ThisArray(AArgs, A) then Exit(False);
  N := ArrayLen(A);
  if not IntArg(AArgs, AArgc, 2, 0, X) then Exit(False);
  A := ObjOf(AArgs[0]);
  V := Arg(AArgs, AArgc, 1);
  for I := RelIndex(X, N) to N - 1 do
  begin
    E := A^.Elements^.Data[I];
    if E = VHole then E := VUndefined;
    if StrictEquals(E, V) or (IsNumber(E) and IsNumber(V) and IsNaN(NumOf(E)) and IsNaN(NumOf(V))) then
    begin ARes := VTrue; Exit(True); end;
  end;
  ARes := VFalse;
  Result := True;
end;

function ArrProto_Fill(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var A: PObj; I, N, S, E_: Integer; X, Y: Int64; V: TValue;
begin
  if not ThisArray(AArgs, A) then Exit(False);
  N := ArrayLen(A);
  if not IntArg(AArgs, AArgc, 2, 0, X) then Exit(False);
  if not IntArg(AArgs, AArgc, 3, N, Y) then Exit(False);
  S := RelIndex(X, N); E_ := RelIndex(Y, N);
  A := ObjOf(AArgs[0]);
  V := Arg(AArgs, AArgc, 1);
  for I := S to E_ - 1 do A^.Elements^.Data[I] := V;
  WriteBarrier(@A^.Elements^.Hdr, V);
  ARes := AArgs[0];
  Result := True;
end;

type
  TIterKind = (ikForEach, ikMap, ikFilter, ikSome, ikEvery, ikFind, ikFindIndex, ikFindLast, ikFindLastIndex);

function ArrayIterate(AArgs: TArgs; AArgc: Integer; var ARes: TValue; AKind: TIterKind): Boolean;
var A, R: PObj; I, N, Base: Integer; V, CR, F: TValue; CallArgs: array[0..2] of TValue; Stop: Boolean;
begin
  if not ThisArray(AArgs, A) then Exit(False);
  F := Arg(AArgs, AArgc, 1);
  if not IsCallable(F) then Exit(ThrowTypeError('callback is not a function'));
  N := ArrayLen(A);
  ARes := VUndefined;
  if AKind in [ikMap] then begin R := NewArray(N, N); ARes := MkPtr(R); end
  else if AKind = ikFilter then begin R := NewArray(0, 4); ARes := MkPtr(R); end;
  if AKind = ikEvery then ARes := VTrue;
  if AKind = ikSome then ARes := VFalse;
  if AKind in [ikFindIndex, ikFindLastIndex] then ARes := MkInt(-1);
  PushRoot(ARes);
  I := 0;
  if AKind in [ikFindLast, ikFindLastIndex] then I := N - 1;
  while (I >= 0) and (I < N) do
  begin
    A := ObjOf(AArgs[0]);
    if I >= ArrayLen(A) then Break;
    V := A^.Elements^.Data[I];
    if V = VHole then
    begin
      if AKind in [ikForEach, ikMap, ikFilter, ikSome, ikEvery] then
      begin
        if AKind in [ikFindLast, ikFindLastIndex] then Dec(I) else Inc(I);
        Continue;
      end;
      V := VUndefined;
    end;
    { callback(value, index, array): the arguments live on the value stack via CallValue's copy }
    CallArgs[0] := V; CallArgs[1] := MkInt(I); CallArgs[2] := AArgs[0];
    PushRoot(V);
    if not CallValue(AArgs[1], Arg(AArgs, AArgc, 2), @CallArgs[0], 3, CR) then begin PopRoots(2); Exit(False); end;
    V := RootAt(TempTop - 1);
    PopRoots(1);
    Stop := False;
    case AKind of
      ikMap: ValueArrayStore(ObjOf(RootAt(TempTop - 1))^.Elements, I, CR);
      ikFilter: if ToBool(CR) then ArrayPush(ObjOf(RootAt(TempTop - 1)), V);
      ikSome: if ToBool(CR) then begin TempRoots[TempTop - 1] := VTrue; Stop := True; end;
      ikEvery: if not ToBool(CR) then begin TempRoots[TempTop - 1] := VFalse; Stop := True; end;
      ikFind, ikFindLast: if ToBool(CR) then begin TempRoots[TempTop - 1] := V; Stop := True; end;
      ikFindIndex, ikFindLastIndex: if ToBool(CR) then begin TempRoots[TempTop - 1] := MkInt(I); Stop := True; end;
    end;
    if Stop then Break;
    if AKind in [ikFindLast, ikFindLastIndex] then Dec(I) else Inc(I);
  end;
  ARes := RootAt(TempTop - 1);
  PopRoots(1);
  Result := True;
end;

function ArrProto_ForEach(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
begin Result := ArrayIterate(AArgs, AArgc, ARes, ikForEach); ARes := VUndefined; end;
function ArrProto_Map(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
begin Result := ArrayIterate(AArgs, AArgc, ARes, ikMap); end;
function ArrProto_Filter(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
begin Result := ArrayIterate(AArgs, AArgc, ARes, ikFilter); end;
function ArrProto_Some(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
begin Result := ArrayIterate(AArgs, AArgc, ARes, ikSome); end;
function ArrProto_Every(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
begin Result := ArrayIterate(AArgs, AArgc, ARes, ikEvery); end;
function ArrProto_Find(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
begin Result := ArrayIterate(AArgs, AArgc, ARes, ikFind); end;
function ArrProto_FindIndex(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
begin Result := ArrayIterate(AArgs, AArgc, ARes, ikFindIndex); end;
function ArrProto_FindLast(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
begin Result := ArrayIterate(AArgs, AArgc, ARes, ikFindLast); end;
function ArrProto_FindLastIndex(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
begin Result := ArrayIterate(AArgs, AArgc, ARes, ikFindLastIndex); end;

function ArrProto_Reduce(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var A: PObj; I, N: Integer; Acc, V: TValue; CallArgs: array[0..3] of TValue;
begin
  if not ThisArray(AArgs, A) then Exit(False);
  if not IsCallable(Arg(AArgs, AArgc, 1)) then Exit(ThrowTypeError('callback is not a function'));
  N := ArrayLen(A);
  I := 0;
  if AArgc >= 2 then Acc := AArgs[2]
  else
  begin
    while (I < N) and (A^.Elements^.Data[I] = VHole) do Inc(I);
    if I >= N then Exit(ThrowTypeError('Reduce of empty array with no initial value'));
    Acc := A^.Elements^.Data[I];
    Inc(I);
  end;
  PushRoot(Acc);
  while I < N do
  begin
    A := ObjOf(AArgs[0]);
    if I >= ArrayLen(A) then Break;
    V := A^.Elements^.Data[I];
    if V <> VHole then
    begin
      CallArgs[0] := RootAt(TempTop - 1); CallArgs[1] := V; CallArgs[2] := MkInt(I); CallArgs[3] := AArgs[0];
      if not CallValue(AArgs[1], VUndefined, @CallArgs[0], 4, Acc) then begin PopRoots(1); Exit(False); end;
      TempRoots[TempTop - 1] := Acc;
    end;
    Inc(I);
  end;
  ARes := RootAt(TempTop - 1);
  PopRoots(1);
  Result := True;
end;

function ArrProto_ReduceRight(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var A: PObj; I: Integer; Acc, V: TValue; CallArgs: array[0..3] of TValue;
begin
  if not ThisArray(AArgs, A) then Exit(False);
  if not IsCallable(Arg(AArgs, AArgc, 1)) then Exit(ThrowTypeError('callback is not a function'));
  I := ArrayLen(A) - 1;
  if AArgc >= 2 then Acc := AArgs[2]
  else
  begin
    while (I >= 0) and (A^.Elements^.Data[I] = VHole) do Dec(I);
    if I < 0 then Exit(ThrowTypeError('Reduce of empty array with no initial value'));
    Acc := A^.Elements^.Data[I];
    Dec(I);
  end;
  PushRoot(Acc);
  while I >= 0 do
  begin
    A := ObjOf(AArgs[0]);
    if I < ArrayLen(A) then
    begin
      V := A^.Elements^.Data[I];
      if V <> VHole then
      begin
        CallArgs[0] := RootAt(TempTop - 1); CallArgs[1] := V; CallArgs[2] := MkInt(I); CallArgs[3] := AArgs[0];
        if not CallValue(AArgs[1], VUndefined, @CallArgs[0], 4, Acc) then begin PopRoots(1); Exit(False); end;
        TempRoots[TempTop - 1] := Acc;
      end;
    end;
    Dec(I);
  end;
  ARes := RootAt(TempTop - 1);
  PopRoots(1);
  Result := True;
end;

threadvar
  SortCmp: TValue;
  SortFailed: Boolean;

function SortLess(A, B: TValue; var ALess: Boolean): Boolean;
var CallArgs: array[0..1] of TValue; R: TValue; D: Double; SA, SB: TValue;
begin
  if A = VUndefined then begin ALess := False; Exit(True); end;
  if B = VUndefined then begin ALess := True; Exit(True); end;
  if SortCmp <> VUndefined then
  begin
    CallArgs[0] := A; CallArgs[1] := B;
    if not CallValue(SortCmp, VUndefined, @CallArgs[0], 2, R) then Exit(False);
    if not ToNumberV(R, D) then Exit(False);
    ALess := D < 0;
    Exit(True);
  end;
  PushRoot(B);
  if not ToStringV(A, SA) then begin PopRoots(1); Exit(False); end;
  PushRoot(SA);
  if not ToStringV(RootAt(TempTop - 2), SB) then begin PopRoots(2); Exit(False); end;
  SA := RootAt(TempTop - 1);
  PopRoots(2);
  ALess := StrCompare(StrOf(SA), StrOf(SB)) < 0;
  Result := True;
end;

function ArrProto_Sort(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var A: PObj; N, I, J, W, L, R_, M, Mid: Integer; Tmp: PObj; Less: Boolean; Src, Dst, Swap: PValueArray; SavedCmp: TValue;
begin
  if not ThisArray(AArgs, A) then Exit(False);
  N := ArrayLen(A);
  SavedCmp := SortCmp;
  SortCmp := Arg(AArgs, AArgc, 1);
  AddRoot(@SortCmp);
  { stable merge sort into a scratch array; elements stay reachable from the array and the scratch }
  Tmp := NewArray(N, N);
  PushRoot(MkPtr(Tmp));
  W := 1;
  while W < N do
  begin
    I := 0;
    while I < N do
    begin
      L := I; M := Min(I + W, N); R_ := Min(I + 2 * W, N); Mid := M;
      J := L;
      while (L < Mid) or (M < R_) do
      begin
        Src := ObjOf(AArgs[0])^.Elements;
        Dst := ObjOf(RootAt(TempTop - 1))^.Elements;
        if L >= Min(I + W, N) then begin Dst^.Data[J] := Src^.Data[M]; Inc(M); end
        else if M >= R_ then begin Dst^.Data[J] := Src^.Data[L]; Inc(L); end
        else
        begin
          if not SortLess(Src^.Data[M], Src^.Data[L], Less) then
          begin
            PopRoots(1); Dec(ExtraRootCount); SortCmp := SavedCmp; Exit(False);
          end;
          Src := ObjOf(AArgs[0])^.Elements;
          Dst := ObjOf(RootAt(TempTop - 1))^.Elements;
          if Less then begin Dst^.Data[J] := Src^.Data[M]; Inc(M); end
          else begin Dst^.Data[J] := Src^.Data[L]; Inc(L); end;
        end;
        Inc(J);
      end;
      Inc(I, 2 * W);
    end;
    Src := ObjOf(AArgs[0])^.Elements;
    Dst := ObjOf(RootAt(TempTop - 1))^.Elements;
    Move(Dst^.Data[0], Src^.Data[0], N * SizeOf(TValue));
    if (Src^.Hdr.Flags and cfOld) <> 0 then Remember(@Src^.Hdr);
    W := W * 2;
  end;
  PopRoots(1);
  Dec(ExtraRootCount);
  SortCmp := SavedCmp;
  ARes := AArgs[0];
  Result := True;
end;

function ArrProto_Values(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
begin
  if IsString(AArgs[0]) or (IsObject(AArgs[0]) and (ObjOf(AArgs[0])^.OKind in [okArray, okArguments, 14])) then
    Exit(GetIterator(AArgs[0], ARes));
  { any other array-like: iterate a snapshot of its indexed elements }
  if not MaterializeArrayLike(AArgs) then Exit(False);
  ARes := NewArrayIterator(AArgs[0], 0);
  Result := True;
end;

function ArrIter_Next(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var V: TValue; Done: Boolean; O: PObj;
begin
  if not IteratorStep(AArgs[0], V, Done) then Exit(False);
  PushRoot(V);
  O := NewPlainObject(ObjectProto, 2);
  V := RootAt(TempTop - 1);
  PopRoots(1);
  AddProperty(O, AtomValue, V, pfDefault);
  AddProperty(O, AtomDone, MkBool(Done), pfDefault);
  ARes := MkPtr(O);
  Result := True;
end;

function Iter_Self(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
begin
  ARes := AArgs[0];
  Result := True;
end;

{ ---------------- String ---------------- }

function ThisStr(AArgs: TArgs; var S: PStr): Boolean; inline;
var V: TValue;
begin
  if IsString(AArgs[0]) then begin S := StrOf(AArgs[0]); Exit(True); end;
  if IsObject(AArgs[0]) and (ObjOf(AArgs[0])^.OKind = okStringObj) then
  begin
    { callers read the receiver again from Args[0]: it must hold the string, not the wrapper }
    AArgs[0] := ObjOf(AArgs[0])^.Aux1;
    S := StrOf(AArgs[0]);
    Exit(True);
  end;
  if IsNullish(AArgs[0]) then Exit(ThrowTypeError('String.prototype method called on null or undefined'));
  if not ToStringV(AArgs[0], V) then Exit(False);
  AArgs[0] := V;
  S := StrOf(V);
  Result := True;
end;

function String_Ctor(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var V: TValue; O: PObj;
begin
  if AArgc = 0 then V := MkPtr(AtomOf(''))
  else if IsPtr(AArgs[1]) and (PCell(PtrOf(AArgs[1]))^.Kind = ckSymbol) then
    V := StrFromUnicode('Symbol(' + StrToUnicode(StrOf(PValue(PByte(PtrOf(AArgs[1])) + 8)^)) + ')')
  else if not ToStringV(AArgs[1], V) then Exit(False);
  if CurNewTarget <> VUndefined then
  begin
    PushRoot(V);
    O := NewObject(RootShapeFor(StringProto), 0, okStringObj);
    O^.Aux1 := RootAt(TempTop - 1);
    PopRoots(1);
    V := MkPtr(O);
  end;
  ARes := V;
  Result := True;
end;

function String_FromCharCode(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var S: PStr; I: Integer; D: Double; Codes: array of Word;
begin
  SetLength(Codes, AArgc);
  for I := 1 to AArgc do
  begin
    if not NumArg(AArgs, AArgc, I, D) then Exit(False);
    Codes[I - 1] := Word(Trunc(D) and $FFFF);
  end;
  S := NewString(AArgc);
  for I := 1 to AArgc do StrChars(S)[I - 1] := WideChar(Codes[I - 1]);
  ARes := MkPtr(S);
  Result := True;
end;

function StrProto_CharAt(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var S: PStr; X: Int64;
begin
  if not ThisStr(AArgs, S) then Exit(False);
  if not IntArg(AArgs, AArgc, 1, 0, X) then Exit(False);
  S := StrOf(AArgs[0]);
  if (X < 0) or (X >= S^.Len) then ARes := MkPtr(AtomOf('')) else ARes := SubString(S, X, 1);
  Result := True;
end;

function StrProto_CharCodeAt(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var S: PStr; X: Int64;
begin
  if not ThisStr(AArgs, S) then Exit(False);
  if not IntArg(AArgs, AArgc, 1, 0, X) then Exit(False);
  S := StrOf(AArgs[0]);
  if (X < 0) or (X >= S^.Len) then ARes := MkDouble(NaN) else ARes := MkInt(Ord(StrChars(S)[X]));
  Result := True;
end;

function StrProto_CodePointAt(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var S: PStr; X: Int64; C, C2: Integer;
begin
  if not ThisStr(AArgs, S) then Exit(False);
  if not IntArg(AArgs, AArgc, 1, 0, X) then Exit(False);
  S := StrOf(AArgs[0]);
  if (X < 0) or (X >= S^.Len) then begin ARes := VUndefined; Exit(True); end;
  C := Ord(StrChars(S)[X]);
  if (C >= $D800) and (C <= $DBFF) and (X + 1 < S^.Len) then
  begin
    C2 := Ord(StrChars(S)[X + 1]);
    if (C2 >= $DC00) and (C2 <= $DFFF) then C := (C - $D800) * $400 + (C2 - $DC00) + $10000;
  end;
  ARes := MkInt(C);
  Result := True;
end;

function FindIn(S, ASub: PStr; AFrom: Integer): Integer;
var I, J: Integer; Ok: Boolean;
begin
  if AFrom > S^.Len then AFrom := S^.Len;
  if ASub^.Len = 0 then Exit(Max(AFrom, 0));
  for I := Max(AFrom, 0) to S^.Len - ASub^.Len do
  begin
    Ok := True;
    for J := 0 to ASub^.Len - 1 do
      if StrChars(S)[I + J] <> StrChars(ASub)[J] then begin Ok := False; Break; end;
    if Ok then Exit(I);
  end;
  Result := -1;
end;

function StrProto_IndexOf(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var S, Sub: PStr; X: Int64;
begin
  if not ThisStr(AArgs, S) then Exit(False);
  if not StrArg(AArgs, AArgc, 1, Sub) then Exit(False);
  if not IntArg(AArgs, AArgc, 2, 0, X) then Exit(False);
  ARes := MkInt(FindIn(StrOf(AArgs[0]), Sub, X));
  Result := True;
end;

function StrProto_LastIndexOf(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var S, Sub: PStr; I, J, Start: Integer; Ok: Boolean; X: Int64;
begin
  if not ThisStr(AArgs, S) then Exit(False);
  if not StrArg(AArgs, AArgc, 1, Sub) then Exit(False);
  if not IntArg(AArgs, AArgc, 2, MaxInt, X) then Exit(False);
  S := StrOf(AArgs[0]);
  if X < 0 then X := 0;
  Start := S^.Len - Sub^.Len;
  if X < Start then Start := X;
  for I := Start downto 0 do
  begin
    Ok := True;
    for J := 0 to Sub^.Len - 1 do
      if StrChars(S)[I + J] <> StrChars(Sub)[J] then begin Ok := False; Break; end;
    if Ok then begin ARes := MkInt(I); Exit(True); end;
  end;
  ARes := MkInt(-1);
  Result := True;
end;

function StrProto_Includes(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var S, Sub: PStr; X: Int64;
begin
  if not ThisStr(AArgs, S) then Exit(False);
  if not StrArg(AArgs, AArgc, 1, Sub) then Exit(False);
  if not IntArg(AArgs, AArgc, 2, 0, X) then Exit(False);
  if X < 0 then X := 0;
  ARes := MkBool(FindIn(StrOf(AArgs[0]), Sub, X) >= 0);
  Result := True;
end;

function StrProto_StartsWith(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var S, Sub: PStr; X: Int64;
begin
  if not ThisStr(AArgs, S) then Exit(False);
  if not StrArg(AArgs, AArgc, 1, Sub) then Exit(False);
  if not IntArg(AArgs, AArgc, 2, 0, X) then Exit(False);
  S := StrOf(AArgs[0]);
  if X < 0 then X := 0;
  if X > S^.Len then X := S^.Len;
  ARes := MkBool((X + Sub^.Len <= S^.Len) and ((Sub^.Len = 0) or (CompareByte(StrChars(S)[X], StrChars(Sub)[0], Sub^.Len * 2) = 0)));
  Result := True;
end;

function StrProto_EndsWith(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var S, Sub: PStr; P: Integer; X: Int64;
begin
  if not ThisStr(AArgs, S) then Exit(False);
  if not StrArg(AArgs, AArgc, 1, Sub) then Exit(False);
  if not IntArg(AArgs, AArgc, 2, MaxInt, X) then Exit(False);
  S := StrOf(AArgs[0]);
  if X < 0 then X := 0;
  if X > S^.Len then X := S^.Len;
  P := X - Sub^.Len;
  ARes := MkBool((P >= 0) and ((Sub^.Len = 0) or (CompareByte(StrChars(S)[P], StrChars(Sub)[0], Sub^.Len * 2) = 0)));
  Result := True;
end;

function StrProto_Slice(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var S: PStr; X, Y: Int64; A, B: Integer;
begin
  if not ThisStr(AArgs, S) then Exit(False);
  if not IntArg(AArgs, AArgc, 1, 0, X) then Exit(False);
  if not IntArg(AArgs, AArgc, 2, StrOf(AArgs[0])^.Len, Y) then Exit(False);
  S := StrOf(AArgs[0]);
  A := RelIndex(X, S^.Len); B := RelIndex(Y, S^.Len);
  if B < A then B := A;
  ARes := SubString(S, A, B - A);
  Result := True;
end;

function StrProto_Substring(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var S: PStr; X, Y: Int64; A, B, T: Integer;
begin
  if not ThisStr(AArgs, S) then Exit(False);
  if not IntArg(AArgs, AArgc, 1, 0, X) then Exit(False);
  if not IntArg(AArgs, AArgc, 2, StrOf(AArgs[0])^.Len, Y) then Exit(False);
  S := StrOf(AArgs[0]);
  A := Max(0, Min(X, S^.Len)); B := Max(0, Min(Y, S^.Len));
  if A > B then begin T := A; A := B; B := T; end;
  ARes := SubString(S, A, B - A);
  Result := True;
end;

function StrProto_Substr(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var S: PStr; X, Y: Int64; A, L: Integer;
begin
  if not ThisStr(AArgs, S) then Exit(False);
  if not IntArg(AArgs, AArgc, 1, 0, X) then Exit(False);
  if not IntArg(AArgs, AArgc, 2, StrOf(AArgs[0])^.Len, Y) then Exit(False);
  S := StrOf(AArgs[0]);
  A := RelIndex(X, S^.Len);
  L := Max(0, Min(Y, S^.Len - A));
  ARes := SubString(S, A, L);
  Result := True;
end;

function StrProto_ToUpper(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var S: PStr;
begin
  if not ThisStr(AArgs, S) then Exit(False);
  ARes := StrFromUnicode(UnicodeUpperCase(StrToUnicode(S)));
  Result := True;
end;

function StrProto_ToLower(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var S: PStr;
begin
  if not ThisStr(AArgs, S) then Exit(False);
  ARes := StrFromUnicode(UnicodeLowerCase(StrToUnicode(S)));
  Result := True;
end;

function IsJSWhite(C: WideChar): Boolean; inline;
begin
  case Ord(C) of
    9..13, 32, $A0, $1680, $2000..$200A, $2028, $2029, $202F, $205F, $3000, $FEFF: Result := True;
  else Result := False;
  end;
end;

function TrimMode(AArgs: TArgs; var ARes: TValue; AMode: Integer): Boolean;
var S: PStr; A, B: Integer;
begin
  if not ThisStr(AArgs, S) then Exit(False);
  A := 0; B := S^.Len;
  if AMode <> 2 then while (A < B) and IsJSWhite(StrChars(S)[A]) do Inc(A);
  if AMode <> 1 then while (B > A) and IsJSWhite(StrChars(S)[B - 1]) do Dec(B);
  if (A = 0) and (B = S^.Len) then ARes := MkPtr(S) else ARes := SubString(S, A, B - A);
  Result := True;
end;

function StrProto_Trim(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
begin Result := TrimMode(AArgs, ARes, 0); end;
function StrProto_TrimStart(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
begin Result := TrimMode(AArgs, ARes, 1); end;
function StrProto_TrimEnd(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
begin Result := TrimMode(AArgs, ARes, 2); end;

function StrProto_At(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var S: PStr; N: Int64;
begin
  if not ThisStr(AArgs, S) then Exit(False);
  if not IntArg(AArgs, AArgc, 1, 0, N) then Exit(False);
  S := StrOf(AArgs[0]);
  if N < 0 then N := N + S^.Len;
  if (N < 0) or (N >= S^.Len) then ARes := VUndefined else ARes := SubString(S, N, 1);
  Result := True;
end;

function StrProto_Normalize(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var S: PStr;
begin
  { the spike's inputs are ASCII or already composed }
  if not ThisStr(AArgs, S) then Exit(False);
  ARes := AArgs[0];
  Result := True;
end;

function StrProto_Repeat(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var S: PStr; N: Int64; T, U: UnicodeString; I: Integer;
begin
  if not ThisStr(AArgs, S) then Exit(False);
  if not IntArg(AArgs, AArgc, 1, 0, N) then Exit(False);
  if N < 0 then Exit(ThrowError('RangeError', 'Invalid count value'));
  T := StrToUnicode(StrOf(AArgs[0]));
  U := '';
  for I := 1 to N do U := U + T;
  ARes := StrFromUnicode(U);
  Result := True;
end;

function StrProto_Pad(AArgs: TArgs; AArgc: Integer; var ARes: TValue; AAtStart: Boolean): Boolean;
var S, F: PStr; N: Int64; T, Fill, P: UnicodeString;
begin
  if not ThisStr(AArgs, S) then Exit(False);
  if not IntArg(AArgs, AArgc, 1, 0, N) then Exit(False);
  if (AArgc >= 2) and (AArgs[2] <> VUndefined) then
  begin
    if not StrArg(AArgs, AArgc, 2, F) then Exit(False);
    Fill := StrToUnicode(F);
  end
  else Fill := ' ';
  T := StrToUnicode(StrOf(AArgs[0]));
  if (N <= Length(T)) or (Fill = '') then begin ARes := AArgs[0]; Exit(True); end;
  P := '';
  while Length(P) < N - Length(T) do P := P + Fill;
  SetLength(P, N - Length(T));
  if AAtStart then ARes := StrFromUnicode(P + T) else ARes := StrFromUnicode(T + P);
  Result := True;
end;
function StrProto_PadStart(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
begin Result := StrProto_Pad(AArgs, AArgc, ARes, True); end;
function StrProto_PadEnd(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
begin Result := StrProto_Pad(AArgs, AArgc, ARes, False); end;

function StrProto_Split(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var S, Sep: PStr; A: PObj; I, P, Start: Integer; Part: TValue; LimD: Double; Lim: Int64;
begin
  if not ThisStr(AArgs, S) then Exit(False);
  Lim := 4294967295;
  if Arg(AArgs, AArgc, 2) <> VUndefined then
  begin
    if not ToNumberV(AArgs[2], LimD) then Exit(False);
    if IsNaN(LimD) or IsInfinite(LimD) then Lim := 0
    else Lim := Int64(Trunc(LimD)) and $FFFFFFFF;   { ToUint32 }
  end;
  A := NewArray(0, 8);
  PushRoot(MkPtr(A));
  if Lim = 0 then begin ARes := RootAt(TempTop - 1); PopRoots(1); Exit(True); end;
  if Arg(AArgs, AArgc, 1) = VUndefined then
  begin
    ArrayPush(ObjOf(RootAt(TempTop - 1)), AArgs[0]);
    ARes := RootAt(TempTop - 1);
    PopRoots(1);
    Exit(True);
  end;
  if not StrArg(AArgs, AArgc, 1, Sep) then begin PopRoots(1); Exit(False); end;
  PushRoot(MkPtr(Sep));
  S := StrOf(AArgs[0]);
  if Sep^.Len = 0 then
  begin
    for I := 0 to S^.Len - 1 do
    begin
      if ArrayLen(ObjOf(RootAt(TempTop - 2))) >= Lim then Break;
      Part := SubString(StrOf(AArgs[0]), I, 1);
      ArrayPush(ObjOf(RootAt(TempTop - 2)), Part);
    end;
  end
  else
  begin
    Start := 0;
    while True do
    begin
      P := FindIn(StrOf(AArgs[0]), StrOf(RootAt(TempTop - 1)), Start);
      if P < 0 then Break;
      Part := SubString(StrOf(AArgs[0]), Start, P - Start);
      ArrayPush(ObjOf(RootAt(TempTop - 2)), Part);
      if ArrayLen(ObjOf(RootAt(TempTop - 2))) >= Lim then
      begin
        ARes := RootAt(TempTop - 2);
        PopRoots(2);
        Exit(True);
      end;
      Start := P + StrOf(RootAt(TempTop - 1))^.Len;
    end;
    Part := SubString(StrOf(AArgs[0]), Start, StrOf(AArgs[0])^.Len - Start);
    ArrayPush(ObjOf(RootAt(TempTop - 2)), Part);
  end;
  ARes := RootAt(TempTop - 2);
  PopRoots(2);
  Result := True;
end;

{ GetSubstitution for a string pattern: no captures, so $n stays literal }
function StringSubstitution(const AMatched, AStr: UnicodeString; APosition: Integer; const ARep: UnicodeString): UnicodeString;
var I: Integer; C: WideChar;
begin
  Result := '';
  I := 1;
  while I <= Length(ARep) do
  begin
    if (ARep[I] = '$') and (I < Length(ARep)) then
    begin
      C := ARep[I + 1];
      case C of
        '$': begin Result := Result + '$'; Inc(I, 2); Continue; end;
        '&': begin Result := Result + AMatched; Inc(I, 2); Continue; end;
        '`': begin Result := Result + Copy(AStr, 1, APosition); Inc(I, 2); Continue; end;
        #39: begin Result := Result + Copy(AStr, APosition + Length(AMatched) + 1, MaxInt); Inc(I, 2); Continue; end;
      end;
    end;
    Result := Result + ARep[I];
    Inc(I);
  end;
end;

{ String.prototype.replace / replaceAll with a string pattern }
function StrProto_Replace(AArgs: TArgs; AArgc: Integer; var ARes: TValue; AAll: Boolean): Boolean;
var S, Pat, Rep: PStr; T, P, R, U: UnicodeString; Positions: array of Integer; N, I, Pos_, Last, Adv: Integer;
  F, CR, M: TValue; CallArgs: array[0..2] of TValue; Functional: Boolean;
begin
  if not ThisStr(AArgs, S) then Exit(False);
  if not StrArg(AArgs, AArgc, 1, Pat) then Exit(False);
  T := StrToUnicode(StrOf(AArgs[0]));
  P := StrToUnicode(Pat);
  F := Arg(AArgs, AArgc, 2);
  Functional := IsCallable(F);
  R := '';
  PushRoot(MkPtr(Pat));
  if not Functional then
  begin
    if not StrArg(AArgs, AArgc, 2, Rep) then begin PopRoots(1); Exit(False); end;
    R := StrToUnicode(Rep);
  end;
  Pat := StrOf(RootAt(TempTop - 1));
  PopRoots(1);   { no allocation until the positions are found }
  { the match positions, 0-based }
  N := 0;
  SetLength(Positions, 0);
  Adv := Max(1, Length(P));
  Pos_ := FindIn(StrOf(AArgs[0]), Pat, 0);
  while Pos_ >= 0 do
  begin
    SetLength(Positions, N + 1);
    Positions[N] := Pos_;
    Inc(N);
    if not AAll then Break;
    if Pos_ + Adv > Length(T) then Break;
    Pos_ := FindIn(StrOf(AArgs[0]), Pat, Pos_ + Adv);
  end;
  if N = 0 then begin ARes := AArgs[0]; Exit(True); end;
  U := '';
  Last := 0;
  PushRoot(F); PushRoot(AArgs[0]);
  for I := 0 to N - 1 do
  begin
    U := U + Copy(T, Last + 1, Positions[I] - Last);
    if Functional then
    begin
      M := StrFromUnicode(P);
      CallArgs[0] := M; CallArgs[1] := MkInt(Positions[I]); CallArgs[2] := RootAt(TempTop - 1);
      if not CallValue(RootAt(TempTop - 2), VUndefined, @CallArgs[0], 3, CR) then begin PopRoots(2); Exit(False); end;
      if not ToStringV(CR, M) then begin PopRoots(2); Exit(False); end;
      U := U + StrToUnicode(StrOf(M));
    end
    else U := U + StringSubstitution(P, T, Positions[I], R);
    Last := Positions[I] + Length(P);
  end;
  PopRoots(2);
  U := U + Copy(T, Last + 1, MaxInt);
  ARes := StrFromUnicode(U);
  Result := True;
end;

function StrProto_ReplaceOne(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
begin Result := StrProto_Replace(AArgs, AArgc, ARes, False); end;
function StrProto_ReplaceAll(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
begin Result := StrProto_Replace(AArgs, AArgc, ARes, True); end;

function StrProto_Concat(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var S: PStr; I: Integer; V: TValue;
begin
  if not ThisStr(AArgs, S) then Exit(False);
  ARes := AArgs[0];
  for I := 1 to AArgc do
  begin
    PushRoot(ARes);
    if not ToStringV(AArgs[I], V) then begin PopRoots(1); Exit(False); end;
    ARes := ConcatStrings(StrOf(RootAt(TempTop - 1)), StrOf(V));
    PopRoots(1);
  end;
  Result := True;
end;

function StrProto_ToString(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var S: PStr;
begin
  if not ThisStr(AArgs, S) then Exit(False);
  ARes := MkPtr(S);
  Result := True;
end;

function StrProto_LocaleCompare(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var S, T: PStr; C: Integer;
begin
  if not ThisStr(AArgs, S) then Exit(False);
  if not StrArg(AArgs, AArgc, 1, T) then Exit(False);
  C := StrCompare(StrOf(AArgs[0]), T);
  if C < 0 then ARes := MkInt(-1) else if C > 0 then ARes := MkInt(1) else ARes := MkInt(0);
  Result := True;
end;

{ ---------------- Number / Boolean ---------------- }

function Number_Ctor(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var D: Double; O: PObj; P: TValue;
begin
  { 21.1.1.1: ToNumeric; a BigInt becomes the Number of its value }
  if AArgc = 0 then D := 0
  else
  begin
    P := AArgs[1];
    if IsObject(P) and not ToPrimitiveHint(P, phNumber, P) then Exit(False);
    if IsBigInt(P) then D := ValueToBig(P).ToDouble
    else if not ToNumberV(P, D) then Exit(False);
  end;
  ARes := MkNumber(D);
  if CurNewTarget <> VUndefined then
  begin
    { new Number: a wrapper }
    O := NewObject(RootShapeFor(NumberProto), 0, okNumber);
    O^.Aux1 := ARes;
    ARes := MkPtr(O);
  end;
  Result := True;
end;

function Number_IsInteger(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var V: TValue; D: Double;
begin
  V := Arg(AArgs, AArgc, 1);
  if not IsNumber(V) then begin ARes := VFalse; Exit(True); end;
  D := NumOf(V);
  ARes := MkBool(not IsNaN(D) and not IsInfinite(D) and (Frac(D) = 0));
  Result := True;
end;

function ThisNum(AArgs: TArgs; var D: Double): Boolean;
begin
  if IsNumber(AArgs[0]) then begin D := NumOf(AArgs[0]); Exit(True); end;
  if IsObject(AArgs[0]) and (ObjOf(AArgs[0])^.OKind = okNumber) then begin D := NumOf(ObjOf(AArgs[0])^.Aux1); Exit(True); end;
  Result := ThrowTypeError('Number.prototype method called on incompatible receiver');
end;

function NumProto_ToString(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var D: Double; Radix: Int64; N: Int64; Neg: Boolean; S: UnicodeString; Dg: Integer;
begin
  if not ThisNum(AArgs, D) then Exit(False);
  if not IntArg(AArgs, AArgc, 1, 10, Radix) then Exit(False);
  if (Radix = 10) or IsNaN(D) or IsInfinite(D) or (Frac(D) <> 0) then begin ARes := NumberToStr(D); Exit(True); end;
  N := Trunc(D); Neg := N < 0; if Neg then N := -N;
  S := '';
  repeat
    Dg := N mod Radix;
    if Dg < 10 then S := WideChar(48 + Dg) + S else S := WideChar(87 + Dg) + S;
    N := N div Radix;
  until N = 0;
  if Neg then S := '-' + S;
  ARes := StrFromUnicode(S);
  Result := True;
end;

function NumProto_ToFixed(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var D: Double; Digits: Int64; FS: TFormatSettings;
begin
  if not ThisNum(AArgs, D) then Exit(False);
  if not IntArg(AArgs, AArgc, 1, 0, Digits) then Exit(False);
  FS := DefaultFormatSettings;
  FS.DecimalSeparator := '.';
  ARes := StrFromUnicode(UnicodeString(FloatToStrF(D, ffFixed, 18, Digits, FS)));
  Result := True;
end;

function NumProto_ValueOf(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var D: Double;
begin
  if not ThisNum(AArgs, D) then Exit(False);
  ARes := MkNumber(D);
  Result := True;
end;

function Boolean_Ctor(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var O: PObj;
begin
  ARes := MkBool(ToBool(Arg(AArgs, AArgc, 1)));
  if CurNewTarget <> VUndefined then
  begin
    O := NewObject(RootShapeFor(BooleanProto), 0, okBoolean);
    O^.Aux1 := ARes;
    ARes := MkPtr(O);
  end;
  Result := True;
end;

{ ---------------- Math ---------------- }

function Math1(AArgs: TArgs; AArgc: Integer; var ARes: TValue; F: Integer): Boolean;
var D: Double;
begin
  if not NumArg(AArgs, AArgc, 1, D) then Exit(False);
  case F of
    0: D := Abs(D);
    1: D := DFloor(D);
    2: D := DCeil(D);
    3: D := DRound(D);
    4: D := Sqrt(D);
    5: D := Sin(D);
    6: D := Cos(D);
    7: D := Tan(D);
    8: D := ArcTan(D);
    9: D := Exp(D);
    10: if D > 0 then D := Ln(D) else if D = 0 then D := NegInfinity else D := NaN;
    11: if not (IsNaN(D) or IsInfinite(D)) then D := Int(D);
    12: if D > 0 then D := 1 else if D < 0 then D := -1;
    13: D := ArcSin(D);
    14: D := ArcCos(D);
    15: if D > 0 then D := Log2(D) else D := NaN;
    16: if D > 0 then D := Log10(D) else D := NaN;
    17: D := Power(Abs(D), 1 / 3) * Sign(D);
  end;
  ARes := MkNumber(D);
  Result := True;
end;

function M_Abs(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
begin
  if IsInt(Arg(AArgs, AArgc, 1)) and (IntOf(AArgs[1]) <> Low(Int32)) then begin ARes := MkInt(Abs(IntOf(AArgs[1]))); Exit(True); end;
  Result := Math1(AArgs, AArgc, ARes, 0);
end;
function M_Floor(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
begin
  if IsInt(Arg(AArgs, AArgc, 1)) then begin ARes := AArgs[1]; Exit(True); end;
  Result := Math1(AArgs, AArgc, ARes, 1);
end;
function M_Ceil(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean; begin Result := Math1(AArgs, AArgc, ARes, 2); end;
function M_Round(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean; begin Result := Math1(AArgs, AArgc, ARes, 3); end;
function M_Sqrt(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean; begin Result := Math1(AArgs, AArgc, ARes, 4); end;
function M_Sin(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean; begin Result := Math1(AArgs, AArgc, ARes, 5); end;
function M_Cos(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean; begin Result := Math1(AArgs, AArgc, ARes, 6); end;
function M_Tan(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean; begin Result := Math1(AArgs, AArgc, ARes, 7); end;
function M_Atan(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean; begin Result := Math1(AArgs, AArgc, ARes, 8); end;
function M_Exp(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean; begin Result := Math1(AArgs, AArgc, ARes, 9); end;
function M_Log(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean; begin Result := Math1(AArgs, AArgc, ARes, 10); end;
function M_Trunc(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean; begin Result := Math1(AArgs, AArgc, ARes, 11); end;
function M_Sign(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean; begin Result := Math1(AArgs, AArgc, ARes, 12); end;
function M_Asin(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean; begin Result := Math1(AArgs, AArgc, ARes, 13); end;
function M_Acos(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean; begin Result := Math1(AArgs, AArgc, ARes, 14); end;
function M_Log2(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean; begin Result := Math1(AArgs, AArgc, ARes, 15); end;
function M_Log10(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean; begin Result := Math1(AArgs, AArgc, ARes, 16); end;
function M_Cbrt(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean; begin Result := Math1(AArgs, AArgc, ARes, 17); end;

function M_MinMax(AArgs: TArgs; AArgc: Integer; var ARes: TValue; AIsMax: Boolean): Boolean;
var I: Integer; D, R: Double; AllInt: Boolean; IR: Int32;
begin
  AllInt := AArgc > 0;
  for I := 1 to AArgc do if not IsInt(AArgs[I]) then begin AllInt := False; Break; end;
  if AllInt then
  begin
    IR := IntOf(AArgs[1]);
    for I := 2 to AArgc do
      if AIsMax then begin if IntOf(AArgs[I]) > IR then IR := IntOf(AArgs[I]); end
      else if IntOf(AArgs[I]) < IR then IR := IntOf(AArgs[I]);
    ARes := MkInt(IR);
    Exit(True);
  end;
  if AIsMax then R := NegInfinity else R := Infinity;
  for I := 1 to AArgc do
  begin
    if not NumArg(AArgs, AArgc, I, D) then Exit(False);
    if IsNaN(D) then R := NaN
    else if not IsNaN(R) then
      if AIsMax then begin if D > R then R := D; end else if D < R then R := D;
  end;
  ARes := MkNumber(R);
  Result := True;
end;
function M_Max(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean; begin Result := M_MinMax(AArgs, AArgc, ARes, True); end;
function M_Min(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean; begin Result := M_MinMax(AArgs, AArgc, ARes, False); end;

function M_Pow(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var A, B: Double;
begin
  if not NumArg(AArgs, AArgc, 1, A) then Exit(False);
  if not NumArg(AArgs, AArgc, 2, B) then Exit(False);
  ARes := MkNumber(NumberExponentiate(A, B));
  Result := True;
end;

function M_Atan2(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var A, B: Double;
begin
  if not NumArg(AArgs, AArgc, 1, A) then Exit(False);
  if not NumArg(AArgs, AArgc, 2, B) then Exit(False);
  ARes := MkNumber(ArcTan2(A, B));
  Result := True;
end;

function M_Hypot(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var I: Integer; D, S: Double;
begin
  S := 0;
  for I := 1 to AArgc do
  begin
    if not NumArg(AArgs, AArgc, I, D) then Exit(False);
    S := S + D * D;
  end;
  ARes := MkNumber(Sqrt(S));
  Result := True;
end;

var
  RngState: QWord = 88172645463325252;

function M_Random(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
begin
  RngState := RngState xor (RngState shl 13);
  RngState := RngState xor (RngState shr 7);
  RngState := RngState xor (RngState shl 17);
  ARes := MkDouble((RngState shr 11) * (1.0 / 9007199254740992.0));
  Result := True;
end;

{ ---------------- JSON ---------------- }

function JSONQuote(const S: UnicodeString): UnicodeString;
var I: Integer; C: WideChar;
begin
  Result := '"';
  for I := 1 to Length(S) do
  begin
    C := S[I];
    case C of
      '"': Result := Result + '\"';
      '\': Result := Result + '\\';
      #8: Result := Result + '\b';
      #9: Result := Result + '\t';
      #10: Result := Result + '\n';
      #12: Result := Result + '\f';
      #13: Result := Result + '\r';
    else
      if Ord(C) < 32 then Result := Result + '\u' + LowerCase(IntToHex(Ord(C), 4))
      else Result := Result + C;
    end;
  end;
  Result := Result + '"';
end;

function JSONStr(V: TValue; var AOut_: UnicodeString; var ASkip: Boolean; ADepth: Integer): Boolean;
var Keys, X, TS: TValue; I, N: Integer; S: UnicodeString; First, Sk: Boolean; K: PStr; F: TValue;
begin
  ASkip := False;
  if ADepth > 200 then Exit(ThrowTypeError('Converting circular structure to JSON'));
  if IsObject(V) then
  begin
    PushRoot(V);
    if not GetV(V, AtomOf('toJSON'), F) then begin PopRoots(1); Exit(False); end;
    if IsCallable(F) then
    begin
      if not CallValue(F, RootAt(TempTop - 1), nil, 0, X) then begin PopRoots(1); Exit(False); end;
      TempRoots[TempTop - 1] := X;
    end;
    V := RootAt(TempTop - 1);
    PopRoots(1);
  end;
  if IsString(V) then begin AOut_ := JSONQuote(StrToUnicode(StrOf(V))); Exit(True); end;
  if IsNumber(V) then
  begin
    if IsNaN(NumOf(V)) or IsInfinite(NumOf(V)) then AOut_ := 'null'
    else begin if not ToStringV(V, TS) then Exit(False); AOut_ := StrToUnicode(StrOf(TS)); end;
    Exit(True);
  end;
  case V of
    VNull: begin AOut_ := 'null'; Exit(True); end;
    VTrue: begin AOut_ := 'true'; Exit(True); end;
    VFalse: begin AOut_ := 'false'; Exit(True); end;
  end;
  if not IsObject(V) or IsCallable(V) then begin ASkip := True; Exit(True); end;
  PushRoot(V);
  if ObjOf(V)^.OKind = okArray then
  begin
    S := '[';
    N := ArrayLen(ObjOf(V));
    for I := 0 to N - 1 do
    begin
      if I > 0 then S := S + ',';
      X := ObjOf(RootAt(TempTop - 1))^.Elements^.Data[I];
      if X = VHole then X := VUndefined;
      if not JSONStr(X, AOut_, Sk, ADepth + 1) then begin PopRoots(1); Exit(False); end;
      if Sk then S := S + 'null' else S := S + AOut_;
    end;
    AOut_ := S + ']';
    PopRoots(1);
    Exit(True);
  end;
  Keys := OwnKeys(ObjOf(V), True);
  if OwnKeysFailed then begin OwnKeysFailed := False; PopRoots(1); Exit(False); end;
  PushRoot(Keys);
  S := '{';
  First := True;
  N := ArrayLen(ObjOf(Keys));
  for I := 0 to N - 1 do
  begin
    K := Atomize(StrOf(ObjOf(RootAt(TempTop - 1))^.Elements^.Data[I]));
    if not GetV(RootAt(TempTop - 2), K, X) then begin PopRoots(2); Exit(False); end;
    if not JSONStr(X, AOut_, Sk, ADepth + 1) then begin PopRoots(2); Exit(False); end;
    if Sk then Continue;
    if not First then S := S + ',';
    First := False;
    S := S + JSONQuote(StrToUnicode(K)) + ':' + AOut_;
  end;
  AOut_ := S + '}';
  PopRoots(2);
  Result := True;
end;

function JSON_Stringify(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var S: UnicodeString; Skip: Boolean;
begin
  if not JSONStr(Arg(AArgs, AArgc, 1), S, Skip, 0) then Exit(False);
  if Skip then ARes := VUndefined else ARes := StrFromUnicode(S);
  Result := True;
end;

type
  TJSONParser = record
    S: UnicodeString;
    P: Integer;
  end;

function JParse(var J: TJSONParser; var ARes: TValue): Boolean;
var Start: Integer; T: UnicodeString; O: PObj; V: TValue; K: PStr; C: WideChar; Code: Integer;
  procedure Ws; begin while (J.P <= Length(J.S)) and CharInSet(J.S[J.P], [' ', #9, #10, #13]) do Inc(J.P); end;
begin
  Ws;
  if J.P > Length(J.S) then Exit(ThrowError('SyntaxError', 'Unexpected end of JSON input'));
  C := J.S[J.P];
  case C of
    '{':
      begin
        Inc(J.P);
        O := NewPlainObject(ObjectProto, 4);
        PushRoot(MkPtr(O));
        Ws;
        if (J.P <= Length(J.S)) and (J.S[J.P] = '}') then begin Inc(J.P); Res := RootAt(TempTop - 1); PopRoots(1); Exit(True); end;
        while True do
        begin
          Ws;
          if not JParse(J, V) then begin PopRoots(1); Exit(False); end;
          if not IsString(V) then begin PopRoots(1); Exit(ThrowError('SyntaxError', 'Bad JSON key')); end;
          K := Atomize(StrOf(V));
          Ws;
          if (J.P > Length(J.S)) or (J.S[J.P] <> ':') then begin PopRoots(1); Exit(ThrowError('SyntaxError', 'Expected :')); end;
          Inc(J.P);
          if not JParse(J, V) then begin PopRoots(1); Exit(False); end;
          DefineOwn(RootAt(TempTop - 1), K, V, pfDefault);
          Ws;
          if (J.P <= Length(J.S)) and (J.S[J.P] = ',') then begin Inc(J.P); Continue; end;
          if (J.P <= Length(J.S)) and (J.S[J.P] = '}') then begin Inc(J.P); Break; end;
          PopRoots(1);
          Exit(ThrowError('SyntaxError', 'Bad JSON object'));
        end;
        Res := RootAt(TempTop - 1);
        PopRoots(1);
        Exit(True);
      end;
    '[':
      begin
        Inc(J.P);
        O := NewArray(0, 4);
        PushRoot(MkPtr(O));
        Ws;
        if (J.P <= Length(J.S)) and (J.S[J.P] = ']') then begin Inc(J.P); Res := RootAt(TempTop - 1); PopRoots(1); Exit(True); end;
        while True do
        begin
          if not JParse(J, V) then begin PopRoots(1); Exit(False); end;
          ArrayPush(ObjOf(RootAt(TempTop - 1)), V);
          Ws;
          if (J.P <= Length(J.S)) and (J.S[J.P] = ',') then begin Inc(J.P); Continue; end;
          if (J.P <= Length(J.S)) and (J.S[J.P] = ']') then begin Inc(J.P); Break; end;
          PopRoots(1);
          Exit(ThrowError('SyntaxError', 'Bad JSON array'));
        end;
        Res := RootAt(TempTop - 1);
        PopRoots(1);
        Exit(True);
      end;
    '"':
      begin
        Inc(J.P);
        T := '';
        while (J.P <= Length(J.S)) and (J.S[J.P] <> '"') do
        begin
          if J.S[J.P] = '\' then
          begin
            Inc(J.P);
            case J.S[J.P] of
              'n': T := T + #10; 't': T := T + #9; 'r': T := T + #13; 'b': T := T + #8; 'f': T := T + #12;
              'u': begin Code := StrToInt('$' + Copy(J.S, J.P + 1, 4)); T := T + WideChar(Code); Inc(J.P, 4); end;
            else T := T + J.S[J.P];
            end;
          end
          else T := T + J.S[J.P];
          Inc(J.P);
        end;
        Inc(J.P);
        Res := StrFromUnicode(T);
        Exit(True);
      end;
    't': begin Inc(J.P, 4); Res := VTrue; Exit(True); end;
    'f': begin Inc(J.P, 5); Res := VFalse; Exit(True); end;
    'n': begin Inc(J.P, 4); Res := VNull; Exit(True); end;
  else
    begin
      Start := J.P;
      while (J.P <= Length(J.S)) and CharInSet(J.S[J.P], ['0'..'9', '-', '+', '.', 'e', 'E']) do Inc(J.P);
      if J.P = Start then Exit(ThrowError('SyntaxError', 'Unexpected token in JSON'));
      Res := MkNumber(StringToNumber(Copy(J.S, Start, J.P - Start)));
      Exit(True);
    end;
  end;
end;

function JSON_Parse(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var S: PStr; J: TJSONParser;
begin
  if not StrArg(AArgs, AArgc, 1, S) then Exit(False);
  J.S := StrToUnicode(S);
  J.P := 1;
  Result := JParse(J, ARes);
end;

{ ---------------- Error ---------------- }

function MakeErrorCtor(AProto: TValue; AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var M: TValue; O: PObj; P: TValue;
begin
  if not ProtoFromNewTarget(AProto, P) then Exit(False);
  PushRoot(P);
  O := NewObject(RootShapeFor(P), 4, okError);
  PopRoots(1);
  PushRoot(MkPtr(O));
  if (AArgc >= 1) and (AArgs[1] <> VUndefined) then
  begin
    if not ToStringV(AArgs[1], M) then begin PopRoots(1); Exit(False); end;
    AddProperty(ObjOf(RootAt(TempTop - 1)), AtomMessage, M, pfHidden);
  end;
  ARes := RootAt(TempTop - 1);
  PopRoots(1);
  Result := True;
end;

function Error_Ctor(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
begin Result := MakeErrorCtor(ErrorProto, AArgs, AArgc, ARes); end;
function TypeError_Ctor(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
begin Result := MakeErrorCtor(TypeErrorProto, AArgs, AArgc, ARes); end;
function RangeError_Ctor(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
begin Result := MakeErrorCtor(RangeErrorProto, AArgs, AArgc, ARes); end;
function SyntaxError_Ctor(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
begin Result := MakeErrorCtor(SyntaxErrorProto, AArgs, AArgc, ARes); end;
function ReferenceError_Ctor(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
begin Result := MakeErrorCtor(ReferenceErrorProto, AArgs, AArgc, ARes); end;

function ErrProto_ToString(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var N, M, SN, SM: TValue;
begin
  if not GetV(AArgs[0], AtomName, N) then Exit(False);
  PushRoot(N);
  if not GetV(AArgs[0], AtomMessage, M) then begin PopRoots(1); Exit(False); end;
  PushRoot(M);
  if RootAt(TempTop - 2) = VUndefined then SN := StrFromUnicode('Error')
  else if not ToStringV(RootAt(TempTop - 2), SN) then begin PopRoots(2); Exit(False); end;
  if RootAt(TempTop - 1) = VUndefined then SM := MkPtr(AtomOf(''))
  else if not ToStringV(RootAt(TempTop - 1), SM) then begin PopRoots(2); Exit(False); end;
  PopRoots(2);
  if StrOf(SM)^.Len = 0 then ARes := SN
  else ARes := StrFromUnicode(StrToUnicode(StrOf(SN)) + ': ' + StrToUnicode(StrOf(SM)));
  Result := True;
end;

{ ---------------- Symbol ---------------- }

var
  SymbolCounter: Integer = 0;

function NewSymbol(const ADesc: UnicodeString): TValue;
var C: PByte;
begin
  C := GetMem(24);
  PCell(C)^.Size := 24;
  PCell(C)^.Kind := ckSymbol;
  PCell(C)^.Flags := cfPermanent or cfOld;
  PCell(C)^.Extra := 0;
  Inc(SymbolCounter);
  PValue(C + 8)^ := MkPtr(AtomOf(ADesc));
  PPointer(C + 16)^ := NewKeyAtom(ADesc, kfSymbol);
  Result := MkPtr(C);
  KeyPayload(PStr(PPointer(C + 16)^))^ := Result;   { symbols are permanent cells }
end;

function NewSymbolValue(const ADesc: UnicodeString): TValue;
begin
  Result := NewSymbol(ADesc);
end;

function Symbol_Ctor(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var S: PStr;
begin
  if CurNewTarget <> VUndefined then Exit(ThrowTypeError('Symbol is not a constructor'));
  if (AArgc >= 1) and (AArgs[1] <> VUndefined) then
  begin
    if not StrArg(AArgs, AArgc, 1, S) then Exit(False);
    ARes := NewSymbol(StrToUnicode(S));
  end
  else
  begin
    ARes := NewSymbol('');
    PCell(PtrOf(ARes))^.Extra := 1;   { [[Description]] undefined }
  end;
  Result := True;
end;

function SymProto_ToString(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
begin
  ARes := StrFromUnicode('Symbol(' + StrToUnicode(StrOf(PValue(PByte(PtrOf(AArgs[0])) + 8)^)) + ')');
  Result := True;
end;

{ ---------------- Map / Set (insertion-ordered, open addressing) ---------------- }

{ backing store: value array [count, deleted, k0, v0, k1, v1, ...] plus a separate index array of slots }
function MapEntries(M: TValue): PValueArray; inline;
begin
  Result := PValueArray(PtrOf(ObjOf(M)^.Aux1));
end;

function SameValueZero(A, B: TValue): Boolean;
begin
  if IsNumber(A) and IsNumber(B) then
    Exit((NumOf(A) = NumOf(B)) or (IsNaN(NumOf(A)) and IsNaN(NumOf(B))));
  Result := StrictEquals(A, B);
end;

{ the hash index (Aux2): a value array of ints
    [0] live count, [1] minor-GC epoch it was built at, [2] 1 when a key is hashed by address,
    [3] mask, [4..4+mask] entry number + 1 (0 = empty)
  objects move only in a minor collection, so an index holding address-hashed keys is rebuilt
  after one; until then lookups fall back to a scan, which never allocates }
const
  MIX_COUNT = 0; MIX_EPOCH = 1; MIX_ADDR = 2; MIX_MASK = 3; MIX_SLOTS = 4;

function MapIndex(M: TValue): PValueArray; inline;
begin
  Result := PValueArray(PtrOf(ObjOf(M)^.Aux2));
end;

function MapCount(M: TValue): Integer; inline;
begin
  Result := IntOf(MapIndex(M)^.Data[MIX_COUNT]);
end;

procedure SetMapCount(M: TValue; N: Integer); inline;
begin
  MapIndex(M)^.Data[MIX_COUNT] := MkInt(N);
end;

function KeyHash(K: TValue; var AByAddr: Boolean): UInt32;
var D: Double; Q: QWord;
begin
  AByAddr := False;
  if IsNumber(K) then
  begin
    D := NumOf(K);
    if D = 0 then Exit(0);
    if IsNaN(D) then Exit($7FF80001);
    Q := PQWord(@D)^;
    Q := Q xor (Q shr 31);
    Exit(UInt32(Q * QWord($9E3779B97F4A7C15) shr 32));
  end;
  if IsString(K) then Exit(StrHash(StrOf(K)));
  if IsPtr(K) then
  begin
    case PCell(PtrOf(K))^.Kind of
      9: Exit(9);   { BigInt: compared by value, one bucket }
      ckSymbol:;   { permanent: its address never changes }
    else AByAddr := True;
    end;
    Q := QWord(PtrUInt(PtrOf(K))) shr 3;
    Exit(UInt32(Q * QWord($9E3779B97F4A7C15) shr 32));
  end;
  Q := K;
  Result := UInt32((Q xor (Q shr 32)) * $9E3779B1);
end;

function MapIndexValid(M: TValue): Boolean; inline;
var Ix: PValueArray;
begin
  Ix := MapIndex(M);
  Result := (Ix^.Data[MIX_ADDR] = MkInt(0)) or (IntOf(Ix^.Data[MIX_EPOCH]) = Int32(GCMinorCount));
end;

{ never allocates: the index when it is current, a scan otherwise }
function MapFind(M: TValue; K: TValue): Integer;
var E, Ix: PValueArray; I, Mask, S: Integer; H: UInt32; ByAddr: Boolean;
begin
  E := MapEntries(M);
  if MapIndexValid(M) then
  begin
    Ix := MapIndex(M);
    Mask := IntOf(Ix^.Data[MIX_MASK]);
    H := KeyHash(K, ByAddr);
    I := Integer(H and UInt32(Mask));
    while True do
    begin
      S := IntOf(Ix^.Data[MIX_SLOTS + I]);
      if S = 0 then Exit(-1);
      if (E^.Data[(S - 1) * 2] <> VHole) and SameValueZero(E^.Data[(S - 1) * 2], K) then Exit((S - 1) * 2);
      I := (I + 1) and Mask;
    end;
  end;
  I := 0;
  while I < Integer(E^.Len) do
  begin
    if (E^.Data[I] <> VHole) and SameValueZero(E^.Data[I], K) then Exit(I);
    Inc(I, 2);
  end;
  Result := -1;
end;

{ entry number N (key at 2N) into an index with room }
procedure MapIndexInsert(AIx: PValueArray; E: PValueArray; N: Integer);
var Mask, I: Integer; H: UInt32; ByAddr: Boolean;
begin
  Mask := IntOf(AIx^.Data[MIX_MASK]);
  H := KeyHash(E^.Data[N * 2], ByAddr);
  if ByAddr then AIx^.Data[MIX_ADDR] := MkInt(1);
  I := Integer(H and UInt32(Mask));
  while AIx^.Data[MIX_SLOTS + I] <> MkInt(0) do I := (I + 1) and Mask;
  AIx^.Data[MIX_SLOTS + I] := MkInt(N + 1);
end;

{ a fresh index for M sized for Want entries; M stays rooted by the caller's stack slot or roots }
procedure MapRebuild(var M: TValue; AWant: Integer);
var Ix, E: PValueArray; Cap, I, N, Count: Integer;
begin
  Cap := 8;
  while Cap < AWant * 2 + 2 do Cap := Cap * 2;
  Count := MapCount(M);
  PushRoot(M);
  Ix := NewValueArray(MIX_SLOTS + Cap);
  M := RootAt(TempTop - 1);
  PopRoots(1);
  Ix^.Len := MIX_SLOTS + Cap;
  Ix^.Data[MIX_COUNT] := MkInt(Count);
  Ix^.Data[MIX_EPOCH] := MkInt(Int32(GCMinorCount));
  Ix^.Data[MIX_ADDR] := MkInt(0);
  Ix^.Data[MIX_MASK] := MkInt(Cap - 1);
  for I := 0 to Cap - 1 do Ix^.Data[MIX_SLOTS + I] := MkInt(0);
  E := MapEntries(M);
  N := Integer(E^.Len) div 2;
  for I := 0 to N - 1 do
    if E^.Data[I * 2] <> VHole then MapIndexInsert(Ix, E, I);
  ObjOf(M)^.Aux2 := MkPtr(Ix);
  WriteBarrier(@ObjOf(M)^.Hdr, MkPtr(Ix));
end;

{ called by the natives before they read their arguments: it may allocate }
procedure MapEnsureIndex(M: TValue);
begin
  if not MapIndexValid(M) then MapRebuild(M, Integer(MapEntries(M)^.Len) div 2);
end;

function NewMapLike(AProto: TValue; AKind: Byte): TValue;
var O: PObj; E, Ix: PValueArray; I: Integer;
begin
  PushRoot(AProto);
  E := NewValueArray(16);
  PushRoot(MkPtr(E));
  Ix := NewValueArray(MIX_SLOTS + 8);
  PushRoot(MkPtr(Ix));
  O := NewObject(RootShapeFor(RootAt(TempTop - 3)), 0, AKind);
  O^.Aux1 := RootAt(TempTop - 2);
  O^.Aux2 := RootAt(TempTop - 1);
  PopRoots(3);
  Ix := PValueArray(PtrOf(O^.Aux2));
  Ix^.Len := MIX_SLOTS + 8;
  Ix^.Data[MIX_COUNT] := MkInt(0);
  Ix^.Data[MIX_EPOCH] := MkInt(Int32(GCMinorCount));
  Ix^.Data[MIX_ADDR] := MkInt(0);
  Ix^.Data[MIX_MASK] := MkInt(7);
  for I := 0 to 7 do Ix^.Data[MIX_SLOTS + I] := MkInt(0);
  Result := MkPtr(O);
end;

procedure MapAppend(M, K, V: TValue);
var E, NE, Ix: PValueArray; N, Cap: Integer;
begin
  PushRoot(M); PushRoot(K); PushRoot(V);
  E := MapEntries(M);
  N := E^.Len;
  if N + 2 > Integer(E^.Cap) then
  begin
    NE := NewValueArray(E^.Cap * 2);
    M := RootAt(TempTop - 3);
    E := MapEntries(M);
    Move(E^.Data[0], NE^.Data[0], N * SizeOf(TValue));
    NE^.Len := N;
    ObjOf(M)^.Aux1 := MkPtr(NE);
    WriteBarrier(@ObjOf(M)^.Hdr, MkPtr(NE));
    if (NE^.Hdr.Flags and cfOld) <> 0 then Remember(@NE^.Hdr);
  end;
  { room in the index for one more entry, current for this GC epoch }
  Ix := MapIndex(RootAt(TempTop - 3));
  Cap := IntOf(Ix^.Data[MIX_MASK]) + 1;
  if not MapIndexValid(RootAt(TempTop - 3)) or ((N div 2 + 1) * 2 > Cap) then
  begin
    M := RootAt(TempTop - 3);
    MapRebuild(M, N div 2 + 1);
  end;
  M := RootAt(TempTop - 3); K := RootAt(TempTop - 2); V := RootAt(TempTop - 1);
  PopRoots(3);
  E := MapEntries(M);
  E^.Data[N] := K;
  E^.Data[N + 1] := V;
  WriteBarrier(@E^.Hdr, K);
  WriteBarrier(@E^.Hdr, V);
  E^.Len := N + 2;
  MapIndexInsert(MapIndex(M), E, N div 2);
  SetMapCount(M, MapCount(M) + 1);
end;

function Map_Ctor(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var M, It, V, K, X: TValue; Done: Boolean;
begin
  if CurNewTarget = VUndefined then Exit(ThrowTypeError('Constructor Map requires ''new'''));
  M := NewMapLike(MapProto, okMap);
  if (AArgc >= 1) and not IsNullish(AArgs[1]) then
  begin
    PushRoot(M);
    if not GetIterator(AArgs[1], It) then begin PopRoots(1); Exit(False); end;
    PushRoot(It);
    while True do
    begin
      if not IteratorStep(RootAt(TempTop - 1), V, Done) then begin PopRoots(2); Exit(False); end;
      if Done then Break;
      PushRoot(V);
      if not GetElemV(V, MkInt(0), K) then begin PopRoots(3); Exit(False); end;
      PushRoot(K);
      if not GetElemV(RootAt(TempTop - 2), MkInt(1), X) then begin PopRoots(4); Exit(False); end;
      K := RootAt(TempTop - 1);
      PopRoots(2);
      PushRoot(K); PushRoot(X);
      MapEnsureIndex(RootAt(TempTop - 4));
      X := RootAt(TempTop - 1); K := RootAt(TempTop - 2);
      PopRoots(2);
      if MapFind(RootAt(TempTop - 2), K) < 0 then MapAppend(RootAt(TempTop - 2), K, X);
    end;
    M := RootAt(TempTop - 2);
    PopRoots(2);
  end;
  ARes := M;
  Result := True;
end;

function Set_Ctor(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var M, It, V: TValue; Done: Boolean;
begin
  if CurNewTarget = VUndefined then Exit(ThrowTypeError('Constructor Set requires ''new'''));
  M := NewMapLike(SetProtoObj, okSet);
  if (AArgc >= 1) and not IsNullish(AArgs[1]) then
  begin
    PushRoot(M);
    if not GetIterator(AArgs[1], It) then begin PopRoots(1); Exit(False); end;
    PushRoot(It);
    while True do
    begin
      if not IteratorStep(RootAt(TempTop - 1), V, Done) then begin PopRoots(2); Exit(False); end;
      if Done then Break;
      PushRoot(V);
      MapEnsureIndex(RootAt(TempTop - 3));
      V := RootAt(TempTop - 1);
      PopRoots(1);
      if MapFind(RootAt(TempTop - 2), V) < 0 then MapAppend(RootAt(TempTop - 2), V, V);
    end;
    M := RootAt(TempTop - 2);
    PopRoots(2);
  end;
  ARes := M;
  Result := True;
end;

function ThisMap(AArgs: TArgs; AKind: Byte): Boolean; inline;
begin
  Result := IsObject(AArgs[0]) and (ObjOf(AArgs[0])^.OKind = AKind);
  if not Result then ThrowTypeError('incompatible receiver');
end;

function MapProto_Get(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var I: Integer;
begin
  if not ThisMap(AArgs, okMap) then Exit(False);
  MapEnsureIndex(AArgs[0]);
  I := MapFind(AArgs[0], Arg(AArgs, AArgc, 1));
  if I < 0 then ARes := VUndefined else ARes := MapEntries(AArgs[0])^.Data[I + 1];
  Result := True;
end;

function MapProto_Set(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var I: Integer; K: TValue;
begin
  if not ThisMap(AArgs, okMap) then Exit(False);
  MapEnsureIndex(AArgs[0]);
  K := Arg(AArgs, AArgc, 1);
  if IsNumber(K) then begin if NumOf(K) = 0 then K := MkInt(0) else K := MkNumber(NumOf(K)); end;
  I := MapFind(AArgs[0], K);
  if I >= 0 then ValueArrayStore(MapEntries(AArgs[0]), I + 1, Arg(AArgs, AArgc, 2))
  else MapAppend(AArgs[0], K, Arg(AArgs, AArgc, 2));
  ARes := AArgs[0];
  Result := True;
end;

function MapProto_Has(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
begin
  if not IsObject(AArgs[0]) or not (ObjOf(AArgs[0])^.OKind in [okMap, okSet]) then Exit(ThrowTypeError('incompatible receiver'));
  MapEnsureIndex(AArgs[0]);
  ARes := MkBool(MapFind(AArgs[0], Arg(AArgs, AArgc, 1)) >= 0);
  Result := True;
end;

function MapProto_Delete(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var I: Integer; E: PValueArray;
begin
  if not IsObject(AArgs[0]) or not (ObjOf(AArgs[0])^.OKind in [okMap, okSet]) then Exit(ThrowTypeError('incompatible receiver'));
  MapEnsureIndex(AArgs[0]);
  I := MapFind(AArgs[0], Arg(AArgs, AArgc, 1));
  if I < 0 then begin ARes := VFalse; Exit(True); end;
  E := MapEntries(AArgs[0]);
  E^.Data[I] := VHole;   { the index keeps pointing here: probes skip the hole }
  E^.Data[I + 1] := VUndefined;
  SetMapCount(AArgs[0], MapCount(AArgs[0]) - 1);
  ARes := VTrue;
  Result := True;
end;

function MapProto_Clear(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var E: PValueArray; I: Integer;
begin
  if not IsObject(AArgs[0]) or not (ObjOf(AArgs[0])^.OKind in [okMap, okSet]) then Exit(ThrowTypeError('incompatible receiver'));
  E := MapEntries(AArgs[0]);
  for I := 0 to Integer(E^.Len) - 1 do E^.Data[I] := VUndefined;
  E^.Len := 0;
  SetMapCount(AArgs[0], 0);
  MapRebuild(AArgs[0], 4);
  ARes := VUndefined;
  Result := True;
end;

function MapProto_Size(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
begin
  if not IsObject(AArgs[0]) or not (ObjOf(AArgs[0])^.OKind in [okMap, okSet]) then Exit(ThrowTypeError('incompatible receiver'));
  ARes := MkInt(MapCount(AArgs[0]));
  Result := True;
end;

function SetProto_Add(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var K: TValue;
begin
  if not ThisMap(AArgs, okSet) then Exit(False);
  MapEnsureIndex(AArgs[0]);
  K := Arg(AArgs, AArgc, 1);
  if IsNumber(K) then begin if NumOf(K) = 0 then K := MkInt(0) else K := MkNumber(NumOf(K)); end;
  if MapFind(AArgs[0], K) < 0 then MapAppend(AArgs[0], K, K);
  ARes := AArgs[0];
  Result := True;
end;

function MapProto_ForEach(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var I: Integer; E: PValueArray; CallArgs: array[0..2] of TValue; R: TValue;
begin
  if not IsObject(AArgs[0]) or not (ObjOf(AArgs[0])^.OKind in [okMap, okSet]) then Exit(ThrowTypeError('incompatible receiver'));
  I := 0;
  while True do
  begin
    E := MapEntries(AArgs[0]);
    if I >= Integer(E^.Len) then Break;
    if E^.Data[I] <> VHole then
    begin
      CallArgs[0] := E^.Data[I + 1]; CallArgs[1] := E^.Data[I]; CallArgs[2] := AArgs[0];
      if not CallValue(Arg(AArgs, AArgc, 1), Arg(AArgs, AArgc, 2), @CallArgs[0], 3, R) then Exit(False);
    end;
    Inc(I, 2);
  end;
  ARes := VUndefined;
  Result := True;
end;

{ Map/Set iterators: snapshot of the entries into an array, then an array iterator }
function MapIterKind(AArgs: TArgs; AKind: Integer; var ARes: TValue): Boolean;
var A, P: PObj; E: PValueArray; I, N: Integer; X: TValue;
begin
  if not IsObject(AArgs[0]) or not (ObjOf(AArgs[0])^.OKind in [okMap, okSet]) then Exit(ThrowTypeError('incompatible receiver'));
  A := NewArray(0, 8);
  PushRoot(MkPtr(A));
  I := 0;
  while True do
  begin
    E := MapEntries(AArgs[0]);
    if I >= Integer(E^.Len) then Break;
    if E^.Data[I] <> VHole then
    begin
      case AKind of
        0: X := E^.Data[I];
        1: X := E^.Data[I + 1];
      else
        begin
          P := NewArray(2, 2);
          E := MapEntries(AArgs[0]);
          P^.Elements^.Data[0] := E^.Data[I];
          P^.Elements^.Data[1] := E^.Data[I + 1];
          X := MkPtr(P);
        end;
      end;
      ArrayPush(ObjOf(RootAt(TempTop - 1)), X);
    end;
    Inc(I, 2);
  end;
  Result := GetIterator(RootAt(TempTop - 1), ARes);
  PopRoots(1);
end;

function MapProto_Keys(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean; begin Result := MapIterKind(AArgs, 0, ARes); end;
function MapProto_Values(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean; begin Result := MapIterKind(AArgs, 1, ARes); end;
function MapProto_Entries(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean; begin Result := MapIterKind(AArgs, 2, ARes); end;

{ ---------------- setup ---------------- }

function NewProtoObject(AProto: TValue): TValue;
begin
  Result := MkPtr(NewPlainObject(AProto, 8));
end;

function MakeCtor(AFn: TNativeFn; const AName: UnicodeString; ALength: Integer; AProto: TValue): TValue;
var C: TValue;
begin
  PushRoot(AProto);
  C := NewNativeFunction(AFn, AName, ALength);
  PushRoot(C);
  DefineOwn(C, AtomPrototype, RootAt(TempTop - 2), 0);
  DefineOwn(RootAt(TempTop - 2), AtomConstructor, RootAt(TempTop - 1), pfHidden);
  DefineOwn(GlobalObj, AtomOf(AName), RootAt(TempTop - 1), pfHidden);
  Result := RootAt(TempTop - 1);
  PopRoots(2);
end;

procedure DefineGetter(O: TValue; const AName: UnicodeString; AFn: TNativeFn);
var F: TValue;
begin
  PushRoot(O);
  F := NewNativeFunction(AFn, 'get ' + AName, 0);
  F := MkPtr(NewAccessor(F, VUndefined));
  DefineOwn(RootAt(TempTop - 1), AtomOf(AName), F, pfConfigurable or pfAccessor);
  PopRoots(1);
end;

threadvar
  MathObj, JSONObj, ConsoleObj, PerfObj, DateObj, CtorTmp, ArrayValuesFnRoot: TValue;

procedure InitBuiltins;
var C, P, G: TValue;
  procedure N(O: TValue; const AName: UnicodeString; AFn: TNativeFn; ALen: Integer);
  begin
    DefineNative(O, AName, AFn, ALen);
  end;
  function ErrorType(const AName: UnicodeString; AFn: TNativeFn; AParent: TValue): TValue;
  var EP, Ctor: TValue;
  begin
    EP := NewProtoObject(AParent);
    CtorTmp := EP;
    DefineHidden(CtorTmp, 'name', MkPtr(AtomOf(AName)));
    DefineHidden(CtorTmp, 'message', MkPtr(AtomOf('')));
    Ctor := MakeCtor(AFn, AName, 1, CtorTmp);
    Result := CtorTmp;
  end;
begin
  AddRoot(@ObjectProto); AddRoot(@FunctionProto); AddRoot(@ArrayProto); AddRoot(@StringProto);
  AddRoot(@NumberProto); AddRoot(@BooleanProto); AddRoot(@ErrorProto); AddRoot(@IteratorProto);
  AddRoot(@ArrayIteratorProto); AddRoot(@GeneratorProto); AddRoot(@MapProto); AddRoot(@SetProtoObj);
  AddRoot(@SymbolProto); AddRoot(@TypeErrorProto); AddRoot(@RangeErrorProto); AddRoot(@SyntaxErrorProto);
  AddRoot(@ReferenceErrorProto); AddRoot(@MathObj); AddRoot(@JSONObj); AddRoot(@ConsoleObj); AddRoot(@PerfObj);
  AddRoot(@DateObj); AddRoot(@CtorTmp); AddRoot(@ErrorProtoType); AddRoot(@SymIterator);

  ObjectProto := MkPtr(NewPlainObject(VNull, 16));
  { 20.2.3: %Function.prototype% is itself a built-in function: it accepts any arguments and returns undefined;
    length 0, name the empty string, not a constructor }
  SetFunctionProto(ObjectProto);
  FunctionProto := NewNativeFunction(FuncProto_Empty, '', 0);
  SetFunctionProto(FunctionProto);
  GlobalObj := MkPtr(NewPlainObject(ObjectProto, 64));
  EmptyShapeForArray := RootShapeFor(ObjectProto);
  ArrayProto := MkPtr(NewArray(0, 0));
  EmptyShapeForArray := RootShapeFor(ArrayProto);
  StringProto := NewProtoObject(ObjectProto);
  NumberProto := NewProtoObject(ObjectProto);
  BooleanProto := NewProtoObject(ObjectProto);
  { 22.1.3, 21.1.3, 20.3.3: the prototypes are a String, Number and Boolean object themselves ("", +0, false) }
  ObjOf(StringProto)^.OKind := okStringObj; ObjOf(StringProto)^.Aux1 := MkPtr(AtomOf(''));
  ObjOf(NumberProto)^.OKind := okNumber; ObjOf(NumberProto)^.Aux1 := MkInt(0);
  ObjOf(BooleanProto)^.OKind := okBoolean; ObjOf(BooleanProto)^.Aux1 := VFalse;
  SymbolProto := NewProtoObject(ObjectProto);
  IteratorProto := NewProtoObject(ObjectProto);
  ArrayIteratorProto := NewProtoObject(IteratorProto);
  MapProto := NewProtoObject(ObjectProto);
  SetProtoObj := NewProtoObject(ObjectProto);
  ErrorProto := NewProtoObject(ObjectProto);

  SymIterator := NewSymbol('Symbol.iterator');
  AddRoot(@SymHasInstance);
  SymHasInstance := NewSymbol('Symbol.hasInstance');
  { the other well-known symbols (6.1.5.1), made once; symbols are permanent cells }
  SymAsyncIterator := NewSymbol('Symbol.asyncIterator');
  SymSpecies := NewSymbol('Symbol.species');
  SymToStringTagW := NewSymbol('Symbol.toStringTag');
  SymToPrimitive := NewSymbol('Symbol.toPrimitive');
  SymIsConcatSpreadable := NewSymbol('Symbol.isConcatSpreadable');
  SymMatch := NewSymbol('Symbol.match');
  SymMatchAll := NewSymbol('Symbol.matchAll');
  SymReplace := NewSymbol('Symbol.replace');
  SymSearch := NewSymbol('Symbol.search');
  SymSplit := NewSymbol('Symbol.split');
  SymUnscopables := NewSymbol('Symbol.unscopables');
  SymDispose := NewSymbol('Symbol.dispose');
  SymAsyncDispose := NewSymbol('Symbol.asyncDispose');
  ToPrimitiveKey := SymKey(SymToPrimitive);

  { Object }
  C := MakeCtor(Object_Ctor, 'Object', 1, ObjectProto);
  CtorTmp := C;
  N(CtorTmp, 'create', Object_Create, 2);
  N(CtorTmp, 'keys', Object_Keys, 1);
  N(CtorTmp, 'values', Object_Values, 1);
  N(CtorTmp, 'entries', Object_Entries, 1);
  N(CtorTmp, 'assign', Object_Assign, 2);
  N(CtorTmp, 'freeze', Object_Freeze, 1);
  N(CtorTmp, 'seal', Object_Freeze, 1);
  N(CtorTmp, 'preventExtensions', Object_Freeze, 1);
  N(CtorTmp, 'getPrototypeOf', Object_GetPrototypeOf, 1);
  N(CtorTmp, 'setPrototypeOf', Object_SetPrototypeOf, 2);
  N(CtorTmp, 'defineProperty', Object_DefineProperty, 3);
  N(CtorTmp, 'getOwnPropertyNames', Object_GetOwnPropertyNames, 1);
  N(ObjectProto, 'hasOwnProperty', ObjProto_HasOwnProperty, 1);
  N(ObjectProto, 'toString', ObjProto_ToString, 0);
  N(ObjectProto, 'toLocaleString', ObjProto_ToString, 0);
  N(ObjectProto, 'valueOf', ObjProto_ValueOf, 0);
  N(ObjectProto, 'isPrototypeOf', ObjProto_IsPrototypeOf, 1);

  { Function }
  C := MakeCtor(Object_Ctor, 'Function', 1, FunctionProto);
  N(FunctionProto, 'call', FuncProto_Call, 1);
  N(FunctionProto, 'apply', FuncProto_Apply, 2);
  N(FunctionProto, 'bind', FuncProto_Bind, 1);
  N(FunctionProto, 'toString', FuncProto_ToString, 0);

  { Array }
  C := MakeCtor(Array_Ctor, 'Array', 1, ArrayProto);
  CtorTmp := C;
  N(CtorTmp, 'isArray', Array_IsArray, 1);
  N(CtorTmp, 'of', Array_Of, 0);
  N(CtorTmp, 'from', Array_From, 1);
  N(ArrayProto, 'push', ArrProto_Push, 1);
  N(ArrayProto, 'pop', ArrProto_Pop, 0);
  N(ArrayProto, 'shift', ArrProto_Shift, 0);
  N(ArrayProto, 'unshift', ArrProto_Unshift, 1);
  N(ArrayProto, 'slice', ArrProto_Slice, 2);
  N(ArrayProto, 'splice', ArrProto_Splice, 2);
  N(ArrayProto, 'concat', ArrProto_Concat, 1);
  N(ArrayProto, 'join', ArrProto_Join, 1);
  N(ArrayProto, 'toString', ArrProto_ToString, 0);
  N(ArrayProto, 'reverse', ArrProto_Reverse, 0);
  N(ArrayProto, 'indexOf', ArrProto_IndexOf, 1);
  N(ArrayProto, 'lastIndexOf', ArrProto_LastIndexOf, 1);
  N(ArrayProto, 'includes', ArrProto_Includes, 1);
  N(ArrayProto, 'fill', ArrProto_Fill, 1);
  N(ArrayProto, 'forEach', ArrProto_ForEach, 1);
  N(ArrayProto, 'map', ArrProto_Map, 1);
  N(ArrayProto, 'filter', ArrProto_Filter, 1);
  N(ArrayProto, 'some', ArrProto_Some, 1);
  N(ArrayProto, 'every', ArrProto_Every, 1);
  N(ArrayProto, 'find', ArrProto_Find, 1);
  N(ArrayProto, 'findIndex', ArrProto_FindIndex, 1);
  N(ArrayProto, 'findLast', ArrProto_FindLast, 1);
  N(ArrayProto, 'findLastIndex', ArrProto_FindLastIndex, 1);
  N(ArrayProto, 'reduce', ArrProto_Reduce, 1);
  N(ArrayProto, 'reduceRight', ArrProto_ReduceRight, 1);
  N(ArrayProto, 'sort', ArrProto_Sort, 1);
  G := NewNativeFunction(ArrProto_Values, 'values', 0);
  ArrayValuesFnRoot := G;
  ArrayValuesFnPublic := G;
  AddRoot(@ArrayValuesFnPublic);
  { 23.1.3.44: Array.prototype[@@iterator] is the values function object itself }
  DefineOwn(ArrayProto, AtomOf('values'), ArrayValuesFnPublic, pfHidden);   { the rooted copy: a define can collect }
  DefineOwn(ArrayProto, PStr(PPointer(PByte(PtrOf(SymIterator)) + 16)^), ArrayValuesFnPublic, pfHidden);
  N(ArrayIteratorProto, 'next', ArrIter_Next, 0);
  AddRoot(@ArrayIterNextFn);
  GetV(ArrayIteratorProto, AtomNext, ArrayIterNextFn);
  G := NewNativeFunction(Iter_Self, '[Symbol.iterator]', 0);
  DefineOwn(IteratorProto, PStr(PPointer(PByte(PtrOf(SymIterator)) + 16)^), G, pfHidden);

  { String }
  C := MakeCtor(String_Ctor, 'String', 1, StringProto);
  CtorTmp := C;
  N(CtorTmp, 'fromCharCode', String_FromCharCode, 1);
  N(StringProto, 'charAt', StrProto_CharAt, 1);
  N(StringProto, 'charCodeAt', StrProto_CharCodeAt, 1);
  N(StringProto, 'codePointAt', StrProto_CodePointAt, 1);
  N(StringProto, 'indexOf', StrProto_IndexOf, 1);
  N(StringProto, 'lastIndexOf', StrProto_LastIndexOf, 1);
  N(StringProto, 'includes', StrProto_Includes, 1);
  N(StringProto, 'startsWith', StrProto_StartsWith, 1);
  N(StringProto, 'endsWith', StrProto_EndsWith, 1);
  N(StringProto, 'slice', StrProto_Slice, 2);
  N(StringProto, 'substring', StrProto_Substring, 2);
  N(StringProto, 'substr', StrProto_Substr, 2);
  N(StringProto, 'toUpperCase', StrProto_ToUpper, 0);
  N(StringProto, 'toLowerCase', StrProto_ToLower, 0);
  N(StringProto, 'trim', StrProto_Trim, 0);
  N(StringProto, 'trimStart', StrProto_TrimStart, 0);
  N(StringProto, 'trimLeft', StrProto_TrimStart, 0);
  N(StringProto, 'trimEnd', StrProto_TrimEnd, 0);
  N(StringProto, 'trimRight', StrProto_TrimEnd, 0);
  N(StringProto, 'at', StrProto_At, 1);
  N(StringProto, 'normalize', StrProto_Normalize, 0);
  N(StringProto, 'toLocaleUpperCase', StrProto_ToUpper, 0);
  N(StringProto, 'toLocaleLowerCase', StrProto_ToLower, 0);
  N(StringProto, 'repeat', StrProto_Repeat, 1);
  N(StringProto, 'padStart', StrProto_PadStart, 2);
  N(StringProto, 'padEnd', StrProto_PadEnd, 2);
  N(StringProto, 'split', StrProto_Split, 2);
  N(StringProto, 'replace', StrProto_ReplaceOne, 2);
  N(StringProto, 'replaceAll', StrProto_ReplaceAll, 2);
  N(StringProto, 'concat', StrProto_Concat, 1);
  N(StringProto, 'toString', StrProto_ToString, 0);
  N(StringProto, 'valueOf', StrProto_ToString, 0);
  N(StringProto, 'localeCompare', StrProto_LocaleCompare, 1);
  G := NewNativeFunction(ArrProto_Values, '[Symbol.iterator]', 0);
  DefineOwn(StringProto, PStr(PPointer(PByte(PtrOf(SymIterator)) + 16)^), G, pfHidden);

  { Number, Boolean, Symbol }
  C := MakeCtor(Number_Ctor, 'Number', 1, NumberProto);
  CtorTmp := C;
  N(CtorTmp, 'isInteger', Number_IsInteger, 1);
  N(CtorTmp, 'isFinite', G_IsFinite, 1);
  N(CtorTmp, 'isNaN', G_IsNaN, 1);
  N(CtorTmp, 'parseFloat', G_ParseFloat, 1);
  N(CtorTmp, 'parseInt', G_ParseInt, 2);
  DefineHidden(CtorTmp, 'MAX_SAFE_INTEGER', MkDouble(9007199254740991.0));
  DefineHidden(CtorTmp, 'MIN_SAFE_INTEGER', MkDouble(-9007199254740991.0));
  DefineHidden(CtorTmp, 'MAX_VALUE', MkDouble(MaxDouble));
  DefineHidden(CtorTmp, 'EPSILON', MkDouble(2.220446049250313e-16));
  DefineHidden(CtorTmp, 'POSITIVE_INFINITY', MkDouble(Infinity));
  DefineHidden(CtorTmp, 'NEGATIVE_INFINITY', MkDouble(NegInfinity));
  DefineHidden(CtorTmp, 'NaN', MkDouble(NaN));
  N(NumberProto, 'toString', NumProto_ToString, 1);
  N(NumberProto, 'toFixed', NumProto_ToFixed, 1);
  N(NumberProto, 'valueOf', NumProto_ValueOf, 0);
  C := MakeCtor(Boolean_Ctor, 'Boolean', 1, BooleanProto);
  C := MakeCtor(Symbol_Ctor, 'Symbol', 0, SymbolProto);
  CtorTmp := C;
  DefineOwn(CtorTmp, AtomOf('iterator'), SymIterator, 0);
  DefineOwn(CtorTmp, AtomOf('hasInstance'), SymHasInstance, 0);
  N(SymbolProto, 'toString', SymProto_ToString, 0);

  { Map, Set }
  C := MakeCtor(Map_Ctor, 'Map', 0, MapProto);
  N(MapProto, 'get', MapProto_Get, 1);
  N(MapProto, 'set', MapProto_Set, 2);
  N(MapProto, 'has', MapProto_Has, 1);
  N(MapProto, 'delete', MapProto_Delete, 1);
  N(MapProto, 'clear', MapProto_Clear, 0);
  N(MapProto, 'forEach', MapProto_ForEach, 1);
  N(MapProto, 'keys', MapProto_Keys, 0);
  N(MapProto, 'values', MapProto_Values, 0);
  N(MapProto, 'entries', MapProto_Entries, 0);
  DefineGetter(MapProto, 'size', MapProto_Size);
  G := NewNativeFunction(MapProto_Entries, 'entries', 0);
  DefineOwn(MapProto, PStr(PPointer(PByte(PtrOf(SymIterator)) + 16)^), G, pfHidden);
  C := MakeCtor(Set_Ctor, 'Set', 0, SetProtoObj);
  N(SetProtoObj, 'add', SetProto_Add, 1);
  N(SetProtoObj, 'has', MapProto_Has, 1);
  N(SetProtoObj, 'delete', MapProto_Delete, 1);
  N(SetProtoObj, 'clear', MapProto_Clear, 0);
  N(SetProtoObj, 'forEach', MapProto_ForEach, 1);
  N(SetProtoObj, 'values', MapProto_Keys, 0);
  N(SetProtoObj, 'keys', MapProto_Keys, 0);
  N(SetProtoObj, 'entries', MapProto_Entries, 0);
  DefineGetter(SetProtoObj, 'size', MapProto_Size);
  G := NewNativeFunction(MapProto_Keys, 'values', 0);
  DefineOwn(SetProtoObj, PStr(PPointer(PByte(PtrOf(SymIterator)) + 16)^), G, pfHidden);

  { Errors }
  DefineHidden(ErrorProto, 'name', MkPtr(AtomOf('Error')));
  DefineHidden(ErrorProto, 'message', MkPtr(AtomOf('')));
  N(ErrorProto, 'toString', ErrProto_ToString, 0);
  C := MakeCtor(Error_Ctor, 'Error', 1, ErrorProto);
  TypeErrorProto := ErrorType('TypeError', TypeError_Ctor, ErrorProto);
  RangeErrorProto := ErrorType('RangeError', RangeError_Ctor, ErrorProto);
  SyntaxErrorProto := ErrorType('SyntaxError', SyntaxError_Ctor, ErrorProto);
  ReferenceErrorProto := ErrorType('ReferenceError', ReferenceError_Ctor, ErrorProto);

  { Math }
  MathObj := NewProtoObject(ObjectProto);
  DefineHidden(GlobalObj, 'Math', MathObj);
  N(MathObj, 'abs', M_Abs, 1); N(MathObj, 'floor', M_Floor, 1); N(MathObj, 'ceil', M_Ceil, 1);
  N(MathObj, 'round', M_Round, 1); N(MathObj, 'sqrt', M_Sqrt, 1); N(MathObj, 'sin', M_Sin, 1);
  N(MathObj, 'cos', M_Cos, 1); N(MathObj, 'tan', M_Tan, 1); N(MathObj, 'atan', M_Atan, 1);
  N(MathObj, 'asin', M_Asin, 1); N(MathObj, 'acos', M_Acos, 1);
  N(MathObj, 'exp', M_Exp, 1); N(MathObj, 'log', M_Log, 1); N(MathObj, 'log2', M_Log2, 1);
  N(MathObj, 'log10', M_Log10, 1); N(MathObj, 'cbrt', M_Cbrt, 1);
  N(MathObj, 'trunc', M_Trunc, 1); N(MathObj, 'sign', M_Sign, 1); N(MathObj, 'max', M_Max, 2);
  N(MathObj, 'min', M_Min, 2); N(MathObj, 'pow', M_Pow, 2); N(MathObj, 'atan2', M_Atan2, 2);
  N(MathObj, 'hypot', M_Hypot, 2); N(MathObj, 'random', M_Random, 0);
  DefineHidden(MathObj, 'PI', MkDouble(Pi));
  DefineHidden(MathObj, 'E', MkDouble(Exp(1)));
  DefineHidden(MathObj, 'LN2', MkDouble(Ln(2)));
  DefineHidden(MathObj, 'LN10', MkDouble(Ln(10)));
  DefineHidden(MathObj, 'SQRT2', MkDouble(Sqrt(2)));

  { JSON, console, performance, Date }
  JSONObj := NewProtoObject(ObjectProto);
  DefineHidden(GlobalObj, 'JSON', JSONObj);
  N(JSONObj, 'stringify', JSON_Stringify, 3);
  N(JSONObj, 'parse', JSON_Parse, 2);
  ConsoleObj := NewProtoObject(ObjectProto);
  DefineHidden(GlobalObj, 'console', ConsoleObj);
  N(ConsoleObj, 'log', Console_Log, 0);
  N(ConsoleObj, 'error', Console_Log, 0);
  N(ConsoleObj, 'warn', Console_Log, 0);
  PerfObj := NewProtoObject(ObjectProto);
  DefineHidden(GlobalObj, 'performance', PerfObj);
  N(PerfObj, 'now', Perf_Now, 0);
  DateObj := NewProtoObject(ObjectProto);
  DefineHidden(GlobalObj, 'Date', DateObj);
  N(DateObj, 'now', Date_Now, 0);

  { globals }
  N(GlobalObj, 'parseInt', G_ParseInt, 2);
  N(GlobalObj, 'parseFloat', G_ParseFloat, 1);
  N(GlobalObj, 'isNaN', G_IsNaN, 1);
  N(GlobalObj, 'isFinite', G_IsFinite, 1);
  DefineHidden(GlobalObj, 'globalThis', GlobalObj);
  N(GlobalObj, 'print', Console_Log, 0);
end;

end.
