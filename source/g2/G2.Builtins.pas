unit G2.Builtins;
{ The built-in objects the benchmark corpora use. }
{$mode delphiunicode}{$H+}
{$MINFPCONSTPREC 64}
{$POINTERMATH ON}
{$Q-}{$R-}

interface

uses
  SysUtils, Math, DateUtils, G2.Core, G2.Bytecode, G2.VM;

procedure InitBuiltins;
var
  { G2.Intl's full Unicode case mapping (Locale '' the root's) and normalization (Form NFC, NFD, NFKC or NFKD) }
  CaseMapHook: function(const S: UnicodeString; const Locale: string; Upper: Boolean; var R: UnicodeString): Boolean = nil;
  NormalizeHook: function(const S, Form: UnicodeString; var R: UnicodeString): Boolean = nil;
function NewSymbolValue(const Desc: UnicodeString): TValue;

threadvar
  { set by an embedder (G2.Engine) to take this thread's console output; nil writes to stdout }
  ConsoleSink: procedure(const Line: UnicodeString);

implementation

uses
  NumericText, Linux, UnixType, G2.ObjModel, BigInteger, G2.BigIntV;

type
  TArgs = PValue;

function Arg(Args: TArgs; Argc, I: Integer): TValue; inline;
begin
  if I <= Argc then Result := Args[I] else Result := VUndefined;
end;

function ThisObj(Args: TArgs): PObj; inline;
begin
  Result := ObjOf(Args[0]);
end;

function NewObjWithProto(Proto: TValue): TValue;
begin
  Result := MkPtr(NewPlainObject(Proto, 4));
end;

function StrArg(Args: TArgs; Argc, I: Integer; var S: PStr): Boolean;
var V: TValue;
begin
  if not ToStringV(Arg(Args, Argc, I), V) then Exit(False);
  S := StrOf(V);
  Result := True;
end;

function NumArg(Args: TArgs; Argc, I: Integer; var D: Double): Boolean; inline;
var V: TValue;
begin
  V := Arg(Args, Argc, I);
  if IsInt(V) then begin D := IntOf(V); Exit(True); end;
  if IsDouble(V) then begin D := DoubleOf(V); Exit(True); end;
  Result := ToNumberV(V, D);
end;

function IntArg(Args: TArgs; Argc, I: Integer; Default: Int64; var R: Int64): Boolean;
var D: Double;
begin
  if (I > Argc) or (Args[I] = VUndefined) then begin R := Default; Exit(True); end;
  if not NumArg(Args, Argc, I, D) then Exit(False);
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

function G_ParseInt(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var S: PStr; T: UnicodeString; Radix: Int64; I, Sign, D: Integer; Acc: Double; Any: Boolean;
begin
  if not StrArg(Args, Argc, 1, S) then Exit(False);
  if not IntArg(Args, Argc, 2, 0, Radix) then Exit(False);
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
  if not Any then Res := MkDouble(NaN) else Res := MkNumber(Sign * Acc);
  Result := True;
end;

function G_ParseFloat(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var S: PStr; T: UnicodeString; I: Integer;
begin
  if not StrArg(Args, Argc, 1, S) then Exit(False);
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
  if (T = '') or (T = '-') or (T = '+') then Res := MkDouble(NaN) else Res := MkNumber(StringToNumber(T));
  Result := True;
end;

function G_IsNaN(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var D: Double;
begin
  if not NumArg(Args, Argc, 1, D) then Exit(False);
  Res := MkBool(IsNaN(D));
  Result := True;
end;

function G_IsFinite(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var D: Double;
begin
  if not NumArg(Args, Argc, 1, D) then Exit(False);
  Res := MkBool(not IsNaN(D) and not IsInfinite(D));
  Result := True;
end;

{ ---------------- console / performance ---------------- }

function Console_Log(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var I: Integer; S: TValue; Line: UnicodeString;
begin
  Line := '';
  for I := 1 to Argc do
  begin
    if not ToStringV(Args[I], S) then Exit(False);
    if I > 1 then Line := Line + ' ';
    Line := Line + StrToUnicode(StrOf(S));
  end;
  if Assigned(ConsoleSink) then ConsoleSink(Line) else WriteLn(UTF8Encode(Line));
  Res := VUndefined;
  Result := True;
end;

function Perf_Now(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var TS: TTimeSpec;
begin
  clock_gettime(CLOCK_MONOTONIC, @TS);
  Res := MkDouble(TS.tv_sec * 1000.0 + TS.tv_nsec / 1e6);
  Result := True;
end;

function Date_Now(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
begin
  Res := MkNumber(Trunc(DateTimeToUnix(Now) * 1000.0));
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

function Object_Ctor(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var V: TValue;
begin
  V := Arg(Args, Argc, 1);
  if IsObject(V) then Res := V
  else if IsNullish(V) then Res := NewObjWithProto(ObjectProto)
  else Res := WrapPrimitive(V);
  Result := True;
end;

function Object_Create(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var P, F, R: TValue; CallArgs: array[0..1] of TValue;
begin
  P := Arg(Args, Argc, 1);
  if not (IsObject(P) or (P = VNull)) then Exit(ThrowTypeError('Object prototype may only be an Object or null'));
  Res := NewObjWithProto(P);
  if Arg(Args, Argc, 2) <> VUndefined then
  begin
    { the properties argument: Object.defineProperties(O, Properties) }
    PushRoot(Res);
    if not GetV(GlobalObj, AtomOf('Object'), F) then begin PopRoots(1); Exit(False); end;
    if not GetV(F, AtomOf('defineProperties'), F) then begin PopRoots(1); Exit(False); end;
    CallArgs[0] := RootAt(TempTop - 1); CallArgs[1] := Args[2];
    if not CallValue(F, VUndefined, @CallArgs[0], 2, R) then begin PopRoots(1); Exit(False); end;
    Res := RootAt(TempTop - 1);
    PopRoots(1);
  end;
  Result := True;
end;

function Object_Keys(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var V: TValue;
begin
  V := Arg(Args, Argc, 1);
  if not IsObject(V) then begin Res := MkPtr(NewArray(0, 0)); Exit(True); end;
  Res := OwnKeys(ObjOf(V), True);
  Result := True;
end;

function Object_Values(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var V, Keys, X: TValue; I, N: Integer; A: PObj;
begin
  V := Arg(Args, Argc, 1);
  if not IsObject(V) then begin Res := MkPtr(NewArray(0, 0)); Exit(True); end;
  Keys := OwnKeys(ObjOf(V), True);
  PushRoot(Keys);
  N := ArrayLen(ObjOf(Keys));
  A := NewArray(0, N);
  PushRoot(MkPtr(A));
  for I := 0 to N - 1 do
  begin
    if not GetV(Args[1], Atomize(StrOf(ObjOf(RootAt(TempTop - 2))^.Elements^.Data[I])), X) then begin PopRoots(2); Exit(False); end;
    ArrayPush(ObjOf(RootAt(TempTop - 1)), X);
  end;
  Res := RootAt(TempTop - 1);
  PopRoots(2);
  Result := True;
end;

function Object_Entries(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var V, Keys, X: TValue; I, N: Integer; A, P: PObj;
begin
  V := Arg(Args, Argc, 1);
  if not IsObject(V) then begin Res := MkPtr(NewArray(0, 0)); Exit(True); end;
  Keys := OwnKeys(ObjOf(V), True);
  PushRoot(Keys);
  N := ArrayLen(ObjOf(Keys));
  A := NewArray(0, N);
  PushRoot(MkPtr(A));
  for I := 0 to N - 1 do
  begin
    if not GetV(Args[1], Atomize(StrOf(ObjOf(RootAt(TempTop - 2))^.Elements^.Data[I])), X) then begin PopRoots(2); Exit(False); end;
    PushRoot(X);
    P := NewArray(2, 2);
    P^.Elements^.Data[0] := ObjOf(RootAt(TempTop - 3))^.Elements^.Data[I];
    P^.Elements^.Data[1] := RootAt(TempTop - 1);
    PopRoots(1);
    ArrayPush(ObjOf(RootAt(TempTop - 1)), MkPtr(P));
  end;
  Res := RootAt(TempTop - 1);
  PopRoots(2);
  Result := True;
end;

function Object_Assign(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var I, J, N: Integer; Keys, V: TValue; K: PStr;
begin
  for I := 2 to Argc do
  begin
    if not IsObject(Args[I]) then Continue;
    Keys := OwnKeys(ObjOf(Args[I]), True);
    PushRoot(Keys);
    N := ArrayLen(ObjOf(Keys));
    for J := 0 to N - 1 do
    begin
      K := Atomize(StrOf(ObjOf(RootAt(TempTop - 1))^.Elements^.Data[J]));
      if not GetV(Args[I], K, V) then begin PopRoots(1); Exit(False); end;
      if not SetV(Args[1], K, V) then begin PopRoots(1); Exit(False); end;
    end;
    PopRoots(1);
  end;
  Res := Args[1];
  Result := True;
end;

function Object_Freeze(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
begin
  Res := Arg(Args, Argc, 1);
  if IsObject(Res) then ObjOf(Res)^.OFlags := ObjOf(Res)^.OFlags or 8;
  Result := True;
end;

function Object_GetPrototypeOf(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var V: TValue;
begin
  V := Arg(Args, Argc, 1);
  if IsObject(V) then Res := ObjOf(V)^.Shape^.Proto
  else if IsString(V) then Res := StringProto
  else if IsNumber(V) then Res := NumberProto
  else Res := VNull;
  Result := True;
end;

function Object_SetPrototypeOf(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
begin
  Res := Arg(Args, Argc, 1);
  if IsObject(Res) then SetProto(ObjOf(Res), Arg(Args, Argc, 2));
  Result := True;
end;

function DescHas(D: TValue; Key: PStr): Boolean;
var P: TValue;
begin
  P := D;
  while IsObject(P) do
  begin
    if ShapeLookup(ObjOf(P)^.Shape, Key) >= 0 then Exit(True);
    P := ObjOf(P)^.Shape^.Proto;
  end;
  Result := False;
end;

{ ValidateAndApplyPropertyDescriptor without the non-configurable checks: absent fields keep the
  existing property's attributes and value }
function Object_DefineProperty(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var D, V, G, S, Tmp, OldV: TValue; K: PStr; Flags, OldFlags: Byte; Slot: Integer; Exists: Boolean;
  KEnum, KConf, KWrit, KValue, KGet, KSet: PStr;
  procedure Pick(Has: Boolean; Bit: Byte; Val: TValue);
  begin
    if Has then
    begin
      if ToBool(Val) then Flags := Flags or Bit;
    end
    else if Exists then Flags := Flags or (OldFlags and Bit);
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

function Object_GetOwnPropertyNames(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var V: TValue;
begin
  V := Arg(Args, Argc, 1);
  if not IsObject(V) then begin Res := MkPtr(NewArray(0, 0)); Exit(True); end;
  Res := OwnKeys(ObjOf(V), False);
  Result := True;
end;

function ObjProto_HasOwnProperty(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var K: PStr; O: PObj; Idx: Int64; V: TValue; F: Byte;
begin
  if not ToPropKey(Arg(Args, Argc, 1), K) then Exit(False);
  if not IsObject(Args[0]) then begin Res := VFalse; Exit(True); end;
  O := ObjOf(Args[0]);
  if (O^.Elements <> nil) and IndexFromString(K, Idx) then
    Res := MkBool((Idx < O^.Elements^.Len) and (O^.Elements^.Data[Idx] <> VHole))
  else
    Res := MkBool(ShapeLookup(O^.Shape, K) >= 0);
  if Res = VFalse then
    if OwnVirtual(ObjOf(Args[0]), K, V, F) then Res := VTrue;
  Result := True;
end;

function ObjProto_ToString(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var V: TValue;
begin
  V := Args[0];
  if V = VUndefined then Res := StrFromUnicode('[object Undefined]')
  else if V = VNull then Res := StrFromUnicode('[object Null]')
  else if IsArrayValue(V) then Res := StrFromUnicode('[object Array]')
  else if IsCallable(V) then Res := StrFromUnicode('[object Function]')
  else Res := StrFromUnicode('[object Object]');
  Result := True;
end;

{ 20.1.3.7 Object.prototype.valueOf: ToObject(this) }
function ObjProto_ValueOf(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
begin
  Result := ToObjectHook(Args[0], Res);
end;

{ 20.1.3.5 Object.prototype.toLocaleString: Invoke(this, "toString") }
function ObjProto_ToLocaleString(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var F: TValue;
begin
  if IsNullish(Args[0]) then Exit(ThrowTypeError('Object.prototype.toLocaleString called on null or undefined'));
  if not GetV(Args[0], AtomToString, F) then Exit(False);
  Result := CallValue(F, Args[0], nil, 0, Res);
end;

function ObjProto_IsPrototypeOf(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var P: TValue;
begin
  Res := VFalse;
  if not IsObject(Arg(Args, Argc, 1)) then Exit(True);
  P := ObjOf(Args[1])^.Shape^.Proto;
  while IsObject(P) do
  begin
    if P = Args[0] then begin Res := VTrue; Break; end;
    P := ObjOf(P)^.Shape^.Proto;
  end;
  Result := True;
end;

{ ---------------- Function ---------------- }

{ 20.2.3.6 Function.prototype [ @@hasInstance ] ( V ) }
function FuncProto_HasInstance(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var B: Boolean;
begin
  Result := OrdinaryHasInstance(Args[0], Arg(Args, Argc, 1), B);
  if Result then Res := MkBool(B);
end;

function FuncProto_Call(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
begin
  if Argc >= 2 then Result := CallValue(Args[0], Args[1], @Args[2], Argc - 1, Res)
  else Result := CallValue(Args[0], Arg(Args, Argc, 1), nil, 0, Res);
end;

function MaterializeArrayLike(Args: TArgs): Boolean;
var LenV, V: TValue; D: Double; N, I: Integer; A: PObj;
begin
  if IsNullish(Args[0]) then Exit(ThrowTypeError('Array.prototype method called on null or undefined'));
  if IsString(Args[0]) then N := StrOf(Args[0])^.Len
  else
  begin
    if not GetV(Args[0], AtomLength, LenV) then Exit(False);
    if not ToNumberV(LenV, D) then Exit(False);
    if IsNaN(D) or (D < 0) then N := 0 else N := Trunc(Min(D, 100000000));
  end;
  A := NewArray(N, N);
  PushRoot(MkPtr(A));
  for I := 0 to N - 1 do
  begin
    if not GetElemV(Args[0], MkInt(I), V) then begin PopRoots(1); Exit(False); end;
    ValueArrayStore(ObjOf(RootAt(TempTop - 1))^.Elements, I, V);
  end;
  Args[0] := RootAt(TempTop - 1);
  PopRoots(1);
  Result := True;
end;

function FuncProto_Apply(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var L: TValue; A: PObj; N, I: Integer; Buf: array of TValue;
begin
  L := Arg(Args, Argc, 2);
  if IsNullish(L) then Exit(CallValue(Args[0], Arg(Args, Argc, 1), nil, 0, Res));
  if not IsObject(L) then Exit(ThrowTypeError('CreateListFromArrayLike called on non-object'));
  if (ObjOf(L)^.Elements = nil) and (ObjOf(L)^.OKind <> okArray) then
  begin
    if not MaterializeArrayLike(@Args[2]) then Exit(False);
    L := Args[2];
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
  Result := CallValue(Args[0], Arg(Args, Argc, 1), @Buf[0], N, Res);
end;

function FuncProto_Empty(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
begin
  Res := VUndefined;
  Result := True;
end;

{ [[GetOwnProperty]] existence, proxies through their trap (Object.prototype.hasOwnProperty) }
function HasOwnPropertyV(O: TValue; Key: PStr; var Has: Boolean): Boolean;
var A: array[0..1] of TValue; R, F: TValue;
begin
  if ObjOf(O)^.OKind = okProxy then
  begin
    A[0] := O; A[1] := MkPtr(Key);
    if not GetV(ObjectProto, AtomOf('hasOwnProperty'), F) then Exit(False);
    if not CallValue(F, O, @A[1], 1, R) then Exit(False);
    Has := ToBool(R);
    Exit(True);
  end;
  Has := (ShapeLookup(ObjOf(O)^.Shape, Key) >= 0);
  if not Has then Has := OwnVirtual(ObjOf(O), Key, R, PByte(@A[0])^);
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
function FuncProto_Bind(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var B: PObj; VA: PValueArray; I, N: Integer; Proto, LenV, NameV: TValue; L, D: Double; Has: Boolean;
begin
  if not IsCallable(Args[0]) then Exit(ThrowTypeError('Bind must be called on a function'));
  { BoundFunctionCreate: the target's [[GetPrototypeOf]] }
  if ObjOf(Args[0])^.OKind = okProxy then
  begin
    if not GetPrototypeOfV(Args[0], Proto) then Exit(False);
  end
  else Proto := ObjOf(Args[0])^.Shape^.Proto;
  N := Max(Argc, 1);
  PushRoot(Proto);
  VA := NewValueArray(N);
  VA^.Len := N;
  VA^.Data[0] := Arg(Args, Argc, 1);
  for I := 2 to Argc do VA^.Data[I - 1] := Args[I];
  PushRoot(MkPtr(VA));
  B := NewObject(FnShapeFor(RootAt(TempTop - 2)), 3, okBound);
  B^.Aux1 := Args[0];
  B^.Aux2 := RootAt(TempTop - 1);
  B^.Slots[0] := MkInt(0);
  B^.Slots[1] := MkPtr(AtomOf(''));
  PopRoots(2);
  PushRoot(MkPtr(B));
  { length: the target's own length less the bound arguments }
  L := 0;
  if not HasOwnPropertyV(Args[0], AtomLength, Has) then begin PopRoots(1); Exit(False); end;
  if Has then
  begin
    if not GetV(Args[0], AtomLength, LenV) then begin PopRoots(1); Exit(False); end;
    if IsNumber(LenV) then
    begin
      D := NumOf(LenV);
      if IsInfinite(D) then begin if D > 0 then L := D else L := 0; end
      else if not IsNaN(D) then L := Max(Trunc(D) - Max(Argc - 1, 0), 0);
    end;
  end;
  ObjOf(RootAt(TempTop - 1))^.Slots[0] := MkNumber(L);
  if not GetV(Args[0], AtomName, NameV) then begin PopRoots(1); Exit(False); end;
  if IsString(NameV) then NameV := StrFromUnicode('bound ' + StrToUnicode(StrOf(NameV)))
  else NameV := MkPtr(AtomOf('bound '));
  ObjOf(RootAt(TempTop - 1))^.Slots[1] := NameV;
  WriteBarrier(@ObjOf(RootAt(TempTop - 1))^.Hdr, NameV);
  Res := RootAt(TempTop - 1);
  PopRoots(1);
  Result := True;
end;

{ the name in 20.2.3.5's NativeFunction syntax: an optional get / set, then a PropertyName (an identifier name or
  a computed [key]); a name that is neither (a private #name, the legacy RegExp.$&) is left out }
function NativeFunctionName(const Nm: UnicodeString): UnicodeString;
var Prefix, Key: UnicodeString; I: Integer; Ok: Boolean;
begin
  Prefix := ''; Key := Nm;
  if (Copy(Nm, 1, 4) = 'get ') or (Copy(Nm, 1, 4) = 'set ') then
  begin
    Prefix := Copy(Nm, 1, 4);
    Key := Copy(Nm, 5, MaxInt);
  end;
  Ok := (Key <> '') and (Key[1] = '[') and (Key[Length(Key)] = ']');
  if not Ok and (Key <> '') and not ((Key[1] >= '0') and (Key[1] <= '9')) then
  begin
    Ok := True;
    for I := 1 to Length(Key) do
      if not ((Key[I] = '$') or (Key[I] = '_') or ((Key[I] >= 'a') and (Key[I] <= 'z')) or
              ((Key[I] >= 'A') and (Key[I] <= 'Z')) or ((Key[I] >= '0') and (Key[I] <= '9')) or (Ord(Key[I]) > 127)) then
      begin
        Ok := False;
        Break;
      end;
  end;
  if Ok then Result := Prefix + Key else Result := Prefix;
end;

function FuncProto_ToString(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var O: PObj; T: PFuncTemplate; Nm: UnicodeString;
begin
  if not IsCallable(Args[0]) then Exit(ThrowTypeError('Function.prototype.toString requires that ''this'' be a Function'));
  O := ObjOf(Args[0]);
  Nm := '';
  if O^.OKind = okFunction then
  begin
    T := PFuncTemplate(O^.Native);
    if not T^.SourceRange.IsEmpty then
    begin
      Res := StrFromUnicode(UTF8Decode(T^.SourceRange.Get));
      Exit(True);
    end;
    { a built-in written in a prelude: its name property (set by SetFunctionName: "[Symbol.match]", "get x") }
    if (ShapeLookup(O^.Shape, AtomName) = 1) and IsString(O^.Slots[1]) then Nm := StrToUnicode(StrOf(O^.Slots[1]))
    else if T^.Name <> nil then Nm := StrToUnicode(T^.Name);
  end
  else if (O^.OKind = okNative) and IsString(O^.Aux1) then Nm := StrToUnicode(StrOf(O^.Aux1));
  Res := StrFromUnicode('function ' + NativeFunctionName(Nm) + '() { [native code] }');
  Result := True;
end;

{ ---------------- Array ---------------- }

function Array_Ctor(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var A: PObj; D: Double; I: Integer;
begin
  if (Argc = 1) and IsNumber(Args[1]) then
  begin
    D := NumOf(Args[1]);
    if (D < 0) or (D > 4294967295.0) or (Frac(D) <> 0) then Exit(ThrowError('RangeError', 'Invalid array length'));
    A := NewArray(Trunc(D), Trunc(D));
  end
  else
  begin
    A := NewArray(Argc, Argc);
    for I := 1 to Argc do A^.Elements^.Data[I - 1] := Args[I];
  end;
  Res := MkPtr(A);
  Result := True;
end;

function Array_IsArray(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
begin
  Res := MkBool(IsArrayValue(Arg(Args, Argc, 1)));
  Result := True;
end;

function Array_Of(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var A: PObj; I: Integer;
begin
  A := NewArray(Argc, Argc);
  for I := 1 to Argc do A^.Elements^.Data[I - 1] := Args[I];
  Res := MkPtr(A);
  Result := True;
end;

function Array_From(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var A: PObj; Src, It, V, F, R: TValue; Done: Boolean; N, I: Integer; LenV: TValue; D: Double;
begin
  A := NewArray(0, 8);
  PushRoot(MkPtr(A));
  Src := Arg(Args, Argc, 1);   { read after the allocation: a collection there moves young values }
  if IsObject(Src) and (ObjOf(Src)^.OKind <> okArray) and (ObjOf(Src)^.Elements = nil) and
     not IsCallable(ObjOf(Src)^.Shape^.Proto) then
  begin
    { array-like or iterable object }
    if not GetIterator(Src, It) then
    begin
      Pending := VUndefined;
      if not GetV(Args[1], AtomLength, LenV) then begin PopRoots(1); Exit(False); end;
      if not ToNumberV(LenV, D) then begin PopRoots(1); Exit(False); end;
      N := Trunc(D);
      for I := 0 to N - 1 do
      begin
        if not GetElemV(Args[1], MkInt(I), V) then begin PopRoots(1); Exit(False); end;
        ArrayPush(ObjOf(RootAt(TempTop - 1)), V);
      end;
      Res := RootAt(TempTop - 1);
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
    if IsCallable(Args[2]) and (Argc >= 2) then
    begin
      PushRoot(V); PushRoot(MkInt(I));
      if not CallValue(Args[2], VUndefined, @TempRoots[TempTop - 2], 2, R) then begin PopRoots(4); Exit(False); end;
      PopRoots(2);
      V := R;
    end;
    ArrayPush(ObjOf(RootAt(TempTop - 2)), V);
    Inc(I);
  end;
  Res := RootAt(TempTop - 2);
  PopRoots(2);
  Result := True;
end;

function ThisArray(Args: TArgs; var A: PObj): Boolean; inline;
begin
  if not IsObject(Args[0]) or ((ObjOf(Args[0])^.Elements = nil) and (ObjOf(Args[0])^.OKind <> okArray)
    and (ObjOf(Args[0])^.OKind <> okArguments)) then
    if not MaterializeArrayLike(Args) then Exit(False);
  A := ObjOf(Args[0]);
  Result := True;
end;

function ArrProto_Push(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var A: PObj; I: Integer; O, LenV: TValue; L: Double;
begin
  if not (IsObject(Args[0]) and (ObjOf(Args[0])^.OKind = okArray) and ((ObjOf(Args[0])^.OFlags and 64) = 0)) then
  begin
    { 23.1.3.23 on any other receiver: ToObject, LengthOfArrayLike, Set(O, k, v, true) and Set(O, "length", true) }
    if not ToObjectV(Args[0], O) then Exit(False);
    PushRoot(O);
    if not GetV(O, AtomLength, LenV) or not ToNumberV(LenV, L) then begin PopRoots(1); Exit(False); end;
    if IsNaN(L) or (L < 0) then L := 0 else L := Int(Min(L, 9007199254740991.0));
    if L + Argc > 9007199254740991.0 then begin PopRoots(1); Exit(ThrowTypeError('Pushing would exceed the maximum array length')); end;
    for I := 1 to Argc do
      if not SetElemVT(RootAt(TempTop - 1), MkNumber(L + I - 1), Args[I]) then begin PopRoots(1); Exit(False); end;
    Res := MkNumber(L + Argc);
    Result := SetVT(RootAt(TempTop - 1), AtomLength, Res);
    PopRoots(1);
    Exit;
  end;
  if not ThisArray(Args, A) then Exit(False);
  for I := 1 to Argc do ArrayPush(ObjOf(Args[0]), Args[I]);
  Res := MkInt(ArrayLen(ObjOf(Args[0])));
  Result := True;
end;

function ArrProto_Pop(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var A: PObj; E: PValueArray;
begin
  if not ThisArray(Args, A) then Exit(False);
  E := A^.Elements;
  if E^.Len = 0 then begin Res := VUndefined; Exit(True); end;
  Dec(E^.Len);
  Res := E^.Data[E^.Len];
  if Res = VHole then Res := VUndefined;
  E^.Data[E^.Len] := VUndefined;
  Result := True;
end;

function ArrProto_Shift(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var A: PObj; E: PValueArray;
begin
  if not ThisArray(Args, A) then Exit(False);
  E := A^.Elements;
  if E^.Len = 0 then begin Res := VUndefined; Exit(True); end;
  Res := E^.Data[0];
  if Res = VHole then Res := VUndefined;
  Move(E^.Data[1], E^.Data[0], (E^.Len - 1) * SizeOf(TValue));
  Dec(E^.Len);
  E^.Data[E^.Len] := VUndefined;
  Result := True;
end;

function ArrProto_Unshift(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var A: PObj; E: PValueArray; I, N: Integer;
begin
  if not ThisArray(Args, A) then Exit(False);
  N := A^.Elements^.Len;
  EnsureElementsCap(A, N + Argc);
  A := ObjOf(Args[0]);
  E := A^.Elements;
  Move(E^.Data[0], E^.Data[Argc], N * SizeOf(TValue));
  for I := 1 to Argc do
  begin
    E^.Data[I - 1] := Args[I];
    WriteBarrier(@E^.Hdr, Args[I]);
  end;
  E^.Len := N + Argc;
  Res := MkInt(E^.Len);
  Result := True;
end;

function RelIndex(V: Int64; Len: Integer): Integer; inline;
begin
  if V < 0 then begin V := V + Len; if V < 0 then V := 0; end
  else if V > Len then V := Len;
  Result := V;
end;

function ArrProto_Slice(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var A, R: PObj; Len, S, E_, I: Integer; X, Y: Int64;
begin
  if not ThisArray(Args, A) then Exit(False);
  Len := ArrayLen(A);
  if not IntArg(Args, Argc, 1, 0, X) then Exit(False);
  if not IntArg(Args, Argc, 2, Len, Y) then Exit(False);
  S := RelIndex(X, Len); E_ := RelIndex(Y, Len);
  if E_ < S then E_ := S;
  R := NewArray(E_ - S, E_ - S);
  A := ObjOf(Args[0]);
  for I := S to E_ - 1 do R^.Elements^.Data[I - S] := A^.Elements^.Data[I];
  Res := MkPtr(R);
  Result := True;
end;

function ArrProto_Splice(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var A, R: PObj; Len, S, DelCount, I, Ins, NewLen: Integer; X, Y: Int64; E: PValueArray;
begin
  if not ThisArray(Args, A) then Exit(False);
  Len := ArrayLen(A);
  if not IntArg(Args, Argc, 1, 0, X) then Exit(False);
  S := RelIndex(X, Len);
  if Argc < 2 then DelCount := Len - S
  else
  begin
    if not IntArg(Args, Argc, 2, 0, Y) then Exit(False);
    DelCount := Max(0, Min(Y, Len - S));
  end;
  Ins := Max(0, Argc - 2);
  R := NewArray(DelCount, DelCount);
  A := ObjOf(Args[0]);
  for I := 0 to DelCount - 1 do R^.Elements^.Data[I] := A^.Elements^.Data[S + I];
  PushRoot(MkPtr(R));
  NewLen := Len - DelCount + Ins;
  EnsureElementsCap(A, NewLen);
  A := ObjOf(Args[0]);
  E := A^.Elements;
  Move(E^.Data[S + DelCount], E^.Data[S + Ins], (Len - S - DelCount) * SizeOf(TValue));
  for I := 0 to Ins - 1 do
  begin
    E^.Data[S + I] := Args[3 + I];
    WriteBarrier(@E^.Hdr, Args[3 + I]);
  end;
  for I := NewLen to Len - 1 do E^.Data[I] := VUndefined;
  E^.Len := NewLen;
  Res := RootAt(TempTop - 1);
  PopRoots(1);
  Result := True;
end;

function ArrProto_Concat(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var R: PObj; I, J: Integer; V: TValue;
begin
  R := NewArray(0, 8);
  PushRoot(MkPtr(R));
  for I := 0 to Argc do
  begin
    V := Args[I];
    if IsArrayValue(V) then
    begin
      for J := 0 to ArrayLen(ObjOf(Args[I])) - 1 do
        ArrayPush(ObjOf(RootAt(TempTop - 1)), ObjOf(Args[I])^.Elements^.Data[J]);
    end
    else ArrayPush(ObjOf(RootAt(TempTop - 1)), V);
  end;
  Res := RootAt(TempTop - 1);
  PopRoots(1);
  Result := True;
end;

function ArrProto_Join(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var A: PObj; Sep: UnicodeString; S: PStr; I, N: Integer; V, SV: TValue; B: TUnicodeStringBuilder;
  Parts: array of UnicodeString; Total: Integer; Out_: PStr; P: Integer;
begin
  if not ThisArray(Args, A) then Exit(False);
  if (Argc >= 1) and (Args[1] <> VUndefined) then
  begin
    if not StrArg(Args, Argc, 1, S) then Exit(False);
    Sep := StrToUnicode(S);
  end
  else Sep := ',';
  N := ArrayLen(ObjOf(Args[0]));
  SetLength(Parts, N);
  for I := 0 to N - 1 do
  begin
    V := ObjOf(Args[0])^.Elements^.Data[I];
    if IsNullish(V) or (V = VHole) then Parts[I] := ''
    else
    begin
      if not ToStringV(V, SV) then Exit(False);
      Parts[I] := StrToUnicode(StrOf(SV));
    end;
  end;
  Res := StrFromUnicode(UnicodeString(''.Join(Sep, Parts)));
  Result := True;
end;

function ArrProto_ToString(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
begin
  if not IsArrayValue(Args[0]) then begin Res := StrFromUnicode('[object Object]'); Exit(True); end;
  Result := ArrProto_Join(Args, 0, Res);
end;

function ArrProto_Reverse(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var A: PObj; I, N: Integer; T: TValue;
begin
  if not ThisArray(Args, A) then Exit(False);
  N := ArrayLen(A);
  for I := 0 to N div 2 - 1 do
  begin
    T := A^.Elements^.Data[I];
    A^.Elements^.Data[I] := A^.Elements^.Data[N - 1 - I];
    A^.Elements^.Data[N - 1 - I] := T;
  end;
  Res := Args[0];
  Result := True;
end;

function ArrProto_IndexOf(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var A: PObj; I, N: Integer; X: Int64; V: TValue;
begin
  if not ThisArray(Args, A) then Exit(False);
  N := ArrayLen(A);
  if not IntArg(Args, Argc, 2, 0, X) then Exit(False);
  A := ObjOf(Args[0]);
  V := Arg(Args, Argc, 1);
  for I := RelIndex(X, N) to N - 1 do
    if StrictEquals(A^.Elements^.Data[I], V) then begin Res := MkInt(I); Exit(True); end;
  Res := MkInt(-1);
  Result := True;
end;

function ArrProto_LastIndexOf(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var A: PObj; I, N, Start: Integer; V: TValue; X: Int64;
begin
  if not ThisArray(Args, A) then Exit(False);
  N := ArrayLen(A);
  { fromIndex: absent means the last element; negative counts from the end }
  if Argc >= 2 then
  begin
    if not IntArg(Args, Argc, 2, N - 1, X) then Exit(False);
    A := ObjOf(Args[0]);
    if X < 0 then X := X + N;
    if X > N - 1 then X := N - 1;
    Start := X;
  end
  else Start := N - 1;
  V := Arg(Args, Argc, 1);
  for I := Start downto 0 do
    if StrictEquals(A^.Elements^.Data[I], V) then begin Res := MkInt(I); Exit(True); end;
  Res := MkInt(-1);
  Result := True;
end;

function ArrProto_Includes(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var A: PObj; I, N: Integer; V, E: TValue; X: Int64;
begin
  if not ThisArray(Args, A) then Exit(False);
  N := ArrayLen(A);
  if not IntArg(Args, Argc, 2, 0, X) then Exit(False);
  A := ObjOf(Args[0]);
  V := Arg(Args, Argc, 1);
  for I := RelIndex(X, N) to N - 1 do
  begin
    E := A^.Elements^.Data[I];
    if E = VHole then E := VUndefined;
    if StrictEquals(E, V) or (IsNumber(E) and IsNumber(V) and IsNaN(NumOf(E)) and IsNaN(NumOf(V))) then
    begin Res := VTrue; Exit(True); end;
  end;
  Res := VFalse;
  Result := True;
end;

function ArrProto_Fill(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var A: PObj; I, N, S, E_: Integer; X, Y: Int64; V: TValue;
begin
  if not ThisArray(Args, A) then Exit(False);
  N := ArrayLen(A);
  if not IntArg(Args, Argc, 2, 0, X) then Exit(False);
  if not IntArg(Args, Argc, 3, N, Y) then Exit(False);
  S := RelIndex(X, N); E_ := RelIndex(Y, N);
  A := ObjOf(Args[0]);
  V := Arg(Args, Argc, 1);
  for I := S to E_ - 1 do A^.Elements^.Data[I] := V;
  WriteBarrier(@A^.Elements^.Hdr, V);
  Res := Args[0];
  Result := True;
end;

type
  TIterKind = (ikForEach, ikMap, ikFilter, ikSome, ikEvery, ikFind, ikFindIndex, ikFindLast, ikFindLastIndex);

function ArrayIterate(Args: TArgs; Argc: Integer; var Res: TValue; Kind: TIterKind): Boolean;
var A, R: PObj; I, N, Base: Integer; V, CR, F: TValue; CallArgs: array[0..2] of TValue; Stop: Boolean;
begin
  if not ThisArray(Args, A) then Exit(False);
  F := Arg(Args, Argc, 1);
  if not IsCallable(F) then Exit(ThrowTypeError('callback is not a function'));
  N := ArrayLen(A);
  Res := VUndefined;
  if Kind in [ikMap] then begin R := NewArray(N, N); Res := MkPtr(R); end
  else if Kind = ikFilter then begin R := NewArray(0, 4); Res := MkPtr(R); end;
  if Kind = ikEvery then Res := VTrue;
  if Kind = ikSome then Res := VFalse;
  if Kind in [ikFindIndex, ikFindLastIndex] then Res := MkInt(-1);
  PushRoot(Res);
  I := 0;
  if Kind in [ikFindLast, ikFindLastIndex] then I := N - 1;
  while (I >= 0) and (I < N) do
  begin
    A := ObjOf(Args[0]);
    if I >= ArrayLen(A) then Break;
    V := A^.Elements^.Data[I];
    if V = VHole then
    begin
      if Kind in [ikForEach, ikMap, ikFilter, ikSome, ikEvery] then
      begin
        if Kind in [ikFindLast, ikFindLastIndex] then Dec(I) else Inc(I);
        Continue;
      end;
      V := VUndefined;
    end;
    { callback(value, index, array): the arguments live on the value stack via CallValue's copy }
    CallArgs[0] := V; CallArgs[1] := MkInt(I); CallArgs[2] := Args[0];
    PushRoot(V);
    if not CallValue(Args[1], Arg(Args, Argc, 2), @CallArgs[0], 3, CR) then begin PopRoots(2); Exit(False); end;
    V := RootAt(TempTop - 1);
    PopRoots(1);
    Stop := False;
    case Kind of
      ikMap: ValueArrayStore(ObjOf(RootAt(TempTop - 1))^.Elements, I, CR);
      ikFilter: if ToBool(CR) then ArrayPush(ObjOf(RootAt(TempTop - 1)), V);
      ikSome: if ToBool(CR) then begin TempRoots[TempTop - 1] := VTrue; Stop := True; end;
      ikEvery: if not ToBool(CR) then begin TempRoots[TempTop - 1] := VFalse; Stop := True; end;
      ikFind, ikFindLast: if ToBool(CR) then begin TempRoots[TempTop - 1] := V; Stop := True; end;
      ikFindIndex, ikFindLastIndex: if ToBool(CR) then begin TempRoots[TempTop - 1] := MkInt(I); Stop := True; end;
    end;
    if Stop then Break;
    if Kind in [ikFindLast, ikFindLastIndex] then Dec(I) else Inc(I);
  end;
  Res := RootAt(TempTop - 1);
  PopRoots(1);
  Result := True;
end;

function ArrProto_ForEach(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
begin Result := ArrayIterate(Args, Argc, Res, ikForEach); Res := VUndefined; end;
function ArrProto_Map(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
begin Result := ArrayIterate(Args, Argc, Res, ikMap); end;
function ArrProto_Filter(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
begin Result := ArrayIterate(Args, Argc, Res, ikFilter); end;
function ArrProto_Some(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
begin Result := ArrayIterate(Args, Argc, Res, ikSome); end;
function ArrProto_Every(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
begin Result := ArrayIterate(Args, Argc, Res, ikEvery); end;
function ArrProto_Find(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
begin Result := ArrayIterate(Args, Argc, Res, ikFind); end;
function ArrProto_FindIndex(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
begin Result := ArrayIterate(Args, Argc, Res, ikFindIndex); end;
function ArrProto_FindLast(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
begin Result := ArrayIterate(Args, Argc, Res, ikFindLast); end;
function ArrProto_FindLastIndex(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
begin Result := ArrayIterate(Args, Argc, Res, ikFindLastIndex); end;

function ArrProto_Reduce(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var A: PObj; I, N: Integer; Acc, V: TValue; CallArgs: array[0..3] of TValue;
begin
  if not ThisArray(Args, A) then Exit(False);
  if not IsCallable(Arg(Args, Argc, 1)) then Exit(ThrowTypeError('callback is not a function'));
  N := ArrayLen(A);
  I := 0;
  if Argc >= 2 then Acc := Args[2]
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
    A := ObjOf(Args[0]);
    if I >= ArrayLen(A) then Break;
    V := A^.Elements^.Data[I];
    if V <> VHole then
    begin
      CallArgs[0] := RootAt(TempTop - 1); CallArgs[1] := V; CallArgs[2] := MkInt(I); CallArgs[3] := Args[0];
      if not CallValue(Args[1], VUndefined, @CallArgs[0], 4, Acc) then begin PopRoots(1); Exit(False); end;
      TempRoots[TempTop - 1] := Acc;
    end;
    Inc(I);
  end;
  Res := RootAt(TempTop - 1);
  PopRoots(1);
  Result := True;
end;

function ArrProto_ReduceRight(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var A: PObj; I: Integer; Acc, V: TValue; CallArgs: array[0..3] of TValue;
begin
  if not ThisArray(Args, A) then Exit(False);
  if not IsCallable(Arg(Args, Argc, 1)) then Exit(ThrowTypeError('callback is not a function'));
  I := ArrayLen(A) - 1;
  if Argc >= 2 then Acc := Args[2]
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
    A := ObjOf(Args[0]);
    if I < ArrayLen(A) then
    begin
      V := A^.Elements^.Data[I];
      if V <> VHole then
      begin
        CallArgs[0] := RootAt(TempTop - 1); CallArgs[1] := V; CallArgs[2] := MkInt(I); CallArgs[3] := Args[0];
        if not CallValue(Args[1], VUndefined, @CallArgs[0], 4, Acc) then begin PopRoots(1); Exit(False); end;
        TempRoots[TempTop - 1] := Acc;
      end;
    end;
    Dec(I);
  end;
  Res := RootAt(TempTop - 1);
  PopRoots(1);
  Result := True;
end;

threadvar
  SortCmp: TValue;
  SortFailed: Boolean;

function SortLess(A, B: TValue; var Less: Boolean): Boolean;
var CallArgs: array[0..1] of TValue; R: TValue; D: Double; SA, SB: TValue;
begin
  if A = VUndefined then begin Less := False; Exit(True); end;
  if B = VUndefined then begin Less := True; Exit(True); end;
  if SortCmp <> VUndefined then
  begin
    CallArgs[0] := A; CallArgs[1] := B;
    if not CallValue(SortCmp, VUndefined, @CallArgs[0], 2, R) then Exit(False);
    if not ToNumberV(R, D) then Exit(False);
    Less := D < 0;
    Exit(True);
  end;
  PushRoot(B);
  if not ToStringV(A, SA) then begin PopRoots(1); Exit(False); end;
  PushRoot(SA);
  if not ToStringV(RootAt(TempTop - 2), SB) then begin PopRoots(2); Exit(False); end;
  SA := RootAt(TempTop - 1);
  PopRoots(2);
  Less := StrCompare(StrOf(SA), StrOf(SB)) < 0;
  Result := True;
end;

function ArrProto_Sort(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var A: PObj; N, I, J, W, L, R_, M, Mid: Integer; Tmp: PObj; Less: Boolean; Src, Dst, Swap: PValueArray; SavedCmp: TValue;
begin
  if not ThisArray(Args, A) then Exit(False);
  N := ArrayLen(A);
  SavedCmp := SortCmp;
  SortCmp := Arg(Args, Argc, 1);
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
        Src := ObjOf(Args[0])^.Elements;
        Dst := ObjOf(RootAt(TempTop - 1))^.Elements;
        if L >= Min(I + W, N) then begin Dst^.Data[J] := Src^.Data[M]; Inc(M); end
        else if M >= R_ then begin Dst^.Data[J] := Src^.Data[L]; Inc(L); end
        else
        begin
          if not SortLess(Src^.Data[M], Src^.Data[L], Less) then
          begin
            PopRoots(1); Dec(ExtraRootCount); SortCmp := SavedCmp; Exit(False);
          end;
          Src := ObjOf(Args[0])^.Elements;
          Dst := ObjOf(RootAt(TempTop - 1))^.Elements;
          if Less then begin Dst^.Data[J] := Src^.Data[M]; Inc(M); end
          else begin Dst^.Data[J] := Src^.Data[L]; Inc(L); end;
        end;
        Inc(J);
      end;
      Inc(I, 2 * W);
    end;
    Src := ObjOf(Args[0])^.Elements;
    Dst := ObjOf(RootAt(TempTop - 1))^.Elements;
    Move(Dst^.Data[0], Src^.Data[0], N * SizeOf(TValue));
    if (Src^.Hdr.Flags and cfOld) <> 0 then Remember(@Src^.Hdr);
    W := W * 2;
  end;
  PopRoots(1);
  Dec(ExtraRootCount);
  SortCmp := SavedCmp;
  Res := Args[0];
  Result := True;
end;

function ArrProto_Values(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
begin
  if IsString(Args[0]) or (IsObject(Args[0]) and (ObjOf(Args[0])^.OKind in [okArray, okArguments, 14])) then
    Exit(GetIterator(Args[0], Res));
  { any other array-like: iterate a snapshot of its indexed elements }
  if not MaterializeArrayLike(Args) then Exit(False);
  Res := NewArrayIterator(Args[0], 0);
  Result := True;
end;

{ %ArrayIteratorPrototype%.next (NativeData 0: OFlags 0, 2-4) and %StringIteratorPrototype%.next (NativeData 1:
  OFlags 1) }
function ArrIter_Next(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var V: TValue; Done: Boolean; O: PObj;
begin
  if not IsObject(Args[0]) or (ObjOf(Args[0])^.OKind <> okIterator) or (ObjOf(Args[0])^.OFlags > 4) or
     ((ObjOf(Args[0])^.OFlags = 1) <> (NativeData = 1)) then
    Exit(ThrowTypeError('next called on an object that is not an iterator of this kind'));
  if not IteratorStep(Args[0], V, Done) then Exit(False);
  PushRoot(V);
  O := NewPlainObject(ObjectProto, 2);
  V := RootAt(TempTop - 1);
  PopRoots(1);
  AddProperty(O, AtomValue, V, pfDefault);
  AddProperty(O, AtomDone, MkBool(Done), pfDefault);
  Res := MkPtr(O);
  Result := True;
end;

{ 22.1.3.36 String.prototype [ @@iterator ] ( ): CreateStringIterator(ToString(this)) }
function StrProto_Iterator(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var S: TValue;
begin
  if IsNullish(Args[0]) then Exit(ThrowTypeError('String.prototype[Symbol.iterator] called on null or undefined'));
  if not ToStringV(Args[0], S) then Exit(False);
  Res := NewArrayIterator(S, 1);
  Result := True;
end;

function Iter_Self(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
begin
  Res := Args[0];
  Result := True;
end;

{ ---------------- String ---------------- }

function ThisStr(Args: TArgs; var S: PStr): Boolean; inline;
var V: TValue;
begin
  if IsString(Args[0]) then begin S := StrOf(Args[0]); Exit(True); end;
  if IsObject(Args[0]) and (ObjOf(Args[0])^.OKind = okStringObj) then
  begin
    { callers read the receiver again from Args[0]: it must hold the string, not the wrapper }
    Args[0] := ObjOf(Args[0])^.Aux1;
    S := StrOf(Args[0]);
    Exit(True);
  end;
  if IsNullish(Args[0]) then Exit(ThrowTypeError('String.prototype method called on null or undefined'));
  if not ToStringV(Args[0], V) then Exit(False);
  Args[0] := V;
  S := StrOf(V);
  Result := True;
end;

function String_Ctor(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var V: TValue; O: PObj;
begin
  if Argc = 0 then V := MkPtr(AtomOf(''))
  { 22.1.1.1 step 2.a: a symbol called without new is its SymbolDescriptiveString (new: ToString throws) }
  else if (CurNewTarget = VUndefined) and IsPtr(Args[1]) and (PCell(PtrOf(Args[1]))^.Kind = ckSymbol) then
  begin
    V := PValue(PByte(PtrOf(Args[1])) + 8)^;
    if IsString(V) then V := StrFromUnicode('Symbol(' + StrToUnicode(StrOf(V)) + ')')
    else V := StrFromUnicode('Symbol()');
  end
  else if not ToStringV(Args[1], V) then Exit(False);
  if CurNewTarget <> VUndefined then
  begin
    PushRoot(V);
    O := NewObject(RootShapeFor(StringProto), 0, okStringObj);
    O^.Aux1 := RootAt(TempTop - 1);
    PopRoots(1);
    V := MkPtr(O);
  end;
  Res := V;
  Result := True;
end;

function String_FromCharCode(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var S: PStr; I: Integer; D: Double; Codes: array of Word;
begin
  SetLength(Codes, Argc);
  for I := 1 to Argc do
  begin
    if not NumArg(Args, Argc, I, D) then Exit(False);
    Codes[I - 1] := Word(Trunc(D) and $FFFF);
  end;
  S := NewString(Argc);
  for I := 1 to Argc do StrChars(S)[I - 1] := WideChar(Codes[I - 1]);
  Res := MkPtr(S);
  Result := True;
end;

function StrProto_CharAt(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var S: PStr; X: Int64;
begin
  if not ThisStr(Args, S) then Exit(False);
  if not IntArg(Args, Argc, 1, 0, X) then Exit(False);
  S := StrOf(Args[0]);
  if (X < 0) or (X >= S^.Len) then Res := MkPtr(AtomOf('')) else Res := SubString(S, X, 1);
  Result := True;
end;

function StrProto_CharCodeAt(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var S: PStr; X: Int64;
begin
  if not ThisStr(Args, S) then Exit(False);
  if not IntArg(Args, Argc, 1, 0, X) then Exit(False);
  S := StrOf(Args[0]);
  if (X < 0) or (X >= S^.Len) then Res := MkDouble(NaN) else Res := MkInt(Ord(StrChars(S)[X]));
  Result := True;
end;

function StrProto_CodePointAt(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var S: PStr; X: Int64; C, C2: Integer;
begin
  if not ThisStr(Args, S) then Exit(False);
  if not IntArg(Args, Argc, 1, 0, X) then Exit(False);
  S := StrOf(Args[0]);
  if (X < 0) or (X >= S^.Len) then begin Res := VUndefined; Exit(True); end;
  C := Ord(StrChars(S)[X]);
  if (C >= $D800) and (C <= $DBFF) and (X + 1 < S^.Len) then
  begin
    C2 := Ord(StrChars(S)[X + 1]);
    if (C2 >= $DC00) and (C2 <= $DFFF) then C := (C - $D800) * $400 + (C2 - $DC00) + $10000;
  end;
  Res := MkInt(C);
  Result := True;
end;

function FindIn(S, Sub: PStr; From: Integer): Integer;
var I, J: Integer; Ok: Boolean;
begin
  if From > S^.Len then From := S^.Len;
  if Sub^.Len = 0 then Exit(Max(From, 0));
  for I := Max(From, 0) to S^.Len - Sub^.Len do
  begin
    Ok := True;
    for J := 0 to Sub^.Len - 1 do
      if StrChars(S)[I + J] <> StrChars(Sub)[J] then begin Ok := False; Break; end;
    if Ok then Exit(I);
  end;
  Result := -1;
end;

function StrProto_IndexOf(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var S, Sub: PStr; X: Int64;
begin
  if not ThisStr(Args, S) then Exit(False);
  if not StrArg(Args, Argc, 1, Sub) then Exit(False);
  if not IntArg(Args, Argc, 2, 0, X) then Exit(False);
  Res := MkInt(FindIn(StrOf(Args[0]), Sub, X));
  Result := True;
end;

function StrProto_LastIndexOf(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var S, Sub: PStr; I, J, Start: Integer; Ok: Boolean; X: Int64; D: Double;
begin
  if not ThisStr(Args, S) then Exit(False);
  if not StrArg(Args, Argc, 1, Sub) then Exit(False);
  { 22.1.3.11 step 5: a NaN position searches from the end }
  if (Argc >= 2) and (Args[2] <> VUndefined) and not IsNumber(Args[2]) then
  begin
    PushRoot(MkPtr(Sub));
    Ok := ToNumberV(Args[2], D);
    Sub := StrOf(RootAt(TempTop - 1));
    PopRoots(1);
    if not Ok then Exit(False);
    Args[2] := MkNumber(D);
  end;
  if (Argc >= 2) and IsNumber(Args[2]) and IsNaN(NumOf(Args[2])) then X := MaxInt
  else if not IntArg(Args, Argc, 2, MaxInt, X) then Exit(False);
  S := StrOf(Args[0]);
  if X < 0 then X := 0;
  Start := S^.Len - Sub^.Len;
  if X < Start then Start := X;
  for I := Start downto 0 do
  begin
    Ok := True;
    for J := 0 to Sub^.Len - 1 do
      if StrChars(S)[I + J] <> StrChars(Sub)[J] then begin Ok := False; Break; end;
    if Ok then begin Res := MkInt(I); Exit(True); end;
  end;
  Res := MkInt(-1);
  Result := True;
end;

{ 7.2.8 IsRegExp(argument) true is a TypeError for includes, startsWith and endsWith }
function RejectRegExp(V: TValue; const Name: UnicodeString): Boolean;
var M: TValue;
begin
  Result := True;
  if not IsObject(V) then Exit;
  PushRoot(V);
  Result := GetElemV(V, SymMatch, M);
  V := RootAt(TempTop - 1);
  PopRoots(1);
  if not Result then Exit;
  if M <> VUndefined then
  begin
    if ToBool(M) then Result := ThrowTypeError('First argument to String.prototype.' + Name + ' must not be a regular expression');
    Exit;
  end;
  if ObjOf(V)^.OKind = 17 then
    Result := ThrowTypeError('First argument to String.prototype.' + Name + ' must not be a regular expression');
end;

function StrProto_Includes(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var S, Sub: PStr; X: Int64;
begin
  if not ThisStr(Args, S) then Exit(False);
  if not RejectRegExp(Arg(Args, Argc, 1), 'includes') then Exit(False);
  if not StrArg(Args, Argc, 1, Sub) then Exit(False);
  if not IntArg(Args, Argc, 2, 0, X) then Exit(False);
  if X < 0 then X := 0;
  Res := MkBool(FindIn(StrOf(Args[0]), Sub, X) >= 0);
  Result := True;
end;

function StrProto_StartsWith(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var S, Sub: PStr; X: Int64;
begin
  if not ThisStr(Args, S) then Exit(False);
  if not RejectRegExp(Arg(Args, Argc, 1), 'startsWith') then Exit(False);
  if not StrArg(Args, Argc, 1, Sub) then Exit(False);
  if not IntArg(Args, Argc, 2, 0, X) then Exit(False);
  S := StrOf(Args[0]);
  if X < 0 then X := 0;
  if X > S^.Len then X := S^.Len;
  Res := MkBool((X + Sub^.Len <= S^.Len) and ((Sub^.Len = 0) or (CompareByte(StrChars(S)[X], StrChars(Sub)[0], Sub^.Len * 2) = 0)));
  Result := True;
end;

function StrProto_EndsWith(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var S, Sub: PStr; P: Integer; X: Int64;
begin
  if not ThisStr(Args, S) then Exit(False);
  if not RejectRegExp(Arg(Args, Argc, 1), 'endsWith') then Exit(False);
  if not StrArg(Args, Argc, 1, Sub) then Exit(False);
  if not IntArg(Args, Argc, 2, MaxInt, X) then Exit(False);
  S := StrOf(Args[0]);
  if X < 0 then X := 0;
  if X > S^.Len then X := S^.Len;
  P := X - Sub^.Len;
  Res := MkBool((P >= 0) and ((Sub^.Len = 0) or (CompareByte(StrChars(S)[P], StrChars(Sub)[0], Sub^.Len * 2) = 0)));
  Result := True;
end;

function StrProto_Slice(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var S: PStr; X, Y: Int64; A, B: Integer;
begin
  if not ThisStr(Args, S) then Exit(False);
  if not IntArg(Args, Argc, 1, 0, X) then Exit(False);
  if not IntArg(Args, Argc, 2, StrOf(Args[0])^.Len, Y) then Exit(False);
  S := StrOf(Args[0]);
  A := RelIndex(X, S^.Len); B := RelIndex(Y, S^.Len);
  if B < A then B := A;
  Res := SubString(S, A, B - A);
  Result := True;
end;

function StrProto_Substring(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var S: PStr; X, Y: Int64; A, B, T: Integer;
begin
  if not ThisStr(Args, S) then Exit(False);
  if not IntArg(Args, Argc, 1, 0, X) then Exit(False);
  if not IntArg(Args, Argc, 2, StrOf(Args[0])^.Len, Y) then Exit(False);
  S := StrOf(Args[0]);
  A := Max(0, Min(X, S^.Len)); B := Max(0, Min(Y, S^.Len));
  if A > B then begin T := A; A := B; B := T; end;
  Res := SubString(S, A, B - A);
  Result := True;
end;

function StrProto_Substr(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var S: PStr; X, Y: Int64; A, L: Integer;
begin
  if not ThisStr(Args, S) then Exit(False);
  if not IntArg(Args, Argc, 1, 0, X) then Exit(False);
  if not IntArg(Args, Argc, 2, StrOf(Args[0])^.Len, Y) then Exit(False);
  S := StrOf(Args[0]);
  A := RelIndex(X, S^.Len);
  L := Max(0, Min(Y, S^.Len - A));
  Res := SubString(S, A, L);
  Result := True;
end;

{ 22.1.3.28 / 22.1.3.30 toLowerCase / toUpperCase: ASCII in place (the string itself when nothing changes), the
  rest by the full Unicode case mapping of CaseMapHook (G2.Intl: ICU's, special casing and final sigma included) }
function CaseConvert(V: TValue; Upper: Boolean; var Res: TValue): Boolean;
var S: PStr; I, N: Integer; C: WideChar; Ascii, Change: Boolean; U, R: UnicodeString;
begin
  S := StrOf(V);
  N := S^.Len;
  Ascii := True; Change := False;
  for I := 0 to N - 1 do
  begin
    C := StrChars(S)[I];
    if Ord(C) >= 128 then begin Ascii := False; Break; end;
    if Upper then Change := Change or ((C >= 'a') and (C <= 'z'))
    else Change := Change or ((C >= 'A') and (C <= 'Z'));
  end;
  if Ascii then
  begin
    if not Change then begin Res := V; Exit(True); end;
    SetLength(U, N);
    for I := 0 to N - 1 do
    begin
      C := StrChars(S)[I];
      if Upper and (C >= 'a') and (C <= 'z') then C := WideChar(Ord(C) - 32)
      else if not Upper and (C >= 'A') and (C <= 'Z') then C := WideChar(Ord(C) + 32);
      U[I + 1] := C;
    end;
    Res := StrFromUnicode(U);
    Exit(True);
  end;
  U := StrToUnicode(S);
  if Assigned(CaseMapHook) and CaseMapHook(U, '', Upper, R) then Res := StrFromUnicode(R)
  else if Upper then Res := StrFromUnicode(UnicodeUpperCase(U))
  else Res := StrFromUnicode(UnicodeLowerCase(U));
  Result := True;
end;

function StrProto_ToUpper(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var S: PStr;
begin
  if not ThisStr(Args, S) then Exit(False);
  Result := CaseConvert(MkPtr(S), True, Res);
end;

function StrProto_ToLower(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var S: PStr;
begin
  if not ThisStr(Args, S) then Exit(False);
  Result := CaseConvert(MkPtr(S), False, Res);
end;

{ 22.1.3.10 / 22.1.3.32 isWellFormed / toWellFormed: a lone surrogate (replaced by U+FFFD) }
function WellFormed(Args: TArgs; Fix: Boolean; var Res: TValue): Boolean;
var S: PStr; I, N: Integer; C: Word; Bad: Boolean; U: UnicodeString;
begin
  if not ThisStr(Args, S) then Exit(False);
  S := StrOf(Args[0]);
  N := S^.Len;
  Bad := False;
  I := 0;
  while I < N do
  begin
    C := Ord(StrChars(S)[I]);
    if (C >= $D800) and (C <= $DBFF) and (I + 1 < N) and (Ord(StrChars(S)[I + 1]) >= $DC00) and
       (Ord(StrChars(S)[I + 1]) <= $DFFF) then Inc(I, 2)
    else if (C >= $D800) and (C <= $DFFF) then begin Bad := True; Break; end
    else Inc(I);
  end;
  if not Fix then begin Res := MkBool(not Bad); Exit(True); end;
  if not Bad then begin Res := Args[0]; Exit(True); end;
  U := StrToUnicode(S);
  I := 1;
  while I <= Length(U) do
  begin
    C := Ord(U[I]);
    if (C >= $D800) and (C <= $DBFF) and (I < Length(U)) and (Ord(U[I + 1]) >= $DC00) and (Ord(U[I + 1]) <= $DFFF) then
      Inc(I, 2)
    else
    begin
      if (C >= $D800) and (C <= $DFFF) then U[I] := #$FFFD;
      Inc(I);
    end;
  end;
  Res := StrFromUnicode(U);
  Result := True;
end;

function StrProto_IsWellFormed(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
begin
  Result := WellFormed(Args, False, Res);
end;

function StrProto_ToWellFormed(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
begin
  Result := WellFormed(Args, True, Res);
end;

function IsJSWhite(C: WideChar): Boolean; inline;
begin
  case Ord(C) of
    9..13, 32, $A0, $1680, $2000..$200A, $2028, $2029, $202F, $205F, $3000, $FEFF: Result := True;
  else Result := False;
  end;
end;

function TrimMode(Args: TArgs; var Res: TValue; Mode: Integer): Boolean;
var S: PStr; A, B: Integer;
begin
  if not ThisStr(Args, S) then Exit(False);
  A := 0; B := S^.Len;
  if Mode <> 2 then while (A < B) and IsJSWhite(StrChars(S)[A]) do Inc(A);
  if Mode <> 1 then while (B > A) and IsJSWhite(StrChars(S)[B - 1]) do Dec(B);
  if (A = 0) and (B = S^.Len) then Res := MkPtr(S) else Res := SubString(S, A, B - A);
  Result := True;
end;

function StrProto_Trim(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
begin Result := TrimMode(Args, Res, 0); end;
function StrProto_TrimStart(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
begin Result := TrimMode(Args, Res, 1); end;
function StrProto_TrimEnd(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
begin Result := TrimMode(Args, Res, 2); end;

function StrProto_At(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var S: PStr; N: Int64;
begin
  if not ThisStr(Args, S) then Exit(False);
  if not IntArg(Args, Argc, 1, 0, N) then Exit(False);
  S := StrOf(Args[0]);
  if N < 0 then N := N + S^.Len;
  if (N < 0) or (N >= S^.Len) then Res := VUndefined else Res := SubString(S, N, 1);
  Result := True;
end;

{ 22.1.3.15 String.prototype.normalize ( [ form ] ): NFC by default; an ASCII string is every form already }
function StrProto_Normalize(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var S: PStr; F: TValue; Form: UnicodeString; I: Integer; Ascii: Boolean; R: UnicodeString;
begin
  if not ThisStr(Args, S) then Exit(False);
  if Arg(Args, Argc, 1) = VUndefined then Form := 'NFC'
  else
  begin
    if not ToStringV(Args[1], F) then Exit(False);
    Form := StrToUnicode(StrOf(F));
  end;
  if (Form <> 'NFC') and (Form <> 'NFD') and (Form <> 'NFKC') and (Form <> 'NFKD') then
    Exit(ThrowError('RangeError', 'The normalization form should be one of NFC, NFD, NFKC, NFKD.'));
  S := StrOf(Args[0]);
  Ascii := True;
  for I := 0 to S^.Len - 1 do
    if Ord(StrChars(S)[I]) >= 128 then begin Ascii := False; Break; end;
  if Ascii or not Assigned(NormalizeHook) or not NormalizeHook(StrToUnicode(S), Form, R) then Res := Args[0]
  else Res := StrFromUnicode(R);
  Result := True;
end;

{ 22.1.3.19 String.prototype.repeat ( count ) }
function StrProto_Repeat(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var S: PStr; D: Double; N, I, L: Int64; U: UnicodeString;
begin
  if not ThisStr(Args, S) then Exit(False);
  if Arg(Args, Argc, 1) = VUndefined then D := 0
  else if not NumArg(Args, Argc, 1, D) then Exit(False);
  if IsNaN(D) then D := 0;
  if (D < 0) or IsInfinite(D) then Exit(ThrowError('RangeError', 'Invalid count value: ' + FloatToStr(D)));
  S := StrOf(Args[0]);
  N := Trunc(D);
  L := S^.Len;
  if (N = 0) or (L = 0) then begin Res := MkPtr(AtomOf('')); Exit(True); end;
  if L * N > 536870888 then Exit(ThrowError('RangeError', 'Invalid string length'));
  SetLength(U, L * N);
  for I := 0 to N - 1 do Move(StrChars(S)[0], U[I * L + 1], L * SizeOf(WideChar));
  Res := StrFromUnicode(U);
  Result := True;
end;

function StrProto_Pad(Args: TArgs; Argc: Integer; var Res: TValue; AtStart: Boolean): Boolean;
var S, F: PStr; N: Int64; T, Fill, P: UnicodeString;
begin
  if not ThisStr(Args, S) then Exit(False);
  if not IntArg(Args, Argc, 1, 0, N) then Exit(False);
  if (Argc >= 2) and (Args[2] <> VUndefined) then
  begin
    if not StrArg(Args, Argc, 2, F) then Exit(False);
    Fill := StrToUnicode(F);
  end
  else Fill := ' ';
  T := StrToUnicode(StrOf(Args[0]));
  if (N <= Length(T)) or (Fill = '') then begin Res := Args[0]; Exit(True); end;
  P := '';
  while Length(P) < N - Length(T) do P := P + Fill;
  SetLength(P, N - Length(T));
  if AtStart then Res := StrFromUnicode(P + T) else Res := StrFromUnicode(T + P);
  Result := True;
end;
function StrProto_PadStart(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
begin Result := StrProto_Pad(Args, Argc, Res, True); end;
function StrProto_PadEnd(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
begin Result := StrProto_Pad(Args, Argc, Res, False); end;

function StrProto_Split(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var S, Sep: PStr; A: PObj; I, P, Start: Integer; Part: TValue; LimD: Double; Lim: Int64;
begin
  if not ThisStr(Args, S) then Exit(False);
  Lim := 4294967295;
  if Arg(Args, Argc, 2) <> VUndefined then
  begin
    if not ToNumberV(Args[2], LimD) then Exit(False);
    if IsNaN(LimD) or IsInfinite(LimD) then Lim := 0
    else Lim := Int64(Trunc(LimD)) and $FFFFFFFF;   { ToUint32 }
  end;
  { 22.1.3.23 steps 5-6: ToString(separator) before the limit's early return }
  if Arg(Args, Argc, 1) <> VUndefined then
  begin
    if not StrArg(Args, Argc, 1, Sep) then Exit(False);
    Args[1] := MkPtr(Sep);
  end;
  A := NewArray(0, 8);
  PushRoot(MkPtr(A));
  if Lim = 0 then begin Res := RootAt(TempTop - 1); PopRoots(1); Exit(True); end;
  if Arg(Args, Argc, 1) = VUndefined then
  begin
    ArrayPush(ObjOf(RootAt(TempTop - 1)), Args[0]);
    Res := RootAt(TempTop - 1);
    PopRoots(1);
    Exit(True);
  end;
  Sep := StrOf(Args[1]);
  PushRoot(MkPtr(Sep));
  S := StrOf(Args[0]);
  if Sep^.Len = 0 then
  begin
    for I := 0 to S^.Len - 1 do
    begin
      if ArrayLen(ObjOf(RootAt(TempTop - 2))) >= Lim then Break;
      Part := SubString(StrOf(Args[0]), I, 1);
      ArrayPush(ObjOf(RootAt(TempTop - 2)), Part);
    end;
  end
  else
  begin
    Start := 0;
    while True do
    begin
      P := FindIn(StrOf(Args[0]), StrOf(RootAt(TempTop - 1)), Start);
      if P < 0 then Break;
      Part := SubString(StrOf(Args[0]), Start, P - Start);
      ArrayPush(ObjOf(RootAt(TempTop - 2)), Part);
      if ArrayLen(ObjOf(RootAt(TempTop - 2))) >= Lim then
      begin
        Res := RootAt(TempTop - 2);
        PopRoots(2);
        Exit(True);
      end;
      Start := P + StrOf(RootAt(TempTop - 1))^.Len;
    end;
    Part := SubString(StrOf(Args[0]), Start, StrOf(Args[0])^.Len - Start);
    ArrayPush(ObjOf(RootAt(TempTop - 2)), Part);
  end;
  Res := RootAt(TempTop - 2);
  PopRoots(2);
  Result := True;
end;

{ GetSubstitution for a string pattern: no captures, so $n stays literal }
function StringSubstitution(const Matched, Str: UnicodeString; Position: Integer; const Rep: UnicodeString): UnicodeString;
var I: Integer; C: WideChar;
begin
  Result := '';
  I := 1;
  while I <= Length(Rep) do
  begin
    if (Rep[I] = '$') and (I < Length(Rep)) then
    begin
      C := Rep[I + 1];
      case C of
        '$': begin Result := Result + '$'; Inc(I, 2); Continue; end;
        '&': begin Result := Result + Matched; Inc(I, 2); Continue; end;
        '`': begin Result := Result + Copy(Str, 1, Position); Inc(I, 2); Continue; end;
        #39: begin Result := Result + Copy(Str, Position + Length(Matched) + 1, MaxInt); Inc(I, 2); Continue; end;
      end;
    end;
    Result := Result + Rep[I];
    Inc(I);
  end;
end;

{ String.prototype.replace / replaceAll with a string pattern }
function StrProto_Replace(Args: TArgs; Argc: Integer; var Res: TValue; All: Boolean): Boolean;
var S, Pat, Rep: PStr; T, P, R, U: UnicodeString; Positions: array of Integer; N, I, Pos_, Last, Adv: Integer;
  F, CR, M: TValue; CallArgs: array[0..2] of TValue; Functional: Boolean;
begin
  if not ThisStr(Args, S) then Exit(False);
  if not StrArg(Args, Argc, 1, Pat) then Exit(False);
  T := StrToUnicode(StrOf(Args[0]));
  P := StrToUnicode(Pat);
  F := Arg(Args, Argc, 2);
  Functional := IsCallable(F);
  R := '';
  PushRoot(MkPtr(Pat));
  if not Functional then
  begin
    if not StrArg(Args, Argc, 2, Rep) then begin PopRoots(1); Exit(False); end;
    R := StrToUnicode(Rep);
  end;
  Pat := StrOf(RootAt(TempTop - 1));
  PopRoots(1);   { no allocation until the positions are found }
  { the match positions, 0-based }
  N := 0;
  SetLength(Positions, 0);
  Adv := Max(1, Length(P));
  Pos_ := FindIn(StrOf(Args[0]), Pat, 0);
  while Pos_ >= 0 do
  begin
    SetLength(Positions, N + 1);
    Positions[N] := Pos_;
    Inc(N);
    if not All then Break;
    if Pos_ + Adv > Length(T) then Break;
    Pos_ := FindIn(StrOf(Args[0]), Pat, Pos_ + Adv);
  end;
  if N = 0 then begin Res := Args[0]; Exit(True); end;
  U := '';
  Last := 0;
  PushRoot(F); PushRoot(Args[0]);
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
  Res := StrFromUnicode(U);
  Result := True;
end;

function StrProto_ReplaceOne(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
begin Result := StrProto_Replace(Args, Argc, Res, False); end;
function StrProto_ReplaceAll(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
begin Result := StrProto_Replace(Args, Argc, Res, True); end;

function StrProto_Concat(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var S: PStr; I: Integer; V: TValue;
begin
  if not ThisStr(Args, S) then Exit(False);
  Res := Args[0];
  for I := 1 to Argc do
  begin
    PushRoot(Res);
    if not ToStringV(Args[I], V) then begin PopRoots(1); Exit(False); end;
    Res := ConcatStrings(StrOf(RootAt(TempTop - 1)), StrOf(V));
    PopRoots(1);
  end;
  Result := True;
end;

{ 22.1.3.29 String.prototype.toString / 22.1.3.35 valueOf: thisStringValue (a string or a String object) }
function StrProto_ToString(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
begin
  if IsString(Args[0]) then Res := Args[0]
  else if IsObject(Args[0]) and (ObjOf(Args[0])^.OKind = okStringObj) then Res := ObjOf(Args[0])^.Aux1
  else Exit(ThrowTypeError('String.prototype.valueOf requires that ''this'' be a String'));
  Result := True;
end;

function StrProto_LocaleCompare(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var S, T: PStr; C: Integer;
begin
  if not ThisStr(Args, S) then Exit(False);
  if not StrArg(Args, Argc, 1, T) then Exit(False);
  C := StrCompare(StrOf(Args[0]), T);
  if C < 0 then Res := MkInt(-1) else if C > 0 then Res := MkInt(1) else Res := MkInt(0);
  Result := True;
end;

{ ---------------- Number / Boolean ---------------- }

function Number_Ctor(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var D: Double; O: PObj; P: TValue;
begin
  { 21.1.1.1: ToNumeric; a BigInt becomes the Number of its value }
  if Argc = 0 then D := 0
  else
  begin
    P := Args[1];
    if IsObject(P) and not ToPrimitiveHint(P, phNumber, P) then Exit(False);
    if IsBigInt(P) then D := ValueToBig(P).ToDouble
    else if not ToNumberV(P, D) then Exit(False);
  end;
  Res := MkNumber(D);
  if CurNewTarget <> VUndefined then
  begin
    { new Number: a wrapper }
    O := NewObject(RootShapeFor(NumberProto), 0, okNumber);
    O^.Aux1 := Res;
    Res := MkPtr(O);
  end;
  Result := True;
end;

function Number_IsInteger(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var V: TValue; D: Double;
begin
  V := Arg(Args, Argc, 1);
  if not IsNumber(V) then begin Res := VFalse; Exit(True); end;
  D := NumOf(V);
  Res := MkBool(not IsNaN(D) and not IsInfinite(D) and (Frac(D) = 0));
  Result := True;
end;

function ThisNum(Args: TArgs; var D: Double): Boolean;
begin
  if IsNumber(Args[0]) then begin D := NumOf(Args[0]); Exit(True); end;
  if IsObject(Args[0]) and (ObjOf(Args[0])^.OKind = okNumber) then begin D := NumOf(ObjOf(Args[0])^.Aux1); Exit(True); end;
  Result := ThrowTypeError('Number.prototype method called on incompatible receiver');
end;

function NumProto_ToString(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var D: Double; Radix: Int64; N: Int64; Neg: Boolean; S: UnicodeString; Dg: Integer;
begin
  if not ThisNum(Args, D) then Exit(False);
  if not IntArg(Args, Argc, 1, 10, Radix) then Exit(False);
  if (Radix = 10) or IsNaN(D) or IsInfinite(D) or (Frac(D) <> 0) then begin Res := NumberToStr(D); Exit(True); end;
  N := Trunc(D); Neg := N < 0; if Neg then N := -N;
  S := '';
  repeat
    Dg := N mod Radix;
    if Dg < 10 then S := WideChar(48 + Dg) + S else S := WideChar(87 + Dg) + S;
    N := N div Radix;
  until N = 0;
  if Neg then S := '-' + S;
  Res := StrFromUnicode(S);
  Result := True;
end;

function NumProto_ToFixed(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var D: Double; Digits: Int64; FS: TFormatSettings;
begin
  if not ThisNum(Args, D) then Exit(False);
  if not IntArg(Args, Argc, 1, 0, Digits) then Exit(False);
  FS := DefaultFormatSettings;
  FS.DecimalSeparator := '.';
  Res := StrFromUnicode(UnicodeString(FloatToStrF(D, ffFixed, 18, Digits, FS)));
  Result := True;
end;

function NumProto_ValueOf(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var D: Double;
begin
  if not ThisNum(Args, D) then Exit(False);
  Res := MkNumber(D);
  Result := True;
end;

function Boolean_Ctor(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var O: PObj;
begin
  Res := MkBool(ToBool(Arg(Args, Argc, 1)));
  if CurNewTarget <> VUndefined then
  begin
    O := NewObject(RootShapeFor(BooleanProto), 0, okBoolean);
    O^.Aux1 := Res;
    Res := MkPtr(O);
  end;
  Result := True;
end;

{ ---------------- Math ---------------- }

{$linklib m}
{ the C library's math functions: correctly rounded or nearly so, with IEEE 754's signed zeros and infinities
  (FPC's Math unit is neither) }
function c_sin(X: Double): Double; cdecl; external 'm' name 'sin';
function c_cos(X: Double): Double; cdecl; external 'm' name 'cos';
function c_tan(X: Double): Double; cdecl; external 'm' name 'tan';
function c_asin(X: Double): Double; cdecl; external 'm' name 'asin';
function c_acos(X: Double): Double; cdecl; external 'm' name 'acos';
function c_atan(X: Double): Double; cdecl; external 'm' name 'atan';
function c_atan2(Y, X: Double): Double; cdecl; external 'm' name 'atan2';
function c_exp(X: Double): Double; cdecl; external 'm' name 'exp';
function c_log(X: Double): Double; cdecl; external 'm' name 'log';
function c_log2(X: Double): Double; cdecl; external 'm' name 'log2';
function c_log10(X: Double): Double; cdecl; external 'm' name 'log10';
function c_cbrt(X: Double): Double; cdecl; external 'm' name 'cbrt';
function c_hypot(X, Y: Double): Double; cdecl; external 'm' name 'hypot';

function Math1(Args: TArgs; Argc: Integer; var Res: TValue; F: Integer): Boolean;
var D: Double;
begin
  if not NumArg(Args, Argc, 1, D) then Exit(False);
  case F of
    0: D := Abs(D);
    1: D := DFloor(D);
    2: D := DCeil(D);
    3: D := DRound(D);
    4: D := Sqrt(D);
    5: D := c_sin(D);
    6: D := c_cos(D);
    7: D := c_tan(D);
    8: D := c_atan(D);
    9: D := c_exp(D);
    10: D := c_log(D);
    11: if not (IsNaN(D) or IsInfinite(D)) then D := Int(D);
    12: if D > 0 then D := 1 else if D < 0 then D := -1;
    13: D := c_asin(D);
    14: D := c_acos(D);
    15: D := c_log2(D);
    16: D := c_log10(D);
    17: D := c_cbrt(D);
  end;
  Res := MkNumber(D);
  Result := True;
end;

function M_Abs(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
begin
  if IsInt(Arg(Args, Argc, 1)) and (IntOf(Args[1]) <> Low(Int32)) then begin Res := MkInt(Abs(IntOf(Args[1]))); Exit(True); end;
  Result := Math1(Args, Argc, Res, 0);
end;
function M_Floor(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
begin
  if IsInt(Arg(Args, Argc, 1)) then begin Res := Args[1]; Exit(True); end;
  Result := Math1(Args, Argc, Res, 1);
end;
function M_Ceil(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := Math1(Args, Argc, Res, 2); end;
function M_Round(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := Math1(Args, Argc, Res, 3); end;
function M_Sqrt(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := Math1(Args, Argc, Res, 4); end;
function M_Sin(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := Math1(Args, Argc, Res, 5); end;
function M_Cos(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := Math1(Args, Argc, Res, 6); end;
function M_Tan(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := Math1(Args, Argc, Res, 7); end;
function M_Atan(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := Math1(Args, Argc, Res, 8); end;
function M_Exp(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := Math1(Args, Argc, Res, 9); end;
function M_Log(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := Math1(Args, Argc, Res, 10); end;
function M_Trunc(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := Math1(Args, Argc, Res, 11); end;
function M_Sign(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := Math1(Args, Argc, Res, 12); end;
function M_Asin(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := Math1(Args, Argc, Res, 13); end;
function M_Acos(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := Math1(Args, Argc, Res, 14); end;
function M_Log2(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := Math1(Args, Argc, Res, 15); end;
function M_Log10(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := Math1(Args, Argc, Res, 16); end;
function M_Cbrt(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := Math1(Args, Argc, Res, 17); end;

function M_MinMax(Args: TArgs; Argc: Integer; var Res: TValue; IsMax: Boolean): Boolean;
var I: Integer; D, R: Double; AllInt: Boolean; IR: Int32;
begin
  AllInt := Argc > 0;
  for I := 1 to Argc do if not IsInt(Args[I]) then begin AllInt := False; Break; end;
  if AllInt then
  begin
    IR := IntOf(Args[1]);
    for I := 2 to Argc do
      if IsMax then begin if IntOf(Args[I]) > IR then IR := IntOf(Args[I]); end
      else if IntOf(Args[I]) < IR then IR := IntOf(Args[I]);
    Res := MkInt(IR);
    Exit(True);
  end;
  if IsMax then R := NegInfinity else R := Infinity;
  for I := 1 to Argc do
  begin
    if not NumArg(Args, Argc, I, D) then Exit(False);
    if IsNaN(D) then R := NaN
    else if not IsNaN(R) then
      if IsMax then
      begin
        { +0 is larger than -0 }
        if (D > R) or ((D = 0) and (R = 0) and (PQWord(@R)^ <> 0)) then R := D;
      end
      else if (D < R) or ((D = 0) and (R = 0) and (PQWord(@D)^ <> 0)) then R := D;
  end;
  Res := MkNumber(R);
  Result := True;
end;
function M_Max(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := M_MinMax(Args, Argc, Res, True); end;
function M_Min(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := M_MinMax(Args, Argc, Res, False); end;

function M_Pow(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var A, B: Double;
begin
  if not NumArg(Args, Argc, 1, A) then Exit(False);
  if not NumArg(Args, Argc, 2, B) then Exit(False);
  Res := MkNumber(NumberExponentiate(A, B));
  Result := True;
end;

function M_Atan2(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var A, B: Double;
begin
  if not NumArg(Args, Argc, 1, A) then Exit(False);
  if not NumArg(Args, Argc, 2, B) then Exit(False);
  Res := MkNumber(c_atan2(A, B));
  Result := True;
end;

{ 21.3.2.18 Math.hypot: every argument coerced first; an infinity wins over NaN; scaled by the largest magnitude }
function M_Hypot(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var I: Integer; D, S, M: Double; Vals: array of Double; Inf, NaNSeen: Boolean;
begin
  SetLength(Vals, Argc);
  Inf := False; NaNSeen := False; M := 0;
  for I := 1 to Argc do
  begin
    if not NumArg(Args, Argc, I, D) then Exit(False);
    Vals[I - 1] := D;
    if IsInfinite(D) then Inf := True
    else if IsNaN(D) then NaNSeen := True
    else if Abs(D) > M then M := Abs(D);
  end;
  if Inf then begin Res := MkDouble(Infinity); Exit(True); end;
  if NaNSeen then begin Res := MkDouble(NaN); Exit(True); end;
  if M = 0 then begin Res := MkInt(0); Exit(True); end;
  if Argc = 2 then begin Res := MkNumber(c_hypot(Vals[0], Vals[1])); Exit(True); end;
  S := 0;
  for I := 0 to Argc - 1 do S := S + Sqr(Vals[I] / M);
  Res := MkNumber(Sqrt(S) * M);
  Result := True;
end;

const
  MathPI: Double = 3.141592653589793;
  MathE: Double = 2.718281828459045;
  MathLN2: Double = 0.6931471805599453;
  MathLN10: Double = 2.302585092994046;
  MathSQRT2: Double = 1.4142135623730951;

var
  RngState: QWord = 88172645463325252;

function M_Random(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
begin
  RngState := RngState xor (RngState shl 13);
  RngState := RngState xor (RngState shr 7);
  RngState := RngState xor (RngState shl 17);
  Res := MkDouble((RngState shr 11) * (1.0 / 9007199254740992.0));
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

function JSONStr(V: TValue; var Out_: UnicodeString; var Skip: Boolean; Depth: Integer): Boolean;
var Keys, X, TS: TValue; I, N: Integer; S: UnicodeString; First, Sk: Boolean; K: PStr; F: TValue;
begin
  Skip := False;
  if Depth > 200 then Exit(ThrowTypeError('Converting circular structure to JSON'));
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
  if IsString(V) then begin Out_ := JSONQuote(StrToUnicode(StrOf(V))); Exit(True); end;
  if IsNumber(V) then
  begin
    if IsNaN(NumOf(V)) or IsInfinite(NumOf(V)) then Out_ := 'null'
    else begin if not ToStringV(V, TS) then Exit(False); Out_ := StrToUnicode(StrOf(TS)); end;
    Exit(True);
  end;
  case V of
    VNull: begin Out_ := 'null'; Exit(True); end;
    VTrue: begin Out_ := 'true'; Exit(True); end;
    VFalse: begin Out_ := 'false'; Exit(True); end;
  end;
  if not IsObject(V) or IsCallable(V) then begin Skip := True; Exit(True); end;
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
      if not JSONStr(X, Out_, Sk, Depth + 1) then begin PopRoots(1); Exit(False); end;
      if Sk then S := S + 'null' else S := S + Out_;
    end;
    Out_ := S + ']';
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
    if not JSONStr(X, Out_, Sk, Depth + 1) then begin PopRoots(2); Exit(False); end;
    if Sk then Continue;
    if not First then S := S + ',';
    First := False;
    S := S + JSONQuote(StrToUnicode(K)) + ':' + Out_;
  end;
  Out_ := S + '}';
  PopRoots(2);
  Result := True;
end;

function JSON_Stringify(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var S: UnicodeString; Skip: Boolean;
begin
  if not JSONStr(Arg(Args, Argc, 1), S, Skip, 0) then Exit(False);
  if Skip then Res := VUndefined else Res := StrFromUnicode(S);
  Result := True;
end;

type
  TJSONParser = record
    S: UnicodeString;
    P: Integer;
  end;

function JParse(var J: TJSONParser; var Res: TValue): Boolean;
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

function JSON_Parse(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var S: PStr; J: TJSONParser;
begin
  if not StrArg(Args, Argc, 1, S) then Exit(False);
  J.S := StrToUnicode(S);
  J.P := 1;
  Result := JParse(J, Res);
end;

{ ---------------- Error ---------------- }

threadvar
  AtomCause, AtomErrors: PStr;

{ 20.5.1.1 Error(message, options) and 20.5.6.1.1 NativeError: OrdinaryCreateFromConstructor, the message
  (Args[MsgIdx]) and InstallErrorCause (20.5.8.1) from the options after it }
function MakeErrorCtorAt(Proto: TValue; Args: TArgs; Argc, MsgIdx: Integer; var Res: TValue): Boolean;
var M, Opt: TValue; O: PObj; P: TValue; Found: Boolean;
begin
  if not ProtoFromNewTarget(Proto, P) then Exit(False);
  PushRoot(P);
  O := NewObject(RootShapeFor(P), 4, okError);
  PopRoots(1);
  PushRoot(MkPtr(O));
  if (Argc >= MsgIdx) and (Args[MsgIdx] <> VUndefined) then
  begin
    if not ToStringV(Args[MsgIdx], M) then begin PopRoots(1); Exit(False); end;
    AddProperty(ObjOf(RootAt(TempTop - 1)), AtomMessage, M, pfHidden);
  end;
  if (Argc >= MsgIdx + 1) and IsObject(Args[MsgIdx + 1]) then
  begin
    Opt := Args[MsgIdx + 1];
    if not HasPropertyV(Opt, AtomCause, Found) then begin PopRoots(1); Exit(False); end;
    if Found then
    begin
      if not GetV(Args[MsgIdx + 1], AtomCause, M) then begin PopRoots(1); Exit(False); end;
      DefineOwn(RootAt(TempTop - 1), AtomCause, M, pfHidden);
    end;
  end;
  Res := RootAt(TempTop - 1);
  PopRoots(1);
  Result := True;
end;

function MakeErrorCtor(Proto: TValue; Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
begin
  Result := MakeErrorCtorAt(Proto, Args, Argc, 1, Res);
end;

function Error_Ctor(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
begin Result := MakeErrorCtor(ErrorProto, Args, Argc, Res); end;
function TypeError_Ctor(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
begin Result := MakeErrorCtor(TypeErrorProto, Args, Argc, Res); end;
function RangeError_Ctor(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
begin Result := MakeErrorCtor(RangeErrorProto, Args, Argc, Res); end;
function SyntaxError_Ctor(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
begin Result := MakeErrorCtor(SyntaxErrorProto, Args, Argc, Res); end;
function ReferenceError_Ctor(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
begin Result := MakeErrorCtor(ReferenceErrorProto, Args, Argc, Res); end;
function EvalError_Ctor(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
begin Result := MakeErrorCtor(EvalErrorProto, Args, Argc, Res); end;
function URIError_Ctor(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
begin Result := MakeErrorCtor(URIErrorProto, Args, Argc, Res); end;

threadvar
  AggregateErrorProto, ErrorCtorV: TValue;

{ 20.5.7.1.1 AggregateError(errors, message, options): the errors property is the list the iterable yields }
function AggregateError_Ctor(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var It, V: TValue; Done: Boolean;
begin
  if not MakeErrorCtorAt(AggregateErrorProto, Args, Argc, 2, Res) then Exit(False);
  PushRoot(Res);
  PushRoot(MkPtr(NewArray(0, 4)));
  if not GetIterator(Arg(Args, Argc, 1), It) then begin PopRoots(2); Exit(False); end;
  PushRoot(It);
  while True do
  begin
    if not IteratorStep(RootAt(TempTop - 1), V, Done) then begin PopRoots(3); Exit(False); end;
    if Done then Break;
    ArrayPush(ObjOf(RootAt(TempTop - 2)), V);
  end;
  DefineOwn(RootAt(TempTop - 3), AtomErrors, RootAt(TempTop - 2), pfHidden);
  Res := RootAt(TempTop - 3);
  PopRoots(3);
  Result := True;
end;

function ErrProto_ToString(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var N, M, SN, SM: TValue;
begin
  if not IsObject(Args[0]) then Exit(ThrowTypeError('Error.prototype.toString called on a non-object'));
  if not GetV(Args[0], AtomName, N) then Exit(False);
  PushRoot(N);
  if not GetV(Args[0], AtomMessage, M) then begin PopRoots(1); Exit(False); end;
  PushRoot(M);
  if RootAt(TempTop - 2) = VUndefined then SN := StrFromUnicode('Error')
  else if not ToStringV(RootAt(TempTop - 2), SN) then begin PopRoots(2); Exit(False); end;
  if RootAt(TempTop - 1) = VUndefined then SM := MkPtr(AtomOf(''))
  else if not ToStringV(RootAt(TempTop - 1), SM) then begin PopRoots(2); Exit(False); end;
  PopRoots(2);
  if StrOf(SM)^.Len = 0 then Res := SN
  else if StrOf(SN)^.Len = 0 then Res := SM
  else Res := StrFromUnicode(StrToUnicode(StrOf(SN)) + ': ' + StrToUnicode(StrOf(SM)));
  Result := True;
end;

{ ---------------- Symbol ---------------- }

var
  SymbolCounter: Integer = 0;

function NewSymbol(const Desc: UnicodeString): TValue;
var C: PByte;
begin
  C := GetMem(24);
  PCell(C)^.Size := 24;
  PCell(C)^.Kind := ckSymbol;
  PCell(C)^.Flags := cfPermanent or cfOld;
  PCell(C)^.Extra := 0;
  Inc(SymbolCounter);
  PValue(C + 8)^ := MkPtr(AtomOf(Desc));
  PPointer(C + 16)^ := NewKeyAtom(Desc, kfSymbol);
  Result := MkPtr(C);
  KeyPayload(PStr(PPointer(C + 16)^))^ := Result;   { symbols are permanent cells }
end;

function NewSymbolValue(const Desc: UnicodeString): TValue;
begin
  Result := NewSymbol(Desc);
end;

function Symbol_Ctor(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var S: PStr;
begin
  if CurNewTarget <> VUndefined then Exit(ThrowTypeError('Symbol is not a constructor'));
  if (Argc >= 1) and (Args[1] <> VUndefined) then
  begin
    if not StrArg(Args, Argc, 1, S) then Exit(False);
    Res := NewSymbol(StrToUnicode(S));
  end
  else
  begin
    Res := NewSymbol('');
    PCell(PtrOf(Res))^.Extra := 1;   { [[Description]] undefined }
  end;
  Result := True;
end;

function SymProto_ToString(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
begin
  Res := StrFromUnicode('Symbol(' + StrToUnicode(StrOf(PValue(PByte(PtrOf(Args[0])) + 8)^)) + ')');
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

function KeyHash(K: TValue; var ByAddr: Boolean): UInt32;
var D: Double; Q: QWord;
begin
  ByAddr := False;
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
      ckSymbol: ;   { permanent: its address never changes }
    else ByAddr := True;
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
procedure MapIndexInsert(Ix: PValueArray; E: PValueArray; N: Integer);
var Mask, I: Integer; H: UInt32; ByAddr: Boolean;
begin
  Mask := IntOf(Ix^.Data[MIX_MASK]);
  H := KeyHash(E^.Data[N * 2], ByAddr);
  if ByAddr then Ix^.Data[MIX_ADDR] := MkInt(1);
  I := Integer(H and UInt32(Mask));
  while Ix^.Data[MIX_SLOTS + I] <> MkInt(0) do I := (I + 1) and Mask;
  Ix^.Data[MIX_SLOTS + I] := MkInt(N + 1);
end;

{ a fresh index for M sized for Want entries; M stays rooted by the caller's stack slot or roots }
procedure MapRebuild(var M: TValue; Want: Integer);
var Ix, E: PValueArray; Cap, I, N, Count: Integer;
begin
  Cap := 8;
  while Cap < Want * 2 + 2 do Cap := Cap * 2;
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

function NewMapLike(Proto: TValue; Kind: Byte): TValue;
var O: PObj; E, Ix: PValueArray; I: Integer;
begin
  PushRoot(Proto);
  E := NewValueArray(16);
  PushRoot(MkPtr(E));
  Ix := NewValueArray(MIX_SLOTS + 8);
  PushRoot(MkPtr(Ix));
  O := NewObject(RootShapeFor(RootAt(TempTop - 3)), 0, Kind);
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

{ 24.1.1.2 AddEntriesFromIterable (Pairs: [key, value] entry objects) / 24.2.1.1 Set step 8: Adder called on
  Target for each value of Iterable; an abrupt completion closes the iterator }
function AddFromIterable(Target, Iterable, Adder: TValue; Pairs: Boolean): Boolean;
var It, NextM, V, R: TValue; Done: Boolean; Base, N: Integer; A: array[0..1] of TValue;
  function Fail(Close: Boolean): Boolean;
  begin
    if Close then IteratorCloseV(RootAt(Base + 2), True);
    PopRoots(TempTop - Base);
    Result := False;
  end;
begin
  Base := TempTop;
  PushRoot(Target); PushRoot(Adder);
  if not GetIteratorRec(Iterable, It, NextM) then Exit(Fail(False));
  PushRoot(It); PushRoot(NextM);
  while True do
  begin
    if not IteratorStepRec(RootAt(Base + 2), RootAt(Base + 3), V, Done) then Exit(Fail(False));
    if Done then Break;
    PushRoot(V);
    if Pairs then
    begin
      if not IsObject(V) then
      begin
        ThrowTypeError('Iterator value is not an entry object');
        Exit(Fail(True));
      end;
      if not GetElemV(V, MkInt(0), R) then Exit(Fail(True));
      PushRoot(R);
      if not GetElemV(RootAt(Base + 4), MkInt(1), V) then Exit(Fail(True));
      A[0] := RootAt(Base + 5); A[1] := V;
      N := 2;
    end
    else
    begin
      A[0] := V;
      N := 1;
    end;
    { CallValue copies the arguments before it allocates }
    if not CallValue(RootAt(Base + 1), RootAt(Base), @A[0], N, R) then Exit(Fail(True));
    PopRoots(TempTop - Base - 4);
  end;
  PopRoots(4);
  Result := True;
end;

{ 24.1.1.1 Map ( [ iterable ] ) / 24.2.1.1 Set ( [ iterable ] ): Kind okMap or okSet }
function MapLikeCtor(Args: TArgs; Argc: Integer; Kind: Byte; var Res: TValue): Boolean;
var P, Adder: TValue;
begin
  if CurNewTarget = VUndefined then
  begin
    if Kind = okMap then Exit(ThrowTypeError('Constructor Map requires ''new'''));
    Exit(ThrowTypeError('Constructor Set requires ''new'''));
  end;
  if Kind = okMap then P := MapProto else P := SetProtoObj;
  if not ProtoFromNewTarget(P, P) then Exit(False);
  Res := NewMapLike(P, Kind);
  if (Argc < 1) or IsNullish(Args[1]) then Exit(True);
  PushRoot(Res);
  if Kind = okMap then Result := GetV(Res, AtomOf('set'), Adder) else Result := GetV(Res, AtomOf('add'), Adder);
  if Result and not IsCallable(Adder) then
  begin
    if Kind = okMap then Result := ThrowTypeError('Map.prototype.set is not a function')
    else Result := ThrowTypeError('Set.prototype.add is not a function');
  end;
  if Result then Result := AddFromIterable(RootAt(TempTop - 1), Args[1], Adder, Kind = okMap);
  Res := RootAt(TempTop - 1);
  PopRoots(1);
end;

function Map_Ctor(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
begin
  Result := MapLikeCtor(Args, Argc, okMap, Res);
end;

function Set_Ctor(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
begin
  Result := MapLikeCtor(Args, Argc, okSet, Res);
end;

function ThisMap(Args: TArgs; Kind: Byte): Boolean; inline;
begin
  Result := IsObject(Args[0]) and (ObjOf(Args[0])^.OKind = Kind);
  if not Result then ThrowTypeError('incompatible receiver');
end;

{ the methods Map.prototype and Set.prototype share check their own brand: a Set's have NativeData 1 (read
  before anything runs: a nested native call overwrites it) }
function MapLikeThis(Args: TArgs): Boolean;
var Kind: Byte;
begin
  if NativeData = 1 then Kind := okSet else Kind := okMap;
  Result := IsObject(Args[0]) and (ObjOf(Args[0])^.OKind = Kind);
  if not Result then ThrowTypeError('incompatible receiver');
end;

function MapProto_Get(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var I: Integer;
begin
  if not ThisMap(Args, okMap) then Exit(False);
  MapEnsureIndex(Args[0]);
  I := MapFind(Args[0], Arg(Args, Argc, 1));
  if I < 0 then Res := VUndefined else Res := MapEntries(Args[0])^.Data[I + 1];
  Result := True;
end;

function MapProto_Set(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var I: Integer; K: TValue;
begin
  if not ThisMap(Args, okMap) then Exit(False);
  MapEnsureIndex(Args[0]);
  K := Arg(Args, Argc, 1);
  if IsNumber(K) then begin if NumOf(K) = 0 then K := MkInt(0) else K := MkNumber(NumOf(K)); end;
  I := MapFind(Args[0], K);
  if I >= 0 then ValueArrayStore(MapEntries(Args[0]), I + 1, Arg(Args, Argc, 2))
  else MapAppend(Args[0], K, Arg(Args, Argc, 2));
  Res := Args[0];
  Result := True;
end;

function MapProto_Has(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
begin
  if not MapLikeThis(Args) then Exit(False);
  MapEnsureIndex(Args[0]);
  Res := MkBool(MapFind(Args[0], Arg(Args, Argc, 1)) >= 0);
  Result := True;
end;

function MapProto_Delete(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var I: Integer; E: PValueArray;
begin
  if not MapLikeThis(Args) then Exit(False);
  MapEnsureIndex(Args[0]);
  I := MapFind(Args[0], Arg(Args, Argc, 1));
  if I < 0 then begin Res := VFalse; Exit(True); end;
  E := MapEntries(Args[0]);
  E^.Data[I] := VHole;   { the index keeps pointing here: probes skip the hole }
  E^.Data[I + 1] := VUndefined;
  SetMapCount(Args[0], MapCount(Args[0]) - 1);
  Res := VTrue;
  Result := True;
end;

function MapProto_Clear(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var E: PValueArray; I: Integer;
begin
  if not MapLikeThis(Args) then Exit(False);
  E := MapEntries(Args[0]);
  for I := 0 to Integer(E^.Len) - 1 do E^.Data[I] := VUndefined;
  E^.Len := 0;
  SetMapCount(Args[0], 0);
  MapRebuild(Args[0], 4);
  Res := VUndefined;
  Result := True;
end;

function MapProto_Size(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
begin
  if not MapLikeThis(Args) then Exit(False);
  Res := MkInt(MapCount(Args[0]));
  Result := True;
end;

function SetProto_Add(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var K: TValue;
begin
  if not ThisMap(Args, okSet) then Exit(False);
  MapEnsureIndex(Args[0]);
  K := Arg(Args, Argc, 1);
  if IsNumber(K) then begin if NumOf(K) = 0 then K := MkInt(0) else K := MkNumber(NumOf(K)); end;
  if MapFind(Args[0], K) < 0 then MapAppend(Args[0], K, K);
  Res := Args[0];
  Result := True;
end;

function MapProto_ForEach(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var I: Integer; E: PValueArray; CallArgs: array[0..2] of TValue; R: TValue;
begin
  if not MapLikeThis(Args) then Exit(False);
  if not IsCallable(Arg(Args, Argc, 1)) then Exit(ThrowTypeError('forEach callback is not a function'));
  I := 0;
  while True do
  begin
    E := MapEntries(Args[0]);
    if I >= Integer(E^.Len) then Break;
    if E^.Data[I] <> VHole then
    begin
      CallArgs[0] := E^.Data[I + 1]; CallArgs[1] := E^.Data[I]; CallArgs[2] := Args[0];
      if not CallValue(Arg(Args, Argc, 1), Arg(Args, Argc, 2), @CallArgs[0], 3, R) then Exit(False);
    end;
    Inc(I, 2);
  end;
  Res := VUndefined;
  Result := True;
end;

threadvar
  MapIterProto, SetIterProto: TValue;

{ 24.1.5.1 CreateMapIterator / 24.2.6.1 CreateSetIterator: a live position in the entry list, which deletions
  leave holes in and additions append to (okIterator, OFlags 5 keys, 6 values, 7 entries; a Set's values 8,
  entries 9) }
function MapIterKind(Args: TArgs; Kind: Integer; var Res: TValue): Boolean;
var O: PObj; IsSet: Boolean;
begin
  if not MapLikeThis(Args) then Exit(False);
  IsSet := ObjOf(Args[0])^.OKind = okSet;
  if IsSet then O := NewObject(RootShapeFor(SetIterProto), 0, okIterator)
  else O := NewObject(RootShapeFor(MapIterProto), 0, okIterator);
  O^.Aux1 := Args[0];
  O^.Aux2 := MkInt(0);
  if IsSet then begin if Kind = 2 then O^.OFlags := 9 else O^.OFlags := 8; end
  else O^.OFlags := 5 + Kind;
  Res := MkPtr(O);
  Result := True;
end;

function MapSetIterNext(Args: TArgs; Lo, Hi: Integer; var Res: TValue): Boolean;
var V: TValue; Done: Boolean; O: PObj;
begin
  if not IsObject(Args[0]) or (ObjOf(Args[0])^.OKind <> okIterator) or (ObjOf(Args[0])^.OFlags < Lo) or
     (ObjOf(Args[0])^.OFlags > Hi) then
    Exit(ThrowTypeError('next called on an object that is not an iterator of this kind'));
  if not IteratorStep(Args[0], V, Done) then Exit(False);
  PushRoot(V);
  O := NewPlainObject(ObjectProto, 2);
  V := RootAt(TempTop - 1);
  PopRoots(1);
  AddProperty(O, AtomValue, V, pfDefault);
  AddProperty(O, AtomDone, MkBool(Done), pfDefault);
  Res := MkPtr(O);
  Result := True;
end;

function MapIter_Next(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
begin Result := MapSetIterNext(Args, 5, 7, Res); end;
function SetIter_Next(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
begin Result := MapSetIterNext(Args, 8, 9, Res); end;

function MapProto_Keys(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := MapIterKind(Args, 0, Res); end;
function MapProto_Values(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := MapIterKind(Args, 1, Res); end;
function MapProto_Entries(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := MapIterKind(Args, 2, Res); end;

{ ---------------- setup ---------------- }

function NewProtoObject(Proto: TValue): TValue;
begin
  Result := MkPtr(NewPlainObject(Proto, 8));
end;

function MakeCtor(Fn: TNativeFn; const Name: UnicodeString; Length: Integer; Proto: TValue): TValue;
var C: TValue;
begin
  PushRoot(Proto);
  C := NewNativeFunction(Fn, Name, Length);
  PushRoot(C);
  DefineOwn(C, AtomPrototype, RootAt(TempTop - 2), 0);
  DefineOwn(RootAt(TempTop - 2), AtomConstructor, RootAt(TempTop - 1), pfHidden);
  DefineOwn(GlobalObj, AtomOf(Name), RootAt(TempTop - 1), pfHidden);
  Result := RootAt(TempTop - 1);
  PopRoots(2);
end;

procedure DefineGetter(O: TValue; const Name: UnicodeString; Fn: TNativeFn);
var F: TValue;
begin
  PushRoot(O);
  F := NewNativeFunction(Fn, 'get ' + Name, 0);
  F := MkPtr(NewAccessor(F, VUndefined));
  DefineOwn(RootAt(TempTop - 1), AtomOf(Name), F, pfConfigurable or pfAccessor);
  PopRoots(1);
end;

procedure DefineGetterData(O: TValue; const Name: UnicodeString; Fn: TNativeFn; Data: UInt32);
begin
  PushRoot(O);
  PushRoot(NewNativeFunctionData(Fn, 'get ' + Name, 0, Data));
  PushRoot(MkPtr(NewAccessor(RootAt(TempTop - 1), VUndefined)));
  DefineOwn(RootAt(TempTop - 3), AtomOf(Name), RootAt(TempTop - 1), pfConfigurable or pfAccessor);
  PopRoots(3);
end;

threadvar
  MathObj, JSONObj, ConsoleObj, PerfObj, DateObj, CtorTmp, ArrayValuesFnRoot: TValue;

procedure InitBuiltins;
var C, P, G: TValue;
  procedure N(O: TValue; const Name: UnicodeString; Fn: TNativeFn; Len: Integer);
  begin
    DefineNative(O, Name, Fn, Len);
  end;
  { a Set.prototype method shared with Map.prototype (MapLikeThis) }
  procedure NS(O: TValue; const Name: UnicodeString; Fn: TNativeFn; Len: Integer);
  begin
    PushRoot(O);
    PushRoot(NewNativeFunctionData(Fn, Name, Len, 1));
    DefineOwn(RootAt(TempTop - 2), AtomOf(Name), RootAt(TempTop - 1), pfHidden);
    PopRoots(2);
  end;
  function ErrorType(const Name: UnicodeString; Fn: TNativeFn; Parent: TValue): TValue;
  var EP, Ctor: TValue;
  begin
    EP := NewProtoObject(Parent);
    CtorTmp := EP;
    DefineHidden(CtorTmp, 'name', MkPtr(AtomOf(Name)));
    DefineHidden(CtorTmp, 'message', MkPtr(AtomOf('')));
    Ctor := MakeCtor(Fn, Name, 1, CtorTmp);
    { 20.5.6.2: a NativeError constructor's [[Prototype]] is %Error% }
    if Parent = ErrorProto then SetProto(ObjOf(Ctor), ErrorCtorV);
    Result := CtorTmp;
  end;
begin
  AddRoot(@ObjectProto); AddRoot(@FunctionProto); AddRoot(@ArrayProto); AddRoot(@StringProto);
  AddRoot(@NumberProto); AddRoot(@BooleanProto); AddRoot(@ErrorProto); AddRoot(@IteratorProto);
  AddRoot(@ArrayIteratorProto); AddRoot(@GeneratorProto); AddRoot(@MapProto); AddRoot(@SetProtoObj);
  AddRoot(@SymbolProto); AddRoot(@TypeErrorProto); AddRoot(@RangeErrorProto); AddRoot(@SyntaxErrorProto);
  AddRoot(@EvalErrorProto); AddRoot(@URIErrorProto); AddRoot(@AggregateErrorProto); AddRoot(@ErrorCtorV);
  AddRoot(@MapIterProto); AddRoot(@SetIterProto); AddRoot(@MapIterNextFn); AddRoot(@SetIterNextFn);
  AddRoot(@ReferenceErrorProto); AddRoot(@MathObj); AddRoot(@JSONObj); AddRoot(@ConsoleObj); AddRoot(@PerfObj);
  AddRoot(@DateObj); AddRoot(@CtorTmp); AddRoot(@ErrorProtoType); AddRoot(@SymIterator); AddRoot(@SortCmp);
  RealmVals([@AggregateErrorProto, @ErrorCtorV, @MapIterProto, @SetIterProto, @MathObj, @JSONObj, @ConsoleObj,
    @PerfObj, @DateObj, @CtorTmp, @ArrayValuesFnRoot]);

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

  AddRoot(@SymHasInstance);
  { the well-known symbols (6.1.5.1), made once (all realms share them); symbols are permanent cells }
  if not RealmInit then
  begin
  SymIterator := NewSymbol('Symbol.iterator');
  SymHasInstance := NewSymbol('Symbol.hasInstance');
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
  end;

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
  N(ObjectProto, 'toLocaleString', ObjProto_ToLocaleString, 0);
  N(ObjectProto, 'valueOf', ObjProto_ValueOf, 0);
  N(ObjectProto, 'isPrototypeOf', ObjProto_IsPrototypeOf, 1);

  { Function }
  C := MakeCtor(Object_Ctor, 'Function', 1, FunctionProto);
  N(FunctionProto, 'call', FuncProto_Call, 1);
  { not writable, enumerable or configurable }
  AddRoot(@FuncHasInstanceFn);
  FuncHasInstanceFn := NewNativeFunction(FuncProto_HasInstance, '[Symbol.hasInstance]', 1);
  DefineOwn(FunctionProto, SymKey(SymHasInstance), FuncHasInstanceFn, 0);
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
  DefineOwn(ArrayIteratorProto, SymKey(SymToStringTagW), MkPtr(AtomOf('Array Iterator')), pfConfigurable);
  { %StringIteratorPrototype% }
  AddRoot(@StringIterProto);
  StringIterProto := NewProtoObject(IteratorProto);
  AddRoot(@StringIterNextFn);
  StringIterNextFn := NewNativeFunctionData(ArrIter_Next, 'next', 0, 1);
  DefineOwn(StringIterProto, AtomNext, StringIterNextFn, pfHidden);
  DefineOwn(StringIterProto, SymKey(SymToStringTagW), MkPtr(AtomOf('String Iterator')), pfConfigurable);
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
  N(StringProto, 'padStart', StrProto_PadStart, 1);
  N(StringProto, 'padEnd', StrProto_PadEnd, 1);
  N(StringProto, 'isWellFormed', StrProto_IsWellFormed, 0);
  N(StringProto, 'toWellFormed', StrProto_ToWellFormed, 0);
  N(StringProto, 'split', StrProto_Split, 2);
  N(StringProto, 'replace', StrProto_ReplaceOne, 2);
  N(StringProto, 'replaceAll', StrProto_ReplaceAll, 2);
  N(StringProto, 'concat', StrProto_Concat, 1);
  N(StringProto, 'toString', StrProto_ToString, 0);
  N(StringProto, 'valueOf', StrProto_ToString, 0);
  N(StringProto, 'localeCompare', StrProto_LocaleCompare, 1);
  G := NewNativeFunction(StrProto_Iterator, '[Symbol.iterator]', 0);
  DefineOwn(StringProto, PStr(PPointer(PByte(PtrOf(SymIterator)) + 16)^), G, pfHidden);

  { Number, Boolean, Symbol }
  C := MakeCtor(Number_Ctor, 'Number', 1, NumberProto);
  CtorTmp := C;
  N(CtorTmp, 'isInteger', Number_IsInteger, 1);
  N(CtorTmp, 'isFinite', G_IsFinite, 1);
  N(CtorTmp, 'isNaN', G_IsNaN, 1);
  N(CtorTmp, 'parseFloat', G_ParseFloat, 1);
  N(CtorTmp, 'parseInt', G_ParseInt, 2);
  { 21.1.2: the constants are not writable, enumerable or configurable }
  DefineOwn(CtorTmp, AtomOf('MAX_SAFE_INTEGER'), MkDouble(9007199254740991.0), 0);
  DefineOwn(CtorTmp, AtomOf('MIN_SAFE_INTEGER'), MkDouble(-9007199254740991.0), 0);
  DefineOwn(CtorTmp, AtomOf('MAX_VALUE'), MkDouble(MaxDouble), 0);
  DefineOwn(CtorTmp, AtomOf('EPSILON'), MkDouble(2.220446049250313e-16), 0);
  DefineOwn(CtorTmp, AtomOf('POSITIVE_INFINITY'), MkDouble(Infinity), 0);
  DefineOwn(CtorTmp, AtomOf('NEGATIVE_INFINITY'), MkDouble(NegInfinity), 0);
  DefineOwn(CtorTmp, AtomOf('NaN'), MkDouble(NaN), 0);
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
  { 24.1.3.12: Map.prototype[@@iterator] is the entries function }
  GetV(MapProto, AtomOf('entries'), G);
  DefineOwn(MapProto, PStr(PPointer(PByte(PtrOf(SymIterator)) + 16)^), G, pfHidden);
  MapIterProto := NewProtoObject(IteratorProto);
  N(MapIterProto, 'next', MapIter_Next, 0);
  GetV(MapIterProto, AtomNext, MapIterNextFn);
  DefineOwn(MapIterProto, SymKey(SymToStringTagW), MkPtr(AtomOf('Map Iterator')), pfConfigurable);
  SetIterProto := NewProtoObject(IteratorProto);
  N(SetIterProto, 'next', SetIter_Next, 0);
  GetV(SetIterProto, AtomNext, SetIterNextFn);
  DefineOwn(SetIterProto, SymKey(SymToStringTagW), MkPtr(AtomOf('Set Iterator')), pfConfigurable);
  C := MakeCtor(Set_Ctor, 'Set', 0, SetProtoObj);
  N(SetProtoObj, 'add', SetProto_Add, 1);
  NS(SetProtoObj, 'has', MapProto_Has, 1);
  NS(SetProtoObj, 'delete', MapProto_Delete, 1);
  NS(SetProtoObj, 'clear', MapProto_Clear, 0);
  NS(SetProtoObj, 'forEach', MapProto_ForEach, 1);
  NS(SetProtoObj, 'values', MapProto_Keys, 0);
  { 24.2.4.11 / 24.2.4.18: keys and @@iterator are the values function }
  GetV(SetProtoObj, AtomOf('values'), G);
  DefineOwn(SetProtoObj, AtomOf('keys'), G, pfHidden);
  NS(SetProtoObj, 'entries', MapProto_Entries, 0);
  DefineGetterData(SetProtoObj, 'size', MapProto_Size, 1);
  GetV(SetProtoObj, AtomOf('values'), G);
  DefineOwn(SetProtoObj, PStr(PPointer(PByte(PtrOf(SymIterator)) + 16)^), G, pfHidden);

  { Errors }
  DefineHidden(ErrorProto, 'name', MkPtr(AtomOf('Error')));
  DefineHidden(ErrorProto, 'message', MkPtr(AtomOf('')));
  N(ErrorProto, 'toString', ErrProto_ToString, 0);
  C := MakeCtor(Error_Ctor, 'Error', 1, ErrorProto);
  ErrorCtorV := C;
  AtomCause := AtomOf('cause');
  AtomErrors := AtomOf('errors');
  TypeErrorProto := ErrorType('TypeError', TypeError_Ctor, ErrorProto);
  RangeErrorProto := ErrorType('RangeError', RangeError_Ctor, ErrorProto);
  SyntaxErrorProto := ErrorType('SyntaxError', SyntaxError_Ctor, ErrorProto);
  ReferenceErrorProto := ErrorType('ReferenceError', ReferenceError_Ctor, ErrorProto);
  EvalErrorProto := ErrorType('EvalError', EvalError_Ctor, ErrorProto);
  URIErrorProto := ErrorType('URIError', URIError_Ctor, ErrorProto);
  AggregateErrorProto := ErrorType('AggregateError', AggregateError_Ctor, ErrorProto);
  { AggregateError's length is 2 }
  GetV(GlobalObj, AtomOf('AggregateError'), C);
  DefineOwn(C, AtomOf('length'), MkInt(2), pfConfigurable);

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
  { 21.3.1: the value properties are not writable, enumerable or configurable }
  DefineOwn(MathObj, AtomOf('PI'), MkDouble(MathPI), 0);
  DefineOwn(MathObj, AtomOf('E'), MkDouble(MathE), 0);
  DefineOwn(MathObj, AtomOf('LN2'), MkDouble(MathLN2), 0);
  DefineOwn(MathObj, AtomOf('LN10'), MkDouble(MathLN10), 0);
  DefineOwn(MathObj, AtomOf('SQRT2'), MkDouble(MathSQRT2), 0);

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
