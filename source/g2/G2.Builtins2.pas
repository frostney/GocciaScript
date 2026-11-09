unit G2.Builtins2;
{ Generators, __proto__, BigInt, ArrayBuffer, typed arrays and DataView. }
{$mode delphiunicode}{$H+}
{$MINFPCONSTPREC 64}
{$POINTERMATH ON}
{$Q-}{$R-}

interface

procedure InitBuiltins2;

implementation

uses
  SysUtils, Math, BigInteger, G2.Core, G2.Bytecode, G2.VM, G2.BigIntV, G2.Typed, G2.Builtins;

type
  TArgs = PValue;

threadvar
  TAProtos: array[0..8] of TValue;
  TypedArrayProto, ArrayBufferProto, DataViewProto, Scratch: TValue;

function Arg(Args: TArgs; Argc, I: Integer): TValue; inline;
begin
  if I <= Argc then Result := Args[I] else Result := VUndefined;
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
  if IsNaN(D) then R := 0 else if D > 9e15 then R := High(Int32) else if D < -9e15 then R := Low(Int32) else R := Trunc(D);
  Result := True;
end;

function MakeIterResult(Value: TValue; Done: Boolean): TValue;
var O: PObj;
begin
  PushRoot(Value);
  O := NewPlainObject(ObjectProto, 2);
  AddProperty(O, AtomValue, RootAt(TempTop - 1), pfDefault);
  AddProperty(O, AtomDone, MkBool(Done), pfDefault);
  PopRoots(1);
  Result := MkPtr(O);
end;

{ ---------------- generators ---------------- }

function GenResumeNative(Args: TArgs; Argc: Integer; var Res: TValue; Mode: Integer): Boolean;
var V: TValue; Done: Boolean;
begin
  GenYieldRaw := False;
  if not GeneratorResume(Args[0], Arg(Args, Argc, 1), Mode, V, Done) then Exit(False);
  if GenYieldRaw then begin GenYieldRaw := False; Res := V; Exit(True); end;
  Res := MakeIterResult(V, Done);
  Result := True;
end;
function Gen_Next(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := GenResumeNative(Args, Argc, Res, 0); end;
function Gen_Throw(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := GenResumeNative(Args, Argc, Res, 1); end;
function Gen_Return(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := GenResumeNative(Args, Argc, Res, 2); end;

{ ---------------- __proto__ ---------------- }

function Proto_Get(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
begin
  if IsObject(Args[0]) then Res := ObjOf(Args[0])^.Shape^.Proto else Res := VNull;
  Result := True;
end;

function Proto_Set(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var P: TValue;
begin
  P := Arg(Args, Argc, 1);
  if IsObject(Args[0]) and (IsObject(P) or (P = VNull)) then SetProto(ObjOf(Args[0]), P);
  Res := VUndefined;
  Result := True;
end;

{ ---------------- BigInt ---------------- }

{ 21.2.1.1 BigInt(value): ToPrimitive(value, number), then NumberToBigInt of a Number or ToBigInt of the rest
  (StringToBigInt with its SyntaxError for a string) }
function BigInt_Ctor(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var V, P: TValue; D: Double;
begin
  V := Arg(Args, Argc, 1);
  if IsBigInt(V) then begin Res := V; Exit(True); end;
  if IsObject(V) then
  begin
    if not ToPrimitiveHint(V, phNumber, P) then Exit(False);
    V := P;
  end;
  if IsNumber(V) then
  begin
    D := NumOf(V);
    if IsNaN(D) or IsInfinite(D) or (Frac(D) <> 0) then Exit(ThrowError('RangeError', 'The number cannot be converted to a BigInt because it is not an integer'));
    Res := BigToValue(TBigInteger.FromDouble(D));
    Exit(True);
  end;
  Result := ToBigIntV(V, Res);
end;

{ 21.2.3 thisBigIntValue: a BigInt, or a BigInt wrapper's value }
function ThisBig(V: TValue; var B: TValue): Boolean;
begin
  if IsBigInt(V) then begin B := V; Exit(True); end;
  if IsObject(V) and (ObjOf(V)^.OKind = okPlain) and IsBigInt(ObjOf(V)^.Aux1) then begin B := ObjOf(V)^.Aux1; Exit(True); end;
  Result := ThrowTypeError('BigInt.prototype method called on an incompatible receiver');
end;

{ 21.2.3.3 toString ( [ radix ] ) }
function BigProto_ToString(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var Radix: Int64; B: TValue;
begin
  if not ThisBig(Args[0], B) then Exit(False);
  PushRoot(B);
  if (Argc >= 1) and (Args[1] <> VUndefined) then
  begin
    if not IntArg(Args, Argc, 1, 10, Radix) then begin PopRoots(1); Exit(False); end;
    if (Radix < 2) or (Radix > 36) then begin PopRoots(1); Exit(ThrowError('RangeError', 'toString() radix must be between 2 and 36')); end;
  end
  else Radix := 10;
  Res := StrFromUnicode(BigToString(RootAt(TempTop - 1), Radix));
  PopRoots(1);
  Result := True;
end;

{ 21.2.3.4 valueOf, 21.2.3.2 toLocaleString }
function BigProto_ValueOf(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
begin
  Result := ThisBig(Args[0], Res);
end;

function BigProto_ToLocaleString(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var B: TValue;
begin
  if not ThisBig(Args[0], B) then Exit(False);
  Res := StrFromUnicode(BigToString(B, 10));
  Result := True;
end;

{ ---------------- ArrayBuffer ---------------- }

function Buffer_Ctor(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var N: Int64;
begin
  if not IntArg(Args, Argc, 1, 0, N) then Exit(False);
  if N < 0 then Exit(ThrowError('RangeError', 'Invalid array buffer length'));
  Res := MkPtr(NewArrayBuffer(N, ArrayBufferProto));
  Result := True;
end;

function Buffer_Slice(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var O, B: PObj; Len, S, E: Int64;
begin
  if not IsObject(Args[0]) or (ObjOf(Args[0])^.OKind <> okArrayBuffer) then Exit(ThrowTypeError('not an ArrayBuffer'));
  Len := BufferLen(ObjOf(Args[0]));
  if not IntArg(Args, Argc, 1, 0, S) then Exit(False);
  if not IntArg(Args, Argc, 2, Len, E) then Exit(False);
  if S < 0 then S := Max(0, S + Len) else S := Min(S, Len);
  if E < 0 then E := Max(0, E + Len) else E := Min(E, Len);
  if E < S then E := S;
  B := NewArrayBuffer(E - S, ArrayBufferProto);
  O := ObjOf(Args[0]);
  Move((BufferData(O) + S)^, BufferData(B)^, E - S);
  Res := MkPtr(B);
  Result := True;
end;

{ ---------------- typed arrays ---------------- }

function TypedCtor(Kind: Integer; Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var A0, It, V, Buf: TValue; N, Off, I: Int64; T: PObj; Src: PObj; Done: Boolean; D: Double; Items: PObj;
begin
  if CurNewTarget = VUndefined then Exit(ThrowTypeError('Constructor ' + TANames[Kind] + ' requires ''new'''));
  A0 := Arg(Args, Argc, 1);
  if IsObject(A0) and (ObjOf(A0)^.OKind = okArrayBuffer) then
  begin
    if not IntArg(Args, Argc, 2, 0, Off) then Exit(False);
    if (Argc >= 3) and (Args[3] <> VUndefined) then
    begin
      if not IntArg(Args, Argc, 3, 0, N) then Exit(False);
    end
    else N := (BufferLen(ObjOf(A0)) - Off) div TAElemSize[Kind];
    if (Off mod TAElemSize[Kind] <> 0) or (Off + N * TAElemSize[Kind] > BufferLen(ObjOf(A0))) or (N < 0) then
      Exit(ThrowError('RangeError', 'Invalid typed array length'));
    Res := MkPtr(NewTypedArray(Kind, A0, Off, N, TAProtos[Kind]));
    Exit(True);
  end;
  if not IsObject(A0) then
  begin
    if not IntArg(Args, Argc, 1, 0, N) then Exit(False);
    if N < 0 then Exit(ThrowError('RangeError', 'Invalid typed array length'));
    Buf := MkPtr(NewArrayBuffer(N * TAElemSize[Kind], ArrayBufferProto));
    Res := MkPtr(NewTypedArray(Kind, Buf, 0, N, TAProtos[Kind]));
    Exit(True);
  end;
  { from an array-like or iterable: collect the values first }
  if ObjOf(A0)^.OKind = okArray then Items := ObjOf(A0)
  else if ObjOf(A0)^.OKind = okTypedArray then
  begin
    Src := ObjOf(A0);
    N := TALength(Src);
    Buf := MkPtr(NewArrayBuffer(N * TAElemSize[Kind], ArrayBufferProto));
    T := NewTypedArray(Kind, Buf, 0, N, TAProtos[Kind]);
    Src := ObjOf(Args[1]);
    for I := 0 to N - 1 do TASet(T, I, NumOf(TAGet(Src, I)));
    Res := MkPtr(T);
    Exit(True);
  end
  else
  begin
    Items := NewArray(0, 8);
    PushRoot(MkPtr(Items));
    if not GetIterator(Args[1], It) then begin PopRoots(1); Exit(False); end;
    PushRoot(It);
    while True do
    begin
      if not IteratorStep(RootAt(TempTop - 1), V, Done) then begin PopRoots(2); Exit(False); end;
      if Done then Break;
      ArrayPush(ObjOf(RootAt(TempTop - 2)), V);
    end;
    Items := ObjOf(RootAt(TempTop - 2));
    PopRoots(2);
  end;
  PushRoot(MkPtr(Items));
  N := ArrayLen(Items);
  Buf := MkPtr(NewArrayBuffer(N * TAElemSize[Kind], ArrayBufferProto));
  T := NewTypedArray(Kind, Buf, 0, N, TAProtos[Kind]);
  PushRoot(MkPtr(T));
  for I := 0 to N - 1 do
  begin
    V := ObjOf(RootAt(TempTop - 2))^.Elements^.Data[I];
    if V = VHole then V := VUndefined;
    if not ToNumberV(V, D) then begin PopRoots(2); Exit(False); end;
    TASet(ObjOf(RootAt(TempTop - 1)), I, D);
  end;
  Res := RootAt(TempTop - 1);
  PopRoots(2);
  Result := True;
end;

function TA_Int8(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := TypedCtor(0, Args, Argc, Res); end;
function TA_Uint8(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := TypedCtor(1, Args, Argc, Res); end;
function TA_Uint8C(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := TypedCtor(2, Args, Argc, Res); end;
function TA_Int16(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := TypedCtor(3, Args, Argc, Res); end;
function TA_Uint16(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := TypedCtor(4, Args, Argc, Res); end;
function TA_Int32(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := TypedCtor(5, Args, Argc, Res); end;
function TA_Uint32(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := TypedCtor(6, Args, Argc, Res); end;
function TA_Float32(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := TypedCtor(7, Args, Argc, Res); end;
function TA_Float64(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := TypedCtor(8, Args, Argc, Res); end;

function ThisTA(Args: TArgs): Boolean; inline;
begin
  Result := IsObject(Args[0]) and (ObjOf(Args[0])^.OKind = okTypedArray);
  if not Result then ThrowTypeError('this is not a typed array');
end;

function TAProto_Set(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var Off: Int64; Src, T: PObj; I, N: Integer; V: TValue; D: Double;
begin
  if not ThisTA(Args) then Exit(False);
  if not IntArg(Args, Argc, 2, 0, Off) then Exit(False);
  if not IsObject(Arg(Args, Argc, 1)) then Exit(ThrowTypeError('invalid source'));
  Src := ObjOf(Args[1]);
  if Src^.OKind = okTypedArray then N := TALength(Src) else if Src^.Elements <> nil then N := ArrayLen(Src)
  else Exit(ThrowTypeError('invalid source'));
  if Off + N > TALength(ObjOf(Args[0])) then Exit(ThrowError('RangeError', 'offset is out of bounds'));
  for I := 0 to N - 1 do
  begin
    Src := ObjOf(Args[1]);
    if Src^.OKind = okTypedArray then D := NumOf(TAGet(Src, I))
    else
    begin
      V := Src^.Elements^.Data[I];
      if V = VHole then V := VUndefined;
      if not ToNumberV(V, D) then Exit(False);
    end;
    TASet(ObjOf(Args[0]), Off + I, D);
  end;
  Res := VUndefined;
  Result := True;
end;

function TAProto_Subarray(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var T: PObj; Len, S, E: Int64;
begin
  if not ThisTA(Args) then Exit(False);
  T := ObjOf(Args[0]);
  Len := TALength(T);
  if not IntArg(Args, Argc, 1, 0, S) then Exit(False);
  if not IntArg(Args, Argc, 2, Len, E) then Exit(False);
  if S < 0 then S := Max(0, S + Len) else S := Min(S, Len);
  if E < 0 then E := Max(0, E + Len) else E := Min(E, Len);
  if E < S then E := S;
  T := ObjOf(Args[0]);
  Res := MkPtr(NewTypedArray(TAKind(T), T^.Aux1, T^.Pad + S * TAElemSize[TAKind(T)], E - S, T^.Shape^.Proto));
  Result := True;
end;

function TAProto_Slice(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var T, R: PObj; Len, S, E: Int64; K: Integer; Buf: TValue;
begin
  if not ThisTA(Args) then Exit(False);
  T := ObjOf(Args[0]);
  Len := TALength(T);
  if not IntArg(Args, Argc, 1, 0, S) then Exit(False);
  if not IntArg(Args, Argc, 2, Len, E) then Exit(False);
  if S < 0 then S := Max(0, S + Len) else S := Min(S, Len);
  if E < 0 then E := Max(0, E + Len) else E := Min(E, Len);
  if E < S then E := S;
  K := TAKind(ObjOf(Args[0]));
  Buf := MkPtr(NewArrayBuffer((E - S) * TAElemSize[K], ArrayBufferProto));
  R := NewTypedArray(K, Buf, 0, E - S, ObjOf(Args[0])^.Shape^.Proto);
  T := ObjOf(Args[0]);
  Move((TAData(T) + S * TAElemSize[K])^, TAData(R)^, (E - S) * TAElemSize[K]);
  Res := MkPtr(R);
  Result := True;
end;

function TAProto_Fill(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var T: PObj; Len, S, E, I: Int64; D: Double;
begin
  if not ThisTA(Args) then Exit(False);
  if not NumArg(Args, Argc, 1, D) then Exit(False);
  T := ObjOf(Args[0]);
  Len := TALength(T);
  if not IntArg(Args, Argc, 2, 0, S) then Exit(False);
  if not IntArg(Args, Argc, 3, Len, E) then Exit(False);
  if S < 0 then S := Max(0, S + Len) else S := Min(S, Len);
  if E < 0 then E := Max(0, E + Len) else E := Min(E, Len);
  T := ObjOf(Args[0]);
  for I := S to E - 1 do TASet(T, I, D);
  Res := Args[0];
  Result := True;
end;

function TAProto_Join(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var T: PObj; I: Integer; S: UnicodeString; V, SV: TValue; Sep: UnicodeString;
begin
  if not ThisTA(Args) then Exit(False);
  Sep := ',';
  if (Argc >= 1) and IsString(Args[1]) then Sep := StrToUnicode(StrOf(Args[1]));
  S := '';
  for I := 0 to TALength(ObjOf(Args[0])) - 1 do
  begin
    if I > 0 then S := S + Sep;
    V := TAGet(ObjOf(Args[0]), I);
    if not ToStringV(V, SV) then Exit(False);
    S := S + StrToUnicode(StrOf(SV));
  end;
  Res := StrFromUnicode(S);
  Result := True;
end;

function TAProto_IndexOf(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var T: PObj; I: Integer; D: Double;
begin
  if not ThisTA(Args) then Exit(False);
  if not IsNumber(Arg(Args, Argc, 1)) then begin Res := MkInt(-1); Exit(True); end;
  D := NumOf(Args[1]);
  T := ObjOf(Args[0]);
  for I := 0 to TALength(T) - 1 do
    if NumOf(TAGet(T, I)) = D then begin Res := MkInt(I); Exit(True); end;
  Res := MkInt(-1);
  Result := True;
end;

function TAProto_ForEach(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var I: Integer; CallArgs: array[0..2] of TValue; R: TValue;
begin
  if not ThisTA(Args) then Exit(False);
  for I := 0 to TALength(ObjOf(Args[0])) - 1 do
  begin
    CallArgs[0] := TAGet(ObjOf(Args[0]), I); CallArgs[1] := MkInt(I); CallArgs[2] := Args[0];
    if not CallValue(Arg(Args, Argc, 1), Arg(Args, Argc, 2), @CallArgs[0], 3, R) then Exit(False);
  end;
  Res := VUndefined;
  Result := True;
end;

function TAProto_Values(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
begin
  if not ThisTA(Args) then Exit(False);
  Result := GetIterator(Args[0], Res);
end;

{ ---------------- DataView ---------------- }

function DV_Ctor(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var Buf: TValue; Off, Len: Int64; O: PObj;
begin
  Buf := Arg(Args, Argc, 1);
  if not IsObject(Buf) or (ObjOf(Buf)^.OKind <> okArrayBuffer) then Exit(ThrowTypeError('First argument to DataView constructor must be an ArrayBuffer'));
  if not IntArg(Args, Argc, 2, 0, Off) then Exit(False);
  if not IntArg(Args, Argc, 3, BufferLen(ObjOf(Args[1])) - Off, Len) then Exit(False);
  if (Off < 0) or (Len < 0) or (Off + Len > BufferLen(ObjOf(Args[1]))) then Exit(ThrowError('RangeError', 'Invalid DataView length'));
  O := NewObject(RootShapeFor(DataViewProto), 0, okDataView);
  O^.Aux1 := Args[1];
  O^.Aux2 := MkInt(Len);
  O^.Pad := Off;
  Res := MkPtr(O);
  Result := True;
end;

function DVAccess(Args: TArgs; Argc: Integer; var Res: TValue; Size: Integer; IsSet: Boolean; Kind: Integer): Boolean;
{ Kind: 0 int signed, 1 int unsigned, 2 float }
var O: PObj; Off: Int64; P: PByte; Little: Boolean; B: array[0..7] of Byte; I: Integer; D: Double;
  U64: QWord; F: Single; LE: Integer;
begin
  if not IsObject(Args[0]) or (ObjOf(Args[0])^.OKind <> okDataView) then Exit(ThrowTypeError('not a DataView'));
  if not IntArg(Args, Argc, 1, 0, Off) then Exit(False);
  if IsSet then
  begin
    if not NumArg(Args, Argc, 2, D) then Exit(False);
    LE := 3;
  end
  else LE := 2;
  Little := ToBool(Arg(Args, Argc, LE));
  O := ObjOf(Args[0]);
  if (Off < 0) or (Off + Size > IntOf(O^.Aux2)) then Exit(ThrowError('RangeError', 'Offset is outside the bounds of the DataView'));
  P := BufferData(ObjOf(O^.Aux1)) + O^.Pad + Off;
  if IsSet then
  begin
    FillChar(B, 8, 0);
    case Kind of
      2: if Size = 4 then begin F := D; Move(F, B, 4); end else Move(D, B, 8);
    else
      begin
        if IsNaN(D) or IsInfinite(D) then U64 := 0 else U64 := QWord(Int64(Trunc(D)));
        Move(U64, B, 8);
      end;
    end;
    { B holds little-endian bytes }
    for I := 0 to Size - 1 do
      if Little then P[I] := B[I] else P[I] := B[Size - 1 - I];
    Res := VUndefined;
    Exit(True);
  end;
  FillChar(B, 8, 0);
  for I := 0 to Size - 1 do
    if Little then B[I] := P[I] else B[I] := P[Size - 1 - I];
  case Kind of
    2: if Size = 4 then begin Move(B, F, 4); Res := MkNumber(F); end else begin Move(B, D, 8); Res := MkNumber(D); end;
    1: begin Move(B, U64, 8); Res := MkNumber(U64); end;
  else
    case Size of
      1: Res := MkInt(ShortInt(B[0]));
      2: Res := MkInt(SmallInt(PWord(@B[0])^));
    else Res := MkInt(LongInt(PLongWord(@B[0])^));
    end;
  end;
  Result := True;
end;

function DV_GetInt8(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := DVAccess(Args, Argc, Res, 1, False, 0); end;
function DV_GetUint8(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := DVAccess(Args, Argc, Res, 1, False, 1); end;
function DV_GetInt16(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := DVAccess(Args, Argc, Res, 2, False, 0); end;
function DV_GetUint16(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := DVAccess(Args, Argc, Res, 2, False, 1); end;
function DV_GetInt32(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := DVAccess(Args, Argc, Res, 4, False, 0); end;
function DV_GetUint32(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := DVAccess(Args, Argc, Res, 4, False, 1); end;
function DV_GetFloat32(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := DVAccess(Args, Argc, Res, 4, False, 2); end;
function DV_GetFloat64(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := DVAccess(Args, Argc, Res, 8, False, 2); end;
function DV_SetInt8(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := DVAccess(Args, Argc, Res, 1, True, 0); end;
function DV_SetUint8(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := DVAccess(Args, Argc, Res, 1, True, 1); end;
function DV_SetInt16(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := DVAccess(Args, Argc, Res, 2, True, 0); end;
function DV_SetUint16(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := DVAccess(Args, Argc, Res, 2, True, 1); end;
function DV_SetInt32(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := DVAccess(Args, Argc, Res, 4, True, 0); end;
function DV_SetUint32(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := DVAccess(Args, Argc, Res, 4, True, 1); end;
function DV_SetFloat32(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := DVAccess(Args, Argc, Res, 4, True, 2); end;
function DV_SetFloat64(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := DVAccess(Args, Argc, Res, 8, True, 2); end;

{ ---------------- Promise (constructor and prototype only, for instanceof checks) ---------------- }

threadvar
  PromiseProto: TValue;

function Promise_Ctor(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
begin
  Result := ThrowTypeError('Promise is not supported by the spike engine');
end;

{ ---------------- setup ---------------- }

function IterAtom: PStr;
begin
  Result := PStr(PPointer(PByte(PtrOf(SymIterator)) + 16)^);
end;

threadvar
  TypedArrayCtor: TValue;

function TypedArray_Abstract(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
begin
  Result := ThrowTypeError('Abstract class TypedArray not directly constructable');
end;

function Ctor(Fn: TNativeFn; const Name: UnicodeString; Len: Integer; var Proto: TValue): TValue;
var C: TValue;
begin
  C := NewNativeFunction(Fn, Name, Len);
  Scratch := C;
  DefineOwn(Scratch, AtomPrototype, Proto, 0);
  DefineOwn(Proto, AtomConstructor, Scratch, pfHidden);
  DefineOwn(GlobalObj, AtomOf(Name), Scratch, pfHidden);
  Result := Scratch;
end;

procedure InitBuiltins2;
const
  TAFns: array[0..8] of TNativeFn = (TA_Int8, TA_Uint8, TA_Uint8C, TA_Int16, TA_Uint16, TA_Int32, TA_Uint32, TA_Float32, TA_Float64);
var I: Integer; G, A: TValue;
begin
  AddRootRange(@TAProtos[0], Length(TAProtos));   { not @TAProtos[I]: see AddRootRange }
  AddRoot(@TypedArrayProto); AddRoot(@ArrayBufferProto); AddRoot(@DataViewProto); AddRoot(@Scratch);
  AddRoot(@BigIntProto);

  { generators }
  GeneratorProto := MkPtr(NewPlainObject(IteratorProto, 4));
  G := NewNativeFunction(Gen_Next, 'next', 1);
  SetGenNextNative(ObjOf(G)^.Native);
  DefineOwn(GeneratorProto, AtomNext, G, pfHidden);
  AddRoot(@GenNextFn);
  GenNextFn := G;
  DefineNative(GeneratorProto, 'throw', Gen_Throw, 1);
  DefineNative(GeneratorProto, 'return', Gen_Return, 1);

  { __proto__ }
  G := NewNativeFunction(Proto_Get, 'get __proto__', 0);
  Scratch := G;
  G := NewNativeFunction(Proto_Set, 'set __proto__', 1);
  A := MkPtr(NewAccessor(Scratch, G));
  DefineOwn(ObjectProto, AtomOf('__proto__'), A, pfConfigurable or pfAccessor);

  { BigInt }
  BigIntProto := MkPtr(NewPlainObject(ObjectProto, 4));
  Ctor(BigInt_Ctor, 'BigInt', 1, BigIntProto);
  DefineNative(BigIntProto, 'toString', BigProto_ToString, 0);
  DefineNative(BigIntProto, 'valueOf', BigProto_ValueOf, 0);
  DefineNative(BigIntProto, 'toLocaleString', BigProto_ToLocaleString, 0);
  DefineOwn(BigIntProto, SymKey(SymToStringTagW), StrFromUnicode('BigInt'), pfConfigurable);

  { ArrayBuffer }
  ArrayBufferProto := MkPtr(NewPlainObject(ObjectProto, 4));
  Ctor(Buffer_Ctor, 'ArrayBuffer', 1, ArrayBufferProto);
  DefineNative(ArrayBufferProto, 'slice', Buffer_Slice, 2);

  { typed arrays }
  TypedArrayProto := MkPtr(NewPlainObject(ObjectProto, 16));
  DefineNative(TypedArrayProto, 'set', TAProto_Set, 1);
  DefineNative(TypedArrayProto, 'subarray', TAProto_Subarray, 2);
  DefineNative(TypedArrayProto, 'slice', TAProto_Slice, 2);
  DefineNative(TypedArrayProto, 'fill', TAProto_Fill, 1);
  DefineNative(TypedArrayProto, 'join', TAProto_Join, 1);
  DefineNative(TypedArrayProto, 'indexOf', TAProto_IndexOf, 1);
  DefineNative(TypedArrayProto, 'forEach', TAProto_ForEach, 1);
  DefineNative(TypedArrayProto, 'values', TAProto_Values, 0);
  G := NewNativeFunction(TAProto_Values, '[Symbol.iterator]', 0);
  DefineOwn(TypedArrayProto, IterAtom, G, pfHidden);
  { 23.2.1 %TypedArray%: an abstract constructor (it always throws), the [[Prototype]] of the nine }
  G := NewNativeFunction(TypedArray_Abstract, 'TypedArray', 0);
  AddRoot(@TypedArrayCtor);
  TypedArrayCtor := G;
  DefineOwn(TypedArrayCtor, AtomPrototype, TypedArrayProto, 0);
  DefineOwn(TypedArrayProto, AtomConstructor, TypedArrayCtor, pfHidden);
  for I := 0 to 8 do
  begin
    TAProtos[I] := MkPtr(NewPlainObject(TypedArrayProto, 4));
    Ctor(TAFns[I], TANames[I], 3, TAProtos[I]);
    SetProto(ObjOf(Scratch), TypedArrayCtor);
    DefineHidden(Scratch, 'BYTES_PER_ELEMENT', MkInt(TAElemSize[I]));
    DefineHidden(TAProtos[I], 'BYTES_PER_ELEMENT', MkInt(TAElemSize[I]));
  end;

  { the well-known symbols as properties of Symbol (20.4.2: not writable, enumerable or configurable) }
  if GetV(GlobalObj, AtomOf('Symbol'), G) then
  begin
    Scratch := G;
    DefineOwn(Scratch, AtomOf('asyncIterator'), SymAsyncIterator, 0);
    DefineOwn(Scratch, AtomOf('species'), SymSpecies, 0);
    DefineOwn(Scratch, AtomOf('toPrimitive'), SymToPrimitive, 0);
    DefineOwn(Scratch, AtomOf('isConcatSpreadable'), SymIsConcatSpreadable, 0);
    DefineOwn(Scratch, AtomOf('match'), SymMatch, 0);
    DefineOwn(Scratch, AtomOf('matchAll'), SymMatchAll, 0);
    DefineOwn(Scratch, AtomOf('replace'), SymReplace, 0);
    DefineOwn(Scratch, AtomOf('search'), SymSearch, 0);
    DefineOwn(Scratch, AtomOf('split'), SymSplit, 0);
    DefineOwn(Scratch, AtomOf('unscopables'), SymUnscopables, 0);
    DefineOwn(Scratch, AtomOf('dispose'), SymDispose, 0);
    DefineOwn(Scratch, AtomOf('asyncDispose'), SymAsyncDispose, 0);
  end;

  { DataView }
  DataViewProto := MkPtr(NewPlainObject(ObjectProto, 16));
  Ctor(DV_Ctor, 'DataView', 1, DataViewProto);
  DefineNative(DataViewProto, 'getInt8', DV_GetInt8, 1);
  DefineNative(DataViewProto, 'getUint8', DV_GetUint8, 1);
  DefineNative(DataViewProto, 'getInt16', DV_GetInt16, 1);
  DefineNative(DataViewProto, 'getUint16', DV_GetUint16, 1);
  DefineNative(DataViewProto, 'getInt32', DV_GetInt32, 1);
  DefineNative(DataViewProto, 'getUint32', DV_GetUint32, 1);
  DefineNative(DataViewProto, 'getFloat32', DV_GetFloat32, 1);
  DefineNative(DataViewProto, 'getFloat64', DV_GetFloat64, 1);
  DefineNative(DataViewProto, 'setInt8', DV_SetInt8, 2);
  DefineNative(DataViewProto, 'setUint8', DV_SetUint8, 2);
  DefineNative(DataViewProto, 'setInt16', DV_SetInt16, 2);
  DefineNative(DataViewProto, 'setUint16', DV_SetUint16, 2);
  DefineNative(DataViewProto, 'setInt32', DV_SetInt32, 2);
  DefineNative(DataViewProto, 'setUint32', DV_SetUint32, 2);
  DefineNative(DataViewProto, 'setFloat32', DV_SetFloat32, 2);
  DefineNative(DataViewProto, 'setFloat64', DV_SetFloat64, 2);
end;

end.
