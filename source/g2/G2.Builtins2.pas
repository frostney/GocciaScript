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
  Math,
  SysUtils,

  BigInteger,
  G2.BigIntV,
  G2.Builtins,
  G2.Bytecode,
  G2.Core,
  G2.Typed,
  G2.VM;

type
  TArgs = PValue;

threadvar
  TAProtos: array[0..8] of TValue;
  TypedArrayProto, ArrayBufferProto, DataViewProto, Scratch: TValue;

function Arg(AArgs: TArgs; AArgc, I: Integer): TValue; inline;
begin
  if I <= AArgc then Result := AArgs[I] else Result := VUndefined;
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
  if IsNaN(D) then R := 0 else if D > 9e15 then R := High(Int32) else if D < -9e15 then R := Low(Int32) else R := Trunc(D);
  Result := True;
end;

function MakeIterResult(AValue: TValue; ADone: Boolean): TValue;
var O: PObj;
begin
  PushRoot(AValue);
  O := NewPlainObject(ObjectProto, 2);
  AddProperty(O, AtomValue, RootAt(TempTop - 1), pfDefault);
  AddProperty(O, AtomDone, MkBool(ADone), pfDefault);
  PopRoots(1);
  Result := MkPtr(O);
end;

{ ---------------- generators ---------------- }

function GenResumeNative(AArgs: TArgs; AArgc: Integer; var ARes: TValue; AMode: Integer): Boolean;
var V: TValue; Done: Boolean;
begin
  GenYieldRaw := False;
  if not GeneratorResume(AArgs[0], Arg(AArgs, AArgc, 1), AMode, V, Done) then Exit(False);
  if GenYieldRaw then begin GenYieldRaw := False; ARes := V; Exit(True); end;
  ARes := MakeIterResult(V, Done);
  Result := True;
end;
function Gen_Next(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean; begin Result := GenResumeNative(AArgs, AArgc, ARes, 0); end;
function Gen_Throw(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean; begin Result := GenResumeNative(AArgs, AArgc, ARes, 1); end;
function Gen_Return(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean; begin Result := GenResumeNative(AArgs, AArgc, ARes, 2); end;

{ ---------------- __proto__ ---------------- }

function Proto_Get(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
begin
  if IsObject(AArgs[0]) then ARes := ObjOf(AArgs[0])^.Shape^.Proto else ARes := VNull;
  Result := True;
end;

function Proto_Set(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var P: TValue;
begin
  P := Arg(AArgs, AArgc, 1);
  if IsObject(AArgs[0]) and (IsObject(P) or (P = VNull)) then SetProto(ObjOf(AArgs[0]), P);
  ARes := VUndefined;
  Result := True;
end;

{ ---------------- BigInt ---------------- }

function BigInt_Ctor(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var V, S: TValue; D: Double;
begin
  V := Arg(AArgs, AArgc, 1);
  if IsBigInt(V) then begin ARes := V; Exit(True); end;
  if IsNumber(V) then
  begin
    D := NumOf(V);
    if IsNaN(D) or IsInfinite(D) or (Frac(D) <> 0) then Exit(ThrowError('RangeError', 'The number cannot be converted to a BigInt because it is not an integer'));
    ARes := BigToValue(TBigInteger.FromDouble(D));
    Exit(True);
  end;
  if V = VTrue then begin ARes := BigToValue(TBigInteger.One); Exit(True); end;
  if V = VFalse then begin ARes := BigToValue(TBigInteger.Zero); Exit(True); end;
  if not ToStringV(V, S) then Exit(False);
  ARes := BigToValue(TBigInteger.FromDecimalString(Trim(StrToUnicode(StrOf(S)))));
  Result := True;
end;

{ 21.2.3 thisBigIntValue: a BigInt, or a BigInt wrapper's value }
function ThisBig(V: TValue; var B: TValue): Boolean;
begin
  if IsBigInt(V) then begin B := V; Exit(True); end;
  if IsObject(V) and (ObjOf(V)^.OKind = okPlain) and IsBigInt(ObjOf(V)^.Aux1) then begin B := ObjOf(V)^.Aux1; Exit(True); end;
  Result := ThrowTypeError('BigInt.prototype method called on an incompatible receiver');
end;

{ 21.2.3.3 toString ( [ radix ] ) }
function BigProto_ToString(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var Radix: Int64; B: TValue;
begin
  if not ThisBig(AArgs[0], B) then Exit(False);
  PushRoot(B);
  if (AArgc >= 1) and (AArgs[1] <> VUndefined) then
  begin
    if not IntArg(AArgs, AArgc, 1, 10, Radix) then begin PopRoots(1); Exit(False); end;
    if (Radix < 2) or (Radix > 36) then begin PopRoots(1); Exit(ThrowError('RangeError', 'toString() radix must be between 2 and 36')); end;
  end
  else Radix := 10;
  ARes := StrFromUnicode(BigToString(RootAt(TempTop - 1), Radix));
  PopRoots(1);
  Result := True;
end;

{ 21.2.3.4 valueOf, 21.2.3.2 toLocaleString }
function BigProto_ValueOf(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
begin
  Result := ThisBig(AArgs[0], ARes);
end;

function BigProto_ToLocaleString(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var B: TValue;
begin
  if not ThisBig(AArgs[0], B) then Exit(False);
  ARes := StrFromUnicode(BigToString(B, 10));
  Result := True;
end;

{ ---------------- ArrayBuffer ---------------- }

function Buffer_Ctor(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var N: Int64;
begin
  if not IntArg(AArgs, AArgc, 1, 0, N) then Exit(False);
  if N < 0 then Exit(ThrowError('RangeError', 'Invalid array buffer length'));
  ARes := MkPtr(NewArrayBuffer(N, ArrayBufferProto));
  Result := True;
end;

function Buffer_Slice(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var O, B: PObj; Len, S, E: Int64;
begin
  if not IsObject(AArgs[0]) or (ObjOf(AArgs[0])^.OKind <> okArrayBuffer) then Exit(ThrowTypeError('not an ArrayBuffer'));
  Len := BufferLen(ObjOf(AArgs[0]));
  if not IntArg(AArgs, AArgc, 1, 0, S) then Exit(False);
  if not IntArg(AArgs, AArgc, 2, Len, E) then Exit(False);
  if S < 0 then S := Max(0, S + Len) else S := Min(S, Len);
  if E < 0 then E := Max(0, E + Len) else E := Min(E, Len);
  if E < S then E := S;
  B := NewArrayBuffer(E - S, ArrayBufferProto);
  O := ObjOf(AArgs[0]);
  Move((BufferData(O) + S)^, BufferData(B)^, E - S);
  ARes := MkPtr(B);
  Result := True;
end;

{ ---------------- typed arrays ---------------- }

function TypedCtor(AKind: Integer; AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var A0, It, V, Buf: TValue; N, Off, I: Int64; T: PObj; Src: PObj; Done: Boolean; D: Double; Items: PObj;
begin
  if CurNewTarget = VUndefined then Exit(ThrowTypeError('Constructor ' + TANames[AKind] + ' requires ''new'''));
  A0 := Arg(AArgs, AArgc, 1);
  if IsObject(A0) and (ObjOf(A0)^.OKind = okArrayBuffer) then
  begin
    if not IntArg(AArgs, AArgc, 2, 0, Off) then Exit(False);
    if (AArgc >= 3) and (AArgs[3] <> VUndefined) then
    begin
      if not IntArg(AArgs, AArgc, 3, 0, N) then Exit(False);
    end
    else N := (BufferLen(ObjOf(A0)) - Off) div TAElemSize[AKind];
    if (Off mod TAElemSize[AKind] <> 0) or (Off + N * TAElemSize[AKind] > BufferLen(ObjOf(A0))) or (N < 0) then
      Exit(ThrowError('RangeError', 'Invalid typed array length'));
    ARes := MkPtr(NewTypedArray(AKind, A0, Off, N, TAProtos[AKind]));
    Exit(True);
  end;
  if not IsObject(A0) then
  begin
    if not IntArg(AArgs, AArgc, 1, 0, N) then Exit(False);
    if N < 0 then Exit(ThrowError('RangeError', 'Invalid typed array length'));
    Buf := MkPtr(NewArrayBuffer(N * TAElemSize[AKind], ArrayBufferProto));
    ARes := MkPtr(NewTypedArray(AKind, Buf, 0, N, TAProtos[AKind]));
    Exit(True);
  end;
  { from an array-like or iterable: collect the values first }
  if ObjOf(A0)^.OKind = okArray then Items := ObjOf(A0)
  else if ObjOf(A0)^.OKind = okTypedArray then
  begin
    Src := ObjOf(A0);
    N := TALength(Src);
    Buf := MkPtr(NewArrayBuffer(N * TAElemSize[AKind], ArrayBufferProto));
    T := NewTypedArray(AKind, Buf, 0, N, TAProtos[AKind]);
    Src := ObjOf(AArgs[1]);
    for I := 0 to N - 1 do TASet(T, I, NumOf(TAGet(Src, I)));
    ARes := MkPtr(T);
    Exit(True);
  end
  else
  begin
    Items := NewArray(0, 8);
    PushRoot(MkPtr(Items));
    if not GetIterator(AArgs[1], It) then begin PopRoots(1); Exit(False); end;
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
  Buf := MkPtr(NewArrayBuffer(N * TAElemSize[AKind], ArrayBufferProto));
  T := NewTypedArray(AKind, Buf, 0, N, TAProtos[AKind]);
  PushRoot(MkPtr(T));
  for I := 0 to N - 1 do
  begin
    V := ObjOf(RootAt(TempTop - 2))^.Elements^.Data[I];
    if V = VHole then V := VUndefined;
    if not ToNumberV(V, D) then begin PopRoots(2); Exit(False); end;
    TASet(ObjOf(RootAt(TempTop - 1)), I, D);
  end;
  ARes := RootAt(TempTop - 1);
  PopRoots(2);
  Result := True;
end;

function TA_Int8(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean; begin Result := TypedCtor(0, AArgs, AArgc, ARes); end;
function TA_Uint8(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean; begin Result := TypedCtor(1, AArgs, AArgc, ARes); end;
function TA_Uint8C(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean; begin Result := TypedCtor(2, AArgs, AArgc, ARes); end;
function TA_Int16(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean; begin Result := TypedCtor(3, AArgs, AArgc, ARes); end;
function TA_Uint16(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean; begin Result := TypedCtor(4, AArgs, AArgc, ARes); end;
function TA_Int32(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean; begin Result := TypedCtor(5, AArgs, AArgc, ARes); end;
function TA_Uint32(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean; begin Result := TypedCtor(6, AArgs, AArgc, ARes); end;
function TA_Float32(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean; begin Result := TypedCtor(7, AArgs, AArgc, ARes); end;
function TA_Float64(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean; begin Result := TypedCtor(8, AArgs, AArgc, ARes); end;

function ThisTA(AArgs: TArgs): Boolean; inline;
begin
  Result := IsObject(AArgs[0]) and (ObjOf(AArgs[0])^.OKind = okTypedArray);
  if not Result then ThrowTypeError('this is not a typed array');
end;

function TAProto_Set(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var Off: Int64; Src, T: PObj; I, N: Integer; V: TValue; D: Double;
begin
  if not ThisTA(AArgs) then Exit(False);
  if not IntArg(AArgs, AArgc, 2, 0, Off) then Exit(False);
  if not IsObject(Arg(AArgs, AArgc, 1)) then Exit(ThrowTypeError('invalid source'));
  Src := ObjOf(AArgs[1]);
  if Src^.OKind = okTypedArray then N := TALength(Src) else if Src^.Elements <> nil then N := ArrayLen(Src)
  else Exit(ThrowTypeError('invalid source'));
  if Off + N > TALength(ObjOf(AArgs[0])) then Exit(ThrowError('RangeError', 'offset is out of bounds'));
  for I := 0 to N - 1 do
  begin
    Src := ObjOf(AArgs[1]);
    if Src^.OKind = okTypedArray then D := NumOf(TAGet(Src, I))
    else
    begin
      V := Src^.Elements^.Data[I];
      if V = VHole then V := VUndefined;
      if not ToNumberV(V, D) then Exit(False);
    end;
    TASet(ObjOf(AArgs[0]), Off + I, D);
  end;
  ARes := VUndefined;
  Result := True;
end;

function TAProto_Subarray(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var T: PObj; Len, S, E: Int64;
begin
  if not ThisTA(AArgs) then Exit(False);
  T := ObjOf(AArgs[0]);
  Len := TALength(T);
  if not IntArg(AArgs, AArgc, 1, 0, S) then Exit(False);
  if not IntArg(AArgs, AArgc, 2, Len, E) then Exit(False);
  if S < 0 then S := Max(0, S + Len) else S := Min(S, Len);
  if E < 0 then E := Max(0, E + Len) else E := Min(E, Len);
  if E < S then E := S;
  T := ObjOf(AArgs[0]);
  ARes := MkPtr(NewTypedArray(TAKind(T), T^.Aux1, T^.Pad + S * TAElemSize[TAKind(T)], E - S, T^.Shape^.Proto));
  Result := True;
end;

function TAProto_Slice(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var T, R: PObj; Len, S, E: Int64; K: Integer; Buf: TValue;
begin
  if not ThisTA(AArgs) then Exit(False);
  T := ObjOf(AArgs[0]);
  Len := TALength(T);
  if not IntArg(AArgs, AArgc, 1, 0, S) then Exit(False);
  if not IntArg(AArgs, AArgc, 2, Len, E) then Exit(False);
  if S < 0 then S := Max(0, S + Len) else S := Min(S, Len);
  if E < 0 then E := Max(0, E + Len) else E := Min(E, Len);
  if E < S then E := S;
  K := TAKind(ObjOf(AArgs[0]));
  Buf := MkPtr(NewArrayBuffer((E - S) * TAElemSize[K], ArrayBufferProto));
  R := NewTypedArray(K, Buf, 0, E - S, ObjOf(AArgs[0])^.Shape^.Proto);
  T := ObjOf(AArgs[0]);
  Move((TAData(T) + S * TAElemSize[K])^, TAData(R)^, (E - S) * TAElemSize[K]);
  ARes := MkPtr(R);
  Result := True;
end;

function TAProto_Fill(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var T: PObj; Len, S, E, I: Int64; D: Double;
begin
  if not ThisTA(AArgs) then Exit(False);
  if not NumArg(AArgs, AArgc, 1, D) then Exit(False);
  T := ObjOf(AArgs[0]);
  Len := TALength(T);
  if not IntArg(AArgs, AArgc, 2, 0, S) then Exit(False);
  if not IntArg(AArgs, AArgc, 3, Len, E) then Exit(False);
  if S < 0 then S := Max(0, S + Len) else S := Min(S, Len);
  if E < 0 then E := Max(0, E + Len) else E := Min(E, Len);
  T := ObjOf(AArgs[0]);
  for I := S to E - 1 do TASet(T, I, D);
  ARes := AArgs[0];
  Result := True;
end;

function TAProto_Join(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var T: PObj; I: Integer; S: UnicodeString; V, SV: TValue; Sep: UnicodeString;
begin
  if not ThisTA(AArgs) then Exit(False);
  Sep := ',';
  if (AArgc >= 1) and IsString(AArgs[1]) then Sep := StrToUnicode(StrOf(AArgs[1]));
  S := '';
  for I := 0 to TALength(ObjOf(AArgs[0])) - 1 do
  begin
    if I > 0 then S := S + Sep;
    V := TAGet(ObjOf(AArgs[0]), I);
    if not ToStringV(V, SV) then Exit(False);
    S := S + StrToUnicode(StrOf(SV));
  end;
  ARes := StrFromUnicode(S);
  Result := True;
end;

function TAProto_IndexOf(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var T: PObj; I: Integer; D: Double;
begin
  if not ThisTA(AArgs) then Exit(False);
  if not IsNumber(Arg(AArgs, AArgc, 1)) then begin ARes := MkInt(-1); Exit(True); end;
  D := NumOf(AArgs[1]);
  T := ObjOf(AArgs[0]);
  for I := 0 to TALength(T) - 1 do
    if NumOf(TAGet(T, I)) = D then begin ARes := MkInt(I); Exit(True); end;
  ARes := MkInt(-1);
  Result := True;
end;

function TAProto_ForEach(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var I: Integer; CallArgs: array[0..2] of TValue; R: TValue;
begin
  if not ThisTA(AArgs) then Exit(False);
  for I := 0 to TALength(ObjOf(AArgs[0])) - 1 do
  begin
    CallArgs[0] := TAGet(ObjOf(AArgs[0]), I); CallArgs[1] := MkInt(I); CallArgs[2] := AArgs[0];
    if not CallValue(Arg(AArgs, AArgc, 1), Arg(AArgs, AArgc, 2), @CallArgs[0], 3, R) then Exit(False);
  end;
  ARes := VUndefined;
  Result := True;
end;

function TAProto_Values(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
begin
  if not ThisTA(AArgs) then Exit(False);
  Result := GetIterator(AArgs[0], ARes);
end;

{ ---------------- DataView ---------------- }

function DV_Ctor(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var Buf: TValue; Off, Len: Int64; O: PObj;
begin
  Buf := Arg(AArgs, AArgc, 1);
  if not IsObject(Buf) or (ObjOf(Buf)^.OKind <> okArrayBuffer) then Exit(ThrowTypeError('First argument to DataView constructor must be an ArrayBuffer'));
  if not IntArg(AArgs, AArgc, 2, 0, Off) then Exit(False);
  if not IntArg(AArgs, AArgc, 3, BufferLen(ObjOf(AArgs[1])) - Off, Len) then Exit(False);
  if (Off < 0) or (Len < 0) or (Off + Len > BufferLen(ObjOf(AArgs[1]))) then Exit(ThrowError('RangeError', 'Invalid DataView length'));
  O := NewObject(RootShapeFor(DataViewProto), 0, okDataView);
  O^.Aux1 := AArgs[1];
  O^.Aux2 := MkInt(Len);
  O^.Pad := Off;
  ARes := MkPtr(O);
  Result := True;
end;

function DVAccess(AArgs: TArgs; AArgc: Integer; var ARes: TValue; ASize: Integer; AIsSet: Boolean; AKind: Integer): Boolean;
{ Kind: 0 int signed, 1 int unsigned, 2 float }
var O: PObj; Off: Int64; P: PByte; Little: Boolean; B: array[0..7] of Byte; I: Integer; D: Double;
  U64: QWord; F: Single; LE: Integer;
begin
  if not IsObject(AArgs[0]) or (ObjOf(AArgs[0])^.OKind <> okDataView) then Exit(ThrowTypeError('not a DataView'));
  if not IntArg(AArgs, AArgc, 1, 0, Off) then Exit(False);
  if AIsSet then
  begin
    if not NumArg(AArgs, AArgc, 2, D) then Exit(False);
    LE := 3;
  end
  else LE := 2;
  Little := ToBool(Arg(AArgs, AArgc, LE));
  O := ObjOf(AArgs[0]);
  if (Off < 0) or (Off + ASize > IntOf(O^.Aux2)) then Exit(ThrowError('RangeError', 'Offset is outside the bounds of the DataView'));
  P := BufferData(ObjOf(O^.Aux1)) + O^.Pad + Off;
  if AIsSet then
  begin
    FillChar(B, 8, 0);
    case AKind of
      2: if ASize = 4 then begin F := D; Move(F, B, 4); end else Move(D, B, 8);
    else
      begin
        if IsNaN(D) or IsInfinite(D) then U64 := 0 else U64 := QWord(Int64(Trunc(D)));
        Move(U64, B, 8);
      end;
    end;
    { B holds little-endian bytes }
    for I := 0 to ASize - 1 do
      if Little then P[I] := B[I] else P[I] := B[ASize - 1 - I];
    ARes := VUndefined;
    Exit(True);
  end;
  FillChar(B, 8, 0);
  for I := 0 to ASize - 1 do
    if Little then B[I] := P[I] else B[I] := P[ASize - 1 - I];
  case AKind of
    2: if ASize = 4 then begin Move(B, F, 4); ARes := MkNumber(F); end else begin Move(B, D, 8); ARes := MkNumber(D); end;
    1: begin Move(B, U64, 8); ARes := MkNumber(U64); end;
  else
    case ASize of
      1: ARes := MkInt(ShortInt(B[0]));
      2: ARes := MkInt(SmallInt(PWord(@B[0])^));
    else ARes := MkInt(LongInt(PLongWord(@B[0])^));
    end;
  end;
  Result := True;
end;

function DV_GetInt8(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean; begin Result := DVAccess(AArgs, AArgc, ARes, 1, False, 0); end;
function DV_GetUint8(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean; begin Result := DVAccess(AArgs, AArgc, ARes, 1, False, 1); end;
function DV_GetInt16(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean; begin Result := DVAccess(AArgs, AArgc, ARes, 2, False, 0); end;
function DV_GetUint16(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean; begin Result := DVAccess(AArgs, AArgc, ARes, 2, False, 1); end;
function DV_GetInt32(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean; begin Result := DVAccess(AArgs, AArgc, ARes, 4, False, 0); end;
function DV_GetUint32(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean; begin Result := DVAccess(AArgs, AArgc, ARes, 4, False, 1); end;
function DV_GetFloat32(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean; begin Result := DVAccess(AArgs, AArgc, ARes, 4, False, 2); end;
function DV_GetFloat64(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean; begin Result := DVAccess(AArgs, AArgc, ARes, 8, False, 2); end;
function DV_SetInt8(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean; begin Result := DVAccess(AArgs, AArgc, ARes, 1, True, 0); end;
function DV_SetUint8(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean; begin Result := DVAccess(AArgs, AArgc, ARes, 1, True, 1); end;
function DV_SetInt16(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean; begin Result := DVAccess(AArgs, AArgc, ARes, 2, True, 0); end;
function DV_SetUint16(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean; begin Result := DVAccess(AArgs, AArgc, ARes, 2, True, 1); end;
function DV_SetInt32(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean; begin Result := DVAccess(AArgs, AArgc, ARes, 4, True, 0); end;
function DV_SetUint32(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean; begin Result := DVAccess(AArgs, AArgc, ARes, 4, True, 1); end;
function DV_SetFloat32(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean; begin Result := DVAccess(AArgs, AArgc, ARes, 4, True, 2); end;
function DV_SetFloat64(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean; begin Result := DVAccess(AArgs, AArgc, ARes, 8, True, 2); end;

{ ---------------- Promise (constructor and prototype only, for instanceof checks) ---------------- }

threadvar
  PromiseProto: TValue;

function Promise_Ctor(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
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

function TypedArray_Abstract(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
begin
  Result := ThrowTypeError('Abstract class TypedArray not directly constructable');
end;

function Ctor(AFn: TNativeFn; const AName: UnicodeString; ALen: Integer; var AProto: TValue): TValue;
var C: TValue;
begin
  C := NewNativeFunction(AFn, AName, ALen);
  Scratch := C;
  DefineOwn(Scratch, AtomPrototype, AProto, 0);
  DefineOwn(AProto, AtomConstructor, Scratch, pfHidden);
  DefineOwn(GlobalObj, AtomOf(AName), Scratch, pfHidden);
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
