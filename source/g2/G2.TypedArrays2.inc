{ G2.TypedArrays, part 2: %TypedArray% statics and prototype, DataView, initialization }

{ ---------------- %TypedArray% statics ---------------- }

{ 23.2.2.1 %TypedArray%.from }
function TA_From(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var Mapping: Boolean; M, L, T, V, LV: TValue; Len, K: Int64; CB: array[0..1] of TValue; Base: Integer;
begin
  if not IsConstructorV(Args[0]) then Exit(ThrowTypeError('%TypedArray%.from: this is not a constructor'));
  Mapping := Arg(Args, Argc, 2) <> VUndefined;
  if Mapping and not IsCallable(Args[2]) then Exit(ThrowTypeError('%TypedArray%.from: the mapper is not a function'));
  if not GetElemV(Arg(Args, Argc, 1), SymIterator, M) then Exit(False);
  Base := TempTop;
  if not ((M = VUndefined) or (M = VNull)) then
  begin
    if not IsCallable(M) then Exit(ThrowTypeError('@@iterator is not a function'));
    if not IterToList(Args[1], M, L) then Exit(False);
  end
  else if not ToObjectV(Args[1], L) then Exit(False);
  PushRoot(L);
  if ((M = VUndefined) or (M = VNull)) then
  begin
    if not LengthOfArrayLike(L, Len) then begin PopRoots(1); Exit(False); end;
  end
  else Len := ObjOf(L)^.Elements^.Len;
  LV := IdxVal(Len);
  if not TACreateFromCtor(Args[0], @LV, 1, Len, T) then begin PopRoots(1); Exit(False); end;
  PushRoot(T);
  for K := 0 to Len - 1 do
  begin
    if ((M = VUndefined) or (M = VNull)) then
    begin
      if not GetElemV(RootAt(Base), IdxVal(K), V) then begin PopRoots(2); Exit(False); end;
    end
    else
    begin
      V := ObjOf(RootAt(Base))^.Elements^.Data[K];
      if V = VHole then V := VUndefined;
    end;
    if Mapping then
    begin
      CB[0] := V; CB[1] := IdxVal(K);
      if not CallValue(Args[2], Arg(Args, Argc, 3), @CB[0], 2, V) then begin PopRoots(2); Exit(False); end;
    end;
    if not SetElemVS(RootAt(Base + 1), IdxVal(K), V, True) then begin PopRoots(2); Exit(False); end;
  end;
  Res := RootAt(Base + 1);
  PopRoots(2);
  Result := True;
end;

{ 23.2.2.2 %TypedArray%.of }
function TA_Of(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var LV, T: TValue; K: Integer;
begin
  if not IsConstructorV(Args[0]) then Exit(ThrowTypeError('%TypedArray%.of: this is not a constructor'));
  LV := MkInt(Argc);
  if not TACreateFromCtor(Args[0], @LV, 1, Argc, T) then Exit(False);
  PushRoot(T);
  for K := 0 to Argc - 1 do
    if not SetElemVS(RootAt(TempTop - 1), MkInt(K), Args[K + 1], True) then begin PopRoots(1); Exit(False); end;
  Res := RootAt(TempTop - 1);
  PopRoots(1);
  Result := True;
end;

{ ---------------- %TypedArray%.prototype getters ---------------- }

function NeedTA(Args: TArgs; const Name: UnicodeString): Boolean; inline;
begin
  Result := IsTA(Args[0]);
  if not Result then ThrowTypeError('%TypedArray%.prototype.' + Name + ' called on an incompatible receiver');
end;

function TAP_Buffer(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
begin
  if not NeedTA(Args, 'buffer') then Exit(False);
  Res := ObjOf(Args[0])^.Aux1;
  Result := True;
end;

function TAP_ByteLength(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
begin
  if not NeedTA(Args, 'byteLength') then Exit(False);
  Res := MkNumber(Int64(TALength(ObjOf(Args[0]))) * TAElemSize[TAKind(ObjOf(Args[0]))]);
  Result := True;
end;

function TAP_ByteOffset(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
begin
  if not NeedTA(Args, 'byteOffset') then Exit(False);
  if TAOutOfBounds(ObjOf(Args[0])) then Res := MkInt(0) else Res := MkNumber(ObjOf(Args[0])^.Pad);
  Result := True;
end;

function TAP_Length(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
begin
  if not NeedTA(Args, 'length') then Exit(False);
  Res := MkInt(TALength(ObjOf(Args[0])));
  Result := True;
end;

{ 23.2.3.38 get %TypedArray%.prototype [ @@toStringTag ] }
function TAP_ToStringTag(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
begin
  if IsTA(Args[0]) then Res := StrFromUnicode(TANames[TAKind(ObjOf(Args[0]))]) else Res := VUndefined;
  Result := True;
end;

{ ---------------- %TypedArray%.prototype methods ---------------- }

{ the receiver's element type's conversion of V: ToBigInt or ToNumber (the value to store) }
function ElemValue(O: TValue; V: TValue; var N: TValue): Boolean;
var D: Double;
begin
  if TAIsBig(ObjOf(O)) then Exit(ToBigIntV(V, N));
  if IsNumber(V) then begin N := V; Exit(True); end;
  if not ToNumberV(V, D) then Exit(False);
  N := MkNumber(D);
  Result := True;
end;

procedure StoreElem(O: PObj; K: Int64; N: TValue); inline;
begin
  if (K < 0) or (K >= TALength(O)) then Exit;
  if TAIsBig(O) then TASetBig(O, K, BigToU64(N)) else TASet(O, K, NumOf(N));
end;

{ 23.2.3.1 at }
function TAP_At(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var Len: Int64; D: Double;
begin
  if not ValidateTA(Args[0], Len) then Exit(False);
  if not ToIntInf(Arg(Args, Argc, 1), D) then Exit(False);
  if D < 0 then D := Len + D;
  if (D < 0) or (D >= Len) then Res := VUndefined else Res := TAGetV(Args[0], Trunc(D));
  Result := True;
end;

{ 23.2.3.6 copyWithin }
function TAP_CopyWithin(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var Len, To_, From, Fin, Count, NewLen: Int64; ES: Integer; O: PObj;
begin
  if not ValidateTAWrite(Args[0], Len) then Exit(False);
  if not RelIdx(Arg(Args, Argc, 1), Len, 0, To_) then Exit(False);
  if not RelIdx(Arg(Args, Argc, 2), Len, 0, From) then Exit(False);
  if not RelIdx(Arg(Args, Argc, 3), Len, Len, Fin) then Exit(False);
  Count := Min(Fin - From, Len - To_);
  if Count > 0 then
  begin
    O := ObjOf(Args[0]);
    if TAOutOfBounds(O) then Exit(ThrowTypeError('the typed array is detached or out of bounds'));
    NewLen := TALength(O);
    ES := TAElemSize[TAKind(O)];
    { the copy stays within the current length }
    Count := Min(Count, Min(NewLen - From, NewLen - To_));
    if Count > 0 then Move((TAData(O) + From * ES)^, (TAData(O) + To_ * ES)^, Count * ES);
  end;
  Res := Args[0];
  Result := True;
end;

function IterKind(Args: TArgs; Kind: Integer; var Res: TValue): Boolean;
var Len: Int64;
begin
  if not ValidateTA(Args[0], Len) then Exit(False);
  Res := NewArrayIterator(Args[0], Kind);
  Result := True;
end;

function TAP_Entries(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := IterKind(Args, 3, Res); end;
function TAP_Keys(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := IterKind(Args, 2, Res); end;
function TAP_Values(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := IterKind(Args, 4, Res); end;

type
  TTAIter = (tiEvery, tiSome, tiForEach, tiFind, tiFindIndex, tiFindLast, tiFindLastIndex);

{ every / some / forEach / find / findIndex / findLast / findLastIndex: each index is read }
function TAIterate(Args: TArgs; Argc: Integer; var Res: TValue; Kind: TTAIter): Boolean;
var Len, K: Int64; V, R: TValue; CB: array[0..2] of TValue; Rev: Boolean;
begin
  if not ValidateTA(Args[0], Len) then Exit(False);
  if not IsCallable(Arg(Args, Argc, 1)) then Exit(ThrowTypeError('callback is not a function'));
  Rev := Kind in [tiFindLast, tiFindLastIndex];
  if Rev then K := Len - 1 else K := 0;
  while (K >= 0) and (K < Len) do
  begin
    V := TAGetV(Args[0], K);
    PushRoot(V);
    CB[0] := V; CB[1] := IdxVal(K); CB[2] := Args[0];
    if not CallValue(Args[1], Arg(Args, Argc, 2), @CB[0], 3, R) then begin PopRoots(1); Exit(False); end;
    V := RootAt(TempTop - 1);
    PopRoots(1);
    case Kind of
      tiEvery: if not ToBool(R) then begin Res := VFalse; Exit(True); end;
      tiSome: if ToBool(R) then begin Res := VTrue; Exit(True); end;
      tiFind, tiFindLast: if ToBool(R) then begin Res := V; Exit(True); end;
      tiFindIndex, tiFindLastIndex: if ToBool(R) then begin Res := IdxVal(K); Exit(True); end;
    end;
    if Rev then Dec(K) else Inc(K);
  end;
  case Kind of
    tiEvery: Res := VTrue;
    tiSome: Res := VFalse;
    tiForEach, tiFind, tiFindLast: Res := VUndefined;
  else Res := MkInt(-1);
  end;
  Result := True;
end;

function TAP_Every(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := TAIterate(Args, Argc, Res, tiEvery); end;
function TAP_Some(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := TAIterate(Args, Argc, Res, tiSome); end;
function TAP_ForEach(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := TAIterate(Args, Argc, Res, tiForEach); end;
function TAP_Find(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := TAIterate(Args, Argc, Res, tiFind); end;
function TAP_FindIndex(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := TAIterate(Args, Argc, Res, tiFindIndex); end;
function TAP_FindLast(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := TAIterate(Args, Argc, Res, tiFindLast); end;
function TAP_FindLastIndex(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := TAIterate(Args, Argc, Res, tiFindLastIndex); end;

{ 23.2.3.9 fill: the value converts before the indices, the length is read again afterwards }
function TAP_Fill(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var Len, K, Fin: Int64; N: TValue; O: PObj;
begin
  if not ValidateTAWrite(Args[0], Len) then Exit(False);
  if not ElemValue(Args[0], Arg(Args, Argc, 1), N) then Exit(False);
  PushRoot(N);
  Result := False;
  if RelIdx(Arg(Args, Argc, 2), Len, 0, K) and RelIdx(Arg(Args, Argc, 3), Len, Len, Fin) then
  begin
    O := ObjOf(Args[0]);
    if TAOutOfBounds(O) then begin PopRoots(1); Exit(ThrowTypeError('the typed array is detached or out of bounds')); end;
    Fin := Min(Fin, TALength(O));
    while K < Fin do
    begin
      StoreElem(O, K, RootAt(TempTop - 1));
      Inc(K);
    end;
    Res := Args[0];
    Result := True;
  end;
  PopRoots(1);
end;

{ 23.2.3.10 filter }
function TAP_Filter(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var Len, K, N: Int64; V, R, LV, T: TValue; CB: array[0..2] of TValue; Base: Integer;
begin
  if not ValidateTA(Args[0], Len) then Exit(False);
  if not IsCallable(Arg(Args, Argc, 1)) then Exit(ThrowTypeError('callback is not a function'));
  Base := TempTop;
  PushRoot(MkPtr(NewArray(0, 8)));
  for K := 0 to Len - 1 do
  begin
    V := TAGetV(Args[0], K);
    PushRoot(V);
    CB[0] := V; CB[1] := IdxVal(K); CB[2] := Args[0];
    if not CallValue(Args[1], Arg(Args, Argc, 2), @CB[0], 3, R) then begin PopRoots(2); Exit(False); end;
    V := RootAt(TempTop - 1);
    PopRoots(1);
    if ToBool(R) then ArrayPush(ObjOf(RootAt(Base)), V);
  end;
  N := ObjOf(RootAt(Base))^.Elements^.Len;
  LV := IdxVal(N);
  if not TASpeciesCreate(Args[0], @LV, 1, N, T) then begin PopRoots(1); Exit(False); end;
  PushRoot(T);
  for K := 0 to N - 1 do
    if not TAStoreValue(RootAt(Base + 1), K, ObjOf(RootAt(Base))^.Elements^.Data[K]) then begin PopRoots(2); Exit(False); end;
  Res := RootAt(Base + 1);
  PopRoots(2);
  Result := True;
end;

function SameValueZeroV(A, B: TValue): Boolean;
begin
  if IsNumber(A) and IsNumber(B) then Exit((NumOf(A) = NumOf(B)) or (IsNaN(NumOf(A)) and IsNaN(NumOf(B))));
  if IsBigInt(A) and IsBigInt(B) then Exit(BigEquals(A, B));
  Result := StrictEquals(A, B);
end;

function StrictEq(A, B: TValue): Boolean;
begin
  if IsBigInt(A) and IsBigInt(B) then Exit(BigEquals(A, B));
  Result := StrictEquals(A, B);
end;

{ 23.2.3.16 includes, .17 indexOf, .20 lastIndexOf }
function TASearch(Args: TArgs; Argc: Integer; var Res: TValue; Mode: Integer): Boolean;
var Len, K: Int64; D: Double; V: TValue;
begin
  if not ValidateTA(Args[0], Len) then Exit(False);
  if Mode = 0 then Res := VFalse else Res := MkInt(-1);
  if Len = 0 then Exit(True);
  if Mode = 2 then
  begin
    if Argc >= 2 then
    begin
      if not ToIntInf(Args[2], D) then Exit(False);
    end
    else D := Len - 1;
    if IsInfinite(D) and (D < 0) then Exit(True);
    if D >= 0 then D := Min(D, Len - 1) else D := Len + D;
    K := Trunc(D);
    while K >= 0 do
    begin
      if K < TALength(ObjOf(Args[0])) then
      begin
        V := TAGet(ObjOf(Args[0]), K);
        if StrictEq(V, Arg(Args, Argc, 1)) then begin Res := IdxVal(K); Exit(True); end;
      end;
      Dec(K);
    end;
    Exit(True);
  end;
  if not ToIntInf(Arg(Args, Argc, 2), D) then Exit(False);
  if D >= Len then Exit(True);
  if D < 0 then begin D := Len + D; if D < 0 then D := 0; end;
  K := Trunc(D);
  while K < Len do
  begin
    if Mode = 0 then
    begin
      if SameValueZeroV(TAGetV(Args[0], K), Arg(Args, Argc, 1)) then begin Res := VTrue; Exit(True); end;
    end
    else if (K < TALength(ObjOf(Args[0]))) and StrictEq(TAGet(ObjOf(Args[0]), K), Arg(Args, Argc, 1)) then
    begin
      Res := IdxVal(K);
      Exit(True);
    end;
    Inc(K);
  end;
  Result := True;
end;

function TAP_Includes(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := TASearch(Args, Argc, Res, 0); end;
function TAP_IndexOf(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := TASearch(Args, Argc, Res, 1); end;
function TAP_LastIndexOf(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := TASearch(Args, Argc, Res, 2); end;

{ 23.2.3.18 join and 23.2.3.31 toLocaleString }
function TAJoin(Args: TArgs; Argc: Integer; var Res: TValue; Locale: Boolean): Boolean;
var Len, K: Int64; Sep, SV, V, F: TValue; B: UnicodeString; LA: array[0..1] of TValue;
begin
  if not ValidateTA(Args[0], Len) then Exit(False);
  if Locale or (Arg(Args, Argc, 1) = VUndefined) then Sep := StrFromUnicode(',')
  else if not ToStringV(Args[1], Sep) then Exit(False);
  PushRoot(Sep);
  B := '';
  for K := 0 to Len - 1 do
  begin
    if K > 0 then B := B + StrToUnicode(StrOf(RootAt(TempTop - 1)));
    V := TAGetV(Args[0], K);
    if V = VUndefined then Continue;
    if Locale then
    begin
      PushRoot(V);
      if not GetV(V, AtomOf('toLocaleString'), F) then begin PopRoots(2); Exit(False); end;
      V := RootAt(TempTop - 1);
      PopRoots(1);
      if not IsCallable(F) then begin PopRoots(1); Exit(ThrowTypeError('toLocaleString is not a function')); end;
      LA[0] := Arg(Args, Argc, 1); LA[1] := Arg(Args, Argc, 2);   { ECMA-402: locales and options pass through }
      if not CallValue(F, V, @LA[0], 2, V) then begin PopRoots(1); Exit(False); end;
    end;
    if not ToStringV(V, SV) then begin PopRoots(1); Exit(False); end;
    B := B + StrToUnicode(StrOf(SV));
  end;
  PopRoots(1);
  Res := StrFromUnicode(B);
  Result := True;
end;

function TAP_Join(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := TAJoin(Args, Argc, Res, False); end;
function TAP_ToLocaleString(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := TAJoin(Args, Argc, Res, True); end;

{ 23.2.3.22 map }
function TAP_Map(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var Len, K: Int64; V, R, LV, T: TValue; CB: array[0..2] of TValue; Base: Integer;
begin
  if not ValidateTA(Args[0], Len) then Exit(False);
  if not IsCallable(Arg(Args, Argc, 1)) then Exit(ThrowTypeError('callback is not a function'));
  LV := IdxVal(Len);
  if not TASpeciesCreate(Args[0], @LV, 1, Len, T) then Exit(False);
  Base := TempTop;
  PushRoot(T);
  for K := 0 to Len - 1 do
  begin
    V := TAGetV(Args[0], K);
    CB[0] := V; CB[1] := IdxVal(K); CB[2] := Args[0];
    if not CallValue(Args[1], Arg(Args, Argc, 2), @CB[0], 3, R) then begin PopRoots(1); Exit(False); end;
    if not TAStoreValue(RootAt(Base), K, R) then begin PopRoots(1); Exit(False); end;
  end;
  Res := RootAt(Base);
  PopRoots(1);
  Result := True;
end;

{ 23.2.3.23 reduce, .24 reduceRight }
function TAReduce(Args: TArgs; Argc: Integer; var Res: TValue; Right: Boolean): Boolean;
var Len, K: Int64; V: TValue; CB: array[0..3] of TValue; Base: Integer;
begin
  if not ValidateTA(Args[0], Len) then Exit(False);
  if not IsCallable(Arg(Args, Argc, 1)) then Exit(ThrowTypeError('callback is not a function'));
  if (Len = 0) and (Argc < 2) then Exit(ThrowTypeError('Reduce of empty array with no initial value'));
  if Right then K := Len - 1 else K := 0;
  Base := TempTop;
  PushRoot(VUndefined);
  if Argc >= 2 then TempRoots[Base] := Args[2]
  else
  begin
    TempRoots[Base] := TAGetV(Args[0], K);
    if Right then Dec(K) else Inc(K);
  end;
  while (K >= 0) and (K < Len) do
  begin
    CB[0] := RootAt(Base); CB[1] := TAGetV(Args[0], K); CB[2] := IdxVal(K); CB[3] := Args[0];
    if not CallValue(Args[1], VUndefined, @CB[0], 4, V) then begin PopRoots(1); Exit(False); end;
    TempRoots[Base] := V;
    if Right then Dec(K) else Inc(K);
  end;
  Res := RootAt(Base);
  PopRoots(1);
  Result := True;
end;

function TAP_Reduce(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := TAReduce(Args, Argc, Res, False); end;
function TAP_ReduceRight(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := TAReduce(Args, Argc, Res, True); end;

{ 23.2.3.25 reverse }
function TAP_Reverse(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var Len, Lo, Hi: Int64; ES, J: Integer; P: PByte; T: Byte;
begin
  if not ValidateTAWrite(Args[0], Len) then Exit(False);
  ES := TAElemSize[TAKind(ObjOf(Args[0]))];
  P := TAData(ObjOf(Args[0]));
  Lo := 0; Hi := Len - 1;
  while Lo < Hi do
  begin
    for J := 0 to ES - 1 do
    begin
      T := P[Lo * ES + J]; P[Lo * ES + J] := P[Hi * ES + J]; P[Hi * ES + J] := T;
    end;
    Inc(Lo); Dec(Hi);
  end;
  Res := Args[0];
  Result := True;
end;

{ 23.2.3.26 set }
function TAP_Set(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var D: Double; Off, TLen, SLen, K: Int64; T, S: PObj; Src, V: TValue; Clone: array of Byte; SP: PByte;
  SK, TK, ES: Integer; Tmp: PObj;
begin
  if not NeedTA(Args, 'set') then Exit(False);
  if ViewImmutable(ObjOf(Args[0])) then Exit(ThrowTypeError('the typed array''s buffer is immutable'));
  if not ToIntInf(Arg(Args, Argc, 2), D) then Exit(False);
  if D < 0 then Exit(ThrowError('RangeError', 'offset is out of bounds'));
  Src := Arg(Args, Argc, 1);
  T := ObjOf(Args[0]);
  if TAOutOfBounds(T) then Exit(ThrowTypeError('the typed array is detached or out of bounds'));
  TLen := TALength(T);
  if IsTA(Src) then
  begin
    { 23.2.3.26.1 SetTypedArrayFromTypedArray }
    S := ObjOf(Src);
    if TAOutOfBounds(S) then Exit(ThrowTypeError('the source typed array is detached or out of bounds'));
    SLen := TALength(S);
    if TAIsBig(T) <> TAIsBig(S) then Exit(ThrowTypeError('Content type mismatch: BigInt and Number typed arrays'));
    if IsInfinite(D) or (SLen + D > TLen) then Exit(ThrowError('RangeError', 'offset is out of bounds'));
    Off := Trunc(D);
    SK := TAKind(S); TK := TAKind(T);
    if SK = TK then
    begin
      if SLen > 0 then Move(TAData(S)^, (TAData(T) + Off * TAElemSize[TK])^, SLen * TAElemSize[TK]);
    end
    else
    begin
      { a copy of the source's bytes first: the two may share the buffer }
      ES := TAElemSize[SK];
      SetLength(Clone, SLen * ES + 8);
      if SLen > 0 then Move(TAData(S)^, Clone[0], SLen * ES);
      SP := @Clone[0];
      for K := 0 to SLen - 1 do
        if TAIsBig(T) then TASetBig(T, Off + K, PQWord(SP)[K])
        else
          case SK of
            taInt8: TASet(T, Off + K, PShortInt(SP)[K]);
            taUint8, taUint8C: TASet(T, Off + K, SP[K]);
            taInt16: TASet(T, Off + K, PSmallInt(SP)[K]);
            taUint16: TASet(T, Off + K, PWord(SP)[K]);
            taInt32: TASet(T, Off + K, PLongInt(SP)[K]);
            taUint32: TASet(T, Off + K, PLongWord(SP)[K]);
            taFloat32: TASet(T, Off + K, PSingle(SP)[K]);
            taFloat16: TASet(T, Off + K, HalfToDouble(PWord(SP)[K]));
          else TASet(T, Off + K, PDouble(SP)[K]);
          end;
    end;
    Res := VUndefined;
    Exit(True);
  end;
  { 23.2.3.26.2 SetTypedArrayFromArrayLike }
  if not ToObjectV(Src, V) then Exit(False);
  PushRoot(V);
  if not LengthOfArrayLike(V, SLen) then begin PopRoots(1); Exit(False); end;
  if IsInfinite(D) or (SLen + D > TLen) then begin PopRoots(1); Exit(ThrowError('RangeError', 'offset is out of bounds')); end;
  Off := Trunc(D);
  for K := 0 to SLen - 1 do
  begin
    if not GetElemV(RootAt(TempTop - 1), IdxVal(K), V) then begin PopRoots(1); Exit(False); end;
    if not TAStoreValue(Args[0], Off + K, V) then begin PopRoots(1); Exit(False); end;
  end;
  PopRoots(1);
  Res := VUndefined;
  Result := True;
end;

{ 23.2.3.27 slice }
function TAP_Slice(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var Len, K, Fin, Count, N: Int64; LV, T: TValue; O, A: PObj; ES: Integer;
begin
  if not ValidateTA(Args[0], Len) then Exit(False);
  if not RelIdx(Arg(Args, Argc, 1), Len, 0, K) then Exit(False);
  if not RelIdx(Arg(Args, Argc, 2), Len, Len, Fin) then Exit(False);
  Count := Max(Fin - K, 0);
  LV := IdxVal(Count);
  if not TASpeciesCreate(Args[0], @LV, 1, Count, T) then Exit(False);
  if Count > 0 then
  begin
    O := ObjOf(Args[0]);
    if TAOutOfBounds(O) then Exit(ThrowTypeError('the typed array is detached or out of bounds'));
    Fin := Min(Fin, TALength(O));
    Count := Max(Fin - K, 0);
    A := ObjOf(T);
    if TAKind(A) = TAKind(O) then
    begin
      ES := TAElemSize[TAKind(O)];
      { a byte copy, element by element in order (the two may share the buffer) }
      for N := 0 to Count * ES - 1 do (TAData(A) + N)^ := (TAData(O) + K * ES + N)^;
    end
    else
    begin
      PushRoot(T);
      N := 0;
      while K < Fin do
      begin
        LV := TAGetV(Args[0], K);   { may allocate (a BigInt): read before the rooted target }
        if not TAStoreValue(RootAt(TempTop - 1), N, LV) then begin PopRoots(1); Exit(False); end;
        Inc(K); Inc(N);
      end;
      T := RootAt(TempTop - 1);
      PopRoots(1);
    end;
  end;
  Res := T;
  Result := True;
end;

{ 23.2.3.30.1 TypedArray SortCompare: Less when x sorts before y }
function TALess(X, Y: TValue; var Less: Boolean): Boolean;
var CB: array[0..1] of TValue; R: TValue; D, A, B: Double;
begin
  Less := False;
  if TASortFn <> VUndefined then
  begin
    CB[0] := X; CB[1] := Y;
    if not CallValue(TASortFn, VUndefined, @CB[0], 2, R) then Exit(False);
    if not ToNumberV(R, D) then Exit(False);
    Less := D < 0;
    Exit(True);
  end;
  if IsBigInt(X) then begin Less := BigCompare(X, Y) < 0; Exit(True); end;
  A := NumOf(X); B := NumOf(Y);
  if IsNaN(A) then Exit(True);
  if IsNaN(B) then begin Less := True; Exit(True); end;
  if A < B then Less := True
  else if (A = 0) and (B = 0) then Less := (PQWord(@A)^ <> 0) and (PQWord(@B)^ = 0);   { -0 before +0 }
  Result := True;
end;

{ the receiver's Len values into a new array at root Base, stable merge-sorted (scratch at Base + 1) }
function TASortValues(O: TValue; Len: Int64; Base: Integer): Boolean;
var K, N, W, I, L, M, R_, J: Int64; Less: Boolean; V, T: TValue; Src: PValueArray;
begin
  TempRoots[Base] := MkPtr(NewArray(0, Max(Len, 1)));
  for K := 0 to Len - 1 do ArrayPush(ObjOf(RootAt(Base)), TAGetV(O, K));
  N := ObjOf(RootAt(Base))^.Elements^.Len;
  TempRoots[Base + 1] := MkPtr(NewArray(N, N));
  W := 1;
  while W < N do
  begin
    I := 0;
    while I < N do
    begin
      L := I; M := Min(I + W, N); R_ := Min(I + 2 * W, N);
      J := I;
      while (L < Min(I + W, N)) or (M < R_) do
      begin
        Src := ObjOf(RootAt(Base))^.Elements;
        if L >= Min(I + W, N) then begin V := Src^.Data[M]; Inc(M); end
        else if M >= R_ then begin V := Src^.Data[L]; Inc(L); end
        else
        begin
          if not TALess(Src^.Data[M], Src^.Data[L], Less) then Exit(False);
          Src := ObjOf(RootAt(Base))^.Elements;
          if Less then begin V := Src^.Data[M]; Inc(M); end
          else begin V := Src^.Data[L]; Inc(L); end;
        end;
        ValueArrayStore(ObjOf(RootAt(Base + 1))^.Elements, J, V);
        Inc(J);
      end;
      Inc(I, 2 * W);
    end;
    T := TempRoots[Base]; TempRoots[Base] := TempRoots[Base + 1]; TempRoots[Base + 1] := T;
    W := W * 2;
  end;
  Result := True;
end;

function TASortCommon(Args: TArgs; Argc: Integer; var Res: TValue; Copy: Boolean): Boolean;
var Len, K: Int64; Saved, T: TValue; Base: Integer;
begin
  if (Arg(Args, Argc, 1) <> VUndefined) and not IsCallable(Args[1]) then
    Exit(ThrowTypeError('The comparison function must be either a function or undefined'));
  if not Copy and IsTA(Args[0]) and ViewImmutable(ObjOf(Args[0])) then
    Exit(ThrowTypeError('the typed array''s buffer is immutable'));
  if not ValidateTA(Args[0], Len) then Exit(False);
  Base := TempTop;
  PushRoot(TASortFn); PushRoot(VUndefined); PushRoot(VUndefined); PushRoot(VUndefined);
  Saved := TASortFn;
  TASortFn := Arg(Args, Argc, 1);
  Result := False;
  if Copy then
  begin
    if not TACreateSameType(Args[0], Len, T) then begin TASortFn := RootAt(Base); PopRoots(4); Exit; end;
    TempRoots[Base + 3] := T;
  end
  else TempRoots[Base + 3] := Args[0];
  if TASortValues(Args[0], Len, Base + 1) then
  begin
    for K := 0 to Len - 1 do
      StoreElem(ObjOf(RootAt(Base + 3)), K, ObjOf(RootAt(Base + 1))^.Elements^.Data[K]);
    Res := RootAt(Base + 3);
    Result := True;
  end;
  TASortFn := RootAt(Base);
  PopRoots(4);
end;

function TAP_Sort(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := TASortCommon(Args, Argc, Res, False); end;
function TAP_ToSorted(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := TASortCommon(Args, Argc, Res, True); end;

{ 23.2.3.29 subarray }
function TAP_Subarray(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var O: PObj; SrcLen, B, E, ES: Int64; CA: array[0..2] of TValue; N: Integer; Tracking: Boolean;
begin
  if not NeedTA(Args, 'subarray') then Exit(False);
  O := ObjOf(Args[0]);
  if TAOutOfBounds(O) then SrcLen := 0 else SrcLen := TALength(O);
  Tracking := IntOf(O^.Aux2) < 0;
  if not RelIdx(Arg(Args, Argc, 1), SrcLen, 0, B) then Exit(False);
  N := 2;
  if not (Tracking and (Arg(Args, Argc, 2) = VUndefined)) then
  begin
    if not RelIdx(Arg(Args, Argc, 2), SrcLen, SrcLen, E) then Exit(False);
    CA[2] := IdxVal(Max(E - B, 0));
    N := 3;
  end;
  O := ObjOf(Args[0]);
  ES := TAElemSize[TAKind(O)];
  CA[0] := O^.Aux1;
  CA[1] := MkNumber(O^.Pad + B * ES);
  Result := TASpeciesCreate(Args[0], @CA[0], N, -1, Res, False);
end;

{ 23.2.3.33 toReversed }
function TAP_ToReversed(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var Len, K: Int64; T: TValue;
begin
  if not ValidateTA(Args[0], Len) then Exit(False);
  if not TACreateSameType(Args[0], Len, T) then Exit(False);
  for K := 0 to Len - 1 do
    StoreElem(ObjOf(T), K, TAGetV(Args[0], Len - 1 - K));
  Res := T;
  Result := True;
end;

{ 23.2.3.36 with }
function TAP_With(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var Len, K, Idx: Int64; D: Double; N, T: TValue; Base: Integer;
begin
  if not ValidateTA(Args[0], Len) then Exit(False);
  if not ToIntInf(Arg(Args, Argc, 1), D) then Exit(False);
  if D < 0 then D := Len + D;
  if not ElemValue(Args[0], Arg(Args, Argc, 2), N) then Exit(False);
  if (D < 0) or (D >= TALength(ObjOf(Args[0]))) then Exit(ThrowError('RangeError', 'Invalid typed array index'));
  Idx := Trunc(D);
  Base := TempTop;
  PushRoot(N);
  if not TACreateSameType(Args[0], Len, T) then begin PopRoots(1); Exit(False); end;
  PushRoot(T);
  for K := 0 to Len - 1 do
    if K = Idx then StoreElem(ObjOf(RootAt(Base + 1)), K, RootAt(Base))
    else if K < TALength(ObjOf(Args[0])) then
    begin
      N := TAGetV(Args[0], K);   { a BigInt read allocates: the new array is re-read after it }
      StoreElem(ObjOf(RootAt(Base + 1)), K, N);
    end
    else
    begin
      { the index's coercion shrank the buffer: Get reads undefined, which Set converts (ToBigInt throws) }
      if TAIsBig(ObjOf(Args[0])) then begin PopRoots(2); Exit(ThrowTypeError('Cannot convert undefined to a BigInt')); end;
      StoreElem(ObjOf(RootAt(Base + 1)), K, MkNumber(NaN));
    end;
  T := RootAt(Base + 1);
  PopRoots(2);
  Res := T;
  Result := True;
end;

{ ---------------- DataView ---------------- }

function IsDV(V: TValue): Boolean; inline;
begin
  Result := IsObject(V) and (ObjOf(V)^.OKind = okDataView);
end;

{ 25.3.1.3 IsViewOutOfBounds }
function DVOutOfBounds(O: PObj): Boolean;
var B: PObj; BL, L: Int64;
begin
  B := ObjOf(O^.Aux1);
  if BufferDetached(B) then Exit(True);
  BL := BufferLen(B);
  L := IntOf(O^.Aux2);
  if L < 0 then Exit(O^.Pad > BL);
  Result := (O^.Pad > BL) or (O^.Pad + L > BL);
end;

function DVLength(O: PObj): Int64;
begin
  if IntOf(O^.Aux2) < 0 then Result := BufferLen(ObjOf(O^.Aux1)) - O^.Pad else Result := IntOf(O^.Aux2);
end;

{ 25.3.2.1 DataView ( buffer [ , byteOffset [ , byteLength ] ] ) }
function DV_Ctor2(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var NT, P: TValue; Off, VL, BL: Int64; B, O: PObj; Auto: Boolean; Base: Integer;
begin
  NT := CurNewTarget;
  if NT = VUndefined then Exit(ThrowTypeError('Constructor DataView requires ''new'''));
  ConstructProtoTaken := True;
  if not IsBuffer(Arg(Args, Argc, 1)) then Exit(ThrowTypeError('First argument to DataView constructor must be an ArrayBuffer'));
  Base := TempTop;
  PushRoot(NT);
  Result := False;
  try
    if not ToIndex(Arg(Args, Argc, 2), Off) then Exit;
    B := ObjOf(Args[1]);
    if BufferDetached(B) then Exit(ThrowTypeError('the ArrayBuffer is detached'));
    BL := BufferLen(B);
    if Off > BL then Exit(ThrowError('RangeError', 'Start offset ' + IntToStr(Off) + ' is outside the bounds of the buffer'));
    Auto := False;
    VL := 0;
    if Arg(Args, Argc, 3) = VUndefined then
    begin
      if (B^.OFlags and bfResizable) = 0 then VL := BL - Off else Auto := True;
    end
    else
    begin
      if not ToIndex(Args[3], VL) then Exit;
      if Off + VL > BL then Exit(ThrowError('RangeError', 'Invalid DataView length ' + IntToStr(VL)));
    end;
    if not ProtoFromCtor(RootAt(Base), DataViewProtoV, P) then Exit;
    B := ObjOf(Args[1]);
    if BufferDetached(B) then Exit(ThrowTypeError('the ArrayBuffer is detached'));
    BL := BufferLen(B);
    if Off > BL then Exit(ThrowError('RangeError', 'Start offset ' + IntToStr(Off) + ' is outside the bounds of the buffer'));
    if (Arg(Args, Argc, 3) <> VUndefined) and (Off + VL > BL) then Exit(ThrowError('RangeError', 'Invalid DataView length ' + IntToStr(VL)));
    O := NewObject(RootShapeFor(P), 0, okDataView);
    O^.Aux1 := Args[1];
    if Auto then O^.Aux2 := MkInt(-1) else O^.Aux2 := MkInt(VL);
    O^.Pad := Off;
    Res := MkPtr(O);
    Result := True;
  finally
    PopRoots(1);
  end;
end;

function NeedDV(Args: TArgs; const Name: UnicodeString): Boolean; inline;
begin
  Result := IsDV(Args[0]);
  if not Result then ThrowTypeError('DataView.prototype.' + Name + ' called on an incompatible receiver');
end;

function DVP_Buffer(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
begin
  if not NeedDV(Args, 'buffer') then Exit(False);
  Res := ObjOf(Args[0])^.Aux1;
  Result := True;
end;

function DVP_ByteLength(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
begin
  if not NeedDV(Args, 'byteLength') then Exit(False);
  if DVOutOfBounds(ObjOf(Args[0])) then Exit(ThrowTypeError('the DataView is detached or out of bounds'));
  Res := MkNumber(DVLength(ObjOf(Args[0])));
  Result := True;
end;

function DVP_ByteOffset(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
begin
  if not NeedDV(Args, 'byteOffset') then Exit(False);
  if DVOutOfBounds(ObjOf(Args[0])) then Exit(ThrowTypeError('the DataView is detached or out of bounds'));
  Res := MkNumber(ObjOf(Args[0])^.Pad);
  Result := True;
end;

{ 25.3.1.5 GetViewValue / 25.3.1.6 SetViewValue; Kind a typed array kind }
function DVAccess2(Args: TArgs; Argc: Integer; var Res: TValue; Kind: Integer; IsSet: Boolean): Boolean;
var Idx: Int64; Little: Boolean; N, LE: TValue; O: PObj; ES, I: Integer; Bytes: array[0..7] of Byte; P: PByte;
  Q: QWord; D: Double; Base: Integer;
begin
  if not NeedDV(Args, 'get/set') then Exit(False);
  if IsSet and ViewImmutable(ObjOf(Args[0])) then Exit(ThrowTypeError('the DataView''s buffer is immutable'));
  if not ToIndex(Arg(Args, Argc, 1), Idx) then Exit(False);
  Base := TempTop;
  N := VUndefined;
  if IsSet then
  begin
    if Kind >= taBigInt64 then
    begin
      if not ToBigIntV(Arg(Args, Argc, 2), N) then Exit(False);
    end
    else
    begin
      if not ToNumberV(Arg(Args, Argc, 2), D) then Exit(False);
      N := MkNumber(D);
    end;
    LE := Arg(Args, Argc, 3);
  end
  else LE := Arg(Args, Argc, 2);
  PushRoot(N);
  Little := ToBool(LE);
  O := ObjOf(Args[0]);
  if DVOutOfBounds(O) then begin PopRoots(1); Exit(ThrowTypeError('the DataView is detached or out of bounds')); end;
  ES := TAElemSize[Kind];
  if Idx + ES > DVLength(O) then begin PopRoots(1); Exit(ThrowError('RangeError', 'Offset is outside the bounds of the DataView')); end;
  P := BufferData(ObjOf(O^.Aux1)) + O^.Pad + Idx;
  if IsSet then
  begin
    N := RootAt(Base);
    case Kind of
      taBigInt64, taBigUint64: PQWord(@Bytes[0])^ := BigToU64(N);
    else
      begin
        { a scratch one-element typed array conversion: TASet's rules for the kind }
        D := NumOf(N);
        case Kind of
          taFloat32: PSingle(@Bytes[0])^ := D;
          taFloat64: PDouble(@Bytes[0])^ := D;
          taFloat16: PWord(@Bytes[0])^ := DoubleToHalf(D);
        else
          begin
            if IsNaN(D) or IsInfinite(D) then Q := 0
            else Q := QWord(Int64(Trunc(Int(D) - 4294967296.0 * DFloor(Int(D) / 4294967296.0))));
            PLongWord(@Bytes[0])^ := LongWord(Q);
          end;
        end;
      end;
    end;
    for I := 0 to ES - 1 do
      if Little then P[I] := Bytes[I] else P[I] := Bytes[ES - 1 - I];
    Res := VUndefined;
  end
  else
  begin
    for I := 0 to ES - 1 do
      if Little then Bytes[I] := P[I] else Bytes[I] := P[ES - 1 - I];
    case Kind of
      taInt8: Res := MkInt(PShortInt(@Bytes[0])^);
      taUint8: Res := MkInt(Bytes[0]);
      taInt16: Res := MkInt(PSmallInt(@Bytes[0])^);
      taUint16: Res := MkInt(PWord(@Bytes[0])^);
      taInt32: Res := MkInt(PLongInt(@Bytes[0])^);
      taUint32: Res := MkNumber(PLongWord(@Bytes[0])^);
      taFloat16: Res := MkNumber(HalfToDouble(PWord(@Bytes[0])^));
      taFloat32: Res := MkNumber(PSingle(@Bytes[0])^);
      taFloat64: Res := MkNumber(PDouble(@Bytes[0])^);
      taBigInt64: Res := BigFromInt64(PInt64(@Bytes[0])^);
    else Res := BigFromUInt64(PQWord(@Bytes[0])^);
    end;
  end;
  PopRoots(1);
  Result := True;
end;

function DVG0(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := DVAccess2(Args, Argc, Res, taInt8, False); end;
function DVG1(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := DVAccess2(Args, Argc, Res, taUint8, False); end;
function DVG3(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := DVAccess2(Args, Argc, Res, taInt16, False); end;
function DVG4(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := DVAccess2(Args, Argc, Res, taUint16, False); end;
function DVG5(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := DVAccess2(Args, Argc, Res, taInt32, False); end;
function DVG6(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := DVAccess2(Args, Argc, Res, taUint32, False); end;
function DVG7(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := DVAccess2(Args, Argc, Res, taFloat32, False); end;
function DVG8(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := DVAccess2(Args, Argc, Res, taFloat64, False); end;
function DVG9(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := DVAccess2(Args, Argc, Res, taFloat16, False); end;
function DVG10(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := DVAccess2(Args, Argc, Res, taBigInt64, False); end;
function DVG11(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := DVAccess2(Args, Argc, Res, taBigUint64, False); end;
function DVS0(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := DVAccess2(Args, Argc, Res, taInt8, True); end;
function DVS1(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := DVAccess2(Args, Argc, Res, taUint8, True); end;
function DVS3(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := DVAccess2(Args, Argc, Res, taInt16, True); end;
function DVS4(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := DVAccess2(Args, Argc, Res, taUint16, True); end;
function DVS5(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := DVAccess2(Args, Argc, Res, taInt32, True); end;
function DVS6(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := DVAccess2(Args, Argc, Res, taUint32, True); end;
function DVS7(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := DVAccess2(Args, Argc, Res, taFloat32, True); end;
function DVS8(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := DVAccess2(Args, Argc, Res, taFloat64, True); end;
function DVS9(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := DVAccess2(Args, Argc, Res, taFloat16, True); end;
function DVS10(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := DVAccess2(Args, Argc, Res, taBigInt64, True); end;
function DVS11(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := DVAccess2(Args, Argc, Res, taBigUint64, True); end;

{ 21.3.2.17 Math.f16round }
function M_F16Round(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var D: Double;
begin
  if not ToNumberV(Arg(Args, Argc, 1), D) then Exit(False);
  Res := MkNumber(HalfToDouble(DoubleToHalf(D)));
  Result := True;
end;

{ ---------------- initialization ---------------- }

procedure InitTypedArrays;
const
  CtorFns: array[0..TAKindCount - 1] of TNativeFn = (TA_C0, TA_C1, TA_C2, TA_C3, TA_C4, TA_C5, TA_C6, TA_C7, TA_C8,
    TA_C9, TA_C10, TA_C11);
var I: Integer; F, AP, Tmp: TValue;
  procedure Def(Target: TValue; const Name: UnicodeString; Fn: TNativeFn; Len: Integer);
  begin
    PushRoot(Target);
    DefineOwn(RootAt(TempTop - 1), AtomOf(Name), NewNativeFunction(Fn, Name, Len), pfHidden);
    PopRoots(1);
  end;
  procedure Getter(Target: TValue; const Name: UnicodeString; Fn: TNativeFn);
  begin
    PushRoot(Target);
    DefineNativeGetterKey(RootAt(TempTop - 1), AtomOf(Name), 'get ' + Name, Fn);
    PopRoots(1);
  end;
  procedure Tag(Target: TValue; const Name: UnicodeString);
  begin
    PushRoot(Target);
    DefineOwn(RootAt(TempTop - 1), SymKey(SymToStringTagW), StrFromUnicode(Name), pfConfigurable);
    PopRoots(1);
  end;
  { a constructor: prototype (fixed), prototype.constructor, the global binding }
  function MakeCtor(Fn: TNativeFn; const Name: UnicodeString; Len: Integer; Proto: TValue; Global: Boolean): TValue;
  begin
    PushRoot(Proto);
    PushRoot(NewNativeFunction(Fn, Name, Len));
    DefineOwn(RootAt(TempTop - 1), AtomPrototype, RootAt(TempTop - 2), 0);
    DefineOwn(RootAt(TempTop - 2), AtomConstructor, RootAt(TempTop - 1), pfHidden);
    if Global then DefineOwn(GlobalObj, AtomOf(Name), RootAt(TempTop - 1), pfHidden);
    Result := RootAt(TempTop - 1);
    PopRoots(2);
  end;
begin
  RealmVals([@ArrayBufferCtor, @ArrayBufferProtoV, @TypedArrayCtorV, @TypedArrayProtoV, @DataViewProtoV, @TASortFn]);
  RealmValRange(@TACtors[0], Length(TACtors));
  RealmValRange(@TAProtosV[0], Length(TAProtosV));
  AddRoot(@ArrayBufferCtor); AddRoot(@ArrayBufferProtoV); AddRoot(@TypedArrayCtorV); AddRoot(@TypedArrayProtoV);
  AddRoot(@DataViewProtoV); AddRoot(@TASortFn);
  AddRootRange(@TACtors[0], TAKindCount);    { not @TACtors[I]: see AddRootRange }
  AddRootRange(@TAProtosV[0], TAKindCount);
  TASortFn := VUndefined;

  { ArrayBuffer }
  ArrayBufferProtoV := MkPtr(NewPlainObject(ObjectProto, 16));
  ArrayBufferCtor := MakeCtor(AB_Ctor, 'ArrayBuffer', 1, ArrayBufferProtoV, True);
  Def(ArrayBufferCtor, 'isView', AB_IsView, 1);
  PushRoot(ArrayBufferCtor);
  DefineNativeGetterKey(RootAt(TempTop - 1), SymKey(SymSpecies), 'get [Symbol.species]', Species_Getter);
  PopRoots(1);
  Getter(ArrayBufferProtoV, 'byteLength', AB_ByteLength);
  Getter(ArrayBufferProtoV, 'maxByteLength', AB_MaxByteLength);
  Getter(ArrayBufferProtoV, 'resizable', AB_Resizable);
  Getter(ArrayBufferProtoV, 'detached', AB_Detached);
  Def(ArrayBufferProtoV, 'slice', AB_Slice, 2);
  Def(ArrayBufferProtoV, 'resize', AB_Resize, 1);
  Def(ArrayBufferProtoV, 'transfer', AB_Transfer, 0);
  Def(ArrayBufferProtoV, 'transferToFixedLength', AB_TransferToFixed, 0);
  Def(ArrayBufferProtoV, 'transferToImmutable', AB_TransferToImmutable, 0);
  Def(ArrayBufferProtoV, 'sliceToImmutable', AB_SliceToImmutable, 2);
  Getter(ArrayBufferProtoV, 'immutable', AB_Immutable);
  Tag(ArrayBufferProtoV, 'ArrayBuffer');

  { %TypedArray% and %TypedArray%.prototype }
  TypedArrayProtoV := MkPtr(NewPlainObject(ObjectProto, 48));
  TypedArrayCtorV := MakeCtor(TA_Abstract, 'TypedArray', 0, TypedArrayProtoV, False);
  Def(TypedArrayCtorV, 'from', TA_From, 1);
  Def(TypedArrayCtorV, 'of', TA_Of, 0);
  PushRoot(TypedArrayCtorV);
  DefineNativeGetterKey(RootAt(TempTop - 1), SymKey(SymSpecies), 'get [Symbol.species]', Species_Getter);
  PopRoots(1);
  Getter(TypedArrayProtoV, 'buffer', TAP_Buffer);
  Getter(TypedArrayProtoV, 'byteLength', TAP_ByteLength);
  Getter(TypedArrayProtoV, 'byteOffset', TAP_ByteOffset);
  Getter(TypedArrayProtoV, 'length', TAP_Length);
  PushRoot(TypedArrayProtoV);
  DefineNativeGetterKey(RootAt(TempTop - 1), SymKey(SymToStringTagW), 'get [Symbol.toStringTag]', TAP_ToStringTag);
  PopRoots(1);
  Def(TypedArrayProtoV, 'at', TAP_At, 1);
  Def(TypedArrayProtoV, 'copyWithin', TAP_CopyWithin, 2);
  Def(TypedArrayProtoV, 'entries', TAP_Entries, 0);
  Def(TypedArrayProtoV, 'every', TAP_Every, 1);
  Def(TypedArrayProtoV, 'fill', TAP_Fill, 1);
  Def(TypedArrayProtoV, 'filter', TAP_Filter, 1);
  Def(TypedArrayProtoV, 'find', TAP_Find, 1);
  Def(TypedArrayProtoV, 'findIndex', TAP_FindIndex, 1);
  Def(TypedArrayProtoV, 'findLast', TAP_FindLast, 1);
  Def(TypedArrayProtoV, 'findLastIndex', TAP_FindLastIndex, 1);
  Def(TypedArrayProtoV, 'forEach', TAP_ForEach, 1);
  Def(TypedArrayProtoV, 'includes', TAP_Includes, 1);
  Def(TypedArrayProtoV, 'indexOf', TAP_IndexOf, 1);
  Def(TypedArrayProtoV, 'join', TAP_Join, 1);
  Def(TypedArrayProtoV, 'keys', TAP_Keys, 0);
  Def(TypedArrayProtoV, 'lastIndexOf', TAP_LastIndexOf, 1);
  Def(TypedArrayProtoV, 'map', TAP_Map, 1);
  Def(TypedArrayProtoV, 'reduce', TAP_Reduce, 1);
  Def(TypedArrayProtoV, 'reduceRight', TAP_ReduceRight, 1);
  Def(TypedArrayProtoV, 'reverse', TAP_Reverse, 0);
  Def(TypedArrayProtoV, 'set', TAP_Set, 1);
  Def(TypedArrayProtoV, 'slice', TAP_Slice, 2);
  Def(TypedArrayProtoV, 'some', TAP_Some, 1);
  Def(TypedArrayProtoV, 'sort', TAP_Sort, 1);
  Def(TypedArrayProtoV, 'subarray', TAP_Subarray, 2);
  Def(TypedArrayProtoV, 'toLocaleString', TAP_ToLocaleString, 0);
  Def(TypedArrayProtoV, 'toReversed', TAP_ToReversed, 0);
  Def(TypedArrayProtoV, 'toSorted', TAP_ToSorted, 1);
  Def(TypedArrayProtoV, 'with', TAP_With, 2);
  { 23.2.3.37 values is also @@iterator; 23.2.3.35 toString is Array.prototype.toString }
  PushRoot(NewNativeFunction(TAP_Values, 'values', 0));
  DefineOwn(TypedArrayProtoV, AtomOf('values'), RootAt(TempTop - 1), pfHidden);
  DefineOwn(TypedArrayProtoV, SymKey(SymIterator), RootAt(TempTop - 1), pfHidden);
  PopRoots(1);
  GetV(ArrayProto, AtomToString, Tmp);
  PushRoot(Tmp);
  DefineOwn(TypedArrayProtoV, AtomToString, RootAt(TempTop - 1), pfHidden);
  PopRoots(1);

  { the twelve constructors }
  for I := 0 to TAKindCount - 1 do
  begin
    TAProtosV[I] := MkPtr(NewPlainObject(TypedArrayProtoV, 4));
    TACtors[I] := MakeCtor(CtorFns[I], TANames[I], 3, TAProtosV[I], True);
    SetProto(ObjOf(TACtors[I]), TypedArrayCtorV);
    DefineOwn(TACtors[I], AtomOf('BYTES_PER_ELEMENT'), MkInt(TAElemSize[I]), 0);
    DefineOwn(TAProtosV[I], AtomOf('BYTES_PER_ELEMENT'), MkInt(TAElemSize[I]), 0);
  end;

  { DataView }
  DataViewProtoV := MkPtr(NewPlainObject(ObjectProto, 32));
  F := MakeCtor(DV_Ctor2, 'DataView', 1, DataViewProtoV, True);
  Getter(DataViewProtoV, 'buffer', DVP_Buffer);
  Getter(DataViewProtoV, 'byteLength', DVP_ByteLength);
  Getter(DataViewProtoV, 'byteOffset', DVP_ByteOffset);
  Def(DataViewProtoV, 'getInt8', DVG0, 1); Def(DataViewProtoV, 'setInt8', DVS0, 2);
  Def(DataViewProtoV, 'getUint8', DVG1, 1); Def(DataViewProtoV, 'setUint8', DVS1, 2);
  Def(DataViewProtoV, 'getInt16', DVG3, 1); Def(DataViewProtoV, 'setInt16', DVS3, 2);
  Def(DataViewProtoV, 'getUint16', DVG4, 1); Def(DataViewProtoV, 'setUint16', DVS4, 2);
  Def(DataViewProtoV, 'getInt32', DVG5, 1); Def(DataViewProtoV, 'setInt32', DVS5, 2);
  Def(DataViewProtoV, 'getUint32', DVG6, 1); Def(DataViewProtoV, 'setUint32', DVS6, 2);
  Def(DataViewProtoV, 'getFloat16', DVG9, 1); Def(DataViewProtoV, 'setFloat16', DVS9, 2);
  Def(DataViewProtoV, 'getFloat32', DVG7, 1); Def(DataViewProtoV, 'setFloat32', DVS7, 2);
  Def(DataViewProtoV, 'getFloat64', DVG8, 1); Def(DataViewProtoV, 'setFloat64', DVS8, 2);
  Def(DataViewProtoV, 'getBigInt64', DVG10, 1); Def(DataViewProtoV, 'setBigInt64', DVS10, 2);
  Def(DataViewProtoV, 'getBigUint64', DVG11, 1); Def(DataViewProtoV, 'setBigUint64', DVS11, 2);
  Tag(DataViewProtoV, 'DataView');

  { Math.f16round }
  if GetV(GlobalObj, AtomOf('Math'), AP) then Def(AP, 'f16round', M_F16Round, 1);
end;
