unit G2.TypedArrays;
{ 25.1 ArrayBuffer, 23.2 TypedArray objects (the twelve constructors, %TypedArray% and its prototype) and 25.3
  DataView, following the spec's steps. Storage and the element conversions are G2.Typed's; [[Get]] / [[Set]] /
  [[HasProperty]] / [[Delete]] / [[DefineOwnProperty]] of typed arrays are in G2.VM and G2.ObjModel.

  Rooting. A native's Args live on the value stack: Args[0] holds the receiver and is read again after any call;
  new objects are temp roots at fixed offsets from Base. }
{$mode delphiunicode}{$H+}
{$MINFPCONSTPREC 64}
{$POINTERMATH ON}
{$Q-}{$R-}

interface

uses
  G2.Core;

procedure InitTypedArrays;
{ $262.detachArrayBuffer: False with a TypeError pending for a non-ArrayBuffer or a SharedArrayBuffer }
function DetachArrayBufferV(B: TValue): Boolean;

implementation

uses
  SysUtils, Math, BigInteger, G2.VM, G2.BigIntV, G2.Typed, G2.ObjModel;

const
  MAX_SAFE = 9007199254740991.0;
  MAX_ALLOC = Int64(1) shl 30;   { larger buffers are a RangeError (CreateByteDataBlock cannot get the memory) }

type
  TArgs = PValue;

threadvar
  ArrayBufferCtor, ArrayBufferProtoV, TypedArrayCtorV, TypedArrayProtoV, DataViewProtoV: TValue;
  TACtors, TAProtosV: array[0..TAKindCount - 1] of TValue;
  TASortFn: TValue;

function Arg(Args: TArgs; Argc, I: Integer): TValue; inline;
begin
  if I <= Argc then Result := Args[I] else Result := VUndefined;
end;

{ 7.1.22 ToIndex }
function ToIndex(V: TValue; var R: Int64): Boolean;
var D: Double;
begin
  if V = VUndefined then begin R := 0; Exit(True); end;
  if IsInt(V) and (IntOf(V) >= 0) then begin R := IntOf(V); Exit(True); end;
  if not ToNumberV(V, D) then Exit(False);
  if IsNaN(D) then D := 0 else D := Int(D);
  if (D < 0) or (D > MAX_SAFE) then Exit(ThrowError('RangeError', 'Invalid index'));
  R := Trunc(D);
  Result := True;
end;

{ 7.1.5 ToIntegerOrInfinity }
function ToIntInf(V: TValue; var D: Double): Boolean;
begin
  if IsInt(V) then begin D := IntOf(V); Exit(True); end;
  if not ToNumberV(V, D) then Exit(False);
  if IsNaN(D) then D := 0
  else if not IsInfinite(D) then D := Int(D);
  if D = 0 then D := 0;
  Result := True;
end;

function RelIdx(V: TValue; Len, Default: Int64; var R: Int64): Boolean;
var D: Double;
begin
  if V = VUndefined then begin R := Default; Exit(True); end;
  if not ToIntInf(V, D) then Exit(False);
  if D < 0 then
  begin
    D := Len + D;
    if D < 0 then D := 0;
  end
  else if D > Len then D := Len;
  R := Trunc(D);
  Result := True;
end;

function IdxVal(K: Int64): TValue; inline;
begin
  if K <= High(Int32) then Result := MkInt(K) else Result := MkNumber(K);
end;

function IsBuffer(V: TValue): Boolean; inline;
begin
  Result := IsObject(V) and (ObjOf(V)^.OKind = okArrayBuffer);
end;

function IsTA(V: TValue): Boolean; inline;
begin
  Result := IsObject(V) and (ObjOf(V)^.OKind = okTypedArray);
end;

{ 10.1.14 GetPrototypeFromConstructor }
function ProtoFromCtor(C, Default: TValue; var P: TValue): Boolean;
begin
  Result := GetPrototypeFromConstructor(C, Default, P);
end;

{ 7.3.22 SpeciesConstructor }
function SpeciesCtor(O, Default: TValue; var C: TValue): Boolean;
var S: TValue;
begin
  if not GetV(O, AtomConstructor, C) then Exit(False);
  if C = VUndefined then begin C := Default; Exit(True); end;
  if not IsObject(C) then Exit(ThrowTypeError('object.constructor is not an object'));
  if not GetElemV(C, SymSpecies, S) then Exit(False);
  if IsNullish(S) then begin C := Default; Exit(True); end;
  if IsConstructorV(S) then begin C := S; Exit(True); end;
  Result := ThrowTypeError('object.constructor[Symbol.species] is not a constructor');
end;

{ 23.2.4.4 ValidateTypedArray: the receiver's length; a TypeError for a non-typed-array or an out-of-bounds or
  detached one }
function ValidateTA(V: TValue; var Len: Int64): Boolean;
begin
  if not IsTA(V) then Exit(ThrowTypeError('this is not a typed array'));
  if TAOutOfBounds(ObjOf(V)) then Exit(ThrowTypeError('the typed array is detached or out of bounds'));
  Len := TALength(ObjOf(V));
  Result := True;
end;

function TAGetV(O: TValue; K: Int64): TValue; inline;
begin
  if (K >= 0) and (K < TALength(ObjOf(O))) then Result := TAGet(ObjOf(O), K) else Result := VUndefined;
end;

{ ---------------- ArrayBuffer ---------------- }

{ 25.1.3.1 AllocateArrayBuffer (MaxLen < 0: fixed length) }
function AllocBuffer(C: TValue; Len, MaxLen: Int64; var B: TValue): Boolean;
var P: TValue;
begin
  if (MaxLen >= 0) and (Len > MaxLen) then Exit(ThrowError('RangeError', 'byteLength exceeds maxByteLength'));
  if not ProtoFromCtor(C, ArrayBufferProtoV, P) then Exit(False);
  if (Len > MAX_ALLOC) or (MaxLen > MAX_ALLOC) then Exit(ThrowError('RangeError', 'Array buffer allocation failed'));
  if MaxLen >= 0 then B := MkPtr(NewResizableBuffer(Len, MaxLen, P))
  else B := MkPtr(NewArrayBuffer(Len, P));
  Result := True;
end;

{ 25.1.4.1 ArrayBuffer ( length [ , options ] ) }
function AB_Ctor(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var Len, MaxLen: Int64; NT, M: TValue;
begin
  NT := CurNewTarget;
  if NT = VUndefined then Exit(ThrowTypeError('Constructor ArrayBuffer requires ''new'''));
  PushRoot(NT);
  Result := False;
  MaxLen := -1;
  if ToIndex(Arg(Args, Argc, 1), Len) then
  begin
    { 25.1.3.7 GetArrayBufferMaxByteLengthOption }
    M := VUndefined;
    if IsObject(Arg(Args, Argc, 2)) and not GetV(Args[2], AtomOf('maxByteLength'), M) then begin PopRoots(1); Exit; end;
    if (M = VUndefined) or ToIndex(M, MaxLen) then
      Result := AllocBuffer(RootAt(TempTop - 1), Len, MaxLen, Res);
  end;
  PopRoots(1);
  ConstructProtoTaken := True;
end;

function ThisBuffer(Args: TArgs; const Name: UnicodeString; var B: PObj): Boolean;
begin
  if not IsBuffer(Args[0]) or ((ObjOf(Args[0])^.OFlags and bfShared) <> 0) then
    Exit(ThrowTypeError('ArrayBuffer.prototype.' + Name + ' called on an incompatible receiver'));
  B := ObjOf(Args[0]);
  Result := True;
end;

function AB_ByteLength(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var B: PObj;
begin
  if not ThisBuffer(Args, 'byteLength', B) then Exit(False);
  Res := MkNumber(BufferLen(B));
  Result := True;
end;

function AB_MaxByteLength(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var B: PObj;
begin
  if not ThisBuffer(Args, 'maxByteLength', B) then Exit(False);
  Res := MkNumber(BufferMaxLen(B));
  Result := True;
end;

function AB_Resizable(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var B: PObj;
begin
  if not ThisBuffer(Args, 'resizable', B) then Exit(False);
  Res := MkBool((B^.OFlags and bfResizable) <> 0);
  Result := True;
end;

function AB_Detached(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var B: PObj;
begin
  if not ThisBuffer(Args, 'detached', B) then Exit(False);
  Res := MkBool(BufferDetached(B));
  Result := True;
end;

{ 25.1.6.7 ArrayBuffer.prototype.slice }
function AB_Slice(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var B, NB: PObj; Len, First, Fin, NewLen, Cnt, CurLen: Int64; C, LV, N: TValue;
begin
  if not ThisBuffer(Args, 'slice', B) then Exit(False);
  if BufferDetached(B) then Exit(ThrowTypeError('ArrayBuffer.prototype.slice called on a detached buffer'));
  Len := BufferLen(B);
  if not RelIdx(Arg(Args, Argc, 1), Len, 0, First) then Exit(False);
  if not RelIdx(Arg(Args, Argc, 2), Len, Len, Fin) then Exit(False);
  NewLen := Max(Fin - First, 0);
  if not SpeciesCtor(Args[0], ArrayBufferCtor, C) then Exit(False);
  LV := IdxVal(NewLen);
  if not Construct(C, @LV, 1, C, N) then Exit(False);
  if not IsBuffer(N) or ((ObjOf(N)^.OFlags and bfShared) <> 0) then
    Exit(ThrowTypeError('ArrayBuffer species constructor did not return an ArrayBuffer'));
  NB := ObjOf(N);
  if BufferDetached(NB) then Exit(ThrowTypeError('ArrayBuffer species constructor returned a detached buffer'));
  if N = Args[0] then Exit(ThrowTypeError('ArrayBuffer species constructor returned the same buffer'));
  if BufferLen(NB) < NewLen then Exit(ThrowTypeError('ArrayBuffer species constructor returned a too small buffer'));
  B := ObjOf(Args[0]);
  if BufferDetached(B) then Exit(ThrowTypeError('the ArrayBuffer was detached'));
  CurLen := BufferLen(B);
  if First < CurLen then
  begin
    Cnt := Min(NewLen, CurLen - First);
    if Cnt > 0 then Move((BufferData(B) + First)^, BufferData(NB)^, Cnt);
  end;
  Res := N;
  Result := True;
end;

{ 25.1.6.6 ArrayBuffer.prototype.resize }
function AB_Resize(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var B: PObj; NewLen, Old: Int64;
begin
  if not ThisBuffer(Args, 'resize', B) or ((B^.OFlags and bfResizable) = 0) then
    Exit(ThrowTypeError('ArrayBuffer.prototype.resize called on a fixed-length buffer'));
  if not ToIndex(Arg(Args, Argc, 1), NewLen) then Exit(False);
  B := ObjOf(Args[0]);
  if BufferDetached(B) then Exit(ThrowTypeError('ArrayBuffer.prototype.resize called on a detached buffer'));
  if NewLen > BufferMaxLen(B) then Exit(ThrowError('RangeError', 'new length exceeds maxByteLength'));
  Old := BufferLen(B);
  { the bytes past the new length read as zero when the buffer grows again }
  if NewLen < Old then FillChar((BufferData(B) + NewLen)^, Old - NewLen, 0);
  B^.Aux2 := MkNumber(NewLen);
  Res := VUndefined;
  Result := True;
end;

{ 25.1.3.3 ArrayBufferCopyAndDetach: transfer / transferToFixedLength }
function CopyAndDetach(Args: TArgs; Argc: Integer; var Res: TValue; Preserve: Boolean; const Name: UnicodeString): Boolean;
var B: PObj; NewLen, MaxLen: Int64; N: TValue;
begin
  if not ThisBuffer(Args, Name, B) then Exit(False);
  if Arg(Args, Argc, 1) = VUndefined then NewLen := BufferLen(B)
  else if not ToIndex(Args[1], NewLen) then Exit(False);
  B := ObjOf(Args[0]);
  if BufferDetached(B) then Exit(ThrowTypeError('ArrayBuffer.prototype.' + Name + ' called on a detached buffer'));
  if Preserve and ((B^.OFlags and bfResizable) <> 0) then MaxLen := BufferMaxLen(B) else MaxLen := -1;
  if not AllocBuffer(ArrayBufferCtor, NewLen, MaxLen, N) then Exit(False);
  B := ObjOf(Args[0]);
  if Min(NewLen, BufferLen(B)) > 0 then Move(BufferData(B)^, BufferData(ObjOf(N))^, Min(NewLen, BufferLen(B)));
  DetachBuffer(B);
  Res := N;
  Result := True;
end;

function AB_Transfer(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
begin Result := CopyAndDetach(Args, Argc, Res, True, 'transfer'); end;
function AB_TransferToFixed(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
begin Result := CopyAndDetach(Args, Argc, Res, False, 'transferToFixedLength'); end;

{ 25.1.5.1 ArrayBuffer.isView }
function AB_IsView(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
begin
  Res := MkBool(IsObject(Arg(Args, Argc, 1)) and (ObjOf(Args[1])^.OKind in [okTypedArray, okDataView]));
  Result := True;
end;

function Species_Getter(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
begin
  Res := Args[0];
  Result := True;
end;

function DetachArrayBufferV(B: TValue): Boolean;
begin
  if not IsBuffer(B) or ((ObjOf(B)^.OFlags and bfShared) <> 0) then
    Exit(ThrowTypeError('detachArrayBuffer: not an ArrayBuffer'));
  DetachBuffer(ObjOf(B));
  Result := True;
end;

{ ---------------- typed array creation ---------------- }

function NewTAOf(Kind: Integer; Len: Int64; Proto: TValue; var T: TValue): Boolean;
var B: TValue;
begin
  if Len * TAElemSize[Kind] > MAX_ALLOC then Exit(ThrowError('RangeError', 'Invalid typed array length: ' + IntToStr(Len)));
  PushRoot(Proto);
  B := MkPtr(NewArrayBuffer(Len * TAElemSize[Kind], ArrayBufferProtoV));
  T := MkPtr(NewTypedArray(Kind, B, 0, Len, RootAt(TempTop - 1)));
  PopRoots(1);
  Result := True;
end;

{ 23.2.5.1.2 InitializeTypedArrayFromTypedArray }
function FromTypedArray(Kind: Integer; Proto, Src: TValue; var T: TValue): Boolean;
var S: PObj; Len, I: Int64; SK: Integer;
begin
  S := ObjOf(Src);
  if TAOutOfBounds(S) then Exit(ThrowTypeError('the source typed array is detached or out of bounds'));
  if TAIsBig(S) <> (Kind >= taBigInt64) then Exit(ThrowTypeError('Content type mismatch: BigInt and Number typed arrays'));
  Len := TALength(S);
  PushRoot(Src);
  if not NewTAOf(Kind, Len, Proto, T) then begin PopRoots(1); Exit(False); end;
  S := ObjOf(RootAt(TempTop - 1));
  PopRoots(1);
  SK := TAKind(S);
  if SK = Kind then
  begin
    if Len > 0 then Move(TAData(S)^, TAData(ObjOf(T))^, Len * TAElemSize[Kind]);
  end
  else if Kind >= taBigInt64 then
    for I := 0 to Len - 1 do PQWord(TAData(ObjOf(T)))[I] := PQWord(TAData(S))[I]
  else
    for I := 0 to Len - 1 do TASet(ObjOf(T), I, NumOf(TAGet(S, I)));
  Result := True;
end;

{ 23.2.5.1.3 InitializeTypedArrayFromArrayBuffer }
function FromBuffer(Kind: Integer; Proto, Buf, OffV, LenV: TValue; var T: TValue): Boolean;
var Off, NewLen, BL, NewBL: Int64; ES: Integer; B: PObj; Fixed: Boolean;
begin
  ES := TAElemSize[Kind];
  PushRoot(Proto); PushRoot(Buf); PushRoot(LenV);
  Result := False;
  try
    if not ToIndex(OffV, Off) then Exit;
    if Off mod ES <> 0 then Exit(ThrowError('RangeError', 'start offset of ' + TANames[Kind] + ' should be a multiple of ' + IntToStr(ES)));
    NewLen := 0;
    if (RootAt(TempTop - 1) <> VUndefined) and not ToIndex(RootAt(TempTop - 1), NewLen) then Exit;
    B := ObjOf(RootAt(TempTop - 2));
    if BufferDetached(B) then Exit(ThrowTypeError('the ArrayBuffer is detached'));
    BL := BufferLen(B);
    Fixed := (B^.OFlags and bfResizable) = 0;
    if (RootAt(TempTop - 1) = VUndefined) and not Fixed then
    begin
      if Off > BL then Exit(ThrowError('RangeError', 'start offset ' + IntToStr(Off) + ' is outside the bounds of the buffer'));
      T := MkPtr(NewTypedArray(Kind, RootAt(TempTop - 2), Off, -1, RootAt(TempTop - 3)));
      Exit(True);
    end;
    if RootAt(TempTop - 1) = VUndefined then
    begin
      if BL mod ES <> 0 then Exit(ThrowError('RangeError', 'byte length of ' + TANames[Kind] + ' should be a multiple of ' + IntToStr(ES)));
      NewBL := BL - Off;
      if NewBL < 0 then Exit(ThrowError('RangeError', 'start offset ' + IntToStr(Off) + ' is outside the bounds of the buffer'));
    end
    else
    begin
      NewBL := NewLen * ES;
      if Off + NewBL > BL then Exit(ThrowError('RangeError', 'Invalid typed array length: ' + IntToStr(NewLen)));
    end;
    if NewBL div ES > High(Int32) then Exit(ThrowError('RangeError', 'Invalid typed array length'));
    T := MkPtr(NewTypedArray(Kind, RootAt(TempTop - 2), Off, NewBL div ES, RootAt(TempTop - 3)));
    Result := True;
  finally
    PopRoots(3);
  end;
end;

{ 23.2.5.1.4 / .5: from a list (a rooted array of the iterated values) or an array-like; Set(O, k, v, true) each }
function FromValues(Kind: Integer; Proto, Src: TValue; FromList: Boolean; var T: TValue): Boolean;
var Len, K: Int64; V: TValue; Base: Integer;
begin
  Base := TempTop;
  PushRoot(Src); PushRoot(Proto);
  Result := False;
  if FromList then Len := ObjOf(Src)^.Elements^.Len
  else if not LengthOfArrayLike(Src, Len) then begin PopRoots(2); Exit; end;
  if not NewTAOf(Kind, Len, RootAt(Base + 1), T) then begin PopRoots(2); Exit; end;
  PushRoot(T);
  for K := 0 to Len - 1 do
  begin
    if FromList then
    begin
      V := ObjOf(RootAt(Base))^.Elements^.Data[K];
      if V = VHole then V := VUndefined;
    end
    else if not GetElemV(RootAt(Base), IdxVal(K), V) then begin PopRoots(3); Exit; end;
    if not TAStoreValue(RootAt(Base + 2), K, V) then begin PopRoots(3); Exit; end;
  end;
  T := RootAt(Base + 2);
  PopRoots(3);
  Result := True;
end;

{ 7.4.x IteratorToList(GetIteratorFromMethod(obj, method)) into a new array }
function IterToList(Obj, Method: TValue; var L: TValue): Boolean;
var It, NextM, V: TValue; Done: Boolean; Base: Integer;
begin
  { the list's allocation can move a young Obj or Method }
  PushRoot(Obj); PushRoot(Method);
  L := MkPtr(NewArray(0, 8));
  Method := RootAt(TempTop - 1); Obj := RootAt(TempTop - 2);
  PopRoots(2);
  Base := TempTop;
  PushRoot(L);
  if not CallValue(Method, Obj, nil, 0, It) then begin PopRoots(1); Exit(False); end;
  if not IsObject(It) then begin PopRoots(1); Exit(ThrowTypeError('iterator is not an object')); end;
  PushRoot(It);
  if not GetV(It, AtomNext, NextM) then begin PopRoots(2); Exit(False); end;
  PushRoot(NextM);
  while True do
  begin
    if not IteratorStepRec(RootAt(Base + 1), RootAt(Base + 2), V, Done) then begin PopRoots(3); Exit(False); end;
    if Done then Break;
    ArrayPush(ObjOf(RootAt(Base)), V);
  end;
  L := RootAt(Base);
  PopRoots(3);
  Result := True;
end;

{ 23.2.5.1 TypedArray ( ...args ) }
function TACtor(Kind: Integer; Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var NT, Proto, A0, M, L: TValue; N: Int64;
begin
  NT := CurNewTarget;
  if NT = VUndefined then Exit(ThrowTypeError('Constructor ' + TANames[Kind] + ' requires ''new'''));
  ConstructProtoTaken := True;
  A0 := Arg(Args, Argc, 1);
  if not IsObject(A0) then
  begin
    if Argc = 0 then N := 0
    else
    begin
      PushRoot(NT);
      if not ToIndex(A0, N) then begin PopRoots(1); Exit(False); end;
      NT := RootAt(TempTop - 1);
      PopRoots(1);
    end;
    if not ProtoFromCtor(NT, TAProtosV[Kind], Proto) then Exit(False);
    Exit(NewTAOf(Kind, N, Proto, Res));
  end;
  { AllocateTypedArray first reads the prototype from new.target }
  if not ProtoFromCtor(NT, TAProtosV[Kind], Proto) then Exit(False);
  PushRoot(Proto);
  Result := False;
  A0 := Args[1];
  if ObjOf(A0)^.OKind = okTypedArray then Result := FromTypedArray(Kind, RootAt(TempTop - 1), A0, Res)
  else if ObjOf(A0)^.OKind = okArrayBuffer then
    Result := FromBuffer(Kind, RootAt(TempTop - 1), A0, Arg(Args, Argc, 2), Arg(Args, Argc, 3), Res)
  else
  begin
    if not GetElemV(A0, SymIterator, M) then begin PopRoots(1); Exit; end;
    if ((M = VUndefined) or (M = VNull)) then Result := FromValues(Kind, RootAt(TempTop - 1), Args[1], False, Res)
    else if not IsCallable(M) then Result := ThrowTypeError('@@iterator is not a function')
    else if IterToList(Args[1], M, L) then Result := FromValues(Kind, RootAt(TempTop - 1), L, True, Res);
  end;
  PopRoots(1);
end;

function TA_C0(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := TACtor(0, Args, Argc, Res); end;
function TA_C1(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := TACtor(1, Args, Argc, Res); end;
function TA_C2(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := TACtor(2, Args, Argc, Res); end;
function TA_C3(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := TACtor(3, Args, Argc, Res); end;
function TA_C4(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := TACtor(4, Args, Argc, Res); end;
function TA_C5(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := TACtor(5, Args, Argc, Res); end;
function TA_C6(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := TACtor(6, Args, Argc, Res); end;
function TA_C7(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := TACtor(7, Args, Argc, Res); end;
function TA_C8(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := TACtor(8, Args, Argc, Res); end;
function TA_C9(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := TACtor(9, Args, Argc, Res); end;
function TA_C10(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := TACtor(10, Args, Argc, Res); end;
function TA_C11(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := TACtor(11, Args, Argc, Res); end;

{ 23.2.1.1 %TypedArray% }
function TA_Abstract(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
begin
  Result := ThrowTypeError('Abstract class TypedArray not directly constructable');
end;

{ 23.2.4.2 TypedArrayCreateFromConstructor (ArgLen >= 0: the argument list is that single number) }
function TACreateFromCtor(C: TValue; ArgsP: PValue; NArgs: Integer; ArgLen: Int64; var T: TValue): Boolean;
var Len: Int64;
begin
  if not Construct(C, ArgsP, NArgs, C, T) then Exit(False);
  if not ValidateTA(T, Len) then Exit(False);
  if (ArgLen >= 0) and (Len < ArgLen) then Exit(ThrowTypeError('the species constructor returned a too short typed array'));
  Result := True;
end;

{ 23.2.4.1 TypedArraySpeciesCreate }
function TASpeciesCreate(Exemplar: TValue; ArgsP: PValue; NArgs: Integer; ArgLen: Int64; var T: TValue): Boolean;
var C: TValue; Big: Boolean;
begin
  Big := TAIsBig(ObjOf(Exemplar));
  if not SpeciesCtor(Exemplar, TACtors[TAKind(ObjOf(Exemplar))], C) then Exit(False);
  if not TACreateFromCtor(C, ArgsP, NArgs, ArgLen, T) then Exit(False);
  if TAIsBig(ObjOf(T)) <> Big then Exit(ThrowTypeError('Content type mismatch: BigInt and Number typed arrays'));
  Result := True;
end;

{ 23.2.4.3 TypedArrayCreateSameType }
function TACreateSameType(Exemplar: TValue; Len: Int64; var T: TValue): Boolean;
begin
  Result := NewTAOf(TAKind(ObjOf(Exemplar)), Len, TAProtosV[TAKind(ObjOf(Exemplar))], T);
end;

{$I G2.TypedArrays2.inc}

end.
