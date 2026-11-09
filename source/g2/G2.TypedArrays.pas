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
  Math,
  SysUtils,

  BigInteger,
  G2.BigIntV,
  G2.ObjModel,
  G2.Typed,
  G2.VM;

const
  MAX_SAFE = 9007199254740991.0;
  MAX_ALLOC = Int64(1) shl 30;   { larger buffers are a RangeError (CreateByteDataBlock cannot get the memory) }

type
  TArgs = PValue;

threadvar
  ArrayBufferCtor, ArrayBufferProtoV, TypedArrayCtorV, TypedArrayProtoV, DataViewProtoV: TValue;
  TACtors, TAProtosV: array[0..TAKindCount - 1] of TValue;
  TASortFn: TValue;

function Arg(AArgs: TArgs; AArgc, I: Integer): TValue; inline;
begin
  if I <= AArgc then Result := AArgs[I] else Result := VUndefined;
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

function RelIdx(V: TValue; ALen, ADefault: Int64; var R: Int64): Boolean;
var D: Double;
begin
  if V = VUndefined then begin R := ADefault; Exit(True); end;
  if not ToIntInf(V, D) then Exit(False);
  if D < 0 then
  begin
    D := ALen + D;
    if D < 0 then D := 0;
  end
  else if D > ALen then D := ALen;
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
function ProtoFromCtor(C, ADefault: TValue; var P: TValue): Boolean;
begin
  if not GetV(C, AtomPrototype, P) then Exit(False);
  if not IsObject(P) then P := ADefault;
  Result := True;
end;

{ 7.3.22 SpeciesConstructor }
function SpeciesCtor(O, ADefault: TValue; var C: TValue): Boolean;
var S: TValue;
begin
  if not GetV(O, AtomConstructor, C) then Exit(False);
  if C = VUndefined then begin C := ADefault; Exit(True); end;
  if not IsObject(C) then Exit(ThrowTypeError('object.constructor is not an object'));
  if not GetElemV(C, SymSpecies, S) then Exit(False);
  if IsNullish(S) then begin C := ADefault; Exit(True); end;
  if IsConstructorV(S) then begin C := S; Exit(True); end;
  Result := ThrowTypeError('object.constructor[Symbol.species] is not a constructor');
end;

{ 23.2.4.4 ValidateTypedArray: the receiver's length; a TypeError for a non-typed-array or an out-of-bounds or
  detached one }
function ValidateTA(V: TValue; var ALen: Int64): Boolean;
begin
  if not IsTA(V) then Exit(ThrowTypeError('this is not a typed array'));
  if TAOutOfBounds(ObjOf(V)) then Exit(ThrowTypeError('the typed array is detached or out of bounds'));
  ALen := TALength(ObjOf(V));
  Result := True;
end;

function TAGetV(O: TValue; K: Int64): TValue; inline;
begin
  if (K >= 0) and (K < TALength(ObjOf(O))) then Result := TAGet(ObjOf(O), K) else Result := VUndefined;
end;

{ ---------------- ArrayBuffer ---------------- }

{ 25.1.3.1 AllocateArrayBuffer (MaxLen < 0: fixed length) }
function AllocBuffer(C: TValue; ALen, AMaxLen: Int64; var B: TValue): Boolean;
var P: TValue;
begin
  if (AMaxLen >= 0) and (ALen > AMaxLen) then Exit(ThrowError('RangeError', 'byteLength exceeds maxByteLength'));
  if not ProtoFromCtor(C, ArrayBufferProtoV, P) then Exit(False);
  if (ALen > MAX_ALLOC) or (AMaxLen > MAX_ALLOC) then Exit(ThrowError('RangeError', 'Array buffer allocation failed'));
  if AMaxLen >= 0 then B := MkPtr(NewResizableBuffer(ALen, AMaxLen, P))
  else B := MkPtr(NewArrayBuffer(ALen, P));
  Result := True;
end;

{ 25.1.4.1 ArrayBuffer ( length [ , options ] ) }
function AB_Ctor(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var Len, MaxLen: Int64; NT, M: TValue;
begin
  NT := CurNewTarget;
  if NT = VUndefined then Exit(ThrowTypeError('Constructor ArrayBuffer requires ''new'''));
  PushRoot(NT);
  Result := False;
  MaxLen := -1;
  if ToIndex(Arg(AArgs, AArgc, 1), Len) then
  begin
    { 25.1.3.7 GetArrayBufferMaxByteLengthOption }
    M := VUndefined;
    if IsObject(Arg(AArgs, AArgc, 2)) and not GetV(AArgs[2], AtomOf('maxByteLength'), M) then begin PopRoots(1); Exit; end;
    if (M = VUndefined) or ToIndex(M, MaxLen) then
      Result := AllocBuffer(RootAt(TempTop - 1), Len, MaxLen, ARes);
  end;
  PopRoots(1);
  ConstructProtoTaken := True;
end;

function ThisBuffer(AArgs: TArgs; const AName: UnicodeString; var B: PObj): Boolean;
begin
  if not IsBuffer(AArgs[0]) or ((ObjOf(AArgs[0])^.OFlags and bfShared) <> 0) then
    Exit(ThrowTypeError('ArrayBuffer.prototype.' + AName + ' called on an incompatible receiver'));
  B := ObjOf(AArgs[0]);
  Result := True;
end;

function AB_ByteLength(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var B: PObj;
begin
  if not ThisBuffer(AArgs, 'byteLength', B) then Exit(False);
  ARes := MkNumber(BufferLen(B));
  Result := True;
end;

function AB_MaxByteLength(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var B: PObj;
begin
  if not ThisBuffer(AArgs, 'maxByteLength', B) then Exit(False);
  ARes := MkNumber(BufferMaxLen(B));
  Result := True;
end;

function AB_Resizable(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var B: PObj;
begin
  if not ThisBuffer(AArgs, 'resizable', B) then Exit(False);
  ARes := MkBool((B^.OFlags and bfResizable) <> 0);
  Result := True;
end;

function AB_Detached(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var B: PObj;
begin
  if not ThisBuffer(AArgs, 'detached', B) then Exit(False);
  ARes := MkBool(BufferDetached(B));
  Result := True;
end;

{ 25.1.6.7 ArrayBuffer.prototype.slice }
function AB_Slice(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var B, NB: PObj; Len, First, Fin, NewLen, Cnt, CurLen: Int64; C, LV, N: TValue;
begin
  if not ThisBuffer(AArgs, 'slice', B) then Exit(False);
  if BufferDetached(B) then Exit(ThrowTypeError('ArrayBuffer.prototype.slice called on a detached buffer'));
  Len := BufferLen(B);
  if not RelIdx(Arg(AArgs, AArgc, 1), Len, 0, First) then Exit(False);
  if not RelIdx(Arg(AArgs, AArgc, 2), Len, Len, Fin) then Exit(False);
  NewLen := Max(Fin - First, 0);
  if not SpeciesCtor(AArgs[0], ArrayBufferCtor, C) then Exit(False);
  LV := IdxVal(NewLen);
  if not Construct(C, @LV, 1, C, N) then Exit(False);
  if not IsBuffer(N) or ((ObjOf(N)^.OFlags and bfShared) <> 0) then
    Exit(ThrowTypeError('ArrayBuffer species constructor did not return an ArrayBuffer'));
  NB := ObjOf(N);
  if BufferDetached(NB) then Exit(ThrowTypeError('ArrayBuffer species constructor returned a detached buffer'));
  if N = AArgs[0] then Exit(ThrowTypeError('ArrayBuffer species constructor returned the same buffer'));
  if BufferLen(NB) < NewLen then Exit(ThrowTypeError('ArrayBuffer species constructor returned a too small buffer'));
  B := ObjOf(AArgs[0]);
  if BufferDetached(B) then Exit(ThrowTypeError('the ArrayBuffer was detached'));
  CurLen := BufferLen(B);
  if First < CurLen then
  begin
    Cnt := Min(NewLen, CurLen - First);
    if Cnt > 0 then Move((BufferData(B) + First)^, BufferData(NB)^, Cnt);
  end;
  ARes := N;
  Result := True;
end;

{ 25.1.6.6 ArrayBuffer.prototype.resize }
function AB_Resize(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var B: PObj; NewLen, Old: Int64;
begin
  if not ThisBuffer(AArgs, 'resize', B) or ((B^.OFlags and bfResizable) = 0) then
    Exit(ThrowTypeError('ArrayBuffer.prototype.resize called on a fixed-length buffer'));
  if not ToIndex(Arg(AArgs, AArgc, 1), NewLen) then Exit(False);
  B := ObjOf(AArgs[0]);
  if BufferDetached(B) then Exit(ThrowTypeError('ArrayBuffer.prototype.resize called on a detached buffer'));
  if NewLen > BufferMaxLen(B) then Exit(ThrowError('RangeError', 'new length exceeds maxByteLength'));
  Old := BufferLen(B);
  { the bytes past the new length read as zero when the buffer grows again }
  if NewLen < Old then FillChar((BufferData(B) + NewLen)^, Old - NewLen, 0);
  B^.Aux2 := MkNumber(NewLen);
  ARes := VUndefined;
  Result := True;
end;

{ 25.1.3.3 ArrayBufferCopyAndDetach: transfer / transferToFixedLength }
function CopyAndDetach(AArgs: TArgs; AArgc: Integer; var ARes: TValue; APreserve: Boolean; const AName: UnicodeString): Boolean;
var B: PObj; NewLen, MaxLen: Int64; N: TValue;
begin
  if not ThisBuffer(AArgs, AName, B) then Exit(False);
  if Arg(AArgs, AArgc, 1) = VUndefined then NewLen := BufferLen(B)
  else if not ToIndex(AArgs[1], NewLen) then Exit(False);
  B := ObjOf(AArgs[0]);
  if BufferDetached(B) then Exit(ThrowTypeError('ArrayBuffer.prototype.' + AName + ' called on a detached buffer'));
  if APreserve and ((B^.OFlags and bfResizable) <> 0) then MaxLen := BufferMaxLen(B) else MaxLen := -1;
  if not AllocBuffer(ArrayBufferCtor, NewLen, MaxLen, N) then Exit(False);
  B := ObjOf(AArgs[0]);
  if Min(NewLen, BufferLen(B)) > 0 then Move(BufferData(B)^, BufferData(ObjOf(N))^, Min(NewLen, BufferLen(B)));
  DetachBuffer(B);
  ARes := N;
  Result := True;
end;

function AB_Transfer(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
begin Result := CopyAndDetach(AArgs, AArgc, ARes, True, 'transfer'); end;
function AB_TransferToFixed(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
begin Result := CopyAndDetach(AArgs, AArgc, ARes, False, 'transferToFixedLength'); end;

{ 25.1.5.1 ArrayBuffer.isView }
function AB_IsView(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
begin
  ARes := MkBool(IsObject(Arg(AArgs, AArgc, 1)) and (ObjOf(AArgs[1])^.OKind in [okTypedArray, okDataView]));
  Result := True;
end;

function Species_Getter(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
begin
  ARes := AArgs[0];
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

function NewTAOf(AKind: Integer; ALen: Int64; AProto: TValue; var T: TValue): Boolean;
var B: TValue;
begin
  if ALen * TAElemSize[AKind] > MAX_ALLOC then Exit(ThrowError('RangeError', 'Invalid typed array length: ' + IntToStr(ALen)));
  PushRoot(AProto);
  B := MkPtr(NewArrayBuffer(ALen * TAElemSize[AKind], ArrayBufferProtoV));
  T := MkPtr(NewTypedArray(AKind, B, 0, ALen, RootAt(TempTop - 1)));
  PopRoots(1);
  Result := True;
end;

{ 23.2.5.1.2 InitializeTypedArrayFromTypedArray }
function FromTypedArray(AKind: Integer; AProto, ASrc: TValue; var T: TValue): Boolean;
var S: PObj; Len, I: Int64; SK: Integer;
begin
  S := ObjOf(ASrc);
  if TAOutOfBounds(S) then Exit(ThrowTypeError('the source typed array is detached or out of bounds'));
  if TAIsBig(S) <> (AKind >= taBigInt64) then Exit(ThrowTypeError('Content type mismatch: BigInt and Number typed arrays'));
  Len := TALength(S);
  PushRoot(ASrc);
  if not NewTAOf(AKind, Len, AProto, T) then begin PopRoots(1); Exit(False); end;
  S := ObjOf(RootAt(TempTop - 1));
  PopRoots(1);
  SK := TAKind(S);
  if SK = AKind then
  begin
    if Len > 0 then Move(TAData(S)^, TAData(ObjOf(T))^, Len * TAElemSize[AKind]);
  end
  else if AKind >= taBigInt64 then
    for I := 0 to Len - 1 do PQWord(TAData(ObjOf(T)))[I] := PQWord(TAData(S))[I]
  else
    for I := 0 to Len - 1 do TASet(ObjOf(T), I, NumOf(TAGet(S, I)));
  Result := True;
end;

{ 23.2.5.1.3 InitializeTypedArrayFromArrayBuffer }
function FromBuffer(AKind: Integer; AProto, ABuf, AOffV, ALenV: TValue; var T: TValue): Boolean;
var Off, NewLen, BL, NewBL: Int64; ES: Integer; B: PObj; Fixed: Boolean;
begin
  ES := TAElemSize[AKind];
  PushRoot(AProto); PushRoot(ABuf); PushRoot(ALenV);
  Result := False;
  try
    if not ToIndex(AOffV, Off) then Exit;
    if Off mod ES <> 0 then Exit(ThrowError('RangeError', 'start offset of ' + TANames[AKind] + ' should be a multiple of ' + IntToStr(ES)));
    NewLen := 0;
    if (RootAt(TempTop - 1) <> VUndefined) and not ToIndex(RootAt(TempTop - 1), NewLen) then Exit;
    B := ObjOf(RootAt(TempTop - 2));
    if BufferDetached(B) then Exit(ThrowTypeError('the ArrayBuffer is detached'));
    BL := BufferLen(B);
    Fixed := (B^.OFlags and bfResizable) = 0;
    if (RootAt(TempTop - 1) = VUndefined) and not Fixed then
    begin
      if Off > BL then Exit(ThrowError('RangeError', 'start offset ' + IntToStr(Off) + ' is outside the bounds of the buffer'));
      T := MkPtr(NewTypedArray(AKind, RootAt(TempTop - 2), Off, -1, RootAt(TempTop - 3)));
      Exit(True);
    end;
    if RootAt(TempTop - 1) = VUndefined then
    begin
      if BL mod ES <> 0 then Exit(ThrowError('RangeError', 'byte length of ' + TANames[AKind] + ' should be a multiple of ' + IntToStr(ES)));
      NewBL := BL - Off;
      if NewBL < 0 then Exit(ThrowError('RangeError', 'start offset ' + IntToStr(Off) + ' is outside the bounds of the buffer'));
    end
    else
    begin
      NewBL := NewLen * ES;
      if Off + NewBL > BL then Exit(ThrowError('RangeError', 'Invalid typed array length: ' + IntToStr(NewLen)));
    end;
    if NewBL div ES > High(Int32) then Exit(ThrowError('RangeError', 'Invalid typed array length'));
    T := MkPtr(NewTypedArray(AKind, RootAt(TempTop - 2), Off, NewBL div ES, RootAt(TempTop - 3)));
    Result := True;
  finally
    PopRoots(3);
  end;
end;

{ 23.2.5.1.4 / .5: from a list (a rooted array of the iterated values) or an array-like; Set(O, k, v, true) each }
function FromValues(AKind: Integer; AProto, ASrc: TValue; AFromList: Boolean; var T: TValue): Boolean;
var Len, K: Int64; V: TValue; Base: Integer;
begin
  Base := TempTop;
  PushRoot(ASrc); PushRoot(AProto);
  Result := False;
  if AFromList then Len := ObjOf(ASrc)^.Elements^.Len
  else if not LengthOfArrayLike(ASrc, Len) then begin PopRoots(2); Exit; end;
  if not NewTAOf(AKind, Len, RootAt(Base + 1), T) then begin PopRoots(2); Exit; end;
  PushRoot(T);
  for K := 0 to Len - 1 do
  begin
    if AFromList then
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
function IterToList(AObj, AMethod: TValue; var L: TValue): Boolean;
var It, NextM, V: TValue; Done: Boolean; Base: Integer;
begin
  Base := TempTop;
  PushRoot(MkPtr(NewArray(0, 8)));
  if not CallValue(AMethod, AObj, nil, 0, It) then begin PopRoots(1); Exit(False); end;
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
function TACtor(AKind: Integer; AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
var NT, Proto, A0, M, L: TValue; N: Int64;
begin
  NT := CurNewTarget;
  if NT = VUndefined then Exit(ThrowTypeError('Constructor ' + TANames[AKind] + ' requires ''new'''));
  ConstructProtoTaken := True;
  A0 := Arg(AArgs, AArgc, 1);
  if not IsObject(A0) then
  begin
    if AArgc = 0 then N := 0
    else
    begin
      PushRoot(NT);
      if not ToIndex(A0, N) then begin PopRoots(1); Exit(False); end;
      NT := RootAt(TempTop - 1);
      PopRoots(1);
    end;
    if not ProtoFromCtor(NT, TAProtosV[AKind], Proto) then Exit(False);
    Exit(NewTAOf(AKind, N, Proto, ARes));
  end;
  { AllocateTypedArray first reads the prototype from new.target }
  if not ProtoFromCtor(NT, TAProtosV[AKind], Proto) then Exit(False);
  PushRoot(Proto);
  Result := False;
  A0 := AArgs[1];
  if ObjOf(A0)^.OKind = okTypedArray then Result := FromTypedArray(AKind, RootAt(TempTop - 1), A0, ARes)
  else if ObjOf(A0)^.OKind = okArrayBuffer then
    Result := FromBuffer(AKind, RootAt(TempTop - 1), A0, Arg(AArgs, AArgc, 2), Arg(AArgs, AArgc, 3), ARes)
  else
  begin
    if not GetElemV(A0, SymIterator, M) then begin PopRoots(1); Exit; end;
    if ((M = VUndefined) or (M = VNull)) then Result := FromValues(AKind, RootAt(TempTop - 1), AArgs[1], False, ARes)
    else if not IsCallable(M) then Result := ThrowTypeError('@@iterator is not a function')
    else if IterToList(AArgs[1], M, L) then Result := FromValues(AKind, RootAt(TempTop - 1), L, True, ARes);
  end;
  PopRoots(1);
end;

function TA_C0(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean; begin Result := TACtor(0, AArgs, AArgc, ARes); end;
function TA_C1(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean; begin Result := TACtor(1, AArgs, AArgc, ARes); end;
function TA_C2(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean; begin Result := TACtor(2, AArgs, AArgc, ARes); end;
function TA_C3(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean; begin Result := TACtor(3, AArgs, AArgc, ARes); end;
function TA_C4(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean; begin Result := TACtor(4, AArgs, AArgc, ARes); end;
function TA_C5(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean; begin Result := TACtor(5, AArgs, AArgc, ARes); end;
function TA_C6(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean; begin Result := TACtor(6, AArgs, AArgc, ARes); end;
function TA_C7(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean; begin Result := TACtor(7, AArgs, AArgc, ARes); end;
function TA_C8(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean; begin Result := TACtor(8, AArgs, AArgc, ARes); end;
function TA_C9(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean; begin Result := TACtor(9, AArgs, AArgc, ARes); end;
function TA_C10(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean; begin Result := TACtor(10, AArgs, AArgc, ARes); end;
function TA_C11(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean; begin Result := TACtor(11, AArgs, AArgc, ARes); end;

{ 23.2.1.1 %TypedArray% }
function TA_Abstract(AArgs: TArgs; AArgc: Integer; var ARes: TValue): Boolean;
begin
  Result := ThrowTypeError('Abstract class TypedArray not directly constructable');
end;

{ 23.2.4.2 TypedArrayCreateFromConstructor (ArgLen >= 0: the argument list is that single number) }
function TACreateFromCtor(C: TValue; AArgsP: PValue; ANArgs: Integer; AArgLen: Int64; var T: TValue): Boolean;
var Len: Int64;
begin
  if not Construct(C, AArgsP, ANArgs, C, T) then Exit(False);
  if not ValidateTA(T, Len) then Exit(False);
  if (AArgLen >= 0) and (Len < AArgLen) then Exit(ThrowTypeError('the species constructor returned a too short typed array'));
  Result := True;
end;

{ 23.2.4.1 TypedArraySpeciesCreate }
function TASpeciesCreate(AExemplar: TValue; AArgsP: PValue; ANArgs: Integer; AArgLen: Int64; var T: TValue): Boolean;
var C: TValue; Big: Boolean;
begin
  Big := TAIsBig(ObjOf(AExemplar));
  if not SpeciesCtor(AExemplar, TACtors[TAKind(ObjOf(AExemplar))], C) then Exit(False);
  if not TACreateFromCtor(C, AArgsP, ANArgs, AArgLen, T) then Exit(False);
  if TAIsBig(ObjOf(T)) <> Big then Exit(ThrowTypeError('Content type mismatch: BigInt and Number typed arrays'));
  Result := True;
end;

{ 23.2.4.3 TypedArrayCreateSameType }
function TACreateSameType(AExemplar: TValue; ALen: Int64; var T: TValue): Boolean;
begin
  Result := NewTAOf(TAKind(ObjOf(AExemplar)), ALen, TAProtosV[TAKind(ObjOf(AExemplar))], T);
end;

{$I G2.TypedArrays2.inc}

end.
