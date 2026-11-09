unit G2.ArrayProto;
{ 23.1 Array objects: Array.isArray / of / from / @@species and Array.prototype, following the spec's steps.

  Every element step asks FastArr first: a plain array (okArray, prototype Array.prototype) whose indices are
  all elements (no element guard) while Array.prototype and Object.prototype have no index properties. Then a
  hole is an absent property and the step reads or writes the elements directly. Otherwise the step takes the
  spec's path: HasProperty / Get / Set / CreateDataPropertyOrThrow / DeletePropertyOrThrow. The check runs per
  element, so a callback that changes the array mid-way moves the rest of the loop to the spec's path.

  Rooting. A native's Args live on the value stack: Args[0] is replaced by ToObject(this) and read again after
  any call; results and accumulators are temp roots at fixed offsets from Base. }
{$mode delphiunicode}{$H+}
{$MINFPCONSTPREC 64}
{$POINTERMATH ON}

interface

procedure InitArrayProto;

implementation

uses
  SysUtils, Math, G2.Core, G2.VM, G2.ObjModel;

const
  MAX_SAFE = 9007199254740991.0;   { 2^53 - 1 }

type
  TArgs = PValue;

threadvar
  CleanShapeA, CleanShapeO: PShape;
  CleanVerA, CleanVerO: Cardinal;
  ObjProtoToStringFn: TValue;   { %Object.prototype.toString%, for Array.prototype.toString }
  ArrayCtorV, ArraySpeciesFn: TValue;   { %Array% and its @@species getter, for ArraySpeciesCreate's fast case }
  JoinStack: TValue;            { arrays being joined: a cycle joins as the empty string }

function Arg(Args: TArgs; Argc, I: Integer): TValue; inline;
begin
  if I <= Argc then Result := Args[I] else Result := VUndefined;
end;

function ShapeHasIndex(S: PShape): Boolean;
var I: Integer; Idx: Int64;
begin
  for I := 0 to S^.Count - 1 do
    if IndexFromString(S^.Keys[I], Idx) then Exit(True);
  Result := False;
end;

{ Array.prototype and Object.prototype hold no index property, so a hole of a plain array reads as absent }
function ProtosCleanSlow: Boolean;
var A, O: PObj;
begin
  A := ObjOf(ArrayProto); O := ObjOf(ObjectProto);
  if (A^.Elements <> nil) and (A^.Elements^.Len <> 0) then Exit(False);
  if (A^.Shape^.Proto <> ObjectProto) or ShapeHasIndex(A^.Shape) or ShapeHasIndex(O^.Shape) then Exit(False);
  CleanShapeA := A^.Shape; CleanVerA := A^.Shape^.Version;
  CleanShapeO := O^.Shape; CleanVerO := O^.Shape^.Version;
  Result := True;
end;

function ProtosClean: Boolean; inline;
var A, O: PObj;
begin
  A := ObjOf(ArrayProto); O := ObjOf(ObjectProto);
  if (A^.Shape = CleanShapeA) and (A^.Shape^.Version = CleanVerA) and (O^.Shape = CleanShapeO) and
     (O^.Shape^.Version = CleanVerO) and ((A^.Elements = nil) or (A^.Elements^.Len = 0)) then Exit(True);
  Result := ProtosCleanSlow;
end;

function FastArr(V: TValue): Boolean; inline;
var O: PObj;
begin
  if not IsObject(V) then Exit(False);
  O := ObjOf(V);
  { an arguments object keeps its length equal to its element count (G2.VM DemoteArguments) }
  Result := (((O^.OKind = okArray) and (O^.Shape^.Proto = ArrayProto)) or
             ((O^.OKind = okArguments) and (O^.Shape^.Proto = ObjectProto))) and
    ((O^.OFlags and (ofElemGuard or ofLenReadOnly)) = 0) and (O^.Elements <> nil) and ProtosClean;
end;

{ after a bulk Move into E: an old array remembers itself (its elements may now be young) }
procedure WriteBarrierAll(E: PValueArray); inline;
begin
  if (E^.Hdr.Flags and (cfOld or cfRemembered)) = cfOld then Remember(@E^.Hdr);
end;

function IdxKey(K: Int64): PStr; inline;
begin
  Result := IntAtom(K);
end;

function IdxVal(K: Int64): TValue; inline;
begin
  if K <= High(Int32) then Result := MkInt(K) else Result := MkNumber(K);
end;

{ 7.3.12 HasProperty(O, ToString(K)) }
function HasIdx(O: TValue; K: Int64; var Has: Boolean): Boolean;
var E: PValueArray;
begin
  if FastArr(O) then
  begin
    E := ObjOf(O)^.Elements;
    Has := (K < E^.Len) and (E^.Data[K] <> VHole);
    Exit(True);
  end;
  Result := HasPropertyV(O, IdxKey(K), Has);
end;

{ 7.3.2 Get(O, ToString(K)) }
function GetIdx(O: TValue; K: Int64; var V: TValue): Boolean;
var E: PValueArray;
begin
  if FastArr(O) then
  begin
    E := ObjOf(O)^.Elements;
    if K < E^.Len then V := E^.Data[K] else V := VHole;
    if V = VHole then V := VUndefined;
    Exit(True);
  end;
  Result := GetV(O, IdxKey(K), V);
end;

{ 7.3.4 Set(O, ToString(K), V, true) }
function SetIdx(O: TValue; K: Int64; V: TValue): Boolean;
var E: PValueArray;
begin
  if FastArr(O) then
  begin
    E := ObjOf(O)^.Elements;
    if (K < E^.Len) and (E^.Data[K] <> VHole) then
    begin
      ValueArrayStore(E, K, V);
      Exit(True);
    end;
  end;
  Result := SetVS(O, IdxKey(K), V, True);
end;

{ 7.3.7 CreateDataPropertyOrThrow(A, ToString(K), V) }
function CreateIdx(A: TValue; K: Int64; V: TValue): Boolean;
var E: PValueArray; D: TPropDesc;
begin
  if FastArr(A) and ((ObjOf(A)^.OFlags and 8) = 0) then
  begin
    E := ObjOf(A)^.Elements;
    if K < E^.Len then
    begin
      ValueArrayStore(E, K, V);
      Exit(True);
    end;
    if K = E^.Len then
    begin
      ArrayPush(ObjOf(A), V);
      Exit(True);
    end;
  end;
  DataDesc(D, V, True, True, True);
  Result := DefinePropertyOrThrow(A, IdxKey(K), D);
end;

{ 7.3.10 DeletePropertyOrThrow }
function DeleteKeyOrThrow(O: TValue; Key: PStr): Boolean;
var Ok: Boolean;
begin
  if ObjOf(O)^.OKind = okProxy then
  begin
    if not ProxyDelete(O, Key, Ok) then Exit(False);
  end
  else Ok := DeleteIfConfigurable(ObjOf(O), Key);
  if not Ok then Exit(ThrowTypeError('Cannot delete property ''' + StrToUnicode(Key) + ''''));
  Result := True;
end;

function DelIdx(O: TValue; K: Int64): Boolean;
var E: PValueArray;
begin
  if FastArr(O) then
  begin
    E := ObjOf(O)^.Elements;
    if K < E^.Len then E^.Data[K] := VHole;
    Exit(True);
  end;
  Result := DeleteKeyOrThrow(O, IdxKey(K));
end;

{ Set(O, "length", N, true) }
function SetLen(O: TValue; N: Int64): Boolean;
begin
  if FastArr(O) and (N <= ObjOf(O)^.Elements^.Len) then
  begin
    ArraySetLength(ObjOf(O), N);
    Exit(True);
  end;
  Result := SetVS(O, AtomLength, IdxVal(N), True);
end;

{ 7.1.5 ToIntegerOrInfinity }
function ToIntInf(V: TValue; var D: Double): Boolean;
begin
  if IsInt(V) then begin D := IntOf(V); Exit(True); end;
  if not ToNumberV(V, D) then Exit(False);
  if IsNaN(D) then D := 0
  else if not IsInfinite(D) then D := Int(D);
  if D = 0 then D := 0;   { -0 to +0 }
  Result := True;
end;

{ a relative index (start / end arguments): undefined is Default, then clamped to 0 .. Len }
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

function ToObj(Args: TArgs): Boolean; inline;
var O: TValue;
begin
  if IsObject(Args[0]) then Exit(True);
  if not ToObjectV(Args[0], O) then Exit(False);
  Args[0] := O;
  Result := True;
end;

{ 7.2.2 IsArray }
function IsArrayV(V: TValue; var R: Boolean): Boolean;
begin
  R := False;
  while IsObject(V) do
  begin
    if ObjOf(V)^.OKind = okArray then begin R := True; Break; end;
    if ObjOf(V)^.OKind <> okProxy then Break;
    if not IsObject(ObjOf(V)^.Aux2) then
      Exit(ThrowTypeError('Cannot perform ''IsArray'' on a proxy that has been revoked'));
    V := ObjOf(V)^.Aux1;
  end;
  Result := True;
end;

{ 10.4.2.2 ArrayCreate }
function ArrayCreate(Len: Int64; var A: TValue): Boolean;
begin
  if Len > 4294967295 then Exit(ThrowError('RangeError', 'Invalid array length'));
  if Len > 64 * 1024 * 1024 then
    Exit(ThrowError('RangeError', 'Array length beyond G2''s dense arrays (no sparse arrays yet)'));
  A := MkPtr(NewArray(Len, Len));
  Result := True;
end;

{ 10.4.2.3 ArraySpeciesCreate (realms not told apart) }
{ O is a plain array whose constructor and @@species are the intrinsic ones: ArraySpeciesCreate is ArrayCreate }
function SpeciesIntact(O: TValue): Boolean;
var P, C: PObj; S: Integer;
begin
  Result := False;
  if not IsObject(O) or (ObjOf(O)^.OKind <> okArray) or (ObjOf(O)^.Shape^.Proto <> ArrayProto) then Exit;
  if ShapeLookup(ObjOf(O)^.Shape, AtomConstructor) >= 0 then Exit;
  P := ObjOf(ArrayProto);
  S := ShapeLookup(P^.Shape, AtomConstructor);
  if (S < 0) or ((P^.Shape^.PFlags[S] and pfAccessor) <> 0) or (GetSlot(P, S) <> ArrayCtorV) then Exit;
  C := ObjOf(ArrayCtorV);
  S := ShapeLookup(C^.Shape, SymKey(SymSpecies));
  if (S < 0) or ((C^.Shape^.PFlags[S] and pfAccessor) = 0) then Exit;
  Result := PAccessor(PtrOf(GetSlot(C, S)))^.Getter = ArraySpeciesFn;
end;

function ArraySpeciesCreate(O: TValue; Len: Int64; var A: TValue): Boolean;
var IsArr: Boolean; C: TValue; LV: TValue;
begin
  if SpeciesIntact(O) then Exit(ArrayCreate(Len, A));
  if not IsArrayV(O, IsArr) then Exit(False);
  if not IsArr then Exit(ArrayCreate(Len, A));
  if not GetV(O, AtomConstructor, C) then Exit(False);
  if IsObject(C) then
  begin
    if not GetElemV(C, SymSpecies, C) then Exit(False);
    if C = VNull then C := VUndefined;
  end;
  if C = VUndefined then Exit(ArrayCreate(Len, A));
  if not IsConstructorV(C) then Exit(ThrowTypeError('object.constructor[Symbol.species] is not a constructor'));
  if C = ArrayCtorV then Exit(ArrayCreate(Len, A));   { %Array%(len) }
  LV := IdxVal(Len);
  Result := Construct(C, @LV, 1, C, A);
end;

{ ---- the iteration methods ---- }

type
  TIterKind = (ikForEach, ikMap, ikFilter, ikSome, ikEvery);

{ 23.1.3.15 forEach, .21 map, .8 filter, .29 some, .6 every }
function IterateCB(Args: TArgs; Argc: Integer; var Res: TValue; Kind: TIterKind): Boolean;
var Len, K, ToIdx: Int64; Has: Boolean; V, R, A: TValue; CB: array[0..2] of TValue; Base: Integer;
begin
  if not ToObj(Args) then Exit(False);
  if not LengthOfArrayLike(Args[0], Len) then Exit(False);
  if not IsCallable(Arg(Args, Argc, 1)) then Exit(ThrowTypeError('callback is not a function'));
  A := VUndefined;
  if Kind = ikMap then
  begin
    if not ArraySpeciesCreate(Args[0], Len, A) then Exit(False);
  end
  else if Kind = ikFilter then
    if not ArraySpeciesCreate(Args[0], 0, A) then Exit(False);
  Base := TempTop;
  PushRoot(A);
  ToIdx := 0;
  K := 0;
  while K < Len do
  begin
    if not HasIdx(Args[0], K, Has) then begin PopRoots(1); Exit(False); end;
    if Has then
    begin
      if not GetIdx(Args[0], K, V) then begin PopRoots(1); Exit(False); end;
      PushRoot(V);
      CB[0] := V; CB[1] := IdxVal(K); CB[2] := Args[0];
      if not CallValue(Args[1], Arg(Args, Argc, 2), @CB[0], 3, R) then begin PopRoots(2); Exit(False); end;
      V := RootAt(TempTop - 1);
      PopRoots(1);
      case Kind of
        ikMap: if not CreateIdx(RootAt(Base), K, R) then begin PopRoots(1); Exit(False); end;
        ikFilter:
          if ToBool(R) then
          begin
            if not CreateIdx(RootAt(Base), ToIdx, V) then begin PopRoots(1); Exit(False); end;
            Inc(ToIdx);
          end;
        ikSome: if ToBool(R) then begin PopRoots(1); Res := VTrue; Exit(True); end;
        ikEvery: if not ToBool(R) then begin PopRoots(1); Res := VFalse; Exit(True); end;
      end;
    end;
    Inc(K);
  end;
  case Kind of
    ikForEach: Res := VUndefined;
    ikMap, ikFilter: Res := RootAt(Base);
    ikSome: Res := VFalse;
    ikEvery: Res := VTrue;
  end;
  PopRoots(1);
  Result := True;
end;

function AP_ForEach(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := IterateCB(Args, Argc, Res, ikForEach); end;
function AP_Map(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := IterateCB(Args, Argc, Res, ikMap); end;
function AP_Filter(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := IterateCB(Args, Argc, Res, ikFilter); end;
function AP_Some(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := IterateCB(Args, Argc, Res, ikSome); end;
function AP_Every(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := IterateCB(Args, Argc, Res, ikEvery); end;

{ 23.1.3.9 find, .10 findIndex, .11 findLast, .12 findLastIndex (FindViaPredicate: every index is read) }
function FindVia(Args: TArgs; Argc: Integer; var Res: TValue; Last, WantIndex: Boolean): Boolean;
var Len, K: Int64; V, R: TValue; CB: array[0..2] of TValue;
begin
  if not ToObj(Args) then Exit(False);
  if not LengthOfArrayLike(Args[0], Len) then Exit(False);
  if not IsCallable(Arg(Args, Argc, 1)) then Exit(ThrowTypeError('predicate is not a function'));
  if Last then K := Len - 1 else K := 0;
  while (K >= 0) and (K < Len) do
  begin
    if not GetIdx(Args[0], K, V) then Exit(False);
    PushRoot(V);
    CB[0] := V; CB[1] := IdxVal(K); CB[2] := Args[0];
    if not CallValue(Args[1], Arg(Args, Argc, 2), @CB[0], 3, R) then begin PopRoots(1); Exit(False); end;
    V := RootAt(TempTop - 1);
    PopRoots(1);
    if ToBool(R) then
    begin
      if WantIndex then Res := IdxVal(K) else Res := V;
      Exit(True);
    end;
    if Last then Dec(K) else Inc(K);
  end;
  if WantIndex then Res := MkInt(-1) else Res := VUndefined;
  Result := True;
end;

function AP_Find(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := FindVia(Args, Argc, Res, False, False); end;
function AP_FindIndex(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := FindVia(Args, Argc, Res, False, True); end;
function AP_FindLast(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := FindVia(Args, Argc, Res, True, False); end;
function AP_FindLastIndex(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := FindVia(Args, Argc, Res, True, True); end;

{ 23.1.3.24 reduce, .25 reduceRight }
function ReduceVia(Args: TArgs; Argc: Integer; var Res: TValue; Right: Boolean): Boolean;
var Len, K: Int64; Has: Boolean; V, R: TValue; CB: array[0..3] of TValue; Base: Integer;
  function InRange: Boolean; begin if Right then Result := K >= 0 else Result := K < Len; end;
  procedure Step; begin if Right then Dec(K) else Inc(K); end;
begin
  if not ToObj(Args) then Exit(False);
  if not LengthOfArrayLike(Args[0], Len) then Exit(False);
  if not IsCallable(Arg(Args, Argc, 1)) then Exit(ThrowTypeError('callback is not a function'));
  if (Len = 0) and (Argc < 2) then Exit(ThrowTypeError('Reduce of empty array with no initial value'));
  if Right then K := Len - 1 else K := 0;
  Base := TempTop;
  PushRoot(VUndefined);
  if Argc >= 2 then TempRoots[Base] := Args[2]
  else
  begin
    Has := False;
    while not Has and InRange do
    begin
      if not HasIdx(Args[0], K, Has) then begin PopRoots(1); Exit(False); end;
      if Has then
      begin
        if not GetIdx(Args[0], K, V) then begin PopRoots(1); Exit(False); end;
        TempRoots[Base] := V;
      end;
      Step;
    end;
    if not Has then begin PopRoots(1); Exit(ThrowTypeError('Reduce of empty array with no initial value')); end;
  end;
  while InRange do
  begin
    if not HasIdx(Args[0], K, Has) then begin PopRoots(1); Exit(False); end;
    if Has then
    begin
      if not GetIdx(Args[0], K, V) then begin PopRoots(1); Exit(False); end;
      CB[0] := RootAt(Base); CB[1] := V; CB[2] := IdxVal(K); CB[3] := Args[0];
      if not CallValue(Args[1], VUndefined, @CB[0], 4, R) then begin PopRoots(1); Exit(False); end;
      TempRoots[Base] := R;
    end;
    Step;
  end;
  Res := RootAt(Base);
  PopRoots(1);
  Result := True;
end;

function AP_Reduce(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := ReduceVia(Args, Argc, Res, False); end;
function AP_ReduceRight(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := ReduceVia(Args, Argc, Res, True); end;

{ ---- search ---- }

{ 23.1.3.17 indexOf }
function AP_IndexOf(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var Len, K: Int64; D: Double; Has: Boolean; V, X: TValue; E: PValueArray;
begin
  if not ToObj(Args) then Exit(False);
  if not LengthOfArrayLike(Args[0], Len) then Exit(False);
  Res := MkInt(-1);
  if Len = 0 then Exit(True);
  if not ToIntInf(Arg(Args, Argc, 2), D) then Exit(False);
  if D >= Len then Exit(True);
  if D < 0 then begin D := Len + D; if D < 0 then D := 0; end;
  K := Trunc(D);
  X := Arg(Args, Argc, 1);
  while K < Len do
  begin
    if FastArr(Args[0]) then
    begin
      { no getters can run: scan the elements }
      E := ObjOf(Args[0])^.Elements;
      while (K < Len) and (K < E^.Len) do
      begin
        V := E^.Data[K];
        if (V <> VHole) and StrictEquals(V, X) then begin Res := IdxVal(K); Exit(True); end;
        Inc(K);
      end;
      Exit(True);
    end;
    if not HasIdx(Args[0], K, Has) then Exit(False);
    if Has then
    begin
      if not GetIdx(Args[0], K, V) then Exit(False);
      if StrictEquals(V, Arg(Args, Argc, 1)) then begin Res := IdxVal(K); Exit(True); end;
    end;
    Inc(K);
  end;
  Result := True;
end;

{ 23.1.3.20 lastIndexOf }
function AP_LastIndexOf(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var Len, K: Int64; D: Double; Has: Boolean; V: TValue; E: PValueArray;
begin
  if not ToObj(Args) then Exit(False);
  if not LengthOfArrayLike(Args[0], Len) then Exit(False);
  Res := MkInt(-1);
  if Len = 0 then Exit(True);
  if Argc >= 2 then
  begin
    if not ToIntInf(Args[2], D) then Exit(False);
  end
  else D := Len - 1;
  if IsInfinite(D) and (D < 0) then Exit(True);
  if D >= 0 then
  begin
    if D > Len - 1 then D := Len - 1;
  end
  else D := Len + D;
  K := Trunc(D);
  if FastArr(Args[0]) then
  begin
    { no getters can run: scan the elements }
    E := ObjOf(Args[0])^.Elements;
    if K >= E^.Len then K := E^.Len - 1;
    while K >= 0 do
    begin
      if (E^.Data[K] <> VHole) and StrictEquals(E^.Data[K], Arg(Args, Argc, 1)) then begin Res := IdxVal(K); Exit(True); end;
      Dec(K);
    end;
    Exit(True);
  end;
  while K >= 0 do
  begin
    if not HasIdx(Args[0], K, Has) then Exit(False);
    if Has then
    begin
      if not GetIdx(Args[0], K, V) then Exit(False);
      if StrictEquals(V, Arg(Args, Argc, 1)) then begin Res := IdxVal(K); Exit(True); end;
    end;
    Dec(K);
  end;
  Result := True;
end;

function SameValueZeroV(A, B: TValue): Boolean;
begin
  if IsNumber(A) and IsNumber(B) then
  begin
    if IsNaN(NumOf(A)) and IsNaN(NumOf(B)) then Exit(True);
    Exit(NumOf(A) = NumOf(B));
  end;
  Result := StrictEquals(A, B);
end;

{ 23.1.3.16 includes }
function AP_Includes(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var Len, K: Int64; D: Double; V: TValue; E: PValueArray;
begin
  if not ToObj(Args) then Exit(False);
  if not LengthOfArrayLike(Args[0], Len) then Exit(False);
  Res := VFalse;
  if Len = 0 then Exit(True);
  if not ToIntInf(Arg(Args, Argc, 2), D) then Exit(False);
  if D >= Len then Exit(True);
  if D < 0 then begin D := Len + D; if D < 0 then D := 0; end;
  K := Trunc(D);
  if FastArr(Args[0]) then
  begin
    { no getters can run; a hole reads as undefined }
    E := ObjOf(Args[0])^.Elements;
    while K < Len do
    begin
      if K < E^.Len then V := E^.Data[K] else V := VHole;
      if V = VHole then V := VUndefined;
      if SameValueZeroV(V, Arg(Args, Argc, 1)) then begin Res := VTrue; Exit(True); end;
      Inc(K);
    end;
    Exit(True);
  end;
  while K < Len do
  begin
    if not GetIdx(Args[0], K, V) then Exit(False);
    if SameValueZeroV(V, Arg(Args, Argc, 1)) then begin Res := VTrue; Exit(True); end;
    Inc(K);
  end;
  Result := True;
end;

{ 23.1.3.1 at }
function AP_At(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var Len: Int64; D: Double;
begin
  if not ToObj(Args) then Exit(False);
  if not LengthOfArrayLike(Args[0], Len) then Exit(False);
  if not ToIntInf(Arg(Args, Argc, 1), D) then Exit(False);
  if D < 0 then D := Len + D;
  if (D < 0) or (D >= Len) then begin Res := VUndefined; Exit(True); end;
  Result := GetIdx(Args[0], Trunc(D), Res);
end;

{ ---- mutators ---- }

{ 23.1.3.23 push }
function AP_Push(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var Len: Int64; I: Integer;
begin
  if FastArr(Args[0]) and ((ObjOf(Args[0])^.OFlags and 8) = 0) then
  begin
    for I := 1 to Argc do ArrayPush(ObjOf(Args[0]), Args[I]);
    Res := MkInt(ObjOf(Args[0])^.Elements^.Len);
    Exit(True);
  end;
  if not ToObj(Args) then Exit(False);
  if not LengthOfArrayLike(Args[0], Len) then Exit(False);
  if Len + Argc > MAX_SAFE then Exit(ThrowTypeError('Pushing ' + IntToStr(Argc) + ' elements on an array-like of length ' + IntToStr(Len) + ' is disallowed'));
  for I := 1 to Argc do
  begin
    if not SetVS(Args[0], IdxKey(Len), Args[I], True) then Exit(False);
    Inc(Len);
  end;
  if not SetVS(Args[0], AtomLength, IdxVal(Len), True) then Exit(False);
  Res := IdxVal(Len);
  Result := True;
end;

{ 23.1.3.22 pop }
function AP_Pop(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var Len: Int64; E: PValueArray;
begin
  if FastArr(Args[0]) then
  begin
    E := ObjOf(Args[0])^.Elements;
    if E^.Len = 0 then begin Res := VUndefined; Exit(True); end;
    Res := E^.Data[E^.Len - 1];
    if Res = VHole then Res := VUndefined;
    ArraySetLength(ObjOf(Args[0]), E^.Len - 1);
    Exit(True);
  end;
  if not ToObj(Args) then Exit(False);
  if not LengthOfArrayLike(Args[0], Len) then Exit(False);
  if Len = 0 then
  begin
    if not SetVS(Args[0], AtomLength, MkInt(0), True) then Exit(False);
    Res := VUndefined;
    Exit(True);
  end;
  if not GetIdx(Args[0], Len - 1, Res) then Exit(False);
  PushRoot(Res);
  if not DelIdx(Args[0], Len - 1) or not SetVS(Args[0], AtomLength, IdxVal(Len - 1), True) then
  begin PopRoots(1); Exit(False); end;
  Res := RootAt(TempTop - 1);
  PopRoots(1);
  Result := True;
end;

{ moves Count properties From.. to To.. one at a time (Dir 1 ascending, -1 descending), the shared loop of shift,
  unshift, splice and copyWithin: HasProperty, then Set or DeletePropertyOrThrow }
function MoveRange(O: TValue; From, To_, Count: Int64; Dir: Integer): Boolean;
var Has: Boolean; V: TValue;
begin
  while Count > 0 do
  begin
    if not HasIdx(O, From, Has) then Exit(False);
    if Has then
    begin
      if not GetIdx(O, From, V) then Exit(False);
      if not SetIdx(O, To_, V) then Exit(False);
    end
    else if not DelIdx(O, To_) then Exit(False);
    Inc(From, Dir); Inc(To_, Dir);
    Dec(Count);
  end;
  Result := True;
end;

{ 23.1.3.27 shift }
function AP_Shift(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var Len: Int64; E: PValueArray;
begin
  if FastArr(Args[0]) then
  begin
    E := ObjOf(Args[0])^.Elements;
    if E^.Len = 0 then begin Res := VUndefined; Exit(True); end;
    Res := E^.Data[0];
    if Res = VHole then Res := VUndefined;
    Move(E^.Data[1], E^.Data[0], (E^.Len - 1) * SizeOf(TValue));
    ArraySetLength(ObjOf(Args[0]), E^.Len - 1);
    Exit(True);
  end;
  if not ToObj(Args) then Exit(False);
  if not LengthOfArrayLike(Args[0], Len) then Exit(False);
  if Len = 0 then
  begin
    if not SetVS(Args[0], AtomLength, MkInt(0), True) then Exit(False);
    Res := VUndefined;
    Exit(True);
  end;
  if not GetIdx(Args[0], 0, Res) then Exit(False);
  PushRoot(Res);
  if not MoveRange(Args[0], 1, 0, Len - 1, 1) or not DelIdx(Args[0], Len - 1) or
     not SetVS(Args[0], AtomLength, IdxVal(Len - 1), True) then begin PopRoots(1); Exit(False); end;
  Res := RootAt(TempTop - 1);
  PopRoots(1);
  Result := True;
end;

{ 23.1.3.34 unshift }
function AP_Unshift(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var Len: Int64; I: Integer; E: PValueArray;
begin
  if FastArr(Args[0]) and ((ObjOf(Args[0])^.OFlags and 8) = 0) then
  begin
    Len := ObjOf(Args[0])^.Elements^.Len;
    if Len + Argc > 64 * 1024 * 1024 then
      Exit(ThrowError('RangeError', 'Array length beyond G2''s dense arrays (no sparse arrays yet)'));
    if Argc > 0 then
    begin
      EnsureElementsCap(ObjOf(Args[0]), Len + Argc);
      E := ObjOf(Args[0])^.Elements;
      Move(E^.Data[0], E^.Data[Argc], Len * SizeOf(TValue));
      for I := 0 to Argc - 1 do E^.Data[I] := VHole;
      E^.Len := Len + Argc;
      for I := 0 to Argc - 1 do ValueArrayStore(E, I, Args[I + 1]);
    end;
    Res := IdxVal(Len + Argc);
    Exit(True);
  end;
  if not ToObj(Args) then Exit(False);
  if not LengthOfArrayLike(Args[0], Len) then Exit(False);
  if Argc > 0 then
  begin
    if Len + Argc > MAX_SAFE then Exit(ThrowTypeError('Unshift of ' + IntToStr(Argc) + ' elements on an array-like of length ' + IntToStr(Len) + ' is disallowed'));
    if not MoveRange(Args[0], Len - 1, Len + Argc - 1, Len, -1) then Exit(False);
    for I := 0 to Argc - 1 do
      if not SetIdx(Args[0], I, Args[I + 1]) then Exit(False);
  end;
  if not SetVS(Args[0], AtomLength, IdxVal(Len + Argc), True) then Exit(False);
  Res := IdxVal(Len + Argc);
  Result := True;
end;

{ 23.1.3.28 slice }
function AP_Slice(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var Len, K, Fin, N: Int64; Has: Boolean; V, A: TValue; Base: Integer;
begin
  if not ToObj(Args) then Exit(False);
  if not LengthOfArrayLike(Args[0], Len) then Exit(False);
  if not RelIdx(Arg(Args, Argc, 1), Len, 0, K) then Exit(False);
  if not RelIdx(Arg(Args, Argc, 2), Len, Len, Fin) then Exit(False);
  if not ArraySpeciesCreate(Args[0], Max(Fin - K, 0), A) then Exit(False);
  { no user code runs from here: a plain array into a fresh one is a copy of the elements (holes stay holes) }
  if FastArr(Args[0]) and FastArr(A) and (ObjOf(A)^.Elements^.Len = Max(Fin - K, 0)) and
     (Fin <= ObjOf(Args[0])^.Elements^.Len) then
  begin
    if Fin > K then
    begin
      Move(ObjOf(Args[0])^.Elements^.Data[K], ObjOf(A)^.Elements^.Data[0], (Fin - K) * SizeOf(TValue));
      WriteBarrierAll(ObjOf(A)^.Elements);
    end;
    Res := A;
    Exit(True);
  end;
  Base := TempTop;
  PushRoot(A);
  N := 0;
  while K < Fin do
  begin
    if not HasIdx(Args[0], K, Has) then begin PopRoots(1); Exit(False); end;
    if Has then
    begin
      if not GetIdx(Args[0], K, V) then begin PopRoots(1); Exit(False); end;
      if not CreateIdx(RootAt(Base), N, V) then begin PopRoots(1); Exit(False); end;
    end;
    Inc(K); Inc(N);
  end;
  if not SetLen(RootAt(Base), N) then begin PopRoots(1); Exit(False); end;
  Res := RootAt(Base);
  PopRoots(1);
  Result := True;
end;

{ 23.1.3.31 splice }
function AP_Splice(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var Len, Start, Skip, ItemCount, K: Int64; D: Double; Has: Boolean; V, A: TValue; Base, I: Integer; E: PValueArray;
begin
  if not ToObj(Args) then Exit(False);
  if not LengthOfArrayLike(Args[0], Len) then Exit(False);
  if not RelIdx(Arg(Args, Argc, 1), Len, 0, Start) then Exit(False);
  ItemCount := Max(Argc - 2, 0);
  if Argc = 0 then Skip := 0
  else if Argc = 1 then Skip := Len - Start
  else
  begin
    if not ToIntInf(Args[2], D) then Exit(False);
    D := Min(Max(D, 0), Len - Start);
    Skip := Trunc(D);
  end;
  if Len + ItemCount - Skip > MAX_SAFE then Exit(ThrowTypeError('Splice result exceeds the maximum array-like length'));
  if not ArraySpeciesCreate(Args[0], Skip, A) then Exit(False);
  { a plain array spliced into a fresh result: moves of the elements (no user code can run) }
  if FastArr(Args[0]) and ((ObjOf(Args[0])^.OFlags and 8) = 0) and FastArr(A) and
     (ObjOf(A)^.Elements^.Len = Skip) and (Len = ObjOf(Args[0])^.Elements^.Len) and
     (Len - Skip + ItemCount <= 64 * 1024 * 1024) then
  begin
    PushRoot(A);
    if Skip > 0 then
    begin
      Move(ObjOf(Args[0])^.Elements^.Data[Start], ObjOf(RootAt(TempTop - 1))^.Elements^.Data[0], Skip * SizeOf(TValue));
      WriteBarrierAll(ObjOf(RootAt(TempTop - 1))^.Elements);
    end;
    if ItemCount > Skip then EnsureElementsCap(ObjOf(Args[0]), Len - Skip + ItemCount);
    E := ObjOf(Args[0])^.Elements;
    if (ItemCount <> Skip) and (Len - Start - Skip > 0) then
      Move(E^.Data[Start + Skip], E^.Data[Start + ItemCount], (Len - Start - Skip) * SizeOf(TValue));
    E^.Len := Len - Skip + ItemCount;
    for I := 0 to ItemCount - 1 do ValueArrayStore(E, Start + I, Args[3 + I]);
    for K := E^.Len to Len - 1 do E^.Data[K] := VUndefined;   { the vacated tail: no stale references }
    Res := RootAt(TempTop - 1);
    PopRoots(1);
    Exit(True);
  end;
  Base := TempTop;
  PushRoot(A);
  for K := 0 to Skip - 1 do
  begin
    if not HasIdx(Args[0], Start + K, Has) then begin PopRoots(1); Exit(False); end;
    if Has then
    begin
      if not GetIdx(Args[0], Start + K, V) then begin PopRoots(1); Exit(False); end;
      if not CreateIdx(RootAt(Base), K, V) then begin PopRoots(1); Exit(False); end;
    end;
  end;
  if not SetLen(RootAt(Base), Skip) then begin PopRoots(1); Exit(False); end;
  if ItemCount < Skip then
  begin
    if not MoveRange(Args[0], Start + Skip, Start + ItemCount, Len - Skip - Start, 1) then begin PopRoots(1); Exit(False); end;
    K := Len;
    while K > Len - Skip + ItemCount do
    begin
      if not DelIdx(Args[0], K - 1) then begin PopRoots(1); Exit(False); end;
      Dec(K);
    end;
  end
  else if ItemCount > Skip then
  begin
    if not MoveRange(Args[0], Len - 1, Len - 1 - Skip + ItemCount, Len - Skip - Start, -1) then begin PopRoots(1); Exit(False); end;
  end;
  for I := 0 to ItemCount - 1 do
    if not SetIdx(Args[0], Start + I, Args[3 + I]) then begin PopRoots(1); Exit(False); end;
  if not SetVS(Args[0], AtomLength, IdxVal(Len - Skip + ItemCount), True) then begin PopRoots(1); Exit(False); end;
  Res := RootAt(Base);
  PopRoots(1);
  Result := True;
end;

{ 23.1.3.26 reverse }
function AP_Reverse(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var Len, Lower, Upper: Int64; LE, UE: Boolean; LV, UV: TValue; Base: Integer;
begin
  if not ToObj(Args) then Exit(False);
  if not LengthOfArrayLike(Args[0], Len) then Exit(False);
  Base := TempTop;
  PushRoot(VUndefined); PushRoot(VUndefined);
  Lower := 0;
  while Lower <> Len div 2 do
  begin
    Upper := Len - Lower - 1;
    if not HasIdx(Args[0], Lower, LE) then begin PopRoots(2); Exit(False); end;
    if LE then
    begin
      if not GetIdx(Args[0], Lower, LV) then begin PopRoots(2); Exit(False); end;
      TempRoots[Base] := LV;
    end;
    if not HasIdx(Args[0], Upper, UE) then begin PopRoots(2); Exit(False); end;
    if UE then
    begin
      if not GetIdx(Args[0], Upper, UV) then begin PopRoots(2); Exit(False); end;
      TempRoots[Base + 1] := UV;
    end;
    if LE and UE then
    begin
      if not SetIdx(Args[0], Lower, RootAt(Base + 1)) or not SetIdx(Args[0], Upper, RootAt(Base)) then begin PopRoots(2); Exit(False); end;
    end
    else if UE then
    begin
      if not SetIdx(Args[0], Lower, RootAt(Base + 1)) or not DelIdx(Args[0], Upper) then begin PopRoots(2); Exit(False); end;
    end
    else if LE then
    begin
      if not DelIdx(Args[0], Lower) or not SetIdx(Args[0], Upper, RootAt(Base)) then begin PopRoots(2); Exit(False); end;
    end;
    Inc(Lower);
  end;
  PopRoots(2);
  Res := Args[0];
  Result := True;
end;

{ 23.1.3.7 fill }
function AP_Fill(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var Len, K, Fin: Int64;
begin
  if not ToObj(Args) then Exit(False);
  if not LengthOfArrayLike(Args[0], Len) then Exit(False);
  if not RelIdx(Arg(Args, Argc, 2), Len, 0, K) then Exit(False);
  if not RelIdx(Arg(Args, Argc, 3), Len, Len, Fin) then Exit(False);
  while K < Fin do
  begin
    if FastArr(Args[0]) and (K < ObjOf(Args[0])^.Elements^.Len) then
      ValueArrayStore(ObjOf(Args[0])^.Elements, K, Arg(Args, Argc, 1))
    else if not SetVS(Args[0], IdxKey(K), Arg(Args, Argc, 1), True) then Exit(False);
    Inc(K);
  end;
  Res := Args[0];
  Result := True;
end;

{ 23.1.3.4 copyWithin }
function AP_CopyWithin(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var Len, To_, From, Fin, Count: Int64;
begin
  if not ToObj(Args) then Exit(False);
  if not LengthOfArrayLike(Args[0], Len) then Exit(False);
  if not RelIdx(Arg(Args, Argc, 1), Len, 0, To_) then Exit(False);
  if not RelIdx(Arg(Args, Argc, 2), Len, 0, From) then Exit(False);
  if not RelIdx(Arg(Args, Argc, 3), Len, Len, Fin) then Exit(False);
  Count := Min(Fin - From, Len - To_);
  if Count > 0 then
  begin
    if (From < To_) and (To_ < From + Count) then
    begin
      if not MoveRange(Args[0], From + Count - 1, To_ + Count - 1, Count, -1) then Exit(False);
    end
    else if not MoveRange(Args[0], From, To_, Count, 1) then Exit(False);
  end;
  Res := Args[0];
  Result := True;
end;

{ ---- concat, flat ---- }

{ 23.1.3.2.1 IsConcatSpreadable }
function IsConcatSpreadable(V: TValue; var R: Boolean): Boolean;
var S: TValue;
begin
  R := False;
  if not IsObject(V) then Exit(True);
  if not GetElemV(V, SymIsConcatSpreadable, S) then Exit(False);
  if S <> VUndefined then begin R := ToBool(S); Exit(True); end;
  Result := IsArrayV(V, R);
end;

{ 23.1.3.2 concat }
function AP_Concat(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var A, E, V: TValue; N, Len, K: Int64; I, Base: Integer; Spread, Has: Boolean;
begin
  if not ToObj(Args) then Exit(False);
  if not ArraySpeciesCreate(Args[0], 0, A) then Exit(False);
  Base := TempTop;
  PushRoot(A);
  N := 0;
  for I := 0 to Argc do
  begin
    E := Args[I];
    if not IsConcatSpreadable(E, Spread) then begin PopRoots(1); Exit(False); end;
    E := Args[I];
    if Spread then
    begin
      if not LengthOfArrayLike(E, Len) then begin PopRoots(1); Exit(False); end;
      if N + Len > MAX_SAFE then begin PopRoots(1); Exit(ThrowTypeError('Concat result exceeds the maximum array-like length')); end;
      { a plain array onto the end of a plain result: append its elements (holes stay holes) }
      if FastArr(E) and FastArr(RootAt(Base)) and (ObjOf(RootAt(Base))^.Elements^.Len = N) and
         (N + Len <= 64 * 1024 * 1024) then
      begin
        EnsureElementsCap(ObjOf(RootAt(Base)), N + Len);
        if Len > 0 then
          Move(ObjOf(Args[I])^.Elements^.Data[0], ObjOf(RootAt(Base))^.Elements^.Data[N], Len * SizeOf(TValue));
        ObjOf(RootAt(Base))^.Elements^.Len := N + Len;
        WriteBarrierAll(ObjOf(RootAt(Base))^.Elements);
        Inc(N, Len);
        Continue;
      end;
      K := 0;
      while K < Len do
      begin
        if not HasIdx(Args[I], K, Has) then begin PopRoots(1); Exit(False); end;
        if Has then
        begin
          if not GetIdx(Args[I], K, V) then begin PopRoots(1); Exit(False); end;
          if not CreateIdx(RootAt(Base), N, V) then begin PopRoots(1); Exit(False); end;
        end;
        Inc(N); Inc(K);
      end;
    end
    else
    begin
      if N >= MAX_SAFE then begin PopRoots(1); Exit(ThrowTypeError('Concat result exceeds the maximum array-like length')); end;
      if not CreateIdx(RootAt(Base), N, E) then begin PopRoots(1); Exit(False); end;
      Inc(N);
    end;
  end;
  if not SetLen(RootAt(Base), N) then begin PopRoots(1); Exit(False); end;
  Res := RootAt(Base);
  PopRoots(1);
  Result := True;
end;

{ 23.1.3.13.1 FlattenIntoArray: Target, Source (and the mapper's this) are roots at Base, Base + 1, Base + 2 }
function FlattenInto(Base: Integer; SourceLen, Start: Int64; Depth: Double; Mapper: TValue; var TargetIdx: Int64): Boolean;
var SI, ELen: Int64; Has, IsArr: Boolean; V: TValue; CB: array[0..2] of TValue; SB: Integer;
begin
  TargetIdx := Start;
  SI := 0;
  while SI < SourceLen do
  begin
    if not HasIdx(RootAt(Base + 1), SI, Has) then Exit(False);
    if Has then
    begin
      if not GetIdx(RootAt(Base + 1), SI, V) then Exit(False);
      if Mapper <> VUndefined then
      begin
        PushRoot(Mapper);
        CB[0] := V; CB[1] := IdxVal(SI); CB[2] := RootAt(Base + 1);
        if not CallValue(Mapper, RootAt(Base + 2), @CB[0], 3, V) then begin PopRoots(1); Exit(False); end;
        Mapper := RootAt(TempTop - 1);
        PopRoots(1);
      end;
      IsArr := False;
      if Depth > 0 then
        if not IsArrayV(V, IsArr) then Exit(False);
      if IsArr then
      begin
        PushRoot(Mapper);
        SB := TempTop;
        PushRoot(RootAt(Base)); PushRoot(V); PushRoot(VUndefined);
        if not LengthOfArrayLike(V, ELen) or
           not FlattenInto(SB, ELen, TargetIdx, Depth - 1, VUndefined, TargetIdx) then begin PopRoots(4); Exit(False); end;
        Mapper := RootAt(SB - 1);
        PopRoots(4);
      end
      else
      begin
        if TargetIdx >= MAX_SAFE then Exit(ThrowTypeError('Flattened array exceeds the maximum array-like length'));
        PushRoot(Mapper);
        if not CreateIdx(RootAt(Base), TargetIdx, V) then begin PopRoots(1); Exit(False); end;
        Mapper := RootAt(TempTop - 1);
        PopRoots(1);
        Inc(TargetIdx);
      end;
    end;
    Inc(SI);
  end;
  Result := True;
end;

{ 23.1.3.13 flat }
function AP_Flat(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var Len, T: Int64; D: Double; A: TValue; Base: Integer;
begin
  if not ToObj(Args) then Exit(False);
  if not LengthOfArrayLike(Args[0], Len) then Exit(False);
  D := 1;
  if Arg(Args, Argc, 1) <> VUndefined then
  begin
    if not ToIntInf(Args[1], D) then Exit(False);
    if D < 0 then D := 0;
  end;
  if not ArraySpeciesCreate(Args[0], 0, A) then Exit(False);
  Base := TempTop;
  PushRoot(A); PushRoot(Args[0]); PushRoot(VUndefined);
  if not FlattenInto(Base, Len, 0, D, VUndefined, T) then begin PopRoots(3); Exit(False); end;
  Res := RootAt(Base);
  PopRoots(3);
  Result := True;
end;

{ 23.1.3.14 flatMap }
function AP_FlatMap(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var Len, T: Int64; A: TValue; Base: Integer;
begin
  if not ToObj(Args) then Exit(False);
  if not LengthOfArrayLike(Args[0], Len) then Exit(False);
  if not IsCallable(Arg(Args, Argc, 1)) then Exit(ThrowTypeError('flatMap mapper is not a function'));
  if not ArraySpeciesCreate(Args[0], 0, A) then Exit(False);
  Base := TempTop;
  PushRoot(A); PushRoot(Args[0]); PushRoot(Arg(Args, Argc, 2));
  if not FlattenInto(Base, Len, 0, 1, Args[1], T) then begin PopRoots(3); Exit(False); end;
  Res := RootAt(Base);
  PopRoots(3);
  Result := True;
end;

{ ---- join, toString, toLocaleString ---- }

function OnJoinStack(O: TValue): Boolean;
var I: Integer; E: PValueArray;
begin
  E := ObjOf(JoinStack)^.Elements;
  for I := 0 to E^.Len - 1 do
    if E^.Data[I] = O then Exit(True);
  Result := False;
end;

procedure JoinStackPop;
var E: PValueArray;
begin
  E := ObjOf(JoinStack)^.Elements;
  if E^.Len > 0 then ArraySetLength(ObjOf(JoinStack), E^.Len - 1);
end;

{ 23.1.3.18 join and 23.1.3.32 toLocaleString (Locale: each element's toLocaleString(locales, options), ECMA-402
  19.5.1), with a cycle joined as the empty string }
function JoinVia(Args: TArgs; Argc: Integer; var Res: TValue; Locale: Boolean): Boolean;
var Len, K: Int64; Sep, SV, V, F: TValue; B: UnicodeString; Used: Integer; LA: array[0..1] of TValue; Base: Integer;
  procedure Add(S: PStr);
  begin
    if Used + S^.Len > Length(B) then SetLength(B, Max(Length(B) * 2, Used + S^.Len + 16));
    if S^.Len > 0 then Move(StrChars(S)[0], B[Used + 1], S^.Len * SizeOf(WideChar));
    Inc(Used, S^.Len);
  end;
begin
  if not ToObj(Args) then Exit(False);
  if not LengthOfArrayLike(Args[0], Len) then Exit(False);
  Base := TempTop;
  if Locale then Sep := StrFromUnicode(',')
  else if Arg(Args, Argc, 1) = VUndefined then Sep := StrFromUnicode(',')
  else if not ToStringV(Args[1], Sep) then Exit(False);
  if OnJoinStack(Args[0]) then begin Res := StrFromUnicode(''); Exit(True); end;
  PushRoot(Sep);
  ArrayPush(ObjOf(JoinStack), Args[0]);
  B := '';
  Used := 0;
  K := 0;
  Result := False;
  while K < Len do
  begin
    if K > 0 then Add(StrOf(RootAt(Base)));
    if not GetIdx(Args[0], K, V) then Break;
    if not IsNullish(V) then
    begin
      if Locale then
      begin
        PushRoot(V);
        if not GetV(V, AtomOf('toLocaleString'), F) then begin PopRoots(1); Break; end;
        V := RootAt(TempTop - 1);
        PopRoots(1);
        if not IsCallable(F) then begin ThrowTypeError('toLocaleString is not a function'); Break; end;
        { ECMA-402 19.5.1: locales and options pass through to each element }
        LA[0] := Arg(Args, Argc, 1); LA[1] := Arg(Args, Argc, 2);
        if not CallValue(F, V, @LA[0], 2, V) then Break;
      end;
      if not ToStringV(V, SV) then Break;
      Add(StrOf(SV));
    end;
    Inc(K);
  end;
  if K >= Len then Result := True;
  JoinStackPop;
  PopRoots(1);
  if Result then
  begin
    SetLength(B, Used);
    Res := StrFromUnicode(B);
  end;
end;

function AP_Join(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := JoinVia(Args, Argc, Res, False); end;
function AP_ToLocaleString(Args: TArgs; Argc: Integer; var Res: TValue): Boolean; begin Result := JoinVia(Args, Argc, Res, True); end;

{ 23.1.3.36 toString }
function AP_ToString(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var F: TValue;
begin
  if not ToObj(Args) then Exit(False);
  if not GetV(Args[0], AtomOf('join'), F) then Exit(False);
  if not IsCallable(F) then F := ObjProtoToStringFn;
  Result := CallValue(F, Args[0], nil, 0, Res);
end;

{ ---- sort ---- }

threadvar
  SortFn: TValue;

{ 23.1.3.30.2 SortCompare: Less when x sorts before y (a stable sort keeps equal ones in order) }
function SortLessThan(X, Y: TValue; var Less: Boolean): Boolean;
var CB: array[0..1] of TValue; R, SX, SY: TValue; D: Double;
begin
  Less := False;
  if X = VUndefined then Exit(True);
  if Y = VUndefined then begin Less := True; Exit(True); end;
  if SortFn <> VUndefined then
  begin
    CB[0] := X; CB[1] := Y;
    if not CallValue(SortFn, VUndefined, @CB[0], 2, R) then Exit(False);
    if not ToNumberV(R, D) then Exit(False);
    Less := D < 0;
    Exit(True);
  end;
  if IsInt(X) and IsInt(Y) and (IntOf(X) >= 0) and (IntOf(Y) >= 0) and
     ((IntOf(X) < 10) = (IntOf(Y) < 10)) and (IntOf(X) < 10) then
  begin
    Less := IntOf(X) < IntOf(Y);
    Exit(True);
  end;
  PushRoot(Y);
  if not ToStringV(X, SX) then begin PopRoots(1); Exit(False); end;
  PushRoot(SX);
  if not ToStringV(RootAt(TempTop - 2), SY) then begin PopRoots(2); Exit(False); end;
  SX := RootAt(TempTop - 1);
  PopRoots(2);
  Less := StrCompare(StrOf(SX), StrOf(SY)) < 0;
  Result := True;
end;

{ 23.1.3.30.1 SortIndexedProperties: the values (holes skipped when SkipHoles) into a new array at root Base,
  sorted by a stable bottom-up merge sort through a scratch array }
function SortIndexed(O: TValue; Len: Int64; SkipHoles: Boolean; Base: Integer): Boolean;
var K, N, W, I, L, M, R_, J: Int64; Has, Less: Boolean; V: TValue; Src, Dst: PValueArray; T: TValue;
begin
  PushRoot(O);
  TempRoots[Base] := MkPtr(NewArray(0, Min(Len, 1024)));
  K := 0;
  while K < Len do
  begin
    Has := True;
    if SkipHoles then
      if not HasIdx(RootAt(TempTop - 1), K, Has) then begin PopRoots(1); Exit(False); end;
    if Has then
    begin
      if not GetIdx(RootAt(TempTop - 1), K, V) then begin PopRoots(1); Exit(False); end;
      ArrayPush(ObjOf(RootAt(Base)), V);
    end;
    Inc(K);
  end;
  PopRoots(1);
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
          if not SortLessThan(Src^.Data[M], Src^.Data[L], Less) then Exit(False);
          Src := ObjOf(RootAt(Base))^.Elements;
          if Less then begin V := Src^.Data[M]; Inc(M); end
          else begin V := Src^.Data[L]; Inc(L); end;
        end;
        Dst := ObjOf(RootAt(Base + 1))^.Elements;
        ValueArrayStore(Dst, J, V);
        Inc(J);
      end;
      Inc(I, 2 * W);
    end;
    T := TempRoots[Base]; TempRoots[Base] := TempRoots[Base + 1]; TempRoots[Base + 1] := T;
    W := W * 2;
  end;
  Result := True;
end;

function SortSetup(Args: TArgs; Argc: Integer; var Saved: TValue): Boolean;
begin
  if (Arg(Args, Argc, 1) <> VUndefined) and not IsCallable(Args[1]) then
    Exit(ThrowTypeError('The comparison function must be either a function or undefined'));
  Saved := SortFn;
  SortFn := Arg(Args, Argc, 1);
  Result := True;
end;

{ 23.1.3.30 sort }
function AP_Sort(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var Len, J, N: Int64; Saved: TValue; Base: Integer; E: PValueArray;
begin
  if not SortSetup(Args, Argc, Saved) then Exit(False);
  Result := False;
  Base := TempTop;
  PushRoot(Saved); PushRoot(VUndefined); PushRoot(VUndefined);
  if ToObj(Args) and LengthOfArrayLike(Args[0], Len) and SortIndexed(Args[0], Len, True, Base + 1) then
  begin
    N := ObjOf(RootAt(Base + 1))^.Elements^.Len;
    J := 0;
    Result := True;
    while Result and (J < N) do
    begin
      E := ObjOf(RootAt(Base + 1))^.Elements;
      Result := SetIdx(Args[0], J, E^.Data[J]);
      Inc(J);
    end;
    while Result and (J < Len) do
    begin
      Result := DelIdx(Args[0], J);
      Inc(J);
    end;
  end;
  SortFn := RootAt(Base);
  PopRoots(3);
  if Result then Res := Args[0];
end;

{ 23.1.3.35 toSorted }
function AP_ToSorted(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var Len: Int64; Saved: TValue; Base: Integer;
begin
  if not SortSetup(Args, Argc, Saved) then Exit(False);
  Result := False;
  Base := TempTop;
  PushRoot(Saved); PushRoot(VUndefined); PushRoot(VUndefined);
  if ToObj(Args) and LengthOfArrayLike(Args[0], Len) and
     ((Len <= 4294967295) or ThrowError('RangeError', 'Invalid array length')) then
    { ArrayCreate(len) filled from the sorted list, which has len values (holes read as undefined): the list
      itself is that array }
    if SortIndexed(Args[0], Len, False, Base + 1) then
    begin
      Res := RootAt(Base + 1);
      Result := True;
    end;
  SortFn := RootAt(Base);
  PopRoots(3);
end;

{ ---- copying methods ---- }

{ 23.1.3.33 toReversed }
function AP_ToReversed(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var Len, K: Int64; A, V: TValue; Base: Integer;
begin
  if not ToObj(Args) then Exit(False);
  if not LengthOfArrayLike(Args[0], Len) then Exit(False);
  if not ArrayCreate(Len, A) then Exit(False);
  Base := TempTop;
  PushRoot(A);
  for K := 0 to Len - 1 do
  begin
    if not GetIdx(Args[0], Len - K - 1, V) then begin PopRoots(1); Exit(False); end;
    if not CreateIdx(RootAt(Base), K, V) then begin PopRoots(1); Exit(False); end;
  end;
  Res := RootAt(Base);
  PopRoots(1);
  Result := True;
end;

{ 23.1.3.35 toSpliced }
function AP_ToSpliced(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var Len, Start, Skip, Ins, NewLen, I, R_: Int64; D: Double; A, V: TValue; Base, J: Integer;
begin
  if not ToObj(Args) then Exit(False);
  if not LengthOfArrayLike(Args[0], Len) then Exit(False);
  if not RelIdx(Arg(Args, Argc, 1), Len, 0, Start) then Exit(False);
  Ins := Max(Argc - 2, 0);
  if Argc = 0 then Skip := 0
  else if Argc = 1 then Skip := Len - Start
  else
  begin
    if not ToIntInf(Args[2], D) then Exit(False);
    Skip := Trunc(Min(Max(D, 0), Len - Start));
  end;
  NewLen := Len + Ins - Skip;
  if NewLen > MAX_SAFE then Exit(ThrowTypeError('toSpliced result exceeds the maximum array-like length'));
  if not ArrayCreate(NewLen, A) then Exit(False);
  Base := TempTop;
  PushRoot(A);
  I := 0;
  R_ := Start + Skip;
  while I < Start do
  begin
    if not GetIdx(Args[0], I, V) or not CreateIdx(RootAt(Base), I, V) then begin PopRoots(1); Exit(False); end;
    Inc(I);
  end;
  for J := 0 to Ins - 1 do
  begin
    if not CreateIdx(RootAt(Base), I, Args[3 + J]) then begin PopRoots(1); Exit(False); end;
    Inc(I);
  end;
  while I < NewLen do
  begin
    if not GetIdx(Args[0], R_, V) or not CreateIdx(RootAt(Base), I, V) then begin PopRoots(1); Exit(False); end;
    Inc(I); Inc(R_);
  end;
  Res := RootAt(Base);
  PopRoots(1);
  Result := True;
end;

{ 23.1.3.39 with }
function AP_With(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var Len, K, Idx: Int64; D: Double; A, V: TValue; Base: Integer;
begin
  if not ToObj(Args) then Exit(False);
  if not LengthOfArrayLike(Args[0], Len) then Exit(False);
  if not ToIntInf(Arg(Args, Argc, 1), D) then Exit(False);
  if D < 0 then D := Len + D;
  if (D >= Len) or (D < 0) then Exit(ThrowError('RangeError', 'Invalid index'));
  Idx := Trunc(D);
  if not ArrayCreate(Len, A) then Exit(False);
  Base := TempTop;
  PushRoot(A);
  for K := 0 to Len - 1 do
  begin
    if K = Idx then V := Arg(Args, Argc, 2)
    else if not GetIdx(Args[0], K, V) then begin PopRoots(1); Exit(False); end;
    if not CreateIdx(RootAt(Base), K, V) then begin PopRoots(1); Exit(False); end;
  end;
  Res := RootAt(Base);
  PopRoots(1);
  Result := True;
end;

{ ---- iterators ---- }

{ 23.1.3.5 entries, .19 keys, .38 values: CreateArrayIterator(ToObject(this), kind) }
function AP_Entries(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
begin
  if not ToObj(Args) then Exit(False);
  Res := NewArrayIterator(Args[0], 3);
  Result := True;
end;

function AP_Keys(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
begin
  if not ToObj(Args) then Exit(False);
  Res := NewArrayIterator(Args[0], 2);
  Result := True;
end;

function AP_Values(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
begin
  if not ToObj(Args) then Exit(False);
  Res := NewArrayIterator(Args[0], 0);
  Result := True;
end;

{ ---- the constructor's functions ---- }

{ 23.1.2.2 Array.isArray }
function Array_IsArray2(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var R: Boolean;
begin
  if not IsArrayV(Arg(Args, Argc, 1), R) then Exit(False);
  Res := MkBool(R);
  Result := True;
end;

{ IsConstructor(C) ? Construct(C, « Len » or « ») : ArrayCreate(Len) }
function CreateFromThis(C: TValue; Len: Int64; WithLen: Boolean; var A: TValue): Boolean;
var LV: TValue;
begin
  if IsConstructorV(C) then
  begin
    LV := IdxVal(Len);
    if WithLen then Result := Construct(C, @LV, 1, C, A)
    else Result := Construct(C, nil, 0, C, A);
    Exit;
  end;
  if WithLen then Result := ArrayCreate(Len, A) else Result := ArrayCreate(0, A);
end;

{ 23.1.2.3 Array.of }
function Array_Of2(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var A: TValue; K, Base: Integer;
begin
  if not CreateFromThis(Args[0], Argc, True, A) then Exit(False);
  Base := TempTop;
  PushRoot(A);
  for K := 0 to Argc - 1 do
    if not CreateIdx(RootAt(Base), K, Args[K + 1]) then begin PopRoots(1); Exit(False); end;
  if not SetVS(RootAt(Base), AtomLength, MkInt(Argc), True) then begin PopRoots(1); Exit(False); end;
  Res := RootAt(Base);
  PopRoots(1);
  Result := True;
end;

{ 23.1.2.1 Array.from }
function Array_From2(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var Mapping, Done: Boolean; UsingIt, A, It, NextM, V, AL: TValue; K, Len: Int64; CB: array[0..1] of TValue;
  Base: Integer;
begin
  Mapping := Arg(Args, Argc, 2) <> VUndefined;
  if Mapping and not IsCallable(Args[2]) then Exit(ThrowTypeError('Array.from: the mapper is not a function'));
  { 7.3.11 GetMethod(items, @@iterator): a nullish items throws in GetV }
  if not GetElemV(Arg(Args, Argc, 1), SymIterator, UsingIt) then Exit(False);
  if IsNullish(UsingIt) then UsingIt := VUndefined
  else if not IsCallable(UsingIt) then Exit(ThrowTypeError('Array.from: @@iterator is not a function'));
  Base := TempTop;
  if UsingIt <> VUndefined then
  begin
    PushRoot(UsingIt);
    if not CreateFromThis(Args[0], 0, False, A) then begin PopRoots(1); Exit(False); end;
    TempRoots[Base] := A;
    { 7.4.4 GetIteratorFromMethod }
    if not CallValue(UsingIt, Args[1], nil, 0, It) then begin PopRoots(1); Exit(False); end;
    if not IsObject(It) then begin PopRoots(1); Exit(ThrowTypeError('Array.from: the iterator is not an object')); end;
    PushRoot(It);
    if not GetV(It, AtomNext, NextM) then begin PopRoots(2); Exit(False); end;
    PushRoot(NextM);
    K := 0;
    while True do
    begin
      if not IteratorStepRec(RootAt(Base + 1), RootAt(Base + 2), V, Done) then begin PopRoots(3); Exit(False); end;
      if Done then
      begin
        if not SetVS(RootAt(Base), AtomLength, IdxVal(K), True) then begin PopRoots(3); Exit(False); end;
        Res := RootAt(Base);
        PopRoots(3);
        Exit(True);
      end;
      if Mapping then
      begin
        CB[0] := V; CB[1] := IdxVal(K);
        if not CallValue(Args[2], Arg(Args, Argc, 3), @CB[0], 2, V) then
        begin
          IteratorCloseV(RootAt(Base + 1), True);
          PopRoots(3);
          Exit(False);
        end;
      end;
      if not CreateIdx(RootAt(Base), K, V) then
      begin
        IteratorCloseV(RootAt(Base + 1), True);
        PopRoots(3);
        Exit(False);
      end;
      Inc(K);
    end;
  end;
  { an array-like }
  if not ToObjectV(Args[1], AL) then Exit(False);
  PushRoot(AL);
  if not LengthOfArrayLike(AL, Len) or not CreateFromThis(Args[0], Len, True, A) then begin PopRoots(1); Exit(False); end;
  PushRoot(A);
  K := 0;
  while K < Len do
  begin
    if not GetIdx(RootAt(Base), K, V) then begin PopRoots(2); Exit(False); end;
    if Mapping then
    begin
      CB[0] := V; CB[1] := IdxVal(K);
      if not CallValue(Args[2], Arg(Args, Argc, 3), @CB[0], 2, V) then begin PopRoots(2); Exit(False); end;
    end;
    if not CreateIdx(RootAt(Base + 1), K, V) then begin PopRoots(2); Exit(False); end;
    Inc(K);
  end;
  if not SetVS(RootAt(Base + 1), AtomLength, IdxVal(Len), True) then begin PopRoots(2); Exit(False); end;
  Res := RootAt(Base + 1);
  PopRoots(2);
  Result := True;
end;

{ 23.1.2.5 get Array [ @@species ] }
function Array_Species(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
begin
  Res := Args[0];
  Result := True;
end;

const
  SpeciesCtors: array[0..4] of UnicodeString = ('Map', 'Set', 'RegExp', 'ArrayBuffer', 'SharedArrayBuffer');

procedure InitArrayProto;
var Ctor, U: TValue; Key: PStr; I: Integer;
  procedure Def(Target: TValue; const Name: UnicodeString; Fn: TNativeFn; Len: Integer);
  begin
    PushRoot(Target);
    DefineOwn(RootAt(TempTop - 1), AtomOf(Name), NewNativeFunction(Fn, Name, Len), pfHidden);
    PopRoots(1);
  end;
  procedure Unscop(const Name: UnicodeString);
  begin
    DefineOwn(RootAt(TempTop - 1), AtomOf(Name), VTrue, pfDefault);
  end;
begin
  AddRoot(@ObjProtoToStringFn); AddRoot(@JoinStack); AddRoot(@SortFn);
  CleanShapeA := nil; CleanShapeO := nil;
  SortFn := VUndefined;
  JoinStack := MkPtr(NewArray(0, 8));
  GetV(ObjectProto, AtomToString, ObjProtoToStringFn);
  GetV(ArrayProto, AtomConstructor, Ctor);
  PushRoot(Ctor);
  Def(RootAt(TempTop - 1), 'isArray', Array_IsArray2, 1);
  Def(RootAt(TempTop - 1), 'of', Array_Of2, 0);
  Def(RootAt(TempTop - 1), 'from', Array_From2, 1);
  DefineNativeGetterKey(RootAt(TempTop - 1), SymKey(SymSpecies), 'get [Symbol.species]', Array_Species);
  PopRoots(1);
  AddRoot(@ArrayCtorV); AddRoot(@ArraySpeciesFn);
  GetV(ArrayProto, AtomConstructor, ArrayCtorV);
  I := ShapeLookup(ObjOf(ArrayCtorV)^.Shape, SymKey(SymSpecies));
  ArraySpeciesFn := PAccessor(PtrOf(GetSlot(ObjOf(ArrayCtorV), I)))^.Getter;
  { the other constructors with a @@species getter returning this (Promise and %TypedArray% have their own) }
  for I := 0 to High(SpeciesCtors) do
    if GetV(GlobalObj, AtomOf(SpeciesCtors[I]), Ctor) and IsObject(Ctor) and
       (ShapeLookup(ObjOf(Ctor)^.Shape, SymKey(SymSpecies)) < 0) then
    begin
      PushRoot(Ctor);
      DefineNativeGetterKey(RootAt(TempTop - 1), SymKey(SymSpecies), 'get [Symbol.species]', Array_Species);
      PopRoots(1);
    end;
  Def(ArrayProto, 'at', AP_At, 1);
  Def(ArrayProto, 'concat', AP_Concat, 1);
  Def(ArrayProto, 'copyWithin', AP_CopyWithin, 2);
  Def(ArrayProto, 'entries', AP_Entries, 0);
  Def(ArrayProto, 'every', AP_Every, 1);
  Def(ArrayProto, 'fill', AP_Fill, 1);
  Def(ArrayProto, 'filter', AP_Filter, 1);
  Def(ArrayProto, 'find', AP_Find, 1);
  Def(ArrayProto, 'findIndex', AP_FindIndex, 1);
  Def(ArrayProto, 'findLast', AP_FindLast, 1);
  Def(ArrayProto, 'findLastIndex', AP_FindLastIndex, 1);
  Def(ArrayProto, 'flat', AP_Flat, 0);
  Def(ArrayProto, 'flatMap', AP_FlatMap, 1);
  Def(ArrayProto, 'forEach', AP_ForEach, 1);
  Def(ArrayProto, 'includes', AP_Includes, 1);
  Def(ArrayProto, 'indexOf', AP_IndexOf, 1);
  Def(ArrayProto, 'join', AP_Join, 1);
  Def(ArrayProto, 'keys', AP_Keys, 0);
  Def(ArrayProto, 'lastIndexOf', AP_LastIndexOf, 1);
  Def(ArrayProto, 'map', AP_Map, 1);
  Def(ArrayProto, 'pop', AP_Pop, 0);
  Def(ArrayProto, 'push', AP_Push, 1);
  Def(ArrayProto, 'reduce', AP_Reduce, 1);
  Def(ArrayProto, 'reduceRight', AP_ReduceRight, 1);
  Def(ArrayProto, 'reverse', AP_Reverse, 0);
  Def(ArrayProto, 'shift', AP_Shift, 0);
  Def(ArrayProto, 'slice', AP_Slice, 2);
  Def(ArrayProto, 'some', AP_Some, 1);
  Def(ArrayProto, 'sort', AP_Sort, 1);
  Def(ArrayProto, 'splice', AP_Splice, 2);
  Def(ArrayProto, 'toLocaleString', AP_ToLocaleString, 0);
  Def(ArrayProto, 'toReversed', AP_ToReversed, 0);
  Def(ArrayProto, 'toSorted', AP_ToSorted, 1);
  Def(ArrayProto, 'toSpliced', AP_ToSpliced, 2);
  Def(ArrayProto, 'toString', AP_ToString, 0);
  Def(ArrayProto, 'unshift', AP_Unshift, 1);
  Def(ArrayProto, 'with', AP_With, 2);
  { values keeps its function object (ArrayValuesFnPublic is Array.prototype[@@iterator] and the arguments
    objects' @@iterator, and fast paths compare against it): only its behaviour becomes 23.1.3.38 }
  ObjOf(ArrayValuesFnPublic)^.Native := Pointer(@AP_Values);
  { 23.1.3.41 Array.prototype [ @@unscopables ] }
  U := MkPtr(NewPlainObject(VNull, 16));
  PushRoot(U);
  Unscop('at'); Unscop('copyWithin'); Unscop('entries'); Unscop('fill'); Unscop('find'); Unscop('findIndex');
  Unscop('findLast'); Unscop('findLastIndex'); Unscop('flat'); Unscop('flatMap'); Unscop('includes');
  Unscop('keys'); Unscop('toReversed'); Unscop('toSorted'); Unscop('toSpliced'); Unscop('values');
  Key := SymKey(SymUnscopables);
  DefineOwn(ArrayProto, Key, RootAt(TempTop - 1), pfConfigurable);
  PopRoots(1);
end;

end.
