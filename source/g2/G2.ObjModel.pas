unit G2.ObjModel;
{ The object internal methods for property descriptors (ES2026 10.1.5 [[GetOwnProperty]], 10.1.6
  [[DefineOwnProperty]] with ValidateAndApplyPropertyDescriptor) and their exotic variants: arrays (10.4.2:
  length and indices), String objects (10.4.3), typed arrays (10.4.5), proxies (10.5.5, 10.5.6, by their traps);
  SetIntegrityLevel / TestIntegrityLevel (7.3.15, 7.3.16).

  Storage. A data property with the default attributes on an index of an object with elements lives in the
  elements; any other index property (other attributes, an accessor) lives in the shape under its string key,
  with a hole in the elements, and the object gets the element guard (OFlags 64) so element writes take the
  checked path. An array's length is not writable when OFlags 128 is set. }
{$mode delphiunicode}{$H+}
{$MINFPCONSTPREC 64}
{$POINTERMATH ON}

interface

uses
  G2.Core;

type
  TPropDesc = record
    HasValue, HasWritable, HasGet, HasSet, HasEnumerable, HasConfigurable: Boolean;
    Value, Get, Set_: TValue;
    Writable, Enumerable, Configurable: Boolean;
  end;

const
  ofElemGuard = 64;     { element writes go through the checked path }
  ofLenReadOnly = 128;  { an array's length is not writable }

{ 6.2.6.5 ToPropertyDescriptor; the descriptor's values are rooted by the caller }
function ToPropertyDescriptor(V: TValue; var D: TPropDesc): Boolean;
{ 6.2.6.4 FromPropertyDescriptor (undefined when not Found) }
function FromPropertyDescriptor(const D: TPropDesc; Found: Boolean): TValue;
{ [[GetOwnProperty]] }
function GetOwnPropertyV(O: TValue; Key: PStr; var D: TPropDesc; var Found: Boolean): Boolean;
{ [[DefineOwnProperty]]: Ok the success flag (DefinePropertyOrThrow turns False into a TypeError) }
function DefineOwnPropertyV(O: TValue; Key: PStr; const DIn: TPropDesc; var Ok: Boolean): Boolean;
function DefinePropertyOrThrow(O: TValue; Key: PStr; const D: TPropDesc): Boolean;
{ 7.3.15 SetIntegrityLevel (Frozen: frozen, else sealed); Ok False when a proxy refused }
function SetIntegrityLevel(O: TValue; Frozen: Boolean; var Ok: Boolean): Boolean;
{ 7.3.16 TestIntegrityLevel }
function TestIntegrityLevel(O: TValue; Frozen: Boolean; var Res: Boolean): Boolean;
{ [[PreventExtensions]] }
function PreventExtensionsV(O: TValue; var Ok: Boolean): Boolean;
{ [[OwnPropertyKeys]] as an array: integer keys ascending, strings in order, then symbols }
function OwnPropertyKeysV(O: TValue; var Res: TValue): Boolean;
{ 7.1.18 ToObject }
function ToObjectV(V: TValue; var O: TValue): Boolean;
{ a data descriptor with all fields present }
procedure DataDesc(var D: TPropDesc; V: TValue; W, E, C: Boolean);
{ 7.2.10 SameValue }
function SameValueV(A, B: TValue): Boolean;
procedure InitObjModel;

implementation

uses
  SysUtils, Math, G2.Bytecode, G2.VM, G2.Typed;

threadvar
  AtomEnumerable, AtomConfigurable, AtomWritable, AtomGet, AtomSet: PStr;

procedure EmptyDesc(var D: TPropDesc); inline;
begin
  FillChar(D, SizeOf(D), 0);
  D.Value := VUndefined; D.Get := VUndefined; D.Set_ := VUndefined;
end;

function IsAccessorDesc(const D: TPropDesc): Boolean; inline;
begin
  Result := D.HasGet or D.HasSet;
end;

function IsDataDesc(const D: TPropDesc): Boolean; inline;
begin
  Result := D.HasValue or D.HasWritable;
end;

function ToPropertyDescriptor(V: TValue; var D: TPropDesc): Boolean;
var T: TValue; Has: Boolean; Base: Integer;
  function Field(Key: PStr; var Present: Boolean; var Res: TValue): Boolean;
  begin
    Res := VUndefined;
    if not HasPropertyV(RootAt(Base), Key, Present) then Exit(False);
    if Present then Result := GetV(RootAt(Base), Key, Res) else Result := True;
  end;
begin
  EmptyDesc(D);
  if not IsObject(V) then Exit(ThrowTypeError('Property description must be an object'));
  PushRoot(V);
  Base := TempTop - 1;   { the descriptor object: the fields read below push more roots }
  Result := False;
  if not Field(AtomEnumerable, Has, T) then begin PopRoots(1); Exit; end;
  if Has then begin D.HasEnumerable := True; D.Enumerable := ToBool(T); end;
  if not Field(AtomConfigurable, Has, T) then begin PopRoots(1); Exit; end;
  if Has then begin D.HasConfigurable := True; D.Configurable := ToBool(T); end;
  if not Field(AtomValue, Has, T) then begin PopRoots(1); Exit; end;
  if Has then begin D.HasValue := True; D.Value := T; PushRoot(T); end;
  if not Field(AtomWritable, Has, T) then begin PopRoots(1 + Ord(D.HasValue)); Exit; end;
  if Has then begin D.HasWritable := True; D.Writable := ToBool(T); end;
  if not Field(AtomGet, Has, T) then begin PopRoots(1 + Ord(D.HasValue)); Exit; end;
  if Has then
  begin
    if not IsCallable(T) and (T <> VUndefined) then begin PopRoots(1 + Ord(D.HasValue)); Exit(ThrowTypeError('Getter must be a function')); end;
    D.HasGet := True; D.Get := T; PushRoot(T);
  end;
  if not Field(AtomSet, Has, T) then begin PopRoots(1 + Ord(D.HasValue) + Ord(D.HasGet)); Exit; end;
  if Has then
  begin
    if not IsCallable(T) and (T <> VUndefined) then
    begin
      PopRoots(1 + Ord(D.HasValue) + Ord(D.HasGet));
      Exit(ThrowTypeError('Setter must be a function'));
    end;
    D.HasSet := True; D.Set_ := T;
  end;
  { the rooted copies were for the Gets in between; the values stay reachable through the descriptor object }
  if D.HasGet then D.Get := RootAt(TempTop - 1);
  if D.HasValue then D.Value := RootAt(TempTop - 1 - Ord(D.HasGet));
  PopRoots(1 + Ord(D.HasValue) + Ord(D.HasGet));
  if IsAccessorDesc(D) and IsDataDesc(D) then
    Exit(ThrowTypeError('Invalid property descriptor. Cannot both specify accessors and a value or writable attribute'));
  Result := True;
end;

function FromPropertyDescriptor(const D: TPropDesc; Found: Boolean): TValue;
var O: TValue;
begin
  if not Found then Exit(VUndefined);
  PushRoot(D.Value); PushRoot(D.Get); PushRoot(D.Set_);
  O := MkPtr(NewPlainObject(ObjectProto, 4));
  PushRoot(O);
  if D.HasValue then DefineOwn(O, AtomValue, RootAt(TempTop - 4), pfDefault);
  if D.HasWritable then DefineOwn(RootAt(TempTop - 1), AtomWritable, MkBool(D.Writable), pfDefault);
  if D.HasGet then DefineOwn(RootAt(TempTop - 1), AtomGet, RootAt(TempTop - 3), pfDefault);
  if D.HasSet then DefineOwn(RootAt(TempTop - 1), AtomSet, RootAt(TempTop - 2), pfDefault);
  if D.HasEnumerable then DefineOwn(RootAt(TempTop - 1), AtomEnumerable, MkBool(D.Enumerable), pfDefault);
  if D.HasConfigurable then DefineOwn(RootAt(TempTop - 1), AtomConfigurable, MkBool(D.Configurable), pfDefault);
  Result := RootAt(TempTop - 1);
  PopRoots(4);
end;

{ a complete descriptor from a shape slot }
procedure DescFromSlot(P: PObj; S: Integer; var D: TPropDesc);
var F: Byte; A: PAccessor;
begin
  EmptyDesc(D);
  F := P^.Shape^.PFlags[S];
  D.HasEnumerable := True; D.Enumerable := (F and pfEnumerable) <> 0;
  D.HasConfigurable := True; D.Configurable := (F and pfConfigurable) <> 0;
  if (F and pfAccessor) <> 0 then
  begin
    A := PAccessor(PtrOf(GetSlot(P, S)));
    D.HasGet := True; D.Get := A^.Getter;
    D.HasSet := True; D.Set_ := A^.Setter;
  end
  else
  begin
    D.HasValue := True; D.Value := GetSlot(P, S);
    D.HasWritable := True; D.Writable := (F and pfWritable) <> 0;
  end;
end;

procedure DataDesc(var D: TPropDesc; V: TValue; W, E, C: Boolean);
begin
  EmptyDesc(D);
  D.HasValue := True; D.Value := V;
  D.HasWritable := True; D.Writable := W;
  D.HasEnumerable := True; D.Enumerable := E;
  D.HasConfigurable := True; D.Configurable := C;
end;

{ 10.5.5 [[GetOwnProperty]] of a proxy, through its trap (the invariant checks against the target are not
  enforced yet) }
function IsExtensibleOM(O: TValue; var Ext: Boolean): Boolean; forward;
function Compatible(Extensible: Boolean; const D: TPropDesc; Found: Boolean; const Cur: TPropDesc): Boolean; forward;

function ProxyGetOwnProperty(PV: TValue; Key: PStr; var D: TPropDesc; var Found: Boolean): Boolean;
var Trap, R: TValue; TD: TPropDesc; TF, Ext: Boolean;
begin
  PushRoot(PV);
  if not ProxyGetTrap(PV, 'getOwnPropertyDescriptor', Trap) then begin PopRoots(1); Exit(False); end;
  PV := RootAt(TempTop - 1);
  PopRoots(1);
  if IsNullish(Trap) then Exit(GetOwnPropertyV(ObjOf(PV)^.Aux1, Key, D, Found));
  PushRoot(PV);
  if not ProxyCallTrap(PV, Trap, 1, KeyToValue(Key), VUndefined, VUndefined, R) then begin PopRoots(1); Exit(False); end;
  if (R <> VUndefined) and not IsObject(R) then
  begin PopRoots(1); Exit(ThrowTypeError('getOwnPropertyDescriptor trap returned neither object nor undefined')); end;
  PushRoot(R);
  { 10.5.5: the target's descriptor and extensibility bound what the trap may report }
  if not GetOwnPropertyV(ObjOf(RootAt(TempTop - 2))^.Aux1, Key, TD, TF) then begin PopRoots(2); Exit(False); end;
  if R = VUndefined then
  begin
    Found := False;
    if not TF then begin PopRoots(2); Exit(True); end;
    if not TD.Configurable then begin PopRoots(2); Exit(ThrowTypeError('proxy getOwnPropertyDescriptor reported a non-configurable property as absent')); end;
    if not IsExtensibleOM(ObjOf(RootAt(TempTop - 2))^.Aux1, Ext) then begin PopRoots(2); Exit(False); end;
    PopRoots(2);
    if not Ext then Exit(ThrowTypeError('proxy getOwnPropertyDescriptor reported an existing property of a non-extensible target as absent'));
    Exit(True);
  end;
  PushRoot(TD.Value); PushRoot(TD.Get); PushRoot(TD.Set_);
  if not IsExtensibleOM(ObjOf(RootAt(TempTop - 5))^.Aux1, Ext) then begin PopRoots(5); Exit(False); end;
  R := RootAt(TempTop - 4);
  Result := ToPropertyDescriptor(R, D);
  TD.Value := RootAt(TempTop - 3); TD.Get := RootAt(TempTop - 2); TD.Set_ := RootAt(TempTop - 1);
  PopRoots(5);
  { CompletePropertyDescriptor }
  if Result then
  begin
    Found := True;
    if not IsAccessorDesc(D) then
    begin
      if not D.HasValue then begin D.HasValue := True; D.Value := VUndefined; end;
      if not D.HasWritable then begin D.HasWritable := True; D.Writable := False; end;
    end
    else
    begin
      if not D.HasGet then begin D.HasGet := True; D.Get := VUndefined; end;
      if not D.HasSet then begin D.HasSet := True; D.Set_ := VUndefined; end;
    end;
    if not D.HasEnumerable then begin D.HasEnumerable := True; D.Enumerable := False; end;
    if not D.HasConfigurable then begin D.HasConfigurable := True; D.Configurable := False; end;
    if not Compatible(Ext, D, TF, TD) then Exit(ThrowTypeError('proxy getOwnPropertyDescriptor reported an incompatible descriptor'));
    if not D.Configurable then
    begin
      if not TF or TD.Configurable then Exit(ThrowTypeError('proxy getOwnPropertyDescriptor reported a non-configurable property the target lacks or has configurable'));
      if D.HasWritable and not D.Writable and TD.Writable then
        Exit(ThrowTypeError('proxy getOwnPropertyDescriptor reported a non-writable property the target has writable'));
    end;
  end;
end;

function GetOwnPropertyV0(O: TValue; Key: PStr; var D: TPropDesc; var Found: Boolean): Boolean; forward;

{ 10.4.4.1 a mapped arguments object's [[GetOwnProperty]]: a mapped index's value is the parameter's }
function GetOwnPropertyV(O: TValue; Key: PStr; var D: TPropDesc; var Found: Boolean): Boolean;
var U: PUpval; Idx: Int64;
begin
  Result := GetOwnPropertyV0(O, Key, D, Found);
  if Result and Found and IsObject(O) and (ObjOf(O)^.OKind = okArgumentsOrd) then
  begin
    U := ArgsMapCell(ObjOf(O), Key, Idx);
    if U <> nil then D.Value := ArgsMapGet(U);
  end;
end;

function GetOwnPropertyV0(O: TValue; Key: PStr; var D: TPropDesc; var Found: Boolean): Boolean;
var P: PObj; S: Integer; Idx: Int64; V: TValue; F: Byte;
begin
  Found := False;
  EmptyDesc(D);
  if not IsObject(O) then Exit(True);
  P := ObjOf(O);
  if P^.OKind = okProxy then Exit(ProxyGetOwnProperty(O, Key, D, Found));
  { 10.4.6.4 a namespace's export: writable, enumerable, not configurable (its value read now) }
  if (P^.OKind = okModuleNS) and ((Key^.Hdr.Extra and kfSymbol) = 0) then
  begin
    if not NSLookup(P, Key, True, V, Found) then Exit(False);
    if Found then DataDesc(D, V, True, True, False);
    Exit(True);
  end;
  Result := True;
  if (Key^.Hdr.Extra and (kfPrivate or kfInternal)) <> 0 then Exit;
  if (P^.OKind = okTypedArray) and IndexFromString(Key, Idx) then
  begin
    { 10.4.5.1: an integer index is a property exactly when it is in range }
    if Idx < TALength(P) then
    begin
      { an immutable buffer's elements are not writable or configurable }
      if ViewImmutable(P) then DataDesc(D, TAGet(P, Idx), False, True, False)
      else DataDesc(D, TAGet(P, Idx), True, True, True);
      Found := True;
    end;
    Exit;
  end;
  if (P^.OKind = okTypedArray) and CanonicalNumericNonIndex(Key) then Exit;
  S := ShapeLookup(P^.Shape, Key);
  if S >= 0 then
  begin
    DescFromSlot(P, S, D);
    Found := True;
    Exit;
  end;
  if (P^.Elements <> nil) and IndexFromString(Key, Idx) and (Idx < P^.Elements^.Len) and (P^.Elements^.Data[Idx] <> VHole) then
  begin
    DataDesc(D, P^.Elements^.Data[Idx], True, True, True);
    Found := True;
    Exit;
  end;
  if (P^.OKind = okArray) and (Key = AtomLength) then
  begin
    DataDesc(D, MkNumber(ArrayLength(P)), (P^.OFlags and ofLenReadOnly) = 0, False, False);
    Found := True;
    Exit;
  end;
  if P^.OKind = okStringObj then
  begin
    if Key = AtomLength then begin DataDesc(D, MkInt(StrOf(P^.Aux1)^.Len), False, False, False); Found := True; Exit; end;
    if IndexFromString(Key, Idx) and (Idx < StrOf(P^.Aux1)^.Len) then
    begin
      DataDesc(D, SubString(StrOf(P^.Aux1), Idx, 1), False, True, False);
      Found := True;
      Exit;
    end;
    Exit;
  end;
  if OwnVirtual(P, Key, V, F) then
  begin
    { a function's prototype made on first use, and the remaining virtual properties }
    DataDesc(D, V, (F and pfWritable) <> 0, (F and pfEnumerable) <> 0, (F and pfConfigurable) <> 0);
    Found := True;
  end;
end;

function SameValueV(A, B: TValue): Boolean;
var X, Y: Double;
begin
  if IsNumber(A) and IsNumber(B) then
  begin
    X := NumOf(A); Y := NumOf(B);
    if IsNaN(X) or IsNaN(Y) then Exit(IsNaN(X) and IsNaN(Y));
    Exit((X = Y) and ((X <> 0) or ((PQWord(@X)^ shr 63) = (PQWord(@Y)^ shr 63))));
  end;
  Result := StrictEquals(A, B);
end;

{ 10.1.6.3 ValidateAndApplyPropertyDescriptor, steps 1-6 (the check against the current descriptor) }
function Compatible(Extensible: Boolean; const D: TPropDesc; Found: Boolean; const Cur: TPropDesc): Boolean;
begin
  if not Found then Exit(Extensible);
  if Cur.Configurable then Exit(True);
  { a non-configurable property }
  if D.HasConfigurable and D.Configurable then Exit(False);
  if D.HasEnumerable and (D.Enumerable <> Cur.Enumerable) then Exit(False);
  if IsAccessorDesc(D) or IsDataDesc(D) then
  begin
    if IsAccessorDesc(D) <> IsAccessorDesc(Cur) then Exit(False);
    if IsAccessorDesc(Cur) then
    begin
      if D.HasGet and not SameValueV(D.Get, Cur.Get) then Exit(False);
      if D.HasSet and not SameValueV(D.Set_, Cur.Set_) then Exit(False);
    end
    else if not Cur.Writable then
    begin
      if D.HasWritable and D.Writable then Exit(False);
      if D.HasValue and not SameValueV(D.Value, Cur.Value) then Exit(False);
    end;
  end;
  Result := True;
end;

{ the merged descriptor: what D gives, the rest from the current one (or the defaults) }
procedure MergeDesc(const D: TPropDesc; Found: Boolean; const Cur: TPropDesc; var M: TPropDesc);
var ToAccessor: Boolean;
begin
  EmptyDesc(M);
  if Found then ToAccessor := IsAccessorDesc(D) or (IsAccessorDesc(Cur) and not IsDataDesc(D))
  else ToAccessor := IsAccessorDesc(D);
  if D.HasEnumerable then M.Enumerable := D.Enumerable else if Found then M.Enumerable := Cur.Enumerable;
  if D.HasConfigurable then M.Configurable := D.Configurable else if Found then M.Configurable := Cur.Configurable;
  M.HasEnumerable := True; M.HasConfigurable := True;
  if ToAccessor then
  begin
    M.HasGet := True; M.HasSet := True;
    if D.HasGet then M.Get := D.Get else if Found and IsAccessorDesc(Cur) then M.Get := Cur.Get;
    if D.HasSet then M.Set_ := D.Set_ else if Found and IsAccessorDesc(Cur) then M.Set_ := Cur.Set_;
  end
  else
  begin
    M.HasValue := True; M.HasWritable := True;
    if D.HasValue then M.Value := D.Value else if Found and IsDataDesc(Cur) then M.Value := Cur.Value;
    if D.HasWritable then M.Writable := D.Writable else if Found and IsDataDesc(Cur) then M.Writable := Cur.Writable;
  end;
end;

function FlagsOf(const M: TPropDesc): Byte;
begin
  Result := Ord(M.Enumerable) * pfEnumerable or Ord(M.Configurable) * pfConfigurable;
  if M.HasGet then Result := Result or pfAccessor else Result := Result or Ord(M.Writable) * pfWritable;
end;

{ stores the complete descriptor M as an own property of P under Key }
procedure StoreProperty(O: TValue; Key: PStr; const M: TPropDesc);
var P: PObj; Idx: Int64; S: Integer; V, MV, MG, MS: TValue; Fl: Byte;
begin
  P := ObjOf(O);
  Fl := FlagsOf(M);
  MV := M.Value; MG := M.Get; MS := M.Set_;
  if (P^.Elements <> nil) and IndexFromString(Key, Idx) and (ShapeLookup(P^.Shape, Key) < 0) and
     (Fl = pfDefault) and ((P^.OKind <> okArray) or (Idx < P^.Elements^.Len) or
      (((P^.OFlags and ofBigLen) = 0) and ArrayDenseOK(P^.Elements, Idx))) then
  begin
    { a default data property on an index: an element }
    if DefineElemRaw(O, Idx, M.Value) then Exit;
  end;
  if (P^.Elements <> nil) and IndexFromString(Key, Idx) and (Idx < P^.Elements^.Len) then
  begin
    { the index moves from the elements to the shape }
    P^.Elements^.Data[Idx] := VHole;
    P^.OFlags := P^.OFlags or ofElemGuard;
  end;
  if (P^.OKind = okArray) and IndexFromString(Key, Idx) and (P^.Elements <> nil) and (Idx >= P^.Elements^.Len) and
     (Idx >= ArrayLength(P)) then
  begin
    { an index at or past the length extends it (10.4.2.1 step 1.k) }
    PushRoot(O); PushRoot(MV); PushRoot(MG); PushRoot(MS);
    if ((P^.OFlags and ofBigLen) = 0) and ArrayDenseOK(P^.Elements, Idx) then ArraySetLength(P, Idx + 1)
    else ArraySetLengthSparse(P, Idx + 1);
    O := RootAt(TempTop - 4);   { the elements' allocation may have moved the array and the values }
    MV := RootAt(TempTop - 3); MG := RootAt(TempTop - 2); MS := RootAt(TempTop - 1);
    P := ObjOf(O);
    if Idx < P^.Elements^.Len then P^.Elements^.Data[Idx] := VHole;
    P^.OFlags := P^.OFlags or ofElemGuard;
    PopRoots(4);
  end;
  if M.HasGet then
  begin
    PushRoot(O);
    V := MkPtr(NewAccessor(MG, MS));
    DefineOwn(RootAt(TempTop - 1), Key, V, Fl);
    PopRoots(1);
  end
  else
  begin
    S := ShapeLookup(P^.Shape, Key);
    DefineOwn(O, Key, MV, Fl);
  end;
end;

{ 10.4.2.4 ArraySetLength with a descriptor }
function ArrayDefineLength(O: TValue; const D: TPropDesc; var Ok: Boolean): Boolean;
var P: PObj; NewLen, OldLen, I: Int64; D1, D2: Double; Cur: TPropDesc; Found: Boolean; S: Integer; K: PStr;
  ReadOnly: Boolean;
begin
  Ok := True;
  P := ObjOf(O);
  if not D.HasValue then
  begin
    { only attributes: length is writable-only-to-false, never configurable or enumerable }
    if (D.HasConfigurable and D.Configurable) or (D.HasEnumerable and D.Enumerable) or IsAccessorDesc(D) then
      begin Ok := False; Exit(True); end;
    if D.HasWritable and D.Writable and ((P^.OFlags and ofLenReadOnly) <> 0) then begin Ok := False; Exit(True); end;
    if D.HasWritable and not D.Writable then P^.OFlags := P^.OFlags or ofLenReadOnly or ofElemGuard;
    Exit(True);
  end;
  PushRoot(O);
  if not ToNumberV(D.Value, D1) or not ToNumberV(D.Value, D2) then begin PopRoots(1); Exit(False); end;
  O := RootAt(TempTop - 1);
  PopRoots(1);
  if IsNaN(D2) or (D2 < 0) or (D2 > 4294967295.0) or (Frac(D2) <> 0) or (D1 <> D2) then
    Exit(ThrowError('RangeError', 'Invalid array length'));
  P := ObjOf(O);
  if (D.HasConfigurable and D.Configurable) or (D.HasEnumerable and D.Enumerable) or IsAccessorDesc(D) then
    begin Ok := False; Exit(True); end;
  NewLen := Trunc(D2);
  OldLen := ArrayLength(P);
  ReadOnly := (P^.OFlags and ofLenReadOnly) <> 0;
  if ReadOnly then
  begin
    if D.HasWritable and D.Writable then begin Ok := False; Exit(True); end;
    Ok := NewLen = OldLen;
    Exit(True);
  end;
  if NewLen < OldLen then
  begin
    { the indices from the end down: a non-configurable one stops the deletion }
    I := ArrayDropShapeIndices(P, NewLen);
    if I > NewLen then
    begin
      ArraySetLengthAny(P, I);
      if D.HasWritable and not D.Writable then P^.OFlags := P^.OFlags or ofLenReadOnly or ofElemGuard;
      Ok := False;
      Exit(True);
    end;
  end;
  ArraySetLengthAny(P, NewLen);
  if D.HasWritable and not D.Writable then P^.OFlags := P^.OFlags or ofLenReadOnly or ofElemGuard;
  Result := True;
end;

{ 10.5.6 [[DefineOwnProperty]] of a proxy, through its trap }
function ProxyDefineOwnProperty(PV: TValue; Key: PStr; const D: TPropDesc; var Ok: Boolean): Boolean;
var Trap, R, DO_: TValue; TD, DD: TPropDesc; TF, Ext, SettingConfigFalse: Boolean;
begin
  PushRoot(PV);
  if not ProxyGetTrap(PV, 'defineProperty', Trap) then begin PopRoots(1); Exit(False); end;
  PV := RootAt(TempTop - 1);
  if IsNullish(Trap) then
  begin
    PopRoots(1);
    Exit(DefineOwnPropertyV(ObjOf(PV)^.Aux1, Key, D, Ok));
  end;
  PushRoot(Trap);
  PushRoot(D.Value); PushRoot(D.Get); PushRoot(D.Set_);
  DO_ := FromPropertyDescriptor(D, True);
  if not ProxyCallTrap(RootAt(TempTop - 5), RootAt(TempTop - 4), 2, KeyToValue(Key), DO_, VUndefined, R) then
  begin PopRoots(5); Exit(False); end;
  Ok := ToBool(R);
  if not Ok then begin PopRoots(5); Exit(True); end;
  { 10.5.6 steps 9-16: the trap cannot report a definition the target contradicts }
  if not GetOwnPropertyV(ObjOf(RootAt(TempTop - 5))^.Aux1, Key, TD, TF) then begin PopRoots(5); Exit(False); end;
  PushRoot(TD.Value); PushRoot(TD.Get); PushRoot(TD.Set_);
  if not IsExtensibleOM(ObjOf(RootAt(TempTop - 8))^.Aux1, Ext) then begin PopRoots(8); Exit(False); end;
  TD.Value := RootAt(TempTop - 3); TD.Get := RootAt(TempTop - 2); TD.Set_ := RootAt(TempTop - 1);
  DD := D;
  DD.Value := RootAt(TempTop - 6); DD.Get := RootAt(TempTop - 5); DD.Set_ := RootAt(TempTop - 4);
  PopRoots(8);
  SettingConfigFalse := DD.HasConfigurable and not DD.Configurable;
  if not TF then
  begin
    if not Ext then Exit(ThrowTypeError('proxy defineProperty trap added a property to a non-extensible target'));
    if SettingConfigFalse then Exit(ThrowTypeError('proxy defineProperty trap defined a non-configurable property the target lacks'));
  end
  else
  begin
    if not Compatible(Ext, DD, True, TD) then Exit(ThrowTypeError('proxy defineProperty trap result is incompatible with the target property'));
    if SettingConfigFalse and TD.Configurable then
      Exit(ThrowTypeError('proxy defineProperty trap defined a non-configurable property the target has configurable'));
    if not IsAccessorDesc(TD) and not TD.Configurable and TD.Writable and DD.HasWritable and not DD.Writable then
      Exit(ThrowTypeError('proxy defineProperty trap made a non-configurable writable property non-writable'));
  end;
  Result := True;
end;


{ CreateDataHook: 7.3.7 CreateDataPropertyOrThrow on an exotic object }
function CreateDataOM(O: TValue; Key: PStr; V: TValue): Boolean;
var D: TPropDesc; Ok: Boolean;
begin
  DataDesc(D, V, True, True, True);
  if not DefineOwnPropertyV(O, Key, D, Ok) then Exit(False);
  if not Ok then Exit(ThrowTypeError('Cannot define property ' + StrToUnicode(Key)));
  Result := True;
end;

function DefineOwnPropertyV0(O: TValue; Key: PStr; const DIn: TPropDesc; var Ok: Boolean): Boolean; forward;

{ 10.4.4.2 a mapped arguments object's [[DefineOwnProperty]]: an accessor, or a non-writable data property, ends
  the index's mapping; a value given goes to the parameter too; made non-writable without a value, it keeps the
  parameter's }
function DefineOwnPropertyV(O: TValue; Key: PStr; const DIn: TPropDesc; var Ok: Boolean): Boolean;
var U: PUpval; Idx: Int64; D: TPropDesc;
begin
  if not IsObject(O) or (ObjOf(O)^.OKind <> okArgumentsOrd) then Exit(DefineOwnPropertyV0(O, Key, DIn, Ok));
  U := ArgsMapCell(ObjOf(O), Key, Idx);
  if U = nil then Exit(DefineOwnPropertyV0(O, Key, DIn, Ok));
  D := DIn;
  if not IsAccessorDesc(D) and not D.HasValue and D.HasWritable and not D.Writable then
  begin
    D.HasValue := True;
    D.Value := ArgsMapGet(U);
  end;
  PushRoot(O); PushRoot(D.Value);
  Result := DefineOwnPropertyV0(O, Key, D, Ok);
  D.Value := RootAt(TempTop - 1); O := RootAt(TempTop - 2);
  PopRoots(2);
  if not Result or not Ok then Exit;
  U := ArgsMapCell(ObjOf(O), Key, Idx);
  if U = nil then Exit;
  if IsAccessorDesc(DIn) then ArgsUnmap(ObjOf(O), Idx)
  else
  begin
    if DIn.HasValue then ArgsMapSet(U, D.Value);
    if DIn.HasWritable and not DIn.Writable then ArgsUnmap(ObjOf(O), Idx);
  end;
end;

function DefineOwnPropertyV0(O: TValue; Key: PStr; const DIn: TPropDesc; var Ok: Boolean): Boolean;
var P: PObj; Cur, M, D: TPropDesc; Found: Boolean; Idx: Int64; Ext: Boolean; Dbl: Double;
begin
  D := DIn;
  Ok := False;
  if not IsObject(O) then Exit(ThrowTypeError('Object.defineProperty called on non-object'));
  P := ObjOf(O);
  if P^.OKind = okProxy then Exit(ProxyDefineOwnProperty(O, Key, D, Ok));
  { 10.4.6.6 a namespace's string key: no change but to what it already is }
  if (P^.OKind = okModuleNS) and ((Key^.Hdr.Extra and kfSymbol) = 0) then
  begin
    if not GetOwnPropertyV(O, Key, Cur, Found) then Exit(False);
    if not Found then Exit(True);
    if (D.HasConfigurable and D.Configurable) or (D.HasEnumerable and not D.Enumerable) or IsAccessorDesc(D) or
       (D.HasWritable and not D.Writable) then Exit(True);
    Ok := not D.HasValue or SameValueV(D.Value, Cur.Value);
    Exit(True);
  end;
  if P^.OKind = okArguments then
  begin
    PushRoot(D.Value); PushRoot(D.Get); PushRoot(D.Set_);
    O := DemoteArguments(O);
    D.Value := RootAt(TempTop - 3); D.Get := RootAt(TempTop - 2); D.Set_ := RootAt(TempTop - 1);
    PopRoots(3);
    P := ObjOf(O);
  end;
  if (P^.OKind = okTypedArray) and IndexFromString(Key, Idx) then
  begin
    { 10.4.5.3 }
    if Idx >= TALength(P) then Exit(True);
    if ViewImmutable(P) then
    begin
      { ValidateAndApplyPropertyDescriptor against the current non-writable, non-configurable element }
      if (D.HasConfigurable and D.Configurable) or (D.HasEnumerable and not D.Enumerable) or IsAccessorDesc(D) or
         (D.HasWritable and D.Writable) or (D.HasValue and not SameValueV(D.Value, TAGet(P, Idx))) then Exit(True);
      Ok := True;
      Exit(True);
    end;
    if (D.HasConfigurable and not D.Configurable) or (D.HasEnumerable and not D.Enumerable) or IsAccessorDesc(D) or
       (D.HasWritable and not D.Writable) then Exit(True);
    if D.HasValue and not TAStoreValue(O, Idx, D.Value) then Exit(False);
    Ok := True;
    Exit(True);
  end;
  if (P^.OKind = okTypedArray) and CanonicalNumericNonIndex(Key) then Exit(True);
  if (P^.OKind = okArray) and (Key = AtomLength) then Exit(ArrayDefineLength(O, D, Ok));
  PushRoot(O);
  if not GetOwnPropertyV(O, Key, Cur, Found) then begin PopRoots(1); Exit(False); end;
  O := RootAt(TempTop - 1);
  P := ObjOf(O);
  Ext := (P^.OFlags and 8) = 0;
  if (P^.OKind = okArray) and IndexFromString(Key, Idx) and not Found and ((P^.OFlags and ofLenReadOnly) <> 0) and
     (Idx >= ArrayLength(P)) then
  begin
    { 10.4.2.1: an index past a read-only length }
    PopRoots(1);
    Exit(True);
  end;
  if not Compatible(Ext, D, Found, Cur) then begin PopRoots(1); Exit(True); end;
  if (P^.OKind = okStringObj) and Found and not Cur.Configurable then
  begin
    { a String object's characters and length are immutable: Compatible held, nothing changes }
    PopRoots(1);
    Ok := True;
    Exit(True);
  end;
  MergeDesc(D, Found, Cur, M);
  PushRoot(M.Value); PushRoot(M.Get); PushRoot(M.Set_);
  StoreProperty(RootAt(TempTop - 4), Key, M);
  PopRoots(4);
  Ok := True;
  Result := True;
end;

function DefinePropertyOrThrow(O: TValue; Key: PStr; const D: TPropDesc): Boolean;
var Ok: Boolean;
begin
  if not DefineOwnPropertyV(O, Key, D, Ok) then Exit(False);
  if not Ok then Exit(ThrowTypeError('Cannot redefine property: ' + StrToUnicode(Key)));
  Result := True;
end;

function PreventExtensionsV(O: TValue; var Ok: Boolean): Boolean;
var Trap, R: TValue; Ext: Boolean;
begin
  Ok := True;
  if not IsObject(O) then Exit(True);
  if ObjOf(O)^.OKind = okProxy then
  begin
    PushRoot(O);
    if not ProxyGetTrap(O, 'preventExtensions', Trap) then begin PopRoots(1); Exit(False); end;
    O := RootAt(TempTop - 1);
    PopRoots(1);
    if IsNullish(Trap) then Exit(PreventExtensionsV(ObjOf(O)^.Aux1, Ok));
    PushRoot(O);
    if not ProxyCallTrap(O, Trap, 0, VUndefined, VUndefined, VUndefined, R) then begin PopRoots(1); Exit(False); end;
    Ok := ToBool(R);
    { 10.5.4 step 8: true only for a target that is no longer extensible }
    if Ok then
    begin
      if not IsExtensibleOM(ObjOf(RootAt(TempTop - 1))^.Aux1, Ext) then begin PopRoots(1); Exit(False); end;
      if Ext then begin PopRoots(1); Exit(ThrowTypeError('proxy preventExtensions trap returned true but the target is extensible')); end;
    end;
    PopRoots(1);
    Exit(True);
  end;
  { 10.4.5.? a typed array's [[PreventExtensions]]: false unless it is fixed length (IsTypedArrayFixedLength: not
    length-tracking, and its buffer fixed length or shared) }
  if (ObjOf(O)^.OKind = okTypedArray) and
     ((Int32(UInt32(ObjOf(O)^.Aux2)) < 0) or
      ((ObjOf(ObjOf(O)^.Aux1)^.OFlags and (bfResizable or bfShared)) = bfResizable)) then
  begin
    Ok := False;
    Exit(True);
  end;
  if (ObjOf(O)^.OFlags and 8) = 0 then
    { a new shape: store caches holding an add transition from the old one must miss }
    ObjOf(O)^.Shape := ShapeFork(ObjOf(O)^.Shape);
  ObjOf(O)^.OFlags := ObjOf(O)^.OFlags or 8;
  if ObjOf(O)^.Elements <> nil then ObjOf(O)^.OFlags := ObjOf(O)^.OFlags or ofElemGuard;
  Result := True;
end;

{ [[IsExtensible]] }
function IsExtensibleOM(O: TValue; var Ext: Boolean): Boolean;
var Trap, R: TValue; TE: Boolean;
begin
  Ext := False;
  if not IsObject(O) then Exit(True);
  if ObjOf(O)^.OKind = okProxy then
  begin
    PushRoot(O);
    if not ProxyGetTrap(O, 'isExtensible', Trap) then begin PopRoots(1); Exit(False); end;
    O := RootAt(TempTop - 1);
    PopRoots(1);
    if IsNullish(Trap) then Exit(IsExtensibleOM(ObjOf(O)^.Aux1, Ext));
    PushRoot(O);
    if not ProxyCallTrap(O, Trap, 0, VUndefined, VUndefined, VUndefined, R) then begin PopRoots(1); Exit(False); end;
    { 10.5.3 step 8: the trap must agree with the target }
    if not IsExtensibleOM(ObjOf(RootAt(TempTop - 1))^.Aux1, TE) then begin PopRoots(1); Exit(False); end;
    PopRoots(1);
    Ext := ToBool(R);
    if Ext <> TE then Exit(ThrowTypeError('proxy isExtensible trap result does not reflect the target'));
    Exit(True);
  end;
  Ext := (ObjOf(O)^.OFlags and 8) = 0;
  Result := True;
end;

function EnumerableOwn(O: TValue; Kind: Integer; var Res: TValue): Boolean; forward;

{ a proxy's string keys, through its ownKeys trap }
function Obj_StringKeys(PV: TValue; var Res: TValue): Boolean;
var Keys, KV: TValue; I, N: Integer;
begin
  if not OwnPropertyKeysV(PV, Keys) then Exit(False);
  PushRoot(Keys);
  PushRoot(MkPtr(NewArray(0, 4)));
  N := ArrayLen(ObjOf(RootAt(TempTop - 2)));   { Keys moved if the allocation above collected }
  for I := 0 to N - 1 do
  begin
    KV := ObjOf(RootAt(TempTop - 2))^.Elements^.Data[I];
    if IsString(KV) then ArrayPush(ObjOf(RootAt(TempTop - 1)), KV);
  end;
  Res := RootAt(TempTop - 1);
  PopRoots(2);
  Result := True;
end;

{ 10.5.8 / 10.5.7 / 10.5.10 / 10.5.11: what a proxy trap may report given its target (G2.VM's ProxyCheckHook) }
function ProxyCheckOM(Kind: Integer; PV: TValue; Key: PStr; var Res: TValue): Boolean;
var D: TPropDesc; Found, Ext: Boolean; Len, I, J: Int64; V, TK: TValue; K: PStr; Keys: array of PStr;
  Used: array of Boolean; NonCfg: array of PStr; Cfg: array of PStr; N: Integer;
begin
  Result := True;
  case Kind of
    0, 1, 2:
      begin
        PushRoot(PV); PushRoot(Res);
        if not GetOwnPropertyV(ObjOf(PV)^.Aux1, Key, D, Found) then begin PopRoots(2); Exit(False); end;
        if Kind = 0 then
        begin
          Res := RootAt(TempTop - 1);
          PopRoots(2);
          if Found and not D.Configurable then
          begin
            if not IsAccessorDesc(D) and not D.Writable and not SameValueV(Res, D.Value) then
              Exit(ThrowTypeError('proxy get trap result differs from a non-writable, non-configurable property'));
            if IsAccessorDesc(D) and (D.Get = VUndefined) and (Res <> VUndefined) then
              Exit(ThrowTypeError('proxy get trap returned a value for a non-configurable accessor without a getter'));
          end;
          Exit(True);
        end;
        if not Found then begin PopRoots(2); Exit(True); end;
        if not D.Configurable then
        begin
          PopRoots(2);
          if Kind = 1 then Exit(ThrowTypeError('proxy has trap reported a non-configurable property as absent'));
          Exit(ThrowTypeError('proxy deleteProperty trap deleted a non-configurable property'));
        end;
        if not IsExtensibleOM(ObjOf(RootAt(TempTop - 2))^.Aux1, Ext) then begin PopRoots(2); Exit(False); end;
        PopRoots(2);
        if not Ext then
        begin
          if Kind = 1 then Exit(ThrowTypeError('proxy has trap reported an existing property of a non-extensible target as absent'));
          Exit(ThrowTypeError('proxy deleteProperty trap deleted a property of a non-extensible target'));
        end;
      end;
    4: Exit(OwnPropertyKeysV(ObjOf(PV)^.Aux1, Res));
    5: Exit(Obj_StringKeys(PV, Res));
    6: Exit(EnumerableOwn(PV, 0, Res));
    3:
      begin
        { CreateListFromArrayLike(trapResultArray, « String, Symbol ») into a fresh array of atoms }
        if not IsObject(Res) then Exit(ThrowTypeError('proxy ownKeys trap result is not an object'));
        PushRoot(PV); PushRoot(Res);
        if not LengthOfArrayLike(Res, Len) then begin PopRoots(2); Exit(False); end;
        PushRoot(MkPtr(NewArray(0, 8)));
        SetLength(Keys, 0);
        I := 0;
        while I < Len do
        begin
          if not GetV(RootAt(TempTop - 2), AtomOf(UnicodeString(IntToStr(I))), V) then begin PopRoots(3); Exit(False); end;
          if not IsString(V) and not (IsPtr(V) and (PCell(PtrOf(V))^.Kind = ckSymbol)) then
          begin PopRoots(3); Exit(ThrowTypeError('proxy ownKeys trap result contains a value that is neither string nor symbol')); end;
          if not ToPropKey(V, K) then begin PopRoots(3); Exit(False); end;
          for J := 0 to High(Keys) do
            if Keys[J] = K then begin PopRoots(3); Exit(ThrowTypeError('proxy ownKeys trap result contains a duplicate key')); end;
          SetLength(Keys, Length(Keys) + 1);
          Keys[High(Keys)] := K;
          TK := KeyToValue(K);
          ArrayPush(ObjOf(RootAt(TempTop - 1)), TK);
          Inc(I);
        end;
        { the target's keys, split by configurability }
        if not IsExtensibleOM(ObjOf(RootAt(TempTop - 3))^.Aux1, Ext) then begin PopRoots(3); Exit(False); end;
        if not OwnPropertyKeysV(ObjOf(RootAt(TempTop - 3))^.Aux1, TK) then begin PopRoots(3); Exit(False); end;
        PushRoot(TK);
        SetLength(NonCfg, 0); SetLength(Cfg, 0);
        N := ArrayLen(ObjOf(TK));
        for I := 0 to N - 1 do
        begin
          V := ObjOf(RootAt(TempTop - 1))^.Elements^.Data[I];
          if not ToPropKey(V, K) then begin PopRoots(4); Exit(False); end;
          if not GetOwnPropertyV(ObjOf(RootAt(TempTop - 4))^.Aux1, K, D, Found) then begin PopRoots(4); Exit(False); end;
          if Found and not D.Configurable then
          begin SetLength(NonCfg, Length(NonCfg) + 1); NonCfg[High(NonCfg)] := K; end
          else
          begin SetLength(Cfg, Length(Cfg) + 1); Cfg[High(Cfg)] := K; end;
        end;
        Res := RootAt(TempTop - 2);
        PopRoots(4);
        if Ext and (Length(NonCfg) = 0) then Exit(True);
        SetLength(Used, Length(Keys));
        for I := 0 to High(Used) do Used[I] := False;
        for I := 0 to High(NonCfg) do
        begin
          J := 0;
          while (J <= High(Keys)) and (Keys[J] <> NonCfg[I]) do Inc(J);
          if J > High(Keys) then Exit(ThrowTypeError('proxy ownKeys trap result omits a non-configurable key of the target'));
          Used[J] := True;
        end;
        if Ext then Exit(True);
        for I := 0 to High(Cfg) do
        begin
          J := 0;
          while (J <= High(Keys)) and (Keys[J] <> Cfg[I]) do Inc(J);
          if J > High(Keys) then Exit(ThrowTypeError('proxy ownKeys trap result omits a key of the non-extensible target'));
          Used[J] := True;
        end;
        for I := 0 to High(Used) do
          if not Used[I] then Exit(ThrowTypeError('proxy ownKeys trap result adds a key to the non-extensible target'));
      end;
  end;
end;

function OwnPropertyKeysV(O: TValue; var Res: TValue): Boolean;
var Keys, Syms: TValue; I, N: Integer; A: PObj;
begin
  if ObjOf(O)^.OKind = okProxy then Exit(ProxyOwnKeys(O, Res));
  PushRoot(O);
  Keys := OwnKeys(ObjOf(O), False);
  PushRoot(Keys);
  { the symbol keys after the strings }
  A := ObjOf(RootAt(TempTop - 2));
  for I := 0 to A^.Shape^.Count - 1 do
    if (A^.Shape^.Keys[I]^.Hdr.Extra and kfSymbol) <> 0 then
    begin
      Syms := KeyToValue(A^.Shape^.Keys[I]);
      ArrayPush(ObjOf(RootAt(TempTop - 1)), Syms);
      A := ObjOf(RootAt(TempTop - 2));
    end;
  Res := RootAt(TempTop - 1);
  PopRoots(2);
  Result := True;
end;

function KeyOf(V: TValue): PStr;
begin
  if IsString(V) then Result := Atomize(StrOf(V)) else Result := SymKey(V);
end;

{ 7.3.25 CopyDataProperties(target, source, « »): every own enumerable property, symbols too, read with [[Get]]
  in [[OwnPropertyKeys]] order; a proxy's ownKeys, getOwnPropertyDescriptor and get traps run }
function CopyDataPropsOM(Target, Src: TValue): Boolean;
var Keys, V: TValue; I, N: Integer; K: PStr; D: TPropDesc; Found: Boolean;
begin
  if IsNullish(Src) then Exit(True);
  PushRoot(Target);
  if not ToObjectV(Src, Src) then begin PopRoots(1); Exit(False); end;
  PushRoot(Src);
  if not OwnPropertyKeysV(Src, Keys) then begin PopRoots(2); Exit(False); end;
  PushRoot(Keys);
  N := ArrayLen(ObjOf(Keys));
  for I := 0 to N - 1 do
  begin
    K := KeyOf(ObjOf(RootAt(TempTop - 1))^.Elements^.Data[I]);
    if not GetOwnPropertyV(RootAt(TempTop - 2), K, D, Found) then begin PopRoots(3); Exit(False); end;
    if not Found or not D.Enumerable then Continue;
    K := KeyOf(ObjOf(RootAt(TempTop - 1))^.Elements^.Data[I]);
    if not GetV(RootAt(TempTop - 2), K, V) then begin PopRoots(3); Exit(False); end;
    K := KeyOf(ObjOf(RootAt(TempTop - 1))^.Elements^.Data[I]);
    DefineOwn(RootAt(TempTop - 3), K, V, pfDefault);
  end;
  PopRoots(3);
  Result := True;
end;

{ CopyDataPropsExclHook: 7.3.25 CopyDataProperties with excludedItems; an excluded key is skipped before its
  [[GetOwnProperty]] }
function CopyDataPropsExclOM(Target, Src, Excl: TValue): Boolean;
var Keys, V: TValue; I, J, N: Integer; K, X: PStr; D: TPropDesc; Found, Skip: Boolean;
begin
  if IsNullish(Src) then Exit(True);
  PushRoot(Excl);
  PushRoot(Target);
  if not ToObjectV(Src, Src) then begin PopRoots(2); Exit(False); end;
  PushRoot(Src);
  if not OwnPropertyKeysV(Src, Keys) then begin PopRoots(3); Exit(False); end;
  PushRoot(Keys);
  N := ArrayLen(ObjOf(Keys));
  for I := 0 to N - 1 do
  begin
    K := KeyOf(ObjOf(RootAt(TempTop - 1))^.Elements^.Data[I]);
    Skip := False;
    for J := 0 to ArrayLen(ObjOf(RootAt(TempTop - 4))) - 1 do
    begin
      { an excluded key is a property key already (a string, a symbol, or an index as a number) }
      if not ToPropKey(ObjOf(RootAt(TempTop - 4))^.Elements^.Data[J], X) then begin PopRoots(4); Exit(False); end;
      K := KeyOf(ObjOf(RootAt(TempTop - 1))^.Elements^.Data[I]);
      if (X = K) or (((X^.Hdr.Extra and kfSymbol) = 0) and ((K^.Hdr.Extra and kfSymbol) = 0) and
         (StrCompare(X, K) = 0)) then
      begin
        Skip := True;
        Break;
      end;
    end;
    if Skip then Continue;
    if not GetOwnPropertyV(RootAt(TempTop - 2), K, D, Found) then begin PopRoots(4); Exit(False); end;
    if not Found or not D.Enumerable then Continue;
    K := KeyOf(ObjOf(RootAt(TempTop - 1))^.Elements^.Data[I]);
    if not GetV(RootAt(TempTop - 2), K, V) then begin PopRoots(4); Exit(False); end;
    K := KeyOf(ObjOf(RootAt(TempTop - 1))^.Elements^.Data[I]);
    DefineOwn(RootAt(TempTop - 3), K, V, pfDefault);
  end;
  PopRoots(4);
  Result := True;
end;

function SetIntegrityLevel(O: TValue; Frozen: Boolean; var Ok: Boolean): Boolean;
var Keys: TValue; I, N: Integer; K: PStr; D, Cur: TPropDesc; Found: Boolean;
begin
  PushRoot(O);
  if not PreventExtensionsV(O, Ok) then begin PopRoots(1); Exit(False); end;
  if not Ok then begin PopRoots(1); Exit(True); end;
  if not OwnPropertyKeysV(RootAt(TempTop - 1), Keys) then begin PopRoots(1); Exit(False); end;
  PushRoot(Keys);
  N := ArrayLen(ObjOf(Keys));
  for I := 0 to N - 1 do
  begin
    K := KeyOf(ObjOf(RootAt(TempTop - 1))^.Elements^.Data[I]);
    EmptyDesc(D);
    D.HasConfigurable := True; D.Configurable := False;
    if Frozen then
    begin
      if not GetOwnPropertyV(RootAt(TempTop - 2), K, Cur, Found) then begin PopRoots(2); Exit(False); end;
      if not Found then Continue;
      if not IsAccessorDesc(Cur) then begin D.HasWritable := True; D.Writable := False; end;
    end;
    if not DefinePropertyOrThrow(RootAt(TempTop - 2), K, D) then begin PopRoots(2); Exit(False); end;
  end;
  if (ObjOf(RootAt(TempTop - 2))^.OKind = okArray) and Frozen then
    ObjOf(RootAt(TempTop - 2))^.OFlags := ObjOf(RootAt(TempTop - 2))^.OFlags or ofLenReadOnly or ofElemGuard;
  PopRoots(2);
  Result := True;
end;

function TestIntegrityLevel(O: TValue; Frozen: Boolean; var Res: Boolean): Boolean;
var Keys: TValue; I, N: Integer; K: PStr; Cur: TPropDesc; Found, Ext: Boolean;
begin
  Res := False;
  PushRoot(O);
  if not IsExtensibleOM(O, Ext) then begin PopRoots(1); Exit(False); end;
  if Ext then begin PopRoots(1); Exit(True); end;
  if not OwnPropertyKeysV(RootAt(TempTop - 1), Keys) then begin PopRoots(1); Exit(False); end;
  PushRoot(Keys);
  N := ArrayLen(ObjOf(Keys));
  for I := 0 to N - 1 do
  begin
    K := KeyOf(ObjOf(RootAt(TempTop - 1))^.Elements^.Data[I]);
    if not GetOwnPropertyV(RootAt(TempTop - 2), K, Cur, Found) then begin PopRoots(2); Exit(False); end;
    if not Found then Continue;
    if Cur.Configurable then begin PopRoots(2); Exit(True); end;
    if Frozen and IsDataDesc(Cur) and Cur.Writable then begin PopRoots(2); Exit(True); end;
  end;
  PopRoots(2);
  Res := True;
  Result := True;
end;

type
  TArgs = PValue;

{ 7.1.18 ToObject }
function ToObjectV(V: TValue; var O: TValue): Boolean;
var P: PObj; Proto: TValue; K: Byte;
begin
  if IsObject(V) then begin O := V; Exit(True); end;
  if IsNullish(V) then Exit(ThrowTypeError('Cannot convert undefined or null to object'));
  if IsString(V) then begin Proto := StringProto; K := okStringObj; end
  else if IsNumber(V) then begin Proto := NumberProto; K := okNumber; end
  else if (V = VTrue) or (V = VFalse) then begin Proto := BooleanProto; K := okBoolean; end
  else if IsPtr(V) and (PCell(PtrOf(V))^.Kind = ckSymbol) then begin Proto := SymbolProto; K := okSymbolObj; end
  else begin Proto := BigIntProto; K := okPlain; end;
  PushRoot(V);
  P := NewObject(RootShapeFor(Proto), 0, K);
  P^.Aux1 := RootAt(TempTop - 1);
  PopRoots(1);
  O := MkPtr(P);
  Result := True;
end;

{ [[GetPrototypeOf]] }
function Object_GetPrototypeOfV(O: TValue; var P: TValue): Boolean;
var Trap, TP: TValue; Ext: Boolean;
begin
  if ObjOf(O)^.OKind = okProxy then
  begin
    PushRoot(O);
    if not ProxyGetTrap(O, 'getPrototypeOf', Trap) then begin PopRoots(1); Exit(False); end;
    O := RootAt(TempTop - 1);
    PopRoots(1);
    if IsNullish(Trap) then Exit(Object_GetPrototypeOfV(ObjOf(O)^.Aux1, P));
    PushRoot(O);
    if not ProxyCallTrap(O, Trap, 0, VUndefined, VUndefined, VUndefined, P) then begin PopRoots(1); Exit(False); end;
    { 10.5.1 steps 8-13 }
    if not IsObject(P) and (P <> VNull) then begin PopRoots(1); Exit(ThrowTypeError('proxy getPrototypeOf trap returned neither object nor null')); end;
    PushRoot(P);
    if not IsExtensibleOM(ObjOf(RootAt(TempTop - 2))^.Aux1, Ext) then begin PopRoots(2); Exit(False); end;
    if not Ext then
    begin
      if not Object_GetPrototypeOfV(ObjOf(RootAt(TempTop - 2))^.Aux1, TP) then begin PopRoots(2); Exit(False); end;
      if TP <> RootAt(TempTop - 1) then begin PopRoots(2); Exit(ThrowTypeError('proxy getPrototypeOf trap result differs from a non-extensible target''s prototype')); end;
    end;
    P := RootAt(TempTop - 1);
    PopRoots(2);
    Exit(True);
  end;
  P := ObjOf(O)^.Shape^.Proto;
  Result := True;
end;

function Arg(Args: TArgs; Argc, I: Integer): TValue; inline;
begin
  if I <= Argc then Result := Args[I] else Result := VUndefined;
end;

function ProxySetOM(PV: TValue; Key: PStr; V, Receiver: TValue; var Ok: Boolean): Boolean; forward;

{ 10.1.9.2 OrdinarySetWithOwnDescriptor with an explicit receiver (Reflect.set) }
function SetWithReceiver(O: TValue; Key: PStr; V, Receiver: TValue; var Ok: Boolean): Boolean;
var D, Ex: TPropDesc; Found, FoundEx: Boolean; P, R: TValue; ND: TPropDesc; Idx: Int64;
begin
  if ObjOf(O)^.OKind = okProxy then Exit(ProxySetOM(O, Key, V, Receiver, Ok));
  { 10.4.4.4 a mapped arguments object as its own receiver: a mapped index's parameter too }
  if (ObjOf(O)^.OKind = okArgumentsOrd) and (O = Receiver) and (ArgsMapCell(ObjOf(O), Key, Idx) <> nil) then
    ArgsMapSet(ArgsMapCell(ObjOf(O), Key, Idx), V);
  { 10.4.6.9 a namespace's [[Set]]: false }
  if ObjOf(O)^.OKind = okModuleNS then begin Ok := False; Exit(True); end;
  { 10.4.5.5 a typed array's [[Set]] of a canonical numeric key: its own receiver stores (TypedArraySetElement,
    true); another receiver gets nothing for an invalid index (true), OrdinarySet for a valid one }
  if (ObjOf(O)^.OKind = okTypedArray) and (CanonicalNumericNonIndex(Key) or IndexFromString(Key, Idx)) then
  begin
    if ViewImmutable(ObjOf(O)) then begin Ok := False; Exit(True); end;
    if O = Receiver then
    begin
      Ok := True;
      if CanonicalNumericNonIndex(Key) then Exit(TAStoreValue(O, -1, V));
      Exit(TAStoreValue(O, Idx, V));
    end;
    if CanonicalNumericNonIndex(Key) or TAOutOfBounds(ObjOf(O)) or (Idx >= TALength(ObjOf(O))) then
    begin
      Ok := True;
      Exit(True);
    end;
  end;
  Ok := False;
  PushRoot(O); PushRoot(V); PushRoot(Receiver);
  Result := False;
  if not GetOwnPropertyV(O, Key, D, Found) then begin PopRoots(3); Exit; end;
  if not Found then
  begin
    if not Object_GetPrototypeOfV(RootAt(TempTop - 3), P) then begin PopRoots(3); Exit; end;
    if IsObject(P) then
    begin
      Result := SetWithReceiver(P, Key, RootAt(TempTop - 2), RootAt(TempTop - 1), Ok);
      PopRoots(3);
      Exit;
    end;
    DataDesc(D, VUndefined, True, True, True);
  end;
  if IsDataDesc(D) or not IsAccessorDesc(D) then
  begin
    if not D.Writable then begin PopRoots(3); Exit(True); end;
    if not IsObject(RootAt(TempTop - 1)) then begin PopRoots(3); Exit(True); end;
    if not GetOwnPropertyV(RootAt(TempTop - 1), Key, Ex, FoundEx) then begin PopRoots(3); Exit; end;
    EmptyDesc(ND);
    ND.HasValue := True; ND.Value := RootAt(TempTop - 2);
    if FoundEx then
    begin
      if IsAccessorDesc(Ex) or not Ex.Writable then begin PopRoots(3); Exit(True); end;
    end
    else
    begin
      ND.HasWritable := True; ND.Writable := True;
      ND.HasEnumerable := True; ND.Enumerable := True;
      ND.HasConfigurable := True; ND.Configurable := True;
    end;
    Result := DefineOwnPropertyV(RootAt(TempTop - 1), Key, ND, Ok);
    PopRoots(3);
    Exit;
  end;
  if not IsCallable(D.Set_) then begin PopRoots(3); Exit(True); end;
  V := RootAt(TempTop - 2);
  Result := CallValue(D.Set_, RootAt(TempTop - 1), @V, 1, R);
  Ok := Result;
  PopRoots(3);
end;

{ 10.5.9 Proxy [[Set]] (P, V, Receiver) }
function ProxySetOM(PV: TValue; Key: PStr; V, Receiver: TValue; var Ok: Boolean): Boolean;
var Trap, R: TValue; D: TPropDesc; Found: Boolean; Base: Integer;
begin
  Ok := False;
  Result := False;
  Base := TempTop;
  PushRoot(PV); PushRoot(V); PushRoot(Receiver);
  if not ProxyGetTrap(PV, 'set', Trap) then begin PopRoots(3); Exit; end;   { a revoked proxy throws here }
  PushRoot(ObjOf(RootAt(Base))^.Aux1);   { the target, Base + 3 }
  if IsNullish(Trap) then
  begin
    Result := SetWithReceiver(RootAt(Base + 3), Key, RootAt(Base + 1), RootAt(Base + 2), Ok);
    PopRoots(4);
    Exit;
  end;
  if not ProxyCallTrap(RootAt(Base), Trap, 3, KeyToValue(Key), RootAt(Base + 1), RootAt(Base + 2), R) then
  begin PopRoots(4); Exit; end;
  if not ToBool(R) then begin PopRoots(4); Exit(True); end;
  if not GetOwnPropertyV(RootAt(Base + 3), Key, D, Found) then begin PopRoots(4); Exit; end;
  if Found and not D.Configurable then
  begin
    if IsDataDesc(D) and not D.Writable and not SameValueV(RootAt(Base + 1), D.Value) then
    begin
      PopRoots(4);
      Exit(ThrowTypeError('proxy set trap: the target''s non-writable, non-configurable property has a different value'));
    end;
    if IsAccessorDesc(D) and IsNullish(D.Set_) then
    begin
      PopRoots(4);
      Exit(ThrowTypeError('proxy set trap: the target''s non-configurable accessor has no setter'));
    end;
  end;
  PopRoots(4);
  Ok := True;
  Result := True;
end;

function Obj_DefineProperty(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var K: PStr; D: TPropDesc;
begin
  if not IsObject(Arg(Args, Argc, 1)) then Exit(ThrowTypeError('Object.defineProperty called on non-object'));
  if not ToPropKey(Arg(Args, Argc, 2), K) then Exit(False);
  if not ToPropertyDescriptor(Arg(Args, Argc, 3), D) then Exit(False);
  if not DefinePropertyOrThrow(Args[1], K, D) then Exit(False);
  Res := Args[1];
  Result := True;
end;

function Reflect_DefineProperty(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var K: PStr; D: TPropDesc; Ok: Boolean;
begin
  if not IsObject(Arg(Args, Argc, 1)) then Exit(ThrowTypeError('Reflect.defineProperty called on non-object'));
  if not ToPropKey(Arg(Args, Argc, 2), K) then Exit(False);
  if not ToPropertyDescriptor(Arg(Args, Argc, 3), D) then Exit(False);
  if not DefineOwnPropertyV(Args[1], K, D, Ok) then Exit(False);
  Res := MkBool(Ok);
  Result := True;
end;

{ 20.1.2.3.1 ObjectDefineProperties }
function ObjectDefineProperties(O, Props: TValue): Boolean;
var P, Keys, DV: TValue; I, N, J: Integer; K: PStr; Cur: TPropDesc; Found: Boolean;
  Descs: array of TPropDesc; DKeys: array of PStr; Base: Integer;
begin
  Base := TempTop;   { O, P, Keys sit at Base..Base+2; the loop pushes three roots per descriptor }
  PushRoot(O);
  Result := False;
  if not ToObjectV(Props, P) then begin PopRoots(1); Exit; end;
  PushRoot(P);
  if not OwnPropertyKeysV(P, Keys) then begin PopRoots(2); Exit; end;
  PushRoot(Keys);
  N := ArrayLen(ObjOf(Keys));
  SetLength(Descs, N); SetLength(DKeys, N);
  J := 0;
  for I := 0 to N - 1 do
  begin
    K := KeyOf(ObjOf(RootAt(Base + 2))^.Elements^.Data[I]);
    if not GetOwnPropertyV(RootAt(Base + 1), K, Cur, Found) then begin PopRoots(3 + J * 3); Exit; end;
    if not Found or not Cur.Enumerable then Continue;
    if not GetV(RootAt(Base + 1), K, DV) then begin PopRoots(3 + J * 3); Exit; end;
    if not ToPropertyDescriptor(DV, Descs[J]) then begin PopRoots(3 + J * 3); Exit; end;
    PushRoot(Descs[J].Value); PushRoot(Descs[J].Get); PushRoot(Descs[J].Set_);
    DKeys[J] := K;
    Inc(J);
  end;
  for I := 0 to J - 1 do
  begin
    Descs[I].Value := RootAt(TempTop - (J - I) * 3);
    Descs[I].Get := RootAt(TempTop - (J - I) * 3 + 1);
    Descs[I].Set_ := RootAt(TempTop - (J - I) * 3 + 2);
    if not DefinePropertyOrThrow(RootAt(Base), DKeys[I], Descs[I]) then begin PopRoots(3 + J * 3); Exit; end;
  end;
  PopRoots(3 + J * 3);
  Result := True;
end;

function Obj_DefineProperties(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
begin
  if not IsObject(Arg(Args, Argc, 1)) then Exit(ThrowTypeError('Object.defineProperties called on non-object'));
  if not ObjectDefineProperties(Args[1], Arg(Args, Argc, 2)) then Exit(False);
  Res := Args[1];
  Result := True;
end;

function Obj_GetOwnPropertyDescriptor(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var O: TValue; K: PStr; D: TPropDesc; Found: Boolean;
begin
  if not ToObjectV(Arg(Args, Argc, 1), O) then Exit(False);
  PushRoot(O);
  if not ToPropKey(Arg(Args, Argc, 2), K) then begin PopRoots(1); Exit(False); end;
  Result := GetOwnPropertyV(RootAt(TempTop - 1), K, D, Found);
  PopRoots(1);
  if Result then Res := FromPropertyDescriptor(D, Found);
end;

function Reflect_GetOwnPropertyDescriptor(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
begin
  if not IsObject(Arg(Args, Argc, 1)) then Exit(ThrowTypeError('Reflect.getOwnPropertyDescriptor called on non-object'));
  Result := Obj_GetOwnPropertyDescriptor(Args, Argc, Res);
end;

function Obj_GetOwnPropertyDescriptors(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var O, Keys, R, DO_: TValue; I, N: Integer; K: PStr; D: TPropDesc; Found: Boolean;
begin
  if not ToObjectV(Arg(Args, Argc, 1), O) then Exit(False);
  PushRoot(O);
  if not OwnPropertyKeysV(O, Keys) then begin PopRoots(1); Exit(False); end;
  PushRoot(Keys);
  R := MkPtr(NewPlainObject(ObjectProto, 8));
  PushRoot(R);
  N := ArrayLen(ObjOf(RootAt(TempTop - 2)));   { Keys moved if the allocation above collected }
  for I := 0 to N - 1 do
  begin
    K := KeyOf(ObjOf(RootAt(TempTop - 2))^.Elements^.Data[I]);
    if not GetOwnPropertyV(RootAt(TempTop - 3), K, D, Found) then begin PopRoots(3); Exit(False); end;
    if not Found then Continue;
    DO_ := FromPropertyDescriptor(D, True);
    DefineOwn(RootAt(TempTop - 1), K, DO_, pfDefault);
  end;
  Res := RootAt(TempTop - 1);
  PopRoots(3);
  Result := True;
end;

function Obj_Integrity(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var Ok: Boolean;
begin
  Res := Arg(Args, Argc, 1);
  if not IsObject(Res) then Exit(True);
  if NativeData = 2 then Result := PreventExtensionsV(Res, Ok)
  else Result := SetIntegrityLevel(Res, NativeData = 1, Ok);
  if Result and not Ok then Result := ThrowTypeError('Cannot change the extensibility of the object');
end;

function Obj_TestIntegrity(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var B: Boolean;
begin
  if not IsObject(Arg(Args, Argc, 1)) then begin Res := MkBool(NativeData <> 2); Exit(True); end;
  if NativeData = 2 then Result := IsExtensibleOM(Args[1], B)
  else Result := TestIntegrityLevel(Args[1], NativeData = 1, B);
  Res := MkBool(B);
end;

function Reflect_PreventExtensions(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var Ok: Boolean;
begin
  if not IsObject(Arg(Args, Argc, 1)) then Exit(ThrowTypeError('Reflect.preventExtensions called on non-object'));
  Result := PreventExtensionsV(Args[1], Ok);
  Res := MkBool(Ok);
end;

function Reflect_IsExtensible(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var B: Boolean;
begin
  if not IsObject(Arg(Args, Argc, 1)) then Exit(ThrowTypeError('Reflect.isExtensible called on non-object'));
  Result := IsExtensibleOM(Args[1], B);
  Res := MkBool(B);
end;

function Reflect_Set(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var K: PStr; Ok: Boolean; Rcv: TValue;
begin
  if not IsObject(Arg(Args, Argc, 1)) then Exit(ThrowTypeError('Reflect.set called on non-object'));
  if not ToPropKey(Arg(Args, Argc, 2), K) then Exit(False);
  if Argc >= 4 then Rcv := Args[4] else Rcv := Args[1];
  Result := SetWithReceiver(Args[1], K, Arg(Args, Argc, 3), Rcv, Ok);
  Res := MkBool(Ok);
end;

{ 20.1.3.2 Object.prototype.hasOwnProperty: ToPropertyKey, then ToObject(this), then [[GetOwnProperty]] }
function ObjProto_HasOwn(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var K: PStr; O: TValue; D: TPropDesc; Found: Boolean;
begin
  if not ToPropKey(Arg(Args, Argc, 1), K) then Exit(False);
  if IsObject(Args[0]) and (ObjOf(Args[0])^.OKind = okPlain) and (ObjOf(Args[0])^.Elements = nil) then
  begin
    Res := MkBool(ShapeLookup(ObjOf(Args[0])^.Shape, K) >= 0);
    Exit(True);
  end;
  if not ToObjectV(Args[0], O) then Exit(False);
  if not GetOwnPropertyV(O, K, D, Found) then Exit(False);
  Res := MkBool(Found);
  Result := True;
end;

{ 20.1.3.4 Object.prototype.propertyIsEnumerable }
function ObjProto_PropIsEnum(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var K: PStr; O: TValue; D: TPropDesc; Found: Boolean;
begin
  if not ToPropKey(Arg(Args, Argc, 1), K) then Exit(False);
  if not ToObjectV(Args[0], O) then Exit(False);
  if not GetOwnPropertyV(O, K, D, Found) then Exit(False);
  Res := MkBool(Found and D.Enumerable);
  Result := True;
end;

{ 20.4.3 Symbol.prototype: thisSymbolValue }
function ThisSymbol(V: TValue; var S: TValue): Boolean;
begin
  if IsPtr(V) and (PCell(PtrOf(V))^.Kind = ckSymbol) then begin S := V; Exit(True); end;
  if IsObject(V) and (ObjOf(V)^.OKind = okSymbolObj) then begin S := ObjOf(V)^.Aux1; Exit(True); end;
  Result := ThrowTypeError('Symbol.prototype method called on an incompatible receiver');
end;

function SymbolDescription(S: TValue): TValue;
begin
  if PCell(PtrOf(S))^.Extra = 1 then Exit(VUndefined);
  Result := PValue(PByte(PtrOf(S)) + 8)^;
end;

{ 20.4.3.3 get Symbol.prototype.description }
function SymProto_Description(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var S: TValue;
begin
  if not ThisSymbol(Args[0], S) then Exit(False);
  Res := SymbolDescription(S);
  Result := True;
end;

{ 20.4.3.4 toString: 20.4.3.4.1 SymbolDescriptiveString }
function SymProto_ToString2(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var S, D: TValue;
begin
  if not ThisSymbol(Args[0], S) then Exit(False);
  D := SymbolDescription(S);
  if D = VUndefined then Res := StrFromUnicode('Symbol()')
  else Res := StrFromUnicode('Symbol(' + StrToUnicode(StrOf(D)) + ')');
  Result := True;
end;

{ 20.4.3.5 valueOf and 20.4.3.6 [ @@toPrimitive ] }
function SymProto_ValueOf(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
begin
  Result := ThisSymbol(Args[0], Res);
end;

{ 20.1.2.12 Object.getPrototypeOf: ToObject, then [[GetPrototypeOf]] }
function Obj_GetPrototypeOf2(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var O: TValue;
begin
  if not ToObjectV(Arg(Args, Argc, 1), O) then Exit(False);
  Result := Object_GetPrototypeOfV(O, Res);
end;

{ 28.1.8 Reflect.getPrototypeOf }
function Reflect_GetPrototypeOf2(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
begin
  if not IsObject(Arg(Args, Argc, 1)) then Exit(ThrowTypeError('Reflect.getPrototypeOf called on non-object'));
  Result := Object_GetPrototypeOfV(Args[1], Res);
end;

{ [[SetPrototypeOf]]: 10.1.2.1 OrdinarySetPrototypeOf (with 10.4.7 immutable %Object.prototype%) and 10.5.2 for
  proxies (the trap's result, its invariant for a non-extensible target not checked) }
function SetPrototypeOfV(O, V: TValue; var Ok: Boolean): Boolean;
var Trap, R, P: TValue; Ext: Boolean;
begin
  Ok := False;
  if ObjOf(O)^.OKind = okProxy then
  begin
    PushRoot(O); PushRoot(V);
    if not ProxyGetTrap(O, 'setPrototypeOf', Trap) then begin PopRoots(2); Exit(False); end;
    O := RootAt(TempTop - 2); V := RootAt(TempTop - 1);
    PopRoots(2);
    if IsNullish(Trap) then Exit(SetPrototypeOfV(ObjOf(O)^.Aux1, V, Ok));
    PushRoot(O); PushRoot(V);
    if not ProxyCallTrap(O, Trap, 1, V, VUndefined, VUndefined, R) then begin PopRoots(2); Exit(False); end;
    Ok := ToBool(R);
    { 10.5.2 steps 9-14 }
    if Ok then
    begin
      if not IsExtensibleOM(ObjOf(RootAt(TempTop - 2))^.Aux1, Ext) then begin PopRoots(2); Exit(False); end;
      if not Ext then
      begin
        if not Object_GetPrototypeOfV(ObjOf(RootAt(TempTop - 2))^.Aux1, P) then begin PopRoots(2); Exit(False); end;
        if P <> RootAt(TempTop - 1) then begin PopRoots(2); Exit(ThrowTypeError('proxy setPrototypeOf trap reported success for a non-extensible target with another prototype')); end;
      end;
    end;
    PopRoots(2);
    Exit(True);
  end;
  Result := True;
  if ObjOf(O)^.Shape^.Proto = V then begin Ok := True; Exit; end;
  if O = ObjectProto then Exit;
  if not IsExtensibleOM(O, Ext) then Exit(False);
  if not Ext then Exit;
  P := V;
  while IsObject(P) do
  begin
    if P = O then Exit;
    if ObjOf(P)^.OKind = okProxy then Break;
    P := ObjOf(P)^.Shape^.Proto;
  end;
  SetProto(ObjOf(O), V);
  Ok := True;
end;

{ 20.1.2.23 Object.setPrototypeOf }
function Obj_SetPrototypeOf2(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var Ok: Boolean;
begin
  if IsNullish(Arg(Args, Argc, 1)) then Exit(ThrowTypeError('Object.setPrototypeOf called on null or undefined'));
  if not IsObject(Arg(Args, Argc, 2)) and (Arg(Args, Argc, 2) <> VNull) then
    Exit(ThrowTypeError('Object prototype may only be an Object or null'));
  Res := Args[1];
  if not IsObject(Res) then Exit(True);
  if not SetPrototypeOfV(Args[1], Args[2], Ok) then Exit(False);
  if not Ok then Exit(ThrowTypeError('Cannot set the prototype of this object'));
  Res := Args[1];
  Result := True;
end;

function NeedTarget(Args: TArgs; Argc: Integer; const Name: UnicodeString): Boolean; inline;
begin
  Result := IsObject(Arg(Args, Argc, 1));
  if not Result then ThrowTypeError('Reflect.' + Name + ' called on non-object');
end;

{ 28.1.14 Reflect.setPrototypeOf }
function Reflect_SetPrototypeOf2(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var Ok: Boolean;
begin
  if not NeedTarget(Args, Argc, 'setPrototypeOf') then Exit(False);
  if not IsObject(Arg(Args, Argc, 2)) and (Arg(Args, Argc, 2) <> VNull) then
    Exit(ThrowTypeError('Object prototype may only be an Object or null'));
  if not SetPrototypeOfV(Args[1], Args[2], Ok) then Exit(False);
  Res := MkBool(Ok);
  Result := True;
end;

{ 28.1.6 Reflect.get }
function Reflect_Get2(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var K: PStr; Rcv: TValue;
begin
  if not NeedTarget(Args, Argc, 'get') then Exit(False);
  if not ToPropKey(Arg(Args, Argc, 2), K) then Exit(False);
  if Argc >= 3 then Rcv := Args[3] else Rcv := Args[1];
  Result := GetFromObject(ObjOf(Args[1]), K, Rcv, Res);
end;

{ 28.1.4 Reflect.deleteProperty }
function Reflect_DeleteProperty2(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var K: PStr; Ok: Boolean;
begin
  if not NeedTarget(Args, Argc, 'deleteProperty') then Exit(False);
  if not ToPropKey(Arg(Args, Argc, 2), K) then Exit(False);
  if ObjOf(Args[1])^.OKind = okProxy then
  begin
    if not ProxyDelete(Args[1], K, Ok) then Exit(False);
  end
  else Ok := DeleteIfConfigurable(ObjOf(Args[1]), K);
  Res := MkBool(Ok);
  Result := True;
end;

{ 28.1.9 Reflect.has }
function Reflect_Has2(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var K: PStr; Found: Boolean;
begin
  if not NeedTarget(Args, Argc, 'has') then Exit(False);
  if not ToPropKey(Arg(Args, Argc, 2), K) then Exit(False);
  if not HasPropertyV(Args[1], K, Found) then Exit(False);
  Res := MkBool(Found);
  Result := True;
end;

{ 28.1.11 Reflect.ownKeys }
function Reflect_OwnKeys2(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
begin
  if not NeedTarget(Args, Argc, 'ownKeys') then Exit(False);
  Result := OwnPropertyKeysV(Args[1], Res);
end;

{ 10.2.4.1 %ThrowTypeError% }
{ a function the legacy function reflection serves: non-strict and ordinary (no arrow, method, class, generator
  or async function) }
function IsSloppyOrdinary(F: TValue): Boolean;
var T: PFuncTemplate;
begin
  Result := False;
  if not IsObject(F) or (ObjOf(F)^.OKind <> okFunction) then Exit;
  T := PFuncTemplate(ObjOf(F)^.Native);
  { a Script's, a module's or eval code's own closure is no function of the program's }
  Result := not (T^.IsStrict or T^.IsArrow or T^.IsMethod or T^.IsClassCtor or T^.IsGenerator or T^.IsAsync or
    T^.TopLevel);
end;

{ %ThrowTypeError%, the getter and setter of Function.prototype.caller and .arguments. Read on a non-strict
  ordinary function F (the legacy function reflection, es-legacy-function-reflection; 17.1 forbids a strict
  caller only): caller is the function of the call below F's newest active call (eval code skipped), null when
  F is not running or that caller is not a non-strict ordinary function; arguments is that call's arguments
  object, null when F is not running. Key: the property being read (AccessorGetKey) }
function LegacyCallerRead(F: TValue; Key: PStr; var Res: TValue): Boolean;
var I, J: Integer; T: PFuncTemplate;
begin
  Result := False;
  if not IsSloppyOrdinary(F) then Exit;
  Result := True;
  Res := VNull;
  for I := FrameTop - 1 downto 0 do
    if Frames[I].Closure = F then
    begin
      if Key = AtomOf('arguments') then
      begin
        T := Frames[I].Tpl;
        if (T^.ArgsReg >= 0) and IsObject(VStack[Frames[I].Base + T^.ArgsReg]) then
          Res := VStack[Frames[I].Base + T^.ArgsReg]
        else Res := NewArgumentsObject(@VStack[Frames[I].Base + 1], Min(Frames[I].Argc, T^.NumParams), False);
        Exit;
      end;
      J := I - 1;
      while (J >= 0) and (Frames[J].Tpl^.SourceName = '<eval>') do Dec(J);
      if (J >= 0) and IsSloppyOrdinary(Frames[J].Closure) then Res := Frames[J].Closure;
      Exit;
    end;
end;

function ThrowTypeError_Native(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var Key: PStr;
begin
  Key := AccessorGetKey;
  AccessorGetKey := nil;
  { a read (no argument) through the caller or arguments accessor of a non-strict function; a write is ignored }
  if LegacyCallerRead(Args[0], Key, Res) then
  begin
    if Argc > 0 then Res := VUndefined;
    Exit(True);
  end;
  Result := ThrowTypeError('''caller'', ''callee'', and ''arguments'' properties may not be accessed on strict mode functions or the arguments objects for calls to them');
end;


{ 7.3.23 EnumerableOwnProperties (Kind 0 keys, 1 values, 2 entries) }
function EnumerableOwn(O: TValue; Kind: Integer; var Res: TValue): Boolean;
var Keys, V, KV, E: TValue; I, N: Integer; K: PStr; D: TPropDesc; Found: Boolean;
begin
  if not (ObjOf(O)^.OKind in [okProxy, okModuleNS]) and (Kind = 0) then begin Res := OwnKeys(ObjOf(O), True); Exit(True); end;
  PushRoot(O);
  if ObjOf(O)^.OKind = okProxy then
  begin
    if not OwnPropertyKeysV(O, Keys) then begin PopRoots(1); Exit(False); end;
  end
  else Keys := OwnKeys(ObjOf(O), True);
  PushRoot(Keys);
  PushRoot(MkPtr(NewArray(0, 8)));
  N := ArrayLen(ObjOf(RootAt(TempTop - 2)));   { Keys moved if the allocation above collected }
  for I := 0 to N - 1 do
  begin
    KV := ObjOf(RootAt(TempTop - 2))^.Elements^.Data[I];
    if not IsString(KV) then Continue;
    K := KeyOf(KV);
    if not GetOwnPropertyV(RootAt(TempTop - 3), K, D, Found) then begin PopRoots(3); Exit(False); end;
    if not Found or not D.Enumerable then Continue;
    if Kind = 0 then begin ArrayPush(ObjOf(RootAt(TempTop - 1)), MkPtr(K)); Continue; end;
    if not GetV(RootAt(TempTop - 3), K, V) then begin PopRoots(3); Exit(False); end;
    if Kind = 2 then
    begin
      PushRoot(V);
      E := MkPtr(NewArray(0, 2));
      ArrayPush(ObjOf(E), MkPtr(K));
      ArrayPush(ObjOf(E), RootAt(TempTop - 1));
      PopRoots(1);
      V := E;
    end;
    ArrayPush(ObjOf(RootAt(TempTop - 1)), V);
  end;
  Res := RootAt(TempTop - 1);
  PopRoots(3);
  Result := True;
end;

{ get Error.prototype.stack (error stack accessor): an implementation-defined string for an object with
  [[ErrorData]] (G2 keeps no trace: empty), undefined for any other object }
function Err_StackGet(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
begin
  if not IsObject(Args[0]) then Exit(ThrowTypeError('Error.prototype.stack getter called on a non-object'));
  if ObjOf(Args[0])^.OKind = okError then Res := MkPtr(AtomOf('')) else Res := VUndefined;
  Result := True;
end;

{ set Error.prototype.stack: SetterThatIgnoresPrototypeProperties(this, %Error.prototype%, "stack", v) }
function Err_StackSet(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var D: TPropDesc; Found: Boolean; V: TValue; K: PStr;
begin
  Res := VUndefined;
  if not IsObject(Args[0]) then Exit(ThrowTypeError('Error.prototype.stack setter called on a non-object'));
  V := Arg(Args, Argc, 1);
  if not IsString(V) then Exit(ThrowTypeError('Error.prototype.stack must be set to a string'));
  if Args[0] = ErrorProto then Exit(ThrowTypeError('Cannot assign to read only property ''stack'' of Error.prototype'));
  K := AtomOf('stack');
  if not GetOwnPropertyV(Args[0], K, D, Found) then Exit(False);
  V := Args[1];
  if not Found then
  begin
    DataDesc(D, V, True, True, True);
    Exit(DefinePropertyOrThrow(Args[0], K, D));
  end;
  Result := SetVT(Args[0], K, V);
end;

{ 20.5.2.1 Error.isError ( arg ) }
function Error_IsError(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
begin
  Res := MkBool(IsObject(Arg(Args, Argc, 1)) and (ObjOf(Args[1])^.OKind = okError));
  Result := True;
end;

{ 20.1.3.3 Object.prototype.isPrototypeOf }
function ObjProto_IsPrototypeOf2(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var O: TValue; B: Boolean;
begin
  Res := VFalse;
  if not IsObject(Arg(Args, Argc, 1)) then Exit(True);
  PushRoot(Args[1]);
  if not ToObjectV(Args[0], O) then begin PopRoots(1); Exit(False); end;
  Result := ProtoChainHas(RootAt(TempTop - 1), O, B);
  PopRoots(1);
  if Result then Res := MkBool(B);
end;

function Obj_Keys(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var O: TValue;
begin
  if not ToObjectV(Arg(Args, Argc, 1), O) then Exit(False);
  Result := EnumerableOwn(O, NativeData, Res);
end;

{ 20.1.2.11.1 GetOwnPropertyKeys: Data 0 strings, 1 symbols }
function Obj_OwnKeysOfType(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var O, Keys, KV: TValue; I, N: Integer; Sym: Boolean;
begin
  if not ToObjectV(Arg(Args, Argc, 1), O) then Exit(False);
  Sym := NativeData = 1;
  if (ObjOf(O)^.OKind <> okProxy) and not Sym then begin Res := OwnKeys(ObjOf(O), False); Exit(True); end;
  if not OwnPropertyKeysV(O, Keys) then Exit(False);
  PushRoot(Keys);
  PushRoot(MkPtr(NewArray(0, 4)));
  N := ArrayLen(ObjOf(RootAt(TempTop - 2)));   { Keys moved if the allocation above collected }
  for I := 0 to N - 1 do
  begin
    KV := ObjOf(RootAt(TempTop - 2))^.Elements^.Data[I];
    if IsString(KV) <> Sym then ArrayPush(ObjOf(RootAt(TempTop - 1)), KV);
  end;
  Res := RootAt(TempTop - 1);
  PopRoots(2);
  Result := True;
end;

{ 20.1.2.1 Object.assign }
function Obj_Assign(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var T, F, Keys, V: TValue; I, J, N: Integer; K: PStr; D: TPropDesc; Found: Boolean;
begin
  if not ToObjectV(Arg(Args, Argc, 1), T) then Exit(False);
  PushRoot(T);
  for I := 2 to Argc do
  begin
    if IsNullish(Args[I]) then Continue;
    if not ToObjectV(Args[I], F) then begin PopRoots(1); Exit(False); end;
    PushRoot(F);
    if not OwnPropertyKeysV(F, Keys) then begin PopRoots(2); Exit(False); end;
    PushRoot(Keys);
    N := ArrayLen(ObjOf(Keys));
    for J := 0 to N - 1 do
    begin
      K := KeyOf(ObjOf(RootAt(TempTop - 1))^.Elements^.Data[J]);
      if not GetOwnPropertyV(RootAt(TempTop - 2), K, D, Found) then begin PopRoots(3); Exit(False); end;
      if not Found or not D.Enumerable then Continue;
      if not GetV(RootAt(TempTop - 2), K, V) then begin PopRoots(3); Exit(False); end;
      if not SetVT(RootAt(TempTop - 3), K, V) then begin PopRoots(3); Exit(False); end;
    end;
    PopRoots(2);
  end;
  Res := RootAt(TempTop - 1);
  PopRoots(1);
  Result := True;
end;

{ 13.2.8.4 GetTemplateObject steps 11-13: the raw strings array, then the template, frozen }
function FreezeTemplate(T: TValue): Boolean;
var Raw: TValue; Ok: Boolean;
begin
  PushRoot(T);
  Result := GetV(T, AtomOf('raw'), Raw) and SetIntegrityLevel(Raw, True, Ok) and
    SetIntegrityLevel(RootAt(TempTop - 1), True, Ok);
  PopRoots(1);
end;

procedure InitObjModel;
var O, R: TValue; Ok_: Boolean;
  procedure Def(Target: TValue; const Name: UnicodeString; Fn: TNativeFn; Len: Integer; Data: UInt32 = 0);
  begin
    PushRoot(Target);
    DefineOwn(RootAt(TempTop - 1), AtomOf(Name), NewNativeFunctionData(Fn, Name, Len, Data), pfHidden);
    PopRoots(1);
  end;
begin
  CreateDataHook := @CreateDataOM;
  ProxySetHook := @ProxySetOM;
  TemplateFreezeHook := @FreezeTemplate;
  TemplateCache := VUndefined;
  RealmVals([@TemplateCache]);
  SetReceiverHook := @SetWithReceiver;
  SetProtoHook := @SetPrototypeOfV;
  ProxyCheckHook := @ProxyCheckOM;
  ToObjectHook := @ToObjectV;
  CopyDataPropsHook := @CopyDataPropsOM;
  CopyDataPropsExclHook := @CopyDataPropsExclOM;
  GetProtoHook := @Object_GetPrototypeOfV;
  Def(SymbolProto, 'toString', SymProto_ToString2, 0);
  Def(SymbolProto, 'valueOf', SymProto_ValueOf, 0);
  DefineNativeGetterKey(SymbolProto, AtomOf('description'), 'get description', SymProto_Description);
  PushRoot(NewNativeFunction(SymProto_ValueOf, '[Symbol.toPrimitive]', 1));
  DefineOwn(SymbolProto, SymKey(SymToPrimitive), RootAt(TempTop - 1), pfConfigurable);
  PopRoots(1);
  Def(ObjectProto, 'hasOwnProperty', ObjProto_HasOwn, 1);
  Def(ObjectProto, 'propertyIsEnumerable', ObjProto_PropIsEnum, 1);
  Def(ObjectProto, 'isPrototypeOf', ObjProto_IsPrototypeOf2, 1);
  { the error stack accessor: error objects hold no own stack }
  PushRoot(NewNativeFunction(Err_StackGet, 'get stack', 0));
  PushRoot(NewNativeFunction(Err_StackSet, 'set stack', 1));
  O := MkPtr(NewAccessor(RootAt(TempTop - 2), RootAt(TempTop - 1)));
  DefineOwn(ErrorProto, AtomOf('stack'), O, pfConfigurable or pfAccessor);
  PopRoots(2);
  if GetV(GlobalObj, AtomOf('Error'), O) then Def(O, 'isError', Error_IsError, 1);
  { %ThrowTypeError%: frozen, its length and name non-configurable; one accessor cell serves every unmapped
    arguments object's callee (non-configurable, so never redefined in place) }
  AddRoot(@ThrowerAcc);
  O := NewNativeFunction(@ThrowTypeError_Native, '', 0);
  PushRoot(O);
  SetIntegrityLevel(RootAt(TempTop - 1), True, Ok_);
  ThrowerAcc := MkPtr(NewAccessor(RootAt(TempTop - 1), RootAt(TempTop - 1)));
  { 10.2.4 AddRestrictedFunctionProperties(%Function.prototype%): caller and arguments (configurable: an accessor
    cell of their own) }
  O := MkPtr(NewAccessor(RootAt(TempTop - 1), RootAt(TempTop - 1)));
  DefineOwn(FunctionProto, AtomOf('caller'), O, pfConfigurable or pfAccessor);
  O := MkPtr(NewAccessor(RootAt(TempTop - 1), RootAt(TempTop - 1)));
  DefineOwn(FunctionProto, AtomOf('arguments'), O, pfConfigurable or pfAccessor);
  PopRoots(1);
  AtomEnumerable := AtomOf('enumerable');
  AtomConfigurable := AtomOf('configurable');
  AtomWritable := AtomOf('writable');
  AtomGet := AtomOf('get');
  AtomSet := AtomOf('set');
  if GetV(GlobalObj, AtomOf('Object'), O) then
  begin
    PushRoot(O);
    Def(RootAt(TempTop - 1), 'defineProperty', Obj_DefineProperty, 3);
    Def(RootAt(TempTop - 1), 'defineProperties', Obj_DefineProperties, 2);
    Def(RootAt(TempTop - 1), 'getOwnPropertyDescriptor', Obj_GetOwnPropertyDescriptor, 2);
    Def(RootAt(TempTop - 1), 'getOwnPropertyDescriptors', Obj_GetOwnPropertyDescriptors, 1);
    Def(RootAt(TempTop - 1), 'freeze', Obj_Integrity, 1, 1);
    Def(RootAt(TempTop - 1), 'seal', Obj_Integrity, 1, 0);
    Def(RootAt(TempTop - 1), 'preventExtensions', Obj_Integrity, 1, 2);
    Def(RootAt(TempTop - 1), 'isFrozen', Obj_TestIntegrity, 1, 1);
    Def(RootAt(TempTop - 1), 'isSealed', Obj_TestIntegrity, 1, 0);
    Def(RootAt(TempTop - 1), 'isExtensible', Obj_TestIntegrity, 1, 2);
    Def(RootAt(TempTop - 1), 'getPrototypeOf', Obj_GetPrototypeOf2, 1);
    Def(RootAt(TempTop - 1), 'setPrototypeOf', Obj_SetPrototypeOf2, 2);
    Def(RootAt(TempTop - 1), 'keys', Obj_Keys, 1, 0);
    Def(RootAt(TempTop - 1), 'values', Obj_Keys, 1, 1);
    Def(RootAt(TempTop - 1), 'entries', Obj_Keys, 1, 2);
    Def(RootAt(TempTop - 1), 'getOwnPropertyNames', Obj_OwnKeysOfType, 1, 0);
    Def(RootAt(TempTop - 1), 'getOwnPropertySymbols', Obj_OwnKeysOfType, 1, 1);
    Def(RootAt(TempTop - 1), 'assign', Obj_Assign, 2);
    PopRoots(1);
  end;
  if GetV(GlobalObj, AtomOf('Reflect'), R) then
  begin
    PushRoot(R);
    Def(RootAt(TempTop - 1), 'defineProperty', Reflect_DefineProperty, 3);
    Def(RootAt(TempTop - 1), 'getOwnPropertyDescriptor', Reflect_GetOwnPropertyDescriptor, 2);
    Def(RootAt(TempTop - 1), 'preventExtensions', Reflect_PreventExtensions, 1);
    Def(RootAt(TempTop - 1), 'isExtensible', Reflect_IsExtensible, 1);
    Def(RootAt(TempTop - 1), 'set', Reflect_Set, 3);
    Def(RootAt(TempTop - 1), 'getPrototypeOf', Reflect_GetPrototypeOf2, 1);
    Def(RootAt(TempTop - 1), 'setPrototypeOf', Reflect_SetPrototypeOf2, 2);
    Def(RootAt(TempTop - 1), 'get', Reflect_Get2, 2);
    Def(RootAt(TempTop - 1), 'deleteProperty', Reflect_DeleteProperty2, 2);
    Def(RootAt(TempTop - 1), 'has', Reflect_Has2, 2);
    Def(RootAt(TempTop - 1), 'ownKeys', Reflect_OwnKeys2, 1);
    DefineOwn(RootAt(TempTop - 1), SymKey(SymToStringTagW), StrFromUnicode('Reflect'), pfConfigurable);
    PopRoots(1);
  end;
end;

end.
