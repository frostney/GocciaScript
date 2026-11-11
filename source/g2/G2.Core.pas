unit G2.Core;
{ Values, heap cells, generational GC, strings, atoms, shapes and objects of the spike engine. }
{$mode delphiunicode}{$H+}
{$MINFPCONSTPREC 64}
{$POINTERMATH ON}
{$INLINE ON}
{$Q-}{$R-}

interface

uses
  SysUtils, Math;

type
  TValue = QWord;
  PValue = ^TValue;

const
  TAG_INT     = QWord($FFF9000000000000);
  TAG_SPECIAL = QWord($FFFA000000000000);
  TAG_PTR     = QWord($FFFB000000000000);
  TAG_MASK    = QWord($FFFF000000000000);
  { the tags' top 16 bits: V shr 48 against these is a shift and a compare (V and TAG_MASK against a tag loads two
    64-bit immediates) }
  TAG_INT_HI     = $FFF9;
  TAG_SPECIAL_HI = $FFFA;
  TAG_PTR_HI     = $FFFB;
  PTR_MASK    = QWord($0000FFFFFFFFFFFF);
  DOUBLE_LIMIT = QWord($FFF9000000000000);
  CANON_NAN   = QWord($7FF8000000000000);

  VUndefined = TAG_SPECIAL or 0;
  VNull      = TAG_SPECIAL or 1;
  VFalse     = TAG_SPECIAL or 2;
  VTrue      = TAG_SPECIAL or 3;
  VHole      = TAG_SPECIAL or 4;

  { cell kinds }
  ckFree = 0;
  ckForward = 1;
  ckString = 2;
  ckValueArray = 3;
  ckUpval = 4;
  ckAccessor = 5;
  ckObject = 6;
  ckSymbol = 7;

  { object kinds (TObj.OKind) }
  okPlain = 0;
  okArray = 1;      { OFlags ofBigLen: Aux2 holds the length (a Number), past the elements }
  okFunction = 2;   { bytecode closure: Native = PFuncTemplate, Aux2 = upvalue array }
  okNative = 3;     { native function: Native = TNativeFn, Aux1 = data }
  okBound = 4;      { Aux1 = target, Aux2 = value array [this, args...] }
  okError = 5;
  okBoolean = 6; okNumber = 7; okStringObj = 8;  { Aux1 = primitive }
  okArguments = 9;
  okModuleNS = 24;       { a module namespace (10.4.6): Aux1 = [key, cell, ...] sorted by key; G2.Modules }
  okSymbolObj = 23;      { a Symbol wrapper (ToObject of a symbol): Aux1 = the symbol }
  okArgumentsOrd = 22;   { an arguments object turned ordinary: length and indices are shape properties }
  okMap = 10; okSet = 11;   { Aux1 = backing table cell }
  okIterator = 12;  { array/string iterator: Aux1 = iterated, Aux2 = index/kind }
  okGenerator = 13;
  okProxy = 21;     { Aux1 = target, Aux2 = handler (not an object once revoked); OFlags 16 = callable }
  ofBigLen = 32;    { an array whose length is past its dense elements (see ArrayLength) }
  ArrayDenseMax = 64 * 1024 * 1024;   { the longest an array's elements grow }

  { cell flags }
  cfMark = 1;
  cfOld = 2;
  cfRemembered = 4;
  cfPermanent = 8;
  cfBuffered = 16;   { a ckString whose characters live in a ckStrBuf: the pointer sits where Chars starts }
  cfEmulatesUndefined = 32;   { B.3.6 [[IsHTMLDDA]]: typeof "undefined", ToBoolean false, == undefined and null }
  cfSlice = 64;      { with cfBuffered: a ckString whose characters are a flat string's, at an Int32 offset after the
                       pointer (a long substring shares its parent's characters instead of copying them) }
  ckStrBuf = 11;

  { property flags }
  pfWritable = 1;
  pfEnumerable = 2;
  pfConfigurable = 4;
  pfAccessor = 8;
  pfDefault = pfWritable or pfEnumerable or pfConfigurable;
  pfHidden = pfWritable or pfConfigurable;   { methods, builtins }

type
  PCell = ^TCellHeader;
  TCellHeader = packed record
    Size: UInt32;     { bytes, multiple of 8 }
    Kind: Byte;
    Flags: Byte;
    Extra: Word;      { kind-specific small data }
  end;

  PStr = ^TStr;
  TStr = packed record
    Hdr: TCellHeader;
    Len: Int32;
    Hash: UInt32;     { 0 = not computed }
    Atom: Pointer;    { interned atom for this text, if known (atoms point to themselves) }
    Chars: array[0..0] of WideChar;
  end;

  PStrBuf = ^TStrBuf;
  TStrBuf = packed record
    Hdr: TCellHeader;
    Cap: Int32;
    Used: Int32;      { chars written: views of length Used may append in place }
    Chars: array[0..0] of WideChar;
  end;

  PValueArray = ^TValueArray;
  TValueArray = packed record
    Hdr: TCellHeader;
    Cap: UInt32;
    Len: UInt32;
    Data: array[0..0] of TValue;
  end;

  PUpval = ^TUpval;
  TUpval = packed record
    Hdr: TCellHeader;
    Value: TValue;
    StackIndex: Int64;   { >= 0 while open: lives at VStack[StackIndex] }
    NextOpen: PUpval;    { open list, sorted by StackIndex descending (non-GC link, updated on move) }
  end;

  PAccessor = ^TAccessor;
  TAccessor = packed record
    Hdr: TCellHeader;
    Getter: TValue;
    Setter: TValue;
  end;

  PShape = ^TShape;

  PObj = ^TObj;
  TObj = packed record
    Hdr: TCellHeader;
    Shape: PShape;
    Elements: PValueArray;   { dense indexed storage (arrays, arguments) }
    Overflow: PValueArray;   { slots beyond InlineCap }
    Native: Pointer;         { non-GC: function template / native code }
    Aux1: TValue;
    Aux2: TValue;
    InlineCap: Word;
    OKind: Byte;
    OFlags: Byte;            { 1 = class constructor, 2 = derived, 4 = arrow, 8 = not extensible }
    Pad: UInt32;
    Slots: array[0..0] of TValue;
  end;

  TShapeEdge = record
    Key: PStr;
    Flags: Byte;
    Child: PShape;
  end;

  TShape = record
    Proto: TValue;          { object or null }
    Parent: PShape;
    Count: Integer;
    Keys: array of PStr;    { atoms, by slot }
    PFlags: array of Byte;
    Edges: array of TShapeEdge;
    EdgeCount: Integer;
    EdgeHash: array of Integer;   { past 8 edges: open addressing over Key^.Hash and Flags, entries index + 1 }
    EdgeMask: Integer;
    { lookup: linear for small, hash for large }
    HashKeys: array of PStr;
    HashSlots: array of Integer;
    HashMask: Integer;
    HasAccessors: Boolean;
    HasIndexKeys: Boolean;  { some key is an array index: element writes into holes consult it }
    IsDict: Boolean;        { unique to one object, mutated in place }
    Version: Cardinal;      { a dictionary's in-place adds: an inline cache that passed it records this }
    ProtoChild: array of record P: TValue; S: PShape; end;  { root shapes per prototype }
  end;

  { native function: Args points at the this slot (Args[0] = this, Args[1..Argc] = arguments).
    Writes Result; returns False when an exception is pending (VM.Pending). }
  TNativeFn = function(Args: PValue; Argc: Integer; var Result: TValue): Boolean;

  TRootScanner = procedure(Visit: Pointer);

{ ---- value helpers ---- }
function IsDouble(V: TValue): Boolean; inline;
function IsInt(V: TValue): Boolean; inline;
function IsNumber(V: TValue): Boolean; inline;
function IsPtr(V: TValue): Boolean; inline;
function PtrOf(V: TValue): Pointer; inline;
function MkPtr(P: Pointer): TValue; inline;
function MkInt(I: Int32): TValue; inline;
{ 7.1.6 / 7.1.7: the integer part of D modulo 2^32, exactly (NaN and the infinities give 0); ToInt32 is its Int32 }
function DoubleToUint32Mod(D: Double): UInt32;
function IntOf(V: TValue): Int32; inline;
function MkDouble(D: Double): TValue; inline;
{ the result of + - * / on two Numbers: every boxed NaN is canonical, so a NaN result is either one of them
  (propagated) or the x86 default NaN, the only one to fix }
function MkArith(D: Double): TValue; inline;
function DoubleOf(V: TValue): Double; inline;
function MkNumber(D: Double): TValue; inline;
{ 6.1.6.1.3 Number::exponentiate }
function NumberExponentiate(B, E: Double): Double;
{ 6.1.6.1.6 Number::remainder (C fmod: the truncating remainder, sign of the dividend) }
function NumberRemainder(N, D: Double): Double; inline;
{ ToInt32 of an integer, or of a double below 2^53 in magnitude, else False (the general case) }
function QuickInt32(V: TValue; out I: Int32): Boolean; inline;
{ floor / ceil / Math.round on doubles over their whole range (FPC's Floor and Ceil return a 32-bit Integer);
  -0 is kept where the spec keeps it }
function DFloor(D: Double): Double;
function DCeil(D: Double): Double;
function DRound(D: Double): Double;
function NumOf(V: TValue): Double; inline;
function MkBool(B: Boolean): TValue; inline;
function IsString(V: TValue): Boolean; inline;
function IsObject(V: TValue): Boolean; inline;
function ObjOf(V: TValue): PObj; inline;
function StrOf(V: TValue): PStr; inline;
function IsNullish(V: TValue): Boolean; inline;
function IsCallable(V: TValue): Boolean; inline;

{ ---- heap ---- }
var
  GCStress: Boolean = False;
  GCDescribeNative: function(P: Pointer): string = nil;   { names a function template in GC diagnostics }
  GCVerify: Boolean = False;
threadvar
  NurseryStart, NurseryTop, NurseryLimit: PByte;
  GCRootScanner: procedure;   { VM visits its roots with GCVisit }
  GCMinorCount, GCMajorCount: Integer;
  GCPromotedBytes, GCOldBytes, GCOldBytesAtMajor: Int64;

procedure HeapInit(NurseryBytes: SizeInt);
{ frees this thread's heap: every cell, atom and shape; nothing of the heap may be used after it }
procedure HeapDone;
function AllocCell(Size: SizeInt; Kind: Byte): PCell;
function AllocOld(Size: SizeInt; Kind: Byte): PCell;
procedure GCVisit(var V: TValue); inline;
procedure GCVisitPtr(var P: Pointer);
procedure CollectMinor;
procedure CollectMajor;
{ weak references: GCWeakHook runs after marking/evacuation in every collection, before old space is swept.
  In it, GCSurvives says whether a cell lives on and updates P when the cell moved. The hook must not allocate. }
var GCWeakHook: procedure = nil;
{ like GCWeakHook, for releasing what dead cells held outside the heap (G2.Shared's buffer references) }
var GCFinalizeHook: procedure = nil;
function GCSurvives(var P: Pointer): Boolean;
function GCInMinor: Boolean;
procedure WriteBarrier(Container: PCell; V: TValue); inline;
procedure Remember(Container: PCell);

{ temp roots for native code: push values that must survive allocation }
threadvar
  TempRoots: array of TValue;
  TempTop: Integer;
procedure PushRoot(V: TValue); inline;
function RootAt(I: Integer): TValue; inline;
procedure PopRoots(N: Integer); inline;

{ ---- strings and atoms ---- }
function StrBufOf(S: PStr): PStrBuf; inline;
function StrChars(S: PStr): PWideChar; inline;
function NewString(Len: Integer): PStr;
function StrFromUnicode(const S: UnicodeString): TValue;
function StrToUnicode(S: PStr): UnicodeString;
function StrHash(S: PStr): UInt32;
function StrEquals(A, B: PStr): Boolean;
function Atomize(S: PStr): PStr;
function AtomOf(const S: UnicodeString): PStr;
{ the atom for Len characters at P (interned on first sight): the scanner's identifiers, straight from the source }
function AtomOfChars(P: PWideChar; Len: Integer): PStr;
{ AtomOfChars with the characters' FNV-1a hash already computed (HashChars' loop, before its 0 -> 1) }
function AtomOfCharsHashed(P: PWideChar; Len: Integer; H: UInt32): PStr;
{ the atom of an integer's decimal text (ToString of an integral Number), without an RTL string }
function IntAtom(I: Int64): PStr;
{ Key atoms no source text can name: kept out of the atom table, told apart by Hdr.Extra (kfSymbol: a
  Symbol's key, its Symbol in the payload; kfPrivate: a class's private name; kfInternal: an engine slot).
  Text is only a description. Ordinary keys (atoms) have Extra 0. }
const
  kfSymbol = 1; kfPrivate = 2; kfInternal = 4;
function NewKeyAtom(const Text: UnicodeString; Flags: Word): PStr;
{ the internal keys under which the global object holds the using-declaration helpers (G2.Disposal), for the
  compiler: 0 AddDisposableResource, 1 DisposeResources, 2 its async form }
function UsingKey(I: Integer): PStr;
function KeyPayload(K: PStr): PValue; inline;
function ConcatStrings(A, B: PStr): TValue;
function SubString(S: PStr; Start, Len: Integer): TValue;
function StrCompare(A, B: PStr): Integer;

{ ---- shapes ---- }
function RootShapeFor(Proto: TValue): PShape;
function NewDictShape(Proto: TValue): PShape;
{ a fresh shape with S's properties and no transitions: an object that takes it misses every inline cache
  that recorded S (the global object, when a global lexical binding starts to shadow one of its properties) }
function ShapeFork(S: PShape): PShape;
function ShapeLookup(S: PShape; Key: PStr): Integer;
function ShapeAdd(S: PShape; Key: PStr; Flags: Byte): PShape;
procedure ScanShapesForGC;
function NurseryCellOK(V: TValue): Boolean;
procedure CheckRoot(const V: TValue; const Where: string; I: Integer);

{ ---- objects ---- }
var
  { bumped when an object used as a prototype (one with a root shape: Hdr.Extra) gains an accessor or a
    read-only property, or a new prototype: a cached add transition (G2.VM opSetProp) holds the epoch it was
    made in, as its proof that no setter or read-only property on the chain applies. Shared by the agents'
    threads (a global loads cheaper than a threadvar): another agent's bump only costs a cache miss. }
  ProtoEpoch: Integer;
  { G2.VM's: forgets the cached add transitions of this agent's code (a bump is rare: the walk is cheap enough) }
  ProtoChangeHook: procedure = nil;
threadvar
  { no add transition is cached once a proxy is some object's prototype (its [[Set]] is the handler's) or the
    prototypes outnumber the root shape indices (ProtoChanged would miss changes to the rest) }
  AddICUnsafe: Boolean;
  ObjectProto, FunctionProto, ArrayProto, StringProto, NumberProto, BooleanProto,
  ErrorProto, IteratorProto, ArrayIteratorProto, GeneratorProto, MapProto, SetProtoObj, SymbolProto: TValue;
  EmptyShapeForArray: PShape;

function NewObject(Shape: PShape; InlineCap: Integer; OKind: Byte): PObj;
function NewPlainObject(Proto: TValue; InlineCap: Integer): PObj;
function NewValueArray(Cap: Integer): PValueArray;
function NewArray(Len: Integer; Cap: Integer): PObj;
function NewUpval: PUpval;
function NewAccessor(Getter, Setter: TValue): PAccessor;
function GetSlot(O: PObj; I: Integer): TValue; inline;
procedure SetSlot(O: PObj; I: Integer; V: TValue); inline;
function AddProperty(O: PObj; Key: PStr; V: TValue; Flags: Byte): Integer;
function GetOwnSlot(O: PObj; Key: PStr): Integer; inline;
procedure DeleteProperty(O: PObj; Key: PStr);
procedure ChangeFlags(O: PObj; Key: PStr; Flags: Byte);
function ProtoOf(O: PObj): TValue; inline;
procedure SetProto(O: PObj; P: TValue);
procedure ProtoChanged;
procedure ArraySetLength(A: PObj; NewLen: Integer);
procedure ArrayPush(A: PObj; V: TValue);
function ArrayLen(A: PObj): Integer; inline;
{ an array's length: past its elements' when ofBigLen is set (the indices past them are shape properties) }
function ArrayLength(A: PObj): Int64; inline;
{ sets an array's length to any valid one (at most 2^32-1); the caller removed what lies at or past it }
procedure ArraySetLengthAny(A: PObj; NewLen: Int64);
{ an array's length past its elements, which stay as they are (NewLen at least their count) }
procedure ArraySetLengthSparse(A: PObj; NewLen: Int64);
{ deletes an array's shape index properties at or past NewLen from the last; Result the length left }
function ArrayDropShapeIndices(P: PObj; NewLen: Int64): Int64;
procedure EnsureElementsCap(A: PObj; Need: Integer);
procedure ValueArrayStore(VA: PValueArray; I: Integer; V: TValue); inline;

{ ---- conversions without calling JS ---- }
function NumberToStr(D: Double): TValue;
function PrimitiveToNumber(V: TValue): Double;
function ToBool(V: TValue): Boolean; inline;
function TypeOfStr(V: TValue): UnicodeString;
function IndexFromValue(V: TValue; out Idx: Int64): Boolean; inline;
function IndexFromString(S: PStr; out Idx: Int64): Boolean;

implementation

uses
  NumericText;

{ ---------------- values ---------------- }

function IsDouble(V: TValue): Boolean; inline; begin Result := V < DOUBLE_LIMIT; end;
function IsInt(V: TValue): Boolean; inline; begin Result := (V shr 48) = TAG_INT_HI; end;
function IsNumber(V: TValue): Boolean; inline; begin Result := (V < DOUBLE_LIMIT) or ((V shr 48) = TAG_INT_HI); end;
function IsPtr(V: TValue): Boolean; inline; begin Result := (V shr 48) = TAG_PTR_HI; end;
function PtrOf(V: TValue): Pointer; inline; begin Result := Pointer(PtrUInt(V and PTR_MASK)); end;
function MkPtr(P: Pointer): TValue; inline; begin Result := TAG_PTR or QWord(PtrUInt(P)); end;
function MkInt(I: Int32): TValue; inline; begin Result := TAG_INT or QWord(UInt32(I)); end;
function IntOf(V: TValue): Int32; inline; begin Result := Int32(UInt32(V)); end;

function DoubleToUint32Mod(D: Double): UInt32;
var Bits, M: QWord; E: Integer;
begin
  Bits := PQWord(@D)^;
  E := Integer((Bits shr 52) and $7FF);
  if (E = $7FF) or (E = 0) then Exit(0);   { NaN, an infinity; zero or below 1 }
  M := (Bits and $FFFFFFFFFFFFF) or (QWord(1) shl 52);
  E := E - 1075;   { D = M * 2^E }
  if E >= 32 then Result := 0
  else if E >= 0 then Result := UInt32(M shl E)
  else if E > -53 then Result := UInt32(M shr (-E))
  else Result := 0;
  if (Bits shr 63) <> 0 then Result := UInt32(-Int64(Result));
end;
function MkArith(D: Double): TValue; inline;
begin
  Result := PQWord(@D)^;
  if Result = QWord($FFF8000000000000) then Result := CANON_NAN;
end;

function MkDouble(D: Double): TValue; inline;
begin
  Result := PQWord(@D)^;
  if Result >= DOUBLE_LIMIT then Result := CANON_NAN;   { negative NaNs }
  if (Result and $7FF0000000000000) = $7FF0000000000000 then
    if (Result and $000FFFFFFFFFFFFF) <> 0 then Result := CANON_NAN;
end;
function DoubleOf(V: TValue): Double; inline; begin Result := PDouble(@V)^; end;
function libm_pow(X, Y: Double): Double; cdecl; external 'm' name 'pow';
function libm_fmod(X, Y: Double): Double; cdecl; external 'm' name 'fmod';

function DFloor(D: Double): Double;
begin
  if IsNan(D) or IsInfinite(D) then Exit(D);
  Result := Int(D);
  if Result > D then Result := Result - 1;
end;

function DCeil(D: Double): Double;
begin
  if IsNan(D) or IsInfinite(D) then Exit(D);
  Result := Int(D);
  if Result < D then
  begin
    Result := Result + 1;
    if Result = 0 then Result := -0.0;   { ceil(-0.5) is -0 }
  end;
end;

{ 21.3.2.28 Math.round: ties toward +Infinity, -0 for -0.5 <= x < 0 }
function DRound(D: Double): Double;
begin
  if IsNan(D) or IsInfinite(D) or (Int(D) = D) then Exit(D);
  if (D < 0) and (D >= -0.5) then Exit(-0.0);
  Result := DFloor(D);
  if D - Result >= 0.5 then Result := Result + 1;
end;

{ C99 pow (glibc: correctly rounded) except where ECMAScript differs: a NaN exponent, and |base| 1 with an
  infinite exponent, are NaN }
function NumberExponentiate(B, E: Double): Double;
begin
  if IsNan(E) then Exit(NaN);
  if IsInfinite(E) and (Abs(B) = 1) then Exit(NaN);
  Result := libm_pow(B, E);
end;

function NumberRemainder(N, D: Double): Double; inline;
begin
  Result := libm_fmod(N, D);
end;

function QuickInt32(V: TValue; out I: Int32): Boolean; inline;
var D: Double;
begin
  if (V shr 48) = TAG_INT_HI then begin I := Int32(UInt32(V)); Exit(True); end;
  if V >= DOUBLE_LIMIT then Exit(False);
  D := PDouble(@V)^;
  { below 2^53 the truncation is exact in an Int64, and ToInt32 is its low 32 bits (NaN fails both tests) }
  if (D > -9007199254740992.0) and (D < 9007199254740992.0) then
  begin
    I := Int32(UInt32(QWord(Int64(Trunc(D))) and $FFFFFFFF));
    Exit(True);
  end;
  Result := False;
end;

function MkNumber(D: Double): TValue; inline;
var I: Int32;
begin
  if (D >= -2147483648.0) and (D <= 2147483647.0) then
  begin
    I := Trunc(D);
    if (I = D) and not ((I = 0) and (PQWord(@D)^ <> 0)) then Exit(MkInt(I));
  end;
  Result := MkDouble(D);
end;
function NumOf(V: TValue): Double; inline;
begin if V < DOUBLE_LIMIT then Result := PDouble(@V)^ else Result := Int32(UInt32(V)); end;
function MkBool(B: Boolean): TValue; inline; begin if B then Result := VTrue else Result := VFalse; end;
function IsString(V: TValue): Boolean; inline;
begin Result := ((V shr 48) = TAG_PTR_HI) and (PCell(PtrUInt(V and PTR_MASK))^.Kind = ckString); end;
function IsObject(V: TValue): Boolean; inline;
begin Result := ((V shr 48) = TAG_PTR_HI) and (PCell(PtrUInt(V and PTR_MASK))^.Kind = ckObject); end;
function ObjOf(V: TValue): PObj; inline; begin Result := PObj(PtrUInt(V and PTR_MASK)); end;
function StrOf(V: TValue): PStr; inline; begin Result := PStr(PtrUInt(V and PTR_MASK)); end;
function IsNullish(V: TValue): Boolean; inline; begin Result := (V = VUndefined) or (V = VNull); end;
function IsCallable(V: TValue): Boolean; inline;
var O: PObj;
begin
  Result := False;
  if not IsObject(V) then Exit;
  O := ObjOf(V);
  Result := (O^.OKind in [okFunction, okNative, okBound]) or ((O^.OKind = okProxy) and ((O^.OFlags and 16) <> 0));
end;

{ ---------------- heap ---------------- }

const
  PAGE_SIZE = 256 * 1024;
  LARGE_LIMIT = 16384;
  NUM_CLASSES = 22;
  ClassSizes: array[0..NUM_CLASSES - 1] of Integer =
    (16, 24, 32, 40, 48, 64, 80, 96, 112, 128, 160, 192, 256, 320, 384, 512, 768, 1024, 2048, 4096, 8192, 16384);

type
  PFreeCell = ^TFreeCell;
  TFreeCell = packed record
    Hdr: TCellHeader;
    Next: PFreeCell;
  end;
  PPage = ^TPage;
  TPage = record
    Base: PByte;
    CellSize: Integer;
    Bump: PByte;     { cells below Bump have been handed out at least once }
    Limit: PByte;
  end;
  PLarge = ^TLarge;
  TLarge = record
    Cell: PCell;
    Next: PLarge;
  end;

threadvar
  ClassOf: array[0..LARGE_LIMIT div 8] of Byte;
  FreeLists: array[0..NUM_CLASSES - 1] of PFreeCell;
  CurPage: array[0..NUM_CLASSES - 1] of PPage;
  Pages: array of PPage;
  PageCount: Integer;
  LargeList: PLarge;
  NurserySize: SizeInt;
  GrayStack: array of Pointer;
  GrayTop: Integer;
  RememberedList: array of PCell;
  RememberedTop: Integer;
  InMinor: Boolean;

{ a major collection runs when old space has grown by the larger of this and its size after the last one }
var GCMajorFloor: Int64 = 64 * 1024 * 1024; GCMajorGrowthPct: Int64 = 100;

procedure HeapInit(NurseryBytes: SizeInt);
var I, C: Integer;
begin
  if GetEnvironmentVariable('G2_MAJOR_FLOOR_MB') <> '' then
    GCMajorFloor := Int64(StrToInt(GetEnvironmentVariable('G2_MAJOR_FLOOR_MB'))) * 1024 * 1024;
  if GetEnvironmentVariable('G2_MAJOR_GROWTH_PCT') <> '' then
    GCMajorGrowthPct := StrToInt(GetEnvironmentVariable('G2_MAJOR_GROWTH_PCT'));
  NurserySize := NurseryBytes;
  NurseryStart := GetMem(NurseryBytes);
  NurseryTop := NurseryStart;
  NurseryLimit := NurseryStart + NurseryBytes;
  C := 0;
  for I := 0 to High(ClassOf) do
  begin
    while (C < NUM_CLASSES - 1) and (ClassSizes[C] < I * 8) do Inc(C);
    ClassOf[I] := C;
  end;
  SetLength(GrayStack, 65536);
  SetLength(RememberedList, 65536);
  SetLength(TempRoots, 65536);
  TempTop := 0;
end;


function NewPage(Cls: Integer): PPage;
begin
  New(Result);
  Result^.Base := GetMem(PAGE_SIZE);
  Result^.CellSize := ClassSizes[Cls];
  Result^.Bump := Result^.Base;
  Result^.Limit := Result^.Base + (PAGE_SIZE div ClassSizes[Cls]) * ClassSizes[Cls];
  if PageCount >= Length(Pages) then SetLength(Pages, PageCount * 2 + 16);
  Pages[PageCount] := Result;
  Inc(PageCount);
end;

function AllocOld(Size: SizeInt; Kind: Byte): PCell;
var Cls: Integer; F: PFreeCell; P: PPage; L: PLarge;
begin
  Size := (Size + 7) and not 7;
  if Size < 16 then Size := 16;
  if Size > LARGE_LIMIT then
  begin
    Result := GetMem(Size);
    New(L); L^.Cell := Result; L^.Next := LargeList; LargeList := L;
    Result^.Size := Size;
  end
  else
  begin
    Cls := ClassOf[Size shr 3];
    F := FreeLists[Cls];
    if F <> nil then
    begin
      FreeLists[Cls] := F^.Next;
      Result := PCell(F);
    end
    else
    begin
      P := CurPage[Cls];
      if (P = nil) or (P^.Bump >= P^.Limit) then
      begin
        P := NewPage(Cls);
        CurPage[Cls] := P;
      end;
      Result := PCell(P^.Bump);
      Inc(P^.Bump, P^.CellSize);
    end;
    Result^.Size := ClassSizes[Cls];
  end;
  Result^.Kind := Kind;
  Result^.Flags := cfOld;
  Result^.Extra := 0;
  Inc(GCOldBytes, Result^.Size);
end;

function AllocCell(Size: SizeInt; Kind: Byte): PCell;
begin
  Size := (Size + 7) and not 7;
  if Size < 16 then Size := 16;
  { up to 1/16 of the nursery a cell starts young: big temporaries (string concatenation results)
    then die in a minor collection instead of filling old space }
  if Size > (NurseryLimit - NurseryStart) div 16 then
  begin
    { direct old-space allocation still counts toward the major trigger }
    if GCOldBytes + Size - GCOldBytesAtMajor > Max(GCMajorFloor, GCOldBytesAtMajor * GCMajorGrowthPct div 100) then
    begin
      CollectMinor;
      CollectMajor;
    end;
    Exit(AllocOld(Size, Kind));
  end;
  if (NurseryTop + Size > NurseryLimit) or GCStress then
  begin
    CollectMinor;
    if (GCOldBytes - GCOldBytesAtMajor > Max(GCMajorFloor, GCOldBytesAtMajor * GCMajorGrowthPct div 100)) or (GCStress and (GCMinorCount mod 8 = 0)) then
      CollectMajor;
  end;
  Result := PCell(NurseryTop);
  Inc(NurseryTop, Size);
  Result^.Size := Size;
  Result^.Kind := Kind;
  Result^.Flags := 0;
  Result^.Extra := 0;
end;

function IsYoung(P: Pointer): Boolean; inline;
begin
  Result := (PByte(P) >= NurseryStart) and (PByte(P) < NurseryLimit);
end;

procedure Remember(Container: PCell);
begin
  Container^.Flags := Container^.Flags or cfRemembered;
  if RememberedTop >= Length(RememberedList) then SetLength(RememberedList, RememberedTop * 2);
  RememberedList[RememberedTop] := Container;
  Inc(RememberedTop);
end;

procedure WriteBarrier(Container: PCell; V: TValue); inline;
begin
  if ((Container^.Flags and (cfOld or cfRemembered)) = cfOld) and ((V shr 48) = TAG_PTR_HI) and
     IsYoung(Pointer(PtrUInt(V and PTR_MASK))) then
    Remember(Container);
end;

procedure PushGray(P: Pointer); inline;
begin
  if GrayTop >= Length(GrayStack) then SetLength(GrayStack, GrayTop * 2);
  GrayStack[GrayTop] := P;
  Inc(GrayTop);
end;

{ minor: copy a young cell to old space, leave a forwarding pointer }
function Evacuate(C: PCell): PCell;
var Size: UInt32;
begin
  { a single exit: with an early Exit, FPC 3.3.1 at -O2 and above moved the result register copy into the
    inlined PushGray below and jumped past it }
  if C^.Kind = ckForward then
    Result := PPointer(PByte(C) + 8)^
  else
  begin
    Size := C^.Size;
    Result := AllocOld(Size, C^.Kind);
    Inc(GCPromotedBytes, Size);
    Move((PByte(C) + 4)^, (PByte(Result) + 4)^, Size - 4);   { keep Result^.Size from AllocOld's class }
    Result^.Kind := C^.Kind;
    Result^.Flags := cfOld or (C^.Flags and (cfBuffered or cfEmulatesUndefined or cfSlice));
    C^.Kind := ckForward;
    PPointer(PByte(C) + 8)^ := Result;
    PushGray(Result);
  end;
end;

{ major: mark }
procedure MarkCell(C: PCell); inline;
begin
  if (C^.Flags and (cfMark or cfPermanent)) = 0 then
  begin
    C^.Flags := C^.Flags or cfMark;
    PushGray(C);
  end;
end;

procedure GCVisitPtr(var P: Pointer);
begin
  if P = nil then Exit;
  if InMinor then
  begin
    if IsYoung(P) then P := Evacuate(PCell(P));
  end
  else
    MarkCell(PCell(P));
end;

procedure GCVisit(var V: TValue); inline;
var P: Pointer;
begin
  if (V shr 48) = TAG_PTR_HI then
  begin
    P := Pointer(PtrUInt(V and PTR_MASK));
    if InMinor then
    begin
      if IsYoung(P) then V := TAG_PTR or QWord(PtrUInt(Evacuate(PCell(P))));
    end
    else
      MarkCell(PCell(P));
  end;
end;

{ a young pointer must land on a cell start (debug check: walks the nursery) }
function NurseryCellOK(V: TValue): Boolean;
var P, Q: PByte;
begin
  Result := True;
  if (V shr 48) <> TAG_PTR_HI then Exit;
  P := PByte(PtrUInt(V and PTR_MASK));
  if (P < NurseryStart) or (P >= NurseryLimit) then Exit;
  if P >= NurseryTop then Exit(False);
  Q := NurseryStart;
  while Q < P do
  begin
    if PCell(Q)^.Size = 0 then Exit(False);
    Inc(Q, PCell(Q)^.Size);
  end;
  Result := Q = P;
end;

{ cold (its strings would give ScanCell an exception frame): an object whose Aux points nowhere valid }
procedure ReportBadScan(O: PObj); noinline;
begin
  Write(ErrOutput, 'scan: object ', IntToHex(PtrUInt(O), 16), ' okind ', O^.OKind, ' oflags ', O^.OFlags,
    ' aux1 ', IntToHex(O^.Aux1, 16), ' aux2 ', IntToHex(O^.Aux2, 16), ' not a nursery cell');
  if (O^.OKind = 2) and Assigned(GCDescribeNative) then Write(ErrOutput, ' fn ', GCDescribeNative(O^.Native));
  WriteLn(ErrOutput, ' minor ', GCMinorCount);
  Halt(9);
end;

procedure ScanCell(C: PCell);
var I: Integer; O: PObj; VA: PValueArray; U: PUpval; A: PAccessor; S: PStr;
begin
  case C^.Kind of
    ckString:
      begin
        S := PStr(C);
        if (S^.Atom <> nil) and (S^.Atom <> Pointer(S)) then GCVisitPtr(S^.Atom);
        if (S^.Hdr.Flags and cfBuffered) <> 0 then GCVisitPtr(PPointer(@S^.Chars[0])^);
      end;
    ckValueArray:
      begin
        VA := PValueArray(C);
        for I := 0 to Integer(VA^.Len) - 1 do GCVisit(VA^.Data[I]);
      end;
    ckUpval:
      begin
        U := PUpval(C);
        GCVisit(U^.Value);
      end;
    ckAccessor:
      begin
        A := PAccessor(C);
        GCVisit(A^.Getter); GCVisit(A^.Setter);
      end;
    ckSymbol:
      GCVisit(PValue(PByte(C) + 8)^);
    ckObject:
      begin
        O := PObj(C);
        if GCVerify and InMinor and (not NurseryCellOK(O^.Aux2) or not NurseryCellOK(O^.Aux1)) then ReportBadScan(O);
        GCVisitPtr(Pointer(O^.Elements));
        GCVisitPtr(Pointer(O^.Overflow));
        GCVisit(O^.Aux1);
        GCVisit(O^.Aux2);
        for I := 0 to Min(O^.InlineCap, O^.Shape^.Count) - 1 do GCVisit(O^.Slots[I]);
        if not InMinor then
          GCVisit(O^.Shape^.Proto);
      end;
  end;
end;

procedure DrainGray;
var C: PCell;
begin
  while GrayTop > 0 do
  begin
    Dec(GrayTop);
    C := PCell(GrayStack[GrayTop]);
    ScanCell(C);
  end;
end;

procedure FixOpenUpvalLinks; forward;
procedure VerifyShapesAfterMinor; forward;
procedure VerifyHeap(const Phase: string); forward;

function GCInMinor: Boolean;
begin
  Result := InMinor;
end;

function GCSurvives(var P: Pointer): Boolean;
begin
  if P = nil then Exit(False);
  if InMinor then
  begin
    if not IsYoung(P) then Exit(True);
    if PCell(P)^.Kind = ckForward then
    begin
      P := PPointer(PByte(P) + 8)^;
      Exit(True);
    end;
    Exit(False);
  end;
  Result := (PCell(P)^.Flags and (cfMark or cfPermanent)) <> 0;
end;

procedure CollectMinor;
var I: Integer; C: PCell;
begin
  Inc(GCMinorCount);
  if GCVerify then VerifyHeap('barrier');   { every old cell pointing into the nursery is remembered }
  InMinor := True;
  GrayTop := 0;
  if Assigned(GCRootScanner) then GCRootScanner;
  for I := 0 to TempTop - 1 do GCVisit(TempRoots[I]);
  ScanShapesForGC;
  { old cells that point into the nursery }
  for I := 0 to RememberedTop - 1 do
  begin
    C := RememberedList[I];
    C^.Flags := C^.Flags and not cfRemembered;
    ScanCell(C);
  end;
  RememberedTop := 0;
  DrainGray;
  FixOpenUpvalLinks;
  if Assigned(GCWeakHook) then GCWeakHook;
  if Assigned(GCFinalizeHook) then GCFinalizeHook;
  InMinor := False;
  if GCVerify then begin VerifyShapesAfterMinor; VerifyHeap('minor'); end;
  NurseryTop := NurseryStart;
end;

procedure SweepPages;
var I: Integer; P: PPage; Q: PByte; C: PCell; F: PFreeCell; Cls, K: Integer; L, Prev, Nx: PLarge;
begin
  for K := 0 to NUM_CLASSES - 1 do FreeLists[K] := nil;
  GCOldBytes := 0;
  for I := 0 to PageCount - 1 do
  begin
    P := Pages[I];
    Cls := ClassOf[P^.CellSize shr 3];
    Q := P^.Base;
    while Q < P^.Bump do
    begin
      C := PCell(Q);
      if C^.Kind <> ckFree then
      begin
        if (C^.Flags and (cfMark or cfPermanent)) <> 0 then
        begin
          C^.Flags := C^.Flags and not cfMark;
          Inc(GCOldBytes, P^.CellSize);
        end
        else
          C^.Kind := ckFree;
      end;
      if C^.Kind = ckFree then
      begin
        F := PFreeCell(C);
        F^.Hdr.Size := P^.CellSize;
        F^.Next := FreeLists[Cls];
        FreeLists[Cls] := F;
      end;
      Inc(Q, P^.CellSize);
    end;
  end;
  Prev := nil; L := LargeList;
  while L <> nil do
  begin
    Nx := L^.Next;
    if (L^.Cell^.Flags and cfMark) <> 0 then
    begin
      L^.Cell^.Flags := L^.Cell^.Flags and not cfMark;
      Inc(GCOldBytes, L^.Cell^.Size);
      Prev := L;
    end
    else
    begin
      FreeMem(L^.Cell);
      if Prev = nil then LargeList := Nx else Prev^.Next := Nx;
      Dispose(L);
    end;
    L := Nx;
  end;
end;

procedure CollectMajor;
var I: Integer;
begin
  { called right after a minor collection: the nursery is empty }
  Inc(GCMajorCount);
  InMinor := False;
  GrayTop := 0;
  if Assigned(GCRootScanner) then GCRootScanner;
  for I := 0 to TempTop - 1 do GCVisit(TempRoots[I]);
  ScanShapesForGC;
  DrainGray;
  if Assigned(GCWeakHook) then GCWeakHook;
  if Assigned(GCFinalizeHook) then GCFinalizeHook;
  SweepPages;
  RememberedTop := 0;
  GCOldBytesAtMajor := GCOldBytes;
  if GCVerify then VerifyHeap('major');
end;

procedure PushRoot(V: TValue); inline;
begin
  TempRoots[TempTop] := V;
  Inc(TempTop);
end;
function RootAt(I: Integer): TValue; inline; begin Result := TempRoots[I]; end;
procedure PopRoots(N: Integer); inline;
begin
  Dec(TempTop, N);
  if TempTop < 0 then RunError(204);   { more pops than pushes: a native's root bookkeeping is off }
end;

{ open upvalues are linked through raw pointers; after a minor GC the moved ones are re-linked by the VM }
var
  OpenUpvalFixer: procedure = nil;
procedure FixOpenUpvalLinks;
begin
  if Assigned(OpenUpvalFixer) then OpenUpvalFixer;
end;

{ ---------------- strings and atoms ---------------- }

threadvar
  AtomTable: array of PStr;
  { the atoms of the one-code-unit strings below U+0100, made on first use (SubString of length 1) }
  CharAtoms: array[0..255] of PStr;
  AtomCount: Integer;
  AtomMask: Integer;

function StrBufOf(S: PStr): PStrBuf; inline;
begin
  Result := PStrBuf(PPointer(@S^.Chars[0])^);
end;

function StrChars(S: PStr): PWideChar; inline;
begin
  if (S^.Hdr.Flags and cfBuffered) <> 0 then
  begin
    if (S^.Hdr.Flags and cfSlice) <> 0 then Result := @PStr(PPointer(@S^.Chars[0])^)^.Chars[PInt32(@S^.Chars[4])^]
    else Result := @PStrBuf(PPointer(@S^.Chars[0])^)^.Chars[0];
  end
  else Result := @S^.Chars[0];
end;

function NewString(Len: Integer): PStr;
begin
  Result := PStr(AllocCell(SizeOf(TStr) - SizeOf(WideChar) + Len * SizeOf(WideChar), ckString));
  Result^.Len := Len;
  Result^.Hash := 0;
  Result^.Atom := nil;
end;

function StrFromUnicode(const S: UnicodeString): TValue;
var P: PStr;
begin
  P := NewString(Length(S));
  if Length(S) > 0 then Move(S[1], StrChars(P)[0], Length(S) * SizeOf(WideChar));
  Result := MkPtr(P);
end;

function StrToUnicode(S: PStr): UnicodeString;
begin
  SetLength(Result, S^.Len);
  if S^.Len > 0 then Move(StrChars(S)[0], Result[1], S^.Len * SizeOf(WideChar));
end;

function HashChars(P: PWideChar; Len: Integer): UInt32;
var I: Integer; H: UInt32;
begin
  H := 2166136261;
  for I := 0 to Len - 1 do H := (H xor Ord(P[I])) * 16777619;
  if H = 0 then H := 1;
  Result := H;
end;

function StrHash(S: PStr): UInt32;
begin
  if S^.Hash = 0 then S^.Hash := HashChars(StrChars(S), S^.Len);
  Result := S^.Hash;
end;

function StrEquals(A, B: PStr): Boolean;
begin
  if A = B then Exit(True);
  if A^.Len <> B^.Len then Exit(False);
  if (A^.Atom <> nil) and (A^.Atom = B^.Atom) then Exit(True);
  if (A^.Hash <> 0) and (B^.Hash <> 0) and (A^.Hash <> B^.Hash) then Exit(False);
  Result := CompareByte(StrChars(A)[0], StrChars(B)[0], A^.Len * 2) = 0;
end;

function StrCompare(A, B: PStr): Integer;
var I, N: Integer;
begin
  N := Min(A^.Len, B^.Len);
  for I := 0 to N - 1 do
    if StrChars(A)[I] <> StrChars(B)[I] then
      Exit(Ord(StrChars(A)[I]) - Ord(StrChars(B)[I]));
  Result := A^.Len - B^.Len;
end;

procedure AtomGrow;
var Old: array of PStr; I, J: Integer;
begin
  Old := AtomTable;
  if Length(Old) = 0 then SetLength(AtomTable, 4096) else SetLength(AtomTable, Length(Old) * 2);
  AtomTable := Copy(AtomTable, 0, Length(AtomTable));
  for I := 0 to High(AtomTable) do AtomTable[I] := nil;
  AtomMask := High(AtomTable);
  for I := 0 to High(Old) do
    if Old[I] <> nil then
    begin
      J := Integer(Old[I]^.Hash) and AtomMask;
      while AtomTable[J] <> nil do J := (J + 1) and AtomMask;
      AtomTable[J] := Old[I];
    end;
end;

function AtomFind(P: PWideChar; Len: Integer; H: UInt32): PStr;
var J: Integer; A: PStr;
begin
  if Length(AtomTable) = 0 then AtomGrow;
  J := Integer(H) and AtomMask;
  while True do
  begin
    A := AtomTable[J];
    if A = nil then Exit(nil);
    if (A^.Hash = H) and (A^.Len = Len) and ((Len = 0) or (CompareByte(StrChars(A)[0], P^, Len * 2) = 0)) then
      Exit(A);
    J := (J + 1) and AtomMask;
  end;
end;

function AtomInsert(P: PWideChar; Len: Integer; H: UInt32): PStr;
var J: Integer; Size: SizeInt;
begin
  if (AtomCount + 1) * 2 > Length(AtomTable) then AtomGrow;
  Size := (SizeOf(TStr) - SizeOf(WideChar) + Len * 2 + 7) and not 7;
  if Size < 16 then Size := 16;
  Result := GetMem(Size);
  Result^.Hdr.Size := Size;
  Result^.Hdr.Kind := ckString;
  Result^.Hdr.Flags := cfPermanent or cfOld;
  Result^.Len := Len;
  Result^.Hash := H;
  Result^.Atom := Result;
  Result^.Hdr.Extra := 0;
  if Len > 0 then Move(P^, StrChars(Result)[0], Len * 2);
  J := Integer(H) and AtomMask;
  while AtomTable[J] <> nil do J := (J + 1) and AtomMask;
  AtomTable[J] := Result;
  Inc(AtomCount);
end;

threadvar
  KeyAtoms: array of PStr;
  KeyAtomCount: Integer;

function KeyPayloadOffset(Len: Integer): SizeInt; inline;
begin
  Result := (SizeOf(TStr) - SizeOf(WideChar) + Len * 2 + 7) and not 7;
end;

threadvar
  UsingKey0, UsingKey1, UsingKey2: PStr;

function UsingKey(I: Integer): PStr;
begin
  case I of
    0: begin if UsingKey0 = nil then UsingKey0 := NewKeyAtom('%usingAdd', kfInternal); Result := UsingKey0; end;
    1: begin if UsingKey1 = nil then UsingKey1 := NewKeyAtom('%usingDispose', kfInternal); Result := UsingKey1; end;
  else begin if UsingKey2 = nil then UsingKey2 := NewKeyAtom('%usingDisposeAsync', kfInternal); Result := UsingKey2; end;
  end;
end;

function NewKeyAtom(const Text: UnicodeString; Flags: Word): PStr;
var Size: SizeInt;
begin
  Size := KeyPayloadOffset(Length(Text)) + 8;
  if Size < 16 then Size := 16;
  Result := GetMem(Size);
  Result^.Hdr.Size := Size;
  Result^.Hdr.Kind := ckString;
  Result^.Hdr.Flags := cfPermanent or cfOld;
  Result^.Hdr.Extra := Flags;
  Result^.Len := Length(Text);
  if Text <> '' then Move(Text[1], StrChars(Result)[0], Length(Text) * 2);
  Result^.Hash := HashChars(StrChars(Result), Length(Text));
  if Result^.Hash = 0 then Result^.Hash := 1;
  Result^.Atom := Result;
  PValue(PByte(Result) + KeyPayloadOffset(Length(Text)))^ := VUndefined;
  if KeyAtomCount >= Length(KeyAtoms) then SetLength(KeyAtoms, KeyAtomCount * 2 + 64);
  KeyAtoms[KeyAtomCount] := Result;
  Inc(KeyAtomCount);
end;

function KeyPayload(K: PStr): PValue; inline;
begin
  Result := PValue(PByte(K) + KeyPayloadOffset(K^.Len));
end;

function Atomize(S: PStr): PStr;
var H: UInt32;
begin
  if S^.Atom <> nil then Exit(PStr(S^.Atom));
  H := StrHash(S);
  Result := AtomFind(StrChars(S), S^.Len, H);
  if Result = nil then Result := AtomInsert(StrChars(S), S^.Len, H);
  S^.Atom := Result;
  if (S^.Hdr.Flags and cfOld) <> 0 then ;   { atoms are permanent: no barrier needed }
end;

function AtomOfCharsHashed(P: PWideChar; Len: Integer; H: UInt32): PStr;
begin
  if H = 0 then H := 1;
  Result := AtomFind(P, Len, H);
  if Result = nil then Result := AtomInsert(P, Len, H);
end;

function AtomOfChars(P: PWideChar; Len: Integer): PStr;
var H: UInt32;
begin
  H := HashChars(P, Len);
  Result := AtomFind(P, Len, H);
  if Result = nil then Result := AtomInsert(P, Len, H);
end;

function AtomOf(const S: UnicodeString): PStr;
var H: UInt32; P: PWideChar;
begin
  if S = '' then P := nil else P := @S[1];
  H := HashChars(P, Length(S));
  Result := AtomFind(P, Length(S), H);
  if Result = nil then Result := AtomInsert(P, Length(S), H);
end;

type
  PIntAtoms = ^TIntAtoms;
  TIntAtoms = array[0..1023] of PStr;

threadvar
  SmallIntAtoms: PIntAtoms;   { the atoms of 0..1023, made on first use (atoms are permanent) }

function IntAtom(I: Int64): PStr;
var Buf: array[0..23] of WideChar; P, N: Integer; U: QWord; H: UInt32; Small: Boolean;
begin
  Small := (I >= 0) and (I <= High(TIntAtoms));
  if Small then
  begin
    if SmallIntAtoms = nil then
    begin
      New(SmallIntAtoms);
      FillChar(SmallIntAtoms^, SizeOf(TIntAtoms), 0);
    end;
    Result := SmallIntAtoms^[I];
    if Result <> nil then Exit;
  end;
  P := Length(Buf);
  if I < 0 then U := QWord(0) - QWord(I) else U := QWord(I);
  repeat
    Dec(P);
    Buf[P] := WideChar(48 + U mod 10);
    U := U div 10;
  until U = 0;
  if I < 0 then begin Dec(P); Buf[P] := '-'; end;
  N := Length(Buf) - P;
  H := HashChars(@Buf[P], N);
  Result := AtomFind(@Buf[P], N, H);
  if Result = nil then Result := AtomInsert(@Buf[P], N, H);
  if Small then SmallIntAtoms^[I] := Result;
end;

{ concatenation of 256+ chars builds a view on a growable buffer: appending to the newest view of a buffer
  writes in place, so repeated s += x is amortised linear. Older views keep their length and stay valid,
  because nothing below a buffer's used length is ever rewritten. }
function ConcatStrings(A, B: PStr): TValue;
var R: PStr; LA, LB, Total, Cap: Integer; Buf: PStrBuf;
begin
  LA := A^.Len; LB := B^.Len;
  if LA = 0 then Exit(MkPtr(B));
  if LB = 0 then Exit(MkPtr(A));
  Total := LA + LB;
  if Total < 256 then
  begin
    PushRoot(MkPtr(A)); PushRoot(MkPtr(B));
    R := NewString(Total);
    A := StrOf(RootAt(TempTop - 2)); B := StrOf(RootAt(TempTop - 1));
    PopRoots(2);
    Move(StrChars(A)^, StrChars(R)^, LA * 2);
    Move(StrChars(B)^, StrChars(R)[LA], LB * 2);
    Exit(MkPtr(R));
  end;
  PushRoot(MkPtr(A)); PushRoot(MkPtr(B));
  if ((A^.Hdr.Flags and (cfBuffered or cfSlice)) = cfBuffered) and (StrBufOf(A)^.Used = LA) and (StrBufOf(A)^.Cap >= Total) then
    Buf := StrBufOf(A)   { A is the newest view and the buffer has room: append in place }
  else
  begin
    Cap := Total * 2;
    if Cap < 1024 then Cap := 1024;
    Buf := PStrBuf(AllocCell(SizeOf(TStrBuf) - SizeOf(WideChar) + Cap * SizeOf(WideChar), ckStrBuf));
    Buf^.Cap := Cap;
    Buf^.Used := 0;
    A := StrOf(RootAt(TempTop - 2));
    Move(StrChars(A)^, Buf^.Chars[0], LA * 2);
    Buf^.Used := LA;
  end;
  PushRoot(MkPtr(Buf));
  R := PStr(AllocCell(SizeOf(TStr) - SizeOf(WideChar) + SizeOf(Pointer), ckString));
  Buf := PStrBuf(PtrOf(RootAt(TempTop - 1)));
  B := StrOf(RootAt(TempTop - 2));
  PopRoots(3);
  Move(StrChars(B)^, Buf^.Chars[LA], LB * 2);
  Buf^.Used := Total;
  R^.Len := Total;
  R^.Hash := 0;
  R^.Atom := nil;
  R^.Hdr.Flags := R^.Hdr.Flags or cfBuffered;
  PPointer(@R^.Chars[0])^ := Buf;
  { the view may be old (allocated past the nursery limit) while the buffer is young }
  WriteBarrier(@R^.Hdr, MkPtr(Buf));
  Result := MkPtr(R);
end;

{ the atom of one code unit (out of SubString: the UnicodeString temporary would give it an exception frame) }
function CharAtom(C: WideChar): PStr; noinline;
begin
  Result := AtomOf(UnicodeString(C));
end;

const
  SLICE_MIN = 32;   { a substring at least this long shares its parent's characters (cfSlice) }

function SubString(S: PStr; Start, Len: Integer): TValue;
var R: PStr; C: Integer;
begin
  if (Start = 0) and (Len = S^.Len) then Exit(MkPtr(S));
  if Len <= 0 then Exit(MkPtr(AtomOf('')));
  if Len = 1 then
  begin
    C := Ord(StrChars(S)[Start]);
    if C < 256 then
    begin
      if CharAtoms[C] = nil then CharAtoms[C] := CharAtom(WideChar(C));
      Exit(MkPtr(CharAtoms[C]));
    end;
    Exit(MkPtr(CharAtom(StrChars(S)[Start])));
  end;
  { a long substring of a flat string or a slice: a slice of the flat string (no copy) }
  if (Len >= SLICE_MIN) and (((S^.Hdr.Flags and cfBuffered) = 0) or ((S^.Hdr.Flags and cfSlice) <> 0)) then
  begin
    if (S^.Hdr.Flags and cfSlice) <> 0 then
    begin
      Inc(Start, PInt32(@S^.Chars[4])^);
      S := PStr(PPointer(@S^.Chars[0])^);
    end;
    PushRoot(MkPtr(S));
    R := PStr(AllocCell(SizeOf(TStr) - SizeOf(WideChar) + 6 * SizeOf(WideChar), ckString));
    S := StrOf(RootAt(TempTop - 1));
    PopRoots(1);
    R^.Len := Len;
    R^.Hash := 0;
    R^.Atom := nil;
    R^.Hdr.Flags := R^.Hdr.Flags or cfBuffered or cfSlice;
    PPointer(@R^.Chars[0])^ := S;
    PInt32(@R^.Chars[4])^ := Start;
    if (R^.Hdr.Flags and cfOld) <> 0 then Remember(@R^.Hdr);
    Exit(MkPtr(R));
  end;
  PushRoot(MkPtr(S));
  R := NewString(Len);
  S := StrOf(RootAt(TempTop - 1));
  PopRoots(1);
  Move(StrChars(S)[Start], StrChars(R)[0], Len * 2);
  Result := MkPtr(R);
end;

{ ---------------- shapes ---------------- }

type
  PRootShapeEntry = ^TRootShapeEntry;
  TRootShapeEntry = record P: TValue; S: PShape; end;

threadvar
  AllShapes: array of PShape;
  ShapeCount: Integer;
  RootShapes: array of TRootShapeEntry;
  RootShapeCount: Integer;

function NewShape(Parent: PShape; Proto: TValue): PShape;
begin
  New(Result);
  FillChar(Result^, SizeOf(TShape), 0);
  Result^.Proto := Proto;
  Result^.Parent := Parent;
  Result^.HashMask := -1;
  if ShapeCount >= Length(AllShapes) then SetLength(AllShapes, ShapeCount * 2 + 1024);
  AllShapes[ShapeCount] := Result;
  Inc(ShapeCount);
end;

{ a dictionary shape: unique to one object and changed in place by adds (see TShape.Version) }
function NewDictShape(Proto: TValue): PShape;
begin
  Result := NewShape(nil, Proto);
  Result^.IsDict := True;
end;

function ShapeFork(S: PShape): PShape;
begin
  Result := NewShape(nil, S^.Proto);
  Result^.IsDict := S^.IsDict;
  Result^.Count := S^.Count;
  Result^.Keys := Copy(S^.Keys, 0, S^.Count);
  Result^.PFlags := Copy(S^.PFlags, 0, S^.Count);
  Result^.HasAccessors := S^.HasAccessors;
  Result^.HasIndexKeys := S^.HasIndexKeys;
end;

function ValidPtr(P: Pointer): Boolean; forward;

{ after a collection: no live old cell may point into the nursery or at a freed cell }
procedure VerifyHeap(const Phase: string);
var I: Integer; P: PPage; Q: PByte; C: PCell; L: PLarge;
  procedure Bad(C: PCell; const Field: string; V: QWord);
  begin
    Write(ErrOutput, 'heap verify (', Phase, ' minor ', GCMinorCount, ' major ', GCMajorCount, '): cell ',
      IntToHex(PtrUInt(C), 16), ' kind ', C^.Kind, ' flags ', C^.Flags);
    if C^.Kind = ckObject then Write(ErrOutput, ' okind ', PObj(C)^.OKind, ' oflags ', PObj(C)^.OFlags);
    WriteLn(ErrOutput, ' field ', Field, ' = ', IntToHex(V, 16));
    Halt(9);
  end;
  procedure CheckP(C: PCell; const Field: string; Ptr: Pointer);
  var B: PByte;
  begin
    if Ptr = nil then Exit;
    B := Ptr;
    if (B >= NurseryStart) and (B < NurseryLimit) then Bad(C, Field + ' (nursery)', PtrUInt(Ptr));
    if Phase = 'barrier' then Exit;
    if not ValidPtr(Ptr) then Bad(C, Field + ' (invalid)', PtrUInt(Ptr));
    if (PCell(Ptr)^.Kind = ckFree) or (PCell(Ptr)^.Kind = ckForward) then Bad(C, Field + ' (freed)', PtrUInt(Ptr));
  end;
  procedure CheckV(C: PCell; const Field: string; V: TValue);
  begin
    if (V shr 48) = TAG_PTR_HI then CheckP(C, Field, Pointer(PtrUInt(V and PTR_MASK)));
  end;
  procedure CheckCell(C: PCell);
  var J: Integer; O: PObj; VA: PValueArray;
  begin
    case C^.Kind of
      ckValueArray:
        begin
          VA := PValueArray(C);
          for J := 0 to Integer(VA^.Len) - 1 do CheckV(C, 'data[' + IntToStr(J) + ']', VA^.Data[J]);
        end;
      ckUpval: CheckV(C, 'value', PUpval(C)^.Value);
      ckAccessor: begin CheckV(C, 'getter', PAccessor(C)^.Getter); CheckV(C, 'setter', PAccessor(C)^.Setter); end;
      ckObject:
        begin
          O := PObj(C);
          CheckP(C, 'elements', O^.Elements);
          CheckP(C, 'overflow', O^.Overflow);
          CheckV(C, 'aux1', O^.Aux1);
          CheckV(C, 'aux2', O^.Aux2);
          for J := 0 to Min(O^.InlineCap, O^.Shape^.Count) - 1 do CheckV(C, 'slot[' + IntToStr(J) + ']', O^.Slots[J]);
        end;
    end;
  end;
begin
  for I := 0 to PageCount - 1 do
  begin
    P := Pages[I];
    Q := P^.Base;
    while Q < P^.Bump do
    begin
      C := PCell(Q);
      if (C^.Kind <> ckFree) and (C^.Kind <> ckForward) and
         ((Phase <> 'barrier') or ((C^.Flags and cfRemembered) = 0)) then CheckCell(C);
      Inc(Q, P^.CellSize);
    end;
  end;
  L := LargeList;
  while L <> nil do
  begin
    if (Phase <> 'barrier') or ((L^.Cell^.Flags and cfRemembered) = 0) then CheckCell(L^.Cell);
    L := L^.Next;
  end;
end;

procedure VerifyShapesAfterMinor;
var I: Integer;
begin

  for I := 0 to ShapeCount - 1 do
    if ((AllShapes[I]^.Proto shr 48) = TAG_PTR_HI) and IsYoung(PtrOf(AllShapes[I]^.Proto)) then
    begin
      WriteLn(ErrOutput, 'after minor ', GCMinorCount, ': shape ', I, ' still points into the nursery: ', IntToHex(AllShapes[I]^.Proto, 16));
      Halt(9);
    end;
end;

function ValidPtr(P: Pointer): Boolean;
var I: Integer; B: PByte;
begin
  B := P;
  if (B >= NurseryStart) and (B < NurseryTop) then Exit(True);
  for I := 0 to PageCount - 1 do
    if (B >= Pages[I]^.Base) and (B < Pages[I]^.Bump) then Exit(((B - Pages[I]^.Base) mod Pages[I]^.CellSize) = 0);
  Result := (PtrUInt(P) and 7) = 0;   { permanent or large: at least aligned }
end;

procedure CheckRoot(const V: TValue; const Where: string; I: Integer);
begin
  if ((V shr 48) = TAG_PTR_HI) and not ValidPtr(Pointer(PtrUInt(V and PTR_MASK))) then
  begin
    WriteLn(ErrOutput, 'bad pointer ', IntToHex(V, 16), ' at ', Where, ' ', I, ' minor ', GCMinorCount, ' major ', GCMajorCount,
      ' nursery ', IntToHex(PtrUInt(NurseryStart), 16));
    Halt(9);
  end;
end;

procedure ScanShapesForGC;
var I, J: Integer; V: TValue;
begin
  for I := 0 to ShapeCount - 1 do
  begin
    if AllShapes[I]^.Proto = TAG_PTR then
    begin
      WriteLn(ErrOutput, 'shape ', I, ' of ', ShapeCount, ' has a nil prototype; count ', AllShapes[I]^.Count,
        ' minor ', GCMinorCount, ' inminor ', InMinor);
      Halt(9);
    end;
    if GCVerify then
    begin
      CheckRoot(AllShapes[I]^.Proto, 'shape', I);
      V := AllShapes[I]^.Proto;
      if ((V shr 48) = TAG_PTR_HI) and ((PCell(PtrOf(V))^.Kind > 40) or (PCell(PtrOf(V))^.Size = 0) or
         (PCell(PtrOf(V))^.Size > 1 shl 30)) then
      begin
        Write(ErrOutput, 'shape ', I, ' of ', ShapeCount, ' (dict ', AllShapes[I]^.IsDict, ', count ', AllShapes[I]^.Count,
          ') has a bad prototype ', IntToHex(V, 16), ' kind ', PCell(PtrOf(V))^.Kind, ' minor ', GCMinorCount, '; keys:');
        for J := 0 to AllShapes[I]^.Count - 1 do Write(ErrOutput, ' ', StrToUnicode(AllShapes[I]^.Keys[J]));
        WriteLn(ErrOutput);
        Halt(9);
      end;
    end;
    GCVisit(AllShapes[I]^.Proto);
  end;
  for I := 0 to RootShapeCount - 1 do
  begin
    V := RootShapes[I].P;
    if GCVerify then CheckRoot(V, 'rootshape', I);
    GCVisit(V);
    RootShapes[I].P := V;
  end;
end;

{ cold: a prototype's first root shape (and the verifier's report) out of RootShapeFor's frame }
function NewRootShape(Proto: TValue): PShape; noinline;
begin
  if GCVerify and ((Proto shr 48) = TAG_PTR_HI) and not NurseryCellOK(Proto) then
  begin
    WriteLn(ErrOutput, 'RootShapeFor: a stale prototype ', IntToHex(Proto, 16));
    PInteger(nil)^ := 0;   { stop here under a debugger }
  end;
  Result := NewShape(nil, Proto);
  if RootShapeCount >= Length(RootShapes) then SetLength(RootShapes, RootShapeCount * 2 + 256);
  RootShapes[RootShapeCount].P := Proto;
  RootShapes[RootShapeCount].S := Result;
  Inc(RootShapeCount);
  if IsObject(Proto) and (RootShapeCount < 65535) then ObjOf(Proto)^.Hdr.Extra := RootShapeCount;
  if IsObject(Proto) and not AddICUnsafe and ((ObjOf(Proto)^.OKind = okProxy) or (RootShapeCount >= 65535)) then
  begin
    AddICUnsafe := True;
    ProtoChanged;
  end;
end;

threadvar
  LastPrimProto: TValue;
  LastPrimShape: PShape;

function RootShapeFor(Proto: TValue): PShape;
var I: Integer; R: PRootShapeEntry;
begin
  { prototypes are few; a linear scan over root shapes keyed by prototype (through a raw pointer: see
    ShapeLookup) }
  R := Pointer(RootShapes);
  if IsObject(Proto) then
  begin
    if ObjOf(Proto)^.Hdr.Extra <> 0 then
    begin
      I := ObjOf(Proto)^.Hdr.Extra - 1;
      if (I < RootShapeCount) and (R[I].P = Proto) then Exit(R[I].S);
    end
    else if RootShapeCount < 65534 then
      Exit(NewRootShape(Proto));   { never given one: its index would be in Extra }
  end
  else
  begin
    { null (Object.create(null), a class extending null) and the other primitives: the last one asked for }
    if (Proto = LastPrimProto) and (LastPrimShape <> nil) then Exit(LastPrimShape);
  end;
  for I := 0 to RootShapeCount - 1 do
    if R[I].P = Proto then
    begin
      if not IsObject(Proto) then begin LastPrimProto := Proto; LastPrimShape := R[I].S; end;
      Exit(R[I].S);
    end;
  Result := NewRootShape(Proto);
  if not IsObject(Proto) then begin LastPrimProto := Proto; LastPrimShape := Result; end;
end;

procedure BuildHash(S: PShape); noinline;   { cold: inlined, its dynamic arrays put an exception frame on every ShapeLookup }
var N, I, J: Integer;
begin
  N := 16;
  while N < S^.Count * 2 do N := N * 2;
  SetLength(S^.HashKeys, N);
  SetLength(S^.HashSlots, N);
  for I := 0 to N - 1 do S^.HashKeys[I] := nil;
  S^.HashMask := N - 1;
  for I := 0 to S^.Count - 1 do
  begin
    J := Integer(S^.Keys[I]^.Hash) and S^.HashMask;
    while S^.HashKeys[J] <> nil do J := (J + 1) and S^.HashMask;
    S^.HashKeys[J] := S^.Keys[I];
    S^.HashSlots[J] := I;
  end;
end;

function ShapeLookup(S: PShape; Key: PStr): Integer;
var I, J, M: Integer; KP: ^PStr;
begin
  { raw pointers into the arrays: at -O4 FPC otherwise hoists S^.Keys into a reference-counted temporary, an
    exception frame and two RTL calls per lookup }
  if S^.Count <= 8 then
  begin
    KP := Pointer(S^.Keys);
    for I := S^.Count - 1 downto 0 do
      if KP[I] = Key then Exit(I);
    Exit(-1);
  end;
  if S^.HashMask < 0 then BuildHash(S);
  KP := Pointer(S^.HashKeys);
  M := S^.HashMask;
  J := Integer(Key^.Hash) and M;
  while True do
  begin
    if KP[J] = Key then Exit(PInteger(Pointer(S^.HashSlots))[J]);
    if KP[J] = nil then Exit(-1);
    J := (J + 1) and M;
  end;
end;

function EdgeSlot(Key: PStr; Flags: Byte): Integer; inline;
begin
  Result := Integer(Key^.Hash xor (UInt32(Flags) * 2654435761));
end;

{ index edge E of S in its hash, rebuilding the table when it fills past half }
procedure EdgeIndexAdd(S: PShape; E: Integer);
var N, I, J: Integer;
begin
  if Length(S^.EdgeHash) < S^.EdgeCount * 2 then
  begin
    N := 32;
    while N < S^.EdgeCount * 4 do N := N * 2;
    SetLength(S^.EdgeHash, N);
    FillChar(S^.EdgeHash[0], N * SizeOf(Integer), 0);
    S^.EdgeMask := N - 1;
    for I := 0 to S^.EdgeCount - 1 do
    begin
      if S^.Edges[I].Key^.Hash = 0 then StrHash(S^.Edges[I].Key);
      J := EdgeSlot(S^.Edges[I].Key, S^.Edges[I].Flags) and S^.EdgeMask;
      while S^.EdgeHash[J] <> 0 do J := (J + 1) and S^.EdgeMask;
      S^.EdgeHash[J] := I + 1;
    end;
    Exit;
  end;
  if S^.Edges[E].Key^.Hash = 0 then StrHash(S^.Edges[E].Key);
  J := EdgeSlot(S^.Edges[E].Key, S^.Edges[E].Flags) and S^.EdgeMask;
  while S^.EdgeHash[J] <> 0 do J := (J + 1) and S^.EdgeMask;
  S^.EdgeHash[J] := E + 1;
end;

{ a new transition, or a dictionary's in-place add: out of ShapeAdd's frame (its Copy calls need one) }
function ShapeAddSlow(S: PShape; Key: PStr; Flags: Byte): PShape; noinline;
var I, J: Integer; N: PShape; MakeDict: Boolean; Idx64: Int64;
begin
  { very wide objects get a private dictionary shape, so they stop minting shared shapes }
  MakeDict := (S^.Count >= 64) and not S^.IsDict;
  if not S^.IsDict and not MakeDict then
    for I := 0 to S^.EdgeCount - 1 do
      if (S^.Edges[I].Key = Key) and (S^.Edges[I].Flags = Flags) then Exit(S^.Edges[I].Child);
  if S^.IsDict then
  begin
    N := S;
    Inc(N^.Version);   { a key added in place can shadow a prototype's: caches that passed this object check it }
  end
  else
  begin
    N := NewShape(S, S^.Proto);
    N^.Count := S^.Count;
    N^.Keys := Copy(S^.Keys, 0, S^.Count);
    N^.PFlags := Copy(S^.PFlags, 0, S^.Count);
    N^.HasAccessors := S^.HasAccessors;
    N^.HasIndexKeys := S^.HasIndexKeys;
    if MakeDict then
      N^.IsDict := True   { unregistered: no other object can reach it }
    else
    begin
      if S^.EdgeCount >= Length(S^.Edges) then SetLength(S^.Edges, S^.EdgeCount * 2 + 2);
      S^.Edges[S^.EdgeCount].Key := Key;
      S^.Edges[S^.EdgeCount].Flags := Flags;
      S^.Edges[S^.EdgeCount].Child := N;
      Inc(S^.EdgeCount);
      if S^.EdgeCount > 8 then EdgeIndexAdd(S, S^.EdgeCount - 1);
    end;
  end;
  if N^.Count >= Length(N^.Keys) then
  begin
    SetLength(N^.Keys, N^.Count * 2 + 4);
    SetLength(N^.PFlags, N^.Count * 2 + 4);
  end;
  N^.Keys[N^.Count] := Key;
  N^.PFlags[N^.Count] := Flags;
  Inc(N^.Count);
  if (Flags and pfAccessor) <> 0 then N^.HasAccessors := True;
  if (Key^.Hdr.Extra = 0) and IndexFromString(Key, Idx64) then N^.HasIndexKeys := True;
  if N^.IsDict and (N^.HashMask >= 0) and (N^.Count * 2 <= N^.HashMask + 1) then
  begin
    { in-place dictionary: insert into the existing table }
    if Key^.Hash = 0 then StrHash(Key);
    J := Integer(Key^.Hash) and N^.HashMask;
    while N^.HashKeys[J] <> nil do J := (J + 1) and N^.HashMask;
    N^.HashKeys[J] := Key;
    N^.HashSlots[J] := N^.Count - 1;
  end
  else
    N^.HashMask := -1;
  Result := N;
end;

function ShapeAdd(S: PShape; Key: PStr; Flags: Byte): PShape;
var I, J: Integer; E: ^TShapeEdge; H: PInteger;
begin
  if not S^.IsDict and (S^.Count < 64) then
  begin
    E := Pointer(S^.Edges);
    if (S^.EdgeCount > 8) and (Key^.Hash <> 0) then
    begin
      H := Pointer(S^.EdgeHash);
      J := EdgeSlot(Key, Flags) and S^.EdgeMask;
      while H[J] <> 0 do
      begin
        I := H[J] - 1;
        if (E[I].Key = Key) and (E[I].Flags = Flags) then Exit(E[I].Child);
        J := (J + 1) and S^.EdgeMask;
      end;
    end
    else
      for I := 0 to S^.EdgeCount - 1 do
        if (E[I].Key = Key) and (E[I].Flags = Flags) then Exit(E[I].Child);
  end;
  Result := ShapeAddSlow(S, Key, Flags);
end;

{ ---------------- objects ---------------- }

function NewObject(Shape: PShape; InlineCap: Integer; OKind: Byte): PObj;
var I: Integer;
begin
  Result := PObj(AllocCell(SizeOf(TObj) - SizeOf(TValue) + InlineCap * SizeOf(TValue), ckObject));
  Result^.Shape := Shape;
  Result^.Elements := nil;
  Result^.Overflow := nil;
  Result^.Native := nil;
  Result^.Aux1 := VUndefined;
  Result^.Aux2 := VUndefined;
  Result^.InlineCap := InlineCap;
  Result^.OKind := OKind;
  Result^.OFlags := 0;
  Result^.Pad := 0;
  for I := 0 to InlineCap - 1 do Result^.Slots[I] := VUndefined;
end;

function NewPlainObject(Proto: TValue; InlineCap: Integer): PObj;
begin
  Result := NewObject(RootShapeFor(Proto), InlineCap, okPlain);
end;

function NewValueArray(Cap: Integer): PValueArray;
var I: Integer;
begin
  if Cap < 1 then Cap := 1;
  Result := PValueArray(AllocCell(SizeOf(TValueArray) - SizeOf(TValue) + Cap * SizeOf(TValue), ckValueArray));
  Result^.Cap := Cap;
  Result^.Len := 0;
  for I := 0 to Cap - 1 do Result^.Data[I] := VUndefined;
end;

function NewArray(Len: Integer; Cap: Integer): PObj;
var E: PValueArray; I: Integer;
begin
  if Cap < Len then Cap := Len;
  E := NewValueArray(Cap);
  PushRoot(MkPtr(E));
  Result := NewObject(EmptyShapeForArray, 0, okArray);
  E := PValueArray(PtrOf(RootAt(TempTop - 1)));
  PopRoots(1);
  E^.Len := Len;
  for I := 0 to Len - 1 do E^.Data[I] := VHole;
  Result^.Elements := E;
  WriteBarrier(@Result^.Hdr, MkPtr(E));
end;

function NewUpval: PUpval;
begin
  Result := PUpval(AllocCell(SizeOf(TUpval), ckUpval));
  Result^.Value := VUndefined;
  Result^.StackIndex := -1;
  Result^.NextOpen := nil;
end;

function NewAccessor(Getter, Setter: TValue): PAccessor;
begin
  PushRoot(Getter); PushRoot(Setter);
  Result := PAccessor(AllocCell(SizeOf(TAccessor), ckAccessor));
  Result^.Getter := RootAt(TempTop - 2);
  Result^.Setter := RootAt(TempTop - 1);
  PopRoots(2);
end;

procedure ValueArrayStore(VA: PValueArray; I: Integer; V: TValue); inline;
begin
  VA^.Data[I] := V;
  WriteBarrier(@VA^.Hdr, V);
end;

function GetSlot(O: PObj; I: Integer): TValue; inline;
begin
  if I < O^.InlineCap then Result := O^.Slots[I]
  else Result := O^.Overflow^.Data[I - O^.InlineCap];
end;

procedure SetSlot(O: PObj; I: Integer; V: TValue); inline;
begin
  if I < O^.InlineCap then
  begin
    O^.Slots[I] := V;
    WriteBarrier(@O^.Hdr, V);
  end
  else
    ValueArrayStore(O^.Overflow, I - O^.InlineCap, V);
end;

function GetOwnSlot(O: PObj; Key: PStr): Integer; inline;
begin
  Result := ShapeLookup(O^.Shape, Key);
end;

{ adds a property that is known to be absent; may allocate (overflow growth) }
function AddProperty(O: PObj; Key: PStr; V: TValue; Flags: Byte): Integer;
var NewShape: PShape; Idx, NewCap, I: Integer; NO: PValueArray;
begin
  if ((Flags and (pfAccessor or pfWritable)) <> pfWritable) and (O^.Hdr.Extra <> 0) then ProtoChanged;
  NewShape := ShapeAdd(O^.Shape, Key, Flags);
  Idx := NewShape^.Count - 1;
  if Idx >= O^.InlineCap then
  begin
    I := Idx - O^.InlineCap;
    if (O^.Overflow = nil) or (I >= Integer(O^.Overflow^.Cap)) then
    begin
      if O^.Overflow = nil then NewCap := 4 else NewCap := O^.Overflow^.Cap * 2;
      PushRoot(MkPtr(O)); PushRoot(V);
      NO := NewValueArray(NewCap);
      O := ObjOf(RootAt(TempTop - 2)); V := RootAt(TempTop - 1);
      PopRoots(2);
      if O^.Overflow <> nil then
        Move(O^.Overflow^.Data[0], NO^.Data[0], O^.Overflow^.Len * SizeOf(TValue));
      if O^.Overflow <> nil then NO^.Len := O^.Overflow^.Len else NO^.Len := 0;
      O^.Overflow := NO;
      WriteBarrier(@O^.Hdr, MkPtr(NO));
      if (NO^.Hdr.Flags and cfOld) <> 0 then Remember(@NO^.Hdr);
    end;
    if Integer(O^.Overflow^.Len) <= I then O^.Overflow^.Len := I + 1;
  end;
  O^.Shape := NewShape;
  SetSlot(O, Idx, V);
  Result := Idx;
end;

procedure DeleteProperty(O: PObj; Key: PStr);
var Idx, I, J: Integer; Old: PShape; N: PShape; Vals: array of TValue;
begin
  Idx := ShapeLookup(O^.Shape, Key);
  if Idx < 0 then Exit;
  Old := O^.Shape;
  SetLength(Vals, Old^.Count);
  for I := 0 to Old^.Count - 1 do Vals[I] := GetSlot(O, I);
  { rebuild along the transition tree from the prototype's root shape without Key }
  N := RootShapeFor(Old^.Proto);
  O^.Shape := N;
  J := 0;
  for I := 0 to Old^.Count - 1 do
    if I <> Idx then
    begin
      O^.Shape := ShapeAdd(O^.Shape, Old^.Keys[I], Old^.PFlags[I]);
      SetSlot(O, J, Vals[I]);
      Inc(J);
    end;
end;

{ same keys in the same order, one key's attributes changed }
procedure ChangeFlags(O: PObj; Key: PStr; Flags: Byte);
var Idx, I: Integer; Old, N: PShape; Vals: array of TValue;
begin
  if ((Flags and (pfAccessor or pfWritable)) <> pfWritable) and (O^.Hdr.Extra <> 0) then ProtoChanged;
  Idx := ShapeLookup(O^.Shape, Key);
  if Idx < 0 then Exit;
  Old := O^.Shape;
  if Old^.IsDict then
  begin
    { a new identity, so inline caches holding the old dictionary shape miss }
    N := NewShape(nil, Old^.Proto);
    N^.Count := Old^.Count;
    N^.Keys := Copy(Old^.Keys, 0, Old^.Count);
    N^.PFlags := Copy(Old^.PFlags, 0, Old^.Count);
    N^.PFlags[Idx] := Flags;
    N^.HasAccessors := Old^.HasAccessors or ((Flags and pfAccessor) <> 0);
    N^.HasIndexKeys := Old^.HasIndexKeys;
    N^.IsDict := True;
    O^.Shape := N;
    Exit;
  end;
  SetLength(Vals, Old^.Count);
  for I := 0 to Old^.Count - 1 do Vals[I] := GetSlot(O, I);
  O^.Shape := RootShapeFor(Old^.Proto);
  for I := 0 to Old^.Count - 1 do
    if I = Idx then O^.Shape := ShapeAdd(O^.Shape, Old^.Keys[I], Flags)
    else O^.Shape := ShapeAdd(O^.Shape, Old^.Keys[I], Old^.PFlags[I]);
  for I := 0 to Old^.Count - 1 do SetSlot(O, I, Vals[I]);
end;

procedure ProtoChanged;
begin
  Inc(ProtoEpoch);
  if Assigned(ProtoChangeHook) then ProtoChangeHook;
end;

function ProtoOf(O: PObj): TValue; inline;
begin
  Result := O^.Shape^.Proto;
end;

procedure SetProto(O: PObj; P: TValue);
var Old, N: PShape; I: Integer;
begin
  Old := O^.Shape;
  if Old^.Proto = P then Exit;
  if O^.Hdr.Extra <> 0 then ProtoChanged;
  N := RootShapeFor(P);
  for I := 0 to Old^.Count - 1 do N := ShapeAdd(N, Old^.Keys[I], Old^.PFlags[I]);
  O^.Shape := N;
end;

function ArrayLen(A: PObj): Integer; inline;
begin
  Result := A^.Elements^.Len;
end;

function ArrayLength(A: PObj): Int64; inline;
begin
  if (A^.OFlags and ofBigLen) <> 0 then Result := Trunc(DoubleOf(A^.Aux2))
  else Result := A^.Elements^.Len;
end;

{ 10.4.2.4 steps 17-18 for the indices kept in the shape (the elements are configurable): deletes those at or
  past NewLen from the last; Result the length left, one past a non-configurable index that stopped it }
function ArrayDropShapeIndices(P: PObj; NewLen: Int64): Int64;
var Idxs: array of Int64; N, I, J, Gap, S: Integer; Idx, T: Int64;
begin
  Result := NewLen;
  if not P^.Shape^.HasIndexKeys then Exit;
  N := 0;
  SetLength(Idxs, P^.Shape^.Count);
  for I := 0 to P^.Shape^.Count - 1 do
    if (P^.Shape^.Keys[I]^.Hdr.Extra = 0) and IndexFromString(P^.Shape^.Keys[I], Idx) and (Idx >= NewLen) then
    begin
      Idxs[N] := Idx;
      Inc(N);
    end;
  { descending (a shell sort) }
  Gap := N div 2;
  while Gap > 0 do
  begin
    for I := Gap to N - 1 do
    begin
      T := Idxs[I];
      J := I;
      while (J >= Gap) and (Idxs[J - Gap] < T) do
      begin
        Idxs[J] := Idxs[J - Gap];
        Dec(J, Gap);
      end;
      Idxs[J] := T;
    end;
    Gap := Gap div 2;
  end;
  for I := 0 to N - 1 do
  begin
    S := ShapeLookup(P^.Shape, IntAtom(Idxs[I]));
    if S < 0 then Continue;
    if (P^.Shape^.PFlags[S] and pfConfigurable) = 0 then Exit(Idxs[I] + 1);
    DeleteProperty(P, IntAtom(Idxs[I]));
  end;
end;

procedure ArraySetLengthAny(A: PObj; NewLen: Int64);
begin
  if NewLen <= A^.Elements^.Len then
  begin
    ArraySetLength(A, NewLen);
    if (A^.OFlags and ofBigLen) <> 0 then
    begin
      A^.OFlags := A^.OFlags and not ofBigLen;
      A^.Aux2 := VUndefined;
    end;
  end
  else if ((A^.OFlags and ofBigLen) = 0) and (NewLen <= ArrayDenseMax) then
    ArraySetLength(A, NewLen)
  else ArraySetLengthSparse(A, NewLen);
end;

procedure ArraySetLengthSparse(A: PObj; NewLen: Int64);
begin
  { every write to the array takes the checked path (G2.ObjModel ofElemGuard) }
  A^.OFlags := A^.OFlags or ofBigLen or 64;
  A^.Aux2 := MkDouble(NewLen);
end;

procedure EnsureElementsCap(A: PObj; Need: Integer);
var E, NE: PValueArray; NewCap: Integer;
begin
  E := A^.Elements;
  if Need <= Integer(E^.Cap) then Exit;
  NewCap := Integer(E^.Cap) * 2;
  if NewCap < Need then NewCap := Need;
  if NewCap < 8 then NewCap := 8;
  PushRoot(MkPtr(A));
  NE := NewValueArray(NewCap);
  A := ObjOf(RootAt(TempTop - 1));
  PopRoots(1);
  E := A^.Elements;
  Move(E^.Data[0], NE^.Data[0], E^.Len * SizeOf(TValue));
  NE^.Len := E^.Len;
  A^.Elements := NE;
  WriteBarrier(@A^.Hdr, MkPtr(NE));
  if (NE^.Hdr.Flags and cfOld) <> 0 then Remember(@NE^.Hdr);
end;

procedure ArraySetLength(A: PObj; NewLen: Integer);
var E: PValueArray; I: Integer;
begin
  if NewLen > Integer(A^.Elements^.Cap) then
  begin
    PushRoot(MkPtr(A));
    EnsureElementsCap(A, NewLen);
    A := ObjOf(RootAt(TempTop - 1));
    PopRoots(1);
  end;
  E := A^.Elements;
  for I := E^.Len to NewLen - 1 do E^.Data[I] := VHole;
  for I := NewLen to Integer(E^.Len) - 1 do E^.Data[I] := VUndefined;
  E^.Len := NewLen;
end;

procedure ArrayPush(A: PObj; V: TValue);
var E: PValueArray;
begin
  E := A^.Elements;
  if E^.Len >= E^.Cap then
  begin
    PushRoot(MkPtr(A)); PushRoot(V);
    EnsureElementsCap(A, E^.Len + 1);
    A := ObjOf(RootAt(TempTop - 2)); V := RootAt(TempTop - 1);
    PopRoots(2);
    E := A^.Elements;
  end;
  E^.Data[E^.Len] := V;
  WriteBarrier(@E^.Hdr, V);
  Inc(E^.Len);
end;

{ ---------------- conversions ---------------- }

function NumberToStr(D: Double): TValue;
begin
  Result := StrFromUnicode(NumericText.NumberToString(D));
end;

{ cold: the RTL string of the conversion stays out of PrimitiveToNumber's frame }
function StringToNumberV(V: TValue): Double; noinline;
begin
  Result := NumericText.StringToNumber(StrToUnicode(StrOf(V)));
end;

function PrimitiveToNumber(V: TValue): Double;
begin
  if V < DOUBLE_LIMIT then Exit(PDouble(@V)^);
  if (V shr 48) = TAG_INT_HI then Exit(Int32(UInt32(V)));
  case V of
    VUndefined: Result := NaN;
    VNull, VFalse: Result := 0;
    VTrue: Result := 1;
  else
    if IsString(V) then Result := StringToNumberV(V)
    else Result := NaN;
  end;
end;

function ToBool(V: TValue): Boolean; inline;
begin
  if (V shr 48) = TAG_INT_HI then Exit(UInt32(V) <> 0);
  if V < DOUBLE_LIMIT then Exit((PDouble(@V)^ <> 0) and (V <> CANON_NAN));
  if V = VTrue then Exit(True);
  if (V shr 48) = TAG_SPECIAL_HI then Exit(False);
  if IsString(V) then Exit(StrOf(V)^.Len > 0);
  if PCell(PtrOf(V))^.Kind = 9 then Exit(PInt32(PByte(PtrOf(V)) + 8)^ <> 0);
  Result := (PCell(PtrOf(V))^.Flags and cfEmulatesUndefined) = 0;
end;

function TypeOfStr(V: TValue): UnicodeString;
begin
  if IsNumber(V) then Exit('number');
  case V of
    VUndefined, VHole: Exit('undefined');
    VNull: Exit('object');
    VTrue, VFalse: Exit('boolean');
  end;
  case PCell(PtrOf(V))^.Kind of
    ckString: Result := 'string';
    ckSymbol: Result := 'symbol';
    9: Result := 'bigint';
    ckObject:
      if (PCell(PtrOf(V))^.Flags and cfEmulatesUndefined) <> 0 then Result := 'undefined'
      else if IsCallable(V) then Result := 'function' else Result := 'object';
  else Result := 'undefined';
  end;
end;

function IndexFromString(S: PStr; out Idx: Int64): Boolean;
var I: Integer; C: Integer;
begin
  Result := False;
  if (S^.Len = 0) or (S^.Len > 10) then Exit;
  if S^.Hdr.Extra <> 0 then Exit;   { a symbol, private or internal key: its text is only a description }
  if (StrChars(S)[0] = '0') and (S^.Len > 1) then Exit;
  Idx := 0;
  for I := 0 to S^.Len - 1 do
  begin
    C := Ord(StrChars(S)[I]) - 48;
    if (C < 0) or (C > 9) then Exit;
    Idx := Idx * 10 + C;
  end;
  Result := Idx < 4294967295;
end;

function IndexFromValue(V: TValue; out Idx: Int64): Boolean; inline;
var D: Double;
begin
  if (V shr 48) = TAG_INT_HI then
  begin
    Idx := Int32(UInt32(V));
    Exit(Idx >= 0);
  end;
  if V < DOUBLE_LIMIT then
  begin
    D := PDouble(@V)^;
    if (D >= 0) and (D < 4294967295.0) and (Frac(D) = 0) then begin Idx := Trunc(D); Exit(True); end;
    Exit(False);
  end;
  if IsString(V) then Exit(IndexFromString(StrOf(V), Idx));
  Result := False;
end;

procedure HeapDone;
var I: Integer; L, Nx: PLarge;
begin
  for I := 0 to PageCount - 1 do begin FreeMem(Pages[I]^.Base); Dispose(Pages[I]); end;
  Pages := nil; PageCount := 0;
  L := LargeList;
  while L <> nil do begin Nx := L^.Next; FreeMem(L^.Cell); Dispose(L); L := Nx; end;
  LargeList := nil;
  for I := 0 to High(CurPage) do CurPage[I] := nil;
  for I := 0 to High(FreeLists) do FreeLists[I] := nil;
  if NurseryStart <> nil then FreeMem(NurseryStart);
  NurseryStart := nil; NurseryTop := nil; NurseryLimit := nil;
  for I := 0 to High(AtomTable) do if AtomTable[I] <> nil then FreeMem(AtomTable[I]);
  AtomTable := nil; AtomCount := 0; AtomMask := 0;
  FillChar(CharAtoms, SizeOf(CharAtoms), 0);
  for I := 0 to KeyAtomCount - 1 do FreeMem(KeyAtoms[I]);
  KeyAtoms := nil; KeyAtomCount := 0;
  for I := 0 to ShapeCount - 1 do Dispose(AllShapes[I]);
  AllShapes := nil; ShapeCount := 0;
  RootShapes := nil; RootShapeCount := 0;
  GrayStack := nil; GrayTop := 0; RememberedList := nil; RememberedTop := 0; TempRoots := nil; TempTop := 0;
  GCOldBytes := 0; GCOldBytesAtMajor := 0; GCPromotedBytes := 0; GCMinorCount := 0; GCMajorCount := 0;
end;

end.
