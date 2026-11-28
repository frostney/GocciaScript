unit Goccia.Core;
{ Values, heap cells, generational GC, strings, atoms, shapes and objects of the spike engine. }
{$mode delphiunicode}{$H+}
{$I Goccia.Checks.inc}
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
  { the kinds of typed array (Goccia.Typed's TAKindCount; Goccia.VM checks they agree) }
  TA_WATCHED_KINDS = 12;
  { the temp root stack's capacity (TempRoots): 32 MB of address space, committed as used }
  TEMP_ROOTS_MAX = 4 * 1024 * 1024;
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
  okModuleNS = 24;       { a module namespace (10.4.6): Aux1 = [key, cell, ...] sorted by key; Goccia.Modules }
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
  cfEphKey = 128;    { a cell some ephemeron (WeakMap entry, table owner) waits on during a collection: marking or
                       evacuating it notes it (GCEphKeyCount) }
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
    { a child shape extends this shape's Keys and PFlags in place (shared arrays: a shape reads only its first
      Count entries, and only a dictionary's are written below its Count); the next child copies them }
    KeysLent: Boolean;
    { baked into machine code (Goccia.Jit.inc JitPin): once retired, its record is kept (its prototype reads
      VRetiredProto there), not freed }
    Pinned: Boolean;
    { a dictionary's in-place adds: an inline cache that passed it records this. Every shape and every in-place
      add takes a version no shape of this engine has had (ShapeVersionNext), so a cache that holds a shape's
      address and version never matches a later shape at the same address }
    Version: Cardinal;
    ProtoChild: array of record P: TValue; S: PShape; end;  { root shapes per prototype }
    { the bytes this shape is charged against HeapCeiling for (ShapeRecharge): its record and the arrays it owns }
    Charged: Int64;
    { Keys and PFlags are its parent's, lent (KeysLent there): charged to the parent until a growth copies them }
    KeysShared: Boolean;
    { a dictionary's: the last major collection that found an object with it, or the next one for a shape made
      since the last; a dictionary no object has is retired (RetireDeadShapes) }
    LiveMajor: Integer;
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
{ D's integer part, a zero signed as D (Int's zero is +0 on some targets: Win64's RTL); NaN and infinities as
  they are }
function DTrunc(D: Double): Double;
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
  { execution limits (Goccia.VM LimitsArm): Interrupt is set, by the timeout thread or the instruction count, once a
    limit is spent; from then on every back edge and call aborts, Aborting is set and no handler runs (catch or
    finally) until the run is over. AbortKind: 0 none, 1 timeout, 2 instruction limit,
    3 memory limit (GCLimitHook). }
  Interrupt: LongInt;
  Aborting: Boolean;
  AbortKind: Integer;
  NurseryStart, NurseryTop, NurseryLimit: PByte;
  GCRootScanner: procedure;   { VM visits its roots with GCVisit }
  GCMinorCount, GCMajorCount: Integer;
  GCEpoch: Integer;   { collections of either kind (GCEpochNow) }
  { Goccia.VM's typed array length fast path: [0] %TypedArray%.prototype, [1 + kind] each kind's prototype (per realm);
    the kinds' entries are cleared (undefined) once the length property or the prototype of any of them changes }
  TAWatched: array[0..TA_WATCHED_KINDS] of TValue;
  GCPromotedBytes, GCOldBytes, GCOldBytesAtMajor: Int64;
  { the bytes allocated before the nursery's used part (HeapAllocatedBytes), and the most the heap held at a
    collection or a direct old-space allocation (HeapPeakBytes) }
  GCAllocatedBytes, GCPeakBytes: Int64;
  { the objects NewObject made on this thread while CountObjects is set (the profiler's allocations) }
  ObjectsMade: Int64;

function CurrentInterruptWord: PLongInt; noinline;
procedure HeapInit(NurseryBytes: SizeInt);
{ The engines a thread has had: once-per-engine set-up (registering a root) keeps the generation it ran for, so a
  thread's next engine (after EngineDone) runs it again. FirstInEngine: whether Gen is not this engine's yet (and
  makes it so); SameEngine: whether it is. Not inline (a threadvar read across units, tlsscan.py). }
function FirstInEngine(var Gen: Integer): Boolean; noinline;
function SameEngine(Gen: Integer): Boolean; noinline;
{ frees this thread's heap: every cell, atom and shape; nothing of the heap may be used after it }
procedure HeapDone;
function AllocCell(Size: SizeInt; Kind: Byte): PCell;
function AllocOld(Size: SizeInt; Kind: Byte): PCell;
procedure GCVisit(var V: TValue); inline;
procedure GCVisitPtr(var P: Pointer);
procedure CollectMinor;
procedure CollectMajor;
{ the nursery's bound under HeapCeiling: no more than the room the old space leaves under it (at least
  NURSERY_MIN_ROOM), so that the collection that tests the limit runs before the heap can pass it. Never below the
  cells allocated already (IsYoung tests against the bound); a minor collection applies it to the emptied nursery }
procedure NurseryFitCeiling;
const
  NURSERY_MIN_ROOM = 64 * 1024;
threadvar
  { Goccia.gc.maxBytes (--max-memory, goccia.json max-memory): an allocation charged against it (HeapReserve) that
    would take the heap past it collects, tests again and is refused; 0 none. Each thread's engine its own: a
    runner's workers run files with different limits side by side (an agent's thread takes its starter's) }
  HeapCeiling: Int64;
{ the old generation and the nursery's used part (Goccia.gc.bytesAllocated) }
function HeapUsedBytes: Int64;
{ every byte the heap has allocated since HeapInit, garbage included (a CLI report's allocatedDuringRunBytes) }
function HeapAllocatedBytes: Int64;
{ the most the heap has held since HeapInit, garbage included until a collection frees it (peakLiveBytes) }
function HeapPeakBytes: Int64;
{ whether Bytes more fit under HeapCeiling, after a full collection when they do not yet: the charge of an
  allocation a RangeError refuses (an ArrayBuffer's bytes). Collects: the caller's values stay rooted }
function HeapReserve(Bytes: Int64): Boolean;
{ Native storage owned by one engine. Charge growth before allocating and release it when the owner frees it.
  A match holding an unrooted heap pointer can ask for a check that never collects. }
type
  THeapExternalCharge = record
  private
    FBytes: Int64;
  public
    function TryGrow(const ABytes: Int64; const ACanCollect: Boolean = True): Boolean;
    procedure Release(const ABytes: Int64);
    procedure Clear;
  end;
  PHeapExternalCharge = ^THeapExternalCharge;
  EHeapExternalMemoryLimit = class(Exception);
function HeapChargedExternalBytes: Int64;
const
  { what the native allocator adds to each block it hands out (FPC's heap manager: its size word, rounded up) }
  NATIVE_BLOCK_OVERHEAD = 16;
  { the least an array index stored past ArrayDenseMax costs, charged as it is added: its slot in the object's
    overflow array (8), its key and flags in the dictionary shape (9) and its entry in the shape's hash at half
    load (24); the index's atom comes on top the first time the index is used anywhere }
  SPARSE_INDEX_BYTES = 41;
{ The engine's native memory for property keys and object layouts, held outside the heap and charged against
  HeapCeiling with the external bytes: atoms, key atoms and symbols, which live as long as the engine
  (HeapPermanentBytes), and shapes with the key, flag and hash arrays they own, freed with their prototype or, for a
  dictionary shape, its object. Delta is what was allocated (negative: freed). A charge that takes the heap over
  HeapCeiling asks for a collection: the next nursery allocation runs it; a permanent charge that alone passes the
  ceiling, which no collection can undo, marks the refusal at once (GCLimitHook). Never collects or allocates }
procedure HeapNativeCharge(Delta: Int64; Permanent: Boolean);
function HeapNativeBytes: Int64;
function HeapPermanentBytes: Int64;
{ the bytes of a value array of Len slots (an array's elements), and of a string of Len code units, as the heap
  allocates them }
function ValueArrayBytes(Len: Int64): Int64; inline;
function StringBytes(Len: Int64): Int64; inline;
var
  { the bytes held outside the heap that count against HeapCeiling (SharedArrayBuffer blocks), for every thread }
  HeapExternalBytes: Int64 = 0;
  { some thread profiles its allocations: NewObject counts ObjectsMade }
  CountObjects: Boolean = False;
procedure HeapExternalAdd(Delta: Int64);
var
  { a full collection that leaves the heap and charged external bytes over HeapCeiling calls it (Goccia.VM: the run ends at
    its next back edge or call, past every handler); it must not allocate }
  GCLimitHook: procedure = nil;
  { a full collection that leaves them within HeapCeiling calls it (Goccia.VM: the script freed what a refusal asked it
    to); it must not allocate }
  GCLimitClearHook: procedure = nil;
{ weak references: GCWeakHook runs after marking/evacuation in every collection, before old space is swept.
  In it, GCSurvives says whether a cell lives on and updates P when the cell moved. The hook must not allocate. }
var GCWeakHook: procedure = nil;
{ like GCWeakHook, run right after it: Goccia.Weak's boxes (WeakRef, FinalizationRegistry) }
var GCWeakRefHook: procedure = nil;
{ ephemerons (Goccia.Weak's WeakMap tables): after marking drains, called again and again while it returns True (it
  visited more values, which were drained in between); then GCWeakRefHook. It may flag cells cfEphKey: the cells of
  those that marking reaches afterwards are listed (GCEphKeyCount / GCEphKeyAt, the original address of an evacuated
  cell) until GCEphKeysClear }
var GCEphemeronHook: function: Boolean = nil;
function GCEphKeyCount: Integer;
function GCEphKeyAt(I: Integer): Pointer;
procedure GCEphKeysClear;
{ P lies in the nursery (outside Goccia.Core: each threadvar read in a statement of its own) }
function GCIsYoung(P: Pointer): Boolean;
{ GCVisit for other units (not inlined: its threadvar reads stay in this unit) }
procedure GCVisitValue(var V: TValue);
{ like GCWeakHook, for releasing what dead cells held outside the heap (Goccia.Shared's buffer references,
  Goccia.FFI's tie rows): each hook added runs after every collection, in the order added }
type
  TGCFinalizeHook = procedure;
procedure AddGCFinalizeHook(Hook: TGCFinalizeHook);
function GCSurvives(var P: Pointer): Boolean;
function GCInMinor: Boolean;
procedure WriteBarrier(Container: PCell; V: TValue); inline;
procedure Remember(Container: PCell);

{ temp roots for native code: push values that must survive allocation }
threadvar
  { the values natives keep rooted while they allocate, as a stack: a mapping of TEMP_ROOTS_MAX values whose pages
    are committed as they are touched, so a native recursion that roots values at every level (a Proxy chain) has
    room long after the native stack guard (NativeStackLow) has stopped it }
  TempRoots: PValue;
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
{ IntAtom's atom when the text is interned already, else nil (no atom is made: no shape can hold the key) }
function IntAtomIfExists(I: Int64): PStr;
{ Key atoms no source text can name: kept out of the atom table, told apart by Hdr.Extra (kfSymbol: a
  Symbol's key, its Symbol in the payload; kfPrivate: a class's private name; kfInternal: an engine slot).
  Text is only a description. Ordinary keys (atoms) have Extra 0. }
const
  kfSymbol = 1; kfPrivate = 2; kfInternal = 4;
function NewKeyAtom(const Text: UnicodeString; Flags: Word): PStr;
{ a permanent cell allocated outside the atom tables (a Symbol, a BigInt a snapshot holds): HeapDone frees it with
  the engine's heap }
procedure AddPermanentCell(P: Pointer);
{ the internal keys under which the global object holds the using-declaration helpers (Goccia.Disposal), for the
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
{ S was retired by a major collection: its prototype died, so no object has it or can get it again }
function ShapeRetired(S: PShape): Boolean;
var
  { a major collection that retired shapes calls it before their records are freed, whose addresses a new shape
    can then take: everything that still names one forgets it (Goccia.VM: the templates' inline caches and constructor
    caches, the megamorphic cache, the compiled code's define and private-member caches, the optimizing tier's
    double shapes). Machine code's own shapes are pinned (TShape.Pinned) and never freed. The shapes the realms'
    intrinsics have (FunctionShape, EmptyShapeForArray, RegExpShape and the like) never retire: the realms keep
    their intrinsics. It must not allocate. }
  GCShapeSweepHook: procedure = nil;
function NurseryCellOK(V: TValue): Boolean;
procedure CheckRoot(const V: TValue; const Where: string; I: Integer);

{ ---- objects ---- }
var
  { bumped when an object used as a prototype (one with a root shape: Hdr.Extra) gains an accessor or a
    read-only property, or a new prototype: a cached add transition (Goccia.VM opSetProp) holds the epoch it was
    made in, as its proof that no setter or read-only property on the chain applies. Shared by the agents'
    threads (a global loads cheaper than a threadvar): another agent's bump only costs a cache miss. }
  ProtoEpoch: Integer;
  { Goccia.VM's: forgets the cached add transitions of this agent's code (a bump is rare: the walk is cheap enough) }
  ProtoChangeHook: procedure = nil;
threadvar
  { Goccia.VM's megamorphic get cache: (shape, key) entries for sites whose inline cache is full (nil until one is) }
  MegaICTab: Pointer;
  { no add transition is cached while a proxy is some object's prototype (its [[Set]] is the handler's) }
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
{ AddProperty's store once its new shape is known (NewShape: O's shape plus the property in its last slot, an
  add transition an inline cache recorded): the overflow slots grown as needed; can collect }
function AddPropertyTo(O: PObj; NewShape: PShape; V: TValue): Integer;
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

{ ---- the language profile ---- }
type
  { GocciaScript's recommended profile with the compatibility flags and unsafe options a host turned on. The
    zero value (Restricted False) is the whole language, as the conformance host and the preludes run it; a
    restricted profile turns on only the forms its flags name (docs/language.md, Compatibility Flags). Code
    compiles under the profile active when its source was parsed: a lazy source keeps its own. }
  TLangProfile = record
    Restricted: Boolean;
    Asi, VarDecl, FuncDecl, ForLoop, WhileLoop, LooseEq, NonStrict, ArgumentsObject, Labels, ForIn,
      { the Annex B surfaces the recommended profile leaves out: the globals escape and unescape (ADR 0177);
        read when the host is installed }
      AnnexB,
      WarningUnsupported, FunctionCtor, ShadowRealm, StrictTypes: Boolean;
  end;

{ the profile new code compiles under, through noinline routines (an inline threadvar access in another unit
  can get FPC 3.3.1's absolute relocation, tlsscan.py) }
function ActiveProfile: TLangProfile; noinline;
procedure SetActiveProfile(const P: TLangProfile); noinline;
{ the profile's switches, all on for an unrestricted one }
function ProfileNonStrict: Boolean; noinline;
function ProfileArgumentsObject: Boolean; noinline;
function ProfileFunctionCtor: Boolean; noinline;
{ types-as-comments annotations checked at run time (strict-types; off for an unrestricted profile) }
function ProfileStrictTypes: Boolean; noinline;
{ errors carry a stack trace (GocciaScript's hosts; not the conformance host) }
function ProfileStackTraces: Boolean; noinline;
{ the collections so far (minor and major): unchanged across a call, nothing moved or was freed meanwhile. Not
  inline: fpc-toolchain 3.3.1+4 can fold a second threadvar read of an expression into an absolute address }
function GCEpochNow: Integer; noinline;
{ O, an object other objects have as prototype, had its length property or prototype changed: when it is one of
  TAWatched, the typed array length fast path is off in this realm }
procedure TAWatchedChanged(O: PObj);

{$IFNDEF CPU64}
{ the RTL's 64-bit exchanges a 32-bit target lacks, by the compiler's 64-bit compare-and-swap (cmpxchg8b) }
function InterlockedCompareExchange64(var Target: Int64; NewValue, Comperand: Int64): Int64;
function InterLockedExchangeAdd64(var Target: Int64; Source: Int64): Int64;
function InterLockedExchange64(var Target: Int64; Source: Int64): Int64;
{$ENDIF}

{ ---- file paths ---- }
{ S names a file from the root: a leading /, and on Windows a leading \ or a drive (C:\ or C:/) }
function IsAbsoluteFilePath(const S: UnicodeString): Boolean;

{ ---- memory from the system ---- }
{ Size bytes of zeroed memory whose pages the system supplies on first touch (nil when it has none) }
function OSAllocZeroed(Size: PtrUInt): Pointer;
procedure OSFree(P: Pointer; Size: PtrUInt);

{ ---- the native stack ---- }
{ records how far this thread's native stack may grow (its bounds from the system, a margin kept for the
  error path); call it on the thread, before deep recursion is possible }
procedure StackGuardInit;
{ True once less than the margin of the native stack is left: a native recursion (a Proxy without traps around a
  prototype cycle, a chain of bound functions) then throws a RangeError instead of overflowing the stack }
function NativeStackLow: Boolean; noinline;
{ the address below which NativeStackLow is True (0: unguarded) }
function NativeGuardLimit: PtrUInt;
{ the guard moved Bytes further down the margin (a realm's set-up, which runs to a fixed depth); the old guard,
  for NativeGuardRestore }
function NativeGuardLend(Bytes: PtrUInt): PtrUInt; noinline;
procedure NativeGuardRestore(Old: PtrUInt); noinline;

{ ---- a host's environment: time zone and randomness ---- }
type
  { Math.random's next value in realm Realm (a host's seeded stream or provider); False with the provider's
    exception pending }
  THostRandomFn = function(Realm: Integer; out D: Double): Boolean of object;
  { a host's clocks (a provider module): Kind 0 the epoch time, 1 the monotonic time, in nanoseconds; False with
    the provider's exception pending }
  THostClockFn = function(Kind: Integer; out Ns: Int64): Boolean of object;
threadvar
  { the zone Date, Intl and Temporal.Now take in place of TZ and the system's ('' for none) }
  TimeZoneOverride: UnicodeString;
  { a host's Math.random (nil: the engine's own) }
  HostRandom: THostRandomFn;
  { a host's clocks (nil: the system's) }
  HostClock: THostClockFn;
  { performance.timeOrigin (epoch milliseconds) and the monotonic time performance.now() counts from
    (TimeOriginInit) }
  TimeOriginMs: Double;
  TimeOriginMonoNs: Int64;

{ ---- the host clock ---- }
{ a host's mocked clocks (fake timers): the epoch time in milliseconds Date, Temporal.Now and the like read, and
  the monotonic time performance.now reads; NaN puts the real clock back }
procedure SetClockOverride(EpochMs, MonoMs: Double); noinline;
{ True with Ms the mocked epoch time }
function EpochOverride(out Ms: Double): Boolean; noinline;
{ True with Ms the mocked monotonic time }
function MonoOverride(out Ms: Double): Boolean; noinline;
{ the epoch and monotonic times in nanoseconds a script reads under the mock: a host's clock, else the system's;
  False with the host clock's exception pending }
function HostEpochNs(out Ns: Int64): Boolean; noinline;
function HostMonoNs(out Ns: Int64): Boolean; noinline;
{ performance's time origin, taken as GocciaScript takes it: the monotonic time, the epoch time (the origin), the
  monotonic time again (performance.now() counts from the middle of the two); each read through the mock when
  there is one; False with the host clock's exception pending }
function TimeOriginInit: Boolean;

implementation

uses
  {$IFDEF UNIX}BaseUnix,{$ENDIF} NumericText, TimingUtils, NativeStackBounds, Goccia.ThreadCleanupRegistry, Goccia.NumberExponentiation,
  Goccia.EngineFault;

{ FPC 3.3.1 writes a call of a procedural threadvar as a direct call of the variable in its GNU assembler output
  (`call U_$...`, for the AFL build, -Aas): called through a local copy, it is called through a register }
procedure CallGCRootScanner;
var H: procedure;
begin
  H := GCRootScanner;
  H;
end;


{$IFDEF MSWINDOWS}
{ (declared here: the Windows unit's names would hide SysUtils' in this unit) }
const
  MEM_COMMIT = $1000;
  MEM_RESERVE = $2000;
  MEM_RELEASE = $8000;
  PAGE_READWRITE = 4;
function VirtualAlloc(Address: Pointer; Size: PtrUInt; AllocationType, Protect: LongWord): Pointer; stdcall;
  external 'kernel32' name 'VirtualAlloc';
function VirtualFree(Address: Pointer; Size: PtrUInt; FreeType: LongWord): LongBool; stdcall;
  external 'kernel32' name 'VirtualFree';
{$ENDIF}

{$IFNDEF CPU64}
function InterlockedCompareExchange64(var Target: Int64; NewValue, Comperand: Int64): Int64;
begin
  Result := AtomicCmpExchange(Target, NewValue, Comperand);
end;

function InterLockedExchangeAdd64(var Target: Int64; Source: Int64): Int64;
begin
  repeat
    Result := Target;
  until InterlockedCompareExchange64(Target, Result + Source, Result) = Result;
end;

function InterLockedExchange64(var Target: Int64; Source: Int64): Int64;
begin
  repeat
    Result := Target;
  until InterlockedCompareExchange64(Target, Source, Result) = Result;
end;
{$ENDIF}

function IsAbsoluteFilePath(const S: UnicodeString): Boolean;
begin
  if S = '' then Exit(False);
  Result := S[1] = '/';
  {$IFDEF MSWINDOWS}
  if not Result then
    Result := (S[1] = '\') or ((Length(S) >= 3) and (S[2] = ':') and (S[3] in ['\', '/']) and
      (((S[1] >= 'A') and (S[1] <= 'Z')) or ((S[1] >= 'a') and (S[1] <= 'z'))));
  {$ENDIF}
end;

function OSAllocZeroed(Size: PtrUInt): Pointer;
begin
  {$IFDEF UNIX}
  Result := fpmmap(nil, Size, PROT_READ or PROT_WRITE, MAP_PRIVATE or MAP_ANONYMOUS, -1, 0);
  if Result = MAP_FAILED then Result := nil;
  {$ELSEIF DEFINED(MSWINDOWS)}
  Result := VirtualAlloc(nil, Size, MEM_RESERVE or MEM_COMMIT, PAGE_READWRITE);
  {$ELSE}
  Result := AllocMem(Size);
  {$ENDIF}
end;

procedure OSFree(P: Pointer; Size: PtrUInt);
begin
  if P = nil then Exit;
  {$IFDEF UNIX}
  fpmunmap(P, Size);
  {$ELSEIF DEFINED(MSWINDOWS)}
  VirtualFree(P, 0, MEM_RELEASE);
  {$ELSE}
  FreeMem(P);
  {$ENDIF}
end;

const
  { what the native stack keeps for the error path and the runtime once the guard trips }
  NATIVE_STACK_MARGIN = 512 * 1024;
  { how far above the guard the shared nesting checks refuse: Goccia.VM's REENTRY_HEADROOM (128 KiB) and 64 KiB more for
    the frames between a parse and the re-entry that runs it }
  NESTING_CHECK_HEADROOM = 192 * 1024;

threadvar
  NativeStackLimit: PtrUInt;   { 0: unguarded }
  CurProfile: TLangProfile;
  { the mocked clocks, each held as its value + 1 so the zero a new thread starts with means none }
  EpochOverrideP1, MonoOverrideP1: Double;
  EpochOverridden, MonoOverridden: Boolean;

procedure StackGuardInit;
var Low, Size: NativeUInt;
begin
  NativeStackLimit := 0;
  if TryGetThreadStackBounds(Low, Size) and (Size > 2 * NATIVE_STACK_MARGIN) then
  begin
    NativeStackLimit := Low + NATIVE_STACK_MARGIN;
    { the shared parser's and pattern compiler's nesting checks (Goccia.NestingLimit): above Goccia.VM's re-entry floor
      (REENTRY_HEADROOM above this guard), with room to run what they produce, so code nested too deep for the stack
      left is a SyntaxError where it is parsed, not a RangeError when it runs }
    SetNativeStackLimit(NativeStackLimit + NESTING_CHECK_HEADROOM);
  end;
end;

function NativeGuardLend(Bytes: PtrUInt): PtrUInt;
begin
  Result := NativeStackLimit;
  if NativeStackLimit > Bytes then NativeStackLimit := NativeStackLimit - Bytes;
end;

procedure NativeGuardRestore(Old: PtrUInt);
begin
  NativeStackLimit := Old;
end;

function NativeGuardLimit: PtrUInt;
begin
  Result := NativeStackLimit;
end;

function NativeStackLow: Boolean;
var Here: Byte;
begin
  Result := PtrUInt(@Here) < NativeStackLimit;
end;

procedure SetClockOverride(EpochMs, MonoMs: Double);
begin
  EpochOverridden := not IsNan(EpochMs);
  if EpochOverridden then EpochOverrideP1 := EpochMs + 1;
  MonoOverridden := not IsNan(MonoMs);
  if MonoOverridden then MonoOverrideP1 := MonoMs + 1;
end;

function EpochOverride(out Ms: Double): Boolean;
begin
  Result := EpochOverridden;
  if Result then Ms := EpochOverrideP1 - 1 else Ms := 0;
end;

function MonoOverride(out Ms: Double): Boolean;
begin
  Result := MonoOverridden;
  if Result then Ms := MonoOverrideP1 - 1 else Ms := 0;
end;

function HostEpochNs(out Ns: Int64): Boolean;
var F: THostClockFn;
begin
  F := HostClock;
  if Assigned(F) then Exit(F(0, Ns));
  Ns := GetEpochNanoseconds;
  Result := True;
end;

function HostMonoNs(out Ns: Int64): Boolean;
var F: THostClockFn;
begin
  F := HostClock;
  if Assigned(F) then Exit(F(1, Ns));
  Ns := GetNanoseconds;
  Result := True;
end;

function TimeOriginInit: Boolean;
var M0, E, M1: Int64; Ms: Double;
begin
  Result := False;
  if MonoOverride(Ms) then M0 := Trunc(Ms * 1000000) else if not HostMonoNs(M0) then Exit;
  if EpochOverride(Ms) then E := Trunc(Ms * 1000000) else if not HostEpochNs(E) then Exit;
  if MonoOverride(Ms) then M1 := Trunc(Ms * 1000000) else if not HostMonoNs(M1) then Exit;
  TimeOriginMs := E / 1000000;
  TimeOriginMonoNs := M0 + (M1 - M0) div 2;
  Result := True;
end;

function ActiveProfile: TLangProfile;
begin
  Result := CurProfile;
end;

procedure SetActiveProfile(const P: TLangProfile);
begin
  CurProfile := P;
end;

function ProfileNonStrict: Boolean;
begin
  Result := not CurProfile.Restricted or CurProfile.NonStrict;
end;

function ProfileArgumentsObject: Boolean;
begin
  Result := not CurProfile.Restricted or CurProfile.ArgumentsObject;
end;

function ProfileFunctionCtor: Boolean;
begin
  Result := not CurProfile.Restricted or CurProfile.FunctionCtor;
end;

function ProfileStrictTypes: Boolean;
begin
  Result := CurProfile.Restricted and CurProfile.StrictTypes;
end;

function GCEpochNow: Integer;
begin
  Result := GCEpoch;
end;

procedure TAWatchedChanged(O: PObj);
var I, J: Integer;
begin
  for I := 0 to TA_WATCHED_KINDS do
    if IsObject(TAWatched[I]) and (ObjOf(TAWatched[I]) = O) then
    begin
      for J := 1 to TA_WATCHED_KINDS do TAWatched[J] := VUndefined;
      Exit;
    end;
end;

function ProfileStackTraces: Boolean;
begin
  Result := CurProfile.Restricted;
end;


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
{$IFNDEF DARWIN}
function libm_pow(X, Y: Double): Double; cdecl; external GOCCIA_LIBM name 'pow';
{$ENDIF}
function libm_fmod(X, Y: Double): Double; cdecl; external GOCCIA_LIBM name 'fmod';

function DTrunc(D: Double): Double;
begin
  if IsNan(D) or IsInfinite(D) then Exit(D);
  Result := Int(D);
  { a zero takes D's sign bit: trunc(-0.5) and trunc(-0) are -0 }
  if Result = 0 then PQWord(@Result)^ := PQWord(@D)^ and QWord($8000000000000000);
end;

function DFloor(D: Double): Double;
begin
  if IsNan(D) or IsInfinite(D) then Exit(D);
  Result := DTrunc(D);
  if Result > D then Result := Result - 1;
end;

function DCeil(D: Double): Double;
begin
  if IsNan(D) or IsInfinite(D) then Exit(D);
  Result := DTrunc(D);
  if Result < D then
  begin
    Result := Result + 1;
    if Result = 0 then PQWord(@Result)^ := QWord($8000000000000000);   { ceil(-0.5) is -0 }
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

{ ES2026 6.1.6.1.3 Number::exponentiate. Exponent -1 and, on macOS, every exponent through
  Goccia.NumberExponentiation, a port of the pow glibc and
  musl ship (Arm optimized-routines), as macOS's libm rounds a subnormal result differently; elsewhere the system's
  pow, which is faster (glibc ships that algorithm), except where ECMAScript differs: a NaN exponent, and |base| 1
  with an infinite exponent, are NaN }
function NumberExponentiate(B, E: Double): Double;
begin
  if IsNan(E) then Exit(NaN);
  if IsInfinite(E) and (Abs(B) = 1) then Exit(NaN);
  { a positive finite base to the power 0.5: its square root, correctly rounded on every target (the Universal
    CRT's pow is not) }
  if (E = 0.5) and (B > 0) and not IsInfinite(B) then Exit(Sqrt(B));
  { exponent -1: the correctly rounded 1 / x, which libm's pow (within an ULP) misses for about one base in a
    thousand; Goccia.NumberExponentiation divides, or for |x| >= 2^1022 rounds the reciprocal in integer
    arithmetic }
  if E = -1 then Exit(NumberExponentiation(B, E));
  {$IFDEF DARWIN}
  Result := NumberExponentiation(B, E);
  {$ELSE}
  Result := libm_pow(B, E);
  {$ENDIF}
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
  EphKeys: array of Pointer;   { cells flagged cfEphKey that marking reached in this collection }
  EphKeyTop: Integer;
  RememberedList: array of PCell;
  RememberedTop: Integer;
  InMinor: Boolean;
  { HeapNativeCharge's: every structure's bytes, the permanent ones among them; ChargeCheckAt the heap and charged
    bytes past which a charge asks for a collection (ChargePending), set by each major collection }
  StructBytes, PermBytes, ChargeCheckAt: Int64;
  ChargePending: Boolean;

{ a major collection runs when old space has grown by the larger of this and its size after the last one }
var GCMajorFloor: Int64 = 64 * 1024 * 1024; GCMajorGrowthPct: Int64 = 100;

threadvar
  EngineGen: Integer;

function CurrentInterruptWord: PLongInt;
begin
  Result := @Interrupt;
end;

function FirstInEngine(var Gen: Integer): Boolean;
var G: Integer;
begin
  G := EngineGen;
  Result := Gen <> G;
  Gen := G;
end;

function SameEngine(Gen: Integer): Boolean;
var G: Integer;
begin
  G := EngineGen;
  Result := Gen = G;
end;

procedure HeapInit(NurseryBytes: SizeInt);
var I, C: Integer;
begin
  Inc(EngineGen);
  if GetEnvironmentVariable('GOCCIA_MAJOR_FLOOR_MB') <> '' then
    GCMajorFloor := Int64(StrToInt(GetEnvironmentVariable('GOCCIA_MAJOR_FLOOR_MB'))) * 1024 * 1024;
  if GetEnvironmentVariable('GOCCIA_MAJOR_GROWTH_PCT') <> '' then
    GCMajorGrowthPct := StrToInt(GetEnvironmentVariable('GOCCIA_MAJOR_GROWTH_PCT'));
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
  TempRoots := OSAllocZeroed(TEMP_ROOTS_MAX * SizeOf(TValue));
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

function OverCeiling(Size: SizeInt): Boolean; forward;
function ShapesDueMajor: Boolean; forward;

function AllocCell(Size: SizeInt; Kind: Byte): PCell;
begin
  Size := (Size + 7) and not 7;
  if Size < 16 then Size := 16;
  { up to 1/16 of the nursery a cell starts young: big temporaries (string concatenation results)
    then die in a minor collection instead of filling old space }
  if Size > (NurseryLimit - NurseryStart) div 16 then
  begin
    { direct old-space allocation still counts toward the major trigger }
    if (GCOldBytes + Size - GCOldBytesAtMajor > Max(GCMajorFloor, GCOldBytesAtMajor * GCMajorGrowthPct div 100)) or
       OverCeiling(Size) then
    begin
      CollectMinor;
      CollectMajor;
    end;
    Result := AllocOld(Size, Kind);
    { born old: the stores that fill it (a rest parameter's or an arguments array's elements, copied without a
      write barrier as into a young cell) may put young values in it, so it starts remembered }
    if (Kind = ckValueArray) or (Kind = ckObject) then Remember(Result);
    Inc(GCAllocatedBytes, Result^.Size);
    if HeapUsedBytes > GCPeakBytes then GCPeakBytes := HeapUsedBytes;
    { a cell the full collection could not make room for: the limit is spent (the run ends at its next back edge
      or call), as when a collection leaves the heap over it }
    if (HeapCeiling > 0) and Assigned(GCLimitHook) and (AbortKind <> 3) and
       (GCOldBytes + HeapChargedExternalBytes > HeapCeiling) then
      GCLimitHook;
    Exit;
  end;
  if (NurseryTop + Size > NurseryLimit) or GCStress then
  begin
    CollectMinor;
    if (GCOldBytes - GCOldBytesAtMajor > Max(GCMajorFloor, GCOldBytesAtMajor * GCMajorGrowthPct div 100)) or
       (GCStress and (GCMinorCount mod 8 = 0)) or OverCeiling(0) or ShapesDueMajor then
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

function GCIsYoung(P: Pointer): Boolean; noinline;
var Lo, Hi: PByte;
begin
  Lo := NurseryStart;
  Hi := NurseryLimit;
  Result := (PByte(P) >= Lo) and (PByte(P) < Hi);
end;

procedure GCVisitValue(var V: TValue); noinline;
begin
  GCVisit(V);
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

procedure NoteEphKey(P: Pointer); noinline;
var N: Integer;
begin
  N := EphKeyTop;
  if N >= Length(EphKeys) then SetLength(EphKeys, N * 2 + 256);
  EphKeys[N] := P;
  EphKeyTop := N + 1;
end;

function GCEphKeyCount: Integer;
begin
  Result := EphKeyTop;
end;

function GCEphKeyAt(I: Integer): Pointer;
begin
  Result := EphKeys[I];
end;

procedure GCEphKeysClear;
begin
  EphKeyTop := 0;
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
    if (C^.Flags and cfEphKey) <> 0 then NoteEphKey(C);
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
    if (C^.Flags and cfEphKey) <> 0 then
    begin
      C^.Flags := C^.Flags and not cfEphKey;
      NoteEphKey(C);
    end;
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
  {$PUSH}{$I-}   { the finding is written with I/O checks off: a failed write cannot raise }
  BeginEngineCheckReport;
  Write(ErrOutput, 'scan: object ', IntToHex(PtrUInt(O), 16), ' okind ', O^.OKind, ' oflags ', O^.OFlags,
    ' aux1 ', IntToHex(O^.Aux1, 16), ' aux2 ', IntToHex(O^.Aux2, 16), ' not a nursery cell');
  if (O^.OKind = 2) and Assigned(GCDescribeNative) then Write(ErrOutput, ' fn ', GCDescribeNative(O^.Native));
  WriteLn(ErrOutput, ' minor ', GCMinorCount);
  StopOnEngineCheck('collector scan');
  {$POP}
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
        begin
          GCVisit(O^.Shape^.Proto);
          { a dictionary lives while its object does (RetireDeadShapes) }
          if O^.Shape^.IsDict then O^.Shape^.LiveMajor := GCMajorCount;
        end;
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
procedure RetireDeadShapes; forward;
procedure ShapesNoteMajor; forward;
procedure VerifyHeap(const Phase: string); forward;

function GCInMinor: Boolean;
begin
  Result := InMinor;
end;

const
  MAX_GC_FINALIZE_HOOKS = 8;
var
  { process-wide (each hook is a procedure over its own threadvars): a slot is written once, before the count that
    publishes it, so a collection reads them without the lock }
  GCFinalizeHooks: array[0..MAX_GC_FINALIZE_HOOKS - 1] of TGCFinalizeHook;
  GCFinalizeHookCount: Integer;
  GCFinalizeHooksLock: TRTLCriticalSection;

procedure AddGCFinalizeHook(Hook: TGCFinalizeHook);
var I: Integer;
begin
  EnterCriticalSection(GCFinalizeHooksLock);
  try
    for I := 0 to GCFinalizeHookCount - 1 do
      if @GCFinalizeHooks[I] = @Hook then Exit;
    if GCFinalizeHookCount = MAX_GC_FINALIZE_HOOKS then raise Exception.Create('too many GC finalize hooks');
    GCFinalizeHooks[GCFinalizeHookCount] := Hook;
    System.WriteBarrier;
    Inc(GCFinalizeHookCount);
  finally
    LeaveCriticalSection(GCFinalizeHooksLock);
  end;
end;

procedure RunGCFinalizeHooks;
var I, N: Integer;
begin
  N := GCFinalizeHookCount;
  System.ReadBarrier;
  for I := 0 to N - 1 do GCFinalizeHooks[I]();
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

procedure NurseryFitCeiling;
var Room, Size: Int64; Lim, Top: PByte;
begin
  Size := NurserySize;
  Room := Size;
  if HeapCeiling > 0 then
  begin
    Room := HeapCeiling - GCOldBytes;
    Room := Room - HeapChargedExternalBytes;
    if Room < NURSERY_MIN_ROOM then Room := NURSERY_MIN_ROOM;
    if Room > Size then Room := Size;
  end;
  Lim := NurseryStart + Room;
  Top := NurseryTop;
  if Lim < Top then Lim := Top;
  NurseryLimit := Lim;
end;

procedure CollectMinor;
var I: Integer; C: PCell; Used: Int64;
begin
  Used := NurseryTop - NurseryStart;
  Inc(GCAllocatedBytes, Used);
  if GCOldBytes + Used > GCPeakBytes then GCPeakBytes := GCOldBytes + Used;
  { the collection a structure charge asked for: the major one it needs follows when the heap stays over the
    ceiling (OverCeiling) }
  ChargePending := False;
  Inc(GCMinorCount);
  Inc(GCEpoch);
  if GCVerify then VerifyHeap('barrier');   { every old cell pointing into the nursery is remembered }
  InMinor := True;
  GrayTop := 0;
  if Assigned(GCRootScanner) then CallGCRootScanner;
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
  if Assigned(GCEphemeronHook) then
    while GCEphemeronHook() do DrainGray;
  FixOpenUpvalLinks;
  if Assigned(GCWeakHook) then GCWeakHook;
  if Assigned(GCWeakRefHook) then GCWeakRefHook;
  RunGCFinalizeHooks;
  InMinor := False;
  if GCVerify then
  begin
    VerifyShapesAfterMinor;
    VerifyHeap('minor');
    { every live cell left the nursery: poisoned, so a pointer that still leads into it (a root the collection
      did not scan) meets a header no cell has at the next one }
    FillChar(NurseryStart^, NurseryLimit - NurseryStart, $DB);
  end;
  NurseryTop := NurseryStart;
  NurseryFitCeiling;
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

{ each threadvar read in a statement of its own: FPC 3.3.1 can drop the %fs prefix of a second one in an
  expression }
function HeapUsedBytes: Int64; noinline;
var Old: Int64; Top, Start: PByte;
begin
  Old := GCOldBytes;
  Top := NurseryTop;
  Start := NurseryStart;
  Result := Old + (Top - Start);
end;

function HeapAllocatedBytes: Int64; noinline;
var Top, Start: PByte;
begin
  Top := NurseryTop;
  Start := NurseryStart;
  Result := GCAllocatedBytes + (Top - Start);
end;

function HeapPeakBytes: Int64; noinline;
begin
  Result := Max(GCPeakBytes, HeapUsedBytes);
end;

threadvar
  EngineExternalBytes: Int64;

function HeapChargedExternalBytes: Int64; noinline;
var EngineBytes, Struct: Int64;
begin
  EngineBytes := EngineExternalBytes;
  Struct := StructBytes;
  Result := EngineBytes + Struct + HeapExternalBytes;
end;

function ValueArrayBytes(Len: Int64): Int64; inline;
begin
  if Len < 1 then Len := 1;
  Result := SizeOf(TValueArray) - SizeOf(TValue) + Len * SizeOf(TValue);
end;

function StringBytes(Len: Int64): Int64; inline;
begin
  Result := SizeOf(TStr) - SizeOf(WideChar) + Len * SizeOf(WideChar);
end;

function HeapNativeBytes: Int64;
begin
  Result := StructBytes;
end;

function HeapPermanentBytes: Int64;
begin
  Result := PermBytes;
end;

procedure HeapNativeCharge(Delta: Int64; Permanent: Boolean);
var Ceiling, Now: Int64; Top: PByte;
begin
  Inc(StructBytes, Delta);
  if Permanent then Inc(PermBytes, Delta);
  Ceiling := HeapCeiling;
  if (Delta <= 0) or (Ceiling <= 0) or (AbortKind = 3) then Exit;
  Now := HeapUsedBytes + HeapChargedExternalBytes;
  if (Now > Ceiling) and (Now > ChargeCheckAt) and not ChargePending then
  begin
    { the next nursery allocation runs the collection (as one over NurseryFitCeiling's bound would) }
    ChargePending := True;
    Top := NurseryTop;
    NurseryLimit := Top;
  end;
  { what lives as long as the engine already fills the ceiling: no collection can make room }
  if Permanent and (PermBytes + HeapExternalBytes > Ceiling) and Assigned(GCLimitHook) then
    GCLimitHook;
end;


function HeapHasRoom(Bytes: Int64): Boolean;
var Ceiling: Int64;
begin
  Ceiling := HeapCeiling;
  Result := (Ceiling <= 0) or (Bytes <= Ceiling - HeapUsedBytes - HeapChargedExternalBytes);
end;

function HeapReserve(Bytes: Int64): Boolean;
begin
  if HeapHasRoom(Bytes) then Exit(True);
  { a collection also releases the blocks of SharedArrayBuffers nothing reaches }
  CollectMinor;
  CollectMajor;
  Result := HeapHasRoom(Bytes);
end;

function THeapExternalCharge.TryGrow(const ABytes: Int64; const ACanCollect: Boolean): Boolean;
begin
  if ACanCollect then Result := HeapReserve(ABytes) else Result := HeapHasRoom(ABytes);
  if not Result then Exit;
  Inc(EngineExternalBytes, ABytes);
  Inc(FBytes, ABytes);
end;

procedure THeapExternalCharge.Release(const ABytes: Int64);
begin
  Dec(EngineExternalBytes, ABytes);
  Dec(FBytes, ABytes);
end;

procedure THeapExternalCharge.Clear;
begin
  Release(FBytes);
end;

procedure HeapExternalAdd(Delta: Int64);
begin
  InterLockedExchangeAdd64(HeapExternalBytes, Delta);
end;

{ whether Size more bytes of old space take the heap past HeapCeiling: then a full collection runs first, and the
  ceiling is enforced after it; not once the limit is spent (AbortKind 3), when the run is already ending }
function OverCeiling(Size: SizeInt): Boolean;
var Old: Int64; Kind: Integer;
begin
  if HeapCeiling <= 0 then Exit(False);
  Kind := AbortKind;
  if Kind = 3 then Exit(False);
  Old := GCOldBytes;
  Result := Old + Size + HeapChargedExternalBytes > HeapCeiling;
end;

procedure CollectMajor;
var I: Integer; Left: Int64;
begin
  { called right after a minor collection: the nursery is empty }
  Inc(GCMajorCount);
  Inc(GCEpoch);
  InMinor := False;
  GrayTop := 0;
  if Assigned(GCRootScanner) then CallGCRootScanner;
  for I := 0 to TempTop - 1 do GCVisit(TempRoots[I]);
  ScanShapesForGC;
  DrainGray;
  if Assigned(GCEphemeronHook) then
    while GCEphemeronHook() do DrainGray;
  if Assigned(GCWeakHook) then GCWeakHook;
  if Assigned(GCWeakRefHook) then GCWeakRefHook;
  RunGCFinalizeHooks;
  RetireDeadShapes;
  ShapesNoteMajor;
  SweepPages;
  RememberedTop := 0;
  GCOldBytesAtMajor := GCOldBytes;
  if GCVerify then VerifyHeap('major');
  { a structure charge asks for the next collection once the heap and the charges pass the ceiling, or, when this
    one leaves them past it, once they have grown by a sixteenth of it more: a charge after each property added
    while a refusal is on its way to a safe point does not collect each time }
  ChargePending := False;
  Left := GCOldBytesAtMajor + HeapChargedExternalBytes;
  if (HeapCeiling > 0) and (Left > HeapCeiling) then
    ChargeCheckAt := Left + Max(Int64(NURSERY_MIN_ROOM), HeapCeiling div 16)
  else
    ChargeCheckAt := HeapCeiling;
  { what survived does not fit: the run ends (the nursery is empty, so the old space is the heap) }
  if (HeapCeiling > 0) and Assigned(GCLimitHook) and (Left > HeapCeiling) then
    GCLimitHook
  else if Assigned(GCLimitClearHook) then
    GCLimitClearHook;
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
  { more pops than pushes: a native's root bookkeeping is off. EInvalidPointer, an engine fault (RunError would halt
    the process from this thread instead, with exit status 204 and no report) }
  if TempTop < 0 then RaiseRuntimeError(204);
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
  { the table only grows: its new slots are the atoms' too }
  HeapNativeCharge(Int64(Length(AtomTable) - Length(Old)) * SizeOf(PStr), True);
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
  HeapNativeCharge(Size + NATIVE_BLOCK_OVERHEAD, True);
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
  PermanentCells: array of Pointer;
  PermanentCellCount: Integer;

procedure AddPermanentCell(P: Pointer);
begin
  if PermanentCellCount >= Length(PermanentCells) then
  begin
    HeapNativeCharge(Int64(PermanentCellCount + 64) * SizeOf(Pointer), True);
    SetLength(PermanentCells, PermanentCellCount * 2 + 64);
  end;
  PermanentCells[PermanentCellCount] := P;
  Inc(PermanentCellCount);
end;

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
  HeapNativeCharge(Size + NATIVE_BLOCK_OVERHEAD, True);
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
  if KeyAtomCount >= Length(KeyAtoms) then
  begin
    HeapNativeCharge(Int64(KeyAtomCount + 64) * SizeOf(PStr), True);
    SetLength(KeyAtoms, KeyAtomCount * 2 + 64);
  end;
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
      HeapNativeCharge(SizeOf(TIntAtoms) + NATIVE_BLOCK_OVERHEAD, True);
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

function IntAtomIfExists(I: Int64): PStr;
var Buf: array[0..23] of WideChar; P, N: Integer; U: QWord;
begin
  if (I >= 0) and (I <= High(TIntAtoms)) and (SmallIntAtoms <> nil) and (SmallIntAtoms^[I] <> nil) then
    Exit(SmallIntAtoms^[I]);
  P := Length(Buf);
  if I < 0 then U := QWord(0) - QWord(I) else U := QWord(I);
  repeat
    Dec(P);
    Buf[P] := WideChar(48 + U mod 10);
    U := U div 10;
  until U = 0;
  if I < 0 then begin Dec(P); Buf[P] := '-'; end;
  N := Length(Buf) - P;
  Result := AtomFind(@Buf[P], N, HashChars(@Buf[P], N));
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

const
  { a retired shape's prototype (RetireDeadShapes): a special value no script value or prototype equals }
  VRetiredProto = TAG_SPECIAL or $DEAD;
  { a prototype object's Hdr.Extra: its root shape table index + 1 for the first ROOT_DIRECT, ROOT_IN_HASH past them
    (RootHash finds those), 0 for an object that never was a prototype }
  ROOT_DIRECT = 65534;
  ROOT_IN_HASH = 65535;

threadvar
  AllShapes: array of PShape;
  ShapeCount: Integer;
  RootShapes: array of TRootShapeEntry;
  RootShapeCount: Integer;
  { the shapes RetireDeadShapes took out of AllShapes, freed with the heap }
  RetiredShapes: array of PShape;
  RetiredCount: Integer;
  RootHash: array of Integer;
  RootHashMask, RootHashUsed: Integer;
  RootHashStale: Boolean;

threadvar
  { shapes made since HeapInit, and that count and the shapes left at the last major collection }
  ShapesMade, ShapesMadeAtMajor, ShapesLiveAtMajor: Int64;

const
  { shape records and their arrays live outside the collected heap, so old space alone would not start a major
    collection however many dead prototypes' shapes pile up: a minor one also runs a major one once the shapes
    made since the last outnumber those it left, and at least SHAPES_MAJOR_FLOOR (a doubling, as old space's) }
  SHAPES_MAJOR_FLOOR = 65536;

function ShapesDueMajor: Boolean;
var Made, AtMajor, Live: Int64;
begin
  Made := ShapesMade;
  AtMajor := ShapesMadeAtMajor;
  Live := ShapesLiveAtMajor;
  Result := Made - AtMajor > Max(SHAPES_MAJOR_FLOOR, Live);
end;

procedure ShapesNoteMajor;
var N: Integer;
begin
  N := ShapeCount;
  ShapesLiveAtMajor := N;
  ShapesMadeAtMajor := ShapesMade;
end;

threadvar
  { the last version a shape took (TShape.Version) }
  ShapeVersionNext: Cardinal;

const
  { a shape record and its entry in AllShapes }
  SHAPE_RECORD_BYTES = SizeOf(TShape) + NATIVE_BLOCK_OVERHEAD + SizeOf(PShape);
  { a dynamic array's header (its reference count and length) and its block }
  DYN_ARRAY_BYTES = 2 * SizeOf(PtrInt) + NATIVE_BLOCK_OVERHEAD;

function DynBytes(Len, ElemSize: SizeInt): Int64; inline;
begin
  if Len = 0 then Result := 0 else Result := Int64(Len) * ElemSize + DYN_ARRAY_BYTES;
end;

{ charges S for what it holds now: its record and the arrays it owns (not Keys and PFlags while its parent lends them) }
procedure ShapeRecharge(S: PShape);
var N: Int64;
begin
  N := SHAPE_RECORD_BYTES + DynBytes(Length(S^.Edges), SizeOf(TShapeEdge)) + DynBytes(Length(S^.EdgeHash), SizeOf(Integer)) +
    DynBytes(Length(S^.HashKeys), SizeOf(PStr)) + DynBytes(Length(S^.HashSlots), SizeOf(Integer)) +
    DynBytes(Length(S^.ProtoChild), SizeOf(TValue) + SizeOf(PShape));
  if not S^.KeysShared then N := N + DynBytes(Length(S^.Keys), SizeOf(PStr)) + DynBytes(Length(S^.PFlags), 1);
  if N <> S^.Charged then
  begin
    HeapNativeCharge(N - S^.Charged, False);
    S^.Charged := N;
  end;
end;

procedure ShapeRecFree(S: PShape); inline;
begin
  HeapNativeCharge(-S^.Charged, False);
  Dispose(S);
end;

function ShapeRetired(S: PShape): Boolean;
begin
  Result := S^.Proto = VRetiredProto;
end;

function NewShape(Parent: PShape; Proto: TValue): PShape;
begin
  Inc(ShapesMade);
  New(Result);
  FillChar(Result^, SizeOf(TShape), 0);
  Result^.Proto := Proto;
  Result^.Parent := Parent;
  Result^.HashMask := -1;
  Inc(ShapeVersionNext);
  Result^.Version := ShapeVersionNext;
  { a dictionary made now has no object yet: it lives through the next major collection, which the object's mark
    then carries on }
  Result^.LiveMajor := GCMajorCount + 1;
  if ShapeCount >= Length(AllShapes) then SetLength(AllShapes, ShapeCount * 2 + 1024);
  AllShapes[ShapeCount] := Result;
  Inc(ShapeCount);
  ShapeRecharge(Result);
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
  ShapeRecharge(Result);
end;

function ValidPtr(P: Pointer): Boolean; forward;

{ after a collection: no live old cell may point into the nursery or at a freed cell }
procedure VerifyHeap(const Phase: string);
var I: Integer; P: PPage; Q: PByte; C: PCell; L: PLarge;
  procedure Bad(C: PCell; const Field: string; V: QWord);
  begin
    {$PUSH}{$I-}   { the finding is written with I/O checks off: a failed write cannot raise }
    BeginEngineCheckReport;
    Write(ErrOutput, 'heap verify (', Phase, ' minor ', GCMinorCount, ' major ', GCMajorCount, '): cell ',
      IntToHex(PtrUInt(C), 16), ' kind ', C^.Kind, ' flags ', C^.Flags);
    if C^.Kind = ckObject then Write(ErrOutput, ' okind ', PObj(C)^.OKind, ' oflags ', PObj(C)^.OFlags);
    WriteLn(ErrOutput, ' field ', Field, ' = ', IntToHex(V, 16));
    StopOnEngineCheck('heap verification');
    {$POP}
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
      {$PUSH}{$I-}   { the finding is written with I/O checks off: a failed write cannot raise }
      BeginEngineCheckReport;
      WriteLn(ErrOutput, 'after minor ', GCMinorCount, ': shape ', I, ' still points into the nursery: ', IntToHex(AllShapes[I]^.Proto, 16));
      StopOnEngineCheck('shape verification');
      {$POP}
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
    {$PUSH}{$I-}   { the finding is written with I/O checks off: a failed write cannot raise }
    BeginEngineCheckReport;
    WriteLn(ErrOutput, 'bad pointer ', IntToHex(V, 16), ' at ', Where, ' ', I, ' minor ', GCMinorCount, ' major ', GCMajorCount,
      ' nursery ', IntToHex(PtrUInt(NurseryStart), 16));
    StopOnEngineCheck('root pointer');
    {$POP}
  end;
end;

procedure ScanShapesForGC;
var I, J: Integer; V: TValue;
begin
  for I := 0 to ShapeCount - 1 do
  begin
    if AllShapes[I]^.Proto = TAG_PTR then
    begin
      {$PUSH}{$I-}   { the finding is written with I/O checks off: a failed write cannot raise }
      BeginEngineCheckReport;
      WriteLn(ErrOutput, 'shape ', I, ' of ', ShapeCount, ' has a nil prototype; count ', AllShapes[I]^.Count,
        ' minor ', GCMinorCount, ' inminor ', InMinor);
      StopOnEngineCheck('shape prototype');
      {$POP}
    end;
    if GCVerify then
    begin
      CheckRoot(AllShapes[I]^.Proto, 'shape', I);
      V := AllShapes[I]^.Proto;
      if ((V shr 48) = TAG_PTR_HI) and ((PCell(PtrOf(V))^.Kind > 40) or (PCell(PtrOf(V))^.Size = 0) or
         (PCell(PtrOf(V))^.Size > 1 shl 30)) then
      begin
        {$PUSH}{$I-}   { the finding is written with I/O checks off: a failed write cannot raise }
        BeginEngineCheckReport;
        Write(ErrOutput, 'shape ', I, ' of ', ShapeCount, ' (dict ', AllShapes[I]^.IsDict, ', count ', AllShapes[I]^.Count,
          ') has a bad prototype ', IntToHex(V, 16), ' kind ', PCell(PtrOf(V))^.Kind, ' minor ', GCMinorCount, '; keys:');
        for J := 0 to AllShapes[I]^.Count - 1 do Write(ErrOutput, ' ', StrToUnicode(AllShapes[I]^.Keys[J]));
        WriteLn(ErrOutput);
        StopOnEngineCheck('shape prototype');
        {$POP}
      end;
    end;
    { a minor collection keeps every shape's prototype (it moves the young ones); a major one marks a prototype
      only through the objects whose shapes name it (ScanCell), and RetireDeadShapes lets the rest go }
    if InMinor then GCVisit(AllShapes[I]^.Proto);
  end;
  for I := 0 to RootShapeCount - 1 do
  begin
    V := RootShapes[I].P;
    if GCVerify then CheckRoot(V, 'rootshape', I);
    if InMinor then
    begin
      GCVisit(V);
      if (V <> RootShapes[I].P) and (I >= ROOT_DIRECT) then RootHashStale := True;
    end;
    RootShapes[I].P := V;
  end;
end;

{ after a major collection's marking: the shapes whose prototype nothing reaches are retired. No live object has
  one (its scan would have marked the prototype) and none can get one again: ShapeAdd keeps a shape's prototype,
  and the caches that make objects of a remembered shape (a constructor template's CtorRootShape) first compare its
  prototype with the one asked for. Its prototype becomes VRetiredProto, which no value equals, so a comparison
  with it fails instead of reading the freed prototype. The root shape table drops the dead prototypes' entries
  and renumbers the survivors' indices (Hdr.Extra). Then GCShapeSweepHook clears what still names the retired
  shapes, and their records are freed, except those machine code compares with (pinned). }
procedure RetireDeadShapes;
var I, N, First: Integer; S: PShape; P: TValue; Unsafe: Boolean;
begin
  First := RetiredCount;
  N := 0;
  for I := 0 to ShapeCount - 1 do
  begin
    S := AllShapes[I];
    P := S^.Proto;
    { its prototype died, or it is a dictionary no live object has: its object died, or moved on to another shape }
    if (((P shr 48) = TAG_PTR_HI) and ((PCell(PtrOf(P))^.Flags and (cfMark or cfPermanent)) = 0)) or
       (S^.IsDict and (S^.LiveMajor < GCMajorCount)) then
    begin
      S^.Proto := VRetiredProto;
      if RetiredCount >= Length(RetiredShapes) then SetLength(RetiredShapes, RetiredCount * 2 + 1024);
      RetiredShapes[RetiredCount] := S;
      Inc(RetiredCount);
    end
    else
    begin
      AllShapes[N] := S;
      Inc(N);
    end;
  end;
  ShapeCount := N;
  N := 0;
  Unsafe := False;
  for I := 0 to RootShapeCount - 1 do
  begin
    P := RootShapes[I].P;
    if RootShapes[I].S^.Proto = VRetiredProto then Continue;
    RootShapes[N] := RootShapes[I];
    if IsObject(P) then
    begin
      if N < ROOT_DIRECT then ObjOf(P)^.Hdr.Extra := N + 1 else ObjOf(P)^.Hdr.Extra := ROOT_IN_HASH;
      if ObjOf(P)^.OKind = okProxy then Unsafe := True;
    end;
    Inc(N);
  end;
  RootShapeCount := N;
  RootHashStale := True;
  { NewRootShape's rule again: a proxy prototype's [[Set]] is its handler's. No add transition was cached while it
    was set; the bump fails one whose fill began before this collection (it compares the epoch it read first) }
  if AddICUnsafe and not Unsafe then
  begin
    AddICUnsafe := False;
    Inc(ProtoEpoch);
  end;
  if RetiredCount = First then Exit;
  { what still names the newly retired shapes forgets them; then their records are freed, except those machine code
    holds (pinned), which keep only what their prototype's comparison reads }
  if Assigned(GCShapeSweepHook) then GCShapeSweepHook;
  N := First;
  for I := First to RetiredCount - 1 do
  begin
    S := RetiredShapes[I];
    if S^.Pinned then
    begin
      S^.Count := 0; S^.Keys := nil; S^.PFlags := nil;
      S^.EdgeCount := 0; S^.Edges := nil; S^.EdgeHash := nil;
      S^.HashKeys := nil; S^.HashSlots := nil; S^.HashMask := -1;
      S^.ProtoChild := nil;
      ShapeRecharge(S);
      RetiredShapes[N] := S;
      Inc(N);
    end
    else
      ShapeRecFree(S);
  end;
  RetiredCount := N;
end;

{ the root shape table's entries past the first ROOT_DIRECT, and those of primitive prototypes, by prototype: open
  addressing over RootHash (entries' indices + 1, 0 free). A minor collection that moves one of those prototypes,
  and a major one that compacts the table, leave it stale; the next lookup rebuilds it }
function RootHashSlot(P: TValue): Integer; inline;
begin
  Result := Integer(((P xor (P shr 32)) * QWord($9E3779B97F4A7C15)) shr 40) and RootHashMask;
end;

function RootHashed(I: Integer): Boolean; inline;
begin
  Result := (I >= ROOT_DIRECT) or not IsObject(RootShapes[I].P);
end;

procedure RootHashPut(I: Integer);
var J: Integer;
begin
  J := RootHashSlot(RootShapes[I].P);
  while RootHash[J] <> 0 do J := (J + 1) and RootHashMask;
  RootHash[J] := I + 1;
  Inc(RootHashUsed);
end;

procedure RootHashRebuild(Extra: Integer); noinline;
var I, N, Need: Integer;
begin
  Need := Extra;
  for I := 0 to RootShapeCount - 1 do
    if RootHashed(I) then Inc(Need);
  N := 64;
  while N < Need * 2 do N := N * 2;
  RootHash := nil;
  SetLength(RootHash, N);   { zeroed }
  RootHashMask := N - 1;
  RootHashUsed := 0;
  RootHashStale := False;
  for I := 0 to RootShapeCount - 1 do
    if RootHashed(I) then RootHashPut(I);
end;

function RootHashFind(P: TValue): Integer;
var J, K: Integer;
begin
  if RootHashStale or (RootHash = nil) then RootHashRebuild(1);
  J := RootHashSlot(P);
  while True do
  begin
    K := RootHash[J];
    if K = 0 then Exit(-1);
    if RootShapes[K - 1].P = P then Exit(K - 1);
    J := (J + 1) and RootHashMask;
  end;
end;

{ cold: a prototype's first root shape (and the verifier's report) out of RootShapeFor's frame }
function NewRootShape(Proto: TValue): PShape; noinline;
var I: Integer;
begin
  if GCVerify and ((Proto shr 48) = TAG_PTR_HI) and not NurseryCellOK(Proto) then
  begin
    WriteLn(ErrOutput, 'RootShapeFor: a stale prototype ', IntToHex(Proto, 16));
    PInteger(nil)^ := 0;   { stop here under a debugger }
  end;
  Result := NewShape(nil, Proto);
  if RootShapeCount >= Length(RootShapes) then SetLength(RootShapes, RootShapeCount * 2 + 256);
  I := RootShapeCount;
  RootShapes[I].P := Proto;
  RootShapes[I].S := Result;
  Inc(RootShapeCount);
  if IsObject(Proto) then
    if I < ROOT_DIRECT then ObjOf(Proto)^.Hdr.Extra := I + 1 else ObjOf(Proto)^.Hdr.Extra := ROOT_IN_HASH;
  if RootHashed(I) and not RootHashStale and (RootHash <> nil) then
  begin
    if (RootHashUsed + 1) * 2 > Length(RootHash) then RootHashRebuild(0)
    else RootHashPut(I);
  end;
  if IsObject(Proto) and not AddICUnsafe and (ObjOf(Proto)^.OKind = okProxy) then
  begin
    AddICUnsafe := True;
    ProtoChanged;
  end;
end;

threadvar
  LastPrimProto: TValue;
  LastPrimShape: PShape;

function RootShapeFor(Proto: TValue): PShape;
var I: Integer; E: Word; R: PRootShapeEntry;
begin
  { an object's entry is at its Hdr.Extra - 1 (through a raw pointer: see ShapeLookup), or in RootHash; an object
    with Extra 0 was never a prototype }
  R := Pointer(RootShapes);
  if IsObject(Proto) then
  begin
    E := ObjOf(Proto)^.Hdr.Extra;
    if E = 0 then Exit(NewRootShape(Proto));
    if E <> ROOT_IN_HASH then
    begin
      I := E - 1;
      if (I < RootShapeCount) and (R[I].P = Proto) then Exit(R[I].S);
      { not where its index says (never expected): the table itself }
      for I := 0 to RootShapeCount - 1 do
        if R[I].P = Proto then Exit(R[I].S);
      Exit(NewRootShape(Proto));
    end;
  end
  else
  begin
    { null (Object.create(null), a class extending null) and the other primitives: the last one asked for }
    if (Proto = LastPrimProto) and (LastPrimShape <> nil) then Exit(LastPrimShape);
  end;
  I := RootHashFind(Proto);
  if I >= 0 then
  begin
    if not IsObject(Proto) then begin LastPrimProto := Proto; LastPrimShape := RootShapes[I].S; end;
    Exit(RootShapes[I].S);
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
  ShapeRecharge(S);
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
  if S^.HashMask < 0 then
  begin
    { without a hash, HashMask counts down the lookups: a shape looked up only a few times (a prototype's shapes
      while its methods are defined one by one) is scanned, not hashed }
    if (S^.HashMask > -4) and (S^.Count <= 64) then
    begin
      Dec(S^.HashMask);
      KP := Pointer(S^.Keys);
      for I := S^.Count - 1 downto 0 do
        if KP[I] = Key then Exit(I);
      Exit(-1);
    end;
    BuildHash(S);
  end;
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
    { a key added in place can shadow a prototype's: caches that passed this object check it }
    Inc(ShapeVersionNext);
    N^.Version := ShapeVersionNext;
  end
  else
  begin
    N := NewShape(S, S^.Proto);
    N^.Count := S^.Count;
    if not MakeDict and not S^.KeysLent then
    begin
      N^.Keys := S^.Keys;
      N^.PFlags := S^.PFlags;
      S^.KeysLent := True;
      N^.KeysShared := True;
    end
    else
    begin
      N^.Keys := Copy(S^.Keys, 0, S^.Count);
      N^.PFlags := Copy(S^.PFlags, 0, S^.Count);
    end;
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
    { a lent array is copied by the growth (its parent still holds it): N's own from now on }
    SetLength(N^.Keys, N^.Count * 2 + 4);
    SetLength(N^.PFlags, N^.Count * 2 + 4);
    N^.KeysShared := False;
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
  ShapeRecharge(N);
  if S <> N then ShapeRecharge(S);
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
  if CountObjects then Inc(ObjectsMade);
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
begin
  if ((Flags and (pfAccessor or pfWritable)) <> pfWritable) and (O^.Hdr.Extra <> 0) then ProtoChanged;
  Result := AddPropertyTo(O, ShapeAdd(O^.Shape, Key, Flags), V);
end;

function GrowValueCap(Cap, Need, Least: Integer): Integer; forward;

function AddPropertyTo(O: PObj; NewShape: PShape; V: TValue): Integer;
var Idx, NewCap, I: Integer; NO: PValueArray;
begin
  Idx := NewShape^.Count - 1;
  if Idx >= O^.InlineCap then
  begin
    I := Idx - O^.InlineCap;
    if (O^.Overflow = nil) or (I >= Integer(O^.Overflow^.Cap)) then
    begin
      if O^.Overflow = nil then NewCap := 4 else NewCap := GrowValueCap(O^.Overflow^.Cap, I + 1, 4);
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
    ShapeRecharge(N);
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
  if O^.Hdr.Extra <> 0 then
  begin
    ProtoChanged;
    TAWatchedChanged(O);
  end;
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
  { every write to the array takes the checked path (Goccia.ObjModel ofElemGuard) }
  A^.OFlags := A^.OFlags or ofBigLen or 64;
  A^.Aux2 := MkDouble(NewLen);
end;

{ a value array's next capacity past Cap that holds Need: twice Cap, or, when that would not fit under HeapCeiling,
  Need and a sixteenth more (Need alone when not even that fits), so the last growth before the limit does not
  allocate twice what is used }
function GrowValueCap(Cap, Need, Least: Integer): Integer;
var Snug: Int64;
begin
  Result := Cap * 2;
  if Result < Need then Result := Need;
  if Result < Least then Result := Least;
  if (HeapCeiling <= 0) or (Result <= Need + Need div 16) or HeapHasRoom(ValueArrayBytes(Result)) then Exit;
  Snug := Int64(Need) + Need div 16;
  if not HeapHasRoom(ValueArrayBytes(Snug)) then Snug := Need;
  if Snug < Result then Result := Snug;
end;

procedure EnsureElementsCap(A: PObj; Need: Integer);
var E, NE: PValueArray; NewCap: Integer;
begin
  E := A^.Elements;
  if Need <= Integer(E^.Cap) then Exit;
  NewCap := GrowValueCap(Integer(E^.Cap), Need, 8);
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
var I: Integer; L, Nx: PLarge; Ints: PIntAtoms;
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
  for I := 0 to PermanentCellCount - 1 do FreeMem(PermanentCells[I]);
  PermanentCells := nil; PermanentCellCount := 0;
  for I := 0 to ShapeCount - 1 do ShapeRecFree(AllShapes[I]);
  AllShapes := nil; ShapeCount := 0;
  for I := 0 to RetiredCount - 1 do ShapeRecFree(RetiredShapes[I]);
  RetiredShapes := nil; RetiredCount := 0;
  ShapesMade := 0; ShapesMadeAtMajor := 0; ShapesLiveAtMajor := 0;
  if MegaICTab <> nil then FreeMem(MegaICTab);
  MegaICTab := nil;
  RootShapes := nil; RootShapeCount := 0;
  RootHash := nil; RootHashMask := 0; RootHashUsed := 0; RootHashStale := False;
  { RootShapeFor's cache of the last primitive prototype's shape, freed with the others above: the thread's next
    engine would take it for its null prototype's }
  LastPrimProto := 0; LastPrimShape := nil;
  UsingKey0 := nil; UsingKey1 := nil; UsingKey2 := nil;
  { IntAtom's atoms of 0..1023, freed with the atom table above }
  Ints := SmallIntAtoms;
  if Ints <> nil then Dispose(Ints);
  SmallIntAtoms := nil;
  GrayStack := nil; GrayTop := 0; RememberedList := nil; RememberedTop := 0; OSFree(TempRoots, TEMP_ROOTS_MAX * SizeOf(TValue));
  TempRoots := nil; TempTop := 0;
  GCOldBytes := 0; GCOldBytesAtMajor := 0; GCPromotedBytes := 0; GCMinorCount := 0; GCMajorCount := 0;
  GCAllocatedBytes := 0; GCPeakBytes := 0;
  { the atoms, symbols and shapes charged were freed above }
  StructBytes := 0; PermBytes := 0; ChargeCheckAt := 0; ChargePending := False;
end;


{ the managed threadvars of this unit, released when the thread ends (Goccia.ThreadCleanupRegistry): FPC does not
  finalize a threadvar, and every engine thread leaves its strings, arrays and objects behind otherwise }
procedure GocciaCoreThreadvarCleanup;
begin
  CurProfile := Default(TLangProfile);
  TimeZoneOverride := '';
  Pages := nil;
  GrayStack := nil;
  EphKeys := nil;
  RememberedList := nil;
  AtomTable := nil;
  KeyAtoms := nil;
  PermanentCells := nil;
  AllShapes := nil;
  RootShapes := nil;
  RetiredShapes := nil;
  RootHash := nil;
end;

initialization
  InitCriticalSection(GCFinalizeHooksLock);
  RegisterThreadvarCleanup(@GocciaCoreThreadvarCleanup);

finalization
  DoneCriticalSection(GCFinalizeHooksLock);
end.
