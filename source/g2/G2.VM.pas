unit G2.VM;
{ Interpreter: one dispatch loop without Pascal exception handling, frames on one value stack,
  Lua-style open upvalues, monomorphic inline caches. }
{$mode delphiunicode}{$H+}
{$MINFPCONSTPREC 64}
{$POINTERMATH ON}
{$GOTO ON}
{$INLINE ON}
{$Q-}{$R-}

interface

uses
  {$IFDEF UNIX}BaseUnix, {$ENDIF}SysUtils, Math, BigInteger, G2.Core, G2.Bytecode, G2.BigIntV, G2.Typed;

const
  FRAME_MAX = 200000;
  okAsyncGen = 26;  { an async generator: Aux1 closure, Aux2 state, as okGenerator }
  STACK_MAX = 4 * 1024 * 1024;
  INTRINSIC_CLASS = $80000000;   { TObj.Pad of a function: a built-in class (MarkIntrinsicClass) }

type
  PFrame = ^TFrame;
  TFrame = record
    Closure: TValue;
    NewTarget: TValue;
    Tpl: PFuncTemplate;
    Base: Integer;
    PC: Integer;
    RetIdx: Integer;
    Argc: Integer;
    Kind: Byte;          { 0 call, 1 construct (object result or this), 2 discard result }
  end;

  THandler = record
    Frame: Integer;
    PC: Integer;
    ExcIdx: Integer;
    Kind: Integer;   { 0 catch; 1 finally or iterator cleanup, which a generator's return completion runs too }
  end;

var
  HostRootScanner: procedure = nil;   { visits host-held roots (the embedding layer's handles) }
  RootCheck: Boolean = False;
  { 10.5.9 Proxy [[Set]] (G2.ObjModel): Ok is the [[Set]] result }
  ProxySetHook: function(PV: TValue; Key: PStr; V, Receiver: TValue; var Ok: Boolean): Boolean = nil;
  { G2.ObjModel's O.[[Set]](Key, V, Receiver) (super property assignment) }
  SetReceiverHook: function(O: TValue; Key: PStr; V, Receiver: TValue; var Ok: Boolean): Boolean = nil;
  { G2.ObjModel's 10.5 invariant checks after a trap returned: Kind 0 get (Res the value), 1 has (reported
    absent), 2 deleteProperty (reported deleted), 3 ownKeys (Res the trap result, replaced by the key list),
    4 ownKeys without a trap (Res the target's own keys) }
  ProxyCheckHook: function(Kind: Integer; PV: TValue; Key: PStr; var Res: TValue): Boolean = nil;
  { G2.Modules: 13.3.10 import(specifier, options) from code of source Referrer (a promise in Res), and
    13.3.12 import.meta of module ModuleId }
  ImportHook: function(const Referrer: UnicodeString; ModuleId: Integer; Spec, Opts: TValue; var Res: TValue): Boolean = nil;
  ImportMetaHook: function(ModuleId: Integer; var Res: TValue): Boolean = nil;
  { G2.ObjModel's 7.3.25 CopyDataProperties for a source the fast path does not take (an exotic object, a symbol
    key, a primitive) }
  CopyDataPropsHook: function(Target, Src: TValue): Boolean = nil;
  { CopyDataProperties with excludedItems (an array of property keys), for an object rest (G2.ObjModel) }
  CopyDataPropsExclHook: function(Target, Src, Excl: TValue): Boolean = nil;
threadvar
  { opImport: the import call's phase while the hook runs (0 evaluation, 1 source, 2 defer) }
  ImportPhase: Integer;
var
  { G2.ObjModel's 7.1.18 ToObject }
  ToObjectHook: function(V: TValue; var O: TValue): Boolean = nil;
  { G2.ObjModel's [[GetPrototypeOf]] (a proxy's trap with its invariants) }
  GetProtoHook: function(O: TValue; var P: TValue): Boolean = nil;
  { G2.ObjModel's [[SetPrototypeOf]]: Ok its result }
  SetProtoHook: function(O, P: TValue; var Ok: Boolean): Boolean = nil;
  { Deferring Module Evaluation (G2.Modules): a deferred namespace's module evaluated before a key's use }
  NSDeferHook: function(NS: TValue): Boolean = nil;
  { module types (G2.Modules): a JSON module's value parsed by %JSON.parse%'s grammar (G2.JSON), a bytes module's
    Uint8Array over an immutable ArrayBuffer (G2.TypedArrays) }
  JSONTextHook: function(const S: UnicodeString; var Res: TValue): Boolean = nil;
  BytesHook: function(const B: RawByteString; var Res: TValue): Boolean = nil;
  { CreateDataPropertyOrThrow on an exotic object through its [[DefineOwnProperty]] (G2.ObjModel) }
  CreateDataHook: function(O: TValue; Key: PStr; V: TValue): Boolean = nil;
  { the native of %Function.prototype.call% (G2.Builtins): Execute calls f.call(t, ...)'s f itself }
  FuncCallNative: Pointer = nil;
  { 13.2.8.4 GetTemplateObject: SetIntegrityLevel(frozen) of the raw strings array and of the template (G2.ObjModel) }
  TemplateFreezeHook: function(T: TValue): Boolean = nil;
threadvar
  { set when OwnKeys met a throwing proxy trap: the caller takes it with TakeOwnKeysFailure }
  OwnKeysFailed: Boolean;
  VStack: PValue;
  StackTop: Integer;          { first value-stack slot not used by any frame }
  Frames: array of TFrame;
  FrameTop: Integer;          { number of live frames }
  Handlers: array of THandler;
  HandlerTop: Integer;
  OpenUpvals: array of TValue;
  OpenCount: Integer;
  GlobalObj: TValue;
  Pending: TValue;
  CurNewTarget: TValue;       { set while a native function runs as a constructor }
  ConstructNT: TValue;        { the new.target Construct hands to the native CallValue is about to run }
  NativeData: UInt32;         { the called native's TObj.Pad, set just before a native runs: per-function host data }
  ExtraRoots: array of PValue;
  ExtraRootCount: Integer;
  ThrowTrace: Boolean;
  GenYielded: Boolean;
  RegExpCreateHook: function(Pattern, Flags: PStr; var Res: TValue): Boolean;
  { G2.RegExpV: the early-error check of a regular expression literal ('' when valid) }
  RegExpValidateHook: function(const Pattern, Flags: UnicodeString): UnicodeString;
  { compiles a lazy template (TFuncTemplate.Lazy, G2.Lazy) before its first frame; False with an exception pending }
  LazyCompileHook: function(T: PFuncTemplate): Boolean;
  InjectThrow: Boolean;
  TypedArrayValuesFn: TValue;
  AtomLength, AtomPrototype, AtomConstructor, AtomName, AtomMessage, AtomNext, AtomDone, AtomValue,
    AtomToString, AtomValueOf, AtomFieldInit, AtomIterator, AtomStack, AtomCallee,
    AtomByteLength, AtomByteOffset, AtomBuffer: PStr;
  SymIterator: TValue;
  ThrowerAcc: TValue;   { an accessor cell with %ThrowTypeError% as getter and setter (G2.ObjModel) }
  SymHasInstance: TValue;
  { global lexical bindings: GLexCells[I] is the upvalue cell of binding I (open while its Script runs),
    VUndefined until its Script's opMakeGLex ran }
  GLexCells: array of TValue;
  GLexNames: array of PStr;
  GLexConst: array of Boolean;
  GLexCount: Integer;
  GLexHash: array of Integer;    { open addressing on the atom: binding index + 1, 0 empty }
  GVarHash: array of PStr;       { the global environment's [[VarNames]] }
  GVarCount: Integer;
  EvalHintMode, EvalHintFrame, EvalHintPC, EvalHintDesc: Integer;
  EvalDesc: TValue;   { TakeEvalHint: a direct eval's description of its caller's bindings (an atom), else undefined }
  RealmEvalFn: TValue;   { the running realm's %eval%: a call is a direct eval only of this function (13.3.6.1) }
  { the native function object being called: a native reads its data (Aux1) from it before allocating }
  NativeSelf: TValue;
  { the key whose accessor getter a property read is about to call (%ThrowTypeError% tells caller from arguments) }
  AccessorGetKey: PStr;
  { set when a native constructor took its instance's prototype from new.target (ProtoFromNewTarget), at the
    step its algorithm does it; Construct gives the prototype to the instance of a native that did not }
  ConstructProto: TValue;
  ConstructProtoTaken: Boolean;
  { G2.Promise: the call of an async function (its frame suspended in G) returns its promise }
  AsyncStartHook: function(G: TValue): TValue;
  { the well-known symbols other than iterator and hasInstance (G2.Builtins makes them) }
  SymAsyncIterator, SymSpecies, SymToStringTagW, SymToPrimitive, SymIsConcatSpreadable, SymMatch, SymMatchAll,
    SymReplace, SymSearch, SymSplit, SymUnscopables, SymDispose, SymAsyncDispose: TValue;
  { set by opYield / opAwait: the suspension was an await }
  SuspendedByAwait: Boolean;
  { the value a generator produced is an inner iterator result of yield*, to be returned as is (27.5.3.7) }
  GenYieldRaw: Boolean;
  { the intrinsic %ArrayIteratorPrototype%.next and %GeneratorPrototype%.next (G2.Builtins sets them): an
    iterator record whose next method is one of them steps without a call }
  ArrayIterNextFn, GenNextFn: TValue;
  MapIterNextFn, SetIterNextFn: TValue;   { %MapIteratorPrototype%.next, %SetIteratorPrototype%.next }
  { the function and iterator intrinsics of generators and async code (G2.Promise makes them) }
  GenFunctionProto, AsyncFunctionProto, AsyncGenFunctionProto, AsyncGeneratorProto, AsyncIteratorProto,
    AsyncFromSyncIterProto: TValue;
  GenFunctionShape, AsyncFunctionShape, AsyncGenFunctionShape: PShape;
  { G2.Promise: the call of an async generator function (its frame suspended in G) returns G, set up }
  AsyncGenStartHook: function(G: TValue): TValue;
  { G2.Promise: 27.1.6.1 CreateAsyncFromSyncIterator of an iterator record }
  AsyncFromSyncHook: function(It, NextM: TValue; var Res: TValue): Boolean;

var ICHit0, ICHitOther, ICMissN, ICMissFill, ICMissDepthFail: Int64;
  OpCounts: array[0..255] of Int64;
  PairCounts: array[0..65535] of Int64;
  PrevOp: Integer;
  ICWhy: array[0..7] of Int64;   { 0 dict receiver, 1 index key, 2 absent, 3 accessor, 4 virtual, 5 dict/deep proto, 6 proxy, 7 fn prototype }
procedure VMInit;
{ releases this thread's VM state (stacks, roots); HeapDone frees the heap after it }
procedure VMDone;
procedure AddRoot(P: PValue);
{ N consecutive roots from P. For a threadvar array, pass element 0: FPC 3.3.1's native-TLS code generator
  gives `@ThreadvarArray[K]` with a constant K <> 0 an absolute relocation instead of a TLS offset (an
  unrolled `for I := 0 to 8 do AddRoot(@A[I])` registered addresses outside the thread's TLS block). }
procedure AddRootRange(P: PValue; N: Integer);
procedure SetFunctionProto(P: TValue);
{ a function shape: the prototype's root shape with length then name (10.2.9, 10.2.10: configurable only) }
function FnShapeFor(Proto: TValue): PShape;
{ 10.2.9 SetFunctionName from a property key (a symbol's description in brackets), with an optional prefix }
procedure SetFnNameFromKey(F: TValue; Key: PStr; const Prefix: UnicodeString);
procedure SetGenNextNative(P: Pointer);
{ the global lexical bindings (let, const and class at the top level of Scripts): index of Key, -1 if none }
function GLexFind(Key: PStr): Integer;
function GLexIsConst(I: Integer): Boolean;
{ 7.2.4 IsConstructor }
function IsConstructorV(V: TValue): Boolean;
{ 7.4.11 IteratorClose: Throwing keeps the pending exception (errors of return() are ignored, False returned) }
function IteratorCloseV(It: TValue; Throwing: Boolean): Boolean;
{ 7.4.2 GetIterator(obj, sync) as an iterator record: the iterator and its next method }
function GetIteratorRec(V: TValue; var It, NextM: TValue): Boolean;
{ 7.4.8 IteratorStep(record) }
function IteratorStepRec(It, NextM: TValue; var Value: TValue; var Done: Boolean): Boolean;
{ the key of a Symbol value }
function SymKey(Sym: TValue): PStr; inline;
{ the prototype a native constructor gives its instance: ConstructProto when Construct fetched one, else
  new.target's prototype property, else Default }
function ProtoFromNewTarget(Default: TValue; var P: TValue): Boolean;
{ an accessor property with a native getter under Key (a symbol key too) }
procedure DefineNativeGetterKey(O: TValue; Key: PStr; const Name: UnicodeString; Fn: TNativeFn);
{ called by the eval native: the opEvalHint mode (0 or 2) when its caller is a direct eval call, else -1 }
function TakeEvalHint: Integer;
function MakeEvalClosure(T: PFuncTemplate; Desc: TValue; var Clo: TValue): Boolean;
function RunScript(T: PFuncTemplate): Boolean;
function RunScriptRes(T: PFuncTemplate; var Res: TValue): Boolean;   { Res: the completion value }
function MakeScriptClosure(T: PFuncTemplate; var Clo: TValue): Boolean;

{ helpers for natives (all may allocate and run JS) }
function CallValue(F, This: TValue; Args: PValue; Argc: Integer; var Res: TValue): Boolean;
function Construct(F: TValue; Args: PValue; Argc: Integer; NewTarget: TValue; var Res: TValue): Boolean;
function GetV(Obj: TValue; Key: PStr; var Res: TValue): Boolean;
function SetV(Obj: TValue; Key: PStr; V: TValue): Boolean;
function GetElemV(Obj, Key: TValue; var Res: TValue): Boolean;
function SetElemV(Obj, Key, V: TValue): Boolean;
{ strict / throwing [[Set]] (PutValue in strict code; Set(O, P, V, true) of the built-ins) }
function SetVS(Obj: TValue; Key: PStr; V: TValue; Strict: Boolean): Boolean;
function SetVT(Obj: TValue; Key: PStr; V: TValue): Boolean;
function SetElemVS(Obj, Key, V: TValue; Strict: Boolean): Boolean;
function SetElemVT(Obj, Key, V: TValue): Boolean;
function ProxyGetTrap(PV: TValue; const Name: UnicodeString; var Trap: TValue): Boolean;
function ProxyCallTrap(PV, Trap: TValue; Argc: Integer; A1, A2, A3: TValue; var Res: TValue): Boolean;
function ArrayDenseOK(E: PValueArray; Idx: Int64): Boolean; inline;
function ToPropKey(V: TValue; var Key: PStr): Boolean;
function ToStringV(V: TValue; var Res: TValue): Boolean;
function ToNumberV(V: TValue; var D: Double): Boolean;
type
  TPrimHint = (phDefault, phNumber, phString);
function ToPrimitiveV(V: TValue; HintString: Boolean; var Res: TValue): Boolean;
{ 7.1.1 ToPrimitive with its preferred type: binary + and == pass phDefault }
function ToPrimitiveHint(V: TValue; Hint: TPrimHint; var Res: TValue): Boolean;
{ 7.1.1.1 OrdinaryToPrimitive }
function OrdinaryToPrimitive(V: TValue; StringFirst: Boolean; var Res: TValue): Boolean;
threadvar
  { the key of Symbol.toPrimitive, set once the builtins created the symbol (nil before) }
  ToPrimitiveKey: PStr;
function ThrowError(const Kind, Msg: UnicodeString): Boolean;
function ThrowTypeError(const Msg: UnicodeString): Boolean;
function MakeError(Proto: TValue; const Msg: UnicodeString): TValue;
function NewNativeFunction(Fn: TNativeFn; const Name: UnicodeString; Length: Integer): TValue;
function NewNativeFunctionData(Fn: TNativeFn; const Name: UnicodeString; Length: Integer; Data: UInt32): TValue;
procedure DefineHidden(O: TValue; const Name: UnicodeString; V: TValue);
procedure DefineNative(O: TValue; const Name: UnicodeString; Fn: TNativeFn; Length: Integer);
function StrictEquals(A, B: TValue): Boolean;
function LooseEquals(A, B: TValue; var Res: Boolean): Boolean;
function InstanceOf(V, C: TValue; var Res: Boolean): Boolean;
{ 7.3.21 OrdinaryHasInstance(C, V) }
function OrdinaryHasInstance(C, V: TValue; var Res: Boolean): Boolean;
function OwnKeys(O: PObj; OnlyEnumerable: Boolean): TValue;   { array of string keys }
function DefineOwn(O: TValue; Key: PStr; V: TValue; Flags: Byte): Boolean;
function GetIterator(V: TValue; var It: TValue): Boolean;
function IteratorStep(It: TValue; var Value: TValue; var Done: Boolean): Boolean;
function FunctionPrototypeOf(F: TValue): TValue;
{ for preludes: marks a class as a built-in (its own prototype is its default prototype) }
function MarkIntrinsicClass(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
function DemoteArguments(Obj: TValue): TValue;
{ 10.4.6 module namespace: Found when Key (a string) is an export; V its binding's value (False with a
  ReferenceError pending while it is uninitialized, when WantValue) }
function NSLookup(O: PObj; Key: PStr; WantValue: Boolean; var V: TValue; var Found: Boolean): Boolean;
{ a return completion in flight (generator return(), carried as an exception through finally handlers) }
function IsRetCompV(V: TValue): Boolean;
{ 7.1.13 ToBigInt }
function ToBigIntV(V: TValue; var B: TValue): Boolean;
{ 7.1.21 CanonicalNumericIndexString is not undefined for a key that is not an array index ("-0", "1.5", "-1",
  "Infinity", an index past 2^32 - 2): a typed array has no such property }
function CanonicalNumericNonIndex(Key: PStr): Boolean;
{ 10.4.5.16 TypedArraySetElement: ToBigInt or ToNumber of V, then the store if Idx is still a valid index (-1:
  a canonical numeric key that is not an index) }
function TAStoreValue(O: TValue; Idx: Int64; V: TValue): Boolean;
{ [[Get]] with a receiver (proxies through their trap) }
function GetFromObject(O: PObj; Key: PStr; Receiver: TValue; var Res: TValue): Boolean;
{ a mapped arguments object's parameter map (10.4.4): the upvalue of index Key, nil when unmapped }
function ArgsMapCell(O: PObj; Key: PStr; var Idx: Int64): PUpval;
function ArgsMapGet(U: PUpval): TValue;
procedure ArgsMapSet(U: PUpval; V: TValue);
procedure ArgsUnmap(O: PObj; Idx: Int64);
{ [[Delete]] on an ordinary object (and the exotic ones G2 models in it): False for a non-configurable property }
function DeleteIfConfigurable(O: PObj; Key: PStr): Boolean;
function ProxyDelete(PV: TValue; Key: PStr; var Ok: Boolean): Boolean;
function ArrayIterationIntact: Boolean;
{ 7.3.18 LengthOfArrayLike: ToLength(Get(O, "length")) }
function LengthOfArrayLike(O: TValue; var Len: Int64): Boolean;
function OwnVirtual(O: PObj; Key: PStr; var V: TValue; var Flags: Byte): Boolean;
function NewArrayIterator(V: TValue; Kind: Integer): TValue;
procedure RegisterSymbolKey(Key: PStr; Sym: TValue);
function KeyToValue(Key: PStr): TValue;
function ProxyInvoke(PV: TValue; const TrapName: UnicodeString; A1: TValue; Argc: Integer; var Res: TValue;
  var HadTrap: Boolean): Boolean;
function ProxyOwnKeys(PV: TValue; var Res: TValue): Boolean;
function HasPropertyV(Obj: TValue; Key: PStr; var Found: Boolean): Boolean;
function DefineElemRaw(Obj: TValue; Idx: Int64; V: TValue): Boolean;
{ whether Proto is on the object V's prototype chain, through [[GetPrototypeOf]] }
function ProtoChainHas(V, Proto: TValue; var Res: Boolean): Boolean;
{ an arguments object over Argc values at Args (Mapped: a sloppy function's, with its parameter map shape) }
function NewArgumentsObject(Args: PValue; Argc: Integer; Mapped: Boolean): TValue;
function GeneratorResume(Gen: TValue; Arg: TValue; Mode: Integer; var Value: TValue; var Done: Boolean): Boolean;

threadvar
  ArrayValuesFnPublic: TValue;
  StringIterProto: TValue;   { %StringIteratorPrototype% }
  StringIterNextFn: TValue;  { %StringIteratorPrototype%.next }
  FuncHasInstanceFn: TValue; { %Function.prototype%[@@hasInstance]: instanceof skips calling it }
  BigIntProto: TValue;
  ErrorProtoType, TypeErrorProto, RangeErrorProto, SyntaxErrorProto, ReferenceErrorProto: TValue;
  EvalErrorProto, URIErrorProto: TValue;
  ArrayIterNative: TNativeFn;

{ ---- realms (9.3) ----
  A realm's intrinsics and global environment live in threadvars while it runs: SwitchRealm saves the running
  realm's values and loads another's. The Init procedures register the threadvars that hold their realm's state
  (RealmVals, RealmPtrs, RealmInts; ignored while CreateRealm reruns them for a new realm). A function object
  carries its [[Realm]] in Pad bits 24-30: a call to a function of another realm switches to it, and the
  caller's realm comes back when the callee returns. }
const
  REALM_SHIFT = 24;
  REALM_MAX = 127;
threadvar
  CurRealm: Integer;
  MultiRealm: Boolean;   { a second realm exists }
  RealmInit: Boolean;    { CreateRealm is running the Init procedures for a new realm }
  IntrinsicsInit: Boolean;   { InitIntrinsics is running: a native given a prototype property is a constructor }
  { 13.2.8.4 the realm's [[TemplateMap]]: template objects by tagged-template site (an array, created on first use;
    a realm slot registered by G2.ObjModel) }
  TemplateCache: TValue;
var
  { the Init procedures of the intrinsics, which CreateRealm runs for a new realm (nil: no realms) }
  RealmInitProc: procedure = nil;
  { some thread has a second realm: FastLoop leaves calls and returns to Execute (a global: cheaper there than
    MultiRealm) }
  MultiRealmAny: Boolean = False;
procedure RealmVals(const P: array of PValue);
procedure RealmValRange(P: PValue; N: Integer);
procedure RealmPtrs(const P: array of Pointer);
procedure RealmInts(const P: array of Pointer);
procedure SwitchRealm(R: Integer);
{ 9.3.1 InitializeHostDefinedRealm for a new realm, left as the running realm's sibling: R its index }
function CreateRealm(var R: Integer): Boolean;
function FnRealm(O: PObj): Integer; inline;
{ 7.3.24 GetFunctionRealm }
function GetFunctionRealm(F: TValue; var R: Integer): Boolean;
{ V, an intrinsic of realm FromR, as the same intrinsic of realm ToR (V itself when it is none) }
function MapIntrinsic(V: TValue; FromR, ToR: Integer): TValue;
{ 10.1.14 GetPrototypeFromConstructor: C's prototype property, else Default (an intrinsic of the running realm)
  as the intrinsic of C's realm }
function GetPrototypeFromConstructor(C, Default: TValue; var P: TValue): Boolean;

implementation

{ N registers from P set to undefined: a frame's registers past its arguments. A call clears about ten, which
  FillQWord's set-up costs more than (about 29 instructions in Richards); this loop stores four per step }
procedure FillUndef(P: PValue; N: Integer); noinline;
var V: TValue;
begin
  V := VUndefined;
  while N >= 4 do
  begin
    P[0] := V; P[1] := V; P[2] := V; P[3] := V;
    Inc(P, 4);
    Dec(N, 4);
  end;
  while N > 0 do
  begin
    P^ := V;
    Inc(P);
    Dec(N);
  end;
end;

function SuspendNewGenerator: TValue; forward;
function GeneratorNextIsIntrinsic(Gen: TValue): Boolean; forward;

{ ---------------- roots ---------------- }

procedure AddRoot(P: PValue);
var I: Integer;
begin
  { a new realm's Init procedures root the same threadvars again }
  if RealmInit then
    for I := 0 to ExtraRootCount - 1 do
      if ExtraRoots[I] = P then Exit;
  if ExtraRootCount >= Length(ExtraRoots) then SetLength(ExtraRoots, ExtraRootCount * 2 + 64);
  ExtraRoots[ExtraRootCount] := P;
  Inc(ExtraRootCount);
end;

procedure AddRootRange(P: PValue; N: Integer);
var I: Integer;
begin
  for I := 0 to N - 1 do AddRoot(P + I);
end;

{ ---------------- realms ---------------- }

type
  TRealmRec = record
    Vals: array of TValue;
    Ptrs: array of Pointer;
    Ints: array of Integer;
    GLexCells: array of TValue;
    GLexNames: array of PStr;
    GLexConst: array of Boolean;
    GLexHash: array of Integer;
    GLexCount: Integer;
    GVarHash: array of PStr;
    GVarCount: Integer;
  end;

threadvar
  FunctionShape: PShape;
  { the intrinsics the preludes registered with markIntrinsic, in order (a JS array): every realm runs the same
    preludes, so a realm's K-th entry is the same intrinsic as another realm's K-th (MapIntrinsic) }
  IntrinsicList: TValue;
  IntrinsicListIdx: Integer;   { its index among the realm values }
  Realms: array of TRealmRec;
  RealmValSlots: array of PValue;
  RealmPtrSlots: array of PPointer;
  RealmIntSlots: array of PInteger;

procedure RealmVals(const P: array of PValue);
var I: Integer;
begin
  if RealmInit then Exit;
  for I := 0 to High(P) do
  begin
    SetLength(RealmValSlots, Length(RealmValSlots) + 1);
    RealmValSlots[High(RealmValSlots)] := P[I];
  end;
end;

procedure RealmValRange(P: PValue; N: Integer);
var I: Integer;
begin
  for I := 0 to N - 1 do RealmVals([P + I]);
end;

procedure RealmPtrs(const P: array of Pointer);
var I: Integer;
begin
  if RealmInit then Exit;
  for I := 0 to High(P) do
  begin
    SetLength(RealmPtrSlots, Length(RealmPtrSlots) + 1);
    RealmPtrSlots[High(RealmPtrSlots)] := P[I];
  end;
end;

procedure RealmInts(const P: array of Pointer);
var I: Integer;
begin
  if RealmInit then Exit;
  for I := 0 to High(P) do
  begin
    SetLength(RealmIntSlots, Length(RealmIntSlots) + 1);
    RealmIntSlots[High(RealmIntSlots)] := P[I];
  end;
end;

procedure SaveRealm(R: Integer);
var I: Integer;
begin
  with Realms[R] do
  begin
    SetLength(Vals, Length(RealmValSlots));
    for I := 0 to High(RealmValSlots) do Vals[I] := RealmValSlots[I]^;
    SetLength(Ptrs, Length(RealmPtrSlots));
    for I := 0 to High(RealmPtrSlots) do Ptrs[I] := RealmPtrSlots[I]^;
    SetLength(Ints, Length(RealmIntSlots));
    for I := 0 to High(RealmIntSlots) do Ints[I] := RealmIntSlots[I]^;
  end;
  Realms[R].GLexCells := GLexCells; Realms[R].GLexNames := GLexNames; Realms[R].GLexConst := GLexConst;
  Realms[R].GLexHash := GLexHash; Realms[R].GLexCount := GLexCount;
  Realms[R].GVarHash := GVarHash; Realms[R].GVarCount := GVarCount;
end;

procedure LoadRealm(R: Integer);
var I: Integer;
begin
  with Realms[R] do
  begin
    for I := 0 to High(RealmValSlots) do RealmValSlots[I]^ := Vals[I];
    for I := 0 to High(RealmPtrSlots) do RealmPtrSlots[I]^ := Ptrs[I];
    for I := 0 to High(RealmIntSlots) do RealmIntSlots[I]^ := Ints[I];
  end;
  GLexCells := Realms[R].GLexCells; GLexNames := Realms[R].GLexNames; GLexConst := Realms[R].GLexConst;
  GLexHash := Realms[R].GLexHash; GLexCount := Realms[R].GLexCount;
  GVarHash := Realms[R].GVarHash; GVarCount := Realms[R].GVarCount;
  { the running realm's state is in the threadvars only (a second reference would make SetLength copy) }
  Realms[R].GLexCells := nil; Realms[R].GLexNames := nil; Realms[R].GLexConst := nil;
  Realms[R].GLexHash := nil; Realms[R].GVarHash := nil;
end;

procedure SwitchRealm(R: Integer);
begin
  if R = CurRealm then Exit;
  SaveRealm(CurRealm);
  LoadRealm(R);
  CurRealm := R;
end;

function ThreadPtr: PByte; assembler; nostackframe;
asm
  movq %fs:0, %rax
end;

function CreateRealm(var R: Integer): Boolean;
var Old, NewR, I, TlsSize: Integer; Before, After: array of Byte; Base: PByte;
begin
  if not Assigned(RealmInitProc) then Exit(ThrowTypeError('this host cannot create realms'));
  if Length(Realms) = 0 then SetLength(Realms, 1);
  if Length(Realms) > REALM_MAX then Exit(ThrowError('RangeError', 'too many realms'));
  { G2_REALMCHECK=<size of the TLS segment>: report the thread-local bytes the new realm's Init procedures changed
    that the switch back did not restore (state a realm needs that no Init procedure registered) }
  TlsSize := StrToIntDef(GetEnvironmentVariable('G2_REALMCHECK'), 0);
  if TlsSize > 0 then
  begin
    Base := ThreadPtr - TlsSize;
    SetLength(Before, TlsSize);
    Move(Base^, Before[0], TlsSize);
  end;
  Old := CurRealm;
  NewR := Length(Realms);
  SaveRealm(Old);
  SetLength(Realms, NewR + 1);
  for I := 0 to High(RealmValSlots) do RealmValSlots[I]^ := VUndefined;
  for I := 0 to High(RealmPtrSlots) do RealmPtrSlots[I]^ := nil;
  for I := 0 to High(RealmIntSlots) do RealmIntSlots[I]^ := 0;
  GLexCells := nil; GLexNames := nil; GLexConst := nil; GLexHash := nil; GLexCount := 0;
  GVarHash := nil; GVarCount := 0;
  FunctionShape := RootShapeFor(VNull);   { as VMInit: replaced once Function.prototype exists }
  CurRealm := NewR;
  MultiRealm := True;
  MultiRealmAny := True;
  RealmInit := True;
  try
    RealmInitProc;
  finally
    RealmInit := False;
  end;
  SaveRealm(NewR);
  LoadRealm(Old);
  CurRealm := Old;
  if TlsSize > 0 then
  begin
    SetLength(After, TlsSize);
    Move(Base^, After[0], TlsSize);
    I := 0;
    while I < TlsSize do
    begin
      if Before[I] <> After[I] then
      begin
        WriteLn(ErrOutput, 'realmcheck: TLS offset ', I, ' changed');
        I := (I or 7) + 1;
      end
      else Inc(I);
    end;
  end;
  R := NewR;
  Result := True;
end;

function FnRealm(O: PObj): Integer; inline;
begin
  Result := (O^.Pad shr REALM_SHIFT) and REALM_MAX;
end;

function GetFunctionRealm(F: TValue; var R: Integer): Boolean;
var O: PObj;
begin
  R := CurRealm;
  if not IsObject(F) then Exit(True);
  O := ObjOf(F);
  case O^.OKind of
    okFunction, okNative: R := FnRealm(O);
    okBound: Exit(GetFunctionRealm(O^.Aux1, R));
    okProxy:
      begin
        if not IsObject(O^.Aux2) then Exit(ThrowTypeError('Cannot get the realm of a revoked proxy'));
        Exit(GetFunctionRealm(O^.Aux1, R));
      end;
  end;
  Result := True;
end;

function MapIntrinsic(V: TValue; FromR, ToR: Integer): TValue;
var I: Integer; LF, LT: TValue;
begin
  Result := V;
  if (FromR = ToR) or (Length(Realms) = 0) then Exit;
  for I := 0 to High(RealmValSlots) do
    if ((FromR = CurRealm) and (RealmValSlots[I]^ = V)) or ((FromR <> CurRealm) and (Realms[FromR].Vals[I] = V)) then
    begin
      if ToR = CurRealm then Exit(RealmValSlots[I]^);
      Exit(Realms[ToR].Vals[I]);
    end;
  { an intrinsic a prelude registered }
  if FromR = CurRealm then LF := RealmValSlots[IntrinsicListIdx]^ else LF := Realms[FromR].Vals[IntrinsicListIdx];
  if ToR = CurRealm then LT := RealmValSlots[IntrinsicListIdx]^ else LT := Realms[ToR].Vals[IntrinsicListIdx];
  if not IsObject(LF) or not IsObject(LT) then Exit;
  for I := 0 to Min(ArrayLen(ObjOf(LF)), ArrayLen(ObjOf(LT))) - 1 do
    if ObjOf(LF)^.Elements^.Data[I] = V then Exit(ObjOf(LT)^.Elements^.Data[I]);
end;

procedure ScanRealms;
var I, J: Integer;
begin
  { the running realm's state, in its threadvars (some are not rooted otherwise: the values a SaveRealm copies must
    be current) }
  for I := 0 to High(RealmValSlots) do GCVisit(RealmValSlots[I]^);
  for J := 0 to High(Realms) do
    if J <> CurRealm then
      with Realms[J] do
      begin
        for I := 0 to High(Vals) do GCVisit(Vals[I]);
        for I := 0 to GLexCount - 1 do GCVisit(GLexCells[I]);
      end;
end;

procedure ScanRoots;
var I: Integer;
begin
  if GCVerify then
  begin
    for I := 0 to StackTop - 1 do CheckRoot(VStack[I], 'stack', I);
    for I := 0 to FrameTop - 1 do begin CheckRoot(Frames[I].Closure, 'frame', I); CheckRoot(Frames[I].NewTarget, 'newtarget', I); end;
    for I := 0 to OpenCount - 1 do CheckRoot(OpenUpvals[I], 'upval', I);
    for I := 0 to ExtraRootCount - 1 do CheckRoot(ExtraRoots[I]^, 'extra', I);
    for I := 0 to TempTop - 1 do CheckRoot(TempRoots[I], 'temp', I);
    CheckRoot(GlobalObj, 'global', 0);
  end;
  for I := 0 to StackTop - 1 do GCVisit(VStack[I]);
  for I := 0 to FrameTop - 1 do
  begin
    GCVisit(Frames[I].Closure);
    GCVisit(Frames[I].NewTarget);
  end;
  for I := 0 to OpenCount - 1 do GCVisit(OpenUpvals[I]);
  for I := 0 to GLexCount - 1 do GCVisit(GLexCells[I]);
  ScanRealms;
  for I := 0 to ExtraRootCount - 1 do GCVisit(ExtraRoots[I]^);
  GCVisit(GlobalObj);
  GCVisit(Pending);
  GCVisit(CurNewTarget);
  GCVisit(ConstructNT);
  if Assigned(HostRootScanner) then HostRootScanner;
end;

{ ---------------- upvalues ---------------- }

function FindOrOpenUpval(Idx: Integer): TValue;
var I, J, Lo, Hi, Mid: Integer; U: PUpval; V: TValue;
begin
  { OpenUpvals is sorted by stack index ascending: find the last one at or below Idx (a function that makes many
    closures keeps many open, so a linear scan per capture was quadratic) }
  Lo := 0; Hi := OpenCount - 1; I := -1;
  while Lo <= Hi do
  begin
    Mid := (Lo + Hi) shr 1;
    if PUpval(PtrOf(OpenUpvals[Mid]))^.StackIndex <= Idx then begin I := Mid; Lo := Mid + 1; end
    else Hi := Mid - 1;
  end;
  if (I >= 0) and (PUpval(PtrOf(OpenUpvals[I]))^.StackIndex = Idx) then Exit(OpenUpvals[I]);
  U := NewUpval;
  U^.StackIndex := Idx;
  V := MkPtr(U);
  if OpenCount >= Length(OpenUpvals) then SetLength(OpenUpvals, OpenCount * 2 + 64);
  for J := OpenCount - 1 downto I + 1 do OpenUpvals[J + 1] := OpenUpvals[J];
  OpenUpvals[I + 1] := V;
  Inc(OpenCount);
  Result := V;
end;

procedure CloseUpvals(FromIdx: Integer); inline;
var U: PUpval;
begin
  while OpenCount > 0 do
  begin
    U := PUpval(PtrOf(OpenUpvals[OpenCount - 1]));
    if U^.StackIndex < FromIdx then Break;
    U^.Value := VStack[U^.StackIndex];
    WriteBarrier(@U^.Hdr, U^.Value);
    U^.StackIndex := -1;
    Dec(OpenCount);
  end;
end;

{ ---------------- global declarations ---------------- }

function AtomSlot(Key: PStr; Mask: Integer): Integer; inline;
begin
  Result := Integer((PtrUInt(Key) shr 3) * 2654435761) and Mask;
end;

function GLexFind(Key: PStr): Integer;
var J: Integer;
begin
  if GLexHash = nil then Exit(-1);
  J := AtomSlot(Key, High(GLexHash));
  while GLexHash[J] <> 0 do
  begin
    if GLexNames[GLexHash[J] - 1] = Key then Exit(GLexHash[J] - 1);
    J := (J + 1) and High(GLexHash);
  end;
  Result := -1;
end;

function GLexIsConst(I: Integer): Boolean;
begin
  Result := GLexConst[I];
end;

function GLexAdd(Key: PStr; IsConst: Boolean): Integer;
var I, J: Integer;
begin
  if GLexCount >= Length(GLexNames) then
  begin
    SetLength(GLexNames, GLexCount * 2 + 16);
    SetLength(GLexConst, Length(GLexNames));
    SetLength(GLexCells, Length(GLexNames));
  end;
  Result := GLexCount;
  GLexNames[Result] := Key;
  GLexConst[Result] := IsConst;
  GLexCells[Result] := VUndefined;
  Inc(GLexCount);
  if GLexCount * 2 > Length(GLexHash) then
  begin
    I := 16;
    while I < GLexCount * 4 do I := I * 2;
    GLexHash := nil;
    SetLength(GLexHash, I);
    for I := 0 to GLexCount - 1 do
    begin
      J := AtomSlot(GLexNames[I], High(GLexHash));
      while GLexHash[J] <> 0 do J := (J + 1) and High(GLexHash);
      GLexHash[J] := I + 1;
    end;
  end
  else
  begin
    J := AtomSlot(Key, High(GLexHash));
    while GLexHash[J] <> 0 do J := (J + 1) and High(GLexHash);
    GLexHash[J] := Result + 1;
  end;
end;

function GVarHas(Key: PStr): Boolean;
var J: Integer;
begin
  if GVarHash = nil then Exit(False);
  J := AtomSlot(Key, High(GVarHash));
  while GVarHash[J] <> nil do
  begin
    if GVarHash[J] = Key then Exit(True);
    J := (J + 1) and High(GVarHash);
  end;
  Result := False;
end;

procedure GVarAdd(Key: PStr);
var Old: array of PStr; I, J, N: Integer;
begin
  if GVarHas(Key) then Exit;
  if (GVarCount + 1) * 2 > Length(GVarHash) then
  begin
    Old := GVarHash;
    N := 32;
    while N < (GVarCount + 1) * 4 do N := N * 2;
    GVarHash := nil;
    SetLength(GVarHash, N);
    for I := 0 to High(Old) do
      if Old[I] <> nil then
      begin
        J := AtomSlot(Old[I], High(GVarHash));
        while GVarHash[J] <> nil do J := (J + 1) and High(GVarHash);
        GVarHash[J] := Old[I];
      end;
  end;
  J := AtomSlot(Key, High(GVarHash));
  while GVarHash[J] <> nil do J := (J + 1) and High(GVarHash);
  GVarHash[J] := Key;
  Inc(GVarCount);
end;

{ the global object's own property Key: its flags, False if absent }
function GlobalOwn(Key: PStr; var Flags: Byte): Boolean;
var S: Integer; P: PObj;
begin
  P := ObjOf(GlobalObj);
  S := ShapeLookup(P^.Shape, Key);
  Result := S >= 0;
  if Result then Flags := P^.Shape^.PFlags[S];
end;

function GlobalExtensible: Boolean; inline;
begin
  Result := (ObjOf(GlobalObj)^.OFlags and 8) = 0;
end;

{ 9.1.1.4.15 CanDeclareGlobalFunction }
function CanDeclareGlobalFunction(Key: PStr): Boolean;
var F: Byte;
begin
  if not GlobalOwn(Key, F) then Exit(GlobalExtensible);
  Result := ((F and pfConfigurable) <> 0) or
    (((F and pfAccessor) = 0) and ((F and pfWritable) <> 0) and ((F and pfEnumerable) <> 0));
end;

{ 9.1.1.4.16 CanDeclareGlobalVar }
function CanDeclareGlobalVar(Key: PStr): Boolean;
var F: Byte;
begin
  Result := GlobalOwn(Key, F) or GlobalExtensible;
end;

{ 9.1.1.4.17 CreateGlobalVarBinding }
procedure CreateGlobalVarBinding(Key: PStr; Deletable: Boolean);
var F: Byte;
begin
  if not GlobalOwn(Key, F) and GlobalExtensible then
    DefineOwn(GlobalObj, Key, VUndefined, pfWritable or pfEnumerable or Ord(Deletable) * pfConfigurable);
  GVarAdd(Key);
end;

{ 9.1.1.4.18 CreateGlobalFunctionBinding }
procedure CreateGlobalFunctionBinding(Key: PStr; V: TValue; Deletable: Boolean);
var F: Byte; P: PObj;
begin
  if not GlobalOwn(Key, F) or ((F and pfConfigurable) <> 0) then
    DefineOwn(GlobalObj, Key, V, pfWritable or pfEnumerable or Ord(Deletable) * pfConfigurable)
  else
  begin
    P := ObjOf(GlobalObj);
    SetSlot(P, ShapeLookup(P^.Shape, Key), V);   { CanDeclareGlobalFunction held: a writable data property }
  end;
  GVarAdd(Key);
end;

function ThrowRedeclared(Key: PStr): Boolean;
begin
  Result := ThrowError('SyntaxError', 'Identifier ''' + StrToUnicode(Key) + ''' has already been declared');
end;

{ 16.1.7 GlobalDeclarationInstantiation (GdiKind 1), and 19.2.1.3 EvalDeclarationInstantiation of non-strict
  eval code whose variable environment is the global one (GdiKind 2). Function objects are bound afterwards by
  opDefGlobalFunc, once their closures exist; nothing in between can fail. }
function GlobalDeclInst(T: PFuncTemplate): Boolean;
var I: Integer; F: Byte; Fork: Boolean;
begin
  Result := False;
  if T^.GdiKind = 1 then
    for I := 0 to High(T^.GdiLex) do
    begin
      { HasLexicalDeclaration, then HasRestrictedGlobalProperty (ES2025 dropped [[VarNames]]: a var made by eval is
        configurable, so a later script's let may shadow it) }
      if GLexFind(T^.GdiLex[I]) >= 0 then Exit(ThrowRedeclared(T^.GdiLex[I]));
      if GlobalOwn(T^.GdiLex[I], F) and ((F and pfConfigurable) = 0) then Exit(ThrowRedeclared(T^.GdiLex[I]));
    end;
  for I := 0 to High(T^.GdiFuncs) do
    if GLexFind(T^.GdiFuncs[I]) >= 0 then Exit(ThrowRedeclared(T^.GdiFuncs[I]));
  for I := 0 to High(T^.GdiVars) do
    if GLexFind(T^.GdiVars[I]) >= 0 then Exit(ThrowRedeclared(T^.GdiVars[I]));
  for I := 0 to High(T^.GdiFuncs) do
    if not CanDeclareGlobalFunction(T^.GdiFuncs[I]) then
      Exit(ThrowTypeError('Cannot declare global function ' + StrToUnicode(T^.GdiFuncs[I])));
  for I := 0 to High(T^.GdiVars) do
    if not CanDeclareGlobalVar(T^.GdiVars[I]) then
      Exit(ThrowTypeError('Cannot declare global variable ' + StrToUnicode(T^.GdiVars[I])));
  T^.GdiLexBase := GLexCount;
  Fork := False;
  for I := 0 to High(T^.GdiLex) do
  begin
    { a configurable global property it shadows from now on: global inline caches that found it must miss }
    if GlobalOwn(T^.GdiLex[I], F) then Fork := True;
    GLexAdd(T^.GdiLex[I], T^.GdiLexConst[I]);
  end;
  if Fork then ObjOf(GlobalObj)^.Shape := ShapeFork(ObjOf(GlobalObj)^.Shape);
  for I := 0 to High(T^.GdiVars) do CreateGlobalVarBinding(T^.GdiVars[I], T^.GdiKind = 2);
  Result := True;
end;

const
  okRetComp = 25;   { a generator's return completion travelling as an exception: Aux1 the value }

function IsRetComp(V: TValue): Boolean; inline;
begin
  Result := IsObject(V) and (ObjOf(V)^.OKind = okRetComp);
end;

function IsRetCompV(V: TValue): Boolean;
begin
  Result := IsRetComp(V);
end;

function NewRetComp(V: TValue): TValue;
var O: PObj;
begin
  PushRoot(V);
  O := NewObject(RootShapeFor(VNull), 0, okRetComp);
  O^.Aux1 := RootAt(TempTop - 1);
  PopRoots(1);
  Result := MkPtr(O);
end;

function TakeEvalHint: Integer;
begin
  Result := -1;
  EvalDesc := VUndefined;
  if (EvalHintMode > 0) and (FrameTop = EvalHintFrame) and (FrameTop > 0) and
     (Frames[FrameTop - 1].PC = EvalHintPC + 1) then
  begin
    Result := EvalHintMode - 1;
    if EvalHintDesc > 0 then EvalDesc := Frames[FrameTop - 1].Tpl^.Consts[EvalHintDesc - 1];
  end;
  EvalHintMode := 0;
end;

function NewClosure(T: PFuncTemplate; Upvals: TValue): PObj; forward;

{ goal 4: the closure of a direct eval's code (compiled as an arrow in a stand-in for its caller): each upvalue the
  caller's binding the description names, a register of the calling frame (opened as an upvalue) or one of its
  closure's upvalues }
function MakeEvalClosure(T: PFuncTemplate; Desc: TValue; var Clo: TValue): Boolean;
var Fr: ^TFrame; Kinds, Idxs: array of Integer; D, Entry: UnicodeString; P, N, J, K: Integer; Fields: TUnicodeStringArray;
  Ups: PValueArray; V: TValue;
begin
  D := StrToUnicode(StrOf(Desc));
  N := 0;
  SetLength(Kinds, 0); SetLength(Idxs, 0);
  while D <> '' do
  begin
    P := Pos(#1, D);
    Entry := Copy(D, 1, P - 1);
    Delete(D, 1, P);
    Fields := Entry.Split([#2]);
    SetLength(Kinds, N + 1); SetLength(Idxs, N + 1);
    Kinds[N] := StrToInt(Fields[1]);
    Idxs[N] := StrToInt(Fields[2]);
    Inc(N);
  end;
  K := Length(T^.Upvals);
  if K = 0 then begin Clo := MkPtr(NewClosure(T, VUndefined)); Exit(True); end;
  Ups := NewValueArray(K);
  Ups^.Len := K;
  for J := 0 to K - 1 do Ups^.Data[J] := VUndefined;
  PushRoot(MkPtr(Ups));
  for J := 0 to K - 1 do
  begin
    Fr := @Frames[FrameTop - 1];
    P := T^.Upvals[J].Index;
    if Kinds[P] = 0 then V := FindOrOpenUpval(Fr^.Base + Idxs[P])
    else V := PValueArray(PtrOf(ObjOf(Fr^.Closure)^.Aux2))^.Data[Idxs[P]];
    ValueArrayStore(PValueArray(PtrOf(RootAt(TempTop - 1))), J, V);
  end;
  Clo := MkPtr(NewClosure(T, RootAt(TempTop - 1)));
  PopRoots(1);
  Result := True;
end;

function ProtoFromNewTarget(Default: TValue; var P: TValue): Boolean;
begin
  P := Default;
  ConstructProtoTaken := True;
  if IsObject(CurNewTarget) then Exit(GetPrototypeFromConstructor(CurNewTarget, Default, P));
  Result := True;
end;

function GetPrototypeFromConstructor(C, Default: TValue; var P: TValue): Boolean;
var R: Integer;
begin
  PushRoot(C); PushRoot(Default);
  Result := GetV(C, AtomPrototype, P);
  Default := RootAt(TempTop - 1); C := RootAt(TempTop - 2);
  PopRoots(2);
  if not Result or IsObject(P) then Exit;
  P := Default;
  { step 3.a GetFunctionRealm: a revoked proxy throws even with one realm }
  if not MultiRealm and IsObject(C) and (ObjOf(C)^.OKind = okProxy) and not GetFunctionRealm(C, R) then Exit(False);
  if MultiRealm then
  begin
    { step 3: the intrinsic of the constructor's realm }
    if not GetFunctionRealm(C, R) then Exit(False);
    P := MapIntrinsic(Default, CurRealm, R);
  end;
end;

function SymKey(Sym: TValue): PStr; inline;
begin
  Result := PStr(PPointer(PByte(PtrOf(Sym)) + 16)^);
end;

procedure DefineNativeGetterKey(O: TValue; Key: PStr; const Name: UnicodeString; Fn: TNativeFn);
var F, A: TValue;
begin
  PushRoot(O);
  F := NewNativeFunction(Fn, Name, 0);
  PushRoot(F);
  A := MkPtr(NewAccessor(RootAt(TempTop - 1), VUndefined));
  DefineOwn(RootAt(TempTop - 2), Key, A, pfConfigurable or pfAccessor);
  PopRoots(2);
end;

function IteratorCloseV(It: TValue; Throwing: Boolean): Boolean;
var Ret, R, Saved: TValue;
begin
  if Throwing then
  begin
    Saved := Pending;
    Pending := VUndefined;
    PushRoot(Saved); PushRoot(It);
    if GetV(It, AtomOf('return'), Ret) and IsCallable(Ret) then CallValue(Ret, RootAt(TempTop - 1), nil, 0, R);
    Pending := RootAt(TempTop - 2);
    PopRoots(2);
    Exit(False);
  end;
  PushRoot(It);
  if not GetV(It, AtomOf('return'), Ret) then begin PopRoots(1); Exit(False); end;
  if IsNullish(Ret) then begin PopRoots(1); Exit(True); end;
  if not CallValue(Ret, RootAt(TempTop - 1), nil, 0, R) then begin PopRoots(1); Exit(False); end;
  PopRoots(1);
  if not IsObject(R) then Exit(ThrowTypeError('iterator result is not an object'));
  Result := True;
end;

{ Array.prototype[@@iterator] and %ArrayIteratorPrototype%.next are the intrinsics: array iteration may skip
  the lookups. Slots are cached by shape; the values are read every time (an assignment keeps the shape). }
threadvar
  AIShapeA, AIShapeB: PShape;
  AISlotA, AISlotB: Integer;

function ArrayIterNextFnOf: TValue; noinline;
begin
  Result := ArrayIterNextFn;
end;

function ArrayIterationIntact: Boolean;
var A, B: PObj;
begin
  A := ObjOf(ArrayProto); B := ObjOf(ArrayIteratorProto);
  if A^.Shape <> AIShapeA then begin AIShapeA := A^.Shape; AISlotA := ShapeLookup(A^.Shape, SymKey(SymIterator)); end;
  if B^.Shape <> AIShapeB then begin AIShapeB := B^.Shape; AISlotB := ShapeLookup(B^.Shape, AtomNext); end;
  Result := (AISlotA >= 0) and (AISlotB >= 0) and ((A^.Shape^.PFlags[AISlotA] and pfAccessor) = 0) and
    ((B^.Shape^.PFlags[AISlotB] and pfAccessor) = 0) and (GetSlot(A, AISlotA) = ArrayValuesFnPublic) and
    (GetSlot(B, AISlotB) = ArrayIterNextFn);
end;

function GetIteratorRec(V: TValue; var It, NextM: TValue): Boolean;
var F: TValue;
begin
  { strings, arguments objects and typed arrays iterate like arrays while the intrinsics are in place }
  if (IsString(V) or (IsObject(V) and (ObjOf(V)^.OKind in [okArguments, okTypedArray]))) and ArrayIterationIntact then
  begin
    if not GetIterator(V, It) then Exit(False);
    if IsObject(It) and (ObjOf(It)^.OKind = okIterator) then
    begin
      if ObjOf(It)^.OFlags = 1 then NextM := StringIterNextFn else NextM := ArrayIterNextFn;
    end
    else
    begin
      PushRoot(It);
      Result := GetV(It, AtomNext, NextM);
      It := RootAt(TempTop - 1);
      PopRoots(1);
      Exit;
    end;
    Exit(True);
  end;
  if IsObject(V) and (ObjOf(V)^.OKind = okArray) and (ObjOf(V)^.Shape^.Proto = ArrayProto) and
     (ShapeLookup(ObjOf(V)^.Shape, SymKey(SymIterator)) < 0) and ArrayIterationIntact then
  begin
    It := NewArrayIterator(V, 0);
    NextM := ArrayIterNextFn;
    Exit(True);
  end;
  PushRoot(V);
  if not GetElemV(V, SymIterator, F) then begin PopRoots(1); Exit(False); end;
  if not IsCallable(F) then begin PopRoots(1); Exit(ThrowTypeError('value is not iterable')); end;
  if not CallValue(F, RootAt(TempTop - 1), nil, 0, It) then begin PopRoots(1); Exit(False); end;
  PopRoots(1);
  if not IsObject(It) then Exit(ThrowTypeError('iterator is not an object'));
  PushRoot(It);
  if not GetV(It, AtomNext, NextM) then begin PopRoots(1); Exit(False); end;
  It := RootAt(TempTop - 1);
  PopRoots(1);
  Result := True;
end;

function IteratorStepRec(It, NextM: TValue; var Value: TValue; var Done: Boolean): Boolean;
var R: TValue;
begin
  if ((NextM = ArrayIterNextFn) or (NextM = MapIterNextFn) or (NextM = SetIterNextFn)) and IsObject(It) and
     (ObjOf(It)^.OKind = okIterator) and ((ObjOf(It)^.OFlags >= 5) = (NextM <> ArrayIterNextFn)) and
     ((ObjOf(It)^.OFlags >= 8) = (NextM = SetIterNextFn)) then
    Exit(IteratorStep(It, Value, Done));
  if (NextM = StringIterNextFn) and IsObject(It) and (ObjOf(It)^.OKind = okIterator) and (ObjOf(It)^.OFlags = 1) then
    Exit(IteratorStep(It, Value, Done));
  if (NextM = GenNextFn) and IsObject(It) and (ObjOf(It)^.OKind = okGenerator) then
  begin
    GenYieldRaw := False;
    if not GeneratorResume(It, VUndefined, 0, Value, Done) then Exit(False);
    if not GenYieldRaw then Exit(True);
    GenYieldRaw := False;
    R := Value;
  end
  else
  begin
    if not IsCallable(NextM) then Exit(ThrowTypeError('iterator.next is not a function'));
    if not CallValue(NextM, It, nil, 0, R) then Exit(False);
  end;
  if not IsObject(R) then Exit(ThrowTypeError('iterator result is not an object'));
  PushRoot(R);
  if not GetV(R, AtomDone, Value) then begin PopRoots(1); Exit(False); end;
  Done := ToBool(Value);
  Value := VUndefined;
  if not Done then
    if not GetV(RootAt(TempTop - 1), AtomValue, Value) then begin PopRoots(1); Exit(False); end;
  PopRoots(1);
  Result := True;
end;

function ThrowGLexTDZ(I: Integer): Boolean;
begin
  Result := ThrowError('ReferenceError', 'Cannot access ''' + StrToUnicode(GLexNames[I]) + ''' before initialization');
end;

{ the value of global lexical binding I; False (ReferenceError) while its Script has not reached it }
function GLexGet(I: Integer; var V: TValue): Boolean;
var U: PUpval;
begin
  if GLexCells[I] = VUndefined then Exit(ThrowGLexTDZ(I));
  U := PUpval(PtrOf(GLexCells[I]));
  if U^.StackIndex >= 0 then V := VStack[U^.StackIndex] else V := U^.Value;
  if V = VHole then Exit(ThrowGLexTDZ(I));
  Result := True;
end;

function GLexSet(I: Integer; V: TValue): Boolean;
var U: PUpval;
begin
  if GLexConst[I] then Exit(ThrowTypeError('Assignment to constant variable.'));
  if GLexCells[I] = VUndefined then Exit(ThrowGLexTDZ(I));
  U := PUpval(PtrOf(GLexCells[I]));
  if U^.StackIndex >= 0 then VStack[U^.StackIndex] := V
  else
  begin
    U^.Value := V;
    WriteBarrier(@U^.Hdr, V);
  end;
  Result := True;
end;

{ ---------------- errors ---------------- }

function MakeError(Proto: TValue; const Msg: UnicodeString): TValue;
var O: PObj; M: TValue;
begin
  PushRoot(Proto);   { the message's allocation can move a young prototype }
  M := StrFromUnicode(Msg);
  PushRoot(M);
  O := NewObject(RootShapeFor(RootAt(TempTop - 2)), 4, okError);
  M := RootAt(TempTop - 1);
  PopRoots(2);
  AddProperty(O, AtomMessage, M, pfHidden);
  Result := MkPtr(O);
end;

function ThrowError(const Kind, Msg: UnicodeString): Boolean;
var P: TValue;
begin
  if Kind = 'TypeError' then P := TypeErrorProto
  else if Kind = 'RangeError' then P := RangeErrorProto
  else if Kind = 'SyntaxError' then P := SyntaxErrorProto
  else if Kind = 'ReferenceError' then P := ReferenceErrorProto
  else if Kind = 'URIError' then P := URIErrorProto
  else if Kind = 'EvalError' then P := EvalErrorProto
  else P := ErrorProto;
  Pending := MakeError(P, Msg);
  Result := False;
end;

function ThrowTypeError(const Msg: UnicodeString): Boolean;
begin
  Result := ThrowError('TypeError', Msg);
end;

{ ---------------- functions ---------------- }

function DescribeTemplate(P: Pointer): string;
var T: PFuncTemplate;
begin
  T := PFuncTemplate(P);
  Result := '?';
  if T = nil then Exit;
  if T^.Name <> nil then Result := string(StrToUnicode(T^.Name)) else Result := '<anon>';
  Result := Result + ' @' + string(T^.SourceName);
  if Length(T^.Lines) > 0 then Result := Result + ':' + IntToStr(T^.Lines[0]);
end;

{ the GC-verify report of a closure made with a bad upvalue array (out of NewClosure: its strings would give it an
  exception frame) }
procedure XBadClosure(T: PFuncTemplate; C: PObj); noinline;
begin
  WriteLn(ErrOutput, 'closure ', DescribeTemplate(T), ' created with a bad upvalue array ', IntToHex(C^.Aux2, 16), ' minor ', GCMinorCount, ' temptop ', TempTop, ' cap ', Length(TempRoots));
  Halt(9);
end;

function NewClosure(T: PFuncTemplate; Upvals: TValue): PObj;
begin
  PushRoot(Upvals);
  if not (T^.IsGenerator or T^.IsAsync) or (GenFunctionShape = nil) then Result := NewObject(FunctionShape, 3, okFunction)
  else if T^.IsGenerator and T^.IsAsync then Result := NewObject(AsyncGenFunctionShape, 3, okFunction)
  else if T^.IsGenerator then Result := NewObject(GenFunctionShape, 3, okFunction)
  else Result := NewObject(AsyncFunctionShape, 3, okFunction);
  Result^.Native := T;
  Result^.Aux2 := RootAt(TempTop - 1);
  Result^.Pad := UInt32(CurRealm) shl REALM_SHIFT;
  PopRoots(1);
  if GCVerify and not NurseryCellOK(Result^.Aux2) then XBadClosure(T, Result);
  if T^.IsArrow then Result^.OFlags := Result^.OFlags or 4;
  Result^.Slots[0] := MkInt(T^.Length);
  Result^.Slots[1] := MkPtr(T^.Name);
end;

function NewNativeFunction(Fn: TNativeFn; const Name: UnicodeString; Length: Integer): TValue;
var O: PObj;
begin
  O := NewObject(FunctionShape, 3, okNative);
  O^.Native := Pointer(@Fn);
  O^.Pad := UInt32(CurRealm) shl REALM_SHIFT;
  O^.Aux1 := MkPtr(AtomOf(Name));
  O^.Aux2 := MkInt(Length);
  O^.Slots[0] := MkInt(Length);
  O^.Slots[1] := MkPtr(AtomOf(Name));
  Result := MkPtr(O);
end;

function NewNativeFunctionData(Fn: TNativeFn; const Name: UnicodeString; Length: Integer; Data: UInt32): TValue;
begin
  Result := NewNativeFunction(Fn, Name, Length);
  ObjOf(Result)^.Pad := ObjOf(Result)^.Pad or Data;
end;

function DefineOwn(O: TValue; Key: PStr; V: TValue; Flags: Byte): Boolean;
var P: PObj; S: Integer;
begin
  P := ObjOf(O);
  { the built-in constructors are the natives given a prototype property (Symbol and BigInt included: their
    [[Construct]] throws) }
  if (Key = AtomPrototype) and (P^.OKind = okNative) and IntrinsicsInit then P^.OFlags := P^.OFlags or 32;
  S := ShapeLookup(P^.Shape, Key);
  if S >= 0 then
  begin
    if P^.Shape^.PFlags[S] <> Flags then
    begin
      { flags change: same position, new attributes }
      ChangeFlags(P, Key, Flags);
      SetSlot(P, S, V);
    end
    else
      SetSlot(P, S, V);
  end
  else
    AddProperty(P, Key, V, Flags);
  Result := True;
end;

procedure DefineHidden(O: TValue; const Name: UnicodeString; V: TValue);
begin
  DefineOwn(O, AtomOf(Name), V, pfHidden);
end;

procedure DefineNative(O: TValue; const Name: UnicodeString; Fn: TNativeFn; Length: Integer);
var F: TValue;
begin
  PushRoot(O);
  F := NewNativeFunction(Fn, Name, Length);
  O := RootAt(TempTop - 1);
  PopRoots(1);
  DefineOwn(O, AtomOf(Name), F, pfHidden);
end;

{ a function's 'prototype' object, created on first use }
{ FunctionPrototypeOf's first use of a function's prototype property: made in the function's realm (MakeConstructor
  ran when it was created) }
function MakeFunctionPrototype(F: TValue): TValue; noinline;
var O, P: PObj; T: PFuncTemplate;
begin
  O := ObjOf(F);
  T := PFuncTemplate(O^.Native);
  if T^.IsGenerator then
  begin
    { 15.5.4 / 15.6.4: a generator's prototype inherits %GeneratorPrototype% (or the async one) and has no
      constructor property }
    PushRoot(F);
    if T^.IsAsync then P := NewPlainObject(MapIntrinsic(AsyncGeneratorProto, CurRealm, FnRealm(O)), 4)
    else P := NewPlainObject(MapIntrinsic(GeneratorProto, CurRealm, FnRealm(O)), 4);
    PushRoot(MkPtr(P));
    F := RootAt(TempTop - 2);
    AddProperty(ObjOf(F), AtomPrototype, RootAt(TempTop - 1), pfWritable);
    Result := RootAt(TempTop - 1);
    PopRoots(2);
    Exit;
  end;
  if T^.IsArrow or T^.IsMethod or T^.IsAsync then Exit(VUndefined);
  PushRoot(F);
  P := NewPlainObject(MapIntrinsic(ObjectProto, CurRealm, FnRealm(O)), 4);
  PushRoot(MkPtr(P));
  AddProperty(P, AtomConstructor, RootAt(TempTop - 2), pfHidden);
  F := RootAt(TempTop - 2);
  AddProperty(ObjOf(F), AtomPrototype, RootAt(TempTop - 1), pfWritable);
  Result := RootAt(TempTop - 1);
  PopRoots(2);
end;

function FunctionPrototypeOf(F: TValue): TValue;
var O: PObj; S: Integer;
begin
  O := ObjOf(F);
  S := ShapeLookup(O^.Shape, AtomPrototype);
  if S >= 0 then Exit(GetSlot(O, S));
  if O^.OKind <> okFunction then Exit(VUndefined);
  Result := MakeFunctionPrototype(F);
end;

{ 10.1.14 GetPrototypeFromConstructor's Get(constructor, "prototype"): a function's own data property (made on
  first use), else a full [[Get]] (an accessor, a proxy, a bound function's own property), which can run code }
function PrototypeForConstruct(NewTarget: TValue; var Proto: TValue): Boolean;
var O: PObj; S: Integer;
begin
  O := ObjOf(NewTarget);
  S := ShapeLookup(O^.Shape, AtomPrototype);
  if ((S >= 0) and ((O^.Shape^.PFlags[S] and pfAccessor) = 0)) or ((S < 0) and (O^.OKind = okFunction)) then
  begin
    Proto := FunctionPrototypeOf(NewTarget);
    Exit(True);
  end;
  Result := GetV(NewTarget, AtomPrototype, Proto);
end;

{ OrdinaryCreateFromConstructor's intrinsicDefaultProto for constructor F, used when newTarget NT's prototype
  is not an object (10.1.14 step 3: the intrinsic of NT's realm): a built-in class written in a prelude (marked
  by MarkIntrinsicClass) falls back to its own prototype property, any other constructor to %Object.prototype% }
function DefaultConstructProto(F, NT: TValue; var P: TValue): Boolean;
var O: PObj; S, R, FR: Integer;
begin
  O := ObjOf(F);
  P := ObjectProto;
  FR := CurRealm;
  if (O^.OKind = okFunction) and ((O^.Pad and INTRINSIC_CLASS) <> 0) then
  begin
    S := ShapeLookup(O^.Shape, AtomPrototype);
    if (S >= 0) and ((O^.Shape^.PFlags[S] and pfAccessor) = 0) then
    begin
      P := GetSlot(O, S);
      FR := FnRealm(O);
    end;
  end;
  Result := True;
  { GetFunctionRealm(new.target): a revoked proxy throws even with one realm }
  if not MultiRealm and IsObject(NT) and (ObjOf(NT)^.OKind = okProxy) and not GetFunctionRealm(NT, R) then Exit(False);
  if MultiRealm then
  begin
    if not GetFunctionRealm(NT, R) then Exit(False);
    P := MapIntrinsic(P, FR, R);
  end;
end;

procedure AppendIntrinsic(V: TValue);
begin
  PushRoot(V);
  if not IsObject(IntrinsicList) then IntrinsicList := MkPtr(NewArray(0, 64));
  ArrayPush(ObjOf(IntrinsicList), RootAt(TempTop - 1));
  PopRoots(1);
end;

{ markIntrinsic(V), for preludes: V is an intrinsic. A function is a built-in class (DefaultConstructProto):
  it and its prototype are registered.
  markIntrinsic(V, C): V, an intrinsic of the running realm, as the same intrinsic of constructor C's realm
  (10.1.14 GetPrototypeFromConstructor step 3 for a prelude's OrdinaryCreateFromConstructor) }
function MarkIntrinsicClass(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var O: PObj; S, R: Integer;
begin
  Res := VUndefined;
  if Argc >= 2 then
  begin
    if not GetFunctionRealm(Args[2], R) then Exit(False);
    Res := MapIntrinsic(Args[1], CurRealm, R);
    Exit(True);
  end;
  Result := True;
  if (Argc < 1) or not IsObject(Args[1]) then Exit;
  O := ObjOf(Args[1]);
  AppendIntrinsic(Args[1]);
  if O^.OKind <> okFunction then Exit;
  O := ObjOf(Args[1]);
  O^.Pad := O^.Pad or INTRINSIC_CLASS;
  S := ShapeLookup(O^.Shape, AtomPrototype);
  if (S >= 0) and ((O^.Shape^.PFlags[S] and pfAccessor) = 0) and IsObject(GetSlot(O, S)) then
    AppendIntrinsic(GetSlot(O, S));
end;

{ ---------------- property access ---------------- }

{ an index write an array keeps dense: near its length, or anywhere below 4M elements }
function ArrayDenseOK(E: PValueArray; Idx: Int64): Boolean; inline;
begin
  { past ArrayDenseMax an index is a property of the shape and the length is kept apart (ArraySetLengthSparse) }
  Result := (Idx < ArrayDenseMax) and ((Idx < Int64(E^.Len) + 1024 + Int64(E^.Len) * 2) or (Idx < 4 * 1024 * 1024));
end;

{ [[Delete]] on an ordinary object: a non-configurable property stays (the delete reports false) }
{ 10.4.4 a mapped arguments object: okArgumentsOrd whose Aux1 is the parameter map, a value array whose entry I
  is parameter I's upvalue (open while the call runs, then closed) or undefined once the index is unmapped }
function ArgsMapCell(O: PObj; Key: PStr; var Idx: Int64): PUpval;
var E: PValueArray;
begin
  Result := nil;
  if (O^.OKind <> okArgumentsOrd) or ((O^.Aux1 shr 48) <> TAG_PTR_HI) then Exit;
  if not IndexFromString(Key, Idx) then Exit;
  E := PValueArray(PtrOf(O^.Aux1));
  if (Idx >= E^.Len) or ((E^.Data[Idx] shr 48) <> TAG_PTR_HI) then Exit;
  Result := PUpval(PtrOf(E^.Data[Idx]));
end;

function ArgsMapGet(U: PUpval): TValue;
begin
  if U^.StackIndex >= 0 then Result := VStack[U^.StackIndex] else Result := U^.Value;
end;

procedure ArgsMapSet(U: PUpval; V: TValue);
begin
  if U^.StackIndex >= 0 then VStack[U^.StackIndex] := V
  else
  begin
    U^.Value := V;
    WriteBarrier(@U^.Hdr, V);
  end;
end;

procedure ArgsUnmap(O: PObj; Idx: Int64);
begin
  ValueArrayStore(PValueArray(PtrOf(O^.Aux1)), Idx, VUndefined);
end;

{ CreateMappedArgumentsObject's parameter map: the arguments object in register ArgsReg made ordinary (its
  indices shape properties), its first N indices aliasing the parameter registers 1 .. N }
function XMapArguments(Base, N, ArgsReg: Integer): Boolean; noinline;
var E: PValueArray; I: Integer; V: TValue;
begin
  VStack[Base + ArgsReg] := DemoteArguments(VStack[Base + ArgsReg]);
  E := NewValueArray(N);
  E^.Len := N;
  for I := 0 to N - 1 do E^.Data[I] := VUndefined;
  PushRoot(MkPtr(E));
  for I := 0 to N - 1 do
  begin
    V := FindOrOpenUpval(Base + 1 + I);
    ValueArrayStore(PValueArray(PtrOf(RootAt(TempTop - 1))), I, V);
  end;
  ObjOf(VStack[Base + ArgsReg])^.Aux1 := RootAt(TempTop - 1);
  WriteBarrier(@ObjOf(VStack[Base + ArgsReg])^.Hdr, RootAt(TempTop - 1));
  PopRoots(1);
  Result := True;
end;

function DeleteIfConfigurable0(O: PObj; Key: PStr): Boolean; forward;

{ 10.4.4.5 [[Delete]]: an index deleted from a mapped arguments object is unmapped }
function DeleteIfConfigurable(O: PObj; Key: PStr): Boolean;
var U: PUpval; Idx: Int64;
begin
  U := ArgsMapCell(O, Key, Idx);
  if U = nil then Exit(DeleteIfConfigurable0(O, Key));
  PushRoot(MkPtr(O));
  Result := DeleteIfConfigurable0(O, Key);
  O := ObjOf(RootAt(TempTop - 1));
  PopRoots(1);
  if Result then ArgsUnmap(O, Idx);
end;

function DeleteIfConfigurable0(O: PObj; Key: PStr): Boolean;
var S: Integer; Idx: Int64; V: TValue; Found: Boolean;
begin
  S := ShapeLookup(O^.Shape, Key);
  if (S >= 0) and ((O^.Shape^.PFlags[S] and pfConfigurable) = 0) then Exit(False);
  if (O^.OKind = okTypedArray) and (S < 0) and CanonicalNumericNonIndex(Key) then Exit(True);
  if (O^.OKind = okModuleNS) and ((Key^.Hdr.Extra and kfSymbol) = 0) then
  begin
    NSLookup(O, Key, False, V, Found);
    Exit(not Found);   { 10.4.6.10 }
  end;
  { an array's length, a String object's length and characters: non-configurable }
  if (S < 0) and (O^.OKind = okArray) and (Key = AtomLength) then Exit(False);
  if (S < 0) and (O^.OKind = okStringObj) and
     ((Key = AtomLength) or (IndexFromString(Key, Idx) and (Idx < StrOf(O^.Aux1)^.Len))) then Exit(False);
  if (O^.OKind = okArguments) and (Key = AtomLength) then
  begin
    O := ObjOf(DemoteArguments(MkPtr(O)));
    DeleteProperty(O, Key);
    Exit(True);
  end;
  if (S < 0) and IndexFromString(Key, Idx) then
  begin
    { 10.4.5.6: a typed array's in-bounds element is not deletable }
    if O^.OKind = okTypedArray then Exit(Idx >= TALength(O));
    { an element (default attributes; non-configurable ones live in the shape) }
    if (O^.Elements <> nil) and (Idx < O^.Elements^.Len) then
    begin
      O^.Elements^.Data[Idx] := VHole;
      Exit(True);
    end;
  end;
  { a function's 'prototype' not created yet is non-configurable too }
  if (S < 0) and (O^.OKind = okFunction) and (Key = AtomPrototype) and ((O^.OFlags and 4) = 0) and
     (PFuncTemplate(O^.Native)^.IsGenerator or
      not (PFuncTemplate(O^.Native)^.IsArrow or PFuncTemplate(O^.Native)^.IsMethod or PFuncTemplate(O^.Native)^.IsAsync)) then
    Exit(False);
  DeleteProperty(O, Key);
  Result := True;
end;

function TypeErrorRead(Obj: TValue; Key: PStr): Boolean;
begin
  if Obj = VNull then Result := ThrowTypeError('Cannot read properties of null (reading ''' + StrToUnicode(Key) + ''')')
  else Result := ThrowTypeError('Cannot read properties of undefined (reading ''' + StrToUnicode(Key) + ''')');
end;

function ProtoForPrimitive(V: TValue): TValue; inline;
begin
  if IsNumber(V) then Result := NumberProto
  else if (V = VTrue) or (V = VFalse) then Result := BooleanProto
  else if IsString(V) then Result := StringProto
  else if IsPtr(V) and (PCell(PtrOf(V))^.Kind = ckSymbol) then Result := SymbolProto
  else if IsBigInt(V) then Result := BigIntProto
  else Result := VNull;
end;

function VirtualProp(O: PObj; Key: PStr; var Res: TValue): Boolean;
var T: PFuncTemplate; Idx: Int64;
begin
  Result := False;
  case O^.OKind of
    okArray, okArguments:
      if Key = AtomLength then
      begin
        if (O^.OFlags and ofBigLen) = 0 then Res := MkInt(O^.Elements^.Len) else Res := MkNumber(ArrayLength(O));
        Exit(True);
      end;
    okStringObj:
      begin
        if Key = AtomLength then begin Res := MkInt(StrOf(O^.Aux1)^.Len); Exit(True); end;
        if IndexFromString(Key, Idx) and (Idx < StrOf(O^.Aux1)^.Len) then
        begin
          Res := SubString(StrOf(O^.Aux1), Idx, 1);
          Exit(True);
        end;
      end;
  end;
end;

{ property lookup on an object chain; Receiver is the this for getters }
function ProxyGet(PV: TValue; Key: PStr; Receiver: TValue; var Res: TValue): Boolean; forward;

function GetFromObject(O: PObj; Key: PStr; Receiver: TValue; var Res: TValue): Boolean;
var S: Integer; Flags: Byte; Acc: PAccessor; P: TValue; Idx: Int64; V: TValue; Found: Boolean; U: PUpval;
begin
  while True do
  begin
    if O^.OKind = okProxy then Exit(ProxyGet(MkPtr(O), Key, Receiver, Res));
    { 10.4.4.3 [[Get]] of a mapped index: the parameter's current value }
    if (O^.OKind = okArgumentsOrd) and ((O^.Aux1 shr 48) = TAG_PTR_HI) then
    begin
      U := ArgsMapCell(O, Key, Idx);
      if U <> nil then begin Res := ArgsMapGet(U); Exit(True); end;
    end;
    if (O^.OKind = okModuleNS) and ((Key^.Hdr.Extra and kfSymbol) = 0) then
    begin
      if not NSLookup(O, Key, True, Res, Found) then Exit(False);
      if not Found then Res := VUndefined;
      Exit(True);
    end;
    { 10.4.5.4: a typed array's integer index is its element or undefined, never inherited }
    if (O^.OKind = okTypedArray) and IndexFromString(Key, Idx) then
    begin
      if Idx < TALength(O) then Res := TAGet(O, Idx) else Res := VUndefined;
      Exit(True);
    end;
    if (O^.OKind = okTypedArray) and CanonicalNumericNonIndex(Key) then begin Res := VUndefined; Exit(True); end;
    S := ShapeLookup(O^.Shape, Key);
    if S >= 0 then
    begin
      Flags := O^.Shape^.PFlags[S];
      V := GetSlot(O, S);
      if (Flags and pfAccessor) <> 0 then
      begin
        Acc := PAccessor(PtrOf(V));
        if Acc^.Getter = VUndefined then begin Res := VUndefined; Exit(True); end;
        AccessorGetKey := Key;
        Exit(CallValue(Acc^.Getter, Receiver, nil, 0, Res));
      end;
      Res := V;
      Exit(True);
    end;
    if VirtualProp(O, Key, Res) then Exit(True);
    if (O^.Elements <> nil) and IndexFromString(Key, Idx) then
    begin
      if Idx < O^.Elements^.Len then
      begin
        Res := O^.Elements^.Data[Idx];
        if Res <> VHole then Exit(True);
      end;
    end;
    if (O^.OKind = okFunction) and (Key = AtomPrototype) then
    begin
      Res := FunctionPrototypeOf(MkPtr(O));
      Exit(True);
    end;
    P := O^.Shape^.Proto;
    if not IsObject(P) then begin Res := VUndefined; Exit(True); end;
    O := ObjOf(P);
  end;
end;

function GetV(Obj: TValue; Key: PStr; var Res: TValue): Boolean;
var Idx: Int64; S: PStr; P: TValue;
begin
  if IsObject(Obj) then Exit(GetFromObject(ObjOf(Obj), Key, Obj, Res));
  if IsNullish(Obj) then Exit(TypeErrorRead(Obj, Key));
  if IsString(Obj) then
  begin
    S := StrOf(Obj);
    if Key = AtomLength then begin Res := MkInt(S^.Len); Exit(True); end;
    if IndexFromString(Key, Idx) then
    begin
      if Idx < S^.Len then Res := SubString(S, Idx, 1) else Res := VUndefined;
      Exit(True);
    end;
  end;
  P := ProtoForPrimitive(Obj);
  if not IsObject(P) then begin Res := VUndefined; Exit(True); end;
  Result := GetFromObject(ObjOf(P), Key, Obj, Res);
end;


{ An arguments object keeps its length equal to its element count, which the array fast paths rely on. An
  operation that would break that (a new index, a write, define or delete of length) first turns it into an
  ordinary object with the indices and length as shape properties. Returns the object (it may have moved). }
function DemoteArguments(Obj: TValue): TValue;
var I, N: Integer; K: PStr; V: TValue;
begin
  PushRoot(Obj);
  N := ObjOf(Obj)^.Elements^.Len;
  for I := 0 to N - 1 do
  begin
    K := IntAtom(I);
    V := ObjOf(RootAt(TempTop - 1))^.Elements^.Data[I];
    if V <> VHole then AddProperty(ObjOf(RootAt(TempTop - 1)), K, V, pfDefault);
  end;
  AddProperty(ObjOf(RootAt(TempTop - 1)), AtomLength, MkInt(N), pfHidden);
  ObjOf(RootAt(TempTop - 1))^.Elements := nil;
  ObjOf(RootAt(TempTop - 1))^.OKind := okArgumentsOrd;
  Result := RootAt(TempTop - 1);
  PopRoots(1);
end;

function NSLookup(O: PObj; Key: PStr; WantValue: Boolean; var V: TValue; var Found: Boolean): Boolean;
var E: PValueArray; Lo, Hi, M, C: Integer; Cell: TValue;
begin
  Found := False;
  Result := True;
  { a deferred namespace (OFlags 16): "then" is no export of it; any other string key evaluates its module first }
  if (O^.OFlags and 16) <> 0 then
  begin
    if StrToUnicode(Key) = 'then' then Exit;
    PushRoot(MkPtr(O));
    Result := NSDeferHook(RootAt(TempTop - 1));
    O := ObjOf(RootAt(TempTop - 1));
    PopRoots(1);
    if not Result then Exit;
  end;
  E := ObjOf(O^.Aux1)^.Elements;
  Lo := 0; Hi := E^.Len div 2 - 1;
  while Lo <= Hi do
  begin
    M := (Lo + Hi) div 2;
    C := StrCompare(PStr(PtrOf(E^.Data[M * 2])), Key);
    if C = 0 then
    begin
      Found := True;
      if not WantValue then Exit;
      Cell := E^.Data[M * 2 + 1];
      if PUpval(PtrOf(Cell))^.StackIndex >= 0 then V := VStack[PUpval(PtrOf(Cell))^.StackIndex]
      else V := PUpval(PtrOf(Cell))^.Value;
      if V = VHole then
        Exit(ThrowError('ReferenceError', 'Cannot access ''' + StrToUnicode(Key) + ''' before initialization'));
      Exit;
    end;
    if C < 0 then Lo := M + 1 else Hi := M - 1;
  end;
end;

{ 7.1.14 StringToBigInt (decimal, or 0x / 0o / 0b without a sign); False: not a StringIntegerLiteral }
function StringToBigIntV(const S0: UnicodeString; var B: TValue): Boolean;
var S: UnicodeString; I, Radix, D: Integer; Neg: Boolean; R: TBigInteger;
begin
  S := Trim(S0);
  Result := False;
  Neg := False;
  Radix := 10;
  I := 1;
  if S = '' then begin B := BigFromInt64(0); Exit(True); end;
  if (Length(S) > 2) and (S[1] = '0') and CharInSet(S[2], ['x', 'X', 'o', 'O', 'b', 'B']) then
  begin
    case S[2] of
      'x', 'X': Radix := 16;
      'o', 'O': Radix := 8;
    else Radix := 2;
    end;
    I := 3;
  end
  else if (S[1] = '+') or (S[1] = '-') then
  begin
    Neg := S[1] = '-';
    I := 2;
  end;
  if I > Length(S) then Exit;
  R := TBigInteger.Zero;
  while I <= Length(S) do
  begin
    case S[I] of
      '0'..'9': D := Ord(S[I]) - 48;
      'a'..'f': D := Ord(S[I]) - 87;
      'A'..'F': D := Ord(S[I]) - 55;
    else Exit;
    end;
    if D >= Radix then Exit;
    R := R.Multiply(TBigInteger.FromInt64(Radix)).Add(TBigInteger.FromInt64(D));
    Inc(I);
  end;
  if Neg then R := R.Negate;
  B := BigToValue(R);
  Result := True;
end;

function ToBigIntV(V: TValue; var B: TValue): Boolean;
var P: TValue;
begin
  if IsObject(V) then
  begin
    if not ToPrimitiveHint(V, phNumber, P) then Exit(False);
    V := P;
  end;
  if IsBigInt(V) then begin B := V; Exit(True); end;
  if V = VTrue then begin B := BigFromInt64(1); Exit(True); end;
  if V = VFalse then begin B := BigFromInt64(0); Exit(True); end;
  if IsString(V) then
  begin
    if not StringToBigIntV(StrToUnicode(StrOf(V)), B) then
      Exit(ThrowError('SyntaxError', 'Cannot convert ' + StrToUnicode(StrOf(V)) + ' to a BigInt'));
    Exit(True);
  end;
  if IsNumber(V) then Exit(ThrowTypeError('Cannot convert a Number to a BigInt'));
  if IsPtr(V) and (PCell(PtrOf(V))^.Kind = ckSymbol) then Exit(ThrowTypeError('Cannot convert a Symbol value to a BigInt'));
  Result := ThrowTypeError('Cannot convert ' + TypeOfStr(V) + ' to a BigInt');
end;

function CanonicalNumericNonIndex(Key: PStr): Boolean;
var C: WideChar; Idx: Int64;
begin
  Result := False;
  if (Key^.Hdr.Extra <> 0) or (Key^.Len = 0) or IndexFromString(Key, Idx) then Exit;
  C := StrChars(Key)[0];
  if not ((C = '-') or ((C >= '0') and (C <= '9')) or (C = 'I') or (C = 'N')) then Exit;
  if StrToUnicode(Key) = '-0' then Exit(True);
  Result := StrToUnicode(StrOf(NumberToStr(PrimitiveToNumber(MkPtr(Key))))) = StrToUnicode(Key);
end;

function TAStoreValue(O: TValue; Idx: Int64; V: TValue): Boolean;
var D: Double; B: TValue; P: PObj;
begin
  PushRoot(O);
  if TAIsBig(ObjOf(O)) then
  begin
    if not ToBigIntV(V, B) then begin PopRoots(1); Exit(False); end;
    P := ObjOf(RootAt(TempTop - 1));
    if (Idx >= 0) and (Idx < TALength(P)) then TASetBig(P, Idx, BigToU64(B));
  end
  else
  begin
    if IsInt(V) then D := IntOf(V)
    else if not ToNumberV(V, D) then begin PopRoots(1); Exit(False); end;
    P := ObjOf(RootAt(TempTop - 1));
    if (Idx >= 0) and (Idx < TALength(P)) then TASet(P, Idx, D);
  end;
  PopRoots(1);
  Result := True;
end;

{ 10.1.9.2 OrdinarySetWithOwnDescriptor (receiver = the object): Ok False where [[Set]] returns false }
function SetOnObjectOk(Obj: TValue; Key: PStr; V: TValue; var Ok: Boolean): Boolean;
var O, W: PObj; S: Integer; Flags: Byte; P: TValue; Acc: PAccessor; Idx: Int64; Dummy: TValue; D1, D2: Double;
  U: PUpval;
begin
  Ok := True;
  O := ObjOf(Obj);
  if O^.OKind = okProxy then Exit(ProxySetHook(Obj, Key, V, Obj, Ok));
  if O^.OKind = okModuleNS then begin Ok := False; Exit(True); end;   { 10.4.6.9 [[Set]]: false }
  { 10.4.3: a String object's characters and length are non-writable own properties }
  if (O^.OKind = okStringObj) and ((Key = AtomLength) or (IndexFromString(Key, Idx) and (Idx < StrOf(O^.Aux1)^.Len))) then
  begin Ok := False; Exit(True); end;
  { 10.4.5.5 [[Set]] with the typed array as its own receiver: TypedArraySetElement, always true (false on an
    immutable buffer) }
  if O^.OKind = okTypedArray then
  begin
    if ViewImmutable(O) and (IndexFromString(Key, Idx) or CanonicalNumericNonIndex(Key)) then
    begin
      Ok := False;
      Exit(True);
    end;
    if IndexFromString(Key, Idx) then Exit(TAStoreValue(Obj, Idx, V));
    if CanonicalNumericNonIndex(Key) then Exit(TAStoreValue(Obj, -1, V));
  end;
  { 10.4.4.4 [[Set]] with the arguments object as its own receiver: a mapped index's parameter too }
  if (O^.OKind = okArgumentsOrd) and ((O^.Aux1 shr 48) = TAG_PTR_HI) then
  begin
    U := ArgsMapCell(O, Key, Idx);
    if U <> nil then ArgsMapSet(U, V);
  end;
  if (O^.OKind = okArguments) and ((Key = AtomLength) or (IndexFromString(Key, Idx) and (Idx >= O^.Elements^.Len))) then
  begin
    PushRoot(V);
    Obj := DemoteArguments(Obj);
    V := RootAt(TempTop - 1);
    PopRoots(1);
    O := ObjOf(Obj);
  end;
  S := ShapeLookup(O^.Shape, Key);
  if S >= 0 then
  begin
    Flags := O^.Shape^.PFlags[S];
    if (Flags and pfAccessor) <> 0 then
    begin
      Acc := PAccessor(PtrOf(GetSlot(O, S)));
      if Acc^.Setter = VUndefined then begin Ok := False; Exit(True); end;
      Exit(CallValue(Acc^.Setter, Obj, @V, 1, Dummy));
    end;
    if (Flags and pfWritable) = 0 then begin Ok := False; Exit(True); end;
    SetSlot(O, S, V);
    Exit(True);
  end;
  { a function's own 'prototype' not created yet (FunctionPrototypeOf): writable, so the value just becomes it }
  if (O^.OKind = okFunction) and (Key = AtomPrototype) and ((O^.OFlags and 4) = 0) and
     (PFuncTemplate(O^.Native)^.IsGenerator or
      not (PFuncTemplate(O^.Native)^.IsArrow or PFuncTemplate(O^.Native)^.IsMethod or PFuncTemplate(O^.Native)^.IsAsync)) then
  begin
    AddProperty(O, AtomPrototype, V, pfWritable);
    Exit(True);
  end;
  { an own element (a default data property) }
  if (O^.Elements <> nil) and IndexFromString(Key, Idx) and (Idx < O^.Elements^.Len) and
     (O^.Elements^.Data[Idx] <> VHole) and (O^.OKind <> okTypedArray) then
  begin
    ValueArrayStore(O^.Elements, Idx, V);
    Exit(True);
  end;
  if O^.OKind = okArray then
  begin
    if Key = AtomLength then
    begin
      if (O^.OFlags and 128) <> 0 then begin Ok := False; Exit(True); end;   { a read-only length }
      { 10.4.2.4 ArraySetLength: ToUint32 and ToNumber of the value must agree }
      PushRoot(Obj); PushRoot(V);
      if not ToNumberV(V, D1) or not ToNumberV(RootAt(TempTop - 1), D2) then begin PopRoots(2); Exit(False); end;
      O := ObjOf(RootAt(TempTop - 2));
      PopRoots(2);
      if IsNaN(D2) or (D2 < 0) or (D2 > 4294967295.0) or (Frac(D2) <> 0) then Exit(ThrowError('RangeError', 'Invalid array length'));
      { 10.2.4.? a read-only length the conversion made (a valueOf that froze the array): false }
      if (O^.OFlags and 128) <> 0 then begin Ok := False; Exit(True); end;
      if Trunc(D2) < ArrayLength(O) then
      begin
        { index properties in the shape: the deletion from the end, stopped by a non-configurable one }
        Idx := ArrayDropShapeIndices(O, Trunc(D2));
        if Idx > Trunc(D2) then
        begin
          ArraySetLengthAny(O, Idx);
          Ok := False;
          Exit(True);
        end;
      end;
      ArraySetLengthAny(O, Trunc(D2));
      Exit(True);
    end;
    { a new index past a read-only length fails (10.4.2.1) }
    if IndexFromString(Key, Idx) and ((O^.OFlags and 128) <> 0) and (Idx >= ArrayLength(O)) then
    begin
      Ok := False;
      Exit(True);
    end;
  end;
  { setters and read-only properties on the prototype chain }
  if O^.Shape^.Proto <> VNull then
  begin
    P := O^.Shape^.Proto;
    while IsObject(P) do
    begin
      W := ObjOf(P);
      { a proxy on the chain: its [[Set]] with this object as the receiver }
      if W^.OKind = okProxy then Exit(ProxySetHook(P, Key, V, Obj, Ok));
      { 10.4.5.5 a typed array on the chain answers a canonical numeric key: an invalid index sets nothing (true), a
        valid one is a writable data property, so the receiver gets its own }
      if W^.OKind = okTypedArray then
        if ViewImmutable(W) and (CanonicalNumericNonIndex(Key) or IndexFromString(Key, Idx)) then
        begin
          Ok := False;
          Exit(True);
        end
        else if CanonicalNumericNonIndex(Key) then Exit(True)
        else if IndexFromString(Key, Idx) then
        begin
          if TAOutOfBounds(W) or (Idx >= TALength(W)) then Exit(True);
          Break;
        end;
      S := ShapeLookup(W^.Shape, Key);
      if S >= 0 then
      begin
        Flags := W^.Shape^.PFlags[S];
        if (Flags and pfAccessor) <> 0 then
        begin
          Acc := PAccessor(PtrOf(GetSlot(W, S)));
          if Acc^.Setter = VUndefined then begin Ok := False; Exit(True); end;
          Exit(CallValue(Acc^.Setter, Obj, @V, 1, Dummy));
        end;
        { an inherited read-only data property: the assignment fails (10.1.9.2 step 2.a) }
        if (Flags and pfWritable) = 0 then begin Ok := False; Exit(True); end;
        Break;
      end;
      P := W^.Shape^.Proto;
    end;
  end;
  O := ObjOf(Obj);
  if (O^.OFlags and 8) <> 0 then begin Ok := False; Exit(True); end;   { not extensible }
  { a new index of an object with elements: an element where the array stays dense }
  if (O^.Elements <> nil) and IndexFromString(Key, Idx) and (O^.OKind in [okArray, okArguments]) and
     ((Idx < O^.Elements^.Len) or (((O^.OFlags and ofBigLen) = 0) and ArrayDenseOK(O^.Elements, Idx))) then
  begin
    PushRoot(Obj); PushRoot(V);
    if Idx >= O^.Elements^.Cap then EnsureElementsCap(O, Idx + 1);
    O := ObjOf(RootAt(TempTop - 2));
    V := RootAt(TempTop - 1);
    PopRoots(2);
    for S := O^.Elements^.Len to Idx - 1 do O^.Elements^.Data[S] := VHole;
    if Idx >= O^.Elements^.Len then O^.Elements^.Len := Idx + 1;
    ValueArrayStore(O^.Elements, Idx, V);
    Exit(True);
  end;
  AddProperty(O, Key, V, pfDefault);
  { 10.4.2.1 step 1.k: an index at or past an array's length extends it; one too far for its elements stays a
    property of the shape }
  if (O^.OKind = okArray) and (O^.Elements <> nil) and IndexFromString(Key, Idx) and (Idx >= ArrayLength(O)) then
    ArraySetLengthSparse(O, Idx + 1)
  else if (O^.OKind = okArray) and IndexFromString(Key, Idx) then
    O^.OFlags := O^.OFlags or 64;
  Result := True;
end;

function SetOnObject(Obj: TValue; Key: PStr; V: TValue): Boolean;
var Ok: Boolean;
begin
  Result := SetOnObjectOk(Obj, Key, V, Ok);
end;

{ cold: the message's strings out of SetOnObjectS's frame }
function ThrowReadOnly(Key: PStr): Boolean; noinline;
begin
  Result := ThrowTypeError('Cannot assign to read only property ''' + StrToUnicode(Key) + ''' of object');
end;

{ PutValue: in strict code a failed [[Set]] is a TypeError }
function SetOnObjectS(Obj: TValue; Key: PStr; V: TValue; Strict: Boolean): Boolean;
var Ok: Boolean;
begin
  if not SetOnObjectOk(Obj, Key, V, Ok) then Exit(False);
  if not Ok and Strict then Exit(ThrowReadOnly(Key));
  Result := True;
end;

function SetVS(Obj: TValue; Key: PStr; V: TValue; Strict: Boolean): Boolean;
var W: TValue; Ok: Boolean;
begin
  if IsObject(Obj) then Exit(SetOnObjectS(Obj, Key, V, Strict));
  if IsNullish(Obj) then
    Exit(ThrowTypeError('Cannot set properties of ' + TypeOfStr(Obj) + ' (setting ''' + StrToUnicode(Key) + ''')'));
  { 6.2.5.6 PutValue step 5.b: ToObject(base).[[Set]](name, value, base) (a setter or a proxy on the prototype
    chain sees the primitive as its receiver; nothing can be created on it) }
  PushRoot(V); PushRoot(Obj);
  if not ToObjectHook(Obj, W) then begin PopRoots(2); Exit(False); end;
  Ok := False;
  Result := SetReceiverHook(W, Key, RootAt(TempTop - 2), RootAt(TempTop - 1), Ok);
  Obj := RootAt(TempTop - 1);
  PopRoots(2);
  if Result and not Ok and Strict then
    Result := ThrowTypeError('Cannot create property ''' + StrToUnicode(Key) + ''' on ' + TypeOfStr(Obj));
end;

function SetVT(Obj: TValue; Key: PStr; V: TValue): Boolean;
begin
  Result := SetVS(Obj, Key, V, True);
end;

function SetV(Obj: TValue; Key: PStr; V: TValue): Boolean;
begin
  if IsObject(Obj) then Exit(SetOnObject(Obj, Key, V));
  if IsNullish(Obj) then
    Exit(ThrowTypeError('Cannot set properties of undefined (setting ''' + StrToUnicode(Key) + ''')'));
  Result := True;
end;

function ToPropKey(V: TValue; var Key: PStr): Boolean;
var S: TValue;
begin
  if IsString(V) then begin Key := Atomize(StrOf(V)); Exit(True); end;
  if IsPtr(V) and (PCell(PtrOf(V))^.Kind = ckSymbol) then
  begin
    Key := PStr(PPointer(PByte(PtrOf(V)) + 16)^);
    Exit(True);
  end;
  if IsInt(V) then begin Key := IntAtom(IntOf(V)); Exit(True); end;
  { 7.1.19 ToPropertyKey: ToPrimitive(hint string), and a symbol is the key }
  if IsObject(V) then
  begin
    if not ToPrimitiveV(V, True, S) then Exit(False);
    Exit(ToPropKey(S, Key));
  end;
  if not ToStringV(V, S) then Exit(False);
  Key := Atomize(StrOf(S));
  Result := True;
end;

{ cold: the TypeError for a property access on undefined or null, naming the key when it is a primitive }
function ThrowNullishBase(Obj, Key: TValue; Setting: Boolean): Boolean; noinline;
var N, T: UnicodeString; V: TValue;
begin
  N := '';
  if IsString(Key) then N := StrToUnicode(StrOf(Key))
  else if IsNumber(Key) and ToStringV(Key, V) then N := StrToUnicode(StrOf(V));
  if Obj = VNull then T := 'null' else T := 'undefined';
  if Setting then
    Result := ThrowTypeError('Cannot set properties of ' + T + ' (setting ''' + N + ''')')
  else
    Result := ThrowTypeError('Cannot read properties of ' + T + ' (reading ''' + N + ''')');
end;

function ElemMissUndefined(O: PObj; Idx: Integer): Boolean; forward;

function GetElemV(Obj, Key: TValue; var Res: TValue): Boolean;
var Idx: Int64; O: PObj; K: PStr; S: PStr; J: Integer;
begin
  if IsObject(Obj) and (ObjOf(Obj)^.OKind = okTypedArray) and IndexFromValue(Key, Idx) then
  begin
    O := ObjOf(Obj);
    if Idx < TALength(O) then Res := TAGet(O, Idx) else Res := VUndefined;
    Exit(True);
  end;
  if IsObject(Obj) and IndexFromValue(Key, Idx) and not IsString(Key) then
  begin
    O := ObjOf(Obj);
    if (O^.Elements <> nil) and (Idx < O^.Elements^.Len) then
    begin
      Res := O^.Elements^.Data[Idx];
      if Res <> VHole then Exit(True);
    end;
    { a missing index of a plain object or array with no index-keyed properties (its own or its prototypes'):
      undefined, without the key's string }
    if (Idx <= High(Integer)) and ((O^.OKind = okPlain) or (O^.OKind = okArray)) then
      if not O^.Shape^.HasIndexKeys then
      begin
        if ElemMissUndefined(O, Integer(Idx)) then
        begin
          Res := VUndefined;
          Exit(True);
        end;
      end
      else if O^.OKind = okPlain then
      begin
        { an own data property keyed by the index in the shape (an object literal's key 10): its slot (atoms
          live outside the heap: IntAtom cannot collect) }
        K := IntAtom(Idx);
        J := ShapeLookup(O^.Shape, K);
        if (J >= 0) and ((O^.Shape^.PFlags[J] and pfAccessor) = 0) then
        begin
          Res := GetSlot(O, J);
          Exit(True);
        end;
      end;
  end
  else if IsString(Obj) and IndexFromValue(Key, Idx) and not IsString(Key) then
  begin
    S := StrOf(Obj);
    if Idx < S^.Len then Res := SubString(S, Idx, 1) else Res := VUndefined;
    Exit(True);
  end;
  { 13.3.2.1 / 6.2.5.5 GetValue: ToObject(base) comes before ToPropertyKey(name) }
  if IsNullish(Obj) then Exit(ThrowNullishBase(Obj, Key, False));
  PushRoot(Obj);
  if not ToPropKey(Key, K) then begin PopRoots(1); Exit(False); end;
  Obj := RootAt(TempTop - 1);
  PopRoots(1);
  Result := GetV(Obj, K, Res);
end;

{ 10.1.9.2 OrdinarySetWithOwnDescriptor for an absent own element: nothing on the prototype chain (a proxy, an
  exotic object, an index-keyed shape property, an element there) can intercept or forbid the new property }
{ a [[Get]] of index Idx that O's own elements lack finds nothing on O's prototypes: none has index keys in its
  shape or a non-hole element there, and each is a plain kind (no proxy, string or typed array exotics) }
function ElemMissUndefined(O: PObj; Idx: Integer): Boolean;
var P: TValue; Q: PObj;
begin
  P := O^.Shape^.Proto;
  while IsObject(P) do
  begin
    Q := ObjOf(P);
    if Q^.Shape^.HasIndexKeys or not (Q^.OKind in [okPlain, okArray, okFunction, okNative]) then Exit(False);
    if (Q^.Elements <> nil) and (Idx < Integer(Q^.Elements^.Len)) and (Q^.Elements^.Data[Idx] <> VHole) then Exit(False);
    P := Q^.Shape^.Proto;
  end;
  Result := True;
end;

{ an append to a plain array meets no index accessor or frozen element on Array.prototype and Object.prototype
  (10.1.9.2 OrdinarySetWithOwnDescriptor: a missing own index consults the prototype chain) }
function AppendProtosPlain(O: PObj): Boolean; inline;
var A: PObj;
begin
  A := ObjOf(ArrayProto);
  Result := (O^.Shape^.Proto = ArrayProto) and not A^.Shape^.HasIndexKeys and ((A^.OFlags and 64) = 0) and
            (A^.Shape^.Proto = ObjectProto) and not ObjOf(ObjectProto)^.Shape^.HasIndexKeys and
            ((ObjOf(ObjectProto)^.OFlags and 64) = 0);
end;

function ProtosPlainForElem(O: PObj; Idx: Int64): Boolean;
var P: TValue; Q: PObj;
begin
  P := O^.Shape^.Proto;
  while IsObject(P) do
  begin
    Q := ObjOf(P);
    if Q^.Shape^.HasIndexKeys or not (Q^.OKind in [okPlain, okArray, okFunction, okNative]) then Exit(False);
    if (Q^.Elements <> nil) and (Idx < Q^.Elements^.Len) and (Q^.Elements^.Data[Idx] <> VHole) and
       ((Q^.OFlags and 64) <> 0) then Exit(False);   { ofElemGuard: a frozen or sealed element }
    P := Q^.Shape^.Proto;
  end;
  Result := True;
end;

{ an own default data element at Idx without [[Set]] (CreateDataProperty's store): False when the object's
  elements cannot hold it }
function DefineElemRaw(Obj: TValue; Idx: Int64; V: TValue): Boolean;
var O: PObj; E: PValueArray; I: Integer;
begin
  O := ObjOf(Obj);
  E := O^.Elements;
  if E = nil then Exit(False);
  if Idx < E^.Len then
  begin
    E^.Data[Idx] := V;
    WriteBarrier(@E^.Hdr, V);
    Exit(True);
  end;
  if (O^.OKind <> okArray) or not ArrayDenseOK(E, Idx) then Exit(False);
  if Idx >= E^.Cap then
  begin
    PushRoot(Obj); PushRoot(V);
    EnsureElementsCap(O, Idx + 1);
    Obj := RootAt(TempTop - 2); V := RootAt(TempTop - 1);
    PopRoots(2);
    O := ObjOf(Obj);
    E := O^.Elements;
  end;
  for I := E^.Len to Idx - 1 do E^.Data[I] := VHole;
  E^.Data[Idx] := V;
  WriteBarrier(@E^.Hdr, V);
  if Idx >= E^.Len then E^.Len := Idx + 1;
  Result := True;
end;

function SetElemVS(Obj, Key, V: TValue; Strict: Boolean): Boolean;
var Idx: Int64; O: PObj; E: PValueArray; K: PStr; I: Integer; D: Double;
begin
  if IsObject(Obj) and (ObjOf(Obj)^.OKind = okTypedArray) and IndexFromValue(Key, Idx) then
  begin
    { 10.4.5.5: an immutable buffer's element is not set ([[Set]] false) }
    if ViewImmutable(ObjOf(Obj)) then
    begin
      if Strict then Exit(ThrowTypeError('Cannot assign to an element of a typed array with an immutable buffer'));
      Exit(True);
    end;
    Exit(TAStoreValue(Obj, Idx, V));
  end;
  if IsObject(Obj) and IndexFromValue(Key, Idx) and ((ObjOf(Obj)^.OFlags and 64) = 0) then
  begin
    O := ObjOf(Obj);
    if O^.Elements <> nil then
    begin
      E := O^.Elements;
      if (Idx < E^.Len) and (E^.Data[Idx] <> VHole) then
      begin
        E^.Data[Idx] := V;
        WriteBarrier(@E^.Hdr, V);
        Exit(True);
      end;
      if (O^.OKind = okArray) and ArrayDenseOK(E, Idx) and ProtosPlainForElem(O, Idx) then
      begin
        if Idx >= E^.Cap then
        begin
          PushRoot(Obj); PushRoot(V);
          EnsureElementsCap(O, Idx + 1);
          Obj := RootAt(TempTop - 2); V := RootAt(TempTop - 1);
          PopRoots(2);
          O := ObjOf(Obj);
          E := O^.Elements;
        end;
        for I := E^.Len to Idx - 1 do E^.Data[I] := VHole;
        E^.Data[Idx] := V;
        WriteBarrier(@E^.Hdr, V);
        if Idx >= E^.Len then E^.Len := Idx + 1;
        Exit(True);
      end;
    end;
  end;
  { 6.2.5.6 PutValue: ToObject(base) before ToPropertyKey(name) }
  if IsNullish(Obj) then Exit(ThrowNullishBase(Obj, Key, True));
  PushRoot(Obj); PushRoot(V);
  if not ToPropKey(Key, K) then begin PopRoots(2); Exit(False); end;
  Obj := RootAt(TempTop - 2); V := RootAt(TempTop - 1);
  PopRoots(2);
  Result := SetVS(Obj, K, V, Strict);
end;

function SetElemV(Obj, Key, V: TValue): Boolean;
begin
  Result := SetElemVS(Obj, Key, V, False);
end;

function SetElemVT(Obj, Key, V: TValue): Boolean;
begin
  Result := SetElemVS(Obj, Key, V, True);
end;

{ ---------------- conversions that may call JS ---------------- }

{ 7.1.1.1 OrdinaryToPrimitive }
function OrdinaryToPrimitive(V: TValue; StringFirst: Boolean; var Res: TValue): Boolean;
var F, R: TValue; I: Integer; Key: PStr;
begin
  PushRoot(V);
  for I := 0 to 1 do
  begin
    if (I = 0) = StringFirst then Key := AtomToString else Key := AtomValueOf;
    if not GetV(RootAt(TempTop - 1), Key, F) then begin PopRoots(1); Exit(False); end;
    if IsCallable(F) then
    begin
      if not CallValue(F, RootAt(TempTop - 1), nil, 0, R) then begin PopRoots(1); Exit(False); end;
      if not IsObject(R) then begin Res := R; PopRoots(1); Exit(True); end;
    end;
  end;
  PopRoots(1);
  Result := ThrowTypeError('Cannot convert object to primitive value');
end;

{ 7.1.1 ToPrimitive }
function ToPrimitiveHint(V: TValue; Hint: TPrimHint; var Res: TValue): Boolean;
const HintNames: array[TPrimHint] of UnicodeString = ('default', 'number', 'string');
var F, R, H: TValue;
begin
  if not IsObject(V) then begin Res := V; Exit(True); end;
  if ToPrimitiveKey <> nil then
  begin
    PushRoot(V);
    if not GetV(V, ToPrimitiveKey, F) then begin PopRoots(1); Exit(False); end;
    if (F <> VUndefined) and (F <> VNull) then
    begin
      if not IsCallable(F) then begin PopRoots(1); Exit(ThrowTypeError('Symbol.toPrimitive is not a function')); end;
      H := MkPtr(AtomOf(HintNames[Hint]));
      if not CallValue(F, RootAt(TempTop - 1), @H, 1, R) then begin PopRoots(1); Exit(False); end;
      PopRoots(1);
      if IsObject(R) then Exit(ThrowTypeError('Cannot convert object to primitive value'));
      Res := R;
      Exit(True);
    end;
    V := RootAt(TempTop - 1);
    PopRoots(1);
  end;
  Result := OrdinaryToPrimitive(V, Hint = phString, Res);
end;

function ToPrimitiveV(V: TValue; HintString: Boolean; var Res: TValue): Boolean;
begin
  if HintString then Result := ToPrimitiveHint(V, phString, Res) else Result := ToPrimitiveHint(V, phNumber, Res);
end;

function ToStringV(V: TValue; var Res: TValue): Boolean;
var P: TValue;
begin
  if IsString(V) then begin Res := V; Exit(True); end;
  if IsInt(V) then begin Res := StrFromUnicode(IntToStr(IntOf(V))); Exit(True); end;
  if IsDouble(V) then begin Res := NumberToStr(DoubleOf(V)); Exit(True); end;
  case V of
    VUndefined, VHole: begin Res := MkPtr(AtomOf('undefined')); Exit(True); end;
    VNull: begin Res := MkPtr(AtomOf('null')); Exit(True); end;
    VTrue: begin Res := MkPtr(AtomOf('true')); Exit(True); end;
    VFalse: begin Res := MkPtr(AtomOf('false')); Exit(True); end;
  end;
  if IsPtr(V) and (PCell(PtrOf(V))^.Kind = ckSymbol) then
    Exit(ThrowTypeError('Cannot convert a Symbol value to a string'));
  if IsBigInt(V) then begin Res := StrFromUnicode(BigToString(V)); Exit(True); end;
  if not ToPrimitiveV(V, True, P) then Exit(False);
  Result := ToStringV(P, Res);
end;

function ToNumberV(V: TValue; var D: Double): Boolean;
var P: TValue;
begin
  if IsObject(V) then
  begin
    if not ToPrimitiveV(V, False, P) then Exit(False);
    V := P;
  end;
  if IsBigInt(V) then Exit(ThrowTypeError('Cannot convert a BigInt value to a number'));
  if IsPtr(V) and (PCell(PtrOf(V))^.Kind = ckSymbol) then Exit(ThrowTypeError('Cannot convert a Symbol value to a number'));
  D := PrimitiveToNumber(V);
  Result := True;
end;

function StrictEquals(A, B: TValue): Boolean;
begin
  if IsNumber(A) and IsNumber(B) then Exit(NumOf(A) = NumOf(B));
  if A = B then Exit(True);
  if IsString(A) and IsString(B) then Exit(StrEquals(StrOf(A), StrOf(B)));
  if IsBigInt(A) and IsBigInt(B) then Exit(BigEquals(A, B));
  Result := False;
end;

{ === on the dispatch path: the same bits are equal unless they are NaN, two int32s are equal only as the same
  bits; anything else (doubles, -0 against 0, strings, BigInts) goes to StrictEquals }
function SEqFast(A, B: TValue): Boolean; inline;
begin
  if A = B then Exit(not IsDouble(A) or (DoubleOf(A) = DoubleOf(A)));
  if IsInt(A) and IsInt(B) then Exit(False);
  Result := StrictEquals(A, B);
end;

{ a BigInt against a Number by their mathematical values (7.2.13 step 13, 7.2.12 step 4.i): -1, 0, 1; 2 when D is
  NaN. Cold: the BigInteger temporaries stay out of the callers' frames }
function BigCmpNum(Big: TValue; D: Double): Integer; noinline;
var T: Double; C: Integer;
begin
  if IsNaN(D) then Exit(2);
  if IsInfinite(D) then
  begin
    if D > 0 then Exit(-1);
    Exit(1);
  end;
  T := Int(D);
  C := ValueToBig(Big).Compare(TBigInteger.FromDouble(T));
  if C < 0 then Exit(-1);
  if C > 0 then Exit(1);
  if D > T then Exit(-1);
  if D < T then Exit(1);
  Result := 0;
end;

function BigEqualsNumber(Big: TValue; D: Double): Boolean;
begin
  Result := BigCmpNum(Big, D) = 0;
end;

{ 7.2.13 steps 6-7: a BigInt against a String is StringToBigInt of the string, unequal when that fails }
function BigEqualsString(Big, S: TValue; var Res: Boolean): Boolean; noinline;
var B: TValue;
begin
  PushRoot(Big);
  Result := True;
  if not StringToBigIntV(StrToUnicode(StrOf(S)), B) then Res := False
  else Res := BigEquals(RootAt(TempTop - 1), B);
  PopRoots(1);
end;

function LooseEquals(A, B: TValue; var Res: Boolean): Boolean;
var P: TValue;
begin
  Result := True;
  if IsNullish(A) or IsNullish(B) then
  begin
    { B.3.6.2: an [[IsHTMLDDA]] object is loosely equal to undefined and null }
    Res := (IsNullish(A) or (IsObject(A) and ((PCell(PtrOf(A))^.Flags and cfEmulatesUndefined) <> 0))) and
           (IsNullish(B) or (IsObject(B) and ((PCell(PtrOf(B))^.Flags and cfEmulatesUndefined) <> 0)));
    Exit;
  end;
  if IsNumber(A) and IsNumber(B) then begin Res := NumOf(A) = NumOf(B); Exit; end;
  if IsString(A) and IsString(B) then begin Res := StrEquals(StrOf(A), StrOf(B)); Exit; end;
  if IsBigInt(A) and IsBigInt(B) then begin Res := BigEquals(A, B); Exit; end;
  if IsBigInt(A) and IsNumber(B) then begin Res := BigEqualsNumber(A, NumOf(B)); Exit; end;
  if IsNumber(A) and IsBigInt(B) then begin Res := BigEqualsNumber(B, NumOf(A)); Exit; end;
  if IsBigInt(A) and IsString(B) then Exit(BigEqualsString(A, B, Res));
  if IsString(A) and IsBigInt(B) then Exit(BigEqualsString(B, A, Res));
  if (A = VTrue) or (A = VFalse) then Exit(LooseEquals(MkInt(Ord(A = VTrue)), B, Res));
  if (B = VTrue) or (B = VFalse) then Exit(LooseEquals(A, MkInt(Ord(B = VTrue)), Res));
  if IsNumber(A) and IsString(B) then begin Res := NumOf(A) = PrimitiveToNumber(B); Exit; end;
  if IsString(A) and IsNumber(B) then begin Res := PrimitiveToNumber(A) = NumOf(B); Exit; end;
  if IsObject(A) and IsObject(B) then begin Res := A = B; Exit; end;
  if IsObject(A) then
  begin
    if not ToPrimitiveHint(A, phDefault, P) then Exit(False);
    Exit(LooseEquals(P, B, Res));
  end;
  if IsObject(B) then
  begin
    if not ToPrimitiveHint(B, phDefault, P) then Exit(False);
    Exit(LooseEquals(A, P, Res));
  end;
  Res := A = B;
end;

{ new's and instanceof's cache of F's prototype property: F's shape and the slot of an own data property }
procedure CacheProtoSlot(F: PObj);
var S: Integer;
begin
  if F^.Shape^.IsDict then Exit;
  S := ShapeLookup(F^.Shape, AtomPrototype);
  if (S < 0) or ((F^.Shape^.PFlags[S] and pfAccessor) <> 0) then Exit;
  PFuncTemplate(F^.Native)^.CtorFnShape := F^.Shape;
  PFuncTemplate(F^.Native)^.CtorProtoSlot := S;
end;

threadvar
  { InstanceOf's fast path: Function.prototype's shape (and dictionary version) whose @@hasInstance was found
    to be the intrinsic one, and the last function shape without an own @@hasInstance }
  HIFPShape, HIPlainShape: PShape;
  HIFPVer: Cardinal;

{ C, an ordinary function, gets Function.prototype's intrinsic @@hasInstance: OrdinaryHasInstance decides }
function HasInstanceIntrinsic(O: PObj): Boolean;
var S: Integer; FP: PObj; K: PStr;
begin
  Result := False;
  if (O^.Shape^.Proto <> FunctionProto) or not IsPtr(SymHasInstance) then Exit;
  FP := ObjOf(FunctionProto);
  if (FP^.Shape <> HIFPShape) or (FP^.Shape^.IsDict and (FP^.Shape^.Version <> HIFPVer)) then
  begin
    K := SymKey(SymHasInstance);
    S := ShapeLookup(FP^.Shape, K);
    if (S < 0) or ((FP^.Shape^.PFlags[S] and pfAccessor) <> 0) or (GetSlot(FP, S) <> FuncHasInstanceFn) then Exit;
    HIFPShape := FP^.Shape;
    HIFPVer := FP^.Shape^.Version;
  end;
  if O^.Shape <> HIPlainShape then
  begin
    if O^.Shape^.IsDict or (ShapeLookup(O^.Shape, SymKey(SymHasInstance)) >= 0) then Exit;
    HIPlainShape := O^.Shape;
  end;
  if O^.OKind = okFunction then PFuncTemplate(O^.Native)^.NoHIShape := O^.Shape;
  Result := True;
end;

function InstanceOf(V, C: TValue; var Res: Boolean): Boolean;
var P, Proto, H, R: TValue;
begin
  if IsObject(C) and (ObjOf(C)^.OKind = okFunction) and HasInstanceIntrinsic(ObjOf(C)) then
    Exit(OrdinaryHasInstance(C, V, Res));
  { InstanceofOperator: a Symbol.hasInstance method on the right-hand side decides }
  if IsObject(C) and IsPtr(SymHasInstance) then
  begin
    PushRoot(V); PushRoot(C);
    if not GetV(C, PStr(PPointer(PByte(PtrOf(SymHasInstance)) + 16)^), H) then begin PopRoots(2); Exit(False); end;
    C := RootAt(TempTop - 1); V := RootAt(TempTop - 2);
    if not IsNullish(H) and (H <> FuncHasInstanceFn) then
    begin
      P := V;
      Result := CallValue(H, C, @P, 1, R);
      PopRoots(2);
      if Result then Res := ToBool(R);
      Exit;
    end;
    PopRoots(2);
  end;
  if not IsCallable(C) then Exit(ThrowTypeError('Right-hand side of ''instanceof'' is not callable'));
  Result := OrdinaryHasInstance(C, V, Res);
end;

function OrdinaryHasInstance(C, V: TValue; var Res: Boolean): Boolean;
var Proto: TValue;
begin
  Res := False;
  if not IsCallable(C) then Exit(True);
  if ObjOf(C)^.OKind = okBound then Exit(InstanceOf(V, ObjOf(C)^.Aux1, Res));
  if not IsObject(V) then Exit(True);
  { a bytecode function's prototype property through new's cache (F's shape, the slot) }
  if (ObjOf(C)^.OKind = okFunction) and (Pointer(ObjOf(C)^.Shape) = PFuncTemplate(ObjOf(C)^.Native)^.CtorFnShape) then
    Proto := GetSlot(ObjOf(C), PFuncTemplate(ObjOf(C)^.Native)^.CtorProtoSlot)
  else
  begin
    PushRoot(V); PushRoot(C);
    if not GetV(C, AtomPrototype, Proto) then begin PopRoots(2); Exit(False); end;
    C := RootAt(TempTop - 1); V := RootAt(TempTop - 2);
    PopRoots(2);
    if ObjOf(C)^.OKind = okFunction then CacheProtoSlot(ObjOf(C));
  end;
  if not IsObject(Proto) then Exit(ThrowTypeError('Function has non-object prototype in instanceof check'));
  Result := ProtoChainHas(V, Proto, Res);
end;

{ 20.1.3.3 / 7.3.21 steps 4-6: whether Proto is on V's prototype chain, through [[GetPrototypeOf]] (a proxy's trap) }
function ProtoChainHas(V, Proto: TValue; var Res: Boolean): Boolean;
var P: TValue;
begin
  Res := False;
  P := V;
  while True do
  begin
    if ObjOf(P)^.OKind = okProxy then
    begin
      PushRoot(Proto);
      if not GetProtoHook(P, P) then begin PopRoots(1); Exit(False); end;
      Proto := RootAt(TempTop - 1);
      PopRoots(1);
    end
    else P := ObjOf(P)^.Shape^.Proto;
    if not IsObject(P) then Exit(True);
    if P = Proto then begin Res := True; Exit(True); end;
  end;
end;

function OwnKeys(O: PObj; OnlyEnumerable: Boolean): TValue;
var A: PObj; I, N, NI: Integer; S: PShape; Key: PStr; OV: TValue; Idxs: array of Int64; Idx: Int64;
  HasShapeIdx: Boolean;
  procedure AddIdx(X: Int64);
  begin
    if NI >= Length(Idxs) then SetLength(Idxs, NI * 2 + 16);
    Idxs[NI] := X;
    Inc(NI);
  end;
  procedure SortIdx;
  var J, K: Integer; T: Int64;
  begin
    for J := 1 to NI - 1 do
    begin
      T := Idxs[J];
      K := J - 1;
      while (K >= 0) and (Idxs[K] > T) do begin Idxs[K + 1] := Idxs[K]; Dec(K); end;
      Idxs[K + 1] := T;
    end;
  end;
begin
  { a function's prototype property is made on first use: it exists for key listing too }
  if (O^.OKind = okFunction) and not OnlyEnumerable and (ShapeLookup(O^.Shape, AtomPrototype) < 0) then
  begin
    PushRoot(MkPtr(O));
    FunctionPrototypeOf(MkPtr(O));
    O := ObjOf(RootAt(TempTop - 1));
    PopRoots(1);
  end;
  { 10.4.6.11: a module namespace's string keys are its exports, already sorted }
  if O^.OKind = okModuleNS then
  begin
    PushRoot(MkPtr(O));
    { a deferred namespace's keys evaluate its module first }
    if ((O^.OFlags and 16) <> 0) and not NSDeferHook(RootAt(TempTop - 1)) then
    begin
      OwnKeysFailed := True;
      PopRoots(1);
      Exit(MkPtr(NewArray(0, 0)));
    end;
    A := NewArray(0, 8);
    PushRoot(MkPtr(A));
    for I := 0 to ObjOf(ObjOf(RootAt(TempTop - 2))^.Aux1)^.Elements^.Len div 2 - 1 do
      ArrayPush(ObjOf(RootAt(TempTop - 1)), ObjOf(ObjOf(RootAt(TempTop - 2))^.Aux1)^.Elements^.Data[I * 2]);
    Result := RootAt(TempTop - 1);
    PopRoots(2);
    Exit;
  end;
  if O^.OKind = okProxy then
  begin
    { the keys through the traps; a throwing trap yields no keys and sets OwnKeysFailed for the caller }
    if not ProxyCheckHook(5 + Ord(OnlyEnumerable), MkPtr(O), nil, OV) then
    begin
      OwnKeysFailed := True;
      OV := MkPtr(NewArray(0, 0));
    end;
    Exit(OV);
  end;
  OV := MkPtr(O);
  PushRoot(OV);
  A := NewArray(0, 8);
  PushRoot(MkPtr(A));
  { 10.1.11.1 OrdinaryOwnPropertyKeys: the array indices ascending (elements, a String object's characters, a typed
    array's elements and index keys kept in the shape), then the other strings in creation order }
  O := ObjOf(RootAt(TempTop - 2));
  SetLength(Idxs, 0);
  NI := 0;
  if O^.OKind = okStringObj then
    for I := 0 to StrOf(O^.Aux1)^.Len - 1 do AddIdx(I)
  else if O^.OKind = okTypedArray then
    for I := 0 to TALength(O) - 1 do AddIdx(I)
  else if O^.Elements <> nil then
    for I := 0 to O^.Elements^.Len - 1 do
      if O^.Elements^.Data[I] <> VHole then AddIdx(I);
  S := O^.Shape;
  HasShapeIdx := False;
  for I := 0 to S^.Count - 1 do
    if (S^.Keys[I]^.Hdr.Extra = 0) and IndexFromString(S^.Keys[I], Idx) and
       (not OnlyEnumerable or ((S^.PFlags[I] and pfEnumerable) <> 0)) then
    begin
      AddIdx(Idx);
      HasShapeIdx := True;
    end;
  if HasShapeIdx then SortIdx;
  for I := 0 to NI - 1 do
  begin
    Key := IntAtom(Idxs[I]);
    ArrayPush(ObjOf(RootAt(TempTop - 1)), MkPtr(Key));
  end;
  O := ObjOf(RootAt(TempTop - 2));
  if not OnlyEnumerable and (O^.OKind in [okArray, okStringObj, okArguments]) then
    ArrayPush(ObjOf(RootAt(TempTop - 1)), MkPtr(AtomLength));
  O := ObjOf(RootAt(TempTop - 2));
  S := O^.Shape;
  for I := 0 to S^.Count - 1 do
  begin
    Key := S^.Keys[I];
    if Key^.Hdr.Extra <> 0 then Continue;   { symbol, private and internal keys }
    if IndexFromString(Key, Idx) then Continue;
    if OnlyEnumerable and ((S^.PFlags[I] and pfEnumerable) = 0) then Continue;
    ArrayPush(ObjOf(RootAt(TempTop - 1)), MkPtr(Key));
    S := ObjOf(RootAt(TempTop - 2))^.Shape;
  end;
  Result := RootAt(TempTop - 1);
  PopRoots(2);
end;

{ ---------------- iteration ---------------- }


function LengthOfArrayLike(O: TValue; var Len: Int64): Boolean;
var V: TValue; D: Double;
begin
  if IsObject(O) and (ObjOf(O)^.OKind in [okArray, okArguments]) and (ObjOf(O)^.Elements <> nil) then
  begin
    if ObjOf(O)^.OKind = okArray then Len := ArrayLength(ObjOf(O)) else Len := ObjOf(O)^.Elements^.Len;
    Exit(True);
  end;
  if not GetV(O, AtomLength, V) then Exit(False);
  if IsInt(V) then D := IntOf(V)
  else if not ToNumberV(V, D) then Exit(False);
  { 7.1.20 ToLength }
  if IsNaN(D) or (D <= 0) then Len := 0
  else if D >= 9007199254740991.0 then Len := 9007199254740991
  else Len := Trunc(D);
  Result := True;
end;

function NewArrayIterator(V: TValue; Kind: Integer): TValue;
var O: PObj;
begin
  PushRoot(V);
  if (Kind = 1) and IsObject(StringIterProto) then O := NewObject(RootShapeFor(StringIterProto), 0, okIterator)
  else O := NewObject(RootShapeFor(ArrayIteratorProto), 0, okIterator);
  O^.Aux1 := RootAt(TempTop - 1);
  O^.Aux2 := MkInt(0);
  O^.OFlags := Kind;   { 0 array values, 1 string code points, 2 keys, 3 entries }
  PopRoots(1);
  Result := MkPtr(O);
end;

function GetIterator(V: TValue; var It: TValue): Boolean;
var F: TValue;
begin
  { an array whose @@iterator is the intrinsic one (no own property shadows it) }
  if IsObject(V) and (ObjOf(V)^.OKind = okArray) and (ObjOf(V)^.Shape^.Proto = ArrayProto) and
     (ShapeLookup(ObjOf(V)^.Shape, SymKey(SymIterator)) < 0) and ArrayIterationIntact then
  begin
    It := NewArrayIterator(V, 0);
    Exit(True);
  end;
  if IsString(V) then
  begin
    It := NewArrayIterator(V, 1);
    Exit(True);
  end;
  if IsObject(V) and (ObjOf(V)^.OKind = okTypedArray) and (ObjOf(V)^.Shape^.Proto <> VNull) then
  begin
    It := NewArrayIterator(V, 4);
    Exit(True);
  end;
  PushRoot(V);
  if not GetV(V, PStr(PPointer(PByte(PtrOf(SymIterator)) + 16)^), F) then begin PopRoots(1); Exit(False); end;
  V := RootAt(TempTop - 1);
  PopRoots(1);
  if not IsCallable(F) then
  begin
    if GetEnvironmentVariable('G2_ITERDBG') <> '' then
    begin
      Write(ErrOutput, 'not iterable: type ', TypeOfStr(V));
      if IsPtr(V) then Write(ErrOutput, ' kind ', PCell(PtrOf(V))^.Kind, ' flags ', PCell(PtrOf(V))^.Flags);
      if IsObject(V) then Write(ErrOutput, ' okind ', ObjOf(V)^.OKind, ' proto=arrayproto ', ObjOf(V)^.Shape^.Proto = ArrayProto, ' F ', IntToHex(F, 16));
      WriteLn(ErrOutput);
    end;
    Exit(ThrowTypeError('value is not iterable'));
  end;
  if (F = ArrayValuesFnPublic) and IsObject(V) and (ObjOf(V)^.OKind in [okArray, okArguments]) then
  begin
    It := NewArrayIterator(V, 0);
    Exit(True);
  end;
  Result := CallValue(F, V, nil, 0, It);
  if Result and not IsObject(It) then Result := ThrowTypeError('iterator is not an object');
end;

function IteratorStep(It: TValue; var Value: TValue; var Done: Boolean): Boolean;
var O: PObj; I: Integer; Src: TValue; S: PStr; NextF, R: TValue; C: Integer; Len: Int64; E: PValueArray;
begin
  Value := VUndefined;   { a done step's value }
  O := ObjOf(It);
  if O^.OKind = okIterator then
  begin
    I := IntOf(O^.Aux2);
    Src := O^.Aux1;
    if O^.OFlags = 1 then
    begin
      S := StrOf(Src);
      if I >= S^.Len then begin Done := True; Exit(True); end;
      C := 1;
      if (Ord(StrChars(S)[I]) >= $D800) and (Ord(StrChars(S)[I]) <= $DBFF) and (I + 1 < S^.Len) and
         (Ord(StrChars(S)[I + 1]) >= $DC00) and (Ord(StrChars(S)[I + 1]) <= $DFFF) then C := 2;
      O^.Aux2 := MkInt(I + C);
      Value := SubString(S, I, C);
      Done := False;
      Exit(True);
    end;
    if O^.OFlags >= 5 then
    begin
      { a Map or Set iterator (G2.Builtins): the next live entry of the map's entry list (Aux1 [k0, v0, k1, ...]),
        undefined once done }
      if Src = VUndefined then begin Done := True; Exit(True); end;
      E := PValueArray(PtrOf(ObjOf(Src)^.Aux1));
      while (I < Integer(E^.Len)) and (E^.Data[I] = VHole) do Inc(I, 2);
      if I >= Integer(E^.Len) then begin O^.Aux1 := VUndefined; Done := True; Exit(True); end;
      O^.Aux2 := MkInt(I + 2);
      Done := False;
      case O^.OFlags of
        5, 8: Value := E^.Data[I];
        6: Value := E^.Data[I + 1];
      else
        begin
          PushRoot(It);
          O := NewArray(2, 2);
          E := PValueArray(PtrOf(ObjOf(ObjOf(RootAt(TempTop - 1))^.Aux1)^.Aux1));
          O^.Elements^.Data[0] := E^.Data[I];
          if ObjOf(RootAt(TempTop - 1))^.OFlags = 7 then O^.Elements^.Data[1] := E^.Data[I + 1]
          else O^.Elements^.Data[1] := E^.Data[I];
          PopRoots(1);
          Value := MkPtr(O);
        end;
      end;
      Exit(True);
    end;
    if O^.OFlags = 4 then
    begin
      { 23.1.5.1 step 2.b.ii: a typed array out of bounds (detached, or its resizable buffer shrunk) throws }
      if (Src <> VUndefined) and TAOutOfBounds(ObjOf(Src)) then
        Exit(ThrowTypeError('the typed array is detached or out of bounds'));
      if (Src = VUndefined) or (I >= TALength(ObjOf(Src))) then begin O^.Aux1 := VUndefined; Done := True; Exit(True); end;
      O^.Aux2 := MkInt(I + 1);
      Value := TAGet(ObjOf(Src), I);
      Done := False;
      Exit(True);
    end;
    if Src = VUndefined then begin Done := True; Exit(True); end;
    { an array or arguments object whose indices are all elements: read them directly }
    if (ObjOf(Src)^.Elements <> nil) and ((ObjOf(Src)^.OFlags and 64) = 0) and
       (ObjOf(Src)^.OKind in [okArray, okArguments]) then
    begin
      if I >= ObjOf(Src)^.Elements^.Len then
      begin
        O^.Aux1 := VUndefined;
        Done := True;
        Exit(True);
      end;
      if O^.OFlags = 2 then
      begin
        O^.Aux2 := MkInt(I + 1);
        Value := MkInt(I);
        Done := False;
        Exit(True);
      end;
      Value := ObjOf(Src)^.Elements^.Data[I];
      if (Value <> VHole) and (O^.OFlags = 0) then
      begin
        O^.Aux2 := MkInt(I + 1);
        Done := False;
        Exit(True);
      end;
    end;
    { 23.1.5.1 CreateArrayIterator's closure: LengthOfArrayLike, then Get of the index (a hole reads through the
      prototype chain; accessors run) }
    if ObjOf(Src)^.OKind = okTypedArray then
    begin
      if TAOutOfBounds(ObjOf(Src)) then Exit(ThrowTypeError('the typed array is detached or out of bounds'));
      Len := TALength(ObjOf(Src));
      PushRoot(It);
    end
    else
    begin
      PushRoot(It);
      if not LengthOfArrayLike(Src, Len) then begin PopRoots(1); Exit(False); end;
    end;
    O := ObjOf(RootAt(TempTop - 1));
    I := IntOf(O^.Aux2);
    if (O^.Aux1 = VUndefined) or (I >= Len) then
    begin
      O^.Aux1 := VUndefined;
      PopRoots(1);
      Value := VUndefined;
      Done := True;
      Exit(True);
    end;
    O^.Aux2 := MkInt(I + 1);
    if O^.OFlags = 2 then Value := MkInt(I)
    else if not GetElemV(O^.Aux1, MkInt(I), Value) then begin PopRoots(1); Exit(False); end;
    if O^.OFlags = 3 then
    begin
      { entries: CreateArrayFromList(« index, value ») }
      PushRoot(Value);
      O := NewArray(2, 2);
      O^.Elements^.Data[0] := MkInt(I);
      O^.Elements^.Data[1] := RootAt(TempTop - 1);
      WriteBarrier(@O^.Elements^.Hdr, RootAt(TempTop - 1));
      PopRoots(1);
      Value := MkPtr(O);
    end;
    PopRoots(1);
    Done := False;
    Exit(True);
  end;
  if (O^.OKind = okGenerator) and GeneratorNextIsIntrinsic(It) then
  begin
    { a yield* hands out the inner iterator's result object as is (GenYieldRaw): its done and value }
    GenYieldRaw := False;
    if not GeneratorResume(It, VUndefined, 0, Value, Done) then Exit(False);
    if not GenYieldRaw then Exit(True);
    GenYieldRaw := False;
    PushRoot(Value);
    if not GetV(RootAt(TempTop - 1), AtomDone, Value) then begin PopRoots(1); Exit(False); end;
    Done := ToBool(Value);
    Value := VUndefined;
    if not Done then
      if not GetV(RootAt(TempTop - 1), AtomValue, Value) then begin PopRoots(1); Exit(False); end;
    PopRoots(1);
    Exit(True);
  end;
  PushRoot(It);
  if not GetV(It, AtomNext, NextF) then begin PopRoots(1); Exit(False); end;
  if not CallValue(NextF, RootAt(TempTop - 1), nil, 0, R) then begin PopRoots(1); Exit(False); end;
  PopRoots(1);
  if not IsObject(R) then Exit(ThrowTypeError('iterator result is not an object'));
  PushRoot(R);
  if not GetV(R, AtomDone, Value) then begin PopRoots(1); Exit(False); end;
  Done := ToBool(Value);
  if not Done then
    if not GetV(RootAt(TempTop - 1), AtomValue, Value) then begin PopRoots(1); Exit(False); end;
  PopRoots(1);
  Result := True;
end;

{ ---------------- calls from natives ---------------- }

function Execute(StopFrame: Integer; var Res: TValue): Boolean; forward;
function RunFieldInit(Ctor, This: TValue): Boolean; forward;

function PushBytecodeFrame(FObj: PObj; This: TValue; Args: PValue; Argc: Integer; RetIdx: Integer; Kind: Byte;
  NewTarget: TValue): Boolean; forward;

{ ---------------- Proxy objects (ECMA-262 Proxy Object Internal Methods, without the invariant checks) ---------------- }

threadvar
  SymbolKeys: array of record Key: PStr; Sym: TValue; end;
  SymbolKeyCount: Integer;

procedure RegisterSymbolKey(Key: PStr; Sym: TValue);
begin
  if SymbolKeyCount >= Length(SymbolKeys) then SetLength(SymbolKeys, SymbolKeyCount * 2 + 64);
  SymbolKeys[SymbolKeyCount].Key := Key;
  SymbolKeys[SymbolKeyCount].Sym := Sym;   { symbols are permanent cells }
  Inc(SymbolKeyCount);
end;

{ the property key as a JS value: the Symbol for a symbol key, the string otherwise }
function KeyToValue(Key: PStr): TValue;
var I: Integer;
begin
  if (Key^.Hdr.Extra and kfSymbol) <> 0 then Exit(KeyPayload(Key)^);
  Result := MkPtr(Key);
end;

{ the handler's trap (undefined when absent); PV stays rooted by the caller }
function ProxyGetTrap(PV: TValue; const Name: UnicodeString; var Trap: TValue): Boolean;
var H: TValue;
begin
  H := ObjOf(PV)^.Aux2;
  if not IsObject(H) then Exit(ThrowTypeError('Cannot perform ''' + Name + ''' on a proxy that has been revoked'));
  Result := GetV(H, AtomOf(Name), Trap);
end;

{ call Trap(target, A1, A2, A3) with the handler as this; Argc counts the extra arguments }
function ProxyCallTrap(PV, Trap: TValue; Argc: Integer; A1, A2, A3: TValue; var Res: TValue): Boolean;
var A: array[0..3] of TValue;
begin
  A[0] := ObjOf(PV)^.Aux1; A[1] := A1; A[2] := A2; A[3] := A3;
  Result := CallValue(Trap, ObjOf(PV)^.Aux2, @A[0], Argc + 1, Res);
end;

function ProxyGet(PV: TValue; Key: PStr; Receiver: TValue; var Res: TValue): Boolean;
var Trap: TValue;
begin
  PushRoot(PV); PushRoot(Receiver);
  if not ProxyGetTrap(PV, 'get', Trap) then begin PopRoots(2); Exit(False); end;
  PV := RootAt(TempTop - 2); Receiver := RootAt(TempTop - 1);
  if IsNullish(Trap) then Result := GetFromObject(ObjOf(ObjOf(PV)^.Aux1), Key, Receiver, Res)
  else
  begin
    Result := ProxyCallTrap(PV, Trap, 2, KeyToValue(Key), Receiver, VUndefined, Res);
    if Result then Result := ProxyCheckHook(0, RootAt(TempTop - 2), Key, Res);
  end;
  PopRoots(2);
end;

function ProxyHas(PV: TValue; Key: PStr; var Found: Boolean): Boolean;
var Trap, R: TValue;
begin
  PushRoot(PV);
  if not ProxyGetTrap(PV, 'has', Trap) then begin PopRoots(1); Exit(False); end;
  PV := RootAt(TempTop - 1);
  if IsNullish(Trap) then Result := HasPropertyV(ObjOf(PV)^.Aux1, Key, Found)
  else
  begin
    Result := ProxyCallTrap(PV, Trap, 1, KeyToValue(Key), VUndefined, VUndefined, R);
    if Result then Found := ToBool(R);
    if Result and not Found then Result := ProxyCheckHook(1, RootAt(TempTop - 1), Key, R);
  end;
  PopRoots(1);
end;

function HasPropertyV(Obj: TValue; Key: PStr; var Found: Boolean): Boolean;
var O: PObj; V: TValue; Idx: Int64;
begin
  Found := False;
  Result := True;
  if not IsObject(Obj) then Exit;
  O := ObjOf(Obj);
  while True do
  begin
    if O^.OKind = okProxy then Exit(ProxyHas(MkPtr(O), Key, Found));
    if (O^.OKind = okModuleNS) and ((Key^.Hdr.Extra and kfSymbol) = 0) then Exit(NSLookup(O, Key, False, V, Found));
    { 10.4.5.2: a typed array has an integer index exactly when it is in range, and never looks further }
    if (O^.OKind = okTypedArray) and IndexFromString(Key, Idx) then begin Found := Idx < TALength(O); Exit; end;
    if (O^.OKind = okTypedArray) and CanonicalNumericNonIndex(Key) then Exit;
    if (O^.Elements <> nil) and IndexFromString(Key, Idx) and (Idx < O^.Elements^.Len) and (O^.Elements^.Data[Idx] <> VHole) then
    begin Found := True; Exit; end;
    if (ShapeLookup(O^.Shape, Key) >= 0) or VirtualProp(O, Key, V) then begin Found := True; Exit; end;
    if not IsObject(O^.Shape^.Proto) then Exit;
    O := ObjOf(O^.Shape^.Proto);
  end;
end;

function ProxyDelete(PV: TValue; Key: PStr; var Ok: Boolean): Boolean;
var Trap, R: TValue;
begin
  PushRoot(PV);
  if not ProxyGetTrap(PV, 'deleteProperty', Trap) then begin PopRoots(1); Exit(False); end;
  PV := RootAt(TempTop - 1);
  Ok := True;
  if IsNullish(Trap) then
  begin
    if IsObject(ObjOf(PV)^.Aux1) and (ObjOf(ObjOf(PV)^.Aux1)^.OKind = okProxy) then Result := ProxyDelete(ObjOf(PV)^.Aux1, Key, Ok)
    else begin Ok := DeleteIfConfigurable(ObjOf(ObjOf(PV)^.Aux1), Key); Result := True; end;
  end
  else
  begin
    Result := ProxyCallTrap(PV, Trap, 1, KeyToValue(Key), VUndefined, VUndefined, R);
    if Result then Ok := ToBool(R);
    if Result and Ok then Result := ProxyCheckHook(2, RootAt(TempTop - 1), Key, R);
  end;
  PopRoots(1);
end;

function ProxyOwnKeys(PV: TValue; var Res: TValue): Boolean;
var Trap: TValue;
begin
  PushRoot(PV);
  if not ProxyGetTrap(PV, 'ownKeys', Trap) then begin PopRoots(1); Exit(False); end;
  PV := RootAt(TempTop - 1);
  if IsNullish(Trap) then Result := ProxyCheckHook(4, PV, nil, Res)
  else
  begin
    Result := ProxyCallTrap(PV, Trap, 0, VUndefined, VUndefined, VUndefined, Res);
    if Result then Result := ProxyCheckHook(3, RootAt(TempTop - 1), nil, Res);
  end;
  PopRoots(1);
end;

{ an invariant-free trap call for the Object statics: HadTrap False means fall back to the target }
function ProxyInvoke(PV: TValue; const TrapName: UnicodeString; A1: TValue; Argc: Integer; var Res: TValue;
  var HadTrap: Boolean): Boolean;
var Trap: TValue;
begin
  PushRoot(PV); PushRoot(A1);
  HadTrap := False;
  if not ProxyGetTrap(PV, TrapName, Trap) then begin PopRoots(2); Exit(False); end;
  PV := RootAt(TempTop - 2); A1 := RootAt(TempTop - 1);
  Result := True;
  if not IsNullish(Trap) then
  begin
    HadTrap := True;
    Result := ProxyCallTrap(PV, Trap, Argc, A1, VUndefined, VUndefined, Res);
  end;
  PopRoots(2);
end;

function ProxyCall(PV, This: TValue; Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var Trap: TValue; A: PObj; Base, SavedTop, I: Integer;
begin
  { the arguments go to the value stack first: Args may be an unrooted buffer }
  SavedTop := StackTop;
  Base := StackTop;
  if Base + Argc + 4 >= STACK_MAX then Exit(ThrowError('RangeError', 'Maximum call stack size exceeded'));
  VStack[Base] := PV; VStack[Base + 1] := This;
  if Args <> nil then Move(Args^, VStack[Base + 2], Argc * SizeOf(TValue));
  StackTop := Base + Argc + 2;
  if not ProxyGetTrap(PV, 'apply', Trap) then begin StackTop := SavedTop; Exit(False); end;
  if IsNullish(Trap) then
    Result := CallValue(ObjOf(VStack[Base])^.Aux1, VStack[Base + 1], @VStack[Base + 2], Argc, Res)
  else
  begin
    PushRoot(Trap);
    A := NewArray(Argc, Argc);
    for I := 0 to Argc - 1 do ValueArrayStore(A^.Elements, I, VStack[Base + 2 + I]);
    Trap := RootAt(TempTop - 1);
    PopRoots(1);
    Result := ProxyCallTrap(VStack[Base], Trap, 2, VStack[Base + 1], MkPtr(A), VUndefined, Res);
  end;
  StackTop := SavedTop;
end;

function ProxyConstruct(PV: TValue; Args: PValue; Argc: Integer; NewTarget: TValue; var Res: TValue): Boolean;
var Trap: TValue; A: PObj; Base, SavedTop, I: Integer;
begin
  SavedTop := StackTop;
  Base := StackTop;
  if Base + Argc + 4 >= STACK_MAX then Exit(ThrowError('RangeError', 'Maximum call stack size exceeded'));
  VStack[Base] := PV; VStack[Base + 1] := NewTarget;
  if Args <> nil then Move(Args^, VStack[Base + 2], Argc * SizeOf(TValue));
  StackTop := Base + Argc + 2;
  if not ProxyGetTrap(PV, 'construct', Trap) then begin StackTop := SavedTop; Exit(False); end;
  if IsNullish(Trap) then
    Result := Construct(ObjOf(VStack[Base])^.Aux1, @VStack[Base + 2], Argc, VStack[Base + 1], Res)
  else
  begin
    PushRoot(Trap);
    A := NewArray(Argc, Argc);
    for I := 0 to Argc - 1 do ValueArrayStore(A^.Elements, I, VStack[Base + 2 + I]);
    Trap := RootAt(TempTop - 1);
    PopRoots(1);
    Result := ProxyCallTrap(VStack[Base], Trap, 2, MkPtr(A), VStack[Base + 1], VUndefined, Res);
    if Result and not IsObject(Res) then Result := ThrowTypeError('proxy [[Construct]] must return an object');
  end;
  StackTop := SavedTop;
end;


{ 10.2.1 [[Call]] step 2 of a class constructor: the TypeError of its realm }
function ThrowClassCall(F: PObj): Boolean;
var R0: Integer;
begin
  R0 := CurRealm;
  if MultiRealm then SwitchRealm(FnRealm(F));
  Result := ThrowTypeError('Class constructor cannot be invoked without ''new''');
  SwitchRealm(R0);
end;

{$IFDEF CPUX86_64}
function JitRunPushed(var Res: TValue; var Ok: Boolean): Boolean; forward;
{$ENDIF}

function CallValue(F, This: TValue; Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var O: PObj; Base, I, ArgBase: Integer; BArgs: PValueArray; N: Integer; Tmp: array[0..15] of TValue;
  P: PValue; SavedTop: Integer;
begin
  if not IsCallable(F) then Exit(ThrowTypeError('value is not a function'));
  O := ObjOf(F);
  case O^.OKind of
    okProxy: Result := ProxyCall(F, This, Args, Argc, Res);
    okNative:
      begin
        SavedTop := StackTop;
        Base := StackTop;
        if Base + Argc + 2 >= STACK_MAX then Exit(ThrowError('RangeError', 'Maximum call stack size exceeded'));
        { args may point into the value stack: copy before writing }
        VStack[Base] := This;
        if Args <> nil then Move(Args^, VStack[Base + 1], Argc * SizeOf(TValue));
        StackTop := Base + Argc + 1;
        Res := VUndefined;
        { a native sees new.target only when Construct called it; the caller's value comes back afterwards
          (saved on the root stack: a raw local would go stale if the native collects) }
        PushRoot(CurNewTarget);
        CurNewTarget := ConstructNT;
        ConstructNT := VUndefined;
        NativeData := O^.Pad and $00FFFFFF;
        NativeSelf := MkPtr(O);
        if MultiRealmAny and (FnRealm(O) <> CurRealm) then
        begin
          { a built-in function runs in its own realm (10.3.1 [[Call]] step 6) }
          I := CurRealm;
          SwitchRealm(FnRealm(O));
          Result := TNativeFn(O^.Native)(@VStack[Base], Argc, Res);
          SwitchRealm(I);
        end
        else
          Result := TNativeFn(O^.Native)(@VStack[Base], Argc, Res);
        CurNewTarget := RootAt(TempTop - 1);
        PopRoots(1);
        StackTop := SavedTop;
      end;
    okBound:
      begin
        BArgs := PValueArray(PtrOf(O^.Aux2));
        N := BArgs^.Len - 1;
        SavedTop := StackTop;
        Base := StackTop;
        if Base + N + Argc + 2 >= STACK_MAX then Exit(ThrowError('RangeError', 'Maximum call stack size exceeded'));
        for I := 0 to N - 1 do VStack[Base + I] := BArgs^.Data[I + 1];
        if Args <> nil then Move(Args^, VStack[Base + N], Argc * SizeOf(TValue));
        StackTop := Base + N + Argc;
        Result := CallValue(O^.Aux1, BArgs^.Data[0], @VStack[Base], N + Argc, Res);
        StackTop := SavedTop;
      end;
  else
    begin
      if (O^.OFlags and 1) <> 0 then
        Exit(ThrowClassCall(O));
      SavedTop := StackTop;
      ArgBase := StackTop;
      if ArgBase + Argc + 2 >= STACK_MAX then Exit(ThrowError('RangeError', 'Maximum call stack size exceeded'));
      VStack[ArgBase] := This;
      if Args <> nil then Move(Args^, VStack[ArgBase + 1], Argc * SizeOf(TValue));
      StackTop := ArgBase + Argc + 1;
      if not PushBytecodeFrame(O, VStack[ArgBase], @VStack[ArgBase + 1], Argc, ArgBase, 0, VUndefined) then
      begin
        StackTop := SavedTop;
        Exit(False);
      end;
      if Frames[FrameTop - 1].Tpl^.IsAsync and not Frames[FrameTop - 1].Tpl^.IsGenerator then
      begin
        Res := SuspendNewGenerator;
        StackTop := SavedTop;
        Res := AsyncStartHook(Res);
        Result := True;
      end
      else
      {$IFDEF CPUX86_64}
      { the callee's compiled code, run natively when it has some (a callback from a native) }
      if not JitRunPushed(Res, Result) then
      {$ENDIF}
        Result := Execute(FrameTop - 1, Res);
      StackTop := SavedTop;
    end;
  end;
end;

function Construct(F: TValue; Args: PValue; Argc: Integer; NewTarget: TValue; var Res: TValue): Boolean;
var O, ThisO: PObj; Proto, SavedNT: TValue; ArgBase, SavedTop: Integer; T: PFuncTemplate; BArgs: PValueArray;
  N, Base, I: Integer;
begin
  if not IsCallable(F) then Exit(ThrowTypeError('value is not a constructor'));
  O := ObjOf(F);
  if O^.OKind = okNative then
  begin
    if (O^.OFlags and 32) = 0 then Exit(ThrowTypeError('not a constructor'));
    { a subclass (new.target other than the built-in): a native that takes its prototype from new.target
      does so at its step (ProtoFromNewTarget); for the others it is given to the instance afterwards, so
      their argument checks still come first }
    PushRoot(F); PushRoot(NewTarget);
    ConstructProtoTaken := False;
    ConstructNT := NewTarget;
    Result := CallValue(F, VUndefined, Args, Argc, Res);
    ConstructNT := VUndefined;
    if Result and IsObject(Res) and not ConstructProtoTaken and (RootAt(TempTop - 1) <> RootAt(TempTop - 2)) and
       IsObject(RootAt(TempTop - 1)) then
    begin
      PushRoot(Res);
      if not GetV(RootAt(TempTop - 2), AtomPrototype, Proto) then Result := False
      else if IsObject(Proto) and (ObjOf(RootAt(TempTop - 1))^.Shape^.Proto <> Proto) then
        SetProto(ObjOf(RootAt(TempTop - 1)), Proto)
      else if not IsObject(Proto) and not MultiRealm then
      begin
        { 10.1.14 step 3.a GetFunctionRealm(new.target): a revoked proxy throws even with one realm }
        if (ObjOf(RootAt(TempTop - 2))^.OKind = okProxy) and not GetFunctionRealm(RootAt(TempTop - 2), N) then
          Result := False;
      end
      else if not IsObject(Proto) and MultiRealm then
      begin
        { the default prototype of new.target's realm }
        if not GetFunctionRealm(RootAt(TempTop - 2), N) then Result := False
        else
        begin
          Proto := MapIntrinsic(ObjOf(RootAt(TempTop - 1))^.Shape^.Proto, FnRealm(ObjOf(RootAt(TempTop - 3))), N);
          if ObjOf(RootAt(TempTop - 1))^.Shape^.Proto <> Proto then SetProto(ObjOf(RootAt(TempTop - 1)), Proto);
        end;
      end;
      Res := RootAt(TempTop - 1);
      PopRoots(1);
    end;
    ConstructProtoTaken := False;
    PopRoots(2);
    Exit;
  end;
  if O^.OKind = okBound then
  begin
    { 10.4.1.2: the bound arguments first; a new.target that is the bound function becomes its target }
    BArgs := PValueArray(PtrOf(O^.Aux2));
    N := BArgs^.Len - 1;
    SavedTop := StackTop;
    Base := StackTop;
    if Base + N + Argc + 2 >= STACK_MAX then Exit(ThrowError('RangeError', 'Maximum call stack size exceeded'));
    for I := 0 to N - 1 do VStack[Base + I] := BArgs^.Data[I + 1];
    if Args <> nil then Move(Args^, VStack[Base + N], Argc * SizeOf(TValue));
    StackTop := Base + N + Argc;
    if NewTarget = F then NewTarget := O^.Aux1;
    Result := Construct(O^.Aux1, @VStack[Base], N + Argc, NewTarget, Res);
    StackTop := SavedTop;
    Exit;
  end;
  if O^.OKind = okProxy then Exit(ProxyConstruct(F, Args, Argc, NewTarget, Res));
  T := PFuncTemplate(O^.Native);
  if T^.IsArrow or (T^.IsMethod and not T^.IsClassCtor) or T^.IsGenerator or T^.IsAsync then
    Exit(ThrowTypeError('not a constructor'));
  if T^.IsDerived then
  begin
    { this is bound by super() }
    SavedTop := StackTop;
    ArgBase := StackTop;
    VStack[ArgBase] := VHole;
    if Args <> nil then Move(Args^, VStack[ArgBase + 1], Argc * SizeOf(TValue));
    StackTop := ArgBase + Argc + 1;
    if not PushBytecodeFrame(ObjOf(F), VHole, @VStack[ArgBase + 1], Argc, ArgBase, 1, NewTarget) then
    begin
      StackTop := SavedTop;
      Exit(False);
    end;
    Result := Execute(FrameTop - 1, Res);
    StackTop := SavedTop;
    Exit;
  end;
  PushRoot(F); PushRoot(NewTarget);
  if not PrototypeForConstruct(NewTarget, Proto) then begin PopRoots(2); Exit(False); end;
  if not IsObject(Proto) and not DefaultConstructProto(RootAt(TempTop - 2), RootAt(TempTop - 1), Proto) then
  begin
    PopRoots(2);
    Exit(False);
  end;
  PushRoot(Proto);
  ThisO := NewObject(RootShapeFor(RootAt(TempTop - 1)), Max(4, T^.ExpectedProps), okPlain);
  F := RootAt(TempTop - 3); NewTarget := RootAt(TempTop - 2);
  PopRoots(3);
  if Assigned(T^.FieldInit) and not T^.IsDerived then
  begin
    PushRoot(F); PushRoot(NewTarget); PushRoot(MkPtr(ThisO));
    if not RunFieldInit(F, MkPtr(ThisO)) then begin PopRoots(3); Exit(False); end;
    ThisO := ObjOf(RootAt(TempTop - 1)); NewTarget := RootAt(TempTop - 2); F := RootAt(TempTop - 3);
    PopRoots(3);
  end;
  SavedTop := StackTop;
  ArgBase := StackTop;
  VStack[ArgBase] := MkPtr(ThisO);
  if Args <> nil then Move(Args^, VStack[ArgBase + 1], Argc * SizeOf(TValue));
  StackTop := ArgBase + Argc + 1;
  if not PushBytecodeFrame(ObjOf(F), VStack[ArgBase], @VStack[ArgBase + 1], Argc, ArgBase, 1, NewTarget) then
  begin
    StackTop := SavedTop;
    Exit(False);
  end;
  Result := Execute(FrameTop - 1, Res);
  StackTop := SavedTop;
end;

{ 10.4.4.6 CreateUnmappedArgumentsObject / 10.4.4.7 CreateMappedArgumentsObject (the parameters are not
  aliased): the elements, a virtual length, then @@iterator and callee in a cached shape's two inline slots.
  The callee (Callee, the top root) is the function for a mapped object, %ThrowTypeError% accessors otherwise. }
threadvar
  ArgsShapeMapped, ArgsShapeUnmapped: PShape;

function NewArgumentsObject(Args: PValue; Argc: Integer; Mapped: Boolean): TValue;
var O: PObj; E: PValueArray; I: Integer; IterKey: PStr;
begin
  if ArgsShapeMapped = nil then
  begin
    IterKey := PStr(PPointer(PByte(PtrOf(SymIterator)) + 16)^);
    O := NewPlainObject(ObjectProto, 2);
    AddProperty(O, IterKey, VUndefined, pfHidden);
    AddProperty(O, AtomCallee, VUndefined, pfHidden);
    ArgsShapeMapped := O^.Shape;
    O := NewPlainObject(ObjectProto, 2);
    AddProperty(O, IterKey, VUndefined, pfHidden);
    AddProperty(O, AtomCallee, VUndefined, pfAccessor);
    ArgsShapeUnmapped := O^.Shape;
  end;
  E := NewValueArray(Argc);
  PushRoot(MkPtr(E));
  if Mapped then O := NewObject(ArgsShapeMapped, 2, okArguments)
  else O := NewObject(ArgsShapeUnmapped, 2, okArguments);
  E := PValueArray(PtrOf(RootAt(TempTop - 1)));
  PopRoots(1);
  for I := 0 to Argc - 1 do E^.Data[I] := Args[I];
  E^.Len := Argc;
  O^.Elements := E;
  WriteBarrier(@O^.Hdr, MkPtr(E));
  SetSlot(O, 0, ArrayValuesFnPublic);
  if Mapped then SetSlot(O, 1, RootAt(TempTop - 1)) else SetSlot(O, 1, ThrowerAcc);
  Result := MkPtr(O);
end;

{ sets up a frame whose this is at Args[-1] (the slot before the first argument) }
{ the frame of a function of another realm is set up in that realm (10.2.1.1 PrepareForOrdinaryCall: its this
  binding, its arguments object); the caller's realm comes back until the callee runs (Execute's LReload) }
function PushBytecodeFrameInRealm(FObj: PObj; This: TValue; Args: PValue; Argc: Integer; RetIdx: Integer; Kind: Byte;
  NewTarget: TValue): Boolean;
var R0: Integer;
begin
  R0 := CurRealm;
  SwitchRealm(FnRealm(FObj));
  Result := PushBytecodeFrame(FObj, This, Args, Argc, RetIdx, Kind, NewTarget);
  SwitchRealm(R0);
end;

{ PushBytecodeFrame's full case: another realm, a rest parameter or an arguments object, or a this to bind }
function PushBytecodeFrameFull(FObj: PObj; This: TValue; Args: PValue; Argc: Integer; RetIdx: Integer; Kind: Byte;
  NewTarget: TValue): Boolean; noinline;
var T: PFuncTemplate; Base, I, NP, Extra: Integer; Fr: PFrame; R: PValue; A: PObj; FV, Tmp, ArgsObj: TValue; FI: Integer;
begin
  if MultiRealmAny and (FnRealm(FObj) <> CurRealm) then
    Exit(PushBytecodeFrameInRealm(FObj, This, Args, Argc, RetIdx, Kind, NewTarget));
  T := PFuncTemplate(FObj^.Native);
  if (T^.Lazy <> nil) and not LazyCompileHook(T) then Exit(False);
  Base := (Args - VStack) - 1;
  if (Base + T^.MaxRegs >= STACK_MAX) or (FrameTop >= FRAME_MAX - 2) then
    Exit(ThrowError('RangeError', 'Maximum call stack size exceeded'));
  FV := MkPtr(FObj);
  R := @VStack[Base];
  if not T^.IsStrict and not T^.IsArrow then
    { 10.2.1.2 OrdinaryCallBindThis: the global object for undefined and null, a primitive as an object of the
      callee's realm (the running one here) }
    if (This = VUndefined) or (This = VNull) then This := GlobalObj
    else if not IsObject(This) and (This <> VHole) then
    begin
      PushRoot(FV); PushRoot(NewTarget);
      Tmp := This;
      if not ToObjectHook(Tmp, This) then begin PopRoots(2); Exit(False); end;
      NewTarget := RootAt(TempTop - 1); FV := RootAt(TempTop - 2);
      PopRoots(2);
      FObj := ObjOf(FV);
    end;
  R[0] := This;
  NP := T^.NumParams;
  if (T^.RestReg >= 0) or (T^.ArgsReg >= 0) then
  begin
    { collect extra / all arguments before the locals overwrite them (the rest array and the arguments object
      allocate: new.target and the function stay rooted, read back afterwards) }
    PushRoot(NewTarget);
    PushRoot(FV);
    if T^.RestReg >= 0 then
    begin
      Extra := Argc - NP;
      if Extra < 0 then Extra := 0;
      A := NewArray(Extra, Extra);
      for I := 0 to Extra - 1 do A^.Elements^.Data[I] := VStack[Base + 1 + NP + I];
      Tmp := MkPtr(A);
    end
    else Tmp := VUndefined;
    if T^.ArgsReg >= 0 then
    begin
      PushRoot(Tmp);
      PushRoot(RootAt(TempTop - 2));   { the function, for a mapped object's callee }
      ArgsObj := NewArgumentsObject(Args, Argc, not T^.IsStrict and T^.SimpleParams);
      Tmp := RootAt(TempTop - 2);
      PopRoots(2);
    end;
    FV := RootAt(TempTop - 1);
    NewTarget := RootAt(TempTop - 2);
    PopRoots(2);
    StackTop := Max(StackTop, Base + T^.MaxRegs);
    I := Min(Argc, NP) + 1;
    if I < T^.MaxRegs then FillUndef(@R[I], T^.MaxRegs - I);
    if T^.RestReg >= 0 then R[T^.RestReg] := Tmp;
    if T^.ArgsReg >= 0 then
    begin
      R[T^.ArgsReg] := ArgsObj;
      if T^.MappedArgs and (Min(Argc, NP) > 0) then
      begin
        PushRoot(NewTarget);
        PushRoot(FV);
        XMapArguments(Base, Min(Argc, NP), T^.ArgsReg);
        FV := RootAt(TempTop - 1);
        NewTarget := RootAt(TempTop - 2);
        PopRoots(2);
        FObj := ObjOf(FV);
      end;
    end;
  end
  else
  begin
    I := Min(Argc, NP) + 1;
    if I < T^.MaxRegs then FillUndef(@R[I], T^.MaxRegs - I);
  end;
  if StackTop < Base + T^.MaxRegs then StackTop := Base + T^.MaxRegs;
  Fr := @Frames[FrameTop];
  Fr^.Closure := FV;
  Fr^.NewTarget := NewTarget;
  Fr^.Tpl := T;
  Fr^.Base := Base;
  Fr^.PC := T^.EntryPC;
  Fr^.RetIdx := RetIdx;
  Fr^.Argc := Argc;
  Fr^.Kind := Kind;
  Inc(FrameTop);
  Result := True;
end;

{ a bytecode function's frame: the arguments at Args (Args[-1] its this slot), Kind 0 call, 1 construct, 2
  script; the common case here, the rest in PushBytecodeFrameFull }
function PushBytecodeFrame(FObj: PObj; This: TValue; Args: PValue; Argc: Integer; RetIdx: Integer; Kind: Byte;
  NewTarget: TValue): Boolean;
var T: PFuncTemplate; Base, I: Integer; Fr: PFrame; R: PValue;
begin
  T := PFuncTemplate(FObj^.Native);
  if (T^.Lazy <> nil) and not LazyCompileHook(T) then Exit(False);
  { a non-strict function's undefined or null this is the global object (OrdinaryCallBindThis) }
  if not T^.IsStrict and not T^.IsArrow and ((This = VUndefined) or (This = VNull)) and not MultiRealmAny then
    This := GlobalObj;
  if MultiRealmAny or (T^.RestReg >= 0) or (T^.ArgsReg >= 0) or
     (not T^.IsStrict and not T^.IsArrow and not IsObject(This)) then
    Exit(PushBytecodeFrameFull(FObj, This, Args, Argc, RetIdx, Kind, NewTarget));
  Base := (Args - VStack) - 1;
  if (Base + T^.MaxRegs >= STACK_MAX) or (FrameTop >= FRAME_MAX - 2) then
    Exit(ThrowError('RangeError', 'Maximum call stack size exceeded'));
  R := @VStack[Base];
  R[0] := This;
  I := Min(Argc, T^.NumParams) + 1;
  if I < T^.MaxRegs then FillUndef(@R[I], T^.MaxRegs - I);
  if StackTop < Base + T^.MaxRegs then StackTop := Base + T^.MaxRegs;
  Fr := @Frames[FrameTop];
  Fr^.Closure := MkPtr(FObj);
  Fr^.NewTarget := NewTarget;
  Fr^.Tpl := T;
  Fr^.Base := Base;
  Fr^.PC := T^.EntryPC;
  Fr^.RetIdx := RetIdx;
  Fr^.Argc := Argc;
  Fr^.Kind := Kind;
  Inc(FrameTop);
  Result := True;
end;

{ ---------------- the interpreter ---------------- }

function ICGetEntry(E: PICEntry; O: PObj; var Res: TValue): Boolean;
var P: PObj; D: Integer;
begin
  if E^.Acc <> 0 then Exit(False);   { an accessor: the miss path calls the getter }
  P := O;
  for D := 0 to E^.Depth - 1 do
  begin
    { a dictionary on the way may have gained the key since }
    if ((E^.VerMask shr D) and 1 <> 0) and (P^.Shape^.Version <> E^.Vers[D]) then Exit(False);
    P := ObjOf(P^.Shape^.Proto);
    if P^.Shape <> E^.Holder[D] then begin {$IFDEF G2_ICSTATS}Inc(ICMissDepthFail);{$ENDIF} Exit(False); end;
  end;
  Res := GetSlot(P, E^.Slot);
  Result := True;
end;

{ the IC's other entries (out of line: the dispatch loop stays small) }
function ICGetOther(IC: PPropIC; O: PObj; var Res: TValue): Boolean;
var I: Integer;
begin
  for I := 1 to IC^.PolyCount - 1 do
    if (IC^.More^[I].Shape = O^.Shape) and (IC^.More^[I].Kind = O^.OKind) then
      begin {$IFDEF G2_ICSTATS}Inc(ICHitOther);{$ENDIF} Exit(ICGetEntry(@IC^.More^[I], O, Res)); end;
  Result := False;
end;

{ a property get through the IC's entries: the receiver's shape and kind pick the entry }
function ICGet(IC: PPropIC; O: PObj; var Res: TValue): Boolean; inline;
begin
  if (IC^.Poly0.Shape = O^.Shape) and (IC^.Poly0.Kind = O^.OKind) and (IC^.Poly0.Acc = 0) then
  begin
    {$IFDEF G2_ICSTATS}Inc(ICHit0);{$ENDIF}
    case IC^.Poly0.Depth of
      0:
        begin
          Res := GetSlot(O, IC^.Poly0.Slot);
          Exit(True);
        end;
      1:
        { a method on the prototype: the common case, inline }
        if (IC^.Poly0.VerMask = 0) and (ObjOf(O^.Shape^.Proto)^.Shape = IC^.Poly0.Holder[0]) then
        begin
          Res := GetSlot(ObjOf(O^.Shape^.Proto), IC^.Poly0.Slot);
          Exit(True);
        end;
    end;
    Exit(ICGetEntry(@IC^.Poly0, O, Res));
  end;
  Result := (IC^.PolyCount > 1) and ICGetOther(IC, O, Res);
end;

{ HasProperty on O's prototypes for a global name: a proxy on the chain answers through its has trap }
function InheritedHasV(O: PObj; Key: PStr; var Found: Boolean): Boolean;
var Pv: TValue;
begin
  Found := False;
  Result := True;
  Pv := O^.Shape^.Proto;
  while IsObject(Pv) do
  begin
    if ObjOf(Pv)^.OKind = okProxy then Exit(HasPropertyV(Pv, Key, Found));
    if ShapeLookup(ObjOf(Pv)^.Shape, Key) >= 0 then begin Found := True; Exit; end;
    Pv := ObjOf(Pv)^.Shape^.Proto;
  end;
end;

function InheritedHas(O: PObj; Key: PStr): Boolean;
var Pv: TValue;
begin
  Pv := O^.Shape^.Proto;
  while IsObject(Pv) do
  begin
    if ShapeLookup(ObjOf(Pv)^.Shape, Key) >= 0 then Exit(True);
    Pv := ObjOf(Pv)^.Shape^.Proto;
  end;
  Result := False;
end;

{ own properties the object model computes instead of storing: value and the spec's attributes }
function OwnVirtual(O: PObj; Key: PStr; var V: TValue; var Flags: Byte): Boolean;
begin
  Result := False;
  if not (O^.OKind in [okArray, okArguments, okFunction, okNative, okStringObj]) then Exit;
  if VirtualProp(O, Key, V) then
  begin
    if O^.OKind in [okFunction, okNative] then Flags := pfConfigurable
    else if O^.OKind = okArguments then Flags := pfWritable or pfConfigurable
    else if O^.OKind = okArray then Flags := pfWritable
    else Flags := 0;
    Exit(True);
  end;
  if (O^.OKind = okFunction) and (Key = AtomPrototype) and ((O^.OFlags and 4) = 0) then
  begin
    V := FunctionPrototypeOf(MkPtr(O));
    Flags := pfWritable;
    Result := not IsNullish(V);
  end;
end;

{ the first shape stays in entry 0 (checked inline); later ones take entries 1-3 in turn }
procedure ICAddEntry(IC: PPropIC; const E: TICEntry);
var N: Integer;
begin
  if IC^.PolyCount = 0 then
  begin
    IC^.Poly0 := E;
    IC^.PolyCount := 1;
    IC^.PolyNext := 1;
  end
  else
  begin
    N := IC^.PolyNext;
    if IC^.More = nil then IC^.More := AllocMem(SizeOf(TICEntryMore));
    IC^.More^[N] := E;
    if N >= IC^.PolyCount then IC^.PolyCount := N + 1;
    if N = 3 then IC^.PolyNext := 1 else IC^.PolyNext := N + 1;
  end;
end;

function ICHasShape(IC: PPropIC; O: PObj): Boolean;
var I: Integer;
begin
  for I := 1 to IC^.PolyCount - 1 do
    if (IC^.More^[I].Shape = O^.Shape) and (IC^.More^[I].Kind = O^.OKind) then Exit(True);
  Result := False;
end;

{ an IC accessor entry for O's shape whose holder still checks out: 1 its getter called (Res its result), 0 the
  getter threw, 2 no such entry }
function ICAccGet(IC: PPropIC; O: PObj; Receiver: TValue; var Res: TValue): Integer;
var E: PICEntry; P: PObj; D, I: Integer; G: TValue;
begin
  Result := 2;
  E := nil;
  if (IC^.Poly0.Shape = O^.Shape) and (IC^.Poly0.Kind = O^.OKind) then E := @IC^.Poly0
  else
    for I := 1 to IC^.PolyCount - 1 do
      if (IC^.More^[I].Shape = O^.Shape) and (IC^.More^[I].Kind = O^.OKind) then begin E := @IC^.More^[I]; Break; end;
  if (E = nil) or (E^.Acc = 0) then Exit;
  P := O;
  for D := 0 to E^.Depth - 1 do
  begin
    if ((E^.VerMask shr D) and 1 <> 0) and (P^.Shape^.Version <> E^.Vers[D]) then Exit;
    P := ObjOf(P^.Shape^.Proto);
    if P^.Shape <> E^.Holder[D] then Exit;
  end;
  G := PAccessor(PtrOf(GetSlot(P, E^.Slot)))^.Getter;
  if G = VUndefined then begin Res := VUndefined; Exit(1); end;
  AccessorGetKey := IC^.Key;
  if CallValue(G, Receiver, nil, 0, Res) then Result := 1 else Result := 0;
end;

{ an IC miss: GetFromObject's walk (same order: own shape, virtual property, elements, function prototype,
  then the prototype), and an entry for the IC when the value is a data property the shapes alone decide }
function ICGetMiss(IC: PPropIC; O: PObj; Receiver: TValue; var Res: TValue): Boolean;
var P: PObj; S, D, N: Integer; Flags: Byte; Acc: PAccessor; Pv, V: TValue; Idx: Int64; E: TICEntry; Cacheable: Boolean;
begin
  { an index-like key can hit virtual properties that depend on more than the shape (a String object's
    characters): never cached }
  {$IFDEF G2_ICSTATS}Inc(ICMissN);{$ENDIF}
  Cacheable := not IndexFromString(IC^.Key, Idx);
  {$IFDEF G2_ICSTATS}if not Cacheable then Inc(ICWhy[1]);{$ENDIF}
  E.Shape := O^.Shape;
  E.Kind := O^.OKind;
  E.VerMask := 0;
  E.Acc := 0;
  P := O;
  D := 0;
  while True do
  begin
    if P^.OKind = okProxy then begin {$IFDEF G2_ICSTATS}Inc(ICWhy[6]);{$ENDIF} Exit(ProxyGet(MkPtr(P), IC^.Key, Receiver, Res)); end;
    { a module namespace's exports are live bindings: never cached }
    if P^.OKind = okModuleNS then Exit(GetFromObject(P, IC^.Key, Receiver, Res));
    S := ShapeLookup(P^.Shape, IC^.Key);
    if S >= 0 then
    begin
      Flags := P^.Shape^.PFlags[S];
      V := GetSlot(P, S);
      if (Flags and pfAccessor) <> 0 then
      begin
        {$IFDEF G2_ICSTATS}Inc(ICWhy[3]);{$ENDIF}
        { an entry for the accessor, once (generic readers treat it as a miss and come back here): FastLoop calls a
          bytecode getter from it without this walk }
        if Cacheable and (IC^.Poly0.Shape <> O^.Shape) and not ICHasShape(IC, O) then
        begin
          E.Depth := D;
          E.Slot := S;
          E.Acc := 1;
          ICAddEntry(IC, E);
        end;
        Acc := PAccessor(PtrOf(V));
        if Acc^.Getter = VUndefined then begin Res := VUndefined; Exit(True); end;
        AccessorGetKey := IC^.Key;
        Exit(CallValue(Acc^.Getter, Receiver, nil, 0, Res));
      end;
      Res := V;
      if Cacheable then
      begin
        {$IFDEF G2_ICSTATS}Inc(ICMissFill);{$ENDIF}
        E.Depth := D;
        E.Slot := S;
        ICAddEntry(IC, E);
      end;
      Exit(True);
    end;
    if VirtualProp(P, IC^.Key, Res) then begin {$IFDEF G2_ICSTATS}Inc(ICWhy[4]);{$ENDIF} Exit(True); end;
    if (P^.Elements <> nil) and IndexFromString(IC^.Key, Idx) then
    begin
      Cacheable := False;   { elements change without a shape change }
      if Idx < P^.Elements^.Len then
      begin
        Res := P^.Elements^.Data[Idx];
        if Res <> VHole then Exit(True);
      end;
    end;
    if (P^.OKind = okFunction) and (IC^.Key = AtomPrototype) then
    begin
      {$IFDEF G2_ICSTATS}Inc(ICWhy[7]);{$ENDIF}
      Res := FunctionPrototypeOf(MkPtr(P));
      Exit(True);
    end;
    Pv := P^.Shape^.Proto;
    if not IsObject(Pv) then begin {$IFDEF G2_ICSTATS}Inc(ICWhy[2]);{$ENDIF} Res := VUndefined; Exit(True); end;
    if D >= IC_MAX_DEPTH then
    begin
      {$IFDEF G2_ICSTATS}if Cacheable then Inc(ICWhy[5]);{$ENDIF}
      Cacheable := False;
    end
    else if P^.Shape^.IsDict then
    begin
      { passing a dictionary: a key added to it later would shadow what is found below }
      E.VerMask := E.VerMask or (1 shl D);
      E.Vers[D] := P^.Shape^.Version;
    end;
    P := ObjOf(Pv);
    if D < IC_MAX_DEPTH then E.Holder[D] := P^.Shape;
    Inc(D);
  end;
end;

{ A property read on a string primitive: a cache of String.prototype's own data property, in the IC's receiver
  shape and slot (a get IC's own entries are Poly0 and More). Deleting or redefining one of the prototype's
  properties gives it a new shape; adding one keeps every key's slot. A string's own properties are its indices
  and length, which no other key names. }
procedure ICFillString(IC: PPropIC); noinline;
var P: PObj; S: Integer;
begin
  if IC^.Key = AtomLength then Exit;
  P := ObjOf(StringProto);
  S := ShapeLookup(P^.Shape, IC^.Key);
  if (S >= 0) and ((P^.Shape^.PFlags[S] and pfAccessor) = 0) then
  begin
    IC^.Shape := P^.Shape;
    IC^.Slot := S;
  end;
end;

procedure ICFillGet(IC: PPropIC; O: PObj);
var P: PObj; D, S: Integer; Pv: TValue;
begin
  IC^.Shape := nil;
  if O^.Shape^.IsDict then Exit;
  P := O;
  D := 0;
  while True do
  begin
    S := ShapeLookup(P^.Shape, IC^.Key);
    if S >= 0 then
    begin
      if (P^.Shape^.PFlags[S] and pfAccessor) <> 0 then Exit;
      IC^.Shape := O^.Shape;
      IC^.Depth := D;
      IC^.Slot := S;
      Exit;
    end;
    { a dictionary prototype changes in place, so a shadowing key added to it would go unseen }
    if (D > 0) and P^.Shape^.IsDict then begin IC^.Shape := nil; Exit; end;
    if P^.OKind in [okArray, okFunction, okNative, okStringObj, okArguments] then
      if (IC^.Key = AtomLength) or (IC^.Key = AtomName) or (IC^.Key = AtomPrototype) then Exit;
    Pv := P^.Shape^.Proto;
    if not IsObject(Pv) or (D >= 4) then Exit;
    P := ObjOf(Pv);
    IC^.Holder[D] := P^.Shape;
    Inc(D);
  end;
end;

function AddNumbersSlow(A, B: TValue; var Res: TValue): Boolean;
var PA, PB, SA, SB: TValue; DA, DB: Double;
begin
  PushRoot(B);
  if not ToPrimitiveHint(A, phDefault, PA) then begin PopRoots(1); Exit(False); end;
  PushRoot(PA);
  if not ToPrimitiveHint(RootAt(TempTop - 2), phDefault, PB) then begin PopRoots(2); Exit(False); end;
  PA := RootAt(TempTop - 1);
  PopRoots(2);
  if (IsBigInt(PA) or IsBigInt(PB)) and not (IsString(PA) or IsString(PB)) then
  begin
    if not (IsBigInt(PA) and IsBigInt(PB)) then Exit(ThrowTypeError('Cannot mix BigInt and other types, use explicit conversions'));
    Res := BigToValue(ValueToBig(PA).Add(ValueToBig(PB)));
    Exit(True);
  end;
  if IsString(PA) or IsString(PB) then
  begin
    PushRoot(PB);
    if not ToStringV(PA, SA) then begin PopRoots(1); Exit(False); end;
    PushRoot(SA);
    if not ToStringV(RootAt(TempTop - 2), SB) then begin PopRoots(2); Exit(False); end;
    SA := RootAt(TempTop - 1);
    PopRoots(2);
    Res := ConcatStrings(StrOf(SA), StrOf(SB));
    Exit(True);
  end;
  { ToNumeric of each: a Symbol throws }
  if not ToNumberV(PA, DA) then Exit(False);
  if not ToNumberV(PB, DB) then Exit(False);
  Res := MkNumber(DA + DB);
  Result := True;
end;

function BigArith(Op: TOp; A, B: TValue; var Res: TValue): Boolean;
var X, Y, Z: TBigInteger;
begin
  if not (IsBigInt(A) and IsBigInt(B)) then Exit(ThrowTypeError('Cannot mix BigInt and other types, use explicit conversions'));
  X := ValueToBig(A); Y := ValueToBig(B);
  case Op of
    opSub: Z := X.Subtract(Y);
    opMul: Z := X.Multiply(Y);
    opDiv: begin if Y.IsZero then Exit(ThrowError('RangeError', 'Division by zero')); Z := X.Divide(Y); end;
    opMod: begin if Y.IsZero then Exit(ThrowError('RangeError', 'Division by zero')); Z := X.Modulo(Y); end;
    opPow:
      begin
        { 6.1.6.2.3 BigInt::exponentiate: a negative exponent is a RangeError }
        if Y.IsNegative then Exit(ThrowError('RangeError', 'Exponent must be non-negative'));
        Z := X.Power(Y);
      end;
    opBitAnd: Z := X.BitwiseAnd(Y);
    opBitOr: Z := X.BitwiseOr(Y);
    opBitXor: Z := X.BitwiseXor(Y);
    opShl: Z := X.ShiftLeft(Y.ToInt64);
    opShr: Z := X.ShiftRight(Y.ToInt64);
  else
    Exit(ThrowTypeError('BigInts have no unsigned right shift, use >> instead'));
  end;
  Res := BigToValue(Z);
  Result := True;
end;

{ 7.1.3 ToNumeric: a BigInt stays one, anything else becomes a Number }
function ToNumericV(V: TValue; var Res: TValue): Boolean;
var D: Double;
begin
  if IsNumber(V) or IsBigInt(V) then begin Res := V; Exit(True); end;
  if IsObject(V) then
  begin
    if not ToPrimitiveHint(V, phNumber, V) then Exit(False);
    if IsBigInt(V) then begin Res := V; Exit(True); end;
  end;
  if not ToNumberV(V, D) then Exit(False);
  Res := MkNumber(D);
  Result := True;
end;

function ArithSlow(Op: TOp; A, B: TValue; var Res: TValue): Boolean;
var DA, DB, R: Double; IA, IB: Int32;
begin
  { 13.15.3 ApplyStringOrNumericBinaryOperator: ToNumeric of both operands, left first; mixed types throw }
  if not (IsNumber(A) and IsNumber(B)) then
  begin
    PushRoot(B);
    if not ToNumericV(A, A) then begin PopRoots(1); Exit(False); end;
    B := RootAt(TempTop - 1);
    PushRoot(A);
    if not ToNumericV(B, B) then begin PopRoots(2); Exit(False); end;
    A := RootAt(TempTop - 1);
    PopRoots(2);
    if IsBigInt(A) or IsBigInt(B) then Exit(BigArith(Op, A, B, Res));
  end;
  DA := NumOf(A);
  DB := NumOf(B);
  case Op of
    opSub: R := DA - DB;
    opMul: R := DA * DB;
    opDiv: R := DA / DB;
    opMod: R := NumberRemainder(DA, DB);
    opPow: R := NumberExponentiate(DA, DB);
  else
    begin
      IA := Int32(DoubleToUint32Mod(DA));
      IB := Int32(DoubleToUint32Mod(DB));
      case Op of
        opBitAnd: Res := MkInt(IA and IB);
        opBitOr: Res := MkInt(IA or IB);
        opBitXor: Res := MkInt(IA xor IB);
        opShl: Res := MkInt(Int32(UInt32(IA) shl (IB and 31)));
        opShr: Res := MkInt(SarLongint(IA, IB and 31));
        opUShr: Res := MkNumber(UInt32(IA) shr (IB and 31));
      end;
      Exit(True);
    end;
  end;
  Res := MkNumber(R);
  Result := True;
end;

function CompareSlow(A, B: TValue; var Res: Integer): Boolean;   { -1 less, 0 equal, 1 greater, 2 undefined (NaN) }
var PA, PB, V: TValue; DA, DB: Double; C: Integer;
begin
  PushRoot(B);
  if not ToPrimitiveV(A, False, PA) then begin PopRoots(1); Exit(False); end;
  PushRoot(PA);
  if not ToPrimitiveV(RootAt(TempTop - 2), False, PB) then begin PopRoots(2); Exit(False); end;
  PA := RootAt(TempTop - 1);
  PopRoots(2);
  { 7.2.12 IsLessThan: a BigInt against a String goes through StringToBigInt (undefined when that fails), against
    anything else through ToNumeric (a Symbol throws) and an exact comparison }
  if IsBigInt(PA) or IsBigInt(PB) then
  begin
    if IsBigInt(PA) and IsBigInt(PB) then C := BigCompare(PA, PB)
    else if IsString(PA) or IsString(PB) then
    begin
      PushRoot(PA); PushRoot(PB);
      if IsString(PA) then
      begin
        if not StringToBigIntV(StrToUnicode(StrOf(PA)), V) then begin PopRoots(2); Res := 2; Exit(True); end;
        C := BigCompare(V, RootAt(TempTop - 1));
      end
      else
      begin
        if not StringToBigIntV(StrToUnicode(StrOf(PB)), V) then begin PopRoots(2); Res := 2; Exit(True); end;
        C := BigCompare(RootAt(TempTop - 2), V);
      end;
      PopRoots(2);
    end
    else if IsBigInt(PA) then
    begin
      if not ToNumberV(PB, DB) then Exit(False);
      C := BigCmpNum(PA, DB);
    end
    else
    begin
      if not ToNumberV(PA, DA) then Exit(False);
      C := BigCmpNum(PB, DA);
      if C <> 2 then C := -C;
    end;
    if C = 2 then Res := 2
    else if C < 0 then Res := -1 else if C > 0 then Res := 1 else Res := 0;
    Exit(True);
  end;
  if IsString(PA) and IsString(PB) then
  begin
    C := StrCompare(StrOf(PA), StrOf(PB));
    if C < 0 then Res := -1 else if C > 0 then Res := 1 else Res := 0;
    Exit(True);
  end;
  if not ToNumberV(PA, DA) then Exit(False);
  if not ToNumberV(PB, DB) then Exit(False);
  if IsNaN(DA) or IsNaN(DB) then Res := 2
  else if DA < DB then Res := -1 else if DA > DB then Res := 1 else Res := 0;
  Result := True;
end;

type
  PTypeofAtoms = ^TTypeofAtoms;
  TTypeofAtoms = record Undef, Obj, Bool, Num, Str, Sym, Big, Fn: PStr; end;

threadvar
  TypeofAtoms: PTypeofAtoms;   { on the heap: the TLS miscompile avoids loads through a threadvar record }

function TypeofAtomsInit: PTypeofAtoms; noinline;
begin
  New(Result);
  Result^.Undef := AtomOf('undefined'); Result^.Obj := AtomOf('object'); Result^.Bool := AtomOf('boolean');
  Result^.Num := AtomOf('number'); Result^.Str := AtomOf('string'); Result^.Sym := AtomOf('symbol');
  Result^.Big := AtomOf('bigint'); Result^.Fn := AtomOf('function');
  TypeofAtoms := Result;
end;

{ 13.5.3 typeof: the result string's atom, made once }
function TypeofValue(V: TValue): TValue;
var A: PTypeofAtoms;
begin
  A := TypeofAtoms;
  if A = nil then A := TypeofAtomsInit;
  if IsNumber(V) then Exit(MkPtr(A^.Num));
  case V of
    VUndefined, VHole: Exit(MkPtr(A^.Undef));
    VNull: Exit(MkPtr(A^.Obj));
    VTrue, VFalse: Exit(MkPtr(A^.Bool));
  end;
  case PCell(PtrOf(V))^.Kind of
    ckString: Result := MkPtr(A^.Str);
    ckSymbol: Result := MkPtr(A^.Sym);
    9: Result := MkPtr(A^.Big);
    ckObject:
      if (PCell(PtrOf(V))^.Flags and cfEmulatesUndefined) <> 0 then Result := MkPtr(A^.Undef)
      else if IsCallable(V) then Result := MkPtr(A^.Fn) else Result := MkPtr(A^.Obj);
  else Result := MkPtr(A^.Undef);
  end;
end;

function SpreadIntoArray(Arr, Src: TValue): Boolean;
var It, V: TValue; Done: Boolean; E: PValueArray; I, N: Integer;
begin
  { an array iterated by the intact intrinsics, with all its indices as elements: read them directly (a hole
    reads through the prototype chain, as the iterator's Get would) }
  if IsObject(Src) and (ObjOf(Src)^.OKind = okArray) and (ObjOf(Src)^.Shape^.Proto = ArrayProto) and
     ((ObjOf(Src)^.OFlags and 64) = 0) and (ShapeLookup(ObjOf(Src)^.Shape, SymKey(SymIterator)) < 0) and
     ArrayIterationIntact then
  begin
    PushRoot(Arr); PushRoot(Src);
    I := 0;
    while True do
    begin
      E := ObjOf(RootAt(TempTop - 1))^.Elements;
      if (E = nil) or (I >= Integer(E^.Len)) then Break;
      V := E^.Data[I];
      if (V = VHole) and not GetElemV(RootAt(TempTop - 1), MkInt(I), V) then begin PopRoots(2); Exit(False); end;
      ArrayPush(ObjOf(RootAt(TempTop - 2)), V);
      Inc(I);
    end;
    PopRoots(2);
    Exit(True);
  end;
  PushRoot(Arr);
  if not GetIterator(Src, It) then begin PopRoots(1); Exit(False); end;
  PushRoot(It);
  while True do
  begin
    if not IteratorStep(RootAt(TempTop - 1), V, Done) then begin PopRoots(2); Exit(False); end;
    if Done then Break;
    ArrayPush(ObjOf(RootAt(TempTop - 2)), V);
  end;
  PopRoots(2);
  Result := True;
end;

function CopyDataProps(Target, Src: TValue): Boolean;
var Keys, V: TValue; I, N: Integer; K: PStr; S: PShape;
begin
  if IsNullish(Src) then Exit(True);
  if not IsObject(Src) then
  begin
    { ToObject: only a String object has own enumerable properties }
    if IsString(Src) and (StrOf(Src)^.Len > 0) then Exit(CopyDataPropsHook(Target, Src));
    Exit(True);
  end;
  if not (ObjOf(Src)^.OKind in [okPlain, okArray, okFunction]) then Exit(CopyDataPropsHook(Target, Src));
  S := ObjOf(Src)^.Shape;
  for I := 0 to S^.Count - 1 do
    if (S^.Keys[I]^.Hdr.Extra and kfSymbol) <> 0 then Exit(CopyDataPropsHook(Target, Src));
  PushRoot(Target); PushRoot(Src);
  Keys := OwnKeys(ObjOf(Src), True);
  if OwnKeysFailed then begin OwnKeysFailed := False; PopRoots(2); Exit(False); end;
  PushRoot(Keys);
  N := ArrayLen(ObjOf(Keys));
  for I := 0 to N - 1 do
  begin
    K := Atomize(StrOf(ObjOf(RootAt(TempTop - 1))^.Elements^.Data[I]));
    if not GetV(RootAt(TempTop - 2), K, V) then begin PopRoots(3); Exit(False); end;
    DefineOwn(RootAt(TempTop - 3), K, V, pfDefault);
  end;
  PopRoots(3);
  Result := True;
end;

{ an own property of an ordinary object (a proxy's are not checked: it shadows nothing here) }
function ForInOwnHas(O: PObj; Key: PStr): Boolean;
var Idx: Int64; V: TValue;
begin
  if O^.OKind = okProxy then Exit(False);
  if (O^.OKind = okTypedArray) and IndexFromString(Key, Idx) then Exit(Idx < TALength(O));
  if (O^.Elements <> nil) and IndexFromString(Key, Idx) and (Idx < O^.Elements^.Len) and (O^.Elements^.Data[Idx] <> VHole) then
    Exit(True);
  Result := (ShapeLookup(O^.Shape, Key) >= 0) or VirtualProp(O, Key, V);
end;

{ 14.7.5.9 EnumerateObjectProperties: the enumerable string keys of V and its prototypes, each once, a prototype's
  key left out when an object before it has that key as an own property (enumerable or not). Obj: ToObject(V), for
  the loop's check that a key is still there when its turn comes }
function ForInKeys(V: TValue; var It, Obj: TValue): Boolean;
var A, K, P, Q: TValue; I, N, J, D: Integer; Key: PStr; Shadow: Boolean;
begin
  Obj := VUndefined;
  if not IsNullish(V) and not IsObject(V) and Assigned(ToObjectHook) then
    if not ToObjectHook(V, V) then Exit(False);
  if not IsObject(V) then
  begin
    A := MkPtr(NewArray(0, 0));
    It := NewArrayIterator(A, 0);
    Exit(True);
  end;
  PushRoot(V);
  A := OwnKeys(ObjOf(V), True);
  if OwnKeysFailed then begin OwnKeysFailed := False; PopRoots(1); Exit(False); end;
  PushRoot(A);
  { a namespace's [[GetOwnProperty]] reads each export: an uninitialized one is a ReferenceError }
  if ObjOf(RootAt(TempTop - 2))^.OKind = okModuleNS then
    for I := 0 to ArrayLen(ObjOf(RootAt(TempTop - 1))) - 1 do
      if not NSLookup(ObjOf(RootAt(TempTop - 2)), StrOf(ObjOf(RootAt(TempTop - 1))^.Elements^.Data[I]), True, K,
        Shadow) then begin PopRoots(2); Exit(False); end;
  P := ObjOf(RootAt(TempTop - 2))^.Shape^.Proto;
  D := 1;
  while IsObject(P) do
  begin
    PushRoot(P);
    K := OwnKeys(ObjOf(P), True);
    if OwnKeysFailed then begin OwnKeysFailed := False; PopRoots(3); Exit(False); end;
    N := ArrayLen(ObjOf(K));
    if N > 0 then
    begin
      PushRoot(K);
      for I := 0 to N - 1 do
      begin
        Key := Atomize(StrOf(ObjOf(RootAt(TempTop - 1))^.Elements^.Data[I]));
        Shadow := False;
        Q := RootAt(TempTop - 4);
        for J := 0 to D - 1 do
        begin
          if ForInOwnHas(ObjOf(Q), Key) then begin Shadow := True; Break; end;
          Q := ObjOf(Q)^.Shape^.Proto;
        end;
        if not Shadow then ArrayPush(ObjOf(RootAt(TempTop - 3)), ObjOf(RootAt(TempTop - 1))^.Elements^.Data[I]);
      end;
      PopRoots(1);
    end;
    P := ObjOf(RootAt(TempTop - 1))^.Shape^.Proto;
    PopRoots(1);
    Inc(D);
  end;
  It := NewArrayIterator(RootAt(TempTop - 1), 0);
  Obj := RootAt(TempTop - 2);
  PopRoots(2);
  Result := True;
end;

{ ---------------- generators ---------------- }

threadvar
  GenNextNative: Pointer;

function GeneratorNextIsIntrinsic(Gen: TValue): Boolean;
var F: TValue;
begin
  Result := False;
  if ShapeLookup(ObjOf(Gen)^.Shape, AtomNext) >= 0 then Exit;
  if not GetFromObject(ObjOf(Gen), AtomNext, Gen, F) then begin Pending := VUndefined; Exit; end;
  Result := IsObject(F) and (ObjOf(F)^.OKind = okNative) and (ObjOf(F)^.Native = GenNextNative);
end;

{ the newest frame becomes a suspended generator: its registers move into the generator object }
function SuspendNewGenerator: TValue;
var Fr: PFrame; T: PFuncTemplate; State: PValueArray; G: PObj; I, NU: Integer; P: TValue;
begin
  Fr := @Frames[FrameTop - 1];
  T := Fr^.Tpl;
  { the open upvalues of the frame: none, unless a mapped arguments object (10.4.4) opened its parameters' }
  NU := 0;
  while (OpenCount - 1 - NU >= 0) and (PUpval(PtrOf(OpenUpvals[OpenCount - 1 - NU]))^.StackIndex >= Fr^.Base) do Inc(NU);
  State := NewValueArray(T^.MaxRegs + 4 + NU * 2);
  State^.Len := T^.MaxRegs + 4 + NU * 2;
  Fr := @Frames[FrameTop - 1];
  Move(VStack[Fr^.Base], State^.Data[0], T^.MaxRegs * SizeOf(TValue));
  State^.Data[T^.MaxRegs] := MkInt(-1);
  State^.Data[T^.MaxRegs + 1] := MkInt(T^.MaxRegs);
  State^.Data[T^.MaxRegs + 2] := MkInt(0);
  State^.Data[T^.MaxRegs + 3] := MkInt(NU);
  for I := 0 to NU - 1 do
  begin
    State^.Data[T^.MaxRegs + 4 + I * 2] := OpenUpvals[OpenCount - NU + I];
    State^.Data[T^.MaxRegs + 5 + I * 2] := MkInt(PUpval(PtrOf(OpenUpvals[OpenCount - NU + I]))^.StackIndex - Fr^.Base);
  end;
  if (NU > 0) and ((State^.Hdr.Flags and (cfOld or cfRemembered)) = cfOld) then Remember(@State^.Hdr);
  PushRoot(MkPtr(State));
  { 27.5.3.1 OrdinaryCreateFromConstructor(functionObject, "%GeneratorPrototype%"): the function's prototype
    property (a data property: generator functions' is not configurable) }
  P := FunctionPrototypeOf(Frames[FrameTop - 1].Closure);
  if not IsObject(P) then
    if T^.IsAsync then P := AsyncGeneratorProto else P := GeneratorProto;
  PushRoot(P);
  G := NewObject(RootShapeFor(RootAt(TempTop - 1)), 0, okGenerator);
  PopRoots(1);
  Fr := @Frames[FrameTop - 1];
  G^.Aux1 := Fr^.Closure;
  G^.Aux2 := RootAt(TempTop - 1);
  G^.Pad := 0;
  G^.OFlags := 0;
  PopRoots(1);
  CloseUpvals(Fr^.Base);
  Dec(FrameTop);
  Result := MkPtr(G);
end;

{ Mode: 0 next, 1 throw, 2 return }
function GeneratorResume(Gen: TValue; Arg: TValue; Mode: Integer; var Value: TValue; var Done: Boolean): Boolean;
var G: PObj; State: PValueArray; T: PFuncTemplate; Base, I, NH, NU, J, ResumeReg, SavedTop, NLive: Integer; Fr: PFrame;
  U: PUpval;
  Res: TValue; Ok: Boolean;
begin
  if not IsObject(Gen) or not (ObjOf(Gen)^.OKind in [okGenerator, okAsyncGen]) then Exit(ThrowTypeError('next method called on incompatible receiver'));
  G := ObjOf(Gen);
  case G^.OFlags of
    2: Exit(ThrowTypeError('Generator is already running'));
    3:
      begin
        Done := True;
        if Mode = 1 then begin Pending := Arg; Exit(False); end;
        if Mode = 2 then Value := Arg else Value := VUndefined;
        Exit(True);
      end;
  end;
  if (G^.OFlags = 0) and (Mode <> 0) then
  begin
    G^.OFlags := 3;
    Done := True;
    if Mode = 1 then begin Pending := Arg; Exit(False); end;
    Value := Arg;
    Exit(True);
  end;
  T := PFuncTemplate(ObjOf(G^.Aux1)^.Native);
  State := PValueArray(PtrOf(G^.Aux2));
  SavedTop := StackTop;
  Base := StackTop;
  if (Base + T^.MaxRegs + 4 >= STACK_MAX) or (FrameTop >= FRAME_MAX - 2) then
    Exit(ThrowError('RangeError', 'Maximum call stack size exceeded'));
  { the registers live at the suspension; the others read as a fresh frame's (and the collector, which scans
    the stack, sees no stale value in them) }
  NLive := IntOf(State^.Data[T^.MaxRegs + 1]);
  Move(State^.Data[0], VStack[Base], NLive * SizeOf(TValue));
  if NLive < T^.MaxRegs then FillQWord(VStack[Base + NLive], T^.MaxRegs - NLive, VUndefined);
  ResumeReg := IntOf(State^.Data[T^.MaxRegs]);
  Fr := @Frames[FrameTop];
  Fr^.Closure := G^.Aux1;
  Fr^.NewTarget := Gen;
  Fr^.Tpl := T;
  Fr^.Base := Base;
  Fr^.PC := G^.Pad;
  Fr^.RetIdx := Base;
  Fr^.Argc := 0;
  Fr^.Kind := 3;
  Inc(FrameTop);
  NH := IntOf(State^.Data[T^.MaxRegs + 2]);
  for I := 0 to NH - 1 do
  begin
    if HandlerTop >= Length(Handlers) then SetLength(Handlers, HandlerTop * 2 + 64);
    Handlers[HandlerTop].Frame := FrameTop - 1;
    Handlers[HandlerTop].PC := IntOf(State^.Data[T^.MaxRegs + 3 + I * 2]) and $0FFFFFFF;
    Handlers[HandlerTop].Kind := IntOf(State^.Data[T^.MaxRegs + 3 + I * 2]) shr 28;
    Handlers[HandlerTop].ExcIdx := Base + IntOf(State^.Data[T^.MaxRegs + 4 + I * 2]);
    Inc(HandlerTop);
  end;
  { reopen the frame's upvalues on the new registers; a closure may have changed a variable meanwhile: its cell
    has the current value }
  NU := IntOf(State^.Data[T^.MaxRegs + 3 + NH * 2]);
  for I := 0 to NU - 1 do
  begin
    U := PUpval(PtrOf(State^.Data[T^.MaxRegs + 4 + NH * 2 + I * 2]));
    J := IntOf(State^.Data[T^.MaxRegs + 5 + NH * 2 + I * 2]);
    if U^.StackIndex < 0 then
    begin
      VStack[Base + J] := U^.Value;
      U^.StackIndex := Base + J;
      if OpenCount >= Length(OpenUpvals) then SetLength(OpenUpvals, OpenCount * 2 + 64);
      OpenUpvals[OpenCount] := MkPtr(U);
      Inc(OpenCount);
    end;
  end;
  { the received value after the reopening: a captured register (y = yield x with y captured) takes it }
  if ResumeReg <= -2 then
  begin
    { suspended in yield*: the step op gets the received value and mode and runs again }
    VStack[Base - ResumeReg - 2] := Arg;
    VStack[Base - ResumeReg - 1] := MkInt(Mode);
    Mode := 0;
  end
  else if (ResumeReg >= 0) and (Mode = 0) then VStack[Base + ResumeReg] := Arg;
  StackTop := Base + T^.MaxRegs;
  G^.OFlags := 2;
  if Mode = 1 then
  begin
    Pending := Arg;
    InjectThrow := True;
  end
  else if Mode = 2 then
  begin
    { 27.5.3.4 GeneratorResumeAbrupt: a return completion at the yield, through the finally blocks }
    PushRoot(Gen);
    Pending := NewRetComp(Arg);
    Gen := RootAt(TempTop - 1);
    PopRoots(1);
    InjectThrow := True;
  end;
  GenYielded := False;
  { the generator may move while it runs: read it back from a root afterwards }
  PushRoot(Gen);
  Ok := Execute(FrameTop - 1, Res);
  Gen := RootAt(TempTop - 1);
  PopRoots(1);
  StackTop := SavedTop;
  G := ObjOf(Gen);
  if not Ok then
  begin
    G^.OFlags := 3;
    if IsRetComp(Pending) then
    begin
      Value := ObjOf(Pending)^.Aux1;
      Pending := VUndefined;
      Done := True;
      GenYieldRaw := False;
      Exit(True);
    end;
    Exit(False);
  end;
  Value := Res;
  if GenYielded then
  begin
    GenYielded := False;
    Done := False;
  end
  else
  begin
    G^.OFlags := 3;
    Done := True;
  end;
  Result := True;
end;

{ ---- generators resumed by for-of inside Execute (frame Kind 4) ----
  A for-of step over a suspended generator whose next is the intrinsic one pushes the generator's frame above
  the consumer's instead of a nested Execute: a yield saves it and hands the value to the consumer's register
  (RetIdx); its completion marks it done and takes the consumer's opIterNext done jump; an exception through
  it marks it done and leaves the consumer's iterator done (an abrupt next() is not closed). }

{ the generator's frame, restored as GeneratorResume restores it for next(undefined) }
{ FastLoop's opGetProp hit on its first IC entry: a data property, own or on a prototype the entry vouches for.
  Out of line so it gets registers of its own (FastLoop's locals live in memory); False leaves the get to the
  general path, which checks the same entry again. }
function ICGetHit(O: PObj; IC: PPropIC; Dst: PValue): Boolean; noinline;
var E: PICEntry; O2: PObj; J, Slot: Integer;
begin
  E := @IC^.Poly0;
  if E^.Shape <> O^.Shape then
  begin
    { a polymorphic site: its other entries, here too (FastLoop searches them with its values in memory) }
    E := nil;
    for J := 1 to IC^.PolyCount - 1 do
      if IC^.More^[J].Shape = O^.Shape then
      begin
        E := @IC^.More^[J];
        Break;
      end;
    if E = nil then Exit(False);
  end;
  { the commonest hit, an own data property: Kind, Depth, VerMask and Acc (four adjacent bytes) are the
    receiver's kind, 0, 0 and 0, one 32-bit compare }
  if PCardinal(@E^.Kind)^ = O^.OKind then
  begin
    Slot := E^.Slot;
    if Slot < O^.InlineCap then Dst^ := O^.Slots[Slot]
    else Dst^ := O^.Overflow^.Data[Slot - O^.InlineCap];
    Exit(True);
  end;
  if (E^.Kind <> O^.OKind) or (E^.Acc <> 0) then Exit(False);
  O2 := O;
  for J := 0 to E^.Depth - 1 do
  begin
    if ((E^.VerMask shr J) and 1 <> 0) and (O2^.Shape^.Version <> E^.Vers[J]) then Exit(False);
    O2 := PObj(PtrUInt(O2^.Shape^.Proto and PTR_MASK));
    if O2^.Shape <> E^.Holder[J] then Exit(False);
  end;
  Slot := E^.Slot;
  if Slot < O2^.InlineCap then Dst^ := O2^.Slots[Slot]
  else Dst^ := O2^.Overflow^.Data[Slot - O2^.InlineCap];
  Result := True;
end;

{ FastLoop's instanceof walk (out of line for registers, as ICGetHit): 1 when Target (an object) is on O's
  prototype chain, 0 when the chain ends first (a prototype is an object or null), 2 at a proxy (Execute's) }
function FastProtoChainHas(O: PObj; Target: TValue): Integer; noinline;
var P: TValue;
begin
  while True do
  begin
    if O^.OKind = okProxy then Exit(2);
    P := O^.Shape^.Proto;
    if P = Target then Exit(1);
    if (P shr 48) <> TAG_PTR_HI then Exit(0);
    O := PObj(PtrUInt(P and PTR_MASK));
  end;
end;

function XGenInline(Gen: TValue; RetIdx: Integer): Boolean; noinline;
var G: PObj; State: PValueArray; T: PFuncTemplate; Base, I, NH, NU, J, ResumeReg, NLive: Integer; Fr: PFrame;
  U: PUpval;
begin
  G := ObjOf(Gen);
  T := PFuncTemplate(ObjOf(G^.Aux1)^.Native);
  State := PValueArray(PtrOf(G^.Aux2));
  Base := StackTop;
  if (Base + T^.MaxRegs + 4 >= STACK_MAX) or (FrameTop >= FRAME_MAX - 2) then
    Exit(ThrowError('RangeError', 'Maximum call stack size exceeded'));
  NLive := IntOf(State^.Data[T^.MaxRegs + 1]);
  Move(State^.Data[0], VStack[Base], NLive * SizeOf(TValue));
  if NLive < T^.MaxRegs then FillQWord(VStack[Base + NLive], T^.MaxRegs - NLive, VUndefined);
  ResumeReg := IntOf(State^.Data[T^.MaxRegs]);
  Fr := @Frames[FrameTop];
  Fr^.Closure := G^.Aux1;
  Fr^.NewTarget := Gen;
  Fr^.Tpl := T;
  Fr^.Base := Base;
  Fr^.PC := G^.Pad;
  Fr^.RetIdx := RetIdx;
  Fr^.Argc := 0;
  Fr^.Kind := 4;
  Inc(FrameTop);
  NH := IntOf(State^.Data[T^.MaxRegs + 2]);
  for I := 0 to NH - 1 do
  begin
    if HandlerTop >= Length(Handlers) then SetLength(Handlers, HandlerTop * 2 + 64);
    Handlers[HandlerTop].Frame := FrameTop - 1;
    Handlers[HandlerTop].PC := IntOf(State^.Data[T^.MaxRegs + 3 + I * 2]) and $0FFFFFFF;
    Handlers[HandlerTop].Kind := IntOf(State^.Data[T^.MaxRegs + 3 + I * 2]) shr 28;
    Handlers[HandlerTop].ExcIdx := Base + IntOf(State^.Data[T^.MaxRegs + 4 + I * 2]);
    Inc(HandlerTop);
  end;
  NU := IntOf(State^.Data[T^.MaxRegs + 3 + NH * 2]);
  for I := 0 to NU - 1 do
  begin
    U := PUpval(PtrOf(State^.Data[T^.MaxRegs + 4 + NH * 2 + I * 2]));
    J := IntOf(State^.Data[T^.MaxRegs + 5 + NH * 2 + I * 2]);
    if U^.StackIndex < 0 then
    begin
      VStack[Base + J] := U^.Value;
      U^.StackIndex := Base + J;
      if OpenCount >= Length(OpenUpvals) then SetLength(OpenUpvals, OpenCount * 2 + 64);
      OpenUpvals[OpenCount] := MkPtr(U);
      Inc(OpenCount);
    end;
  end;
  { next() with undefined, after the reopening (as GeneratorResume) }
  if ResumeReg <= -2 then
  begin
    VStack[Base - ResumeReg - 2] := VUndefined;
    VStack[Base - ResumeReg - 1] := MkInt(0);
  end
  else if ResumeReg >= 0 then VStack[Base + ResumeReg] := VUndefined;
  StackTop := Base + T^.MaxRegs;
  G^.OFlags := 2;
  GenYielded := False;
  Result := True;
end;

{ the consumer below a Kind 4 frame that is gone: its iterator register (opIterNext's B) done; Jump: also take
  that instruction's done jump }
procedure XGenConsumerDone(Jump: Boolean);
var CF: PFrame; Ins: QWord;
begin
  CF := @Frames[FrameTop - 1];
  Ins := CF^.Tpl^.Code[CF^.PC - 1];
  VStack[CF^.Base + Integer((Ins shr 24) and $FFFF)] := VUndefined;
  if Jump then Inc(CF^.PC, SarInt64(Int64(Ins), 40));
end;

{ a Kind 4 frame returned (the generator completed): it is done, the consumer's loop ends }
procedure XGenInlineReturn(Fr: PFrame); noinline;
begin
  ObjOf(Fr^.NewTarget)^.OFlags := 3;
  CloseUpvals(Fr^.Base);
  while (HandlerTop > 0) and (Handlers[HandlerTop - 1].Frame >= FrameTop - 1) do Dec(HandlerTop);
  Dec(FrameTop);
  XGenConsumerDone(True);
end;

{ an exception unwinds the frame at FrameTop - 1 (its upvalues closed by the caller): a Kind 4 one leaves its
  generator done and its consumer's iterator done }
procedure XGenUnwind; noinline;
begin
  ObjOf(Frames[FrameTop - 1].NewTarget)^.OFlags := 3;
  Dec(FrameTop);
  XGenConsumerDone(False);
  Inc(FrameTop);
end;

{ runs the instance field initializer of a class constructor on This, if it has one }
function RunFieldInit(Ctor, This: TValue): Boolean;
var CI: Integer; V: TValue;
begin
  CI := ShapeLookup(ObjOf(Ctor)^.Shape, AtomFieldInit);
  if CI < 0 then Exit(True);
  Result := CallValue(GetSlot(ObjOf(Ctor), CI), This, nil, 0, V);
end;

function DescribeValue(V: TValue): UnicodeString;
var M: TValue; S: Integer;
begin
  if IsObject(V) then
  begin
    S := ShapeLookup(ObjOf(V)^.Shape, AtomMessage);
    if S >= 0 then
    begin
      M := GetSlot(ObjOf(V), S);
      if IsString(M) then Exit(StrToUnicode(StrOf(M)));
    end;
    Exit('[object]');
  end;
  if IsString(V) then Exit(StrToUnicode(StrOf(V)));
  Result := TypeOfStr(V);
end;

{ for a failed call: the property or variable the callee register was loaded from, when the previous
  instruction loaded it }
function CalleeDescription(T: PFuncTemplate; CallPC, Reg: Integer): UnicodeString;
var P: Integer; I: QWord;
begin
  Result := 'value';
  for P := CallPC - 1 downto Max(0, CallPC - 64) do
  begin
    I := T^.Code[P];
    if ((I shr 8) and $FFFF) <> QWord(Reg) then Continue;
    case TOp(I and $FF) of
      opGetProp, opGetLength, opGetGlobal, opGetMethod: Exit(StrToUnicode(T^.ICs[I shr 40].Key));
      opGetSuper: if SarInt64(Int64(I), 40) >= 0 then Exit('super.' + StrToUnicode(T^.ICs[I shr 40].Key));
    end;
    Exit;
  end;
end;

{ ---------------- private elements (7.3.31 PrivateGet, 7.3.32 PrivateSet) ---------------- }

function PrivateGet(Obj: TValue; Key: PStr; var V: TValue): Boolean;
var O: PObj; CI: Integer; A: PAccessor;
begin
  if not IsObject(Obj) then Exit(ThrowTypeError('Cannot read private member ' + StrToUnicode(Key) + ' from a primitive'));
  O := ObjOf(Obj);
  CI := ShapeLookup(O^.Shape, Key);
  if CI < 0 then
    Exit(ThrowTypeError('Cannot read private member ' + StrToUnicode(Key) + ' from an object whose class did not declare it'));
  if (O^.Shape^.PFlags[CI] and pfAccessor) = 0 then begin V := GetSlot(O, CI); Exit(True); end;
  A := PAccessor(PtrOf(GetSlot(O, CI)));
  if not IsCallable(A^.Getter) then Exit(ThrowTypeError('''' + StrToUnicode(Key) + ''' was defined without a getter'));
  Result := CallValue(A^.Getter, Obj, nil, 0, V);
end;

function PrivateSet(Obj: TValue; Key: PStr; V: TValue): Boolean;
var O: PObj; CI: Integer; F: Byte; A: PAccessor; R: TValue;
begin
  if not IsObject(Obj) then Exit(ThrowTypeError('Cannot write private member ' + StrToUnicode(Key) + ' to a primitive'));
  O := ObjOf(Obj);
  CI := ShapeLookup(O^.Shape, Key);
  if CI < 0 then
    Exit(ThrowTypeError('Cannot write private member ' + StrToUnicode(Key) + ' to an object whose class did not declare it'));
  F := O^.Shape^.PFlags[CI];
  if (F and pfAccessor) <> 0 then
  begin
    A := PAccessor(PtrOf(GetSlot(O, CI)));
    if not IsCallable(A^.Setter) then Exit(ThrowTypeError('''' + StrToUnicode(Key) + ''' was defined without a setter'));
    Exit(CallValue(A^.Setter, Obj, @V, 1, R));
  end;
  if (F and pfWritable) = 0 then Exit(ThrowTypeError('Private method ' + StrToUnicode(Key) + ' is not writable'));
  SetSlot(O, CI, V);
  Result := True;
end;

{ 7.3.7 CreateDataPropertyOrThrow for class fields }
function CreateDataPropOrThrow(Obj: TValue; Key: PStr; V: TValue): Boolean;
var O: PObj; CI: Integer; Idx: Int64;
begin
  O := ObjOf(Obj);
  if (O^.OKind = okArray) and IndexFromString(Key, Idx) then Exit(SetElemV(Obj, MkPtr(Key), V));
  if O^.OKind in [okProxy, okModuleNS, okTypedArray, okArguments, okStringObj] then Exit(CreateDataHook(Obj, Key, V));
  CI := ShapeLookup(O^.Shape, Key);
  if CI >= 0 then
  begin
    if (O^.Shape^.PFlags[CI] and pfConfigurable) = 0 then
      Exit(ThrowTypeError('Cannot redefine property: ' + StrToUnicode(Key)));
  end
  else if (O^.OFlags and 8) <> 0 then
    Exit(ThrowTypeError('Cannot define property ' + StrToUnicode(Key) + ', object is not extensible'));
  DefineOwn(Obj, Key, V, pfDefault);
  Result := True;
end;

{ 7.2.4 IsConstructor }
function IsConstructorV(V: TValue): Boolean;
var O: PObj; T: PFuncTemplate;
begin
  if not IsCallable(V) then Exit(False);
  O := ObjOf(V);
  case O^.OKind of
    okFunction:
      begin
        T := PFuncTemplate(O^.Native);
        Result := not (T^.IsArrow or (T^.IsMethod and not T^.IsClassCtor) or T^.IsGenerator or T^.IsAsync);
      end;
    okBound: Result := IsConstructorV(O^.Aux1);
    okProxy: Result := IsConstructorV(O^.Aux1);
    okNative: Result := (O^.OFlags and 32) <> 0;
  else
    Result := False;
  end;
end;

{ the running generator frame (the newest) suspends: its live registers (Live, from the suspending instruction's C;
  0 for all of them), the resume code (>= 0 a register for the resumption value, <= -2 a yield* step's received
  value / mode registers) and its handlers go into its generator object; it resumes at ResumePC. The state array
  holds the registers, then the resume code, the live register count, the handlers and the open upvalues. }
procedure SaveSuspension(Fr: PFrame; Tpl: PFuncTemplate; R: PValue; ResumeCode, ResumePC, Live: Integer);
var NP, NU, J, Off, N: Integer; O: PObj; E: PValueArray;
begin
  if (Live <= 0) or (Live > Tpl^.MaxRegs) then Live := Tpl^.MaxRegs;
  NP := 0;
  while (HandlerTop - 1 - NP >= 0) and (Handlers[HandlerTop - 1 - NP].Frame = FrameTop - 1) do
  begin
    if Handlers[HandlerTop - 1 - NP].ExcIdx - Fr^.Base >= Live then Live := Handlers[HandlerTop - 1 - NP].ExcIdx - Fr^.Base + 1;
    Inc(NP);
  end;
  { the frame's open upvalues (closures made in it): closed now, reopened on its registers when it resumes, so
    the closures and the frame keep sharing the variables }
  NU := 0;
  while (OpenCount - 1 - NU >= 0) and (PUpval(PtrOf(OpenUpvals[OpenCount - 1 - NU]))^.StackIndex >= Fr^.Base) do Inc(NU);
  if ResumeCode >= Live then Live := ResumeCode + 1
  else if (ResumeCode <= -2) and (-ResumeCode > Live) then Live := -ResumeCode;
  if Live > Tpl^.MaxRegs then Live := Tpl^.MaxRegs;
  N := Tpl^.MaxRegs + 4 + NP * 2 + NU * 2;
  O := ObjOf(Fr^.NewTarget);
  E := PValueArray(PtrOf(O^.Aux2));
  if Integer(E^.Cap) < N then
  begin
    E := NewValueArray(N);
    O := ObjOf(Fr^.NewTarget);
    O^.Aux2 := MkPtr(E);
    WriteBarrier(@O^.Hdr, O^.Aux2);
  end;
  E^.Len := N;
  Move(R^, E^.Data[0], Live * SizeOf(TValue));
  E^.Data[Tpl^.MaxRegs] := MkInt(ResumeCode);
  E^.Data[Tpl^.MaxRegs + 1] := MkInt(Live);
  E^.Data[Tpl^.MaxRegs + 2] := MkInt(NP);
  for J := 0 to NP - 1 do
  begin
    E^.Data[Tpl^.MaxRegs + 3 + J * 2] := MkInt(Handlers[HandlerTop - NP + J].PC or (Handlers[HandlerTop - NP + J].Kind shl 28));
    E^.Data[Tpl^.MaxRegs + 4 + J * 2] := MkInt(Handlers[HandlerTop - NP + J].ExcIdx - Fr^.Base);
  end;
  Off := Tpl^.MaxRegs + 3 + NP * 2;
  E^.Data[Off] := MkInt(NU);
  for J := 0 to NU - 1 do
  begin
    { a captured variable outside the live registers would not come back: keep its register too }
    if PUpval(PtrOf(OpenUpvals[OpenCount - NU + J]))^.StackIndex - Fr^.Base >= Live then
    begin
      Move(R[Live], E^.Data[Live], (PUpval(PtrOf(OpenUpvals[OpenCount - NU + J]))^.StackIndex - Fr^.Base + 1 - Live) * SizeOf(TValue));
      Live := PUpval(PtrOf(OpenUpvals[OpenCount - NU + J]))^.StackIndex - Fr^.Base + 1;
      E^.Data[Tpl^.MaxRegs + 1] := MkInt(Live);
    end;
    E^.Data[Off + 1 + J * 2] := OpenUpvals[OpenCount - NU + J];
    E^.Data[Off + 2 + J * 2] := MkInt(PUpval(PtrOf(OpenUpvals[OpenCount - NU + J]))^.StackIndex - Fr^.Base);
  end;
  if (E^.Hdr.Flags and (cfOld or cfRemembered)) = cfOld then Remember(@E^.Hdr);
  Dec(HandlerTop, NP);
  O^.Pad := ResumePC;
  O^.OFlags := 1;
  CloseUpvals(Fr^.Base);
  Dec(FrameTop);
  GenYielded := True;
end;

{ SaveSuspension can collect (it may grow the state array): V, the value the suspension hands out, stays rooted }
procedure SuspendKeep(Fr: PFrame; Tpl: PFuncTemplate; R: PValue; ResumeCode, ResumePC, Live: Integer; var V: TValue);
begin
  PushRoot(V);
  SaveSuspension(Fr, Tpl, R, ResumeCode, ResumePC, Live);
  V := RootAt(TempTop - 1);
  PopRoots(1);
end;

{ ---- Execute's cold paths: their strings out of its frame (managed temporaries would be initialized and
  finalized, under an exception frame, on every entry to Execute) ---- }

{ opImport: the import call's referrer is the active script or module, the caller's for eval code and dynamic
  functions (19.2.1.1 step 23: an eval context's ScriptOrModule is its caller's) }
function XImport(Tpl: PFuncTemplate; Spec, Opts: TValue; var V: TValue): Boolean; noinline;
var J: Integer; T: PFuncTemplate;
begin
  if not Assigned(ImportHook) then Exit(ThrowError('SyntaxError', 'import() is not supported by this host'));
  T := Tpl;
  J := FrameTop - 2;
  while (T^.SourceName <> '') and (T^.SourceName[1] = '<') and (J >= 0) do
  begin
    if Frames[J].Tpl <> nil then T := Frames[J].Tpl;
    Dec(J);
  end;
  if (T^.SourceName <> '') and (T^.SourceName[1] = '<') then T := Tpl;
  Result := ImportHook(T^.SourceName, T^.ModuleId, Spec, Opts, V);
end;

{ 9.1.1.2.6 GetBindingValue of an object environment: the property may have gone since HasBinding (undefined,
  or a ReferenceError for a strict reference); C's bit 23 (I's bit 63) the reference's strictness }
function XWithGet(Obj: TValue; Key: PStr; Strict: Boolean; var V: TValue): Boolean; noinline;
var Found: Boolean;
begin
  PushRoot(Obj);
  Result := HasPropertyV(Obj, Key, Found);
  Obj := RootAt(TempTop - 1);
  PopRoots(1);
  if not Result then Exit;
  if not Found then
  begin
    if Strict then Exit(ThrowError('ReferenceError', StrToUnicode(Key) + ' is not defined'));
    V := VUndefined;
    Exit(True);
  end;
  Result := GetV(Obj, Key, V);
end;

{ opWithSet. Mode (I's bits 63 and 62): 0 a plain Set (a var's initialization in an eval variable environment);
  2 / 3 9.1.1.2.5 SetMutableBinding of an object environment, sloppy / strict: the property may have gone since
  HasBinding (a ReferenceError for a strict reference), then Set(O, N, V, S) }
function XWithSet(Obj: TValue; Key: PStr; V: TValue; Mode: Integer): Boolean; noinline;
var Found: Boolean;
begin
  if Mode = 0 then Exit(SetVS(Obj, Key, V, False));
  PushRoot(Obj); PushRoot(V);
  Result := HasPropertyV(Obj, Key, Found);
  V := RootAt(TempTop - 1); Obj := RootAt(TempTop - 2);
  PopRoots(2);
  if not Result then Exit;
  if not Found and (Mode = 3) then Exit(ThrowError('ReferenceError', StrToUnicode(Key) + ' is not defined'));
  Result := SetVS(Obj, Key, V, Mode = 3);
end;

function XThrowNotDefined(Key: PStr): Boolean; noinline;
begin
  Result := ThrowError('ReferenceError', StrToUnicode(Key) + ' is not defined');
end;

function XPrivateKey(Name: PStr): PStr; noinline;
begin
  Result := NewKeyAtom(StrToUnicode(Name), kfPrivate);
end;

threadvar
  PreludePrivAt: array of UnicodeString;   { the opNewPrivName instructions of prelude code, and their names }
  PreludePrivKey: array of PStr;
  PreludePrivCount: Integer;

{ a private name of a prelude's class: one per instruction for all of this agent's realms, so a built-in's internal
  slots (private fields) are the same slots whichever realm's methods check them (an iterator helper's next from
  another realm, say). The prelude's classes are evaluated once per realm. }
function XPrivateKeyAt(Tpl: PFuncTemplate; PC: Integer; Name: PStr): PStr; noinline;
var SN, At: UnicodeString; I: Integer;
begin
  SN := Tpl^.SourceName;
  if (SN = '') or (SN[1] <> '<') or (SN = '<eval>') or (SN = '<Function>') or (SN = '<ShadowRealm>') then
    Exit(XPrivateKey(Name));
  { a realm runs its own copy of the prelude's templates: the instruction is named by the prelude, the function
    and its index }
  At := SN + '/';
  if Tpl^.Name <> nil then At := At + StrToUnicode(Tpl^.Name);
  At := At + '/' + IntToStr(PC) + '/' + StrToUnicode(Name);
  for I := 0 to PreludePrivCount - 1 do
    if PreludePrivAt[I] = At then Exit(PreludePrivKey[I]);
  Result := XPrivateKey(Name);
  if PreludePrivCount >= Length(PreludePrivAt) then
  begin
    SetLength(PreludePrivAt, PreludePrivCount * 2 + 32);
    SetLength(PreludePrivKey, PreludePrivCount * 2 + 32);
  end;
  PreludePrivAt[PreludePrivCount] := At;
  PreludePrivKey[PreludePrivCount] := Result;
  Inc(PreludePrivCount);
end;

function XThrowInitTwice(Key: PStr): Boolean; noinline;
begin
  Result := ThrowTypeError('Cannot initialize ' + StrToUnicode(Key) + ' twice on the same object');
end;

function XThrowCannotDelete(Key: PStr): Boolean; noinline;
begin
  Result := ThrowTypeError('Cannot delete property ''' + StrToUnicode(Key) + ''' of object');
end;

function XThrowNotFunction(Tpl: PFuncTemplate; PC, A: Integer): Boolean; noinline;
begin
  Result := ThrowTypeError(CalleeDescription(Tpl, PC, A) + ' is not a function');
end;

function XThrowNotCtor(T: PFuncTemplate): Boolean; noinline;
begin
  Result := ThrowTypeError(StrToUnicode(T^.Name) + ' is not a constructor');
end;

function XThrowTDZ(Key: PStr): Boolean; noinline;
begin
  Result := ThrowError('ReferenceError', 'Cannot access ''' + StrToUnicode(Key) + ''' before initialization');
end;

function XThrowReadOnlySuper(Key: PStr): Boolean; noinline;
begin
  Result := ThrowTypeError('Cannot assign to read only property ''' + StrToUnicode(Key) + ''' through super');
end;

function XThrowUnimplemented(Op: Integer): Boolean; noinline;
begin
  Result := ThrowError('Error', 'unimplemented opcode ' + IntToStr(Op));
end;

procedure XReportRootImbalance(O: PObj; Delta: Integer; Ok: Boolean; Tpl: PFuncTemplate); noinline;
begin
  if IsString(O^.Aux1) then WriteLn(ErrOutput, 'root imbalance ', Delta, ' after native ',
    UTF8Encode(StrToUnicode(StrOf(O^.Aux1))), ' ok=', Ok, ' in ', DescribeTemplate(Tpl))
  else WriteLn(ErrOutput, 'root imbalance ', Delta, ' after an unnamed native ok=', Ok, ' in ', DescribeTemplate(Tpl));
end;

procedure XReportBadUps(J, GcBefore, Reg, Base: Integer; Tpl: PFuncTemplate); noinline;
begin
  WriteLn(ErrOutput, 'opClosure: ups went bad at J=', J, ' gc before ', GcBefore, ' after ', GCMinorCount,
    ' StackTop ', StackTop, ' reg ', Reg, ' base ', Base, ' maxregs ', Tpl^.MaxRegs, ' frames ', FrameTop,
    ' fn ', DescribeTemplate(Tpl));
end;

procedure XTraceThrow; noinline;
var Fr: PFrame; CI: Integer;
begin
  Fr := @Frames[FrameTop - 1];
  if (Fr^.PC > 0) and (Fr^.PC - 1 < Length(Fr^.Tpl^.Lines)) then
  begin
    WriteLn(ErrOutput, 'throw in ', UTF8Encode(StrToUnicode(Fr^.Tpl^.Name)), ' line ', Fr^.Tpl^.Lines[Fr^.PC - 1], ': ',
      UTF8Encode(DescribeValue(Pending)), ' [op ', Fr^.Tpl^.Code[Fr^.PC - 1] and $FF, ' pc ', Fr^.PC - 1, ']');
    if GetEnvironmentVariable('G2_DUMPIC') <> '' then
      for CI := 0 to High(Fr^.Tpl^.ICs) do
        if Fr^.Tpl^.ICs[CI].Key <> nil then Write(ErrOutput, ' ', CI, ':', UTF8Encode(StrToUnicode(Fr^.Tpl^.ICs[CI].Key)));
    if GetEnvironmentVariable('G2_DUMPIC') <> '' then WriteLn(ErrOutput);
  end;
end;

function XBigAddInt(V: TValue; N: Integer): TValue; noinline;
begin
  Result := BigToValue(ValueToBig(V).Add(TBigInteger.FromInt64(N)));
end;

{ opGetSuper: 13.3.7.3 GetSuperBase, then GetValue: ToObject of the base (null throws), then the key's
  ToPropertyKey (CI < 0: KeyV; else the IC's key), then [[Get]] with This as the receiver }
function XGetSuper(Home, KeyV: TValue; CI: Integer; Tpl: PFuncTemplate; This: TValue; var Res: TValue): Boolean; noinline;
var Base: TValue; Key: PStr; Ok: Boolean;
begin
  if IsObject(Home) then Base := ObjOf(Home)^.Shape^.Proto else Base := VUndefined;
  if not IsObject(Base) then
    Exit(ThrowTypeError('Cannot read a super property: the home object''s prototype is not an object'));
  if CI < 0 then
  begin
    PushRoot(Base); PushRoot(This);
    Ok := ToPropKey(KeyV, Key);
    This := RootAt(TempTop - 1); Base := RootAt(TempTop - 2);
    PopRoots(2);
    if not Ok then Exit(False);
  end
  else Key := Tpl^.ICs[CI].Key;
  Result := GetFromObject(ObjOf(Base), Key, This, Res);
end;

{ 13.5.1.2 delete of a property reference whose base is not an object: ToObject (null and undefined throw), then
  the key's ToPropertyKey (Key nil: KeyV), then [[Delete]] on the wrapper (a String's indices and length are not
  configurable) }
function XDeletePrim(Base, KeyV: TValue; Key: PStr; Strict: Boolean; var Res: Boolean): Boolean; noinline;
var W: TValue; Ok: Boolean;
begin
  if IsNullish(Base) then Exit(ThrowTypeError('Cannot convert undefined or null to object'));
  PushRoot(KeyV);
  Ok := ToObjectHook(Base, W);
  KeyV := RootAt(TempTop - 1);
  PopRoots(1);
  if not Ok then Exit(False);
  if Key = nil then
  begin
    PushRoot(W);
    Ok := ToPropKey(KeyV, Key);
    W := RootAt(TempTop - 1);
    PopRoots(1);
    if not Ok then Exit(False);
  end;
  Res := DeleteIfConfigurable(ObjOf(W), Key);
  if not Res and Strict then
  begin
    XThrowCannotDelete(Key);
    Exit(False);
  end;
  Result := True;
end;

{ opTemplateObj: Mode 0 Res := the realm's template object for Site (undefined until made); Mode 1 freezes the
  strings array T (and its raw array) and records it for Site (13.2.8.4 GetTemplateObject steps 2-14) }
function XTemplateObj(Mode, Site: Integer; T: TValue; var Res: TValue): Boolean; noinline;
var E: PValueArray;
begin
  Res := VUndefined;
  if Mode = 0 then
  begin
    if IsObject(TemplateCache) then
    begin
      E := ObjOf(TemplateCache)^.Elements;
      if (E <> nil) and (Site < Integer(E^.Len)) and (E^.Data[Site] <> VHole) then Res := E^.Data[Site];
    end;
    Exit(True);
  end;
  PushRoot(T);
  if Assigned(TemplateFreezeHook) and not TemplateFreezeHook(RootAt(TempTop - 1)) then begin PopRoots(1); Exit(False); end;
  if not IsObject(TemplateCache) then TemplateCache := MkPtr(NewArray(0, 16));
  Result := SetElemV(TemplateCache, MkInt(Site), RootAt(TempTop - 1));
  Res := RootAt(TempTop - 1);
  PopRoots(1);
end;

{ the immediate bitwise and shift instructions outside FastLoop: the binary operator with a Number constant }
function XBitImm(Op: TOp; VA: TValue; CI: Int32; var Res: TValue): Boolean; noinline;
const Base: array[opBitAndI..opUShrI] of TOp = (opBitAnd, opBitOr, opBitXor, opShl, opShr, opUShr);
begin
  Result := ArithSlow(Base[Op], VA, MkInt(CI), Res);
end;

{ new F: PrototypeForConstruct, and the prototype property's slot cached when it is an own data property }
function XCtorProto(T: PFuncTemplate; F: TValue; var Proto: TValue): Boolean; noinline;
var S: Integer; O: PObj;
begin
  PushRoot(F);
  Result := PrototypeForConstruct(F, Proto);
  O := ObjOf(RootAt(TempTop - 1));
  PopRoots(1);
  if not Result then Exit;
  S := ShapeLookup(O^.Shape, AtomPrototype);
  if (S >= 0) and ((O^.Shape^.PFlags[S] and pfAccessor) = 0) and not O^.Shape^.IsDict then
  begin
    T^.CtorFnShape := O^.Shape;
    T^.CtorProtoSlot := S;
  end;
end;

{ opNew: the instance field initializer of constructor C as a frame above C's just pushed frame (a native
  or missing one: RunFieldInit now, before the body) }
function XPushFieldInit(C: TValue): Boolean; noinline;
var CI, J: Integer; V: TValue; T: PFuncTemplate;
begin
  T := PFuncTemplate(ObjOf(C)^.Native);
  if Pointer(ObjOf(C)^.Shape) = T^.FIShape then CI := T^.FISlot
  else
  begin
    CI := ShapeLookup(ObjOf(C)^.Shape, AtomFieldInit);
    if not ObjOf(C)^.Shape^.IsDict then
    begin
      T^.FIShape := ObjOf(C)^.Shape;
      T^.FISlot := CI;
    end;
  end;
  if CI < 0 then Exit(True);
  V := GetSlot(ObjOf(C), CI);
  if not IsObject(V) or (ObjOf(V)^.OKind <> okFunction) then
    Exit(RunFieldInit(C, VStack[Frames[FrameTop - 1].Base]));
  J := Frames[FrameTop - 1].Base + Frames[FrameTop - 1].Tpl^.MaxRegs;
  if J < StackTop then J := StackTop;
  Result := PushBytecodeFrame(ObjOf(V), VStack[Frames[FrameTop - 1].Base], @VStack[J + 1], 0, J, 0, VUndefined);
end;

{ 13.3.7.1 SuperCall in the running frame (A its register, the arguments at A + 2, Argc of them) with a bytecode
  parent VA: the parent's [[Construct]] (10.2.2) as Construct's, its frame pushed as Kind 5 instead of run in a
  nested Execute (this from new.target's prototype, unbound for a derived parent; the parent's field
  initializer's frame above it); the Kind 5 return binds this (XSuperReturn). 1 pushed, 0 for Construct (a
  constructor it rejects), -1 a throw pending (from the parent's frame when it was pushed) }
function XSuperPush(VA: TValue; A, Argc: Integer): Integer; noinline;
var Fr: PFrame; T: PFuncTemplate; Proto, NT, This: TValue; ThisO: PObj; Base: Integer;
begin
  Fr := @Frames[FrameTop - 1];
  T := PFuncTemplate(ObjOf(VA)^.Native);
  if T^.IsArrow or (T^.IsMethod and not T^.IsClassCtor) or T^.IsGenerator or T^.IsAsync then Exit(0);
  NT := Fr^.NewTarget;
  Base := Fr^.Base + A + 1;
  if T^.IsDerived then This := VHole
  else
  begin
    PushRoot(VA); PushRoot(NT);
    if not PrototypeForConstruct(NT, Proto) then begin PopRoots(2); Exit(-1); end;
    if not IsObject(Proto) and not DefaultConstructProto(RootAt(TempTop - 2), RootAt(TempTop - 1), Proto) then
    begin
      PopRoots(2);
      Exit(-1);
    end;
    PushRoot(Proto);
    ThisO := NewObject(RootShapeFor(RootAt(TempTop - 1)), Max(4, T^.ExpectedProps), okPlain);
    VA := RootAt(TempTop - 3); NT := RootAt(TempTop - 2);
    PopRoots(3);
    This := MkPtr(ThisO);
  end;
  VStack[Base] := This;
  if not PushBytecodeFrame(ObjOf(VA), This, @VStack[Base + 1], Argc, Base - 1, 5, NT) then Exit(-1);
  { the frame holds the parent: read it there (pushing the frame can collect) }
  if not T^.IsDerived and Assigned(T^.FieldInit) and not XPushFieldInit(Frames[FrameTop - 1].Closure) then Exit(-1);
  Result := 1;
end;

{ a Kind 5 frame (XSuperPush's) returned V, the parent's constructed object; FrameTop is past it, Fr its record:
  13.3.7.1 SuperCall steps 7-11 in the frame that called super, BindThisValue (a second super() throws), the
  result in the call's register, then that constructor's field initializers (as RunFieldInit) }
function XSuperReturn(Fr: PFrame; V: TValue): Boolean; noinline;
var CF: PFrame;
begin
  CF := @Frames[FrameTop - 1];
  if VStack[CF^.Base] <> VHole then Exit(ThrowError('ReferenceError', 'Super constructor may only be called once'));
  VStack[CF^.Base] := V;
  VStack[Fr^.RetIdx] := V;
  Result := XPushFieldInit(CF^.Closure);
end;

{ FastLoop's opSuperCall (A its register, Argc arguments at A + 2, C 1 when the super constructor was read
  into A + 1 before them): XSuperPush's result for a bytecode parent, 0 (Execute's) for anything else }
function XFastSuper(A, Argc, C: Integer; Spread: Boolean): Integer; noinline;
var Fr: PFrame; VA, V: TValue; T: PFuncTemplate; O: PObj; J: Integer;
begin
  if MultiRealmAny then Exit(0);
  Fr := @Frames[FrameTop - 1];
  if C = 1 then VA := VStack[Fr^.Base + A + 1] else VA := ObjOf(Fr^.Closure)^.Shape^.Proto;
  if not IsObject(VA) or (ObjOf(VA)^.OKind <> okFunction) or not IsConstructorV(VA) then Exit(0);
  T := PFuncTemplate(ObjOf(VA)^.Native);
  if T^.IsArrow or (T^.IsMethod and not T^.IsClassCtor) or T^.IsGenerator or T^.IsAsync then Exit(0);
  if Spread then
  begin
    { super(...args): the array in A + 2 spread into the registers from there (as Execute), once nothing can
      send the call back to Execute }
    O := ObjOf(VStack[Fr^.Base + A + 2]);
    Argc := ArrayLen(O);
    if Fr^.Base + A + 2 + Argc + 4 >= STACK_MAX then Exit(0);
    for J := 0 to Argc - 1 do
    begin
      V := O^.Elements^.Data[J];
      if V = VHole then V := VUndefined;
      VStack[Fr^.Base + A + 2 + J] := V;
    end;
  end;
  if StackTop < Fr^.Base + A + 2 + Argc then StackTop := Fr^.Base + A + 2 + Argc;
  Result := XSuperPush(VA, A, Argc);
end;

{ the slow path of the immediate adds: opAddI is binary + (a string concatenates, a BigInt throws), opSubI binary
  - (C is the negated constant), opIncI ++ and -- (ToNumeric, then a BigInt steps by C) }
function XAddImmSlow(Op: TOp; VA: TValue; CI: Integer; var Res: TValue): Boolean; noinline;
begin
  case Op of
    opAddI: Result := AddNumbersSlow(VA, MkInt(CI), Res);
    opSubI: Result := ArithSlow(opSub, VA, MkInt(-CI), Res);
  else
    begin
      if not ToNumericV(VA, VA) then Exit(False);
      if IsBigInt(VA) then Res := XBigAddInt(VA, CI) else Res := MkNumber(NumOf(VA) + CI);
      Result := True;
    end;
  end;
end;

function XBigNegate(V: TValue): TValue; noinline;
begin
  Result := BigToValue(ValueToBig(V).Negate);
end;

{ the instructions FastLoop has a fast path for: Execute calls it only before one of them }
var
  FastOp: array[0..255] of Boolean;
  {$IFDEF G2_BAILSTATS}BailCounts: array[0..255] of Int64; FastOpI: Integer;{$ENDIF}
  {$IFDEF G2_JITSTATS}{$IFNDEF G2_BAILSTATS}FastOpI: Integer;{$ENDIF}{$ENDIF}

procedure InitFastOps;
const
  Ops: array[0..78] of TOp = (opSuperCall, opSuperCallV, opClosure, opNew, opNewObj, opDefProp, opIterNext, opYield, opNop, opMov, opLoadK, opLoadI, opLoadUndef, opLoadNull, opLoadTrue, opLoadFalse,
    opLoadHole, opGetUpv, opSetUpv, opGetProp, opGetLength, opGetMethod, opSetProp, opGetElem, opSetElem, opAdd, opAddI, opSubI, opIncI, opBitAndI, opBitOrI, opBitXorI, opShlI, opShrI, opUShrI, opCheckHole, opGetGlobal, opInstanceof, opSub,
    opMul, opDiv, opMod, opShr, opUShr, opBitAnd, opBitOr, opBitXor, opShl, opNeg, opBitNot, opNot, opSEq, opSNe,
    opLt, opLe, opGt, opGe, opJmp, opJT, opJF, opJNullish, opJNotNullish, opJUndef, opJNotUndef, opJNLt, opJNLe,
    opJNGt, opJNGe, opJNSEq, opJSEq, opCall, opRet, opRetUndef, opToNum, opToNumeric, opEq, opNe, opTypeof, opDefField);
var Op: TOp;
begin
  for Op in Ops do FastOp[Ord(Op)] := True;
end;

{ The hot instructions' fast paths, in a function small enough for FPC to keep its loop variables in registers.
  FPC spills whole live ranges and picks the variable with the most interferences, so in Execute, whose size
  gives the dispatch variables the most, PC, R and Code live in memory: every instruction loads and stores them,
  and PC's store-to-load chain bounds the dispatch rate. FastLoop runs from PC until an instruction it does not
  handle, or whose fast path does not apply, and returns that instruction's PC before doing any of it: Execute
  runs it. Each case is Execute's fast path for the instruction. FastLoop calls little (a call would make the
  loop variables compete for the callee-saved registers), out of line (XFastNative, XFastNativeGetter). Where it
  allocates or calls a native, which can collect, no local holds a cell across it: the registers are roots up to
  StackTop and every instruction reads them afresh. Fr is the running frame (its closure's upvalue array is read through it: a collection in Execute
  can move the array). A store the write barrier would have to remember is left to Execute.
  A call of a plain bytecode function and its return run here too, so a call does not end the fast path.
  FastLoop then returns -1: the frame it stopped in is no longer Execute's, and its PC is in the frame record.
  It never returns from StopFrame, the frame Execute was entered for. }
{ StrEquals without a call, for FastLoop: lengths, atoms, hashes, then the characters }
function FastStrEquals(A, B: PStr): Boolean; inline;
var P, Q: PWideChar; N: Integer;
begin
  if A^.Len <> B^.Len then Exit(False);
  if (A^.Atom <> nil) and (A^.Atom = B^.Atom) then Exit(True);
  if (A^.Hash <> 0) and (B^.Hash <> 0) and (A^.Hash <> B^.Hash) then Exit(False);
  P := StrChars(A);
  Q := StrChars(B);
  for N := 0 to A^.Len - 1 do
    if P[N] <> Q[N] then Exit(False);
  Result := True;
end;

{ FastLoop's opGetElem past its fast paths, as Execute's: GetElemV, its result in Dst^ }
function XFastGetElem(VA, VB: TValue; Dst: PValue): Boolean; noinline;
var V: TValue;
begin
  Result := GetElemV(VA, VB, V);
  if Result then Dst^ := V;
end;

{ FastLoop's opGetProp on an object its IC misses, as Execute's: an accessor entry's getter, else ICGetMiss }
function XFastGetPropMiss(IC: PPropIC; O: PObj; VB: TValue; Dst: PValue): Boolean; noinline;
var V: TValue; J: Integer;
begin
  J := ICAccGet(IC, O, VB, V);
  if J = 0 then Exit(False);
  if J <> 1 then
    if not ICGetMiss(IC, O, VB, V) then Exit(False);
  Dst^ := V;
  Result := True;
end;

{ FastLoop's call of a native function O (as Execute's opCall): its result in R[A]; False with its throw pending.
  Apart from FastLoop, whose register allocation the call's work would spoil }
function XFastNative(O: PObj; R: PValue; A: NativeInt; I: QWord; Fr: PFrame): Boolean; noinline;
var Argc, SavedTop, NP: Integer; V: TValue;
begin
  Argc := (I shr 24) and $FFFF;
  if (I shr 40) and 1 <> 0 then R[A + 1] := VUndefined;
  SavedTop := StackTop;
  StackTop := Fr^.Base + A + 2 + Argc;
  V := VUndefined;
  PushRoot(CurNewTarget);   { a plain call: the native must not see an outer new.target }
  CurNewTarget := VUndefined;
  NP := TempTop;
  NativeData := O^.Pad and $00FFFFFF;
  NativeSelf := MkPtr(O);
  Result := TNativeFn(O^.Native)(@R[A + 1], Argc, V);
  if RootCheck and (TempTop <> NP) then
  begin
    XReportRootImbalance(O, TempTop - NP, Result, Fr^.Tpl);
    Halt(9);
  end;
  CurNewTarget := RootAt(TempTop - 1);
  PopRoots(1);
  StackTop := SavedTop;
  if Result then R[A] := V;
end;

{ ToInt32 (7.1.6) of a double QuickInt32 leaves (|D| >= 2^53, an integer, or NaN or an infinity, 0): its low
  32 bits, from the mantissa shifted to the units }
function BigDoubleToInt32(V: TValue): Int32; noinline;
var E: Integer; L: UInt32;
begin
  E := Integer((V shr 52) and $7FF) - 1075;   { the weight of the mantissa's last bit: 2^E }
  if (E < 0) or (E >= 32) then Exit(0);
  L := UInt32(((V and $000FFFFFFFFFFFFF) or $0010000000000000) shl E);
  if (V shr 63) <> 0 then L := UInt32(0) - L;
  Result := Int32(L);
end;

{ FastLoop's opClosure (as Execute's): the closure of the running frame's child template Idx in R[A] }
procedure XFastClosure(Fr: PFrame; R: PValue; A: NativeInt; Idx: Integer); noinline;
var T: PFuncTemplate; NP, J, NP2: Integer; Ups: PValueArray; V: TValue; O: PObj;
begin
  T := Fr^.Tpl^.Children[Idx];
  NP := Length(T^.Upvals);
  if NP > 0 then
  begin
    Ups := NewValueArray(NP);
    Ups^.Len := NP;
    R[A] := MkPtr(Ups);   { keep it rooted while upvalues allocate }
    for J := 0 to NP - 1 do
    begin
      NP2 := GCMinorCount;
      if T^.Upvals[J].FromParentLocal then V := FindOrOpenUpval(Fr^.Base + T^.Upvals[J].Index)
      else V := PValueArray(PtrOf(ObjOf(Fr^.Closure)^.Aux2))^.Data[T^.Upvals[J].Index];
      if GCVerify and not NurseryCellOK(R[A]) then
      begin
        XReportBadUps(J, NP2, Fr^.Base + A, Fr^.Base, Fr^.Tpl);
        Halt(9);
      end;
      ValueArrayStore(PValueArray(PtrOf(R[A])), J, V);
    end;
    O := NewClosure(T, R[A]);
  end
  else
    O := NewClosure(T, VUndefined);
  R[A] := MkPtr(O);
end;

{ FastLoop's opNew (as Execute's, which takes anything else): 0 for Execute; 1 a built-in constructor's result in
  R[A]; 2 a base constructor's frame pushed (10.2.2 [[Construct]]: this from the template's caches of the
  prototype slot and its root shape; the field initializer's frame above it, step 6.b) with StackTop set; -1 a
  throw pending (from the constructor's frame when it was pushed). The caller saved the frame's PC. }
function XFastNew(R: PValue; A: NativeInt; Argc: Integer): Integer; noinline;
var VA, V: TValue; O: PObj; T: PFuncTemplate; Base, J: Integer; Fr: PFrame;
begin
  VA := R[A];
  if not IsObject(VA) or MultiRealmAny then Exit(0);
  O := ObjOf(VA);
  if (O^.OKind = okNative) and ((O^.OFlags and 32) <> 0) then
  begin
    V := VUndefined;
    if not Construct(VA, @R[A + 2], Argc, VA, V) then Exit(-1);
    R[A] := V;
    Exit(1);
  end;
  if O^.OKind <> okFunction then Exit(0);
  T := PFuncTemplate(O^.Native);
  if T^.IsArrow or (T^.IsMethod and not T^.IsClassCtor) or T^.IsGenerator or T^.IsAsync then Exit(0);
  Fr := @Frames[FrameTop - 1];
  Base := Fr^.Base + A + 1;
  if StackTop < Base + 1 + Argc then StackTop := Base + 1 + Argc;
  if T^.IsDerived then
  begin
    { this is bound by super() }
    R[A + 1] := VHole;
    if not PushBytecodeFrame(O, VHole, @R[A + 2], Argc, Base - 1, 1, VA) then Exit(-1);
    Exit(2);
  end;
  if (Pointer(O^.Shape) <> T^.CtorFnShape) or (T^.CtorRootShape = nil) or
     (PShape(T^.CtorRootShape)^.Proto <> GetSlot(O, T^.CtorProtoSlot)) then Exit(0);
  { the arguments are roots while the new object is allocated (a collection can move the callee: read again
    from its register) }
  R[A + 1] := MkPtr(NewObject(PShape(T^.CtorRootShape), Max(4, T^.ExpectedProps), okPlain));
  VA := R[A];
  if not T^.FastCall then
  begin
    { a rest parameter, an arguments object, not compiled yet: PushBytecodeFrame's work }
    if not PushBytecodeFrame(ObjOf(VA), R[A + 1], @R[A + 2], Argc, Base - 1, 1, VA) then Exit(-1);
    if Assigned(T^.FieldInit) and not XPushFieldInit(Frames[FrameTop - 1].Closure) then Exit(-1);
    Exit(2);
  end;
  if (Base + T^.MaxRegs >= STACK_MAX) or (FrameTop >= FRAME_MAX - 2) then
  begin
    { PushBytecodeFrame's RangeError }
    if not PushBytecodeFrame(ObjOf(VA), R[A + 1], @R[A + 2], Argc, Base - 1, 1, VA) then Exit(-1);
    Exit(2);
  end;
  if Argc < T^.NumParams then J := Argc + 1 else J := T^.NumParams + 1;
  if J < T^.MaxRegs then FillUndef(@VStack[Base + J], T^.MaxRegs - J);
  Inc(Fr);
  Fr^.Closure := VA;
  Fr^.NewTarget := VA;
  Fr^.Tpl := T;
  Fr^.Base := Base;
  Fr^.PC := 0;   { EntryPC, as opCall's }
  Fr^.RetIdx := Base - 1;
  Fr^.Argc := Argc;
  Fr^.Kind := 1;
  Inc(FrameTop);
  StackTop := Base + T^.MaxRegs;
  if Assigned(T^.FieldInit) and not XPushFieldInit(VA) then Exit(-1);
  Result := 2;
end;

{ opSetProp past its cache (Execute's and FastLoop's): the store to the object or primitive in Reg^ (a register,
  read again after the store, which can collect), then the cache of an own data store or an add transition }
function XSetPropSlow(IC: PPropIC; Reg: PValue; V: TValue; Strict: Boolean): Boolean; noinline;
var VA, VB: TValue; O: PObj; CI: Integer;
begin
  VA := Reg^;
  if not IsObject(VA) then Exit(SetVS(VA, IC^.Key, V, Strict));
  O := ObjOf(VA);
  if (O^.Shape = IC^.Shape) and (IC^.NewShape <> nil) then
  begin
    { the cached add transition, its overflow slots full: grown here (the callers took every other case) }
    AddPropertyTo(O, IC^.NewShape, V);
    Exit(True);
  end;
  CI := ShapeLookup(O^.Shape, IC^.Key);
  if (CI >= 0) and ((O^.Shape^.PFlags[CI] and (pfAccessor or pfWritable)) = pfWritable) and not O^.Shape^.IsDict then
  begin
    IC^.Shape := O^.Shape; IC^.Slot := CI; IC^.NewShape := nil;
    SetSlot(O, CI, V);
    Exit(True);
  end;
  if CI < 0 then
  begin
    IC^.Depth := ProtoEpoch;   { an add transition is cached only if no prototype changed meanwhile }
    VB := MkPtr(O^.Shape);
    if not SetOnObjectS(VA, IC^.Key, V, Strict) then Exit(False);
    O := ObjOf(Reg^);
    if (O^.OKind = okPlain) and (O^.Shape^.Parent = PShape(PtrOf(VB))) and (O^.Shape^.Count > 0) and
       (O^.Shape^.Keys[O^.Shape^.Count - 1] = IC^.Key) and (IC^.Depth = ProtoEpoch) and not AddICUnsafe then
    begin
      { G2.Core ProtoChanged forgets it when a prototype gains a setter or read-only property }
      IC^.Shape := PShape(PtrOf(VB));
      IC^.NewShape := O^.Shape;
      IC^.Slot := O^.Shape^.Count - 1;
    end;
    Exit(True);
  end;
  Result := SetOnObjectS(VA, IC^.Key, V, Strict);
end;

{ FastLoop's f.call(t, ...args) (R[A] the intrinsic call, R[A + 1] f, Argc counting t): 1 when FastLoop can run
  it once the registers are shifted, a native f or a bytecode f that FastLoop's opCall takes (FastCall, not a
  class constructor, room on the stacks, a this it needs not box); 0 for Execute }
function XFastCallCheck(R: PValue; A: NativeInt; Argc: Integer): Integer; noinline;
var VB, This: TValue; O: PObj; T: PFuncTemplate;
begin
  Result := 0;
  VB := R[A + 1];
  if not IsObject(VB) then Exit;
  O := ObjOf(VB);
  if O^.OKind = okNative then
  begin
    if O^.Native = FuncCallNative then Exit;
    Exit(1);
  end;
  if (O^.OKind <> okFunction) or ((O^.OFlags and 1) <> 0) then Exit;
  T := PFuncTemplate(O^.Native);
  if not T^.FastCall then Exit;
  if (Frames[FrameTop - 1].Base + A + 1 + T^.MaxRegs >= STACK_MAX) or (FrameTop >= FRAME_MAX - 2) then Exit;
  if Argc = 0 then This := VUndefined else This := R[A + 2];
  if not T^.IsStrict and not T^.IsArrow and not IsObject(This) and (This <> VUndefined) and (This <> VNull) then Exit;
  Result := 1;
end;

{ FastLoop's opNewObj (as Execute's): an object literal's object with room for N properties in R[A] }
procedure XFastNewObj(R: PValue; A: NativeInt; N: Integer); noinline;
begin
  R[A] := MkPtr(NewObject(RootShapeFor(ObjectProto), Max(4, N), okPlain));
end;

{ FastLoop's opDefProp (as Execute's): an object literal's data property Key on Obj }
procedure XFastDefProp(Obj: TValue; Key: PStr; V: TValue); noinline;
var O: PObj; CI: Integer;
begin
  O := ObjOf(Obj);
  CI := ShapeLookup(O^.Shape, Key);
  if CI < 0 then AddProperty(O, Key, V, pfDefault)
  else if O^.Shape^.PFlags[CI] = pfDefault then SetSlot(O, CI, V)
  else DefineOwn(Obj, Key, V, pfDefault);   { replaces an accessor of the same name }
end;

{ ToInt32 of a Number (False for anything else), out of line for FastLoop's two-operand bitwise operators }
function NumToInt32(V: TValue; out I: Int32): Boolean; noinline;
begin
  if QuickInt32(V, I) then Exit(True);
  if V >= DOUBLE_LIMIT then Exit(False);
  I := BigDoubleToInt32(V);
  Result := True;
end;

{ FastLoop's call of a native getter O on This (an accessor property's [[Get]], as ICAccGet's CallValue): its
  result in Dst^; False with its throw pending. The caller checked the stack has room for the receiver }
function XFastNativeGetter(O: PObj; This: TValue; Dst: PValue): Boolean; noinline;
var SavedTop, NP: Integer; V: TValue;
begin
  SavedTop := StackTop;
  VStack[SavedTop] := This;
  StackTop := SavedTop + 1;
  V := VUndefined;
  PushRoot(CurNewTarget);   { a plain call: the native must not see an outer new.target }
  CurNewTarget := VUndefined;
  NP := TempTop;
  NativeData := O^.Pad and $00FFFFFF;
  NativeSelf := MkPtr(O);
  Result := TNativeFn(O^.Native)(@VStack[SavedTop], 0, V);
  if RootCheck and (TempTop <> NP) then
  begin
    XReportRootImbalance(O, TempTop - NP, Result, nil);
    Halt(9);
  end;
  CurNewTarget := RootAt(TempTop - 1);
  PopRoots(1);
  StackTop := SavedTop;
  if Result then Dst^ := V;
end;

{$I G2.Jit.inc}

function FastLoop(Code: PQWord; PC: Integer; R, K: PValue; ICs: PPropIC; Fr: PFrame; StopFrame: Integer): Integer;
label LBail, LGetProp, LSetPropSlow;
var
  T: PFuncTemplate;
  Base, Argc, J: Integer;
  Changed: Boolean;
  VS: PValue;
  NS, NL: PByte;   { the nursery, which no collection moves while FastLoop runs }
  A: NativeInt;   { pointer-sized: R[A] needs no sign extension }
  CI, NP: Integer;
  VA, VB, V: TValue;
  Sum: Int64;
  O, O2: PObj;
  Ent: PICEntry;
  TA: PTypeofAtoms;
  E: PValueArray;
  IC: PPropIC;
  U: PUpval;
  P: PByte;
  Bool: Boolean;
  IP: PQWord;   { the next instruction: a pointer, not Code[PC], saves FastLoop's spilled Code load per step }
begin
  {$IFDEF CPUX86_64}
  { a compiled template's code first (it stops at an instruction it does not take) }
  if (JitEnabled = 1) and (Fr^.Tpl^.JitState <> 2) then
  begin
    PC := XJitRun(Fr, PC);
    if PC < 0 then begin InjectThrow := True; Exit(-1); end;   { a throw out of a call the code made }
  end;
  {$ENDIF}
  IP := @Code[PC];
  VS := VStack;
  NS := NurseryStart;
  NL := NurseryLimit;
  Changed := False;
  while True do
  begin
    { the operands are read from the instruction in memory (A its bytes 1-2, B 3-4, C the 24 bits above): no
      register holds the instruction word }
    A := PWord(PByte(IP) + 1)^;
    Inc(IP);
    case TOp(PByte(IP)[-8]) of
      opNop: ;
      opMov: R[A] := R[PWord(PByte(IP) - 5)^];
      opLoadK: R[A] := K[(PCardinal(PByte(IP) - 4)^ shr 8)];
      opLoadI: R[A] := MkInt(SarLongint(PLongInt(PByte(IP) - 4)^, 8));
      opLoadUndef: R[A] := VUndefined;
      opLoadNull: R[A] := VNull;
      opLoadTrue: R[A] := VTrue;
      opLoadFalse: R[A] := VFalse;
      opLoadHole: R[A] := VHole;
      opGetUpv:
        begin
          U := PUpval(PtrOf(PValueArray(PtrOf(ObjOf(Fr^.Closure)^.Aux2))^.Data[PWord(PByte(IP) - 5)^]));
          if U^.StackIndex >= 0 then R[A] := VS[U^.StackIndex] else R[A] := U^.Value;
        end;
      opSetUpv:
        begin
          U := PUpval(PtrOf(PValueArray(PtrOf(ObjOf(Fr^.Closure)^.Aux2))^.Data[PWord(PByte(IP) - 5)^]));
          if U^.StackIndex >= 0 then VS[U^.StackIndex] := R[A]
          else
          begin
            V := R[A];
            { a store the write barrier has to remember: an old container gets a nursery pointer }
            if ((U^.Hdr.Flags and (cfOld or cfRemembered)) = cfOld) and ((V shr 48) = TAG_PTR_HI) and
               (PByte(PtrUInt(V and PTR_MASK)) >= NS) and (PByte(PtrUInt(V and PTR_MASK)) < NL) then Remember(@U^.Hdr);
            U^.Value := V;
          end;
        end;
      opGetMethod:
        begin
          { this := the receiver, then the method as opGetProp reads it (from the receiver's register) }
          R[A + 1] := R[PWord(PByte(IP) - 5)^];
          goto LGetProp;
        end;
      opGetProp, opGetLength:
        begin
LGetProp:
          VB := R[PWord(PByte(IP) - 5)^];
          if (VB shr 48) <> TAG_PTR_HI then begin {$IFDEF G2_BAILSTATS}Inc(BailCounts[250]);{$ENDIF} goto LBail; end;
          O := PObj(PtrUInt(VB and PTR_MASK));
          if O^.Hdr.Kind = ckObject then
          begin
            if ICGetHit(O, @ICs[(PCardinal(PByte(IP) - 4)^ shr 8)], @R[A]) then Continue;
            IC := @ICs[(PCardinal(PByte(IP) - 4)^ shr 8)];
            { ICGet: the entry for the receiver's shape, then its holder through the prototypes }
            Ent := nil;
            if (IC^.Poly0.Shape = O^.Shape) and (IC^.Poly0.Kind = O^.OKind) then Ent := @IC^.Poly0
            else
              for J := 1 to IC^.PolyCount - 1 do
                if (IC^.More^[J].Shape = O^.Shape) and (IC^.More^[J].Kind = O^.OKind) then
                begin
                  Ent := @IC^.More^[J];
                  Break;
                end;
            if Ent <> nil then
            begin
              O2 := O;
              for J := 0 to Ent^.Depth - 1 do
              begin
                if ((Ent^.VerMask shr J) and 1 <> 0) and (O2^.Shape^.Version <> Ent^.Vers[J]) then
                begin
                  {$IFDEF G2_BAILSTATS}Inc(BailCounts[251]);{$ENDIF}
                  goto LBail;
                end;
                O2 := ObjOf(O2^.Shape^.Proto);
                if O2^.Shape <> Ent^.Holder[J] then
                begin
                  {$IFDEF G2_BAILSTATS}Inc(BailCounts[252]);{$ENDIF}
                  goto LBail;
                end;
              end;
              if Ent^.Acc = 0 then
              begin
                R[A] := GetSlot(O2, Ent^.Slot);
                Continue;
              end;
              { an accessor: a bytecode getter runs in a frame of its own whose result lands in R[A] (opRet's RetIdx),
                this the receiver; anything else is Execute's }
              V := PAccessor(PtrOf(GetSlot(O2, Ent^.Slot)))^.Getter;
              if V = VUndefined then begin R[A] := VUndefined; Continue; end;
              if not IsObject(V) or MultiRealmAny then goto LBail;
              O2 := ObjOf(V);
              if (O2^.OKind = okNative) and (StackTop + 2 < STACK_MAX) then
              begin
                { a built-in getter (byteLength, size): called here, its throw Execute's with the PC past this }
                Fr^.PC := Integer((PtrUInt(IP) - PtrUInt(Code)) shr 3);
                if not XFastNativeGetter(O2, VB, @R[A]) then
                begin
                  InjectThrow := True;
                  Exit(-1);
                end;
                Continue;
              end;
              if (O2^.OKind <> okFunction) or ((O2^.OFlags and 1) <> 0) then goto LBail;
              T := PFuncTemplate(O2^.Native);
              if T^.IsAsync or T^.IsGenerator or (T^.RestReg >= 0) or (T^.ArgsReg >= 0) or (T^.Lazy <> nil) then
                goto LBail;
              Base := StackTop;
              if (Base + T^.MaxRegs >= STACK_MAX) or (FrameTop >= FRAME_MAX - 2) then goto LBail;
              Fr^.PC := Integer((PtrUInt(IP) - PtrUInt(Code)) shr 3);
              VS[Base] := VB;
              if T^.MaxRegs > 1 then FillUndef(@VS[Base + 1], T^.MaxRegs - 1);
              J := Fr^.Base + A;
              Fr := @Frames[FrameTop];
              Fr^.Closure := V;
              Fr^.NewTarget := VUndefined;
              Fr^.Tpl := T;
              Fr^.Base := Base;
              Fr^.PC := T^.EntryPC;
              Fr^.RetIdx := J;
              Fr^.Argc := 0;
              Fr^.Kind := 0;
              Inc(FrameTop);
              StackTop := Base + T^.MaxRegs;
              R := @VS[Base];
              Code := @T^.Code[0];
              IP := @Code[T^.EntryPC];
              K := T^.KBase;
              ICs := T^.ICBase;
              Changed := True;
              Continue;
            end;
            if (TOp(PByte(IP)[-8]) = opGetLength) and (O^.OKind = okArray) and ((O^.OFlags and ofBigLen) = 0) then
            begin
              R[A] := MkInt(O^.Elements^.Len);
              Continue;
            end;
            { the intrinsic length getter of a typed array without own properties over an ordinary buffer }
            if (TOp(PByte(IP)[-8]) = opGetLength) and (O^.OKind = okTypedArray) and (O^.Shape^.Count = 0) and
               ((ObjOf(O^.Aux1)^.OFlags and (bfShared or bfResizable or bfDetached)) = 0) then
            begin
              R[A] := MkInt(Int32(UInt32(O^.Aux2)));
              Continue;
            end;
            {$IFDEF G2_BAILSTATS}if IC^.PolyCount = 0 then Inc(BailCounts[248]) else if IC^.PolyCount < 4 then Inc(BailCounts[249]) else Inc(BailCounts[253]);{$ENDIF}
            Fr^.PC := Integer((PtrUInt(IP) - PtrUInt(Code)) shr 3);
            if not XFastGetPropMiss(IC, O, VB, @R[A]) then
            begin
              InjectThrow := True;
              Exit(-1);
            end;
            Continue;
          end;
          if O^.Hdr.Kind = ckString then
          begin
            if TOp(PByte(IP)[-8]) = opGetLength then
            begin
              R[A] := MkInt(PStr(O)^.Len);
              Continue;
            end;
            { ICFillString's cache of String.prototype's property }
            O2 := ObjOf(StringProto);
            IC := @ICs[(PCardinal(PByte(IP) - 4)^ shr 8)];
            if O2^.Shape = IC^.Shape then
            begin
              R[A] := GetSlot(O2, IC^.Slot);
              Continue;
            end;
          end;
          goto LBail;
        end;
      opClosure: XFastClosure(Fr, R, A, PCardinal(PByte(IP) - 4)^ shr 8);
      opDefProp: XFastDefProp(R[A], PStr(PtrOf(K[PCardinal(PByte(IP) - 4)^ shr 8])), R[PWord(PByte(IP) - 5)^]);
      opNewObj:
        begin
          { an object literal's object, as Execute (a collection here is safe: no local holds a cell across
            instructions, the registers are roots up to StackTop) }
          XFastNewObj(R, A, PCardinal(PByte(IP) - 4)^ shr 8);
        end;
      opSetProp:
        begin
          { a cached own data store or add transition into an inline slot; anything else XSetPropSlow's }
          VA := R[A];
          IC := @ICs[(PCardinal(PByte(IP) - 4)^ shr 8)];
          if not IsObject(VA) then goto LSetPropSlow;
          O := ObjOf(VA);
          if O^.Shape <> IC^.Shape then goto LSetPropSlow;
          V := R[PWord(PByte(IP) - 5)^];
          if IC^.Slot >= O^.InlineCap then
          begin
            { a property in the overflow slots: an existing one, or an add into their spare capacity (growing them
              allocates: XSetPropSlow's) }
            E := O^.Overflow;
            if IC^.NewShape <> nil then
            begin
              if (E = nil) or (IC^.Slot - O^.InlineCap >= Integer(E^.Cap)) then goto LSetPropSlow;
              if Integer(E^.Len) <= IC^.Slot - O^.InlineCap then E^.Len := IC^.Slot - O^.InlineCap + 1;
              O^.Shape := IC^.NewShape;
            end;
            if ((E^.Hdr.Flags and (cfOld or cfRemembered)) = cfOld) and ((V shr 48) = TAG_PTR_HI) and
               (PByte(PtrUInt(V and PTR_MASK)) >= NS) and (PByte(PtrUInt(V and PTR_MASK)) < NL) then Remember(@E^.Hdr);
            E^.Data[IC^.Slot - O^.InlineCap] := V;
            Continue;
          end;
          if ((O^.Hdr.Flags and (cfOld or cfRemembered)) = cfOld) and ((V shr 48) = TAG_PTR_HI) and
             (PByte(PtrUInt(V and PTR_MASK)) >= NS) and (PByte(PtrUInt(V and PTR_MASK)) < NL) then Remember(@O^.Hdr);
          if IC^.NewShape <> nil then O^.Shape := IC^.NewShape;
          O^.Slots[IC^.Slot] := V;
          Continue;
LSetPropSlow:
          Fr^.PC := Integer((PtrUInt(IP) - PtrUInt(Code)) shr 3);
          if not XSetPropSlow(IC, @R[A], R[PWord(PByte(IP) - 5)^], Fr^.Tpl^.IsStrict) then
          begin
            InjectThrow := True;
            Exit(-1);
          end;
        end;
      opDefField:
        begin
          { a class field's cached add transition (an extensible object with the cached shape) }
          O := ObjOf(R[A]);
          IC := @ICs[(PCardinal(PByte(IP) - 4)^ shr 8)];
          if (O^.Shape <> IC^.Shape) or (IC^.NewShape = nil) or (IC^.Slot >= O^.InlineCap) or
             ((O^.OFlags and 8) <> 0) then goto LBail;
          V := R[PWord(PByte(IP) - 5)^];
          if ((O^.Hdr.Flags and (cfOld or cfRemembered)) = cfOld) and ((V shr 48) = TAG_PTR_HI) and
             (PByte(PtrUInt(V and PTR_MASK)) >= NS) and (PByte(PtrUInt(V and PTR_MASK)) < NL) then Remember(@O^.Hdr);
          O^.Shape := IC^.NewShape;
          O^.Slots[IC^.Slot] := V;
        end;
      opGetElem:
        begin
          VA := R[PWord(PByte(IP) - 5)^];
          VB := R[(PCardinal(PByte(IP) - 4)^ shr 8)];
          if not (IsInt(VB) and IsObject(VA)) then
          begin
            { a string key, a primitive base: GetElemV, as Execute (a nullish base's error is Execute's) }
            if (VA = VUndefined) or (VA = VNull) then goto LBail;
            Fr^.PC := Integer((PtrUInt(IP) - PtrUInt(Code)) shr 3);
            if not XFastGetElem(VA, VB, @R[A]) then
            begin
              InjectThrow := True;
              Exit(-1);
            end;
            Continue;
          end;
          O := ObjOf(VA);
          E := O^.Elements;
          CI := IntOf(VB);
          if (E <> nil) and (CI >= 0) and (CI < Integer(E^.Len)) and (E^.Data[CI] <> VHole) then
          begin
            R[A] := E^.Data[CI];
            Continue;
          end;
          { a hole or an index past the end of a plain object or array: undefined when no prototype has indexed
            properties }
          if (CI >= 0) and (O^.OKind in [okPlain, okArray]) and not O^.Shape^.HasIndexKeys and ElemMissUndefined(O, CI) then
          begin
            R[A] := VUndefined;
            Continue;
          end;
          { a typed array over an ordinary buffer (the length is the view's), of a kind read without a call }
          if (O^.OKind <> okTypedArray) or (CI < 0) or
             ((ObjOf(O^.Aux1)^.OFlags and (bfShared or bfResizable or bfDetached)) <> 0) or
             (CI >= Int32(UInt32(O^.Aux2))) then goto LBail;
          P := @PBytes(PtrOf(ObjOf(O^.Aux1)^.Aux1))^.Data[0] + O^.Pad;
          case O^.OFlags shr 4 of
            taInt8: R[A] := MkInt(PShortInt(P)[CI]);
            taUint8, taUint8C: R[A] := MkInt(P[CI]);
            taInt16: R[A] := MkInt(PSmallInt(P)[CI]);
            taUint16: R[A] := MkInt(PWord(P)[CI]);
            taInt32: R[A] := MkInt(PLongInt(P)[CI]);
            taUint32: R[A] := MkNumber(PLongWord(P)[CI]);
            taFloat32: R[A] := MkNumber(PSingle(P)[CI]);
            taFloat64: R[A] := MkNumber(PDouble(P)[CI]);
          else goto LBail;
          end;
        end;
      opSetElem:
        begin
          VA := R[A];
          VB := R[PWord(PByte(IP) - 5)^];
          V := R[(PCardinal(PByte(IP) - 4)^ shr 8)];
          if not (IsInt(VB) and IsObject(VA)) then goto LBail;
          O := ObjOf(VA);
          E := O^.Elements;
          CI := IntOf(VB);
          if (E <> nil) and (CI >= 0) and ((O^.OFlags and 64) = 0) and
             (((CI < Integer(E^.Len)) and (E^.Data[CI] <> VHole)) or
              ((CI = Integer(E^.Len)) and (CI < Integer(E^.Cap)) and (O^.OKind = okArray) and AppendProtosPlain(O))) then
          begin
            { a store the write barrier has to remember: an old container gets a nursery pointer }
            if ((E^.Hdr.Flags and (cfOld or cfRemembered)) = cfOld) and ((V shr 48) = TAG_PTR_HI) and
               (PByte(PtrUInt(V and PTR_MASK)) >= NS) and (PByte(PtrUInt(V and PTR_MASK)) < NL) then Remember(@E^.Hdr);
            E^.Data[CI] := V;
            if CI = Integer(E^.Len) then E^.Len := CI + 1;
            Continue;
          end;
          { a store into a hole of an extensible plain object or array: an add, unless a prototype has something
            at the index (a setter, a read-only element) }
          if (E <> nil) and (CI >= 0) and (CI < Integer(E^.Len)) and (E^.Data[CI] = VHole) and
             (O^.OKind in [okPlain, okArray]) and ((O^.OFlags and (8 or 64)) = 0) and not O^.Shape^.HasIndexKeys and
             ElemMissUndefined(O, CI) then
          begin
            if ((E^.Hdr.Flags and (cfOld or cfRemembered)) = cfOld) and ((V shr 48) = TAG_PTR_HI) and
               (PByte(PtrUInt(V and PTR_MASK)) >= NS) and (PByte(PtrUInt(V and PTR_MASK)) < NL) then Remember(@E^.Hdr);
            E^.Data[CI] := V;
            Continue;
          end;
          if (O^.OKind <> okTypedArray) or (CI < 0) or
             ((ObjOf(O^.Aux1)^.OFlags and (bfShared or bfResizable or bfDetached or bfImmutable)) <> 0) or
             (CI >= Int32(UInt32(O^.Aux2))) then goto LBail;
          P := @PBytes(PtrOf(ObjOf(O^.Aux1)^.Aux1))^.Data[0] + O^.Pad;
          if IsInt(V) then
            case O^.OFlags shr 4 of
              taInt8, taUint8: P[CI] := Byte(IntOf(V));
              taInt16, taUint16: PWord(P)[CI] := Word(IntOf(V));
              taInt32, taUint32: PLongWord(P)[CI] := LongWord(IntOf(V));
              taFloat32: PSingle(P)[CI] := IntOf(V);
              taFloat64: PDouble(P)[CI] := IntOf(V);
            else goto LBail;
            end
          else if IsDouble(V) then
            case O^.OFlags shr 4 of
              taFloat32: PSingle(P)[CI] := DoubleOf(V);
              taFloat64: PDouble(P)[CI] := DoubleOf(V);
            else goto LBail;
            end
          else goto LBail;
        end;
      opAdd:
        begin
          VA := R[PWord(PByte(IP) - 5)^];
          VB := R[(PCardinal(PByte(IP) - 4)^ shr 8)];
          if ((VA shr 48) = TAG_INT_HI) and ((VB shr 48) = TAG_INT_HI) then
          begin
            Sum := Int64(Int32(UInt32(VA))) + Int64(Int32(UInt32(VB)));
            if (Sum >= -2147483648) and (Sum <= 2147483647) then R[A] := TAG_INT or QWord(UInt32(Sum))
            else R[A] := MkDouble(Sum);
          end
          else if (VA < DOUBLE_LIMIT) and (VB < DOUBLE_LIMIT) then R[A] := MkArith(PDouble(@VA)^ + PDouble(@VB)^)
          else if IsNumber(VA) and IsNumber(VB) then R[A] := MkArith(NumOf(VA) + NumOf(VB))
          else goto LBail;
        end;
      opAddI, opSubI, opIncI:
        begin
          VA := R[PWord(PByte(IP) - 5)^];
          CI := SarLongint(PLongInt(PByte(IP) - 4)^, 8);
          if (VA shr 48) = TAG_INT_HI then
          begin
            Sum := Int64(Int32(UInt32(VA))) + CI;
            if (Sum >= -2147483648) and (Sum <= 2147483647) then R[A] := TAG_INT or QWord(UInt32(Sum))
            else R[A] := MkDouble(Sum);
          end
          else if VA < DOUBLE_LIMIT then R[A] := MkArith(PDouble(@VA)^ + CI)
          else goto LBail;
        end;
      { int32 operands, then two doubles (the numeric kernels' common case), then mixed numbers }
      opSub:
        begin
          VA := R[PWord(PByte(IP) - 5)^];
          VB := R[(PCardinal(PByte(IP) - 4)^ shr 8)];
          if ((VA shr 48) = TAG_INT_HI) and ((VB shr 48) = TAG_INT_HI) then
          begin
            Sum := Int64(Int32(UInt32(VA))) - Int64(Int32(UInt32(VB)));
            if (Sum >= -2147483648) and (Sum <= 2147483647) then R[A] := TAG_INT or QWord(UInt32(Sum))
            else R[A] := MkDouble(Sum);
          end
          else if (VA < DOUBLE_LIMIT) and (VB < DOUBLE_LIMIT) then R[A] := MkArith(PDouble(@VA)^ - PDouble(@VB)^)
          else if IsNumber(VA) and IsNumber(VB) then R[A] := MkArith(NumOf(VA) - NumOf(VB))
          else goto LBail;
        end;
      opMul:
        begin
          VA := R[PWord(PByte(IP) - 5)^];
          VB := R[(PCardinal(PByte(IP) - 4)^ shr 8)];
          if ((VA shr 48) = TAG_INT_HI) and ((VB shr 48) = TAG_INT_HI) then
          begin
            Sum := Int64(Int32(UInt32(VA))) * Int64(Int32(UInt32(VB)));
            { a zero product of a negative operand is -0, a double }
            if (Sum >= -2147483648) and (Sum <= 2147483647) and
               not ((Sum = 0) and ((Int32(UInt32(VA)) < 0) or (Int32(UInt32(VB)) < 0))) then
              R[A] := TAG_INT or QWord(UInt32(Sum))
            else R[A] := MkDouble(Double(Int32(UInt32(VA))) * Double(Int32(UInt32(VB))));
          end
          else if (VA < DOUBLE_LIMIT) and (VB < DOUBLE_LIMIT) then R[A] := MkArith(PDouble(@VA)^ * PDouble(@VB)^)
          else if IsNumber(VA) and IsNumber(VB) then R[A] := MkArith(NumOf(VA) * NumOf(VB))
          else goto LBail;
        end;
      opDiv:
        begin
          VA := R[PWord(PByte(IP) - 5)^];
          VB := R[(PCardinal(PByte(IP) - 4)^ shr 8)];
          if IsNumber(VA) and IsNumber(VB) then R[A] := MkNumber(NumOf(VA) / NumOf(VB))
          else goto LBail;
        end;
      opMod:
        begin
          VA := R[PWord(PByte(IP) - 5)^];
          VB := R[(PCardinal(PByte(IP) - 4)^ shr 8)];
          if ((VA shr 48) = TAG_INT_HI) and ((VB shr 48) = TAG_INT_HI) and (Int32(UInt32(VB)) > 0) and
             (Int32(UInt32(VA)) >= 0) then
            R[A] := MkInt(Int32(UInt32(VA)) mod Int32(UInt32(VB)))
          else goto LBail;
        end;
      opShr, opUShr:
        begin
          VA := R[PWord(PByte(IP) - 5)^];
          VB := R[(PCardinal(PByte(IP) - 4)^ shr 8)];
          if QuickInt32(VA, CI) and QuickInt32(VB, NP) then
          begin
            if TOp(PByte(IP)[-8]) = opShr then R[A] := MkInt(SarLongint(CI, NP and 31))
            else R[A] := MkNumber(UInt32(CI) shr (NP and 31));
          end
          else goto LBail;
        end;
      { a TDZ check that passes stays here; the ReferenceError is Execute's }
      opCheckHole: if R[A] = VHole then goto LBail;
      { x instanceof F for a bytecode function F whose shape Execute verified (no own @@hasInstance under an intact
        Function.prototype, its prototype property's slot cached): the prototype chain walk, without proxies }
      opInstanceof:
        begin
          VB := R[(PCardinal(PByte(IP) - 4)^ shr 8)];
          if (VB shr 48) <> TAG_PTR_HI then goto LBail;
          O := PObj(PtrUInt(VB and PTR_MASK));
          if (O^.Hdr.Kind <> ckObject) or (O^.OKind <> okFunction) or MultiRealmAny then goto LBail;
          T := PFuncTemplate(O^.Native);
          if (Pointer(O^.Shape) <> T^.NoHIShape) or (Pointer(O^.Shape) <> T^.CtorFnShape) or
             (ObjOf(FunctionProto)^.Shape <> HIFPShape) then goto LBail;
          V := GetSlot(O, T^.CtorProtoSlot);
          if not IsObject(V) then goto LBail;
          VA := R[PWord(PByte(IP) - 5)^];
          if not IsObject(VA) then begin R[A] := VFalse; Continue; end;
          CI := FastProtoChainHas(ObjOf(VA), V);
          if CI = 2 then goto LBail;
          R[A] := MkBool(CI = 1);
        end;
      { a global: the global object's cached slot, or a global lexical binding's cell (Execute's IC fill) }
      opGetGlobal:
        begin
          IC := @ICs[(PCardinal(PByte(IP) - 4)^ shr 8)];
          O := ObjOf(GlobalObj);
          if O^.Shape = IC^.Shape then R[A] := GetSlot(O, IC^.Slot)
          else if IC^.Depth <= -2 then
          begin
            V := GLexCells[-2 - IC^.Depth];
            if V = VUndefined then goto LBail;
            U := PUpval(PtrOf(V));
            if U^.StackIndex >= 0 then V := VS[U^.StackIndex] else V := U^.Value;
            if V = VHole then goto LBail;
            R[A] := V;
          end
          else goto LBail;
        end;
      opBitAndI, opBitOrI, opBitXorI, opShlI, opShrI, opUShrI:
        begin
          VA := R[PWord(PByte(IP) - 5)^];
          NP := Int32(SarLongint(PLongInt(PByte(IP) - 4)^, 8));
          if (VA shr 48) = TAG_INT_HI then CI := Int32(UInt32(VA))
          else if not QuickInt32(VA, CI) then
          begin
            if VA >= DOUBLE_LIMIT then goto LBail;
            CI := BigDoubleToInt32(VA);
          end;
          case TOp(PByte(IP)[-8]) of
            opBitAndI: R[A] := MkInt(CI and NP);
            opBitOrI: R[A] := MkInt(CI or NP);
            opBitXorI: R[A] := MkInt(CI xor NP);
            opShlI: R[A] := MkInt(Int32(UInt32(CI) shl (NP and 31)));
            opShrI: R[A] := MkInt(SarLongint(CI, NP and 31));
          else R[A] := MkNumber(UInt32(CI) shr (NP and 31));
          end;
        end;
      opBitAnd, opBitOr, opBitXor, opShl:
        begin
          VA := R[PWord(PByte(IP) - 5)^];
          VB := R[(PCardinal(PByte(IP) - 4)^ shr 8)];
          if not (QuickInt32(VA, CI) and QuickInt32(VB, NP)) then
            if not (NumToInt32(VA, CI) and NumToInt32(VB, NP)) then goto LBail;
          case TOp(PByte(IP)[-8]) of
            opBitAnd: R[A] := MkInt(CI and NP);
            opBitOr: R[A] := MkInt(CI or NP);
            opBitXor: R[A] := MkInt(CI xor NP);
          else R[A] := MkInt(Int32(UInt32(CI) shl (NP and 31)));
          end;
        end;
      opNeg:
        begin
          VA := R[PWord(PByte(IP) - 5)^];
          if IsInt(VA) and (IntOf(VA) <> 0) and (IntOf(VA) <> Low(Int32)) then R[A] := MkInt(-IntOf(VA))
          else if VA < DOUBLE_LIMIT then R[A] := MkDouble(-PDouble(@VA)^)
          else goto LBail;
        end;
      opBitNot:
        begin
          VA := R[PWord(PByte(IP) - 5)^];
          if IsInt(VA) then R[A] := MkInt(not IntOf(VA)) else goto LBail;
        end;
      opNot:
        begin
          R[A] := MkBool(not ToBool(R[PWord(PByte(IP) - 5)^]));
        end;
      opSEq, opSNe:
        begin
          VA := R[PWord(PByte(IP) - 5)^];
          VB := R[(PCardinal(PByte(IP) - 4)^ shr 8)];
          { StrictEquals }
          if VA = VB then Bool := not IsDouble(VA) or (PDouble(@VA)^ = PDouble(@VA)^)
          else if ((VA shr 48) = TAG_PTR_HI) and ((VB shr 48) = TAG_PTR_HI) then
          begin
            if IsString(VA) and IsString(VB) then Bool := FastStrEquals(StrOf(VA), StrOf(VB))
            else if IsBigInt(VA) and IsBigInt(VB) then goto LBail
            else Bool := False;   { objects and symbols: the same one only }
          end
          { a double against a Number: equal by value (1 and 1.0, 0 and -0); any other pair of different bits (ints,
            the specials, a pointer and a non-pointer) differs }
          else if (VA < DOUBLE_LIMIT) or (VB < DOUBLE_LIMIT) then Bool := IsNumber(VA) and IsNumber(VB) and (NumOf(VA) = NumOf(VB))
          else Bool := False;
          if TOp(PByte(IP)[-8]) = opSNe then Bool := not Bool;
          R[A] := MkBool(Bool);
        end;
      opTypeof:
        begin
          { TypeofValue, once its atoms exist }
          TA := TypeofAtoms;
          if TA = nil then goto LBail;
          VA := R[PWord(PByte(IP) - 5)^];
          if IsNumber(VA) then R[A] := MkPtr(TA^.Num)
          else if (VA = VUndefined) or (VA = VHole) then R[A] := MkPtr(TA^.Undef)
          else if VA = VNull then R[A] := MkPtr(TA^.Obj)
          else if (VA = VTrue) or (VA = VFalse) then R[A] := MkPtr(TA^.Bool)
          else
            case PCell(PtrOf(VA))^.Kind of
              ckString: R[A] := MkPtr(TA^.Str);
              ckSymbol: R[A] := MkPtr(TA^.Sym);
              9: R[A] := MkPtr(TA^.Big);
              ckObject:
                if (PCell(PtrOf(VA))^.Flags and cfEmulatesUndefined) <> 0 then R[A] := MkPtr(TA^.Undef)   { [[IsHTMLDDA]] }
                else if IsCallable(VA) then R[A] := MkPtr(TA^.Fn) else R[A] := MkPtr(TA^.Obj);
            else R[A] := MkPtr(TA^.Undef);
            end;
        end;
      opToNum, opToNumeric:
        begin
          VA := R[PWord(PByte(IP) - 5)^];
          if not IsNumber(VA) then goto LBail;
          R[A] := VA;
        end;
      opEq, opNe:
        begin
          { LooseEquals' first cases: nullish, two numbers, two strings, the same value }
          VA := R[PWord(PByte(IP) - 5)^];
          VB := R[(PCardinal(PByte(IP) - 4)^ shr 8)];
          if (VA = VUndefined) or (VA = VNull) or (VB = VUndefined) or (VB = VNull) then
          begin
            Bool := ((VA = VUndefined) or (VA = VNull)) and ((VB = VUndefined) or (VB = VNull));
            { B.3.6.2: an [[IsHTMLDDA]] object against undefined or null }
            if not Bool and (((VA shr 48) = TAG_PTR_HI) and ((PCell(PtrUInt(VA and PTR_MASK))^.Flags and cfEmulatesUndefined) <> 0) or
               ((VB shr 48) = TAG_PTR_HI) and ((PCell(PtrUInt(VB and PTR_MASK))^.Flags and cfEmulatesUndefined) <> 0)) then
              Bool := True;
          end
          else if IsNumber(VA) and IsNumber(VB) then Bool := NumOf(VA) = NumOf(VB)
          else if IsString(VA) and IsString(VB) then Bool := FastStrEquals(StrOf(VA), StrOf(VB))
          else if (VA = VB) and not IsBigInt(VA) then Bool := True
          else goto LBail;
          if TOp(PByte(IP)[-8]) = opNe then Bool := not Bool;
          R[A] := MkBool(Bool);
        end;
      opLt, opLe, opGt, opGe:
        begin
          VA := R[PWord(PByte(IP) - 5)^];
          VB := R[(PCardinal(PByte(IP) - 4)^ shr 8)];
          if IsInt(VA) and IsInt(VB) then
          begin
            case TOp(PByte(IP)[-8]) of
              opLt: Bool := IntOf(VA) < IntOf(VB);
              opLe: Bool := IntOf(VA) <= IntOf(VB);
              opGt: Bool := IntOf(VA) > IntOf(VB);
            else Bool := IntOf(VA) >= IntOf(VB);
            end;
          end
          else if IsNumber(VA) and IsNumber(VB) then
          begin
            case TOp(PByte(IP)[-8]) of
              opLt: Bool := NumOf(VA) < NumOf(VB);
              opLe: Bool := NumOf(VA) <= NumOf(VB);
              opGt: Bool := NumOf(VA) > NumOf(VB);
            else Bool := NumOf(VA) >= NumOf(VB);
            end;
          end
          else goto LBail;
          R[A] := MkBool(Bool);
        end;
      opJmp:
        begin
          CI := SarLongint(PLongInt(PByte(IP) - 4)^, 8);
          Inc(IP, CI);
          {$IFDEF CPUX86_64}
          { a loop's back edge: into the compiled loop }
          if (JitEnabled = 1) and (CI < 0) and (Fr^.Tpl^.JitState <> 2) then
          begin
            CI := XJitRun(Fr, Integer((PtrUInt(IP) - PtrUInt(Code)) shr 3));
            if CI < 0 then begin InjectThrow := True; Exit(-1); end;
            IP := @Code[CI];
          end;
          {$ENDIF}
        end;
      { a boolean first: most conditions are comparison results }
      opJT:
        begin
          V := R[A];
          if (V = VTrue) or ((V <> VFalse) and ToBool(V)) then Inc(IP, SarLongint(PLongInt(PByte(IP) - 4)^, 8));
        end;
      opJF:
        begin
          V := R[A];
          if (V = VFalse) or ((V <> VTrue) and not ToBool(V)) then Inc(IP, SarLongint(PLongInt(PByte(IP) - 4)^, 8));
        end;
      opJNullish: if (R[A] = VUndefined) or (R[A] = VNull) then Inc(IP, SarLongint(PLongInt(PByte(IP) - 4)^, 8));
      opJNotNullish: if (R[A] <> VUndefined) and (R[A] <> VNull) then Inc(IP, SarLongint(PLongInt(PByte(IP) - 4)^, 8));
      opJUndef: if R[A] = VUndefined then Inc(IP, SarLongint(PLongInt(PByte(IP) - 4)^, 8));
      opJNotUndef: if R[A] <> VUndefined then Inc(IP, SarLongint(PLongInt(PByte(IP) - 4)^, 8));
      opJNLt, opJNLe, opJNGt, opJNGe:
        begin
          VA := R[A];
          VB := R[PWord(PByte(IP) - 5)^];
          if ((VA shr 48) = TAG_INT_HI) and ((VB shr 48) = TAG_INT_HI) then
          begin
            case TOp(PByte(IP)[-8]) of
              opJNLt: Bool := Int32(UInt32(VA)) < Int32(UInt32(VB));
              opJNLe: Bool := Int32(UInt32(VA)) <= Int32(UInt32(VB));
              opJNGt: Bool := Int32(UInt32(VA)) > Int32(UInt32(VB));
            else Bool := Int32(UInt32(VA)) >= Int32(UInt32(VB));
            end;
          end
          else if IsNumber(VA) and IsNumber(VB) then
          begin
            case TOp(PByte(IP)[-8]) of
              opJNLt: Bool := NumOf(VA) < NumOf(VB);
              opJNLe: Bool := NumOf(VA) <= NumOf(VB);
              opJNGt: Bool := NumOf(VA) > NumOf(VB);
            else Bool := NumOf(VA) >= NumOf(VB);
            end;
          end
          else goto LBail;
          if not Bool then Inc(IP, SarLongint(PLongInt(PByte(IP) - 4)^, 8));
        end;
      opJNSEq, opJSEq:
        begin
          VA := R[A];
          VB := R[PWord(PByte(IP) - 5)^];
          { StrictEquals }
          if VA = VB then Bool := not IsDouble(VA) or (PDouble(@VA)^ = PDouble(@VA)^)
          else if ((VA shr 48) = TAG_PTR_HI) and ((VB shr 48) = TAG_PTR_HI) then
          begin
            if IsString(VA) and IsString(VB) then Bool := FastStrEquals(StrOf(VA), StrOf(VB))
            else if IsBigInt(VA) and IsBigInt(VB) then goto LBail
            else Bool := False;   { objects and symbols: the same one only }
          end
          { a double against a Number: equal by value (1 and 1.0, 0 and -0); any other pair of different bits (ints,
            the specials, a pointer and a non-pointer) differs }
          else if (VA < DOUBLE_LIMIT) or (VB < DOUBLE_LIMIT) then Bool := IsNumber(VA) and IsNumber(VB) and (NumOf(VA) = NumOf(VB))
          else Bool := False;
          if Bool = (TOp(PByte(IP)[-8]) = opJSEq) then Inc(IP, SarLongint(PLongInt(PByte(IP) - 4)^, 8));
        end;
      opCall:
        begin
          { PushBytecodeFrame's work for a function with no rest parameter or arguments object, not async and
            not a class constructor; the callee then runs here }
          VA := R[A];
          if not IsObject(VA) or MultiRealmAny then goto LBail;
          O := ObjOf(VA);
          Argc := PWord(PByte(IP) - 5)^;
          if (PByte(IP)[-3] and 1) <> 0 then R[A + 1] := VUndefined;
          if (O^.OKind = okNative) and (O^.Native = FuncCallNative) then
          begin
            { f.call(t, ...args) with the intrinsic call (as Execute's opCall): the registers shift down a place, f
              the callee and t its this, f called here; checked before the shift (a bail after it would run the
              call again on the shifted registers) }
            if XFastCallCheck(R, A, Argc) = 0 then goto LBail;
            VA := R[A + 1];
            R[A] := VA;
            if Argc = 0 then R[A + 1] := VUndefined
            else
            begin
              for J := 0 to Argc - 1 do R[A + 1 + J] := R[A + 2 + J];
              Dec(Argc);
            end;
            O := ObjOf(VA);
            if O^.OKind = okNative then
            begin
              Fr^.PC := Integer((PtrUInt(IP) - PtrUInt(Code)) shr 3);
              { XFastNative's operands: Argc, this already in R[A + 1] }
              if not XFastNative(O, R, A, (PQWord(IP)[-1] and not ((QWord($FFFF) shl 24) or (QWord(1) shl 40))) or
                (QWord(Argc) shl 24), Fr) then
              begin
                InjectThrow := True;
                Exit(-1);
              end;
              Continue;
            end;
          end
          else if O^.OKind = okNative then
          begin
            { a native function, called here as Execute's opCall does; its throw goes to Execute with the frame's
              PC past this call }
            Fr^.PC := Integer((PtrUInt(IP) - PtrUInt(Code)) shr 3);
            if not XFastNative(O, R, A, PQWord(IP)[-1], Fr) then
            begin
              InjectThrow := True;
              Exit(-1);
            end;
            Continue;
          end;
          if (O^.OKind <> okFunction) or ((O^.OFlags and 1) <> 0) then goto LBail;
          T := PFuncTemplate(O^.Native);
          if not T^.FastCall then goto LBail;
          Base := Fr^.Base + A + 1;
          if (Base + T^.MaxRegs >= STACK_MAX) or (FrameTop >= FRAME_MAX - 2) then goto LBail;
          Fr^.PC := Integer((PtrUInt(IP) - PtrUInt(Code)) shr 3);
          R := @VS[Base];
          if not T^.IsStrict and not T^.IsArrow then
          begin
            { a primitive this: Execute's PushBytecodeFrame boxes it (no frame is pushed yet; the stack covers the
              arguments when Execute takes over) }
            V := R[0];
            if ((V shr 48) = TAG_PTR_HI) and (PCell(PtrUInt(V and PTR_MASK))^.Kind = ckObject) then
            else if (V = VUndefined) or (V = VNull) then R[0] := GlobalObj
            else
            begin
              if StackTop < Base + 1 + Argc then StackTop := Base + 1 + Argc;
              goto LBail;
            end;
          end;
          if Argc < T^.NumParams then J := Argc + 1 else J := T^.NumParams + 1;
          { the registers past the arguments start undefined (FillQWord: FastLoop's register pressure made an
            inline loop cost about seven instructions a register) }
          NP := T^.MaxRegs;
          if J < NP then FillUndef(@R[J], NP - J);
          Inc(Fr);   { the callee's frame, after the caller's at FrameTop - 1 (Frames never moves) }
          Fr^.Closure := VA;
          Fr^.NewTarget := VUndefined;
          Fr^.Tpl := T;
          Fr^.Base := Base;
          Fr^.PC := 0;   { EntryPC: CompactNops leaves every function starting at its first instruction }
          Fr^.RetIdx := Base - 1;
          Fr^.Argc := Argc;
          Fr^.Kind := 0;
          Inc(FrameTop);
          StackTop := Base + T^.MaxRegs;
          Code := @T^.Code[0];
          IP := Code;
          K := T^.KBase;
          ICs := T^.ICBase;
          Changed := True;
          {$IFDEF CPUX86_64}
          if (JitEnabled = 1) and (T^.JitState <> 2) then
          begin
            CI := XJitRun(Fr, 0);
            if CI < 0 then begin InjectThrow := True; Exit(-1); end;
            IP := @Code[CI];
          end;
          {$ENDIF}
        end;
      opSuperCall, opSuperCallV:
        begin
          { 13.3.7.1 SuperCall with a bytecode parent (XFastSuper): its frame pushed as Kind 5, or Execute's }
          Fr^.PC := Integer((PtrUInt(IP) - PtrUInt(Code)) shr 3);
          J := XFastSuper(A, PWord(PByte(IP) - 5)^, PCardinal(PByte(IP) - 4)^ shr 8, TOp(PByte(IP)[-8]) = opSuperCallV);
          if J = 0 then goto LBail;
          if J < 0 then
          begin
            InjectThrow := True;
            Exit(-1);
          end;
          Fr := @Frames[FrameTop - 1];
          T := Fr^.Tpl;
          R := @VS[Fr^.Base];
          Code := @T^.Code[0];
          IP := @Code[Fr^.PC];
          K := T^.KBase;
          ICs := T^.ICBase;
          Changed := True;
        end;
      opNew:
        begin
          { 10.2.2 [[Construct]] (XFastNew): a frame pushed, the result of a built-in constructor in R[A], or Execute's }
          Fr^.PC := Integer((PtrUInt(IP) - PtrUInt(Code)) shr 3);
          J := XFastNew(R, A, PWord(PByte(IP) - 5)^);
          if J = 0 then goto LBail;
          if J < 0 then
          begin
            InjectThrow := True;
            Exit(-1);
          end;
          if J = 2 then
          begin
            Fr := @Frames[FrameTop - 1];
            T := Fr^.Tpl;
            R := @VS[Fr^.Base];
            Code := @T^.Code[0];
            IP := @Code[Fr^.PC];
            K := T^.KBase;
            ICs := T^.ICBase;
            Changed := True;
          end;
        end;
      opIterNext:
        begin
          { an array iterator's step; a suspended generator with the intrinsic next resumed in a Kind 4 frame
            (as Execute's opIterNext); anything else is Execute's }
          CI := PWord(PByte(IP) - 5)^;
          VB := R[CI];
          if (VB shr 48) <> TAG_PTR_HI then goto LBail;
          O := PObj(PtrUInt(VB and PTR_MASK));
          if O^.Hdr.Kind <> ckObject then goto LBail;
          if (O^.OKind = okIterator) and (O^.OFlags = 0) and (R[CI + 1] = ArrayIterNextFn) then
          begin
            VA := O^.Aux1;
            if not IsObject(VA) then goto LBail;
            O2 := ObjOf(VA);
            if (O2^.Elements = nil) or ((O2^.OFlags and 64) <> 0) or not (O2^.OKind in [okArray, okArguments]) then
              goto LBail;
            NP := IntOf(O^.Aux2);
            if NP >= Integer(O2^.Elements^.Len) then goto LBail;
            V := O2^.Elements^.Data[NP];
            if V = VHole then goto LBail;
            R[A] := V;
            O^.Aux2 := MkInt(NP + 1);
            Continue;
          end;
          if (O^.OKind = okGenerator) and (O^.OFlags <= 1) and (R[CI + 1] = GenNextFn) and not MultiRealmAny then
          begin
            T := PFuncTemplate(ObjOf(O^.Aux1)^.Native);
            if (StackTop + T^.MaxRegs + 4 >= STACK_MAX) or (FrameTop >= FRAME_MAX - 2) then goto LBail;
            Fr^.PC := Integer((PtrUInt(IP) - PtrUInt(Code)) shr 3);
            if not XGenInline(VB, Fr^.Base + A) then goto LBail;
            Fr := @Frames[FrameTop - 1];
            T := Fr^.Tpl;
            R := @VS[Fr^.Base];
            Code := @T^.Code[0];
            IP := @Code[Fr^.PC];
            K := T^.KBase;
            ICs := T^.ICBase;
            Changed := True;
            { the generator's compiled code from its resumption }
            {$IFDEF CPUX86_64}
            if (JitEnabled = 1) and (T^.JitState <> 2) then
            begin
              CI := XJitRun(Fr, Fr^.PC);
              if CI < 0 then begin InjectThrow := True; Exit(-1); end;
              IP := @Code[CI];
            end;
            {$ENDIF}
            Continue;
          end;
          goto LBail;
        end;
      opYield:
        begin
          { a for-of's Kind 4 generator yields: suspended, its value in the consumer's register (as Execute) }
          if (Fr^.Kind <> 4) or MultiRealmAny then goto LBail;
          PushRoot(R[PWord(PByte(IP) - 5)^]);
          J := Fr^.RetIdx;
          SaveSuspension(Fr, Fr^.Tpl, R, A, Integer((PtrUInt(IP) - PtrUInt(Code)) shr 3), Integer((PCardinal(PByte(IP) - 4)^ shr 8)));
          GenYielded := False;
          VS[J] := RootAt(TempTop - 1);
          PopRoots(1);
          Fr := @Frames[FrameTop - 1];
          T := Fr^.Tpl;
          StackTop := Fr^.Base + T^.MaxRegs;
          R := @VS[Fr^.Base];
          Code := @T^.Code[0];
          IP := @Code[Fr^.PC];
          K := T^.KBase;
          ICs := T^.ICBase;
          Changed := True;
          { the consumer's compiled code past its opIterNext }
          {$IFDEF CPUX86_64}
          if (JitEnabled = 1) and (T^.JitState = 1) then
          begin
            CI := XJitRun(Fr, Fr^.PC);
            if CI < 0 then begin InjectThrow := True; Exit(-1); end;
            IP := @Code[CI];
          end;
          {$ENDIF}
          Continue;
        end;
      opRet, opRetUndef:
        begin
          { a call's or a construction's return to a bytecode caller inside this Execute }
          if (FrameTop - 1 = StopFrame) or MultiRealmAny then goto LBail;
          if TOp(PByte(IP)[-8]) = opRet then V := R[A] else V := VUndefined;
          if Fr^.Kind <> 0 then
          begin
            if Fr^.Kind = 4 then
            begin
              { a for-of's generator completes (as Execute): done, its consumer's loop ends }
              XGenInlineReturn(Fr);
              Fr := @Frames[FrameTop - 1];
              T := Fr^.Tpl;
              StackTop := Fr^.Base + T^.MaxRegs;
              R := @VS[Fr^.Base];
              Code := @T^.Code[0];
              IP := @Code[Fr^.PC];
              K := T^.KBase;
              ICs := T^.ICBase;
              Changed := True;
              {$IFDEF CPUX86_64}
              if (JitEnabled = 1) and (T^.JitState = 1) then
              begin
                CI := XJitRun(Fr, Fr^.PC);
                if CI < 0 then begin InjectThrow := True; Exit(-1); end;
                IP := @Code[CI];
              end;
              {$ENDIF}
              Continue;
            end;
            if (Fr^.Kind <> 1) and (Fr^.Kind <> 5) then goto LBail;
            if not IsObject(V) then
            begin
              { 10.2.2 [[Construct]] steps 10-12 (Kind 1, and Kind 5's super() construction); a derived
                constructor's checks are Execute's }
              T := Fr^.Tpl;
              { a derived constructor: undefined with this bound returns this; its throws are Execute's }
              if T^.IsDerived and T^.IsClassCtor and ((V <> VUndefined) or (R[0] = VHole)) then goto LBail;
              V := R[0];
              if IsObject(V) and (T^.ExpectedProps < ObjOf(V)^.Shape^.Count) then
                if ObjOf(V)^.Shape^.Count < 64 then T^.ExpectedProps := ObjOf(V)^.Shape^.Count else T^.ExpectedProps := 64;
            end;
          end;
          { CloseUpvals, once none of the frame's upvalues needs the write barrier to remember it }
          J := OpenCount;
          { a template that cannot capture its registers has nothing open here: no scan, nothing to close }
          if not Fr^.Tpl^.NoCaptures then
          while (J > 0) and (PUpval(PtrOf(OpenUpvals[J - 1]))^.StackIndex >= Fr^.Base) do
          begin
            U := PUpval(PtrOf(OpenUpvals[J - 1]));
            VB := VS[U^.StackIndex];
            if ((U^.Hdr.Flags and (cfOld or cfRemembered)) = cfOld) and ((VB shr 48) = TAG_PTR_HI) and
               (PByte(PtrUInt(VB and PTR_MASK)) >= NS) and (PByte(PtrUInt(VB and PTR_MASK)) < NL) then goto LBail;
            Dec(J);
          end;
          while OpenCount > J do
          begin
            U := PUpval(PtrOf(OpenUpvals[OpenCount - 1]));
            U^.Value := VS[U^.StackIndex];
            U^.StackIndex := -1;
            Dec(OpenCount);
          end;
          if not Fr^.Tpl^.NoHandlers then
            while (HandlerTop > 0) and (Handlers[HandlerTop - 1].Frame >= FrameTop - 1) do Dec(HandlerTop);
          Dec(FrameTop);
          if Fr^.Kind = 5 then
          begin
            { super()'s construction: this bound in the frame that called it, its field initializers' frame
              above it (XSuperReturn); a second super() throws from that frame }
            if not XSuperReturn(Fr, V) then
            begin
              InjectThrow := True;
              Exit(-1);
            end;
            Fr := @Frames[FrameTop - 1];
          end
          else
          begin
            VS[Fr^.RetIdx] := V;
            Dec(Fr);   { the caller's frame: Frames never moves (allocated once, FRAME_MAX long) }
          end;
          T := Fr^.Tpl;
          StackTop := Fr^.Base + T^.MaxRegs;
          Code := @T^.Code[0];
          IP := @Code[Fr^.PC];
          R := @VS[Fr^.Base];
          K := T^.KBase;
          ICs := T^.ICBase;
          Changed := True;
          {$IFDEF CPUX86_64}
          if (JitEnabled = 1) and (T^.JitState = 1) then
          begin
            CI := XJitRun(Fr, Fr^.PC);
            if CI < 0 then begin InjectThrow := True; Exit(-1); end;
            IP := @Code[CI];
          end;
          {$ENDIF}
        end;
    else
      goto LBail;
    end;
  end;
LBail:
  { the instruction at PC - 1 is Execute's }
  PC := Integer((PtrUInt(IP) - PtrUInt(Code)) shr 3);
  {$IFDEF G2_BAILSTATS}Inc(BailCounts[Code[PC - 1] and $FF]);{$ENDIF}
  if Changed then
  begin
    Fr^.PC := PC - 1;
    Exit(-1);
  end;
  Result := PC - 1;
end;

{ Execute's frame Fr runs in its function's realm }
procedure SyncFrameRealm(Fr: PFrame); noinline;
begin
  if IsObject(Fr^.Closure) and (ObjOf(Fr^.Closure)^.OKind = okFunction) and
     (FnRealm(ObjOf(Fr^.Closure)) <> CurRealm) then
    SwitchRealm(FnRealm(ObjOf(Fr^.Closure)));
end;

{ 10.2.2 [[Construct]] steps 10-12 of a derived constructor: its errors come after the caller's context is
  restored, so from the caller's realm (Kind 0 the TypeError of a non-object result, 1 the ReferenceError of an
  uninitialized this; EntryRealm the realm Execute was called from) }
procedure XThrowConstructResult(Kind, StopFrame, EntryRealm: Integer); noinline;
var R0, RC: Integer;
begin
  R0 := CurRealm;
  RC := R0;
  if MultiRealm then
    if (FrameTop - 1 > StopFrame) and IsObject(Frames[FrameTop - 2].Closure) and
       (ObjOf(Frames[FrameTop - 2].Closure)^.OKind = okFunction) then RC := FnRealm(ObjOf(Frames[FrameTop - 2].Closure))
    else if FrameTop - 1 = StopFrame then RC := EntryRealm;
  SwitchRealm(RC);
  if Kind = 0 then ThrowTypeError('Derived constructors may only return object or undefined')
  else ThrowError('ReferenceError', 'Must call super constructor in derived class before accessing ''this'' or returning from derived constructor');
  SwitchRealm(R0);
end;

function Execute(StopFrame: Integer; var Res: TValue): Boolean;
label LThrow, LReload, LNext, LDone;
var
  Fr: PFrame;
  Tpl: PFuncTemplate;
  Code: PQWord;
  PC: Integer;
  R: PValue;
  K: PValue;
  I: QWord;
  A, B, C: Integer;
  VA, VB, V: TValue;
  O, O2: PObj;
  IC: PPropIC;
  Sum: Int64;
  D: Double;
  E: PValueArray;
  Idx: Int64;
  U: PUpval;
  Ups: PValueArray;
  T: PFuncTemplate;
  CI, NP, Argc, Base, J, Cmp: Integer;
  Key: PStr;
  Bool: Boolean;
  H: ^THandler;
  Done: Boolean;
  Ok: Boolean;
  SavedTop: Integer;
  NP2: Integer;
  ICB: PPropIC;
  R0: Integer;
begin
  R0 := CurRealm;   { the caller's realm, restored when this returns }
LReload:
  Fr := @Frames[FrameTop - 1];
  { the running function's realm (a call or a return crossed into another) }
  if MultiRealmAny then SyncFrameRealm(Fr);
  Tpl := Fr^.Tpl;
  StackTop := Fr^.Base + Tpl^.MaxRegs;
  Code := @Tpl^.Code[0];
  PC := Fr^.PC;
  R := @VStack[Fr^.Base];
  K := Tpl^.KBase;
  ICB := Tpl^.ICBase;
  if InjectThrow then
  begin
    InjectThrow := False;
    goto LThrow;
  end;
  while True do
  begin
    {$IFNDEF G2_OPSTATS}
    if FastOp[Code[PC] and $FF] then
    begin
      PC := FastLoop(Code, PC, R, K, ICB, Fr, StopFrame);
      if PC < 0 then goto LReload;
    end;
    {$ENDIF}
    I := Code[PC];
    A := PWord(PByte(@Code[PC]) + 1)^;
    Inc(PC);
    {$IFDEF G2_OPSTATS}Inc(OpCounts[I and $FF]); Inc(PairCounts[PrevOp * 256 + (I and $FF)]); PrevOp := I and $FF;{$ENDIF}
    case TOp(I and $FF) of
      opNop: ;
      opMov: R[A] := R[(I shr 24) and $FFFF];
      opLoadK: R[A] := K[I shr 40];
      opLoadI: R[A] := MkInt(SarInt64(Int64(I), 40));
      opLoadUndef: R[A] := VUndefined;
      opLoadNull: R[A] := VNull;
      opLoadTrue: R[A] := VTrue;
      opLoadFalse: R[A] := VFalse;
      opLoadHole: R[A] := VHole;

      opGetUpv:
        begin
          Ups := PValueArray(PtrOf(ObjOf(Fr^.Closure)^.Aux2));
          U := PUpval(PtrOf(Ups^.Data[(I shr 24) and $FFFF]));
          if U^.StackIndex >= 0 then R[A] := VStack[U^.StackIndex] else R[A] := U^.Value;
        end;
      opSetUpv:
        begin
          Ups := PValueArray(PtrOf(ObjOf(Fr^.Closure)^.Aux2));
          U := PUpval(PtrOf(Ups^.Data[(I shr 24) and $FFFF]));
          if U^.StackIndex >= 0 then VStack[U^.StackIndex] := R[A]
          else
          begin
            U^.Value := R[A];
            WriteBarrier(@U^.Hdr, R[A]);
          end;
        end;
      opClose: CloseUpvals(Fr^.Base + A);

      opGetGlobal, opTypeofGlobal:
        begin
          IC := @Tpl^.ICs[I shr 40];
          O := ObjOf(GlobalObj);
          if O^.Shape = IC^.Shape then R[A] := GetSlot(O, IC^.Slot)
          else
          begin
            { a global lexical binding comes before the global object (9.1.1.4.1 HasBinding order); creating
              one that shadows a property forks the global object's shape, so a hit above cannot skip it }
            CI := GLexFind(IC^.Key);
            if CI >= 0 then
            begin
              { a global lexical binding is never removed: FastLoop reads its cell from the IC (Depth -2 - index) }
              IC^.Shape := nil;
              IC^.Depth := -2 - CI;
              Fr^.PC := PC;
              if not GLexGet(CI, V) then goto LThrow;   { typeof of a binding in its TDZ throws too }
              R[A] := V;
              if TOp(I and $FF) = opTypeofGlobal then R[A] := TypeofValue(R[A]);
              Continue;
            end;
            CI := ShapeLookup(O^.Shape, IC^.Key);
            if CI < 0 then
            begin
              { the global object inherits from Object.prototype: a bare hasOwnProperty resolves there }
              Fr^.PC := PC;
              if not InheritedHasV(O, IC^.Key, Bool) then goto LThrow;
              if Bool then
              begin
                Fr^.PC := PC;
                if not GetV(GlobalObj, IC^.Key, V) then goto LThrow;
                R[A] := V;
                if TOp(I and $FF) = opTypeofGlobal then R[A] := TypeofValue(R[A]);
                Continue;
              end;
              if TOp(I and $FF) = opTypeofGlobal then begin R[A] := MkPtr(AtomOf('undefined')); Continue; end;
              Fr^.PC := PC;
              XThrowNotDefined(IC^.Key);
              goto LThrow;
            end;
            if (O^.Shape^.PFlags[CI] and pfAccessor) = 0 then
            begin
              IC^.Shape := O^.Shape; IC^.Slot := CI;
              R[A] := GetSlot(O, CI);
            end
            else
            begin
              Fr^.PC := PC;
              if not GetV(GlobalObj, IC^.Key, V) then goto LThrow;
              R[A] := V;
            end;
          end;
          if TOp(I and $FF) = opTypeofGlobal then R[A] := TypeofValue(R[A]);
        end;
      opSetGlobal:
        begin
          IC := @Tpl^.ICs[I shr 40];
          O := ObjOf(GlobalObj);
          if O^.Shape = IC^.Shape then SetSlot(O, IC^.Slot, R[A])
          else
          begin
            Fr^.PC := PC;
            CI := GLexFind(IC^.Key);
            if CI >= 0 then
            begin
              if not GLexSet(CI, R[A]) then goto LThrow;
              Continue;
            end;
            CI := ShapeLookup(O^.Shape, IC^.Key);
            if (CI >= 0) and ((O^.Shape^.PFlags[CI] and (pfAccessor or pfWritable)) = pfWritable) then
            begin
              { an own writable data property: cache it (the global object's dictionary shape keeps its
                identity while properties are added, and gets a new one when one is deleted or changed) }
              IC^.Shape := O^.Shape; IC^.Slot := CI;
              SetSlot(O, CI, R[A]);
              Continue;
            end;
            { 6.2.5.6 PutValue: an unresolvable reference in strict code throws }
            if (CI < 0) and Tpl^.IsStrict then
            begin
              if not InheritedHasV(O, IC^.Key, Bool) then goto LThrow;
              if not Bool then
              begin
                XThrowNotDefined(IC^.Key);
                goto LThrow;
              end;
            end;
            if not SetVS(GlobalObj, IC^.Key, R[A], Tpl^.IsStrict) then goto LThrow;
          end;
        end;
      opDeclGlobals:
        begin
          Fr^.PC := PC;
          if not GlobalDeclInst(Tpl) then goto LThrow;
        end;
      opDefGlobalFunc: CreateGlobalFunctionBinding(PStr(PtrOf(K[I shr 40])), R[A], ((I shr 24) and $FFFF) = 1);
      opMakeGLex: GLexCells[Tpl^.GdiLexBase + Integer(I shr 40)] := FindOrOpenUpval(Fr^.Base + A);
      opGetGLex:
        begin
          if not GLexGet(I shr 40, V) then begin Fr^.PC := PC; goto LThrow; end;
          R[A] := V;
        end;
      opSetGLex:
        if not GLexSet(I shr 40, R[A]) then begin Fr^.PC := PC; goto LThrow; end;
      opNewPrivName:
        R[A] := MkPtr(XPrivateKeyAt(Tpl, PC, PStr(PtrOf(K[I shr 40]))));
      opGetPriv:
        begin
          Fr^.PC := PC;
          if not PrivateGet(R[(I shr 24) and $FFFF], PStr(PtrOf(R[I shr 40])), V) then goto LThrow;
          R[A] := V;
        end;
      opSetPriv:
        begin
          Fr^.PC := PC;
          if not PrivateSet(R[A], PStr(PtrOf(R[I shr 40])), R[(I shr 24) and $FFFF]) then goto LThrow;
        end;
      opDefPrivField, opAddPriv:
        begin
          Fr^.PC := PC;
          Key := PStr(PtrOf(R[I shr 40]));
          if not IsObject(R[A]) then begin ThrowTypeError('Cannot add a private element to a primitive'); goto LThrow; end;
          if ShapeLookup(ObjOf(R[A])^.Shape, Key) >= 0 then
          begin
            if TOp(I and $FF) = opDefPrivField then
              XThrowInitTwice(Key)
            else
              ThrowTypeError('Cannot initialize private methods of class twice on the same object');
            goto LThrow;
          end;
          { non-extensible applies to private: no new private element on a non-extensible object }
          if (ObjOf(R[A])^.OKind <> okProxy) and ((ObjOf(R[A])^.OFlags and 8) <> 0) then
          begin
            ThrowTypeError('Cannot add a private element to a non-extensible object');
            goto LThrow;
          end;
          B := (I shr 24) and $FFFF;
          if TOp(I and $FF) = opDefPrivField then AddProperty(ObjOf(R[A]), Key, R[B], pfWritable)
          else if (B and $8000) = 0 then AddProperty(ObjOf(R[A]), Key, R[B], 0)
          else
          begin
            B := B and $7FFF;
            VB := MkPtr(NewAccessor(R[B], R[B + 1]));
            AddProperty(ObjOf(R[A]), Key, VB, pfAccessor);
          end;
        end;
      opPrivIn:
        begin
          VB := R[(I shr 24) and $FFFF];
          if not IsObject(VB) then
          begin
            Fr^.PC := PC;
            ThrowTypeError('Cannot use ''in'' operator to search for a private field in a non-object');
            goto LThrow;
          end;
          if ShapeLookup(ObjOf(VB)^.Shape, PStr(PtrOf(R[I shr 40]))) >= 0 then R[A] := VTrue else R[A] := VFalse;
        end;
      opDefField, opDefFieldC:
        begin
          Fr^.PC := PC;
          if TOp(I and $FF) = opDefField then
          begin
            { a field's CreateDataPropertyOrThrow ignores the prototype chain: an object with the cached shape
              (extensible, without the key: preventExtensions forks the shape) takes the cached add transition }
            IC := @Tpl^.ICs[I shr 40];
            Key := IC^.Key;
            O := ObjOf(R[A]);
            if (O^.Shape = IC^.Shape) and (IC^.NewShape <> nil) and (IC^.Slot < O^.InlineCap) and
               ((O^.OFlags and 8) = 0) then
            begin
              V := R[(I shr 24) and $FFFF];
              O^.Shape := IC^.NewShape;
              O^.Slots[IC^.Slot] := V;
              WriteBarrier(@O^.Hdr, V);
              Continue;
            end;
            VB := MkPtr(O^.Shape);
            if not CreateDataPropOrThrow(R[A], Key, R[(I shr 24) and $FFFF]) then goto LThrow;
            O := ObjOf(R[A]);
            if (O^.OKind = okPlain) and not O^.Shape^.IsDict and (O^.Shape^.Parent = PShape(PtrOf(VB))) and
               (O^.Shape^.Count > 0) and (O^.Shape^.Keys[O^.Shape^.Count - 1] = Key) then
            begin
              IC^.Shape := PShape(PtrOf(VB));
              IC^.NewShape := O^.Shape;
              IC^.Slot := O^.Shape^.Count - 1;
            end;
            Continue;
          end
          else if not ToPropKey(R[I shr 40], Key) then goto LThrow;
          if not CreateDataPropOrThrow(R[A], Key, R[(I shr 24) and $FFFF]) then goto LThrow;
        end;
      opSetHome:
        begin
          ObjOf(R[A])^.Aux1 := R[(I shr 24) and $FFFF];
          WriteBarrier(@ObjOf(R[A])^.Hdr, R[(I shr 24) and $FFFF]);
        end;
      opCheckThis:
        if R[A] = VHole then
        begin
          Fr^.PC := PC;
          ThrowError('ReferenceError', 'Must call super constructor in derived class before accessing ''this'' or returning from derived constructor');
          goto LThrow;
        end;
      opEvalHint:
        begin
          { 13.3.6.1 step 6: direct only when the callee (the next call's base register) is this realm's %eval%
            itself: not a bound eval, nor another realm's }
          if R[(Code[PC] shr 8) and $FFFF] = RealmEvalFn then
          begin
            EvalHintMode := A + 1; EvalHintFrame := FrameTop; EvalHintPC := PC; EvalHintDesc := I shr 40;
          end
          else EvalHintMode := 0;
        end;

      opGetProp, opGetLength, opGetMethod:
        begin
          if TOp(I and $FF) = opGetMethod then
          begin
            { this := the receiver; the rest is opGetProp's, reading the receiver from A + 1 }
            R[A + 1] := R[(I shr 24) and $FFFF];
            I := (I and $FFFFFF0000FFFF00) or (QWord(A + 1) shl 24) or QWord(Ord(opGetProp));
          end;
          VB := R[(I shr 24) and $FFFF];
          IC := @Tpl^.ICs[I shr 40];
          if (VB shr 48) = TAG_PTR_HI then
          begin
            O := PObj(PtrUInt(VB and PTR_MASK));
            if O^.Hdr.Kind = ckObject then
            begin
              if ICGet(IC, O, R[A]) then Continue;
              if (TOp(I and $FF) = opGetLength) and (O^.OKind = okArray) and ((O^.OFlags and ofBigLen) = 0) then
              begin
                R[A] := MkInt(O^.Elements^.Len);
                Continue;
              end;
              { the intrinsic %TypedArray%.prototype.length getter's value, for a typed array without own
                properties (a redefined getter on the prototype is not seen here) }
              if (TOp(I and $FF) = opGetLength) and (O^.OKind = okTypedArray) and (O^.Shape^.Count = 0) then
              begin
                R[A] := MkInt(TALength(O));
                Continue;
              end;
              Fr^.PC := PC;
              { an accessor entry: its getter called without the miss path's walk }
              J := ICAccGet(IC, O, VB, V);
              if J = 0 then goto LThrow;
              if J = 1 then begin R[A] := V; Continue; end;
              if not ICGetMiss(IC, O, VB, V) then goto LThrow;
              R[A] := V;
              Continue;
            end
            else if O^.Hdr.Kind = ckString then
            begin
              if TOp(I and $FF) = opGetLength then
              begin
                R[A] := MkInt(PStr(O)^.Len);
                Continue;
              end;
              O := ObjOf(StringProto);
              if O^.Shape = IC^.Shape then
              begin
                R[A] := GetSlot(O, IC^.Slot);
                Continue;
              end;
              ICFillString(IC);
            end;
          end;
          Fr^.PC := PC;
          if not GetV(VB, IC^.Key, V) then goto LThrow;
          R[A] := V;
        end;
      opSetProp:
        begin
          VA := R[A];
          IC := @Tpl^.ICs[I shr 40];
          V := R[(I shr 24) and $FFFF];
          if IsObject(VA) then
          begin
            O := ObjOf(VA);
            if O^.Shape = IC^.Shape then
            begin
              if IC^.NewShape = nil then
              begin
                SetSlot(O, IC^.Slot, V);
                Continue;
              end;
              if IC^.Slot < O^.InlineCap then
              begin
                O^.Shape := IC^.NewShape;
                O^.Slots[IC^.Slot] := V;
                WriteBarrier(@O^.Hdr, V);
                Continue;
              end;
              { an add into the overflow slots' spare capacity }
              if (O^.Overflow <> nil) and (IC^.Slot - O^.InlineCap < Integer(O^.Overflow^.Cap)) then
              begin
                if Integer(O^.Overflow^.Len) <= IC^.Slot - O^.InlineCap then O^.Overflow^.Len := IC^.Slot - O^.InlineCap + 1;
                O^.Shape := IC^.NewShape;
                SetSlot(O, IC^.Slot, V);
                Continue;
              end;
            end;
          end;
          Fr^.PC := PC;
          if not XSetPropSlow(IC, @R[A], V, Tpl^.IsStrict) then goto LThrow;
        end;

      opGetElem:
        begin
          VA := R[(I shr 24) and $FFFF];
          VB := R[I shr 40];
          if IsInt(VB) and IsObject(VA) then
          begin
            O := ObjOf(VA);
            E := O^.Elements;
            CI := IntOf(VB);
            if (E <> nil) and (CI >= 0) and (CI < Integer(E^.Len)) and (E^.Data[CI] <> VHole) then
            begin
              R[A] := E^.Data[CI];
              Continue;
            end;
            if (O^.OKind = okTypedArray) and (CI >= 0) and (CI < TALength(O)) then
            begin
              R[A] := TAGet(O, CI);
              Continue;
            end;
          end;
          Fr^.PC := PC;
          if not GetElemV(VA, VB, V) then goto LThrow;
          R[A] := V;
        end;
      opSetElem:
        begin
          VA := R[A];
          VB := R[(I shr 24) and $FFFF];
          V := R[I shr 40];
          if IsInt(VB) and IsObject(VA) then
          begin
            O := ObjOf(VA);
            E := O^.Elements;
            CI := IntOf(VB);
            if (E <> nil) and (CI >= 0) and (CI < Integer(E^.Len)) and ((O^.OFlags and 64) = 0) and (E^.Data[CI] <> VHole) then
            begin
              E^.Data[CI] := V;
              WriteBarrier(@E^.Hdr, V);
              Continue;
            end;
            if (E <> nil) and (CI = Integer(E^.Len)) and (CI < Integer(E^.Cap)) and ((O^.OFlags and 64) = 0) and
               (O^.OKind = okArray) and AppendProtosPlain(O) then
            begin
              E^.Data[CI] := V;
              WriteBarrier(@E^.Hdr, V);
              E^.Len := CI + 1;
              Continue;
            end;
            if (O^.OKind = okTypedArray) and (CI >= 0) and (CI < TALength(O)) and not TAIsBig(O) and
               not ViewImmutable(O) then
            begin
              if IsInt(V) then begin TASetInt(O, CI, IntOf(V)); Continue; end;
              if IsDouble(V) then begin TASet(O, CI, DoubleOf(V)); Continue; end;
            end;
          end;
          Fr^.PC := PC;
          if not SetElemVS(VA, VB, V, Tpl^.IsStrict) then goto LThrow;
        end;
      opDelProp:
        begin
          VB := R[(I shr 24) and $FFFF];
          Bool := True;
          if IsObject(VB) and (ObjOf(VB)^.OKind = okProxy) then
          begin
            Fr^.PC := PC;
            if not ProxyDelete(VB, PStr(PtrOf(K[I shr 40])), Bool) then goto LThrow;
          end
          else if IsObject(VB) then Bool := DeleteIfConfigurable(ObjOf(VB), PStr(PtrOf(K[I shr 40])))
          else
          begin
            Fr^.PC := PC;
            if not XDeletePrim(VB, VUndefined, PStr(PtrOf(K[I shr 40])), Tpl^.IsStrict, Bool) then goto LThrow;
          end;
          { 13.5.1.2 step 5.e: in strict code a delete that returns false is a TypeError }
          if not Bool and Tpl^.IsStrict then
          begin
            Fr^.PC := PC;
            XThrowCannotDelete(PStr(PtrOf(K[I shr 40])));
            goto LThrow;
          end;
          R[A] := MkBool(Bool);
        end;
      opDelElem:
        begin
          VA := R[(I shr 24) and $FFFF];
          VB := R[I shr 40];
          Fr^.PC := PC;
          Bool := True;
          if IsObject(VA) then
          begin
            O := ObjOf(VA);
            if IndexFromValue(VB, Idx) and (O^.Elements <> nil) and (Idx < O^.Elements^.Len) and
               ((O^.OFlags and 64) = 0) and (O^.OKind <> okTypedArray) then
              O^.Elements^.Data[Idx] := VHole
            else
            begin
              if not ToPropKey(VB, Key) then goto LThrow;
              if ObjOf(R[(I shr 24) and $FFFF])^.OKind = okProxy then
              begin
                if not ProxyDelete(R[(I shr 24) and $FFFF], Key, Bool) then goto LThrow;
              end
              else Bool := DeleteIfConfigurable(ObjOf(R[(I shr 24) and $FFFF]), Key);
              if not Bool and Tpl^.IsStrict then
              begin
                XThrowCannotDelete(Key);
                goto LThrow;
              end;
            end;
          end
          else if not XDeletePrim(VA, VB, nil, Tpl^.IsStrict, Bool) then goto LThrow;
          R[A] := MkBool(Bool);
        end;
      opIn:
        begin
          VA := R[(I shr 24) and $FFFF];   { key }
          VB := R[I shr 40];               { object }
          Fr^.PC := PC;
          if not IsObject(VB) then begin ThrowTypeError('Cannot use ''in'' operator to search for a key in a non-object'); goto LThrow; end;
          O := ObjOf(VB);
          Bool := False;
          if IndexFromValue(VA, Idx) and (O^.Elements <> nil) and (Idx < O^.Elements^.Len) and
             (O^.Elements^.Data[Idx] <> VHole) then Bool := True
          else
          begin
            if not ToPropKey(VA, Key) then goto LThrow;
            if not HasPropertyV(R[I shr 40], Key, Bool) then goto LThrow;
          end;
          R[A] := MkBool(Bool);
        end;
      opInstanceof:
        begin
          Fr^.PC := PC;
          if not InstanceOf(R[(I shr 24) and $FFFF], R[I shr 40], Bool) then goto LThrow;
          R[A] := MkBool(Bool);
        end;

      opLoadHome: R[A] := ObjOf(Fr^.Closure)^.Aux1;
      opLoadNewTarget: R[A] := Fr^.NewTarget;
      opDelGlobal:
        begin
          Key := PStr(PtrOf(K[I shr 40]));
          if GLexFind(Key) >= 0 then R[A] := VFalse
          else R[A] := MkBool(DeleteIfConfigurable(ObjOf(GlobalObj), Key));
        end;
      opSetSuper:
        begin
          Fr^.PC := PC;
          CI := SarInt64(Int64(I), 40);
          if CI < 0 then
          begin
            if not ToPropKey(R[-1 - CI], Key) then goto LThrow;
          end
          else Key := Tpl^.ICs[CI].Key;
          VB := R[(I shr 24) and $FFFF];   { home object }
          if IsObject(VB) then VA := ObjOf(VB)^.Shape^.Proto else VA := VUndefined;
          if not IsObject(VA) then begin ThrowNullishBase(VA, MkPtr(Key), True); goto LThrow; end;
          if not SetReceiverHook(VA, Key, R[A], R[A + 1], Bool) then goto LThrow;
          if not Bool and Tpl^.IsStrict then begin XThrowReadOnlySuper(Key); goto LThrow; end;
        end;
      opGetSuper:
        begin
          Fr^.PC := PC;
          CI := SarInt64(Int64(I), 40);
          if CI < 0 then VA := R[-1 - CI] else VA := VUndefined;
          if not XGetSuper(R[(I shr 24) and $FFFF], VA, CI, Tpl, R[0], V) then goto LThrow;
          R[A] := V;
        end;

      opAdd:
        begin
          VA := R[(I shr 24) and $FFFF];
          VB := R[I shr 40];
          if ((VA and VB and TAG_MASK) = TAG_INT) and ((VA shr 48) = TAG_INT_HI) and ((VB shr 48) = TAG_INT_HI) then
          begin
            Sum := Int64(Int32(UInt32(VA))) + Int64(Int32(UInt32(VB)));
            if (Sum >= -2147483648) and (Sum <= 2147483647) then R[A] := TAG_INT or QWord(UInt32(Sum))
            else R[A] := MkDouble(Sum);
          end
          else if IsNumber(VA) and IsNumber(VB) then
            R[A] := MkDouble(NumOf(VA) + NumOf(VB))
          else if IsString(VA) and IsString(VB) then
            R[A] := ConcatStrings(StrOf(VA), StrOf(VB))
          else
          begin
            Fr^.PC := PC;
            if not AddNumbersSlow(VA, VB, V) then goto LThrow;
            R[A] := V;
          end;
        end;
      opTemplateObj:
        begin
          Fr^.PC := PC;
          if not XTemplateObj((I shr 24) and $FFFF, I shr 40, R[A], V) then goto LThrow;
          R[A] := V;
        end;
      opAddI, opSubI, opIncI:
        begin
          VA := R[(I shr 24) and $FFFF];
          CI := SarInt64(Int64(I), 40);
          if (VA shr 48) = TAG_INT_HI then
          begin
            Sum := Int64(Int32(UInt32(VA))) + CI;
            if (Sum >= -2147483648) and (Sum <= 2147483647) then R[A] := TAG_INT or QWord(UInt32(Sum))
            else R[A] := MkDouble(Sum);
          end
          else if VA < DOUBLE_LIMIT then R[A] := MkDouble(PDouble(@VA)^ + CI)
          else
          begin
            Fr^.PC := PC;
            if not XAddImmSlow(TOp(I and $FF), VA, CI, V) then goto LThrow;
            R[A] := V;
          end;
        end;
      opSub, opMul:
        begin
          VA := R[(I shr 24) and $FFFF];
          VB := R[I shr 40];
          if ((VA shr 48) = TAG_INT_HI) and ((VB shr 48) = TAG_INT_HI) then
          begin
            if TOp(I and $FF) = opSub then Sum := Int64(Int32(UInt32(VA))) - Int64(Int32(UInt32(VB)))
            else Sum := Int64(Int32(UInt32(VA))) * Int64(Int32(UInt32(VB)));
            if (Sum >= -2147483648) and (Sum <= 2147483647) and not ((Sum = 0) and (TOp(I and $FF) = opMul) and
               ((Int32(UInt32(VA)) < 0) or (Int32(UInt32(VB)) < 0))) then
              R[A] := TAG_INT or QWord(UInt32(Sum))
            else if TOp(I and $FF) = opSub then R[A] := MkDouble(Sum)
            else R[A] := MkDouble(Double(Int32(UInt32(VA))) * Double(Int32(UInt32(VB))));
          end
          else if IsNumber(VA) and IsNumber(VB) then
          begin
            if TOp(I and $FF) = opSub then R[A] := MkDouble(NumOf(VA) - NumOf(VB))
            else R[A] := MkDouble(NumOf(VA) * NumOf(VB));
          end
          else
          begin
            Fr^.PC := PC;
            if not ArithSlow(TOp(I and $FF), VA, VB, V) then goto LThrow;
            R[A] := V;
          end;
        end;
      opDiv:
        begin
          VA := R[(I shr 24) and $FFFF];
          VB := R[I shr 40];
          if IsNumber(VA) and IsNumber(VB) then R[A] := MkNumber(NumOf(VA) / NumOf(VB))
          else
          begin
            Fr^.PC := PC;
            if not ArithSlow(opDiv, VA, VB, V) then goto LThrow;
            R[A] := V;
          end;
        end;
      opMod:
        begin
          VA := R[(I shr 24) and $FFFF];
          VB := R[I shr 40];
          if ((VA shr 48) = TAG_INT_HI) and ((VB shr 48) = TAG_INT_HI) and (Int32(UInt32(VB)) > 0) and
             (Int32(UInt32(VA)) >= 0) then
            R[A] := MkInt(Int32(UInt32(VA)) mod Int32(UInt32(VB)))
          else if IsNumber(VA) and IsNumber(VB) then
            R[A] := MkNumber(NumberRemainder(NumOf(VA), NumOf(VB)))
          else
          begin
            Fr^.PC := PC;
            if not ArithSlow(opMod, VA, VB, V) then goto LThrow;
            R[A] := V;
          end;
        end;
      opBitAndI, opBitOrI, opBitXorI, opShlI, opShrI, opUShrI:
        begin
          Fr^.PC := PC;
          if not XBitImm(TOp(I and $FF), R[(I shr 24) and $FFFF], Int32(SarInt64(Int64(I), 40)), V) then goto LThrow;
          R[A] := V;
        end;
      opPow, opShr, opUShr:
        begin
          VA := R[(I shr 24) and $FFFF];
          VB := R[I shr 40];
          if (TOp(I and $FF) <> opPow) and QuickInt32(VA, CI) and QuickInt32(VB, NP) then
          begin
            if TOp(I and $FF) = opShr then R[A] := MkInt(SarLongint(CI, NP and 31))
            else R[A] := MkNumber(UInt32(CI) shr (NP and 31));
          end
          else if (TOp(I and $FF) = opPow) and IsNumber(VA) and IsNumber(VB) then
            R[A] := MkNumber(NumberExponentiate(NumOf(VA), NumOf(VB)))
          else
          begin
            Fr^.PC := PC;
            if not ArithSlow(TOp(I and $FF), VA, VB, V) then goto LThrow;
            R[A] := V;
          end;
        end;
      opBitAnd, opBitOr, opBitXor, opShl:
        begin
          VA := R[(I shr 24) and $FFFF];
          VB := R[I shr 40];
          if ((VA shr 48) = TAG_INT_HI) and ((VB shr 48) = TAG_INT_HI) then
          begin
            case TOp(I and $FF) of
              opBitAnd: R[A] := MkInt(IntOf(VA) and IntOf(VB));
              opBitOr: R[A] := MkInt(IntOf(VA) or IntOf(VB));
              opBitXor: R[A] := MkInt(IntOf(VA) xor IntOf(VB));
            else R[A] := MkInt(Int32(UInt32(IntOf(VA)) shl (IntOf(VB) and 31)));
            end;
          end
          else if QuickInt32(VA, CI) and QuickInt32(VB, NP) then
          begin
            { a double operand within int32 range: ToInt32 is its truncation }
            case TOp(I and $FF) of
              opBitAnd: R[A] := MkInt(CI and NP);
              opBitOr: R[A] := MkInt(CI or NP);
              opBitXor: R[A] := MkInt(CI xor NP);
            else R[A] := MkInt(Int32(UInt32(CI) shl (NP and 31)));
            end;
          end
          else
          begin
            Fr^.PC := PC;
            if not ArithSlow(TOp(I and $FF), VA, VB, V) then goto LThrow;
            R[A] := V;
          end;
        end;
      opNeg:
        begin
          VA := R[(I shr 24) and $FFFF];
          if IsInt(VA) and (IntOf(VA) <> 0) and (IntOf(VA) <> Low(Int32)) then R[A] := MkInt(-IntOf(VA))
          else if IsBigInt(VA) then R[A] := XBigNegate(VA)
          else
          begin
            Fr^.PC := PC;
            if not ToNumericV(VA, V) then goto LThrow;
            if IsBigInt(V) then R[A] := XBigNegate(V) else R[A] := MkDouble(-NumOf(V));
          end;
        end;
      opBitNot:
        begin
          VA := R[(I shr 24) and $FFFF];
          if IsInt(VA) then R[A] := MkInt(not IntOf(VA))
          else
          begin
            { 13.5.6: ~x of a BigInt is -x - 1 }
            Fr^.PC := PC;
            if not ToNumericV(VA, V) then goto LThrow;
            if IsBigInt(V) then R[A] := XBigAddInt(XBigNegate(V), -1)
            else if not ArithSlow(opBitXor, V, MkInt(-1), V) then goto LThrow
            else R[A] := V;
          end;
        end;
      opNot: R[A] := MkBool(not ToBool(R[(I shr 24) and $FFFF]));
      opTypeof: R[A] := TypeofValue(R[(I shr 24) and $FFFF]);
      opToNum, opToNumeric:
        begin
          VA := R[(I shr 24) and $FFFF];
          if IsNumber(VA) then R[A] := VA
          else if IsBigInt(VA) and (TOp(I and $FF) = opToNumeric) then R[A] := VA
          else
          begin
            Fr^.PC := PC;
            if not ToNumberV(VA, D) then goto LThrow;
            R[A] := MkNumber(D);
          end;
        end;
      opToStr:
        begin
          VA := R[(I shr 24) and $FFFF];
          if not IsString(VA) then
          begin
            Fr^.PC := PC;
            if not ToStringV(VA, V) then goto LThrow;
            R[A] := V;
          end
          else R[A] := VA;
        end;
      opToPropKey:
        begin
          VA := R[(I shr 24) and $FFFF];
          if not IsInt(VA) then
          begin
            Fr^.PC := PC;
            { C = base + 1: GetValue's ToObject(base) precedes the key's conversion }
            if ((I shr 40) <> 0) and IsObject(VA) and IsNullish(R[(I shr 40) - 1]) then
            begin
              ThrowNullishBase(R[(I shr 40) - 1], VUndefined, False);
              goto LThrow;
            end;
            if not ToPropKey(VA, Key) then goto LThrow;
            if IsPtr(R[(I shr 24) and $FFFF]) and (PCell(PtrOf(R[(I shr 24) and $FFFF]))^.Kind = ckSymbol) then
              R[A] := R[(I shr 24) and $FFFF]
            else
              R[A] := MkPtr(Key);
          end
          else R[A] := VA;
        end;

      opSEq: R[A] := MkBool(SEqFast(R[(I shr 24) and $FFFF], R[I shr 40]));
      opSNe: R[A] := MkBool(not SEqFast(R[(I shr 24) and $FFFF], R[I shr 40]));
      opEq, opNe:
        begin
          Fr^.PC := PC;
          if not LooseEquals(R[(I shr 24) and $FFFF], R[I shr 40], Bool) then goto LThrow;
          if TOp(I and $FF) = opNe then Bool := not Bool;
          R[A] := MkBool(Bool);
        end;
      opLt, opLe, opGt, opGe:
        begin
          VA := R[(I shr 24) and $FFFF];
          VB := R[I shr 40];
          if IsInt(VA) and IsInt(VB) then
          begin
            case TOp(I and $FF) of
              opLt: Bool := IntOf(VA) < IntOf(VB);
              opLe: Bool := IntOf(VA) <= IntOf(VB);
              opGt: Bool := IntOf(VA) > IntOf(VB);
            else Bool := IntOf(VA) >= IntOf(VB);
            end;
          end
          else if IsNumber(VA) and IsNumber(VB) then
          begin
            case TOp(I and $FF) of
              opLt: Bool := NumOf(VA) < NumOf(VB);
              opLe: Bool := NumOf(VA) <= NumOf(VB);
              opGt: Bool := NumOf(VA) > NumOf(VB);
            else Bool := NumOf(VA) >= NumOf(VB);
            end;
          end
          else
          begin
            Fr^.PC := PC;
            if not CompareSlow(VA, VB, Cmp) then goto LThrow;
            case TOp(I and $FF) of
              opLt: Bool := Cmp = -1;
              opLe: Bool := (Cmp = -1) or (Cmp = 0);
              opGt: Bool := Cmp = 1;
            else Bool := (Cmp = 1) or (Cmp = 0);
            end;
          end;
          R[A] := MkBool(Bool);
        end;

      opJmp: Inc(PC, SarInt64(Int64(I), 40));
      opJT: if ToBool(R[A]) then Inc(PC, SarInt64(Int64(I), 40));
      opJF: if not ToBool(R[A]) then Inc(PC, SarInt64(Int64(I), 40));
      opJNullish: if (R[A] = VUndefined) or (R[A] = VNull) then Inc(PC, SarInt64(Int64(I), 40));
      opJNotNullish: if (R[A] <> VUndefined) and (R[A] <> VNull) then Inc(PC, SarInt64(Int64(I), 40));
      opJUndef: if R[A] = VUndefined then Inc(PC, SarInt64(Int64(I), 40));
      opJNotUndef: if R[A] <> VUndefined then Inc(PC, SarInt64(Int64(I), 40));
      opJNLt, opJNLe, opJNGt, opJNGe:
        begin
          VA := R[A];
          VB := R[(I shr 24) and $FFFF];
          if ((VA shr 48) = TAG_INT_HI) and ((VB shr 48) = TAG_INT_HI) then
          begin
            case TOp(I and $FF) of
              opJNLt: Bool := Int32(UInt32(VA)) < Int32(UInt32(VB));
              opJNLe: Bool := Int32(UInt32(VA)) <= Int32(UInt32(VB));
              opJNGt: Bool := Int32(UInt32(VA)) > Int32(UInt32(VB));
            else Bool := Int32(UInt32(VA)) >= Int32(UInt32(VB));
            end;
          end
          else if IsNumber(VA) and IsNumber(VB) then
          begin
            case TOp(I and $FF) of
              opJNLt: Bool := NumOf(VA) < NumOf(VB);
              opJNLe: Bool := NumOf(VA) <= NumOf(VB);
              opJNGt: Bool := NumOf(VA) > NumOf(VB);
            else Bool := NumOf(VA) >= NumOf(VB);
            end;
          end
          else
          begin
            Fr^.PC := PC;
            if not CompareSlow(VA, VB, Cmp) then goto LThrow;
            case TOp(I and $FF) of
              opJNLt: Bool := Cmp = -1;
              opJNLe: Bool := (Cmp = -1) or (Cmp = 0);
              opJNGt: Bool := Cmp = 1;
            else Bool := (Cmp = 1) or (Cmp = 0);
            end;
          end;
          if not Bool then Inc(PC, SarInt64(Int64(I), 40));
        end;
      opJNSEq: if not SEqFast(R[A], R[(I shr 24) and $FFFF]) then Inc(PC, SarInt64(Int64(I), 40));
      opJSEq: if SEqFast(R[A], R[(I shr 24) and $FFFF]) then Inc(PC, SarInt64(Int64(I), 40));

      opCall, opCallV, opNew, opNewV:
        begin
          VA := R[A];
          { a plain call's this: the compiler sets bit 40 instead of loading undefined first }
          if (I shr 40) and 1 <> 0 then R[A + 1] := VUndefined;
          if (TOp(I and $FF) = opCallV) or (TOp(I and $FF) = opNewV) then
          begin
            { spread call: arguments array in R[A+2] -> registers }
            O := ObjOf(R[A + 2]);
            Argc := ArrayLen(O);
            if Fr^.Base + A + 2 + Argc + 4 >= STACK_MAX then
            begin
              Fr^.PC := PC; ThrowError('RangeError', 'Maximum call stack size exceeded'); goto LThrow;
            end;
            for J := 0 to Argc - 1 do
            begin
              V := O^.Elements^.Data[J];
              if V = VHole then V := VUndefined;
              R[A + 2 + J] := V;
            end;
            if StackTop < Fr^.Base + A + 2 + Argc then StackTop := Fr^.Base + A + 2 + Argc;
          end
          else
            Argc := (I shr 24) and $FFFF;
          Fr^.PC := PC;
          if not IsCallable(VA) then
          begin
            XThrowNotFunction(Tpl, PC - 1, A);
            goto LThrow;
          end;
          O := ObjOf(VA);
          if (TOp(I and $FF) = opNew) or (TOp(I and $FF) = opNewV) then
          begin
            if (O^.OKind = okFunction) then
            begin
              T := PFuncTemplate(O^.Native);
              if T^.IsArrow or (T^.IsMethod and not T^.IsClassCtor) then
              begin
                XThrowNotCtor(T);
                goto LThrow;
              end;
              if T^.IsGenerator or T^.IsAsync then
              begin
                XThrowNotCtor(T);
                goto LThrow;
              end;
              SavedTop := StackTop;
              StackTop := Max(StackTop, Fr^.Base + A + 2 + Argc);
              if T^.IsDerived then
              begin
                { 10.2.2 [[Construct]]: a derived constructor's this is bound by super() }
                R[A + 1] := VHole;
                if not PushBytecodeFrame(ObjOf(VA), VHole, @R[A + 2], Argc, Fr^.Base + A, 1, VA) then goto LThrow;
                goto LReload;
              end;
              { the prototype property's slot and the prototype's root shape from the template's caches }
              if Pointer(O^.Shape) = T^.CtorFnShape then V := GetSlot(O, T^.CtorProtoSlot)
              else if not XCtorProto(T, VA, V) then goto LThrow;
              if not IsObject(V) and not DefaultConstructProto(R[A], R[A], V) then goto LThrow;
              if (T^.CtorRootShape = nil) or (PShape(T^.CtorRootShape)^.Proto <> V) then
              begin
                PushRoot(V);
                T^.CtorRootShape := RootShapeFor(RootAt(TempTop - 1));
                PopRoots(1);
              end;
              O := NewObject(PShape(T^.CtorRootShape), Max(4, T^.ExpectedProps), okPlain);
              R[A + 1] := MkPtr(O);
              VA := R[A];
              StackTop := Max(StackTop, Fr^.Base + A + 2 + Argc);
              if not PushBytecodeFrame(ObjOf(VA), R[A + 1], @R[A + 2], Argc, Fr^.Base + A, 1, VA) then goto LThrow;
              { 10.2.2 step 6.b InitializeInstanceElements before the body: the field initializer's frame goes above
                the constructor's (which has not run yet), its result into its own first slot; its return resumes the
                constructor at its entry, all in this Execute }
              if Assigned(T^.FieldInit) and not T^.IsDerived and not XPushFieldInit(VA) then goto LThrow;
              goto LReload;
            end;
            { native or bound constructor }
            if not Construct(VA, @R[A + 2], Argc, VA, V) then goto LThrow;
            R[A] := V;
            Continue;
          end;
          { f.call(t, ...args) with the intrinsic call and a bytecode or native f: the registers shift down a place
            (f the callee, t its this) and f is called here, without the call native's nested Execute }
          if (O^.OKind = okNative) and (O^.Native = FuncCallNative) and ((R[A + 1] shr 48) = TAG_PTR_HI) and
             (PCell(PtrUInt(R[A + 1] and PTR_MASK))^.Kind = ckObject) and (ObjOf(R[A + 1])^.OKind in [okFunction, okNative]) then
          begin
            VA := R[A + 1];
            R[A] := VA;
            if Argc = 0 then R[A + 1] := VUndefined
            else
            begin
              for J := 0 to Argc - 1 do R[A + 1 + J] := R[A + 2 + J];
              Dec(Argc);
            end;
            O := ObjOf(VA);
          end;
          if O^.OKind = okFunction then
          begin
            if (O^.OFlags and 1) <> 0 then
            begin
              ThrowClassCall(O);
              goto LThrow;
            end;
            StackTop := Max(StackTop, Fr^.Base + A + 2 + Argc);
            if not PushBytecodeFrame(O, R[A + 1], @R[A + 2], Argc, Fr^.Base + A, 0, VUndefined) then goto LThrow;
            if Frames[FrameTop - 1].Tpl^.IsAsync and not Frames[FrameTop - 1].Tpl^.IsGenerator then
            begin
              V := SuspendNewGenerator;
              StackTop := Fr^.Base + Tpl^.MaxRegs;
              V := AsyncStartHook(V);
              R[A] := V;
              Continue;
            end;
            goto LReload;
          end;
          if (O^.OKind = okNative) and not MultiRealmAny then
          begin
            SavedTop := StackTop;
            StackTop := Fr^.Base + A + 2 + Argc;
            V := VUndefined;
            PushRoot(CurNewTarget);   { a plain call: the native must not see an outer new.target }
            CurNewTarget := VUndefined;
            NP2 := TempTop;
            NativeData := O^.Pad and $00FFFFFF;
            NativeSelf := MkPtr(O);
            Ok := TNativeFn(O^.Native)(@R[A + 1], Argc, V);
            if RootCheck and (TempTop <> NP2) then
            begin
              XReportRootImbalance(O, TempTop - NP2, Ok, Tpl);
              Halt(9);
            end;
            CurNewTarget := RootAt(TempTop - 1);
            PopRoots(1);
            StackTop := SavedTop;
            if not Ok then goto LThrow;
            R[A] := V;
            Continue;
          end;
          if not CallValue(VA, R[A + 1], @R[A + 2], Argc, V) then goto LThrow;
          R[A] := V;
        end;

      opSuperCall, opSuperCallV:
        begin
          if TOp(I and $FF) = opSuperCallV then
          begin
            O := ObjOf(R[A + 2]);
            Argc := ArrayLen(O);
            for J := 0 to Argc - 1 do
            begin
              V := O^.Elements^.Data[J];
              if V = VHole then V := VUndefined;
              R[A + 2 + J] := V;
            end;
          end
          else Argc := (I shr 24) and $FFFF;
          Fr^.PC := PC;
          { 13.3.7.1 SuperCall: GetSuperConstructor is the running constructor's [[GetPrototypeOf]], read before
            the arguments into R[A + 1] when C is 1 }
          if (I shr 40) = 1 then VA := R[A + 1] else VA := ObjOf(Fr^.Closure)^.Shape^.Proto;
          if not IsConstructorV(VA) then begin ThrowTypeError('Super constructor is not a constructor'); goto LThrow; end;
          StackTop := Max(StackTop, Fr^.Base + A + 2 + Argc);
          if (ObjOf(VA)^.OKind = okFunction) and not MultiRealmAny then
          begin
            { a bytecode parent's frame, in this Execute (XSuperPush; its return binds this) }
            J := XSuperPush(VA, A, Argc);
            if J < 0 then goto LThrow;
            if J > 0 then goto LReload;
          end;
          if not Construct(VA, @R[A + 2], Argc, Fr^.NewTarget, V) then goto LThrow;
          StackTop := Fr^.Base + Tpl^.MaxRegs;
          { BindThisValue }
          if R[0] <> VHole then
          begin
            ThrowError('ReferenceError', 'Super constructor may only be called once');
            goto LThrow;
          end;
          R[0] := V;
          R[A] := V;
          if not RunFieldInit(Fr^.Closure, V) then goto LThrow;
        end;

      opRet, opRetUndef:
        begin
          if Fr^.Kind = 4 then
          begin
            XGenInlineReturn(Fr);
            goto LReload;
          end;
          if TOp(I and $FF) = opRet then V := R[A] else V := VUndefined;
          if ((Fr^.Kind = 1) or (Fr^.Kind = 5)) and not IsObject(V) then
          begin
            { 10.2.2 [[Construct]] steps 10-12 }
            if Tpl^.IsDerived and Tpl^.IsClassCtor then
            begin
              Fr^.PC := PC;
              if V <> VUndefined then
              begin
                XThrowConstructResult(0, StopFrame, R0);
                goto LThrow;
              end;
              if R[0] = VHole then
              begin
                XThrowConstructResult(1, StopFrame, R0);
                goto LThrow;
              end;
            end;
            V := R[0];
            if IsObject(V) and (Tpl^.ExpectedProps < ObjOf(V)^.Shape^.Count) then
              Tpl^.ExpectedProps := Min(ObjOf(V)^.Shape^.Count, 64);
          end;
          CloseUpvals(Fr^.Base);
          while (HandlerTop > 0) and (Handlers[HandlerTop - 1].Frame >= FrameTop - 1) do Dec(HandlerTop);
          Dec(FrameTop);
          if FrameTop = StopFrame then
          begin
            Res := V;
            begin Result := True; goto LDone; end;
          end;
          if Fr^.Kind = 5 then
          begin
            { the parent's construction for super(): this bound in the frame that called it }
            if not XSuperReturn(Fr, V) then InjectThrow := True;
            goto LReload;
          end;
          if Fr^.Kind <> 2 then VStack[Fr^.RetIdx] := V;
          goto LReload;
        end;

      opNewObj:
        begin
          O := NewObject(RootShapeFor(ObjectProto), Max(4, I shr 40), okPlain);
          R[A] := MkPtr(O);
        end;
      opNewArr:
        begin
          B := (I shr 24) and $FFFF;
          O := NewArray(B, Max(B, 4));
          C := I shr 40;
          { a large literal's elements are born old: those stores need the barrier }
          if (O^.Elements^.Hdr.Flags and cfOld) <> 0 then
            for J := 0 to B - 1 do ValueArrayStore(O^.Elements, J, R[C + J])
          else
            for J := 0 to B - 1 do O^.Elements^.Data[J] := R[C + J];
          R[A] := MkPtr(O);
        end;
      opArrPush: ArrayPush(ObjOf(R[A]), R[(I shr 24) and $FFFF]);
      opArrHole: ArrayPush(ObjOf(R[A]), VHole);
      opArrSpread:
        begin
          Fr^.PC := PC;
          if not SpreadIntoArray(R[A], R[(I shr 24) and $FFFF]) then goto LThrow;
        end;
      opObjSpread, opCopyDataProps:
        begin
          Fr^.PC := PC;
          if (I shr 40) <> 0 then
          begin
            { an object rest: the keys in the array R[C - 1] excluded }
            if not CopyDataPropsExclHook(R[A], R[(I shr 24) and $FFFF], R[(I shr 40) - 1]) then goto LThrow;
          end
          else if not CopyDataProps(R[A], R[(I shr 24) and $FFFF]) then goto LThrow;
        end;
      opWithObj:
        begin
          Fr^.PC := PC;
          if IsNullish(R[A]) then begin ThrowTypeError('Cannot convert undefined or null to object'); goto LThrow; end;
          if not IsObject(R[A]) then
          begin
            if not ToObjectHook(R[A], V) then goto LThrow;
            R[A] := V;
          end;
        end;
      opWithHas:
        begin
          { 9.1.1.2.1 HasBinding of an object environment with withEnvironment true }
          Fr^.PC := PC;
          Key := PStr(PtrOf(K[I shr 40]));
          if not HasPropertyV(R[(I shr 24) and $FFFF], Key, Bool) then goto LThrow;
          if Bool then
          begin
            if not GetElemV(R[(I shr 24) and $FFFF], SymUnscopables, V) then goto LThrow;
            if IsObject(V) then
            begin
              if not GetV(V, PStr(PtrOf(K[I shr 40])), V) then goto LThrow;
              if ToBool(V) then Bool := False;
            end;
          end;
          R[A] := MkBool(Bool);
        end;
      opWithGet:
        begin
          Fr^.PC := PC;
          if not XWithGet(R[(I shr 24) and $FFFF], PStr(PtrOf(K[(I shr 40) and $7FFFFF])), (I shr 63) <> 0, V) then goto LThrow;
          R[A] := V;
        end;
      opWithSet:
        begin
          Fr^.PC := PC;
          if not XWithSet(R[A], PStr(PtrOf(K[(I shr 40) and $3FFFFF])), R[(I shr 24) and $FFFF], I shr 62) then goto LThrow;
        end;
      opMakeModCell:
        begin
          V := FindOrOpenUpval(Fr^.Base + A);
          ValueArrayStore(ObjOf(R[(I shr 24) and $FFFF])^.Elements, I shr 40, V);
        end;
      opModGet:
        begin
          V := ObjOf(R[(I shr 24) and $FFFF])^.Elements^.Data[I shr 40];
          if IsPtr(V) then
          begin
            if PUpval(PtrOf(V))^.StackIndex >= 0 then V := VStack[PUpval(PtrOf(V))^.StackIndex]
            else V := PUpval(PtrOf(V))^.Value;
          end
          else V := VHole;
          if V = VHole then
          begin
            Fr^.PC := PC;
            ThrowError('ReferenceError', 'Cannot access an imported binding before its initialization');
            goto LThrow;
          end;
          R[A] := V;
        end;
      opImport:
        begin
          Fr^.PC := PC;
          { C: options register ($FFFF none) in its low 16 bits, the phase (1 source, 2 defer) above }
          ImportPhase := (I shr 56) and $FF;
          if ((I shr 40) and $FFFF) = $FFFF then VB := VUndefined else VB := R[(I shr 40) and $FFFF];
          if not XImport(Tpl, R[(I shr 24) and $FFFF], VB, V) then begin ImportPhase := 0; goto LThrow; end;
          ImportPhase := 0;
          R[A] := V;
        end;
      opImportMeta:
        begin
          Fr^.PC := PC;
          if not Assigned(ImportMetaHook) or not ImportMetaHook(Tpl^.ModuleId, V) then goto LThrow;
          R[A] := V;
        end;
      opSetProtoLit:
        begin
          V := R[(I shr 24) and $FFFF];
          if IsObject(V) or (V = VNull) then SetProto(ObjOf(R[A]), V);
        end;
      opDefProp:
        begin
          Key := PStr(PtrOf(K[I shr 40]));
          O := ObjOf(R[A]);
          V := R[(I shr 24) and $FFFF];
          CI := ShapeLookup(O^.Shape, Key);
          if CI < 0 then AddProperty(O, Key, V, pfDefault)
          else if O^.Shape^.PFlags[CI] = pfDefault then SetSlot(O, CI, V)
          else DefineOwn(R[A], Key, V, pfDefault);   { replaces an accessor of the same name }
        end;
      opDefPropC:
        begin
          Fr^.PC := PC;
          VB := R[(I shr 24) and $FFFF];
          if IsInt(VB) and (ObjOf(R[A])^.OKind = okArray) then
          begin
            if not SetElemV(R[A], VB, R[I shr 40]) then goto LThrow;
            Continue;
          end;
          if not ToPropKey(VB, Key) then goto LThrow;
          DefineOwn(R[A], Key, R[I shr 40], pfDefault);
        end;
      opDefMethod:
        begin
          Key := PStr(PtrOf(K[I shr 40]));
          V := R[(I shr 24) and $FFFF];
          ObjOf(V)^.Aux1 := R[A];
          WriteBarrier(@ObjOf(V)^.Hdr, R[A]);
          DefineOwn(R[A], Key, V, pfHidden);
        end;
      opDefGetter, opDefSetter:
        begin
          Key := PStr(PtrOf(K[I shr 40]));
          B := (I shr 24) and $FFFF;
          Bool := (B and $8000) <> 0;
          B := B and $7FFF;
          V := R[B];
          ObjOf(V)^.Aux1 := R[A];
          WriteBarrier(@ObjOf(V)^.Hdr, R[A]);
          O := ObjOf(R[A]);
          CI := ShapeLookup(O^.Shape, Key);
          if (CI >= 0) and ((O^.Shape^.PFlags[CI] and pfAccessor) <> 0) then
          begin
            U := PUpval(PtrOf(GetSlot(O, CI)));   { an accessor cell }
            if TOp(I and $FF) = opDefGetter then PAccessor(U)^.Getter := V else PAccessor(U)^.Setter := V;
            WriteBarrier(@PAccessor(U)^.Hdr, V);
          end
          else
          begin
            if TOp(I and $FF) = opDefGetter then VB := MkPtr(NewAccessor(R[B], VUndefined))
            else VB := MkPtr(NewAccessor(VUndefined, R[B]));
            if Bool then DefineOwn(R[A], Key, VB, pfEnumerable or pfConfigurable or pfAccessor)
            else DefineOwn(R[A], Key, VB, pfConfigurable or pfAccessor);
          end;
        end;

      opClosure:
        begin
          T := Tpl^.Children[I shr 40];
          NP := Length(T^.Upvals);
          if NP > 0 then
          begin
            Ups := NewValueArray(NP);
            Ups^.Len := NP;
            R[A] := MkPtr(Ups);   { keep it rooted while upvalues allocate }
            for J := 0 to NP - 1 do
            begin
              NP2 := GCMinorCount;
              if T^.Upvals[J].FromParentLocal then V := FindOrOpenUpval(Fr^.Base + T^.Upvals[J].Index)
              else V := PValueArray(PtrOf(ObjOf(Fr^.Closure)^.Aux2))^.Data[T^.Upvals[J].Index];
              if GCVerify and not NurseryCellOK(R[A]) then
              begin
                XReportBadUps(J, NP2, Fr^.Base + A, Fr^.Base, Tpl);
                Halt(9);
              end;
              ValueArrayStore(PValueArray(PtrOf(R[A])), J, V);
            end;
            O := NewClosure(T, R[A]);
          end
          else
            O := NewClosure(T, VUndefined);
          R[A] := MkPtr(O);
        end;
      opClassSetup:
        begin
          Fr^.PC := PC;
          { R[A] constructor closure, R[B] parent (C = 1 when derived) }
          VB := R[(I shr 24) and $FFFF];
          if (I shr 40) = 1 then
          begin
            if VB = VNull then V := VNull
            else
            begin
              if not IsConstructorV(VB) then begin ThrowTypeError('Class extends value is not a constructor or null'); goto LThrow; end;
              if not GetV(VB, AtomPrototype, V) then goto LThrow;
              if not IsObject(V) and (V <> VNull) then
              begin
                ThrowTypeError('Class extends value does not have valid prototype property');
                goto LThrow;
              end;
            end;
          end
          else V := ObjectProto;
          O := NewPlainObject(V, 8);
          V := MkPtr(O);
          AddProperty(O, AtomConstructor, R[A], pfHidden);
          O := ObjOf(R[A]);
          O^.OFlags := O^.OFlags or 1;
          if (I shr 40) = 1 then
          begin
            O^.OFlags := O^.OFlags or 2;
            if IsObject(R[(I shr 24) and $FFFF]) then SetProto(O, R[(I shr 24) and $FFFF]);
          end;
          O := ObjOf(R[A]);
          O^.Aux1 := V;
          WriteBarrier(@O^.Hdr, V);
          AddProperty(O, AtomPrototype, V, 0);
        end;
      opClassField:
        begin
          V := R[(I shr 24) and $FFFF];
          ObjOf(V)^.Aux1 := ObjOf(R[A])^.Aux1;
          DefineOwn(R[A], AtomFieldInit, V, 0);
        end;

      opGetIter:
        begin
          { the iterator record: R[A] the iterator (undefined once done), R[A + 1] its next method }
          Fr^.PC := PC;
          if not GetIteratorRec(R[(I shr 24) and $FFFF], V, VA) then goto LThrow;
          R[A] := V;
          R[A + 1] := VA;
        end;
      opForIn:
        begin
          Fr^.PC := PC;
          if not ForInKeys(R[(I shr 24) and $FFFF], V, VB) then goto LThrow;
          R[A] := V;
          R[I shr 40] := VB;
          R[A + 1] := ArrayIterNextFnOf;   { not the threadvar directly: FPC 3.3.1+3 gave this load an absolute
                                             relocation (R_X86_64_32S) here; tools/tlsscan.py caught it }
        end;
      opIterNext:
        begin
          B := (I shr 24) and $FFFF;
          VB := R[B];
          if not IsObject(VB) then
          begin
            { a done iterator record: no next() call, the value stays as it is (undefined in patterns) }
            Inc(PC, SarInt64(Int64(I), 40));
            Continue;
          end;
          O := ObjOf(VB);
          if (O^.OKind = okIterator) and (O^.OFlags = 0) and (R[B + 1] = ArrayIterNextFn) then
          begin
            VA := O^.Aux1;
            CI := IntOf(O^.Aux2);
            if VA = VUndefined then
            begin
              R[B] := VUndefined;   { done: nothing to close }
              Inc(PC, SarInt64(Int64(I), 40));
              Continue;
            end;
            if (ObjOf(VA)^.Elements <> nil) and ((ObjOf(VA)^.OFlags and 64) = 0) and
               (ObjOf(VA)^.OKind in [okArray, okArguments]) then
            begin
              if CI >= Integer(ObjOf(VA)^.Elements^.Len) then
              begin
                O^.Aux1 := VUndefined;
                R[B] := VUndefined;
                Inc(PC, SarInt64(Int64(I), 40));
                Continue;
              end;
              V := ObjOf(VA)^.Elements^.Data[CI];
              if V <> VHole then
              begin
                R[A] := V;
                O^.Aux2 := MkInt(CI + 1);
                Continue;
              end;
            end;
          end;
          Fr^.PC := PC;
          { a suspended generator with the intrinsic next: resumed in this Execute (Kind 4) }
          if (O^.OKind = okGenerator) and (O^.OFlags <= 1) and (R[B + 1] = GenNextFn) and not MultiRealmAny then
          begin
            if not XGenInline(VB, Fr^.Base + A) then goto LThrow;
            goto LReload;
          end;
          if not IteratorStepRec(VB, R[B + 1], V, Done) then
          begin
            R[B] := VUndefined;   { an abrupt next() leaves the iterator done (7.4.8): it is not closed }
            goto LThrow;
          end;
          if Done then
          begin
            R[B] := VUndefined;
            Inc(PC, SarInt64(Int64(I), 40));
          end
          else R[A] := V;
        end;
      opIterClose:
        if IsObject(R[A]) then
        begin
          Fr^.PC := PC;
          VB := R[A];
          R[A] := VUndefined;
          if not IteratorCloseV(VB, False) then goto LThrow;
        end;
      opYieldStar:
        begin
          Fr^.PC := PC;
          B := (I shr 24) and $FFFF;
          J := IntOf(R[B + 3]);
          V := R[B + 2];
          R[B + 3] := MkInt(0);
          case J of
            0: begin
                 if not IsCallable(R[B + 1]) then begin ThrowTypeError('iterator.next is not a function'); goto LThrow; end;
                 if not CallValue(R[B + 1], R[B], @V, 1, VA) then goto LThrow;
               end;
            1: begin
                 if not GetV(R[B], AtomOf('throw'), VB) then goto LThrow;
                 if IsNullish(VB) then
                 begin
                   { no throw method: close the iterator, then a TypeError }
                   if not IteratorCloseV(R[B], False) then goto LThrow;
                   ThrowTypeError('The iterator does not provide a ''throw'' method');
                   goto LThrow;
                 end;
                 if not CallValue(VB, R[B], @V, 1, VA) then goto LThrow;
               end;
          else
            begin
              if not GetV(R[B], AtomOf('return'), VB) then goto LThrow;
              if IsNullish(VB) then begin Pending := NewRetComp(R[B + 2]); goto LThrow; end;
              if not CallValue(VB, R[B], @V, 1, VA) then goto LThrow;
            end;
          end;
          if not IsObject(VA) then begin ThrowTypeError('iterator result is not an object'); goto LThrow; end;
          R[B + 2] := VA;   { rooted while done and value are read }
          if not GetV(VA, AtomDone, V) then goto LThrow;
          if ToBool(V) then
          begin
            if not GetV(R[B + 2], AtomValue, V) then goto LThrow;
            if J = 2 then begin Pending := NewRetComp(V); goto LThrow; end;
            R[A] := V;
            Continue;
          end;
          { yield the inner result as is; resume here with the next received value and mode }
          if Fr^.Kind = 4 then
          begin
            { a for-of's generator (Kind 4): suspended at the step, the consumer gets the inner result's value }
            V := R[B + 2];
            J := Fr^.RetIdx;
            SuspendKeep(Fr, Tpl, R, -(B + 2) - 2, PC - 1, Integer(Code[PC - 1] shr 40), V);
            GenYielded := False;
            if not GetV(V, AtomValue, V) then goto LThrow;
            VStack[J] := V;
            goto LReload;
          end;
          if (Fr^.Kind <> 3) or (FrameTop - 1 <> StopFrame) then
          begin
            ThrowError('SyntaxError', 'yield outside a running generator');
            goto LThrow;
          end;
          V := R[B + 2];
          SuspendKeep(Fr, Tpl, R, -(B + 2) - 2, PC - 1, Integer(Code[PC - 1] shr 40), V);   { the step runs again }
          GenYieldRaw := True;
          SuspendedByAwait := False;
          Res := V;
          begin Result := True; goto LDone; end;
        end;
      opLoadCallee: R[A] := Fr^.Closure;
      opSuperCallUp:
        begin
          Fr^.PC := PC;
          Argc := (I shr 24) and $FFFF;
          VA := ObjOf(R[A])^.Shape^.Proto;
          if not IsConstructorV(VA) then begin ThrowTypeError('Super constructor is not a constructor'); goto LThrow; end;
          StackTop := Max(StackTop, Fr^.Base + A + 2 + Argc);
          if not Construct(VA, @R[A + 2], Argc, R[A + 1], V) then goto LThrow;
          StackTop := Fr^.Base + Tpl^.MaxRegs;
          { BindThisValue through the constructor's this, an upvalue of this arrow }
          Ups := PValueArray(PtrOf(ObjOf(Fr^.Closure)^.Aux2));
          U := PUpval(PtrOf(Ups^.Data[I shr 40]));
          if U^.StackIndex >= 0 then VB := VStack[U^.StackIndex] else VB := U^.Value;
          if VB <> VHole then
          begin
            ThrowError('ReferenceError', 'Super constructor may only be called once');
            goto LThrow;
          end;
          if U^.StackIndex >= 0 then VStack[U^.StackIndex] := V
          else
          begin
            U^.Value := V;
            WriteBarrier(@U^.Hdr, V);
          end;
          R[A + 2] := V;
          if not RunFieldInit(R[A], V) then goto LThrow;
          R[A] := R[A + 2];
        end;
      opGetAsyncIter:
        begin
          Fr^.PC := PC;
          VB := R[(I shr 24) and $FFFF];
          if not GetElemV(VB, SymAsyncIterator, V) then goto LThrow;
          if IsNullish(V) then
          begin
            { 7.4.2 step 1.b: the sync iterator, wrapped by CreateAsyncFromSyncIterator }
            if not GetIteratorRec(R[(I shr 24) and $FFFF], VA, V) then goto LThrow;
            R[A] := VA; R[A + 1] := V;
            if not AsyncFromSyncHook(R[A], R[A + 1], V) then goto LThrow;
          end
          else
          begin
            if not IsCallable(V) then begin ThrowTypeError('Symbol.asyncIterator is not a function'); goto LThrow; end;
            if not CallValue(V, R[(I shr 24) and $FFFF], nil, 0, V) then goto LThrow;
            if not IsObject(V) then begin ThrowTypeError('async iterator is not an object'); goto LThrow; end;
          end;
          R[A] := V;
          if not GetV(V, AtomNext, VA) then goto LThrow;
          R[A + 1] := VA;
        end;
      opIterCallNext:
        begin
          Fr^.PC := PC;
          B := (I shr 24) and $FFFF;
          if not IsCallable(R[B + 1]) then
          begin
            R[B] := VUndefined;
            ThrowTypeError('iterator.next is not a function');
            goto LThrow;
          end;
          if not CallValue(R[B + 1], R[B], nil, 0, V) then begin R[B] := VUndefined; goto LThrow; end;
          R[A] := V;
        end;
      opIterUnpack:
        begin
          Fr^.PC := PC;
          if not IsObject(R[A]) then begin ThrowTypeError('iterator result is not an object'); goto LThrow; end;
          if not GetV(R[A], AtomDone, V) then goto LThrow;
          if ToBool(V) then
          begin
            R[(I shr 24) and $FFFF] := VUndefined;
            Inc(PC, SarInt64(Int64(I), 40));
            Continue;
          end;
          if not GetV(R[A], AtomValue, V) then goto LThrow;
          R[A] := V;
        end;
      opAsyncIterReturn:
        begin
          Fr^.PC := PC;
          VB := R[(I shr 24) and $FFFF];
          if not IsObject(VB) then begin Inc(PC, SarInt64(Int64(I), 40)); Continue; end;
          if not GetV(VB, AtomOf('return'), V) then goto LThrow;
          if IsNullish(V) then begin Inc(PC, SarInt64(Int64(I), 40)); Continue; end;
          if not CallValue(V, R[(I shr 24) and $FFFF], nil, 0, V) then goto LThrow;
          R[A] := V;
        end;
      opIterCheckObj:
        if not IsObject(R[A]) then
        begin
          Fr^.PC := PC;
          ThrowTypeError('iterator result is not an object');
          goto LThrow;
        end;
      opYieldStarAsync:
        begin
          { 15.5.5 yield* for async generators, as phases (R[B + 4]): 0 call the inner method for the received
            mode, then await its result (phase 1, or 3 for return); 1/3 the awaited inner result: done ends the
            yield* (3: with a return, after awaiting the value: phase 5) or its value is yielded (phase 0);
            2 throw without a throw method: the closing return() result awaited, then a TypeError; 4 return
            without a return method: the received value awaited, then the return }
          Fr^.PC := PC;
          if (Fr^.Kind <> 3) or (FrameTop - 1 <> StopFrame) then
          begin
            ThrowError('SyntaxError', 'yield outside a running generator');
            goto LThrow;
          end;
          B := (I shr 24) and $FFFF;
          J := IntOf(R[B + 4]);
          case J of
            0:
              begin
                CI := IntOf(R[B + 3]);
                V := R[B + 2];
                R[B + 3] := MkInt(0);
                case CI of
                  0: begin
                       if not IsCallable(R[B + 1]) then begin ThrowTypeError('iterator.next is not a function'); goto LThrow; end;
                       if not CallValue(R[B + 1], R[B], @V, 1, VA) then goto LThrow;
                       R[B + 4] := MkInt(1);
                     end;
                  1: begin
                       if not GetV(R[B], AtomOf('throw'), VB) then goto LThrow;
                       if IsNullish(VB) then
                       begin
                         if not GetV(R[B], AtomOf('return'), VB) then goto LThrow;
                         if IsNullish(VB) then begin ThrowTypeError('The iterator does not provide a ''throw'' method'); goto LThrow; end;
                         if not CallValue(VB, R[B], nil, 0, VA) then goto LThrow;
                         R[B + 4] := MkInt(2);
                       end
                       else
                       begin
                         if not CallValue(VB, R[B], @V, 1, VA) then goto LThrow;
                         R[B + 4] := MkInt(1);
                       end;
                     end;
                else
                  begin
                    if not GetV(R[B], AtomOf('return'), VB) then goto LThrow;
                    if IsNullish(VB) then
                    begin
                      VA := V;
                      R[B + 4] := MkInt(4);
                    end
                    else
                    begin
                      if not CallValue(VB, R[B], @V, 1, VA) then goto LThrow;
                      R[B + 4] := MkInt(3);
                    end;
                  end;
                end;
                { await VA, then run again }
                SuspendKeep(Fr, Tpl, R, B + 2, PC - 1, Integer(Code[PC - 1] shr 40), VA);
                SuspendedByAwait := True;
                GenYieldRaw := False;
                Res := VA;
                begin Result := True; goto LDone; end;
              end;
            1, 3:
              begin
                VA := R[B + 2];
                if not IsObject(VA) then begin R[B + 4] := MkInt(0); ThrowTypeError('iterator result is not an object'); goto LThrow; end;
                if not GetV(VA, AtomDone, V) then begin R[B + 4] := MkInt(0); goto LThrow; end;
                if ToBool(V) then
                begin
                  if not GetV(R[B + 2], AtomValue, V) then begin R[B + 4] := MkInt(0); goto LThrow; end;
                  if J = 3 then
                  begin
                    R[B + 4] := MkInt(5);
                    SuspendKeep(Fr, Tpl, R, B + 2, PC - 1, Integer(Code[PC - 1] shr 40), V);
                    SuspendedByAwait := True;
                    GenYieldRaw := False;
                    Res := V;
                    begin Result := True; goto LDone; end;
                  end;
                  R[B + 4] := MkInt(0);
                  R[A] := V;
                  Continue;
                end;
                if not GetV(R[B + 2], AtomValue, V) then begin R[B + 4] := MkInt(0); goto LThrow; end;
                R[B + 4] := MkInt(0);
                SuspendKeep(Fr, Tpl, R, -(B + 2) - 2, PC - 1, Integer(Code[PC - 1] shr 40), V);
                SuspendedByAwait := False;
                GenYieldRaw := False;
                Res := V;
                begin Result := True; goto LDone; end;
              end;
            2:
              begin
                R[B + 4] := MkInt(0);
                if not IsObject(R[B + 2]) then ThrowTypeError('iterator result is not an object')
                else ThrowTypeError('The iterator does not provide a ''throw'' method');
                goto LThrow;
              end;
          else
            begin
              { 4 and 5: the awaited value is returned }
              R[B + 4] := MkInt(0);
              Pending := NewRetComp(R[B + 2]);
              goto LThrow;
            end;
          end;
        end;
      opSetFnName:
        begin
          Fr^.PC := PC;
          if not ToPropKey(R[(I shr 24) and $FFFF], Key) then goto LThrow;
          case I shr 40 of
            1: SetFnNameFromKey(R[A], Key, 'get');
            2: SetFnNameFromKey(R[A], Key, 'set');
          else
            SetFnNameFromKey(R[A], Key, '');
          end;
        end;
      opCheckHole:
        if R[A] = VHole then
        begin
          Fr^.PC := PC;
          XThrowTDZ(PStr(PtrOf(K[I shr 40])));
          goto LThrow;
        end;
      opHasGlobalRef:
        begin
          IC := @Tpl^.ICs[I shr 40];
          O := ObjOf(GlobalObj);
          Bool := (GLexFind(IC^.Key) >= 0) or (ShapeLookup(O^.Shape, IC^.Key) >= 0);
          if not Bool then
          begin
            Fr^.PC := PC;
            if not InheritedHasV(O, IC^.Key, Bool) then goto LThrow;
          end;
          R[A] := MkBool(Bool);
        end;
      opThrowConst:
        begin
          Fr^.PC := PC;
          { B = 1: 13.5.1.2 delete of a super reference; 2: a strict assignment to a binding (Consts[C]) its object
            environment lost; 3: a ReferenceError with the message Consts[C] (Annex B call assignment targets) }
          case (I shr 24) and $FFFF of
            1: ThrowError('ReferenceError', 'Unsupported reference to ''super''');
            2: XThrowNotDefined(PStr(PtrOf(K[I shr 40])));
            3: ThrowError('ReferenceError', StrToUnicode(PStr(PtrOf(K[I shr 40]))));
          else ThrowTypeError('Assignment to constant variable.');
          end;
          goto LThrow;
        end;
      opSuperBase:
        begin
          VB := R[(I shr 24) and $FFFF];
          if IsObject(VB) then R[A] := ObjOf(VB)^.Shape^.Proto else R[A] := VUndefined;
        end;
      opGetSuperB, opSetSuperB:
        begin
          { GetValue / PutValue: ToObject(base) (null: TypeError), then ToPropertyKey, then [[Get]] / [[Set]] with
            this as the receiver }
          Fr^.PC := PC;
          B := (I shr 24) and $FFFF;
          CI := SarInt64(Int64(I), 40);
          if not IsObject(R[B]) then
          begin
            if CI >= 0 then ThrowNullishBase(R[B], MkPtr(Tpl^.ICs[CI].Key), TOp(I and $FF) = opSetSuperB)
            else ThrowNullishBase(R[B], R[-1 - CI], TOp(I and $FF) = opSetSuperB);
            goto LThrow;
          end;
          if CI < 0 then
          begin
            if not ToPropKey(R[-1 - CI], Key) then goto LThrow;
            R[-1 - CI] := KeyToValue(Key);
          end
          else Key := Tpl^.ICs[CI].Key;
          if TOp(I and $FF) = opGetSuperB then
          begin
            if not GetFromObject(ObjOf(R[B]), Key, R[B + 1], V) then goto LThrow;
            R[A] := V;
          end
          else
          begin
            if not SetReceiverHook(R[B], Key, R[A], R[B + 1], Bool) then goto LThrow;
            if not Bool and Tpl^.IsStrict then begin XThrowReadOnlySuper(Key); goto LThrow; end;
          end;
        end;
      opIterCloseThrow:
        if IsObject(R[A]) then
        begin
          VB := R[A];
          R[A] := VUndefined;
          { C = 1: R[B] is the completion; a generator's return completion closes as a normal one would (7.4.11
            IteratorClose step 5: return()'s errors and a non-object result are thrown) }
          if ((I shr 40) = 1) and IsRetComp(R[(I shr 24) and $FFFF]) then
          begin
            Fr^.PC := PC;
            if not IteratorCloseV(VB, False) then goto LThrow;
            Continue;
          end;
          V := Pending;
          Pending := VUndefined;
          IteratorCloseV(VB, True);
          Pending := V;
        end;

      opTry:
        begin
          if HandlerTop >= Length(Handlers) then SetLength(Handlers, HandlerTop * 2 + 64);
          Handlers[HandlerTop].Frame := FrameTop - 1;
          Handlers[HandlerTop].PC := PC + SarInt64(Int64(I), 40);
          Handlers[HandlerTop].ExcIdx := Fr^.Base + A;
          Handlers[HandlerTop].Kind := (I shr 24) and $FFFF;
          Inc(HandlerTop);
        end;
      opEndTry: Dec(HandlerTop);
      opThrow:
        begin
          Fr^.PC := PC;
          Pending := R[A];
          goto LThrow;
        end;
      opRegExp:
        begin
          Fr^.PC := PC;
          if not Assigned(RegExpCreateHook) then begin ThrowError('SyntaxError', 'RegExp is not available'); goto LThrow; end;
          if not RegExpCreateHook(PStr(PtrOf(K[(I shr 24) and $FFFF])), PStr(PtrOf(K[I shr 40])), V) then goto LThrow;
          R[A] := V;
        end;
      opYield, opAwait:
        begin
          Fr^.PC := PC;
          SuspendedByAwait := TOp(I and $FF) = opAwait;
          if (Fr^.Kind = 4) and (TOp(I and $FF) = opYield) then
          begin
            { a for-of's generator yields: suspended, its value in the consumer's register }
            PushRoot(R[(I shr 24) and $FFFF]);
            J := Fr^.RetIdx;
            SaveSuspension(Fr, Tpl, R, A, PC, Integer(Code[PC - 1] shr 40));
            GenYielded := False;
            VStack[J] := RootAt(TempTop - 1);
            PopRoots(1);
            goto LReload;
          end;
          if (Fr^.Kind <> 3) or (FrameTop - 1 <> StopFrame) then
          begin
            ThrowError('SyntaxError', 'yield outside a running generator');
            goto LThrow;
          end;
          { SaveSuspension may grow the state array (a collection): the value is read back from a root }
          PushRoot(R[(I shr 24) and $FFFF]);
          SaveSuspension(Fr, Tpl, R, A, PC, Integer(Code[PC - 1] shr 40));
          Res := RootAt(TempTop - 1);
          PopRoots(1);
          begin Result := True; goto LDone; end;
        end;
      opGenStart:
        begin
          { the generator object (OrdinaryCreateFromConstructor: the function's prototype property, else the
            intrinsic), suspended at start, is this call's result }
          Fr^.PC := PC;
          V := FunctionPrototypeOf(Fr^.Closure);
          if not IsObject(V) then
            if Tpl^.IsAsync then V := AsyncGeneratorProto else V := GeneratorProto;
          PushRoot(V);
          E := NewValueArray(Tpl^.MaxRegs + 4);
          PushRoot(MkPtr(E));
          O := NewObject(RootShapeFor(RootAt(TempTop - 2)), 0, okGenerator);
          O^.Aux1 := Fr^.Closure;
          O^.Aux2 := RootAt(TempTop - 1);
          O^.OFlags := 0;
          PopRoots(2);
          Fr^.NewTarget := MkPtr(O);
          SaveSuspension(Fr, Tpl, R, -1, PC, Integer(Code[PC - 1] shr 40));
          GenYielded := False;
          V := Fr^.NewTarget;
          ObjOf(V)^.OFlags := 0;   { suspended start }
          if Tpl^.IsAsync then V := AsyncGenStartHook(V);
          if FrameTop = StopFrame then
          begin
            Res := V;
            begin Result := True; goto LDone; end;
          end;
          if Fr^.Kind <> 2 then VStack[Fr^.RetIdx] := V;
          goto LReload;
        end;
      opDefMethodC:
        begin
          Fr^.PC := PC;
          V := R[(I shr 24) and $FFFF];
          ObjOf(V)^.Aux1 := R[A];
          WriteBarrier(@ObjOf(V)^.Hdr, R[A]);
          if not ToPropKey(R[I shr 40], Key) then goto LThrow;
          { DefineMethodProperty: DefinePropertyOrThrow fails on a non-configurable own property (a class's prototype) }
          CI := ShapeLookup(ObjOf(R[A])^.Shape, Key);
          if (CI >= 0) and ((ObjOf(R[A])^.Shape^.PFlags[CI] and pfConfigurable) = 0) then
          begin
            ThrowTypeError('Cannot redefine a non-configurable property with a computed method name');
            goto LThrow;
          end;
          DefineOwn(R[A], Key, R[(I shr 24) and $FFFF], pfHidden);
        end;
      opDefGetterC, opDefSetterC:
        begin
          Fr^.PC := PC;
          if not ToPropKey(R[I shr 40], Key) then goto LThrow;
          CI := ShapeLookup(ObjOf(R[A])^.Shape, Key);
          if (CI >= 0) and ((ObjOf(R[A])^.Shape^.PFlags[CI] and pfConfigurable) = 0) then
          begin
            ThrowTypeError('Cannot redefine a non-configurable property with a computed accessor name');
            goto LThrow;
          end;
          B := (I shr 24) and $FFFF;
          Bool := (B and $8000) <> 0;   { enumerable: an object literal's accessor (a class's is not) }
          B := B and $7FFF;
          ObjOf(R[B])^.Aux1 := R[A];
          WriteBarrier(@ObjOf(R[B])^.Hdr, R[A]);
          O := ObjOf(R[A]);
          CI := ShapeLookup(O^.Shape, Key);
          if (CI >= 0) and ((O^.Shape^.PFlags[CI] and pfAccessor) <> 0) then
          begin
            U := PUpval(PtrOf(GetSlot(O, CI)));
            if TOp(I and $FF) = opDefGetterC then PAccessor(U)^.Getter := R[B] else PAccessor(U)^.Setter := R[B];
            WriteBarrier(@PAccessor(U)^.Hdr, R[B]);
          end
          else
          begin
            if TOp(I and $FF) = opDefGetterC then VB := MkPtr(NewAccessor(R[B], VUndefined))
            else VB := MkPtr(NewAccessor(VUndefined, R[B]));
            DefineOwn(R[A], Key, VB, pfConfigurable or pfAccessor or Ord(Bool) * pfEnumerable);
          end;
        end;

    else
      begin
        Fr^.PC := PC;
        XThrowUnimplemented(I and $FF);
        goto LThrow;
      end;
    end;
    Continue;
LThrow:
    if ThrowTrace then XTraceThrow;
    { find a handler in a frame this activation owns; a generator's return completion passes catch handlers }
    if IsRetComp(Pending) then
      while (HandlerTop > 0) and (Handlers[HandlerTop - 1].Frame >= StopFrame) and (Handlers[HandlerTop - 1].Kind = 0) do
        Dec(HandlerTop);
    if (HandlerTop > 0) and (Handlers[HandlerTop - 1].Frame >= StopFrame) then
    begin
      Dec(HandlerTop);
      H := @Handlers[HandlerTop];
      while FrameTop - 1 > H^.Frame do
      begin
        CloseUpvals(Frames[FrameTop - 1].Base);
        if Frames[FrameTop - 1].Kind = 4 then XGenUnwind;
        Dec(FrameTop);
      end;
      Frames[FrameTop - 1].PC := H^.PC;
      VStack[H^.ExcIdx] := Pending;
      Pending := VUndefined;
      goto LReload;
    end;
    while FrameTop > StopFrame do
    begin
      CloseUpvals(Frames[FrameTop - 1].Base);
      if (Frames[FrameTop - 1].Kind = 4) and (FrameTop - 1 > StopFrame) then XGenUnwind
      else if Frames[FrameTop - 1].Kind = 4 then ObjOf(Frames[FrameTop - 1].NewTarget)^.OFlags := 3;
      Dec(FrameTop);
    end;
    Res := VUndefined;
    begin Result := False; goto LDone; end;
  end;
LDone:
  if CurRealm <> R0 then SwitchRealm(R0);
end;

{ ---------------- entry ---------------- }

procedure VMDone;
begin
  {$IFDEF UNIX}
  if VStack <> nil then fpmunmap(VStack, STACK_MAX * SizeOf(TValue));
  {$ELSE}
  if VStack <> nil then FreeMem(VStack);
  {$ENDIF}
  VStack := nil; StackTop := 0;
  Frames := nil; FrameTop := 0; Handlers := nil; HandlerTop := 0; OpenUpvals := nil; OpenCount := 0;
  ExtraRoots := nil; ExtraRootCount := 0;
  GlobalObj := VUndefined; Pending := VUndefined; CurNewTarget := VUndefined; ConstructNT := VUndefined;
end;

procedure VMInit;
begin
  GCDescribeNative := @DescribeTemplate;
  { Zeroed pages on first touch: an eager GetMem + FillChar of the 32 MB stack
    was 8,192 page faults and 32 MB of RSS in every run, empty script included. }
  {$IFDEF UNIX}
  VStack := fpmmap(nil, STACK_MAX * SizeOf(TValue), PROT_READ or PROT_WRITE, MAP_PRIVATE or MAP_ANONYMOUS, -1, 0);
  if VStack = PValue(MAP_FAILED) then raise EOutOfMemory.Create('cannot reserve the register stack');
  {$ELSE}
  VStack := GetMem(STACK_MAX * SizeOf(TValue));
  FillChar(VStack^, STACK_MAX * SizeOf(TValue), 0);
  {$ENDIF}
  StackTop := 0;
  SetLength(Frames, FRAME_MAX);
  FrameTop := 0;
  SetLength(Handlers, 256);
  HandlerTop := 0;
  SetLength(OpenUpvals, 256);
  OpenCount := 0;
  Pending := VUndefined;
  CurNewTarget := VUndefined;
  ConstructNT := VUndefined;   { a threadvar starts as 0, which is the number 0, not undefined }
  ArgsShapeMapped := nil; ArgsShapeUnmapped := nil;   { shapes of this engine's Object.prototype }
  ConstructProto := VUndefined;
  NativeSelf := VUndefined;
  GCRootScanner := @ScanRoots;
  AtomLength := AtomOf('length');
  AtomPrototype := AtomOf('prototype');
  AtomConstructor := AtomOf('constructor');
  AtomName := AtomOf('name');
  AtomMessage := AtomOf('message');
  AtomNext := AtomOf('next');
  AtomDone := AtomOf('done');
  AtomValue := AtomOf('value');
  AtomToString := AtomOf('toString');
  AtomValueOf := AtomOf('valueOf');
  AtomFieldInit := NewKeyAtom('fieldinit', kfInternal);
  AtomStack := AtomOf('stack');
  AtomCallee := AtomOf('callee');
  AtomByteLength := AtomOf('byteLength');
  AtomByteOffset := AtomOf('byteOffset');
  AtomBuffer := AtomOf('buffer');
  FunctionShape := RootShapeFor(VNull);   { replaced once Function.prototype exists }
  { a realm's intrinsics (the builtins' Init procedures make them) }
  RealmVals([@ObjectProto, @FunctionProto, @ArrayProto, @StringProto, @NumberProto, @BooleanProto, @ErrorProto,
    @IteratorProto, @ArrayIteratorProto, @GeneratorProto, @MapProto, @SetProtoObj, @SymbolProto, @GlobalObj,
    @TypedArrayValuesFn, @ThrowerAcc, @ArrayIterNextFn, @GenNextFn, @MapIterNextFn, @SetIterNextFn,
    @GenFunctionProto, @AsyncFunctionProto, @AsyncGenFunctionProto, @AsyncGeneratorProto, @AsyncIteratorProto,
    @AsyncFromSyncIterProto, @ArrayValuesFnPublic, @StringIterProto, @StringIterNextFn, @FuncHasInstanceFn, @BigIntProto, @ErrorProtoType, @TypeErrorProto,
    @RangeErrorProto, @SyntaxErrorProto, @ReferenceErrorProto, @EvalErrorProto, @URIErrorProto]);
  AddRoot(@IntrinsicList);
  IntrinsicList := VUndefined;
  RealmVals([@IntrinsicList]);
  IntrinsicListIdx := High(RealmValSlots);
  RealmPtrs([@EmptyShapeForArray, @FunctionShape, @GenFunctionShape, @AsyncFunctionShape, @AsyncGenFunctionShape,
    @ArgsShapeMapped, @ArgsShapeUnmapped]);
end;

procedure SetGenNextNative(P: Pointer);
begin
  GenNextNative := P;
end;

function FnShapeFor(Proto: TValue): PShape;
begin
  Result := ShapeAdd(ShapeAdd(RootShapeFor(Proto), AtomOf('length'), pfConfigurable), AtomOf('name'), pfConfigurable);
end;

procedure SetFnNameFromKey(F: TValue; Key: PStr; const Prefix: UnicodeString);
var N: UnicodeString; S: TValue;
begin
  if (Key^.Hdr.Extra and kfSymbol) <> 0 then
  begin
    { [description] (an empty description gives "[]"), or empty without one (the symbol cell's Extra 1) }
    S := PValue(PByte(PtrOf(KeyPayload(Key)^)) + 8)^;
    if (PCell(PtrOf(KeyPayload(Key)^))^.Extra <> 1) and IsString(S) then N := '[' + StrToUnicode(StrOf(S)) + ']'
    else N := '';
  end
  else if (Key^.Hdr.Extra and kfPrivate) <> 0 then N := StrToUnicode(Key)
  else N := StrToUnicode(Key);
  if Prefix <> '' then N := Prefix + ' ' + N;
  PushRoot(F);
  S := StrFromUnicode(N);
  DefineOwn(RootAt(TempTop - 1), AtomName, S, pfConfigurable);
  PopRoots(1);
end;

procedure SetFunctionProto(P: TValue);
begin
  FunctionShape := FnShapeFor(P);
end;

function MakeScriptClosure(T: PFuncTemplate; var Clo: TValue): Boolean;
begin
  Clo := MkPtr(NewClosure(T, VUndefined));
  Result := True;
end;

function RunScript(T: PFuncTemplate): Boolean;
var Clo: PObj; Res: TValue;
begin
  FunctionShape := FnShapeFor(FunctionProto);
  Clo := NewClosure(T, VUndefined);
  VStack[0] := VUndefined;
  StackTop := 1;
  if not PushBytecodeFrame(Clo, GlobalObj, @VStack[1], 0, 0, 2, VUndefined) then Exit(False);   { a Script's this: the global object }
  Result := Execute(0, Res);
end;

function RunScriptRes(T: PFuncTemplate; var Res: TValue): Boolean;
var Clo: PObj;
begin
  FunctionShape := FnShapeFor(FunctionProto);
  Clo := NewClosure(T, VUndefined);
  VStack[0] := VUndefined;
  StackTop := 1;
  Res := VUndefined;
  if not PushBytecodeFrame(Clo, GlobalObj, @VStack[1], 0, 0, 2, VUndefined) then Exit(False);   { a Script's this: the global object }
  Result := Execute(0, Res);
end;

initialization
  InitFastOps;
  {$IFDEF CPUX86_64}JitOn;{$ENDIF}
{$IFDEF G2_JITSTATS}
finalization
  WriteLn(ErrOutput, 'jitentries ', JitEntries);
  WriteLn(ErrOutput, 'jitfallbacks ', JitFallbacks);
  WriteLn(ErrOutput, 'jitcompiles ', JitCompiles, ' bytes ', JitCodeBytes);
  for FastOpI := 0 to 255 do
  begin
    if JitExits[FastOpI] > 0 then WriteLn(ErrOutput, 'jitexit ', FastOpI, ' ', JitExits[FastOpI]);
    if JitNestedExits[FastOpI] > 0 then WriteLn(ErrOutput, 'jitnested ', FastOpI, ' ', JitNestedExits[FastOpI]);
    if JitNoEntry[FastOpI] > 0 then WriteLn(ErrOutput, 'jitnoentry ', FastOpI, ' ', JitNoEntry[FastOpI]);
    if JitOpCount[FastOpI] > 0 then WriteLn(ErrOutput, 'jitopbytes ', FastOpI, ' ', JitOpCount[FastOpI], ' ', JitOpBytes[FastOpI]);
  end;
{$ENDIF}
{$IFDEF G2_BAILSTATS}
finalization
  for FastOpI := 0 to 255 do
    if BailCounts[FastOpI] > 0 then WriteLn(ErrOutput, 'bail ', FastOpI, ' ', BailCounts[FastOpI]);
{$ENDIF}

end.
