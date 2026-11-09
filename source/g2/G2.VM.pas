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
threadvar
  { opImport: the import call's phase while the hook runs (0 evaluation, 1 source, 2 defer) }
  ImportPhase: Integer;
var
  { G2.ObjModel's 7.1.18 ToObject }
  ToObjectHook: function(V: TValue; var O: TValue): Boolean = nil;
  { G2.ObjModel's [[GetPrototypeOf]] (a proxy's trap with its invariants) }
  GetProtoHook: function(O: TValue; var P: TValue): Boolean = nil;
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
  { the native function object being called: a native reads its data (Aux1) from it before allocating }
  NativeSelf: TValue;
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
function OwnKeys(O: PObj; OnlyEnumerable: Boolean): TValue;   { array of string keys }
function DefineOwn(O: TValue; Key: PStr; V: TValue; Flags: Byte): Boolean;
function GetIterator(V: TValue; var It: TValue): Boolean;
function IteratorStep(It: TValue; var Value: TValue; var Done: Boolean): Boolean;
function FunctionPrototypeOf(F: TValue): TValue;
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
function GeneratorResume(Gen: TValue; Arg: TValue; Mode: Integer; var Value: TValue; var Done: Boolean): Boolean;

threadvar
  ArrayValuesFnPublic: TValue;
  BigIntProto: TValue;
  ErrorProtoType, TypeErrorProto, RangeErrorProto, SyntaxErrorProto, ReferenceErrorProto: TValue;
  ArrayIterNative: TNativeFn;

implementation

function SuspendNewGenerator: TValue; forward;
function GeneratorNextIsIntrinsic(Gen: TValue): Boolean; forward;

{ ---------------- roots ---------------- }

procedure AddRoot(P: PValue);
begin
  if ExtraRootCount >= Length(ExtraRoots) then SetLength(ExtraRoots, ExtraRootCount * 2 + 64);
  ExtraRoots[ExtraRootCount] := P;
  Inc(ExtraRootCount);
end;

procedure AddRootRange(P: PValue; N: Integer);
var I: Integer;
begin
  for I := 0 to N - 1 do AddRoot(P + I);
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
  for I := 0 to ExtraRootCount - 1 do GCVisit(ExtraRoots[I]^);
  GCVisit(GlobalObj);
  GCVisit(Pending);
  GCVisit(CurNewTarget);
  GCVisit(ConstructNT);
  if Assigned(HostRootScanner) then HostRootScanner;
end;

{ ---------------- upvalues ---------------- }

function FindOrOpenUpval(Idx: Integer): TValue;
var I, J: Integer; U: PUpval; V: TValue;
begin
  { OpenUpvals is sorted by stack index ascending }
  I := OpenCount - 1;
  while I >= 0 do
  begin
    U := PUpval(PtrOf(OpenUpvals[I]));
    if U^.StackIndex = Idx then Exit(OpenUpvals[I]);
    if U^.StackIndex < Idx then Break;
    Dec(I);
  end;
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
  if IsObject(CurNewTarget) then
  begin
    if not GetV(CurNewTarget, AtomPrototype, P) then Exit(False);
    if not IsObject(P) then P := Default;
  end;
  Result := True;
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
    if IsObject(It) and (ObjOf(It)^.OKind = okIterator) then NextM := ArrayIterNextFn
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
  if (NextM = ArrayIterNextFn) and IsObject(It) and (ObjOf(It)^.OKind = okIterator) then
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
  M := StrFromUnicode(Msg);
  PushRoot(M);
  O := NewObject(RootShapeFor(Proto), 4, okError);
  M := RootAt(TempTop - 1);
  PopRoots(1);
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
  else P := ErrorProto;
  Pending := MakeError(P, Msg);
  Result := False;
end;

function ThrowTypeError(const Msg: UnicodeString): Boolean;
begin
  Result := ThrowError('TypeError', Msg);
end;

{ ---------------- functions ---------------- }

threadvar
  FunctionShape: PShape;

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

function NewClosure(T: PFuncTemplate; Upvals: TValue): PObj;
begin
  PushRoot(Upvals);
  if not (T^.IsGenerator or T^.IsAsync) or (GenFunctionShape = nil) then Result := NewObject(FunctionShape, 3, okFunction)
  else if T^.IsGenerator and T^.IsAsync then Result := NewObject(AsyncGenFunctionShape, 3, okFunction)
  else if T^.IsGenerator then Result := NewObject(GenFunctionShape, 3, okFunction)
  else Result := NewObject(AsyncFunctionShape, 3, okFunction);
  Result^.Native := T;
  Result^.Aux2 := RootAt(TempTop - 1);
  PopRoots(1);
  if GCVerify and not NurseryCellOK(Result^.Aux2) then
  begin
    WriteLn(ErrOutput, 'closure ', DescribeTemplate(T), ' created with a bad upvalue array ', IntToHex(Result^.Aux2, 16), ' minor ', GCMinorCount, ' temptop ', TempTop, ' cap ', Length(TempRoots));
    Halt(9);
  end;
  if T^.IsArrow then Result^.OFlags := Result^.OFlags or 4;
  Result^.Slots[0] := MkInt(T^.Length);
  Result^.Slots[1] := MkPtr(T^.Name);
end;

function NewNativeFunction(Fn: TNativeFn; const Name: UnicodeString; Length: Integer): TValue;
var O: PObj;
begin
  O := NewObject(FunctionShape, 3, okNative);
  O^.Native := Pointer(@Fn);
  O^.Aux1 := MkPtr(AtomOf(Name));
  O^.Aux2 := MkInt(Length);
  O^.Slots[0] := MkInt(Length);
  O^.Slots[1] := MkPtr(AtomOf(Name));
  Result := MkPtr(O);
end;

function NewNativeFunctionData(Fn: TNativeFn; const Name: UnicodeString; Length: Integer; Data: UInt32): TValue;
begin
  Result := NewNativeFunction(Fn, Name, Length);
  ObjOf(Result)^.Pad := Data;
end;

function DefineOwn(O: TValue; Key: PStr; V: TValue; Flags: Byte): Boolean;
var P: PObj; S: Integer;
begin
  P := ObjOf(O);
  { the built-in constructors are the natives given a prototype property (Symbol and BigInt included: their
    [[Construct]] throws) }
  if (Key = AtomPrototype) and (P^.OKind = okNative) then P^.OFlags := P^.OFlags or 32;
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
function FunctionPrototypeOf(F: TValue): TValue;
var O, P: PObj; S: Integer; T: PFuncTemplate;
begin
  O := ObjOf(F);
  S := ShapeLookup(O^.Shape, AtomPrototype);
  if S >= 0 then Exit(GetSlot(O, S));
  if O^.OKind <> okFunction then Exit(VUndefined);
  T := PFuncTemplate(O^.Native);
  if T^.IsGenerator then
  begin
    { 15.5.4 / 15.6.4: a generator's prototype inherits %GeneratorPrototype% (or the async one) and has no
      constructor property }
    PushRoot(F);
    if T^.IsAsync then P := NewPlainObject(AsyncGeneratorProto, 4) else P := NewPlainObject(GeneratorProto, 4);
    PushRoot(MkPtr(P));
    F := RootAt(TempTop - 2);
    AddProperty(ObjOf(F), AtomPrototype, RootAt(TempTop - 1), pfWritable);
    Result := RootAt(TempTop - 1);
    PopRoots(2);
    Exit;
  end;
  if T^.IsArrow or T^.IsMethod or T^.IsAsync then Exit(VUndefined);
  PushRoot(F);
  P := NewPlainObject(ObjectProto, 4);
  PushRoot(MkPtr(P));
  AddProperty(P, AtomConstructor, RootAt(TempTop - 2), pfHidden);
  F := RootAt(TempTop - 2);
  AddProperty(ObjOf(F), AtomPrototype, RootAt(TempTop - 1), pfWritable);
  Result := RootAt(TempTop - 1);
  PopRoots(2);
end;

{ ---------------- property access ---------------- }

{ an index write an array keeps dense: near its length, or anywhere below 4M elements }
function ArrayDenseOK(E: PValueArray; Idx: Int64): Boolean; inline;
begin
  Result := (Idx < Int64(E^.Len) + 1024 + Int64(E^.Len) * 2) or (Idx < 4 * 1024 * 1024);
end;

{ [[Delete]] on an ordinary object: a non-configurable property stays (the delete reports false) }
function DeleteIfConfigurable(O: PObj; Key: PStr): Boolean;
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
      if Key = AtomLength then begin Res := MkInt(ArrayLen(O)); Exit(True); end;
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
var S: Integer; Flags: Byte; Acc: PAccessor; P: TValue; Idx: Int64; V: TValue; Found: Boolean;
begin
  while True do
  begin
    if O^.OKind = okProxy then Exit(ProxyGet(MkPtr(O), Key, Receiver, Res));
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
begin
  Ok := True;
  O := ObjOf(Obj);
  if O^.OKind = okProxy then Exit(ProxySetHook(Obj, Key, V, Obj, Ok));
  if O^.OKind = okModuleNS then begin Ok := False; Exit(True); end;   { 10.4.6.9 [[Set]]: false }
  { 10.4.3: a String object's characters and length are non-writable own properties }
  if (O^.OKind = okStringObj) and ((Key = AtomLength) or (IndexFromString(Key, Idx) and (Idx < StrOf(O^.Aux1)^.Len))) then
  begin Ok := False; Exit(True); end;
  { 10.4.5.5 [[Set]] with the typed array as its own receiver: TypedArraySetElement, always true }
  if O^.OKind = okTypedArray then
  begin
    if IndexFromString(Key, Idx) then Exit(TAStoreValue(Obj, Idx, V));
    if CanonicalNumericNonIndex(Key) then Exit(TAStoreValue(Obj, -1, V));
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
      if (O^.OFlags and 64) <> 0 then
      begin
        { index properties in the shape: ArraySetLength's deletion from the end, stopped by a non-configurable one }
        PushRoot(Obj); PushRoot(V);
        if not ToNumberV(V, D1) or not ToNumberV(RootAt(TempTop - 1), D2) then begin PopRoots(2); Exit(False); end;
        O := ObjOf(RootAt(TempTop - 2));
        if IsNaN(D2) or (D2 < 0) or (D2 > 4294967295.0) or (Frac(D2) <> 0) then begin PopRoots(2); Exit(ThrowError('RangeError', 'Invalid array length')); end;
        Idx := ArrayLen(O) - 1;
        while Idx >= Trunc(D2) do
        begin
          S := ShapeLookup(O^.Shape, IntAtom(Idx));
          if S >= 0 then
          begin
            if (O^.Shape^.PFlags[S] and pfConfigurable) = 0 then
            begin
              ArraySetLength(O, Idx + 1);
              PopRoots(2);
              Ok := False;
              Exit(True);
            end;
            DeleteProperty(O, IntAtom(Idx));
            O := ObjOf(RootAt(TempTop - 2));
          end;
          Dec(Idx);
        end;
        if D2 <= 64 * 1024 * 1024 then ArraySetLength(O, Trunc(D2));
        PopRoots(2);
        Exit(True);
      end;
      { 10.4.2.4 ArraySetLength: ToUint32 and ToNumber of the value must agree }
      PushRoot(Obj); PushRoot(V);
      if not ToNumberV(V, D1) or not ToNumberV(RootAt(TempTop - 1), D2) then begin PopRoots(2); Exit(False); end;
      O := ObjOf(RootAt(TempTop - 2));
      PopRoots(2);
      if IsNaN(D2) or (D2 < 0) or (D2 > 4294967295.0) or (Frac(D2) <> 0) then Exit(ThrowError('RangeError', 'Invalid array length'));
      if D2 > 64 * 1024 * 1024 then Exit(ThrowError('RangeError', 'Array length beyond G2''s dense arrays (no sparse arrays yet)'));
      ArraySetLength(O, Trunc(D2));
      Exit(True);
    end;
    { a new index past a read-only length fails (10.4.2.1) }
    if IndexFromString(Key, Idx) and ((O^.OFlags and 128) <> 0) and (Idx >= ArrayLen(O)) then
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
     ((Idx < O^.Elements^.Len) or ArrayDenseOK(O^.Elements, Idx)) then
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
begin
  if IsObject(Obj) then Exit(SetOnObjectS(Obj, Key, V, Strict));
  if IsNullish(Obj) then
    Exit(ThrowTypeError('Cannot set properties of ' + TypeOfStr(Obj) + ' (setting ''' + StrToUnicode(Key) + ''')'));
  { a primitive base: there is no object to hold the property }
  if Strict then Exit(ThrowTypeError('Cannot create property ''' + StrToUnicode(Key) + ''' on ' + TypeOfStr(Obj)));
  Result := True;
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

function GetElemV(Obj, Key: TValue; var Res: TValue): Boolean;
var Idx: Int64; O: PObj; K: PStr; S: PStr;
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
    Exit(TAStoreValue(Obj, Idx, V));
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

{ cold: the BigInteger temporaries out of LooseEquals's frame }
function BigEqualsNumber(Big: TValue; D: Double): Boolean; noinline;
begin
  Result := ValueToBig(Big).ToDouble = D;
end;

function LooseEquals(A, B: TValue; var Res: Boolean): Boolean;
var P: TValue;
begin
  Result := True;
  if IsNullish(A) or IsNullish(B) then begin Res := IsNullish(A) and IsNullish(B); Exit; end;
  if IsNumber(A) and IsNumber(B) then begin Res := NumOf(A) = NumOf(B); Exit; end;
  if IsString(A) and IsString(B) then begin Res := StrEquals(StrOf(A), StrOf(B)); Exit; end;
  if IsBigInt(A) and IsBigInt(B) then begin Res := BigEquals(A, B); Exit; end;
  if IsBigInt(A) and IsNumber(B) then begin Res := BigEqualsNumber(A, NumOf(B)); Exit; end;
  if IsNumber(A) and IsBigInt(B) then begin Res := BigEqualsNumber(B, NumOf(A)); Exit; end;
  if (A = VTrue) or (A = VFalse) then A := MkInt(Ord(A = VTrue));
  if (B = VTrue) or (B = VFalse) then B := MkInt(Ord(B = VTrue));
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

function InstanceOf(V, C: TValue; var Res: Boolean): Boolean;
var P, Proto, H, R: TValue;
begin
  { InstanceofOperator: a Symbol.hasInstance method on the right-hand side decides }
  if IsObject(C) and IsPtr(SymHasInstance) then
  begin
    PushRoot(V); PushRoot(C);
    if not GetV(C, PStr(PPointer(PByte(PtrOf(SymHasInstance)) + 16)^), H) then begin PopRoots(2); Exit(False); end;
    C := RootAt(TempTop - 1); V := RootAt(TempTop - 2);
    if not IsNullish(H) then
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
  if ObjOf(C)^.OKind = okBound then Exit(InstanceOf(V, ObjOf(C)^.Aux1, Res));
  Res := False;
  if not IsObject(V) then Exit(True);
  PushRoot(V);
  if not GetV(C, AtomPrototype, Proto) then begin PopRoots(1); Exit(False); end;
  V := RootAt(TempTop - 1);
  PopRoots(1);
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
    Len := ObjOf(O)^.Elements^.Len;
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
  O := NewObject(RootShapeFor(ArrayIteratorProto), 0, okIterator);
  O^.Aux1 := RootAt(TempTop - 1);
  O^.Aux2 := MkInt(0);
  O^.OFlags := Kind;   { 0 array values, 1 string code points, 2 keys, 3 entries }
  PopRoots(1);
  Result := MkPtr(O);
end;

function GetIterator(V: TValue; var It: TValue): Boolean;
var F: TValue;
begin
  if IsObject(V) and (ObjOf(V)^.OKind = okArray) and (ObjOf(V)^.Shape^.Proto = ArrayProto) then
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
var O: PObj; I: Integer; Src: TValue; S: PStr; NextF, R: TValue; C: Integer; Len: Int64;
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
      if (Ord(StrChars(S)[I]) >= $D800) and (Ord(StrChars(S)[I]) <= $DBFF) and (I + 1 < S^.Len) then C := 2;
      O^.Aux2 := MkInt(I + C);
      Value := SubString(S, I, C);
      Done := False;
      Exit(True);
    end;
    if O^.OFlags = 4 then
    begin
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
    PushRoot(It);
    if not LengthOfArrayLike(Src, Len) then begin PopRoots(1); Exit(False); end;
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
    Exit(GeneratorResume(It, VUndefined, 0, Value, Done));
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
        NativeData := O^.Pad;
        NativeSelf := MkPtr(O);
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
        Exit(ThrowTypeError('Class constructor cannot be invoked without ''new'''));
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
        SetProto(ObjOf(RootAt(TempTop - 1)), Proto);
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
  Proto := FunctionPrototypeOf(NewTarget);
  if not IsObject(Proto) then Proto := ObjectProto;
  ThisO := NewObject(RootShapeFor(Proto), Max(4, T^.ExpectedProps), okPlain);
  F := RootAt(TempTop - 2); NewTarget := RootAt(TempTop - 1);
  PopRoots(2);
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
function PushBytecodeFrame(FObj: PObj; This: TValue; Args: PValue; Argc: Integer; RetIdx: Integer; Kind: Byte;
  NewTarget: TValue): Boolean;
var T: PFuncTemplate; Base, I, NP, Extra: Integer; Fr: PFrame; R: PValue; A: PObj; FV, Tmp, ArgsObj: TValue; FI: Integer;
begin
  T := PFuncTemplate(FObj^.Native);
  Base := (Args - VStack) - 1;
  if (Base + T^.MaxRegs >= STACK_MAX) or (FrameTop >= FRAME_MAX - 2) then
    Exit(ThrowError('RangeError', 'Maximum call stack size exceeded'));
  FV := MkPtr(FObj);
  R := @VStack[Base];
  if not T^.IsStrict and not T^.IsArrow and ((This = VUndefined) or (This = VNull)) then This := GlobalObj;
  R[0] := This;
  NP := T^.NumParams;
  if (T^.RestReg >= 0) or (T^.ArgsReg >= 0) then
  begin
    { collect extra / all arguments before the locals overwrite them }
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
    PopRoots(1);
    StackTop := Max(StackTop, Base + T^.MaxRegs);
    I := Min(Argc, NP) + 1;
    if I < T^.MaxRegs then FillQWord(R[I], T^.MaxRegs - I, VUndefined);
    if T^.RestReg >= 0 then R[T^.RestReg] := Tmp;
    if T^.ArgsReg >= 0 then R[T^.ArgsReg] := ArgsObj;
  end
  else
  begin
    I := Min(Argc, NP) + 1;
    if I < T^.MaxRegs then FillQWord(R[I], T^.MaxRegs - I, VUndefined);
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

{ ---------------- the interpreter ---------------- }

function ICGetEntry(E: PICEntry; O: PObj; var Res: TValue): Boolean;
var P: PObj; D: Integer;
begin
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
  if (IC^.Poly0.Shape = O^.Shape) and (IC^.Poly0.Kind = O^.OKind) then
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
        Acc := PAccessor(PtrOf(V));
        if Acc^.Getter = VUndefined then begin Res := VUndefined; Exit(True); end;
        Exit(CallValue(Acc^.Getter, Receiver, nil, 0, Res));
      end;
      Res := V;
      if Cacheable then
      begin
        {$IFDEF G2_ICSTATS}Inc(ICMissFill);{$ENDIF}
        E.Depth := D;
        E.Slot := S;
        { the first shape stays in entry 0 (checked inline); later ones take entries 1-3 in turn }
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
    if D >= 4 then
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
    if D < 4 then E.Holder[D] := P^.Shape;
    Inc(D);
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
  DA := PrimitiveToNumber(PA);
  DB := PrimitiveToNumber(PB);
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
    opPow: Z := X.Power(Y);
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

function ArithSlow(Op: TOp; A, B: TValue; var Res: TValue): Boolean;
var DA, DB, R: Double; IA, IB: Int32;
begin
  if IsBigInt(A) or IsBigInt(B) then Exit(BigArith(Op, A, B, Res));
  PushRoot(B);
  if not ToNumberV(A, DA) then begin PopRoots(1); Exit(False); end;
  if not ToNumberV(RootAt(TempTop - 1), DB) then begin PopRoots(1); Exit(False); end;
  PopRoots(1);
  case Op of
    opSub: R := DA - DB;
    opMul: R := DA * DB;
    opDiv: R := DA / DB;
    opMod: R := NumberRemainder(DA, DB);
    opPow: R := NumberExponentiate(DA, DB);
  else
    begin
      if IsNaN(DA) or IsInfinite(DA) then IA := 0 else IA := Int32(UInt32(Int64(Trunc(DA)) and $FFFFFFFF));
      if IsNaN(DB) or IsInfinite(DB) then IB := 0 else IB := Int32(UInt32(Int64(Trunc(DB)) and $FFFFFFFF));
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
var PA, PB: TValue; DA, DB: Double; C: Integer;
begin
  PushRoot(B);
  if not ToPrimitiveV(A, False, PA) then begin PopRoots(1); Exit(False); end;
  PushRoot(PA);
  if not ToPrimitiveV(RootAt(TempTop - 2), False, PB) then begin PopRoots(2); Exit(False); end;
  PA := RootAt(TempTop - 1);
  PopRoots(2);
  if IsBigInt(PA) or IsBigInt(PB) then
  begin
    if IsBigInt(PA) and IsBigInt(PB) then C := BigCompare(PA, PB)
    else
    begin
      if IsBigInt(PA) then DA := ValueToBig(PA).ToDouble else DA := PrimitiveToNumber(PA);
      if IsBigInt(PB) then DB := ValueToBig(PB).ToDouble else DB := PrimitiveToNumber(PB);
      if IsNaN(DA) or IsNaN(DB) then begin Res := 2; Exit(True); end;
      if DA < DB then C := -1 else if DA > DB then C := 1 else C := 0;
    end;
    if C < 0 then Res := -1 else if C > 0 then Res := 1 else Res := 0;
    Exit(True);
  end;
  if IsString(PA) and IsString(PB) then
  begin
    C := StrCompare(StrOf(PA), StrOf(PB));
    if C < 0 then Res := -1 else if C > 0 then Res := 1 else Res := 0;
    Exit(True);
  end;
  DA := PrimitiveToNumber(PA);
  DB := PrimitiveToNumber(PB);
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
    ckObject: if IsCallable(V) then Result := MkPtr(A^.Fn) else Result := MkPtr(A^.Obj);
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
var Keys, V: TValue; I, N: Integer; K: PStr;
begin
  if not IsObject(Src) then Exit(True);
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

function ForInKeys(V: TValue; var It: TValue): Boolean;
var A, K, P: TValue; I, N: Integer; O: PObj;
begin
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
  P := ObjOf(RootAt(TempTop - 2))^.Shape^.Proto;
  while IsObject(P) and (P <> ObjectProto) and (P <> ArrayProto) and (P <> FunctionProto) do
  begin
    PushRoot(P);
    K := OwnKeys(ObjOf(P), True);
    if OwnKeysFailed then begin OwnKeysFailed := False; PopRoots(3); Exit(False); end;
    N := ArrayLen(ObjOf(K));
    PushRoot(K);
    for I := 0 to N - 1 do ArrayPush(ObjOf(RootAt(TempTop - 3)), ObjOf(RootAt(TempTop - 1))^.Elements^.Data[I]);
    P := ObjOf(RootAt(TempTop - 2))^.Shape^.Proto;
    PopRoots(2);
  end;
  It := NewArrayIterator(RootAt(TempTop - 1), 0);
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
var Fr: PFrame; T: PFuncTemplate; State: PValueArray; G: PObj; I: Integer; P: TValue;
begin
  Fr := @Frames[FrameTop - 1];
  T := Fr^.Tpl;
  State := NewValueArray(T^.MaxRegs + 4);
  State^.Len := T^.MaxRegs + 4;
  Fr := @Frames[FrameTop - 1];
  Move(VStack[Fr^.Base], State^.Data[0], T^.MaxRegs * SizeOf(TValue));
  State^.Data[T^.MaxRegs] := MkInt(-1);
  State^.Data[T^.MaxRegs + 1] := MkInt(T^.MaxRegs);
  State^.Data[T^.MaxRegs + 2] := MkInt(0);
  State^.Data[T^.MaxRegs + 3] := MkInt(0);   { no open upvalues: nothing ran yet }
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
  if ResumeReg <= -2 then
  begin
    { suspended in yield*: the step op gets the received value and mode and runs again }
    VStack[Base - ResumeReg - 2] := Arg;
    VStack[Base - ResumeReg - 1] := MkInt(Mode);
    Mode := 0;
  end
  else if (ResumeReg >= 0) and (Mode = 0) then VStack[Base + ResumeReg] := Arg;
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
      opGetProp, opGetLength, opGetGlobal: Exit(StrToUnicode(T^.ICs[I shr 40].Key));
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

function XThrowNotDefined(Key: PStr): Boolean; noinline;
begin
  Result := ThrowError('ReferenceError', StrToUnicode(Key) + ' is not defined');
end;

function XPrivateKey(Name: PStr): PStr; noinline;
begin
  Result := NewKeyAtom(StrToUnicode(Name), kfPrivate);
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

function XBigNegate(V: TValue): TValue; noinline;
begin
  Result := BigToValue(ValueToBig(V).Negate);
end;

function Execute(StopFrame: Integer; var Res: TValue): Boolean;
label LThrow, LReload, LNext;
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
begin
LReload:
  Fr := @Frames[FrameTop - 1];
  Tpl := Fr^.Tpl;
  StackTop := Fr^.Base + Tpl^.MaxRegs;
  Code := @Tpl^.Code[0];
  PC := Fr^.PC;
  R := @VStack[Fr^.Base];
  if Length(Tpl^.Consts) > 0 then K := @Tpl^.Consts[0] else K := nil;
  if InjectThrow then
  begin
    InjectThrow := False;
    goto LThrow;
  end;
  while True do
  begin
    I := Code[PC];
    Inc(PC);
    A := (I shr 8) and $FFFF;
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
              if InheritedHas(O, IC^.Key) then
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
            if (CI < 0) and Tpl^.IsStrict and not InheritedHas(O, IC^.Key) then
            begin
              XThrowNotDefined(IC^.Key);
              goto LThrow;
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
        R[A] := MkPtr(XPrivateKey(PStr(PtrOf(K[I shr 40]))));
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
          EvalHintMode := A + 1; EvalHintFrame := FrameTop; EvalHintPC := PC; EvalHintDesc := I shr 40;
        end;

      opGetProp, opGetLength:
        begin
          VB := R[(I shr 24) and $FFFF];
          IC := @Tpl^.ICs[I shr 40];
          if (VB and TAG_MASK) = TAG_PTR then
          begin
            O := PObj(PtrUInt(VB and PTR_MASK));
            if O^.Hdr.Kind = ckObject then
            begin
              if ICGet(IC, O, R[A]) then Continue;
              if (TOp(I and $FF) = opGetLength) and (O^.OKind = okArray) then
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
              if not ICGetMiss(IC, O, VB, V) then goto LThrow;
              R[A] := V;
              Continue;
            end
            else if (O^.Hdr.Kind = ckString) and (TOp(I and $FF) = opGetLength) then
            begin
              R[A] := MkInt(PStr(O)^.Len);
              Continue;
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
            end;
            { slow path, then try to cache an own data store or an add transition }
            Fr^.PC := PC;
            CI := ShapeLookup(O^.Shape, IC^.Key);
            if (CI >= 0) and ((O^.Shape^.PFlags[CI] and (pfAccessor or pfWritable)) = pfWritable) and not O^.Shape^.IsDict then
            begin
              IC^.Shape := O^.Shape; IC^.Slot := CI; IC^.NewShape := nil;
              SetSlot(O, CI, V);
              Continue;
            end;
            if CI < 0 then
            begin
              IC^.Misses := IC^.Misses;
              VB := MkPtr(O^.Shape);
              if not SetOnObjectS(VA, IC^.Key, V, Tpl^.IsStrict) then goto LThrow;
              O := ObjOf(R[A]);
              if (O^.OKind = okPlain) and (O^.Shape^.Parent = PShape(PtrOf(VB))) and (O^.Shape^.Count > 0) and
                 (O^.Shape^.Keys[O^.Shape^.Count - 1] = IC^.Key) then
              begin
                IC^.Shape := PShape(PtrOf(VB));
                IC^.NewShape := O^.Shape;
                IC^.Slot := O^.Shape^.Count - 1;
              end;
              Continue;
            end;
            if not SetOnObjectS(VA, IC^.Key, V, Tpl^.IsStrict) then goto LThrow;
            Continue;
          end;
          Fr^.PC := PC;
          if not SetVS(VA, IC^.Key, V, Tpl^.IsStrict) then goto LThrow;
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
               (O^.OKind = okArray) then
            begin
              E^.Data[CI] := V;
              WriteBarrier(@E^.Hdr, V);
              E^.Len := CI + 1;
              Continue;
            end;
            if (O^.OKind = okTypedArray) and (CI >= 0) and (CI < TALength(O)) and not TAIsBig(O) then
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
          else if IsObject(VB) then Bool := DeleteIfConfigurable(ObjOf(VB), PStr(PtrOf(K[I shr 40])));
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
          end;
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
          { a computed key: opToPropKey leaves integers as they are }
          if CI < 0 then
          begin
            if not ToPropKey(R[-1 - CI], Key) then goto LThrow;
          end
          else Key := Tpl^.ICs[CI].Key;
          VB := R[(I shr 24) and $FFFF];   { home object }
          if not IsObject(VB) or not IsObject(ObjOf(VB)^.Shape^.Proto) then begin R[A] := VUndefined; Continue; end;
          if not GetFromObject(ObjOf(ObjOf(VB)^.Shape^.Proto), Key, R[0], V) then goto LThrow;
          R[A] := V;
        end;

      opAdd:
        begin
          VA := R[(I shr 24) and $FFFF];
          VB := R[I shr 40];
          if ((VA and VB and TAG_MASK) = TAG_INT) and ((VA and TAG_MASK) = TAG_INT) and ((VB and TAG_MASK) = TAG_INT) then
          begin
            Sum := Int64(Int32(UInt32(VA))) + Int64(Int32(UInt32(VB)));
            if (Sum >= -2147483648) and (Sum <= 2147483647) then R[A] := TAG_INT or (QWord(Sum) and $FFFFFFFF)
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
      opAddI:
        begin
          VA := R[(I shr 24) and $FFFF];
          CI := SarInt64(Int64(I), 40);
          if (VA and TAG_MASK) = TAG_INT then
          begin
            Sum := Int64(Int32(UInt32(VA))) + CI;
            if (Sum >= -2147483648) and (Sum <= 2147483647) then R[A] := TAG_INT or (QWord(Sum) and $FFFFFFFF)
            else R[A] := MkDouble(Sum);
          end
          else if VA < DOUBLE_LIMIT then R[A] := MkDouble(PDouble(@VA)^ + CI)
          else if IsBigInt(VA) then
          begin
            Fr^.PC := PC;
            R[A] := XBigAddInt(VA, CI);
          end
          else
          begin
            Fr^.PC := PC;
            if not AddNumbersSlow(VA, MkInt(CI), V) then goto LThrow;
            R[A] := V;
          end;
        end;
      opSub, opMul:
        begin
          VA := R[(I shr 24) and $FFFF];
          VB := R[I shr 40];
          if ((VA and TAG_MASK) = TAG_INT) and ((VB and TAG_MASK) = TAG_INT) then
          begin
            if TOp(I and $FF) = opSub then Sum := Int64(Int32(UInt32(VA))) - Int64(Int32(UInt32(VB)))
            else Sum := Int64(Int32(UInt32(VA))) * Int64(Int32(UInt32(VB)));
            if (Sum >= -2147483648) and (Sum <= 2147483647) and not ((Sum = 0) and (TOp(I and $FF) = opMul) and
               ((Int32(UInt32(VA)) < 0) or (Int32(UInt32(VB)) < 0))) then
              R[A] := TAG_INT or (QWord(Sum) and $FFFFFFFF)
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
          if ((VA and TAG_MASK) = TAG_INT) and ((VB and TAG_MASK) = TAG_INT) and (Int32(UInt32(VB)) > 0) and
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
          if ((VA and TAG_MASK) = TAG_INT) and ((VB and TAG_MASK) = TAG_INT) then
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
            if not ToNumberV(VA, D) then goto LThrow;
            R[A] := MkDouble(-D);
          end;
        end;
      opBitNot:
        begin
          VA := R[(I shr 24) and $FFFF];
          if IsInt(VA) then R[A] := MkInt(not IntOf(VA))
          else
          begin
            Fr^.PC := PC;
            if not ArithSlow(opBitXor, VA, MkInt(-1), V) then goto LThrow;
            R[A] := V;
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
          if ((VA and TAG_MASK) = TAG_INT) and ((VB and TAG_MASK) = TAG_INT) then
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
              V := FunctionPrototypeOf(VA);
              if not IsObject(V) then V := ObjectProto;
              O := NewObject(RootShapeFor(V), Max(4, T^.ExpectedProps), okPlain);
              R[A + 1] := MkPtr(O);
              VA := R[A];
              if Assigned(T^.FieldInit) and not T^.IsDerived then
              begin
                StackTop := Max(StackTop, Fr^.Base + A + 2 + Argc);
                if not RunFieldInit(R[A], R[A + 1]) then goto LThrow;
              end;
              VA := R[A];
              StackTop := Max(StackTop, Fr^.Base + A + 2 + Argc);
              if not PushBytecodeFrame(ObjOf(VA), R[A + 1], @R[A + 2], Argc, Fr^.Base + A, 1, VA) then goto LThrow;
              goto LReload;
            end;
            { native or bound constructor }
            if not Construct(VA, @R[A + 2], Argc, VA, V) then goto LThrow;
            R[A] := V;
            Continue;
          end;
          if O^.OKind = okFunction then
          begin
            if (O^.OFlags and 1) <> 0 then
            begin
              ThrowTypeError('Class constructor cannot be invoked without ''new''');
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
          if O^.OKind = okNative then
          begin
            SavedTop := StackTop;
            StackTop := Fr^.Base + A + 2 + Argc;
            V := VUndefined;
            PushRoot(CurNewTarget);   { a plain call: the native must not see an outer new.target }
            CurNewTarget := VUndefined;
            NP2 := TempTop;
            NativeData := O^.Pad;
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
          { 13.3.7.1 SuperCall: GetSuperConstructor is the running constructor's [[GetPrototypeOf]] }
          VA := ObjOf(Fr^.Closure)^.Shape^.Proto;
          if not IsConstructorV(VA) then begin ThrowTypeError('Super constructor is not a constructor'); goto LThrow; end;
          StackTop := Max(StackTop, Fr^.Base + A + 2 + Argc);
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
          if TOp(I and $FF) = opRet then V := R[A] else V := VUndefined;
          if (Fr^.Kind = 1) and not IsObject(V) then
          begin
            { 10.2.2 [[Construct]] steps 10-12 }
            if Tpl^.IsDerived and Tpl^.IsClassCtor then
            begin
              Fr^.PC := PC;
              if V <> VUndefined then
              begin
                ThrowTypeError('Derived constructors may only return object or undefined');
                goto LThrow;
              end;
              if R[0] = VHole then
              begin
                ThrowError('ReferenceError', 'Must call super constructor in derived class before accessing ''this'' or returning from derived constructor');
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
            Exit(True);
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
          if not CopyDataProps(R[A], R[(I shr 24) and $FFFF]) then goto LThrow;
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
          if not GetV(R[(I shr 24) and $FFFF], PStr(PtrOf(K[I shr 40])), V) then goto LThrow;
          R[A] := V;
        end;
      opWithSet:
        begin
          Fr^.PC := PC;
          if not SetVS(R[A], PStr(PtrOf(K[I shr 40])), R[(I shr 24) and $FFFF], False) then goto LThrow;
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
          if not Assigned(ImportHook) then begin ThrowError('SyntaxError', 'import() is not supported by this host'); goto LThrow; end;
          if not ImportHook(Tpl^.SourceName, Tpl^.ModuleId, R[(I shr 24) and $FFFF], VB, V) then begin ImportPhase := 0; goto LThrow; end;
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
          if CI >= 0 then SetSlot(O, CI, V) else AddProperty(O, Key, V, pfDefault);
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
          if not ForInKeys(R[(I shr 24) and $FFFF], V) then goto LThrow;
          R[A] := V;
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
          Exit(True);
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
                Exit(True);
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
                    Exit(True);
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
                Exit(True);
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
      opThrowConst:
        begin
          Fr^.PC := PC;
          ThrowTypeError('Assignment to constant variable.');
          goto LThrow;
        end;
      opIterCloseThrow:
        if IsObject(R[A]) then
        begin
          VB := R[A];
          R[A] := VUndefined;
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
          Exit(True);
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
            Exit(True);
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
      Dec(FrameTop);
    end;
    Res := VUndefined;
    Exit(False);
  end;
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
    { [description], or empty without one }
    S := PValue(PByte(PtrOf(KeyPayload(Key)^)) + 8)^;
    if IsString(S) and (StrOf(S)^.Len > 0) then N := '[' + StrToUnicode(StrOf(S)) + ']' else N := '';
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

end.
