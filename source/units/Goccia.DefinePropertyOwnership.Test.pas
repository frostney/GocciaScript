program Goccia.DefinePropertyOwnership.Test;

{ A definition takes its descriptor on every exit (ADR 0184): DefineProperty,
  DefineSymbolProperty and their Try forms store or free it whether they
  succeed, refuse or raise, so a descriptor built inline in the call never
  leaks when something raises inside it.

  The first suite raises inside the ordinary object, function, class, array,
  typed array, String object, mapped arguments and Proxy definitions, at
  points that can raise there (a host's abort at a value allocation, the
  memory limit at the property map's growth, a TypeError, a lazy factory,
  guest code), and after a heap snapshot restore's commit, and counts how
  often the descriptor it passed was freed: exactly once.

  The second suite is the embedder's case end to end. A host that arms its
  abort handle before it builds the engine can have a request land anywhere in
  built-in registration, and one that arrives while a script runs can land in
  any definition the script reaches. It runs on a worker thread, as a host
  does, arms the handle before the engine is built, and requests the abort
  when that thread makes its Nth heap allocation counted from the start of
  one phase: the build (a trial ends once the engine exists) or the run of a
  definition-heavy script on an engine built unstopped, for every second N up
  to that phase's whole count in a clean trial. A request lands at the next value
  allocation, so every N from the block before it up to that allocation's own
  block stops the run at the same point: a definition's descriptor and the
  value that raises inside the definition are two blocks of that range, and
  every second N reaches each such point. Against the engine before ADR 0184
  every leaking range was two blocks wide or more. A memory manager wrapped
  around the default one tracks the blocks the worker allocates; any that are
  still allocated once the worker's runtime has shut down leaked. }

{$I Goccia.inc}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes,
  SysUtils,

  TestingPascalLibrary,

  Goccia.Arguments.Collection,
  Goccia.Capabilities,
  Goccia.Constants.PropertyNames,
  Goccia.Engine,
  Goccia.ExecutionAbort,
  Goccia.GarbageCollector,
  Goccia.InstructionLimit,
  Goccia.MemoryLimit,
  Goccia.Realm,
  Goccia.Runtime,
  Goccia.TestSetup,
  Goccia.Threading,
  Goccia.Threading.Init,
  Goccia.Timeout,
  Goccia.Values.ArgumentsObjectValue,
  Goccia.Values.ArrayValue,
  Goccia.Values.ClassValue,
  Goccia.Values.Error,
  Goccia.Values.ErrorHelper,
  Goccia.Values.NativeFunction,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives,
  Goccia.Values.ProxyValue,
  Goccia.Values.Shape,
  Goccia.Values.StringObjectValue,
  Goccia.Values.SymbolValue,
  Goccia.Values.TypedArrayValue;

const
  { Allocation-heavy definitions a script reaches: an intrinsic name or
    length materialized on a function and a class, a String object's index,
    class statics and an object built from entries. }
  SCRIPT_DEFINITIONS =
    'const made = [];' + LineEnding +
    'for (const i of [1, 2, 3, 4, 5, 6, 7, 8]) {' + LineEnding +
    '  const f = () => i;' + LineEnding +
    '  Reflect.defineProperty(f, "name", { value: "f" + i });' + LineEnding +
    '  Object.defineProperty(f, "length", { value: i });' + LineEnding +
    '  class C { static s = i; static m() { return i; } }' + LineEnding +
    '  Reflect.defineProperty(C, "name", { value: "C" + i });' + LineEnding +
    '  const s = new String("ab" + i);' + LineEnding +
    '  Reflect.defineProperty(s, "1", { value: "b" });' + LineEnding +
    '  made.push(f, C, s, Object.fromEntries([["k" + i, i]]));' + LineEnding +
    '}' + LineEnding +
    'export const n = made.length;';
  { Every second allocation: see the header. }
  SWEEP_STRIDE = 2;

type
  { A data descriptor that counts its frees. }
  TCountedDescriptor = class(TGocciaPropertyDescriptorData)
  public
    destructor Destroy; override;
  end;

  { A lazy one, for the definitions that store a lazy descriptor whole. }
  TCountedLazyDescriptor = class(TGocciaLazyPropertyDescriptorData)
  public
    destructor Destroy; override;
  end;

  { A property map whose growth gate refuses while GRefuseGrowth is set, as
    the memory limit refuses a growth it cannot fit. }
  TRefusingPropertyMap = class(TGocciaShapedPropertyMap)
  protected
    procedure ConsultStorageBudget(const ABytes, ANewBytes: Int64); override;
  end;

  TRefusingObjectValue = class(TGocciaObjectValue)
  public
    constructor CreateRefusing;
    function MapHasRoom: Boolean;
    procedure KeepRefusedValues;
    procedure StoreRaw(const AName: string;
      const ADescriptor: TGocciaPropertyDescriptor);
  end;

  TOwnershipTests = class(TTestSuite)
  private
    FRealm: TGocciaRealm;
    FPreviousRealm: TGocciaRealm;
    FHandle: IGocciaAbortHandle;
    function Counted(const AValue: TGocciaValue = nil): TCountedDescriptor;
    function MakeString: TGocciaValue;
    function NoOp(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function RefusingTrap(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    procedure RequestAbortNow;
    procedure TestFunctionIntrinsicAbortFreesOnce;
    procedure TestFunctionIntrinsicAbortTryFreesOnce;
    procedure TestClassIntrinsicAbortFreesOnce;
    procedure TestLazyMaterializeAbortFreesOnce;
    procedure TestStringObjectIndexAbortFreesOnce;
    procedure TestArrayExtensionAbortFreesOnce;
    procedure TestRefusedGrowthFreesOnce;
    procedure TestRefusedGrowthOfALazyDescriptorFreesOnce;
    procedure TestRefusedGrowthTryFreesOnce;
    procedure TestRefusedGrowthKeepsAValueTheMapWasNotGiven;
    procedure TestNonExtensibleThrowFreesOnce;
    procedure TestNonExtensibleSymbolThrowFreesOnce;
    procedure TestArrayLengthRangeErrorFreesOnce;
    procedure TestClassPrototypeThrowFreesOnce;
    procedure TestRevokedProxyThrowFreesOnce;
    procedure TestProxyTrapRefusalFreesOnce;
    procedure TestSuccessfulDefinitionsFreeOnce;
    procedure TestTypedArrayNumericKeyAbortFreesOnce;
    procedure TestMappedArgumentsLazyCopyThrowFreesOnce;
    procedure TestRestoredMapFreesRefusedValuesAgain;
    procedure TestLazySubclassFactoryThrowFreesOnce;
    procedure TestLazySubclassFactoryThrowTryFreesOnce;
    procedure TestTryDefineSymbolPropertyRefusalFreesOnce;
    function ThrowingFactory: TGocciaValue;
  protected
    procedure BeforeEach; override;
    procedure AfterEach; override;
  public
    procedure SetupTests; override;
  end;

  { One engine build and run on its own thread, with the abort handle armed
    before the engine exists and requested at the thread's AAbortAtBlock-th
    heap allocation (0: never). The outcome is a code rather than a string so
    that nothing the worker allocates outlives it. }
  TAbortAtRun = class(TThread)
  private
    FAbortAtBlock: Int64;
    FInScript: Boolean;
  protected
    procedure Execute; override;
  public
    Aborted: Boolean;
    Completed: Boolean;
    { Anything else that ended the trial: a failure of the test itself. }
    Unexpected: Boolean;
    BlocksSeen: Int64;
    constructor Create(const AAbortAtBlock: Int64; const AInScript: Boolean);
  end;

  TEmbedderAbortTests = class(TTestSuite)
  private
    function Sweep(const AInScript: Boolean; out ATrials, AAborted,
      ALeakingTrials, AUnexpected: Integer; out ALeakedBlocks: Int64): Int64;
    procedure TestControlRunLeavesNothing;
    procedure TestAbortAnywhereInTheBuildLeavesNothing;
    procedure TestAbortAnywhereInAScriptLeavesNothing;
  public
    procedure SetupTests; override;
  end;

var
  GFreed: Integer;
  GRefuseGrowth: Boolean;

{ The tracking memory manager }

const
  TRACK_SLOTS = 1 shl 17;
  TOMBSTONE = Pointer(1);

var
  PreviousManager: TMemoryManager;
  TrackTable: array[0..TRACK_SLOTS - 1] of Pointer;
  TrackLive: Int64;
  TrackActive: Boolean;
  TrackOverflow: Boolean;
  TrackLock: LongInt;

threadvar
  TrackThisThread: Boolean;
  BlocksOnThisThread: Int64;
  AbortAtBlock: Int64;
  AbortHandleToRequest: Pointer;
  InAbortRequest: Boolean;

procedure LockTrack;
begin
  while InterlockedCompareExchange(TrackLock, 1, 0) <> 0 do
    ThreadSwitch;
end;

procedure UnlockTrack;
begin
  InterlockedExchange(TrackLock, 0);
end;

function SlotOf(const APointer: Pointer): PtrUInt;
begin
  Result := (PtrUInt(APointer) shr 4) and (TRACK_SLOTS - 1);
end;

procedure TrackAdd(const APointer: Pointer);
var
  I, Probes: PtrUInt;
begin
  if APointer = nil then
    Exit;
  LockTrack;
  try
    I := SlotOf(APointer);
    Probes := 0;
    while (TrackTable[I] <> nil) and (TrackTable[I] <> TOMBSTONE) do
    begin
      I := (I + 1) and (TRACK_SLOTS - 1);
      Inc(Probes);
      if Probes >= TRACK_SLOTS then
      begin
        TrackOverflow := True;
        Exit;
      end;
    end;
    TrackTable[I] := APointer;
    Inc(TrackLive);
  finally
    UnlockTrack;
  end;
end;

procedure TrackRemove(const APointer: Pointer);
var
  I, Probes: PtrUInt;
begin
  if (APointer = nil) or not TrackActive then
    Exit;
  LockTrack;
  try
    I := SlotOf(APointer);
    Probes := 0;
    while TrackTable[I] <> nil do
    begin
      if TrackTable[I] = APointer then
      begin
        TrackTable[I] := TOMBSTONE;
        Dec(TrackLive);
        Exit;
      end;
      I := (I + 1) and (TRACK_SLOTS - 1);
      Inc(Probes);
      if Probes >= TRACK_SLOTS then
        Exit;
    end;
  finally
    UnlockTrack;
  end;
end;

procedure NoteAllocation(const APointer: Pointer);
begin
  if not TrackThisThread or InAbortRequest then
    Exit;
  TrackAdd(APointer);
  Inc(BlocksOnThisThread);
  if (AbortAtBlock > 0) and (BlocksOnThisThread = AbortAtBlock) and
     Assigned(AbortHandleToRequest) then
  begin
    // Request takes a lock and allocates nothing; the guard keeps a block
    // it might allocate from counting or requesting again.
    InAbortRequest := True;
    try
      IGocciaAbortHandle(AbortHandleToRequest).Request;
    finally
      InAbortRequest := False;
    end;
  end;
end;

function TrackingGetMem(ASize: PtrUInt): Pointer;
begin
  Result := PreviousManager.GetMem(ASize);
  NoteAllocation(Result);
end;

function TrackingAllocMem(ASize: PtrUInt): Pointer;
begin
  Result := PreviousManager.AllocMem(ASize);
  NoteAllocation(Result);
end;

function TrackingFreeMem(P: Pointer): PtrUInt;
begin
  TrackRemove(P);
  Result := PreviousManager.FreeMem(P);
end;

function TrackingFreeMemSize(P: Pointer; ASize: PtrUInt): PtrUInt;
begin
  TrackRemove(P);
  Result := PreviousManager.FreeMemSize(P, ASize);
end;

function TrackingReAllocMem(var P: Pointer; ASize: PtrUInt): Pointer;
var
  Old: Pointer;
begin
  Old := P;
  TrackRemove(Old);
  Result := PreviousManager.ReAllocMem(P, ASize);
  if (Result <> nil) and (TrackThisThread or (Old <> nil)) then
    NoteAllocation(Result);
end;

procedure InstallTrackingManager;
var
  Tracking: TMemoryManager;
begin
  GetMemoryManager(PreviousManager);
  Tracking := PreviousManager;
  Tracking.GetMem := TrackingGetMem;
  Tracking.AllocMem := TrackingAllocMem;
  Tracking.FreeMem := TrackingFreeMem;
  Tracking.FreeMemSize := TrackingFreeMemSize;
  Tracking.ReAllocMem := TrackingReAllocMem;
  SetMemoryManager(Tracking);
end;

procedure StartTracking;
begin
  FillChar(TrackTable, SizeOf(TrackTable), 0);
  TrackLive := 0;
  TrackOverflow := False;
  TrackActive := True;
end;

function StopTracking: Int64;
begin
  TrackActive := False;
  Result := TrackLive;
  if TrackOverflow then
    Result := -1;
end;

{ TCountedDescriptor }

destructor TCountedDescriptor.Destroy;
begin
  Inc(GFreed);
  inherited Destroy;
end;

destructor TCountedLazyDescriptor.Destroy;
begin
  Inc(GFreed);
  inherited Destroy;
end;

{ TRefusingPropertyMap }

procedure TRefusingPropertyMap.ConsultStorageBudget(const ABytes,
  ANewBytes: Int64);
begin
  if GRefuseGrowth then
    raise TGocciaMemoryLimitError.Create(ABytes, 0);
  inherited ConsultStorageBudget(ABytes, ANewBytes);
end;

{ TRefusingObjectValue }

constructor TRefusingObjectValue.CreateRefusing;
var
  Map: TRefusingPropertyMap;
begin
  inherited Create;
  FProperties.Free;
  Map := TRefusingPropertyMap.Create;
  Map.Owner := Self;
  FProperties := Map;
end;

function TRefusingObjectValue.MapHasRoom: Boolean;
begin
  Result := FProperties.HasRoomForAdd;
end;

procedure TRefusingObjectValue.KeepRefusedValues;
begin
  TRefusingPropertyMap(FProperties).KeepsRefusedValue := True;
end;

procedure TRefusingObjectValue.StoreRaw(const AName: string;
  const ADescriptor: TGocciaPropertyDescriptor);
begin
  FProperties.Add(AName, ADescriptor);
end;

{ TOwnershipTests }

procedure TOwnershipTests.SetupTests;
begin
  Test('an abort materializing a function''s name frees the descriptor once',
    TestFunctionIntrinsicAbortFreesOnce);
  Test('the same through TryDefineProperty',
    TestFunctionIntrinsicAbortTryFreesOnce);
  Test('an abort synthesizing a class''s name frees the descriptor once',
    TestClassIntrinsicAbortFreesOnce);
  Test('an abort materializing a lazy property frees the descriptor once',
    TestLazyMaterializeAbortFreesOnce);
  Test('an abort building a String object''s index frees the descriptor once',
    TestStringObjectIndexAbortFreesOnce);
  Test('an abort extending an array''s elements frees the descriptor once',
    TestArrayExtensionAbortFreesOnce);
  Test('a refused property-map growth frees the descriptor once',
    TestRefusedGrowthFreesOnce);
  Test('a refused growth storing a lazy descriptor frees it once',
    TestRefusedGrowthOfALazyDescriptorFreesOnce);
  Test('a refused growth through TryDefineProperty frees the descriptor once',
    TestRefusedGrowthTryFreesOnce);
  Test('a map that keeps refused values leaves the descriptor to its owner',
    TestRefusedGrowthKeepsAValueTheMapWasNotGiven);
  Test('a TypeError for a non-extensible object frees the descriptor once',
    TestNonExtensibleThrowFreesOnce);
  Test('the same for a symbol key', TestNonExtensibleSymbolThrowFreesOnce);
  Test('a RangeError for an array length frees the descriptor once',
    TestArrayLengthRangeErrorFreesOnce);
  Test('a TypeError redefining a class''s prototype frees the descriptor once',
    TestClassPrototypeThrowFreesOnce);
  Test('a revoked Proxy frees the descriptor once',
    TestRevokedProxyThrowFreesOnce);
  Test('a Proxy trap that refuses frees the descriptor once',
    TestProxyTrapRefusalFreesOnce);
  Test('successful definitions free the descriptor once',
    TestSuccessfulDefinitionsFreeOnce);
  Test('an abort deciding a typed array''s numeric key frees the descriptor once',
    TestTypedArrayNumericKeyAbortFreesOnce);
  Test('a throwing copy of a lazy descriptor on mapped arguments frees it once',
    TestMappedArgumentsLazyCopyThrowFreesOnce);
  Test('a restored object''s map frees a refused value again',
    TestRestoredMapFreesRefusedValuesAgain);
  Test('a stored lazy subclass whose factory throws frees the descriptor once',
    TestLazySubclassFactoryThrowFreesOnce);
  Test('the same through TryDefineProperty',
    TestLazySubclassFactoryThrowTryFreesOnce);
  Test('TryDefineSymbolProperty refusing a non-extensible object frees it once',
    TestTryDefineSymbolPropertyRefusalFreesOnce);
end;

procedure TOwnershipTests.BeforeEach;
begin
  inherited BeforeEach;
  GFreed := 0;
  GRefuseGrowth := False;
  ClearAbort;
  FPreviousRealm := CurrentRealm;
  FRealm := TGocciaRealm.Create('define-property-ownership-test');
  SetCurrentRealm(FRealm);
end;

procedure TOwnershipTests.AfterEach;
begin
  GRefuseGrowth := False;
  FHandle := nil;
  ClearAbort;
  ClearInstructionLimit;
  ClearExecutionTimeout;
  SetCurrentRealm(FPreviousRealm);
  FRealm.Free;
  inherited AfterEach;
end;

function TOwnershipTests.Counted(
  const AValue: TGocciaValue): TCountedDescriptor;
begin
  if Assigned(AValue) then
    Result := TCountedDescriptor.Create(AValue, [pfConfigurable])
  else
    Result := TCountedDescriptor.Create(
      TGocciaUndefinedLiteralValue.UndefinedValue, [pfConfigurable]);
end;

function TOwnershipTests.MakeString: TGocciaValue;
begin
  Result := TGocciaStringLiteralValue.Create('materialized');
end;

function TOwnershipTests.NoOp(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TOwnershipTests.RefusingTrap(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaBooleanLiteralValue.FalseValue;
end;

{ The abort is sticky: from here every value allocation on this thread
  raises, so the next one inside the definition is where it lands. }
procedure TOwnershipTests.RequestAbortNow;
begin
  FHandle := CaptureAbortHandle;
  FHandle.Request;
end;

{ 'throw' for an error a script could catch, else the Pascal class. }
function RaisedName(const AError: Exception): string;
begin
  if AError is TGocciaThrowValue then
    Result := 'throw'
  else
    Result := AError.ClassName;
end;

procedure TOwnershipTests.TestFunctionIntrinsicAbortFreesOnce;
var
  Raised: string;
  Fn: TGocciaNativeFunctionValue;
  Descriptor: TCountedDescriptor;
begin
  Fn := TGocciaNativeFunctionValue.CreateWithoutPrototype(NoOp, 'probe', 1);
  Descriptor := Counted;
  RequestAbortNow;
  Raised := 'nothing';
  try
    Fn.DefineProperty(PROP_NAME, Descriptor);
  except
    on E: Exception do
      Raised := RaisedName(E);
  end;
  ClearAbort;
  Expect<string>(Raised).ToBe(TGocciaAbortError.ClassName);
  Expect<Integer>(GFreed).ToBe(1);
end;

procedure TOwnershipTests.TestFunctionIntrinsicAbortTryFreesOnce;
var
  Raised: string;
  Fn: TGocciaNativeFunctionValue;
  Descriptor: TCountedDescriptor;
begin
  Fn := TGocciaNativeFunctionValue.CreateWithoutPrototype(NoOp, 'probe', 1);
  Descriptor := Counted;
  RequestAbortNow;
  Raised := 'nothing';
  try
    Fn.TryDefineProperty(PROP_NAME, Descriptor);
  except
    on E: Exception do
      Raised := RaisedName(E);
  end;
  ClearAbort;
  Expect<string>(Raised).ToBe(TGocciaAbortError.ClassName);
  Expect<Integer>(GFreed).ToBe(1);
end;

procedure TOwnershipTests.TestClassIntrinsicAbortFreesOnce;
var
  Raised: string;
  ClassValue: TGocciaClassValue;
  Descriptor: TCountedDescriptor;
begin
  ClassValue := TGocciaClassValue.Create('Probe', nil);
  Descriptor := Counted;
  RequestAbortNow;
  Raised := 'nothing';
  try
    ClassValue.DefineProperty(PROP_NAME, Descriptor);
  except
    on E: Exception do
      Raised := RaisedName(E);
  end;
  ClearAbort;
  Expect<string>(Raised).ToBe(TGocciaAbortError.ClassName);
  Expect<Integer>(GFreed).ToBe(1);
end;

procedure TOwnershipTests.TestLazyMaterializeAbortFreesOnce;
var
  Raised: string;
  Obj: TGocciaObjectValue;
  Descriptor: TCountedDescriptor;
begin
  Obj := TGocciaObjectValue.Create;
  Obj.DefineProperty('lazy', TGocciaLazyPropertyDescriptorData.Create(
    MakeString, [pfConfigurable, pfWritable]));
  Descriptor := Counted;
  RequestAbortNow;
  Raised := 'nothing';
  try
    Obj.DefineProperty('lazy', Descriptor);
  except
    on E: Exception do
      Raised := RaisedName(E);
  end;
  ClearAbort;
  Expect<string>(Raised).ToBe(TGocciaAbortError.ClassName);
  Expect<Integer>(GFreed).ToBe(1);
end;

procedure TOwnershipTests.TestStringObjectIndexAbortFreesOnce;
var
  Raised: string;
  StringObject: TGocciaStringObjectValue;
  Descriptor: TCountedDescriptor;
begin
  StringObject := TGocciaStringObjectValue.Create(
    TGocciaStringLiteralValue.Create('ab'));
  Descriptor := Counted;
  RequestAbortNow;
  Raised := 'nothing';
  try
    StringObject.DefineProperty('1', Descriptor);
  except
    on E: Exception do
      Raised := RaisedName(E);
  end;
  ClearAbort;
  Expect<string>(Raised).ToBe(TGocciaAbortError.ClassName);
  Expect<Integer>(GFreed).ToBe(1);
end;

procedure TOwnershipTests.TestArrayExtensionAbortFreesOnce;
var
  Raised: string;
  ArrayValue: TGocciaArrayValue;
  Descriptor: TCountedDescriptor;
begin
  ArrayValue := TGocciaArrayValue.Create;
  // Far enough past the end that the extension polls between batches of
  // holes, near enough that the element stays dense.
  Descriptor := TCountedDescriptor.Create(
    TGocciaNumberLiteralValue.Create(7),
    [pfEnumerable, pfConfigurable, pfWritable]);
  RequestAbortNow;
  Raised := 'nothing';
  try
    ArrayValue.DefineProperty('5000', Descriptor);
  except
    on E: Exception do
      Raised := RaisedName(E);
  end;
  ClearAbort;
  Expect<string>(Raised).ToBe(TGocciaAbortError.ClassName);
  Expect<Integer>(GFreed).ToBe(1);
  Expect<Integer>(ArrayValue.Elements.Count).ToBe(0);
end;

{ Fills a refusing object's map until the next new key has to grow it past
  the size where the gate is consulted, then refuses that growth. }
procedure FillUntilTheNextAddGrows(const AObj: TRefusingObjectValue);
var
  I: Integer;
begin
  I := 0;
  // 4 KiB of entries puts every later growth through the gate.
  while (I < 256) or AObj.MapHasRoom do
  begin
    AObj.DefineProperty('k' + IntToStr(I), TGocciaPropertyDescriptorData.Create(
      TGocciaUndefinedLiteralValue.UndefinedValue,
      [pfEnumerable, pfConfigurable, pfWritable]));
    Inc(I);
  end;
end;

procedure TOwnershipTests.TestRefusedGrowthFreesOnce;
var
  Raised: string;
  Obj: TRefusingObjectValue;
  Descriptor: TCountedDescriptor;
begin
  Obj := TRefusingObjectValue.CreateRefusing;
  FillUntilTheNextAddGrows(Obj);
  Descriptor := Counted;
  GRefuseGrowth := True;
  Raised := 'nothing';
  try
    Obj.DefineProperty('refused', Descriptor);
  except
    on E: Exception do
      Raised := RaisedName(E);
  end;
  ClearAbort;
  Expect<string>(Raised).ToBe(TGocciaMemoryLimitError.ClassName);
  Expect<Integer>(GFreed).ToBe(1);
  Expect<Boolean>(Obj.HasOwnProperty('refused')).ToBe(False);
end;

procedure TOwnershipTests.TestRefusedGrowthOfALazyDescriptorFreesOnce;
var
  Raised: string;
  Obj: TRefusingObjectValue;
  Descriptor: TCountedLazyDescriptor;
begin
  Obj := TRefusingObjectValue.CreateRefusing;
  FillUntilTheNextAddGrows(Obj);
  Descriptor := TCountedLazyDescriptor.Create(MakeString,
    [pfConfigurable, pfWritable]);
  GRefuseGrowth := True;
  Raised := 'nothing';
  try
    Obj.DefineProperty('refused', Descriptor);
  except
    on E: Exception do
      Raised := RaisedName(E);
  end;
  ClearAbort;
  Expect<string>(Raised).ToBe(TGocciaMemoryLimitError.ClassName);
  Expect<Integer>(GFreed).ToBe(1);
  Expect<Boolean>(Obj.HasOwnProperty('refused')).ToBe(False);
end;

procedure TOwnershipTests.TestRefusedGrowthTryFreesOnce;
var
  Raised: string;
  Obj: TRefusingObjectValue;
  Descriptor: TCountedDescriptor;
begin
  Obj := TRefusingObjectValue.CreateRefusing;
  FillUntilTheNextAddGrows(Obj);
  Descriptor := Counted;
  GRefuseGrowth := True;
  Raised := 'nothing';
  try
    Obj.TryDefineProperty('refused', Descriptor);
  except
    on E: Exception do
      Raised := RaisedName(E);
  end;
  ClearAbort;
  Expect<string>(Raised).ToBe(TGocciaMemoryLimitError.ClassName);
  Expect<Integer>(GFreed).ToBe(1);
end;

{ A heap snapshot restore builds its new layout in a map of its own from
  descriptors the object or the reader still owns: that map leaves a refused
  one to them. }
procedure TOwnershipTests.TestRefusedGrowthKeepsAValueTheMapWasNotGiven;
var
  Obj: TRefusingObjectValue;
  Descriptor: TCountedDescriptor;
  Raised: string;
begin
  Obj := TRefusingObjectValue.CreateRefusing;
  Obj.KeepRefusedValues;
  FillUntilTheNextAddGrows(Obj);
  Descriptor := Counted;
  GRefuseGrowth := True;
  Raised := 'nothing';
  try
    Obj.StoreRaw('refused', Descriptor);
  except
    on E: Exception do
      Raised := RaisedName(E);
  end;
  GRefuseGrowth := False;
  Expect<string>(Raised).ToBe(TGocciaMemoryLimitError.ClassName);
  Expect<Integer>(GFreed).ToBe(0);
  Descriptor.Free;
  Expect<Integer>(GFreed).ToBe(1);
end;

procedure TOwnershipTests.TestNonExtensibleThrowFreesOnce;
var
  Raised: string;
  Obj: TGocciaObjectValue;
  Descriptor: TCountedDescriptor;
begin
  Obj := TGocciaObjectValue.Create;
  Obj.PreventExtensions;
  Descriptor := Counted;
  Raised := 'nothing';
  try
    Obj.DefineProperty('added', Descriptor);
  except
    on E: Exception do
      Raised := RaisedName(E);
  end;
  ClearAbort;
  Expect<string>(Raised).ToBe('throw');
  Expect<Integer>(GFreed).ToBe(1);
end;

procedure TOwnershipTests.TestNonExtensibleSymbolThrowFreesOnce;
var
  Raised: string;
  Obj: TGocciaObjectValue;
  Descriptor: TCountedDescriptor;
  Symbol: TGocciaSymbolValue;
begin
  Obj := TGocciaObjectValue.Create;
  Obj.PreventExtensions;
  Symbol := TGocciaSymbolValue.Create('added');
  Descriptor := Counted;
  Raised := 'nothing';
  try
    Obj.DefineSymbolProperty(Symbol, Descriptor);
  except
    on E: Exception do
      Raised := RaisedName(E);
  end;
  ClearAbort;
  Expect<string>(Raised).ToBe('throw');
  Expect<Integer>(GFreed).ToBe(1);
end;

procedure TOwnershipTests.TestArrayLengthRangeErrorFreesOnce;
var
  Raised: string;
  ArrayValue: TGocciaArrayValue;
  Descriptor: TCountedDescriptor;
begin
  ArrayValue := TGocciaArrayValue.Create;
  Descriptor := Counted(TGocciaNumberLiteralValue.Create(-1));
  Raised := 'nothing';
  try
    ArrayValue.DefineProperty(PROP_LENGTH, Descriptor);
  except
    on E: Exception do
      Raised := RaisedName(E);
  end;
  ClearAbort;
  Expect<string>(Raised).ToBe('throw');
  Expect<Integer>(GFreed).ToBe(1);
end;

procedure TOwnershipTests.TestClassPrototypeThrowFreesOnce;
var
  Raised: string;
  ClassValue: TGocciaClassValue;
  Descriptor: TCountedDescriptor;
begin
  ClassValue := TGocciaClassValue.Create('Probe', nil);
  Descriptor := TCountedDescriptor.Create(
    TGocciaUndefinedLiteralValue.UndefinedValue,
    [pfEnumerable, pfConfigurable, pfWritable]);
  Raised := 'nothing';
  try
    ClassValue.DefineProperty(PROP_PROTOTYPE, Descriptor);
  except
    on E: Exception do
      Raised := RaisedName(E);
  end;
  ClearAbort;
  Expect<string>(Raised).ToBe('throw');
  Expect<Integer>(GFreed).ToBe(1);
end;

procedure TOwnershipTests.TestRevokedProxyThrowFreesOnce;
var
  Raised: string;
  Proxy: TGocciaProxyValue;
  Descriptor: TCountedDescriptor;
begin
  Proxy := TGocciaProxyValue.Create(TGocciaObjectValue.Create,
    TGocciaObjectValue.Create);
  Proxy.Revoke;
  Descriptor := Counted;
  Raised := 'nothing';
  try
    Proxy.DefineProperty('x', Descriptor);
  except
    on E: Exception do
      Raised := RaisedName(E);
  end;
  ClearAbort;
  Expect<string>(Raised).ToBe('throw');
  Expect<Integer>(GFreed).ToBe(1);
end;

procedure TOwnershipTests.TestProxyTrapRefusalFreesOnce;
var
  Raised: string;
  Handler: TGocciaObjectValue;
  Proxy: TGocciaProxyValue;
  Descriptor: TCountedDescriptor;
begin
  Handler := TGocciaObjectValue.Create;
  Handler.AssignProperty(PROP_DEFINE_PROPERTY,
    TGocciaNativeFunctionValue.Create(RefusingTrap, PROP_DEFINE_PROPERTY, 3));
  Proxy := TGocciaProxyValue.Create(TGocciaObjectValue.Create, Handler);
  Descriptor := Counted;
  Raised := 'nothing';
  try
    Proxy.DefineProperty('x', Descriptor);
  except
    on E: Exception do
      Raised := RaisedName(E);
  end;
  ClearAbort;
  Expect<string>(Raised).ToBe('throw');
  Expect<Integer>(GFreed).ToBe(1);
end;

procedure TOwnershipTests.TestSuccessfulDefinitionsFreeOnce;
var
  Obj: TGocciaObjectValue;
  Fn: TGocciaNativeFunctionValue;
  ArrayValue: TGocciaArrayValue;
begin
  Obj := TGocciaObjectValue.Create;
  Obj.DefineProperty('a', Counted);
  Expect<Boolean>(Obj.TryDefineProperty('b', Counted)).ToBe(True);
  Obj.DefineSymbolProperty(TGocciaSymbolValue.Create('c'), Counted);
  Fn := TGocciaNativeFunctionValue.CreateWithoutPrototype(NoOp, 'probe', 1);
  Fn.DefineProperty(PROP_NAME, Counted(TGocciaStringLiteralValue.Create('n')));
  ArrayValue := TGocciaArrayValue.Create;
  ArrayValue.DefineProperty('0', TCountedDescriptor.Create(
    TGocciaNumberLiteralValue.Create(1),
    [pfEnumerable, pfConfigurable, pfWritable]));
  Expect<Integer>(GFreed).ToBe(5);
  Expect<Boolean>(Obj.HasOwnProperty('a')).ToBe(True);
  Expect<Boolean>(Obj.HasOwnProperty('b')).ToBe(True);
  Expect<Integer>(ArrayValue.Elements.Count).ToBe(1);
end;

function TOwnershipTests.ThrowingFactory: TGocciaValue;
begin
  ThrowTypeError('the factory refuses');
  Result := nil;
end;

{ "1.5" is no integer index, so deciding whether it is a canonical numeric
  string allocates its Number::toString. }
procedure TOwnershipTests.TestTypedArrayNumericKeyAbortFreesOnce;
var
  TypedArray: TGocciaTypedArrayValue;
  Descriptor: TCountedDescriptor;
  Raised: string;
begin
  TypedArray := TGocciaTypedArrayValue.Create(takFloat64, 4);
  Descriptor := Counted(TGocciaNumberLiteralValue.Create(2));
  RequestAbortNow;
  Raised := 'nothing';
  try
    TypedArray.DefineProperty('1.5', Descriptor);
  except
    on E: Exception do
      Raised := RaisedName(E);
  end;
  ClearAbort;
  Expect<string>(Raised).ToBe(TGocciaAbortError.ClassName);
  Expect<Integer>(GFreed).ToBe(1);
end;

{ The mapped arguments object copies a definition's descriptor before the
  inherited definition takes it; copying a lazy one runs its factory. }
procedure TOwnershipTests.TestMappedArgumentsLazyCopyThrowFreesOnce;
var
  Args: TGocciaArgumentsCollection;
  Arguments: TGocciaObjectValue;
  Raised: string;
begin
  // Built in the realm the shared prototypes were made in.
  SetCurrentRealm(FPreviousRealm);
  Args := TGocciaArgumentsCollection.Create;
  try
    Arguments := CreateMappedBytecodeArgumentsObject(Args, [],
      TGocciaUndefinedLiteralValue.UndefinedValue);
    Raised := 'nothing';
    try
      Arguments.DefineProperty('x', TCountedLazyDescriptor.Create(
        ThrowingFactory, [pfConfigurable, pfWritable]));
    except
      on E: Exception do
        Raised := RaisedName(E);
    end;
  finally
    Args.Free;
    SetCurrentRealm(FRealm);
  end;
  Expect<string>(Raised).ToBe('throw');
  Expect<Integer>(GFreed).ToBe(1);
end;

{ A heap snapshot restore builds the layout in a map that leaves refused
  values to their owners; once installed it is the object's map like any. }
procedure TOwnershipTests.TestRestoredMapFreesRefusedValuesAgain;
var
  Obj: TGocciaObjectValue;
  Prepared: TGocciaRestoredOwnProperties;
  GC: TGarbageCollector;
  SavedMaxBytes: Int64;
  Count, Attempts, I: Integer;
  Raised: string;
begin
  Obj := TGocciaObjectValue.Create;
  Obj.CreateDataPropertyOrThrow('a', TGocciaNumberLiteralValue.Create(1));
  Prepared := Obj.PrepareOwnPropertiesForRestore(['a'], [nil], [], []);
  try
    Obj.CommitOwnPropertiesForRestore(Prepared);
  finally
    Prepared.Free;
  end;
  Expect<Boolean>(TGocciaShapedPropertyMap(Obj.Properties).KeepsRefusedValue)
    .ToBe(False);
  // Enough entries that every later growth is asked about, then a budget
  // smaller than any growth: the restored map's next growth is refused.
  for I := 0 to 299 do
    Obj.DefineProperty('k' + IntToStr(I), TGocciaPropertyDescriptorData.Create(
      TGocciaUndefinedLiteralValue.UndefinedValue,
      [pfEnumerable, pfConfigurable, pfWritable]));
  Count := Obj.Properties.Count;
  GC := TGarbageCollector.Instance;
  SavedMaxBytes := GC.MaxBytes;
  GC.MaxBytes := 1;
  Attempts := 0;
  Raised := 'nothing';
  try
    try
      while Attempts < 10000 do
      begin
        Inc(Attempts);
        Obj.DefineProperty('r' + IntToStr(Attempts), Counted);
      end;
    except
      on E: Exception do
        Raised := RaisedName(E);
    end;
  finally
    GC.MaxBytes := SavedMaxBytes;
  end;
  Expect<string>(Raised).ToBe(TGocciaMemoryLimitError.ClassName);
  // Every definition freed its descriptor once, the refused one included.
  Expect<Integer>(GFreed).ToBe(Attempts);
  Expect<Integer>(Obj.Properties.Count).ToBe(Count + Attempts - 1);
  Expect<Boolean>(Obj.HasOwnProperty('a')).ToBe(True);
end;

{ A lazy descriptor subclass is stored whole, like the class itself, so its
  factory runs when a definition replaces it, under the definition's
  cleanup. }
procedure TOwnershipTests.TestLazySubclassFactoryThrowFreesOnce;
var
  Obj: TGocciaObjectValue;
  Raised: string;
begin
  Obj := TGocciaObjectValue.Create;
  Obj.DefineProperty('lazy', TCountedLazyDescriptor.Create(ThrowingFactory,
    [pfConfigurable, pfWritable]));
  Raised := 'nothing';
  try
    // A fieldless definition validates against the entry as it stands.
    Obj.DefineProperty('lazy', TCountedDescriptor.CreatePartial(
      TGocciaUndefinedLiteralValue.UndefinedValue, [], []));
  except
    on E: Exception do
      Raised := RaisedName(E);
  end;
  Expect<string>(Raised).ToBe('throw');
  Expect<Integer>(GFreed).ToBe(1);
end;

procedure TOwnershipTests.TestLazySubclassFactoryThrowTryFreesOnce;
var
  Obj: TGocciaObjectValue;
  Raised: string;
begin
  Obj := TGocciaObjectValue.Create;
  Obj.DefineProperty('lazy', TCountedLazyDescriptor.Create(ThrowingFactory,
    [pfConfigurable, pfWritable]));
  Raised := 'nothing';
  try
    Obj.TryDefineProperty('lazy', TCountedDescriptor.CreatePartial(
      TGocciaUndefinedLiteralValue.UndefinedValue, [], []));
  except
    on E: Exception do
      Raised := RaisedName(E);
  end;
  Expect<string>(Raised).ToBe('throw');
  Expect<Integer>(GFreed).ToBe(1);
end;

procedure TOwnershipTests.TestTryDefineSymbolPropertyRefusalFreesOnce;
var
  Obj: TGocciaObjectValue;
begin
  Obj := TGocciaObjectValue.Create;
  Obj.PreventExtensions;
  Expect<Boolean>(Obj.TryDefineSymbolProperty(TGocciaSymbolValue.Create('s'),
    Counted)).ToBe(False);
  Expect<Integer>(GFreed).ToBe(1);
end;

{ TAbortAtRun }

constructor TAbortAtRun.Create(const AAbortAtBlock: Int64;
  const AInScript: Boolean);
begin
  FAbortAtBlock := AAbortAtBlock;
  FInScript := AInScript;
  inherited Create(True);
end;

procedure TAbortAtRun.Execute;
var
  Source: TStringList;
  Runtime: TGocciaRuntime;
  Handle: IGocciaAbortHandle;
begin
  TrackThisThread := True;
  AbortAtBlock := 0;
  InitThreadRuntime;
  try
    Source := TStringList.Create;
    Runtime := nil;
    try
      try
        Source.Text := SCRIPT_DEFINITIONS;
        // As a host that arms before it builds the engine. The count starts
        // where the phase the trial stops in starts: the build, or the run.
        Handle := CaptureAbortHandle;
        AbortHandleToRequest := Pointer(Handle);
        BlocksOnThisThread := 0;
        if not FInScript then
          AbortAtBlock := FAbortAtBlock;
        Runtime := TGocciaRuntime.Create('main.js', Source,
          TGocciaCapabilities.None);
        Runtime.Engine.SourceType := stModule;
        if FInScript then
        begin
          BlocksOnThisThread := 0;
          AbortAtBlock := FAbortAtBlock;
          Runtime.Execute;
        end;
        Completed := True;
      except
        on E: TGocciaAbortError do
          Aborted := True;
        on E: Exception do
          Unexpected := True;
      end;
    finally
      AbortAtBlock := 0;
      BlocksSeen := BlocksOnThisThread;
      Runtime.Free;
      Source.Free;
    end;
    ClearAbort;
    AbortHandleToRequest := nil;
    Handle := nil;
  finally
    ShutdownThreadRuntime;
    TrackThisThread := False;
  end;
end;

{ TEmbedderAbortTests }

procedure TEmbedderAbortTests.SetupTests;
begin
  Test('a run that is never aborted leaves no block behind',
    TestControlRunLeavesNothing);
  Test('an abort anywhere in the engine build leaves no block behind',
    TestAbortAnywhereInTheBuildLeavesNothing);
  Test('an abort anywhere in a script leaves no block behind',
    TestAbortAnywhereInAScriptLeavesNothing);
end;

function RunOnce(const AAbortAtBlock: Int64; const AInScript: Boolean;
  out ARun: TAbortAtRun): Int64;
begin
  ARun := TAbortAtRun.Create(AAbortAtBlock, AInScript);
  StartTracking;
  ARun.Start;
  // WaitFor on the main thread checks for synchronized calls every 100 ms
  // until the thread has finished; once it has, it only joins it.
  while not ARun.Finished do
    Sleep(1);
  ARun.WaitFor;
  Result := StopTracking;
  // An exception that escaped the thread (its runtime's set-up or shut-down)
  // is kept alive until the thread is freed: a failure of the trial, not a
  // leak.
  if Assigned(ARun.FatalException) then
    ARun.Unexpected := True;
end;

procedure TEmbedderAbortTests.TestControlRunLeavesNothing;
var
  Run: TAbortAtRun;
  Leaked: Int64;
begin
  // Warms process-wide caches, then measures a clean run.
  RunOnce(0, False, Run);
  Run.Free;
  Leaked := RunOnce(0, False, Run);
  try
    Expect<Boolean>(Run.Completed).ToBe(True);
    Expect<Boolean>(Run.Unexpected).ToBe(False);
    Expect<Int64>(Leaked).ToBe(0);
  finally
    Run.Free;
  end;
end;

{ Aborts at every SWEEP_STRIDE-th allocation from the first to the last one
  a clean run makes in that phase, and returns that count. }
function TEmbedderAbortTests.Sweep(const AInScript: Boolean; out ATrials,
  AAborted, ALeakingTrials, AUnexpected: Integer;
  out ALeakedBlocks: Int64): Int64;
var
  Run: TAbortAtRun;
  Leaked: Int64;
  N: Int64;
begin
  RunOnce(0, AInScript, Run);
  try
    // The count of a clean, complete run, or no sweep at all.
    if Run.Completed and not Run.Unexpected then
      Result := Run.BlocksSeen
    else
      Result := 0;
  finally
    Run.Free;
  end;
  ATrials := 0;
  AAborted := 0;
  ALeakingTrials := 0;
  AUnexpected := 0;
  ALeakedBlocks := 0;
  N := 1;
  while N <= Result do
  begin
    Leaked := RunOnce(N, AInScript, Run);
    try
      Inc(ATrials);
      if Run.Aborted then
        Inc(AAborted);
      if Run.Unexpected then
        Inc(AUnexpected)
      else if Leaked <> 0 then
      begin
        Inc(ALeakingTrials);
        Inc(ALeakedBlocks, Abs(Leaked));
      end;
    finally
      Run.Free;
    end;
    Inc(N, SWEEP_STRIDE);
  end;
end;

procedure TEmbedderAbortTests.TestAbortAnywhereInTheBuildLeavesNothing;
var
  Blocks: Int64;
  Trials, Aborted, LeakingTrials, Unexpected: Integer;
  LeakedBlocks: Int64;
begin
  Blocks := Sweep(False, Trials, Aborted, LeakingTrials, Unexpected,
    LeakedBlocks);
  Expect<Integer>(Unexpected).ToBe(0);
  // The build alone: most requests landed before it finished.
  Expect<Boolean>(Blocks > 1000).ToBe(True);
  Expect<Boolean>(Aborted > Trials div 2).ToBe(True);
  Expect<Integer>(LeakingTrials).ToBe(0);
  Expect<Int64>(LeakedBlocks).ToBe(0);
end;

procedure TEmbedderAbortTests.TestAbortAnywhereInAScriptLeavesNothing;
var
  Blocks: Int64;
  Trials, Aborted, LeakingTrials, Unexpected: Integer;
  LeakedBlocks: Int64;
begin
  Blocks := Sweep(True, Trials, Aborted, LeakingTrials, Unexpected,
    LeakedBlocks);
  Expect<Integer>(Unexpected).ToBe(0);
  Expect<Boolean>(Blocks > 100).ToBe(True);
  Expect<Boolean>(Aborted > Trials div 2).ToBe(True);
  Expect<Integer>(LeakingTrials).ToBe(0);
  Expect<Int64>(LeakedBlocks).ToBe(0);
end;

begin
  InstallTrackingManager;
  EnsureSharedPrototypesInitialized;
  TestRunnerProgram.AddSuite(TOwnershipTests.Create('Definitions own their descriptor'));
  TestRunnerProgram.AddSuite(TEmbedderAbortTests.Create('A host abort during build or run'));
  RunGocciaTests;
  ExitCode := TestResultToExitCode;
end.
