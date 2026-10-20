program Goccia.HeapSnapshot.Hardening.Test;

{$I Goccia.inc}

{ ADR 0148: the heap snapshot reader's hardening. A snapshot is untrusted
  input to the reader, so real snapshots are taken apart field by field and
  mutated where a hostile one would be: every count, index and length of the
  header and its tables (not those inside payloads, which the payload bytes
  cover a byte at a time) set to its boundary values, every object entry
  read by every other codec, dropped, doubled and moved, every reference to
  an object or a built-in pointed at the other objects and built-ins, and
  the payload bytes set to the value tags (the sizes below say how many of
  each). Each mutant restores or is refused, never faults, and what restores
  is used: its functions called and its generators resumed, returned or
  thrown into.

  The walks over the heap are iterative (ADR 0147): each recursive shape a
  graph can take (closures, suspended generators, accessors, symbol-keyed
  properties and a chain mixing all of them) round-trips a hundred thousand
  links or more deep, as does a refusal at the end of a deep chain, the
  accounting check and a restore stopped by the memory ceiling half way.

  GC stress: with CollectBeforeEveryReservation, every reservation the reader
  makes (its tables, the frames it restores, a string's storage) collects
  first, as the last resort a reservation over the ceiling runs would, and
  restores under 401 ceilings spread evenly from nothing to what a restore
  needs stop at as many points along the way; a value the reader held only
  in a local would be freed there. Restoring the same mutants again and again leaves the heap
  where it was.

  Two sizes; the test names say which ran. By default (every pull request)
  each payload byte takes a quarter of the values and each reference a
  quarter of the targets, rotating, so every byte, value, reference and
  target is used, and the chains are 200,000 links long (50,000
  generators). With GOCCIA_HEAP_SNAPSHOT_HARDENING=full (the scheduled fuzz
  workflow) every byte takes every value and every reference every target,
  and the chains are 1,000,000 long (250,000 generators). The fuzz workflow
  also runs the default size under heaptrc. }

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes,
  Generics.Collections,
  Math,
  SysUtils,

  TestingPascalLibrary,

  Goccia.Arguments.Collection,
  Goccia.Engine,
  Goccia.ExecutionContext,
  Goccia.FFI.Types,
  Goccia.GarbageCollector,
  Goccia.HeapSnapshot,
  Goccia.HeapSnapshot.Codec,
  Goccia.HeapSnapshot.Exercise,
  Goccia.HeapSnapshot.RoundTrip,
  Goccia.HeapSnapshot.TestSupport,
  Goccia.InstructionLimit,
  Goccia.MemoryLimit,
  Goccia.NativeLimits,
  Goccia.Runtime,
  Goccia.RuntimeExtensions.CSV,
  Goccia.RuntimeExtensions.JSON5,
  Goccia.RuntimeExtensions.JSONL,
  Goccia.RuntimeExtensions.TOML,
  Goccia.RuntimeExtensions.TSV,
  Goccia.RuntimeExtensions.YAML,
  Goccia.Scope.BindingMap,
  Goccia.SourcePipeline,
  Goccia.TestSetup,
  Goccia.Timeout,
  Goccia.Values.Error,
  Goccia.Values.FFILibrary,
  Goccia.Values.FFIPointer,
  Goccia.Values.FFIType,
  Goccia.Values.NativeFunction,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

const
  // A graph with every value the core codecs write: shared references and a
  // cycle, holes and a grown length, BigInts, -0, a lone surrogate, local,
  // registered and well-known symbols, built-ins, integrity levels, a null
  // and a restored prototype, and an accessor.
  PLAIN_SEED = '(() => { const shared = { s: "shared" }; ' +
    'const local = Symbol("k"); const o = { list: [1, , "two", 3n, -0, ' +
    'shared, [shared]], [local]: Symbol.for("hardening"), ' +
    '[Symbol.iterator]: Array.prototype.values, m: Math, ' +
    'frozen: Object.freeze({ x: 1 }), sealed: Object.seal([1, 2]), ' +
    'bare: Object.create(null), proto: Object.create(shared), ' +
    'acc: Object.defineProperty({}, "g", { get: Object.prototype.valueOf, ' +
    'enumerable: true }), big: -(2n ** 70n), s: "\uD800x" }; ' +
    'o.self = o; o.list.length = 9; return o; })()';

  // Closures sharing a cell, suspended generators (one inside try/finally,
  // one with a binding in its temporal dead zone, one with rest arguments,
  // one not started), methods with their home object and a getter.
  VM_SEED = '(() => { let n = 0; const inc = () => { n += 1; return n; }; ' +
    'const log = []; const source = { *steps(start) { try { ' +
    'const a = yield start; const b = yield a * 2; return a + b + start; } ' +
    'finally { log.push(n); } }, *late() { const peek = () => x; ' +
    'yield peek; let x = 5; yield peek(); }, *wide(...args) { yield 0; ' +
    'yield args.length + args[args.length - 1]; }, get count() { ' +
    'return n; } }; const g = source.steps(1); g.next(); g.next(10); ' +
    'inc(); const late = source.late(); const peek = late.next().value; ' +
    'const wide = source.wide(1, 2, 3, 4); wide.next(); ' +
    'capture({ inc, g, log, late, peek, wide, source, ' +
    'fresh: source.steps(7) }); })();';

  // The VM seed used as it would have gone on: the generator finishes and
  // runs its finally block, the binding leaves its dead zone.
  VM_CHECK = '[restored.inc(), restored.g.next(5).value, restored.log.join(), ' +
    'restored.late.next().value, restored.peek(), restored.wide.next().value, ' +
    'restored.source.count, restored.fresh.next().value].join()';
  VM_CHECKED = '2,16,2,5,5,8,2,7';

  // Bounds on using a restored mutant: a mutant can make an array billions
  // long or a loop endless.
  EXERCISE_TIMEOUT_MS = 2000;
  EXERCISE_INSTRUCTIONS = 20000000;

  INTERESTING_U32: array[0..9] of Cardinal = (0, 1, 2, 3, $7F, $FFFF,
    $7FFFFFFF, $80000000, $FFFFFFFE, $FFFFFFFF);
  INTERESTING_U16: array[0..4] of Word = (0, 1, 2, $7FFF, $FFFF);
  // The value tags, one past the last, and the bytes a length's top byte
  // takes when a hostile writer means it.
  INTERESTING_BYTES: array[0..15] of Byte = (0, 1, 2, 3, 4, 5, 6, 7, 8, 9,
    10, 11, 12, $7F, $80, $FF);
  HSV_OBJECT_TAG = 9;
  HSV_BUILTIN_TAG = 10;

  // The sizes (see above).
  FULL_ENVIRONMENT = 'GOCCIA_HEAP_SNAPSHOT_HARDENING';
  FULL_LINKS = 1000000;
  DEFAULT_LINKS = 200000;
  DEFAULT_BYTE_SHARE = 4;

type
  // A field of a snapshot's structure: a count, an index or a length.
  TSnapshotField = record
    At: Integer;
    Size: Integer;
  end;

  // Where a snapshot's parts are (the format of Goccia.HeapSnapshot.Codec).
  TSnapshotLayout = record
    Fields: array of TSnapshotField;
    EntryAt: array of Integer;
    PayloadAt: array of Integer;
    PayloadLength: array of Integer;
    CodecCount: Integer;
    BuiltinCount: Integer;
    ObjectCountAt: Integer;
    RootsAt: Integer;
  end;

  // An object of a class no codec covers, ever: a snapshot of a graph that
  // reaches one is refused.
  TOpaqueValue = class(TGocciaObjectValue);

  // A deliberately faulty codec: native state omitted by the ordinary
  // descriptor oracle changes on restore. Only byte comparison catches it.
  TDriftingSnapshotValue = class(TGocciaObjectValue)
  public
    Payload: Cardinal;
  end;

  TDriftingSnapshotCodec = class(TGocciaSnapshotCodec)
  public
    function Tag: string; override;
    function SchemaVersion: Word; override;
    procedure Write(const AWriter: TGocciaHeapSnapshotWriter;
      const AObject: TGCManagedObject); override;
    function Allocate(const AReader: TGocciaHeapSnapshotReader):
      TGCManagedObject; override;
    procedure Fill(const AReader: TGocciaHeapSnapshotReader;
      const AObject: TGCManagedObject); override;
  end;

  // A stream that reports more bytes than one allocation can hold on this
  // target, without holding any: the reader must refuse it from its size.
  TOversizedStream = class(TStream)
  protected
    function GetSize: Int64; override;
  public
    function Read(var ABuffer; ACount: Longint): Longint; override;
    function Seek(const AOffset: Int64; AOrigin: TSeekOrigin): Int64; override;
  end;

  // One engine for many restores of mutants of one seed: it compiled the
  // seed's module when there is one, so functions resolve. Counts what
  // restored and what was refused; anything else propagates and fails the
  // test.
  TMutantFeeder = class
  private
    FEngine: TGocciaEngine;
    FLines: TStringList;
    FCapture: TCapture;
    FExerciser: THeapSnapshotExerciser;
    FFed: Integer;
    procedure Exercise(const ARoot: TGCManagedObject);
  public
    Restored: Integer;
    Refused: Integer;
    // Set while the reader runs: every reservation collects first
    // (CollectBeforeEveryReservation). Not while what restored is used:
    // that is the engine's code, not the reader's.
    Stressed: Boolean;
    // What ACheck returned for the last input that restored.
    LastCheck: string;
    // Exercise what restored before ACheck runs, rather than instead, bound
    // by the instruction limit alone: what ACheck counts then does not
    // depend on how fast the machine is (under Valgrind the wall-clock
    // timeout stopped the exercise before it called anything).
    ExerciseBeforeCheck: Boolean;
    constructor Create(const AModuleSource: string);
    destructor Destroy; override;
    // '' when ABytes restored and was used, else the refusal. Uses what
    // restored with the exercise script, or with ACheck when given. A
    // positive ACeiling is the collector's MaxBytes while the reader runs.
    function Feed(const ABytes: TBytes; const ACheck: string = '';
      const ACeiling: Int64 = 0): string;
    property Engine: TGocciaEngine read FEngine;
  end;

  THeapSnapshotHardeningTests = class(TTestSuite)
  private
    FFactoryCalls: Integer;
    FPlain: TBytes;
    FVM: TBytes;
    function CreateRoundTripDestination(const ASourceEngine: TGocciaEngine):
      TGocciaEngine;
    function PlainSeed: TBytes;
    function VMSeed: TBytes;
    function Opaque(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    procedure SweepFields(const ASeed: TBytes; const AModule: string);
    procedure SweepEntries(const ASeed: TBytes; const AModule: string);
    procedure SweepReferences(const ASeed: TBytes; const AModule: string);
    procedure SweepPayloadBytes(const ASeed: TBytes; const AModule: string);
    procedure SweepCeilings(const ASeed: TBytes; const AModule: string;
      const AStressed: Boolean);
    procedure TestSeedsRestoreAndRun;
    procedure TestRestoredGeneratorReturnsAndThrows;
    procedure TestExerciseReachesEveryFunction;
    procedure TestFieldsOfThePlainSeed;
    procedure TestFieldsOfTheVMSeed;
    procedure TestEntriesOfThePlainSeed;
    procedure TestEntriesOfTheVMSeed;
    procedure TestReferencesOfThePlainSeed;
    procedure TestReferencesOfTheVMSeed;
    procedure TestPayloadBytesOfThePlainSeed;
    procedure TestPayloadBytesOfTheVMSeed;
    procedure TestDeepClosureChain;
    procedure TestDeepGeneratorChain;
    procedure TestDeepAccessorChain;
    procedure TestDeepSymbolKeyedChain;
    procedure TestDeepMixedChain;
    procedure TestDeepRefusalStaysBounded;
    procedure TestDeepAccountingCheck;
    procedure TestDeepRestoreStoppedByTheCeiling;
    procedure TestEveryReservationCollects;
    procedure TestMutantsWhileEveryReservationCollects;
    procedure TestEveryCeilingRestoresOrRefuses;
    procedure TestEveryCeilingWhileEveryReservationCollects;
    procedure TestRepeatedMutantsLeaveTheHeap;
    procedure TestEngineBuiltWhileEveryReservationCollects;
    procedure TestGlobalEvalComputesNaN;
    procedure TestReservationPastOneAllocationRefuses;
    procedure TestInputPastOneAllocationRefuses;
    procedure TestLateBuiltinOrderIsStable;
    procedure TestObservedObjectsRoundTrip;
    procedure TestLateFFIPrototypesRoundTrip;
    procedure TestLazyNativeNamespacesRestore;
    procedure TestCanonicalDescriptionDetectsChanges;
    procedure TestObservedRestoreRefusalFails;
    procedure TestResnapshotDetectsPrivateStateDrift;
  public
    procedure SetupTests; override;
  end;

var
  // Set from FULL_ENVIRONMENT before the suite is built.
  // How long the deep chains are; the generator chain is a quarter of it.
  DeepLinks: Integer;
  // Each payload byte takes one in ByteShare of INTERESTING_BYTES, and each
  // reference one in ByteShare of the objects and built-ins.
  ByteShare: Integer;

{ Layout }

function U32At(const ABytes: TBytes; const AAt: Integer): Cardinal;
begin
  Move(ABytes[AAt], Result, 4);
  Result := LEtoN(Result);
end;

function U16At(const ABytes: TBytes; const AAt: Integer): Word;
begin
  Move(ABytes[AAt], Result, 2);
  Result := LEtoN(Result);
end;

procedure PutU32(var ABytes: TBytes; const AAt: Integer; const AValue: Cardinal);
var
  V: Cardinal;
begin
  V := NtoLE(AValue);
  Move(V, ABytes[AAt], 4);
end;

procedure PutU16(var ABytes: TBytes; const AAt: Integer; const AValue: Word);
var
  V: Word;
begin
  V := NtoLE(AValue);
  Move(V, ABytes[AAt], 2);
end;

function ParseLayout(const ABytes: TBytes): TSnapshotLayout;
var
  At, I, Count: Integer;

  procedure Field(const AAt, ASize: Integer);
  begin
    SetLength(Result.Fields, Length(Result.Fields) + 1);
    Result.Fields[High(Result.Fields)].At := AAt;
    Result.Fields[High(Result.Fields)].Size := ASize;
  end;

  // A section of ACount entries of AEach fields of AFieldSize bytes, with
  // AExtra bytes after each (a codec's schema version).
  function Section(const AEach, AFieldSize, AExtra: Integer): Integer;
  var
    J, K: Integer;
  begin
    Field(At, 4);
    Result := Integer(U32At(ABytes, At));
    Inc(At, 4);
    for J := 1 to Result do
    begin
      for K := 1 to AEach do
      begin
        Field(At, AFieldSize);
        Inc(At, AFieldSize);
      end;
      if AExtra > 0 then
      begin
        Field(At, AExtra);
        Inc(At, AExtra);
      end;
    end;
  end;

begin
  Result := Default(TSnapshotLayout);
  Field(4, 2);                                     // format version
  At := 6;
  Field(At, 4);                                    // build identity
  At := At + 4 + 2 * Integer(U32At(ABytes, At));
  Field(At, 4);                                    // strings
  Count := Integer(U32At(ABytes, At));
  Inc(At, 4);
  for I := 1 to Count do
  begin
    Field(At, 4);
    At := At + 4 + 2 * Integer(U32At(ABytes, At));
  end;
  Result.CodecCount := Section(1, 4, 2);           // codecs: tag, schema
  Result.BuiltinCount := Section(1, 4, 0);         // built-in names
  Section(2, 4, 0);                                // built-ins the run built
  Section(1, 4, 0);                                // modules
  Result.ObjectCountAt := At;
  Field(At, 4);
  Count := Integer(U32At(ABytes, At));
  Inc(At, 4);
  SetLength(Result.EntryAt, Count);
  SetLength(Result.PayloadAt, Count);
  SetLength(Result.PayloadLength, Count);
  for I := 0 to Count - 1 do
  begin
    Result.EntryAt[I] := At;
    Field(At, 2);                                  // codec index
    Field(At + 2, 4);                              // payload length
    Result.PayloadLength[I] := Integer(U32At(ABytes, At + 2));
    Result.PayloadAt[I] := At + 6;
    At := At + 6 + Result.PayloadLength[I];
  end;
  Result.RootsAt := At;
  Field(At, 4);                                    // roots
  Expect<Boolean>(At + 4 <= Length(ABytes)).ToBe(True);
end;

// ABytes without the bytes at [AAt, AAt + ACount), then AInsert at AAt.
function Splice(const ABytes: TBytes; const AAt, ACount: Integer;
  const AInsert: TBytes): TBytes;
begin
  SetLength(Result, Length(ABytes) - ACount + Length(AInsert));
  if AAt > 0 then
    Move(ABytes[0], Result[0], AAt);
  if Length(AInsert) > 0 then
    Move(AInsert[0], Result[AAt], Length(AInsert));
  if Length(ABytes) - AAt - ACount > 0 then
    Move(ABytes[AAt + ACount], Result[AAt + Length(AInsert)],
      Length(ABytes) - AAt - ACount);
end;

// What ChainWalk returns for a chain of ALength links whose values are
// their indexes.
function IndexChainWalked(const ALength: Integer): string;
begin
  Result := IntToStr(ALength) + ',' +
    IntToStr(Int64(ALength) * (ALength - 1) div 2);
end;

// A count of links as the test names write it: 1,000,000.
function Links(const ACount: Integer): string;
begin
  Result := FormatFloat('#,##0', ACount);
end;

{ TMutantFeeder }

constructor TMutantFeeder.Create(const AModuleSource: string);
begin
  inherited Create;
  FLines := TStringList.Create;
  FCapture := TCapture.Create;
  FLines.Text := AModuleSource;
  FEngine := TGocciaEngine.Create(TEST_PATH, FLines);
  if AModuleSource <> '' then
  begin
    FEngine.GlobalScope.DefineLexicalBinding('capture',
      TGocciaNativeFunctionValue.Create(FCapture.Capture, 'capture', 1),
      dtConst);
    FEngine.Execute;
  end;
  FExerciser := THeapSnapshotExerciser.Create(FEngine);
end;

destructor TMutantFeeder.Destroy;
begin
  if Assigned(FCapture.Value) then
    TGarbageCollector.Instance.RemoveTempRoot(FCapture.Value);
  FExerciser.Free;
  FEngine.Free;
  FCapture.Free;
  FLines.Free;
  inherited;
end;

function TMutantFeeder.Feed(const ABytes: TBytes; const ACheck: string;
  const ACeiling: Int64): string;
var
  Restore: TGocciaHeapSnapshotRestore;
  Stream: TBytesStream;
  GC: TGarbageCollector;
  Previous: Int64;
  Root: TGCManagedObject;
  Global: TGocciaObjectValue;
  Stage: TGocciaGlobalEvalStage;
begin
  Result := '';
  Restore := nil;
  GC := TGarbageCollector.Instance;
  Stream := TBytesStream.Create(ABytes);
  try
    Previous := GC.MaxBytes;
    if ACeiling > 0 then
      GC.MaxBytes := ACeiling;
    GC.CollectBeforeEveryReservation := Stressed;
    try
      try
        Restore := TGocciaHeapSnapshotRestore.Create(FEngine, Stream,
          TGocciaHeapSnapshotLimits.Defaults);
      except
        on E: EGocciaHeapSnapshotInvalid do
          Result := E.Message;
      end;
    finally
      GC.CollectBeforeEveryReservation := False;
      GC.MaxBytes := Previous;
    end;
    if Result <> '' then
    begin
      Inc(Refused);
      Exit;
    end;
    Inc(Restored);
    // A root can be any object a codec writes, a cell among them; what is
    // used is a value.
    Root := Restore.Roots[0];
    if (ACheck = '') or ExerciseBeforeCheck then
      Exercise(Root);
    if ACheck <> '' then
    begin
      Global := TGocciaObjectValue(FEngine.Realm.GlobalObject);
      Global.AssignProperty('restored', TGocciaValue(Root));
      LastCheck := FEngine.EvaluateGlobalEval(ACheck, TEST_PATH,
        Stage).ToStringLiteral.Value;
      Global.DeleteProperty('restored');
    end;
  finally
    Restore.Free;
    Stream.Free;
    Inc(FFed);
    // What the restores left behind is garbage; collect it now and then.
    if FFed mod 128 = 0 then
      GC.Collect;
  end;
end;

procedure TMutantFeeder.Exercise(const ARoot: TGCManagedObject);
begin
  if not (ARoot is TGocciaValue) then
    Exit;
  if not ExerciseBeforeCheck then
    StartExecutionTimeout(EXERCISE_TIMEOUT_MS);
  StartInstructionLimit(EXERCISE_INSTRUCTIONS);
  try
    try
      // Each feed uses generators another way (resume, return first,
      // throw first), in turn.
      FExerciser.Exercise(TGocciaValue(ARoot), FFed);
    except
      // The run's own limits are outcomes a run can have, and so is a
      // rejection the run left unhandled (a built-in such as
      // Array.fromAsync called with the wrong arguments).
      on E: TGocciaThrowValue do;
      on E: TGocciaTimeoutError do;
      on E: TGocciaInstructionLimitError do;
      on E: TGocciaMemoryLimitError do;
    end;
  finally
    ClearInstructionLimit;
    if not ExerciseBeforeCheck then
      ClearExecutionTimeout;
  end;
end;

{ THeapSnapshotHardeningTests }

procedure THeapSnapshotHardeningTests.SetupTests;
var
  Targets, Shares: string;
begin
  Test('Both seeds restore, and using them runs', TestSeedsRestoreAndRun);
  Test('A restored suspended generator returns and throws through its ' +
    'finally block', TestRestoredGeneratorReturnsAndThrows);
  Test('The exercise calls every function of a restored chain of 64, and ' +
    'past thousands of primitives',
    TestExerciseReachesEveryFunction);
  Test('Every header and table count, index and length of the plain seed ' +
    'at its boundaries',
    TestFieldsOfThePlainSeed);
  Test('Every header and table count, index and length of the VM seed at ' +
    'its boundaries',
    TestFieldsOfTheVMSeed);
  Test('Every entry of the plain seed retyped, dropped, doubled and moved',
    TestEntriesOfThePlainSeed);
  Test('Every entry of the VM seed retyped, dropped, doubled and moved',
    TestEntriesOfTheVMSeed);
  if ByteShare = 1 then
  begin
    Targets := 'every object and built-in';
    Shares := 'every value tag';
  end
  else
  begin
    Targets := Format('a rotating one in %d of the objects and built-ins',
      [ByteShare]);
    Shares := Format('a rotating one in %d of the value tags', [ByteShare]);
  end;
  Test('Every reference of the plain seed pointed at ' + Targets,
    TestReferencesOfThePlainSeed);
  Test('Every reference of the VM seed pointed at ' + Targets,
    TestReferencesOfTheVMSeed);
  Test('Every payload byte of the plain seed set to ' + Shares,
    TestPayloadBytesOfThePlainSeed);
  Test('Every payload byte of the VM seed set to ' + Shares,
    TestPayloadBytesOfTheVMSeed);
  Test('A chain of ' + Links(DeepLinks) +
    ' closures, each closing over the last', TestDeepClosureChain);
  Test('A chain of ' + Links(DeepLinks div 4) +
    ' suspended generators, each holding the last', TestDeepGeneratorChain);
  Test('A chain of ' + Links(DeepLinks) +
    ' getters, each returning the last object', TestDeepAccessorChain);
  Test('A chain of ' + Links(DeepLinks) +
    ' objects linked by a symbol-keyed property', TestDeepSymbolKeyedChain);
  Test('A chain of ' + Links(DeepLinks) +
    ' links mixing every kind of link', TestDeepMixedChain);
  Test('A refusal at the end of a chain of ' + Links(DeepLinks) +
    ' stays short', TestDeepRefusalStaysBounded);
  Test('The accounting check over a chain of ' + Links(DeepLinks),
    TestDeepAccountingCheck);
  Test('A deep restore the ceiling stops half way leaves collectable garbage',
    TestDeepRestoreStoppedByTheCeiling);
  Test('Round trips while every reservation collects first',
    TestEveryReservationCollects);
  Test('Mutants while every reservation collects first',
    TestMutantsWhileEveryReservationCollects);
  Test('Restores under 401 ceilings up to what one needs restore or refuse',
    TestEveryCeilingRestoresOrRefuses);
  Test('401 ceilings while every reservation collects first',
    TestEveryCeilingWhileEveryReservationCollects);
  Test('Restoring the same mutants again leaves the heap where it was',
    TestRepeatedMutantsLeaveTheHeap);
  Test('An engine built while every reservation collects first',
    TestEngineBuiltWhileEveryReservationCollects);
  Test('A reservation past what one allocation can hold is refused before ' +
    'it is charged', TestReservationPastOneAllocationRefuses);
  Test('An input past what one allocation can hold is refused from its ' +
    'size', TestInputPastOneAllocationRefuses);
  Test('Late builtin manifests have stable order across materialization orders',
    TestLateBuiltinOrderIsStable);
  Test('Observed objects round-trip at idle, retain identity and count refusals',
    TestObservedObjectsRoundTrip);
  Test('late FFI prototypes restore without native state codecs',
    TestLateFFIPrototypesRoundTrip);
  Test('Lazy native namespaces restore in a fresh runtime without imports',
    TestLazyNativeNamespacesRestore);
  Test('The native description detects graph changes without calling getters',
    TestCanonicalDescriptionDetectsChanges);
  Test('A refusal after writing is a round-trip failure, not a skipped object',
    TestObservedRestoreRefusalFails);
  Test('Re-snapshot bytes detect private codec state lost at restore',
    TestResnapshotDetectsPrivateStateDrift);
  Test('EvaluateGlobalEval called directly computes NaN',
    TestGlobalEvalComputesNaN);
end;

function THeapSnapshotHardeningTests.PlainSeed: TBytes;
var
  Refusal: string;
begin
  if Length(FPlain) = 0 then
  begin
    FPlain := Snapshot(PLAIN_SEED, Refusal);
    Expect<string>(Refusal).ToBe('');
  end;
  Result := FPlain;
end;

function THeapSnapshotHardeningTests.VMSeed: TBytes;
var
  Refusal: string;
begin
  if Length(FVM) = 0 then
  begin
    FVM := SnapshotModule(VM_SEED, Refusal);
    Expect<string>(Refusal).ToBe('');
  end;
  Result := FVM;
end;

function THeapSnapshotHardeningTests.Opaque(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TOpaqueValue.Create;
end;

procedure THeapSnapshotHardeningTests.TestSeedsRestoreAndRun;
var
  Feeder: TMutantFeeder;
begin
  Expect<string>(RestoreAndCheck(PlainSeed,
    '[restored.self === restored, restored.list.length, ' +
    'restored.list[5] === Object.getPrototypeOf(restored.proto), ' +
    'restored.m === Math, ' +
    'Object.isFrozen(restored.frozen), restored.big, ' +
    'restored.s.charCodeAt(0)].join()'))
    .ToBe('true,9,true,true,true,-1180591620717411303424,55296');
  Expect<string>(RoundTripModuleBytes(VM_SEED, VMSeed, VM_CHECK))
    .ToBe(VM_CHECKED);
  Feeder := TMutantFeeder.Create('');
  try
    Expect<string>(Feeder.Feed(PlainSeed)).ToBe('');
  finally
    Feeder.Free;
  end;
  Feeder := TMutantFeeder.Create(VM_SEED);
  try
    Expect<string>(Feeder.Feed(VMSeed)).ToBe('');
  finally
    Feeder.Free;
  end;
end;

procedure THeapSnapshotHardeningTests.TestRestoredGeneratorReturnsAndThrows;
begin
  // Each from a restore of its own, while the generator is still suspended
  // inside its try block: returning or throwing into it runs its finally
  // block (which logs n) and closes it; the one not started closes without.
  Expect<string>(RoundTripModuleBytes(VM_SEED, VMSeed,
    '[JSON.stringify(restored.g.return(7)), restored.log.join(), ' +
    'JSON.stringify(restored.g.next()), ' +
    'JSON.stringify(restored.fresh.return(3)), restored.log.join()]' +
    '.join("|")'))
    .ToBe('{"value":7,"done":true}|1|{"done":true}|' +
      '{"value":3,"done":true}|1');
  Expect<string>(RoundTripModuleBytes(VM_SEED, VMSeed,
    '(() => { let m = ""; try { restored.g.throw(new Error("boom")); } ' +
    'catch (e) { m = e.message; } return [m, restored.log.join(), ' +
    'JSON.stringify(restored.g.next())].join("|"); })()'))
    .ToBe('boom|1|{"done":true}');
end;

procedure THeapSnapshotHardeningTests.TestExerciseReachesEveryFunction;
const
  // Each closure counts its calls; the walk meets the built-ins of the
  // root's prototype chain before the chain's far end.
  CHAIN = '(() => { let calls = 0; const head = Array.from({ length: 64 }, ' +
    '(_, i) => i).reduce((prev, i) => { const f = () => { calls += 1; ' +
    'return prev; }; f.i = i; return f; }, null); ' +
    'capture({ head, calls: () => calls }); })();';
  PADDED = '(() => { let calls = 0; const shared = {}; ' +
    'const second = () => { calls += 1; return 0; }; ' +
    'const head = () => { calls += 1; return second; }; ' +
    'capture({ padding: Array.from({ length: 4000 }, () => 0), ' +
    'repeats: Array.from({ length: 4000 }, () => shared), head, ' +
    'calls: () => calls }); })();';
var
  Feeder: TMutantFeeder;
  Bytes: TBytes;
  Refusal: string;
begin
  Bytes := SnapshotModule(CHAIN, Refusal);
  Expect<string>(Refusal).ToBe('');
  Feeder := TMutantFeeder.Create(CHAIN);
  try
    Feeder.ExerciseBeforeCheck := True;
    Expect<string>(Feeder.Feed(Bytes, 'String(restored.calls())')).ToBe('');
    Expect<string>(Feeder.LastCheck).ToBe('64');
  finally
    Feeder.Free;
  end;
  // Thousands of primitives and repeats before a function do not crowd out
  // the function its call returns.
  Bytes := SnapshotModule(PADDED, Refusal);
  Expect<string>(Refusal).ToBe('');
  Feeder := TMutantFeeder.Create(PADDED);
  try
    Feeder.ExerciseBeforeCheck := True;
    Expect<string>(Feeder.Feed(Bytes, 'String(restored.calls())')).ToBe('');
    Expect<string>(Feeder.LastCheck).ToBe('2');
  finally
    Feeder.Free;
  end;
end;

{$PUSH}{$Q-}{$R-}
procedure THeapSnapshotHardeningTests.SweepFields(const ASeed: TBytes;
  const AModule: string);
var
  Layout: TSnapshotLayout;
  Feeder: TMutantFeeder;
  Mutant: TBytes;
  I, J: Integer;
  Field: TSnapshotField;
  Original: Cardinal;
begin
  Layout := ParseLayout(ASeed);
  Feeder := TMutantFeeder.Create(AModule);
  try
    for I := 0 to High(Layout.Fields) do
    begin
      Field := Layout.Fields[I];
      if Field.Size = 4 then
      begin
        Original := U32At(ASeed, Field.At);
        for J := 0 to High(INTERESTING_U32) + 2 do
        begin
          Mutant := Copy(ASeed);
          if J <= High(INTERESTING_U32) then
            PutU32(Mutant, Field.At, INTERESTING_U32[J])
          else if J = High(INTERESTING_U32) + 1 then
            PutU32(Mutant, Field.At, Original + 1)
          else
            PutU32(Mutant, Field.At, Original - 1);
          Feeder.Feed(Mutant);
        end;
      end
      else
      begin
        Original := U16At(ASeed, Field.At);
        for J := 0 to High(INTERESTING_U16) + 2 do
        begin
          Mutant := Copy(ASeed);
          if J <= High(INTERESTING_U16) then
            PutU16(Mutant, Field.At, INTERESTING_U16[J])
          else if J = High(INTERESTING_U16) + 1 then
            PutU16(Mutant, Field.At, Word(Original + 1))
          else
            PutU16(Mutant, Field.At, Word(Original - 1));
          Feeder.Feed(Mutant);
        end;
      end;
    end;
    Expect<Boolean>(Feeder.Refused > Length(Layout.Fields)).ToBe(True);
  finally
    Feeder.Free;
  end;
end;

procedure THeapSnapshotHardeningTests.SweepEntries(const ASeed: TBytes;
  const AModule: string);
var
  Layout: TSnapshotLayout;
  Feeder: TMutantFeeder;
  Mutant, Entry, Next: TBytes;
  I, C, Count, EntryLength, NextLength: Integer;
begin
  Layout := ParseLayout(ASeed);
  Count := Length(Layout.EntryAt);
  Feeder := TMutantFeeder.Create(AModule);
  try
    for I := 0 to Count - 1 do
    begin
      EntryLength := 6 + Layout.PayloadLength[I];
      Entry := Copy(ASeed, Layout.EntryAt[I], EntryLength);
      // Read by every other codec the snapshot names.
      for C := 0 to Layout.CodecCount - 1 do
        if C <> U16At(ASeed, Layout.EntryAt[I]) then
        begin
          Mutant := Copy(ASeed);
          PutU16(Mutant, Layout.EntryAt[I], C);
          Feeder.Feed(Mutant);
        end;
      // Dropped: the objects after it move down one, and the last
      // reference to the end of the table goes out of range.
      Mutant := Splice(ASeed, Layout.EntryAt[I], EntryLength, nil);
      PutU32(Mutant, Layout.ObjectCountAt, Count - 1);
      Feeder.Feed(Mutant);
      // Doubled.
      Mutant := Splice(ASeed, Layout.EntryAt[I], 0, Entry);
      PutU32(Mutant, Layout.ObjectCountAt, Count + 1);
      Feeder.Feed(Mutant);
      // Swapped with the next: each object's references now name the other.
      if I < Count - 1 then
      begin
        NextLength := 6 + Layout.PayloadLength[I + 1];
        Next := Copy(ASeed, Layout.EntryAt[I + 1], NextLength);
        Mutant := Splice(ASeed, Layout.EntryAt[I], EntryLength + NextLength,
          Splice(Next, NextLength, 0, Entry));
        Feeder.Feed(Mutant);
      end;
      // Its payload one byte short, and one byte long into the next entry.
      Mutant := Copy(ASeed);
      PutU32(Mutant, Layout.EntryAt[I] + 2, Layout.PayloadLength[I] - 1);
      Feeder.Feed(Mutant);
      Mutant := Copy(ASeed);
      PutU32(Mutant, Layout.EntryAt[I] + 2, Layout.PayloadLength[I] + 1);
      Feeder.Feed(Mutant);
    end;
    Expect<Boolean>(Feeder.Refused > 0).ToBe(True);
  finally
    Feeder.Free;
  end;
end;

procedure THeapSnapshotHardeningTests.SweepReferences(const ASeed: TBytes;
  const AModule: string);
var
  Layout: TSnapshotLayout;
  Feeder: TMutantFeeder;
  Mutant: TBytes;
  Sites: TList<Integer>;
  I, P, S, Target, Count, Last: Integer;

  function IsReference(const AAt: Integer): Boolean;
  begin
    Result := ((ASeed[AAt] = HSV_OBJECT_TAG) and
      (U32At(ASeed, AAt + 1) < Cardinal(Count))) or
      ((ASeed[AAt] = HSV_BUILTIN_TAG) and
      (U32At(ASeed, AAt + 1) < Cardinal(Layout.BuiltinCount)));
  end;

begin
  Layout := ParseLayout(ASeed);
  Count := Length(Layout.EntryAt);
  Feeder := TMutantFeeder.Create(AModule);
  Sites := TList<Integer>.Create;
  try
    // A reference: its tag and an index into the object table or the
    // built-in names. A payload byte that only looks like one is mutated
    // too, which is as good a mutant.
    for I := 0 to Count - 1 do
    begin
      Last := Layout.PayloadAt[I] + Layout.PayloadLength[I] - 5;
      for P := Layout.PayloadAt[I] to Last do
        if IsReference(P) then
          Sites.Add(P);
    end;
    for P := Layout.RootsAt + 4 to Length(ASeed) - 5 do
      if IsReference(P) then
        Sites.Add(P);
    Expect<Boolean>(Sites.Count >= ByteShare).ToBe(True);
    // Each site is pointed at the objects and built-ins whose index is the
    // site's own modulo ByteShare: all of them in the full run.
    for S := 0 to Sites.Count - 1 do
    begin
      P := Sites[S];
      for Target := 0 to Count - 1 do
        if (Target mod ByteShare = S mod ByteShare) and
           not ((ASeed[P] = HSV_OBJECT_TAG) and
                (Cardinal(Target) = U32At(ASeed, P + 1))) then
        begin
          Mutant := Copy(ASeed);
          Mutant[P] := HSV_OBJECT_TAG;
          PutU32(Mutant, P + 1, Target);
          Feeder.Feed(Mutant);
        end;
      for Target := 0 to Layout.BuiltinCount - 1 do
        if (Target mod ByteShare = S mod ByteShare) and
           not ((ASeed[P] = HSV_BUILTIN_TAG) and
                (Cardinal(Target) = U32At(ASeed, P + 1))) then
        begin
          Mutant := Copy(ASeed);
          Mutant[P] := HSV_BUILTIN_TAG;
          PutU32(Mutant, P + 1, Target);
          Feeder.Feed(Mutant);
        end;
    end;
    Expect<Boolean>(Feeder.Restored > 0).ToBe(True);
  finally
    Sites.Free;
    Feeder.Free;
  end;
end;

// Sets each payload byte to the values of INTERESTING_BYTES whose index is
// the byte's own position modulo ByteShare: all of them in the full run,
// else a different share at each of ByteShare neighbouring bytes.
procedure THeapSnapshotHardeningTests.SweepPayloadBytes(const ASeed: TBytes;
  const AModule: string);
var
  Layout: TSnapshotLayout;
  Feeder: TMutantFeeder;
  Mutant: TBytes;
  I, P, V, Fed: Integer;
  Used: array[0..High(INTERESTING_BYTES)] of Boolean;
begin
  Layout := ParseLayout(ASeed);
  Feeder := TMutantFeeder.Create(AModule);
  Fed := 0;
  FillChar(Used, SizeOf(Used), 0);
  try
    for I := 0 to High(Layout.EntryAt) do
      for P := Layout.PayloadAt[I] to
          Layout.PayloadAt[I] + Layout.PayloadLength[I] - 1 do
        for V := 0 to High(INTERESTING_BYTES) do
          if (V mod ByteShare = P mod ByteShare) and
             (ASeed[P] <> INTERESTING_BYTES[V]) then
          begin
            Mutant := Copy(ASeed);
            Mutant[P] := INTERESTING_BYTES[V];
            Feeder.Feed(Mutant);
            Used[V] := True;
            Inc(Fed);
          end;
    // Every value was used, at a share of the bytes at least.
    for V := 0 to High(INTERESTING_BYTES) do
      Expect<Boolean>(Used[V]).ToBe(True);
    Expect<Integer>(Feeder.Restored + Feeder.Refused).ToBe(Fed);
    Expect<Boolean>(Feeder.Refused > 0).ToBe(True);
    Expect<Boolean>(Feeder.Restored > 0).ToBe(True);
  finally
    Feeder.Free;
  end;
end;
{$POP}

procedure THeapSnapshotHardeningTests.TestFieldsOfThePlainSeed;
begin
  SweepFields(PlainSeed, '');
end;

procedure THeapSnapshotHardeningTests.TestFieldsOfTheVMSeed;
begin
  SweepFields(VMSeed, VM_SEED);
end;

procedure THeapSnapshotHardeningTests.TestEntriesOfThePlainSeed;
begin
  SweepEntries(PlainSeed, '');
end;

procedure THeapSnapshotHardeningTests.TestEntriesOfTheVMSeed;
begin
  SweepEntries(VMSeed, VM_SEED);
end;

procedure THeapSnapshotHardeningTests.TestReferencesOfThePlainSeed;
begin
  SweepReferences(PlainSeed, '');
end;

procedure THeapSnapshotHardeningTests.TestReferencesOfTheVMSeed;
begin
  SweepReferences(VMSeed, VM_SEED);
end;

procedure THeapSnapshotHardeningTests.TestPayloadBytesOfThePlainSeed;
begin
  SweepPayloadBytes(PlainSeed, '');
end;

procedure THeapSnapshotHardeningTests.TestPayloadBytesOfTheVMSeed;
begin
  SweepPayloadBytes(VMSeed, VM_SEED);
end;

// A walk of a chain in the restoring engine, from `restored` through
// `step(p)` until it returns null: its length and the sum of `value(p)`.
// An iterator, so neither engine recurses once per link.
function ChainWalk(const AStep, AValue: string): string;
begin
  Result := '(() => { const step = ' + AStep + '; const value = ' + AValue +
    '; const chain = { [Symbol.iterator]() { let p = restored.head; ' +
    'return { next() { if (p === null) return { done: true, value: 0 }; ' +
    'const v = value(p); p = step(p); return { done: false, value: v }; } }; ' +
    '} }; let n = 0; let sum = 0; for (const v of chain) { n++; sum += v; } ' +
    'return n + "," + sum; })()';
end;

// A module that builds a chain ALength long with ALink(prev, i) from null
// and captures it as `head`, with AExtra beside it.
function ChainModule(const ALength: Integer; const ALink: string;
  const APrelude: string = ''; const AExtra: string = ''): string;
begin
  Result := '(() => { ' + APrelude + ' const head = Array.from({ length: ' +
    IntToStr(ALength) + ' }, (_, i) => i).reduce(' + ALink + ', null); ' +
    'capture({ head' + AExtra + ' }); })();';
end;

procedure THeapSnapshotHardeningTests.TestDeepClosureChain;
begin
  // Function -> cell -> function ... : each closure keeps the last in the
  // cell of its callback's parameter.
  Expect<string>(RoundTripModule(
    ChainModule(DeepLinks, '(prev, i) => { const f = () => prev; f.i = i; ' +
      'return f; }'),
    ChainWalk('(p) => p()', '(p) => p.i')))
    .ToBe(IndexChainWalked(DeepLinks));
end;

procedure THeapSnapshotHardeningTests.TestDeepGeneratorChain;
begin
  // Generator -> its frame's registers -> generator ... : each suspended
  // inside its body with the last in a register.
  Expect<string>(RoundTripModule(
    ChainModule(DeepLinks div 4, '(prev, i) => { ' +
      'const g = source.hold(prev, i); g.next(); return g; }',
      'const source = { *hold(inner, i) { const keep = inner; yield i; ' +
      'yield keep; } };'),
    ChainWalk('(p) => p.next().value', '(p) => 1')))
    .ToBe(IntToStr(DeepLinks div 4) + ',' + IntToStr(DeepLinks div 4));
end;

procedure THeapSnapshotHardeningTests.TestDeepAccessorChain;
begin
  // Object -> getter -> cell -> object ... .
  Expect<string>(RoundTripModule(
    ChainModule(DeepLinks, '(prev, i) => Object.defineProperty({ i }, ' +
      '"next", { get: () => prev, enumerable: true })'),
    ChainWalk('(p) => p.next', '(p) => p.i')))
    .ToBe(IndexChainWalked(DeepLinks));
end;

procedure THeapSnapshotHardeningTests.TestDeepSymbolKeyedChain;
begin
  Expect<string>(RoundTripModule(
    ChainModule(DeepLinks, '(prev, i) => ({ [key]: prev, i })',
      'const key = Symbol("next");', ', key'),
    ChainWalk('(p) => p[restored.key]', '(p) => p.i')))
    .ToBe(IndexChainWalked(DeepLinks));
end;

procedure THeapSnapshotHardeningTests.TestDeepMixedChain;
const
  // Link i holds the last through: an object literal, an array, a plain
  // object without a prototype, a closure, a frozen object, a symbol key, a
  // getter, a suspended generator and a sealed array, in turn.
  LINK = '(prev, i) => { switch (i % 9) { ' +
    'case 0: return { i, next: prev }; ' +
    'case 1: return [i, prev]; ' +
    'case 2: { const o = Object.create(null); o.i = i; o.next = prev; ' +
    'return o; } ' +
    'case 3: { const f = () => prev; f.i = i; return f; } ' +
    'case 4: return Object.freeze({ i, next: prev }); ' +
    'case 5: return { i, [key]: prev }; ' +
    'case 6: return Object.defineProperty({ i }, "next", { get: () => prev }); ' +
    'case 7: { const g = source.hold(prev, i); g.next(); g.i = i; return g; } ' +
    'default: return Object.seal([i, prev]); } }';
  STEP = '(p) => { const k = (Array.isArray(p) ? p[0] : p.i) % 9; ' +
    'if (k === 1 || k === 8) return p[1]; if (k === 3) return p(); ' +
    'if (k === 5) return p[restored.key]; if (k === 7) return p.next().value; ' +
    'return p.next; }';
  VALUE = '(p) => Array.isArray(p) ? p[0] : p.i';
begin
  Expect<string>(RoundTripModule(
    ChainModule(DeepLinks, LINK, 'const key = Symbol("next"); ' +
      'const source = { *hold(inner, i) { const keep = inner; yield i; ' +
      'yield keep; } };', ', key'),
    ChainWalk(STEP, VALUE)))
    .ToBe(IndexChainWalked(DeepLinks));
end;

procedure THeapSnapshotHardeningTests.TestDeepRefusalStaysBounded;
var
  Engine: TGocciaEngine;
  Session: TGocciaHeapSnapshotSession;
  Lines: TStringList;
  Stream: TBytesStream;
  Stage: TGocciaGlobalEvalStage;
  Value: TGocciaValue;
  Refusal: string;
begin
  Lines := TStringList.Create;
  Stream := TBytesStream.Create;
  Engine := nil;
  Session := nil;
  try
    Engine := TGocciaEngine.Create(TEST_PATH, Lines);
    Engine.GlobalScope.DefineLexicalBinding('opaque',
      TGocciaNativeFunctionValue.Create(Opaque, 'opaque', 0), dtConst);
    Session := TGocciaHeapSnapshotSession.Create(Engine);
    Session.CheckAccounting := False;
    // The object no codec covers is the last one the writer reaches.
    Value := Engine.EvaluateGlobalEval('Array.from({ length: ' +
      IntToStr(DeepLinks) + ' }, (_, i) => i).reduce((next, i) => ({ i, next }), { end: opaque() })',
      TEST_PATH, Stage);
    TGarbageCollector.Instance.AddTempRoot(Value);
    try
      Refusal := '';
      try
        Session.Snapshot([Value], Stream);
      except
        on E: EGocciaHeapSnapshotRefused do
          Refusal := E.Message;
      end;
    finally
      TGarbageCollector.Instance.RemoveTempRoot(Value);
    end;
    Expect<Boolean>(Pos('no snapshot codec for TOpaqueValue', Refusal) > 0)
      .ToBe(True);
    // The chain of objects that reached it is named, a few links of it.
    Expect<Boolean>(Pos(' <- ', Refusal) > 0).ToBe(True);
    Expect<Boolean>(Length(Refusal) < 1024).ToBe(True);
    Expect<Integer>(Integer(Stream.Size)).ToBe(0);
  finally
    Session.Free;
    Engine.Free;
    Stream.Free;
    Lines.Free;
  end;
end;

procedure THeapSnapshotHardeningTests.TestDeepAccountingCheck;
var
  Refusal: string;
  Bytes: TBytes;
begin
  // Every one of the objects is written, so nothing is missed.
  Bytes := Snapshot('Array.from({ length: ' + IntToStr(DeepLinks) +
    ' }, (_, i) => i).reduce((next, i) => ({ i, next }), null)', Refusal,
    True);
  Expect<string>(Refusal).ToBe('');
  Expect<string>(RestoreAndCheck(Bytes, 'String(restored.i)'))
    .ToBe(IntToStr(DeepLinks - 1));
end;

procedure THeapSnapshotHardeningTests.TestDeepRestoreStoppedByTheCeiling;
var
  Refusal: string;
  Bytes: TBytes;
  Engine: TGocciaEngine;
  Lines: TStringList;
  GC: TGarbageCollector;
  Before, Previous, Used: Int64;
  Restore: TGocciaHeapSnapshotRestore;
  Stream: TBytesStream;
begin
  Bytes := Snapshot('Array.from({ length: ' + IntToStr(DeepLinks) +
    ' }, (_, i) => i).reduce((next, i) => ({ i, next }), null)', Refusal);
  Expect<string>(Refusal).ToBe('');
  GC := TGarbageCollector.Instance;
  Lines := TStringList.Create;
  Engine := TGocciaEngine.Create(TEST_PATH, Lines);
  try
    // What a whole restore takes, then half of it as the ceiling.
    GC.Collect;
    Before := GC.BytesAllocated;
    GC.ResetPeakBytesAllocated;
    Stream := TBytesStream.Create(Bytes);
    try
      Restore := TGocciaHeapSnapshotRestore.Create(Engine, Stream,
        TGocciaHeapSnapshotLimits.Defaults);
      Used := GC.PeakBytesAllocated - Before;
      Restore.Free;
    finally
      Stream.Free;
    end;
    GC.Collect;
    Previous := GC.MaxBytes;
    GC.MaxBytes := GC.BytesAllocated + Used div 2;
    try
      Expect<Boolean>(Pos('memory', RestoreRefusalIn(Engine, Bytes)) > 0)
        .ToBe(True);
    finally
      GC.MaxBytes := Previous;
    end;
    // What the stopped restore made is garbage, half the chain's shells
    // filled or not: collected without native recursion, and the engine
    // restores the whole chain afterwards.
    GC.Collect;
    Expect<string>(RestoreRefusalIn(Engine, Bytes)).ToBe('');
  finally
    Engine.Free;
    Lines.Free;
  end;
end;

procedure THeapSnapshotHardeningTests.TestEveryReservationCollects;
const
  CLOSURES = '(prev, i) => { const f = () => prev; f.i = i; return f; }';
var
  Feeder: TMutantFeeder;
  Refusal: string;
  Chain: TBytes;
  Collections: Integer;
begin
  // Engines are made first: building one is not a restore, and it does not
  // expect its reservations to collect.
  Feeder := TMutantFeeder.Create('');
  try
    Feeder.Stressed := True;
    Collections := TGarbageCollector.Instance.TotalCollections;
    Expect<string>(Feeder.Feed(PlainSeed, '[restored.self === restored, ' +
      'restored.list.length, restored.big, restored.s.length].join()'))
      .ToBe('');
    Expect<string>(Feeder.LastCheck)
      .ToBe('true,9,-1180591620717411303424,2');
    // The reader's reservations did collect, a few dozen times.
    Expect<Boolean>(TGarbageCollector.Instance.TotalCollections >
      Collections + 20).ToBe(True);
    Expect<string>(Feeder.Feed(PlainSeed)).ToBe('');
  finally
    Feeder.Free;
  end;
  Feeder := TMutantFeeder.Create(VM_SEED);
  try
    Feeder.Stressed := True;
    Expect<string>(Feeder.Feed(VMSeed, VM_CHECK)).ToBe('');
    Expect<string>(Feeder.LastCheck).ToBe(VM_CHECKED);
    Expect<string>(Feeder.Feed(VMSeed)).ToBe('');
  finally
    Feeder.Free;
  end;
  Chain := SnapshotModule(ChainModule(2000, CLOSURES), Refusal);
  Expect<string>(Refusal).ToBe('');
  Feeder := TMutantFeeder.Create(ChainModule(2000, CLOSURES));
  try
    Feeder.Stressed := True;
    Expect<string>(Feeder.Feed(Chain,
      ChainWalk('(p) => p()', '(p) => p.i'))).ToBe('');
    Expect<string>(Feeder.LastCheck).ToBe('2000,1999000');
  finally
    Feeder.Free;
  end;
end;

// ACount mutants of ASeed, one or two random bytes each, the same ones for
// the same ASeed and count: a xorshift from a fixed start.
{$PUSH}{$Q-}{$R-}
procedure FeedRandomMutants(const AFeeder: TMutantFeeder; const ASeed: TBytes;
  const ACount: Integer);
var
  State: Cardinal;
  Mutant: TBytes;
  I: Integer;

  function NextRandom: Cardinal;
  begin
    State := State xor (State shl 13);
    State := State xor (State shr 17);
    State := State xor (State shl 5);
    Result := State;
  end;

begin
  State := $0DDBA11;
  for I := 1 to ACount do
  begin
    Mutant := Copy(ASeed);
    Mutant[NextRandom mod Cardinal(Length(Mutant))] := Byte(NextRandom);
    if NextRandom mod 2 = 0 then
      Mutant[NextRandom mod Cardinal(Length(Mutant))] :=
        INTERESTING_BYTES[NextRandom mod Cardinal(Length(INTERESTING_BYTES))];
    AFeeder.Feed(Mutant);
  end;
end;
{$POP}

procedure THeapSnapshotHardeningTests.TestMutantsWhileEveryReservationCollects;
const
  STRESSED_MUTANTS = 400;
var
  Feeder: TMutantFeeder;
  Seed: TBytes;
begin
  Seed := VMSeed;
  Feeder := TMutantFeeder.Create(VM_SEED);
  try
    Feeder.Stressed := True;
    FeedRandomMutants(Feeder, Seed, STRESSED_MUTANTS);
    Expect<Integer>(Feeder.Restored + Feeder.Refused).ToBe(STRESSED_MUTANTS);
    Expect<Boolean>(Feeder.Restored > 0).ToBe(True);
  finally
    Feeder.Free;
  end;
end;

// Restores ASeed under STEPS + 1 ceilings spread evenly from what the
// engine holds before the restore up to what the restore needs, so the
// restore stops at that many points along the way: a sample, not every
// reservation (two that fall between neighbouring ceilings stop at the
// same one). Each restores (and is used) or is refused for memory.
procedure THeapSnapshotHardeningTests.SweepCeilings(const ASeed: TBytes;
  const AModule: string; const AStressed: Boolean);
const
  STEPS = 400;
var
  Feeder: TMutantFeeder;
  GC: TGarbageCollector;
  Before, Needed: Int64;
  I, Restored: Integer;
  Refusal: string;
begin
  Feeder := TMutantFeeder.Create(AModule);
  GC := TGarbageCollector.Instance;
  try
    GC.Collect;
    Before := GC.BytesAllocated;
    GC.ResetPeakBytesAllocated;
    Expect<string>(Feeder.Feed(ASeed, '1')).ToBe('');
    Needed := GC.PeakBytesAllocated - Before;
    Restored := 0;
    Feeder.Stressed := AStressed;
    for I := 0 to STEPS do
    begin
      GC.Collect;
      Refusal := Feeder.Feed(ASeed, '',
        GC.BytesAllocated + Needed * I div STEPS + 1);
      if Refusal = '' then
        Inc(Restored)
      else
        Expect<Boolean>(Pos('memory', LowerCase(Refusal)) > 0).ToBe(True);
    end;
    // The lowest ceilings refuse, the highest restore.
    Expect<Boolean>(Restored > 0).ToBe(True);
    Expect<Boolean>(Restored < STEPS + 1).ToBe(True);
  finally
    Feeder.Free;
  end;
end;

procedure THeapSnapshotHardeningTests.TestEveryCeilingRestoresOrRefuses;
begin
  SweepCeilings(PlainSeed, '', False);
  SweepCeilings(VMSeed, VM_SEED, False);
end;

procedure THeapSnapshotHardeningTests.TestEveryCeilingWhileEveryReservationCollects;
begin
  SweepCeilings(PlainSeed, '', True);
  SweepCeilings(VMSeed, VM_SEED, True);
end;

procedure THeapSnapshotHardeningTests.TestRepeatedMutantsLeaveTheHeap;
const
  MUTANTS = 300;
  PASSES = 4;
  // What one pass may leave on the heap: far less than one restored seed's
  // objects per mutant would take.
  SLACK_BYTES = 256 * 1024;
var
  Feeder: TMutantFeeder;
  GC: TGarbageCollector;
  Pass: Integer;
  Heap, Managed: array[1..PASSES] of Int64;
  Seed: TBytes;
begin
  // The heap a restore, its refusal or its use leaves behind is collected
  // with what it restored: after the first pass (which creates what any run
  // keeps, such as registered symbols and the exercise's own code), the
  // same mutants again add nothing that stays.
  Seed := VMSeed;
  Feeder := TMutantFeeder.Create(VM_SEED);
  GC := TGarbageCollector.Instance;
  try
    for Pass := 1 to PASSES do
    begin
      FeedRandomMutants(Feeder, Seed, MUTANTS);
      GC.Collect;
      Heap[Pass] := Int64(GetFPCHeapStatus.CurrHeapUsed);
      Managed[Pass] := GC.BytesAllocated;
    end;
    Expect<Boolean>(Feeder.Restored > 0).ToBe(True);
    Expect<Boolean>(Feeder.Refused > 0).ToBe(True);
    if (Managed[PASSES] > Managed[2]) or
       (Heap[PASSES] - Heap[2] >= SLACK_BYTES) then
      Fail(Format('after passes 2 and %d: %d and %d managed bytes, ' +
        '%d and %d heap bytes', [PASSES, Managed[2], Managed[PASSES],
        Heap[2], Heap[PASSES]]));
  finally
    Feeder.Free;
  end;
end;

procedure THeapSnapshotHardeningTests.TestEngineBuiltWhileEveryReservationCollects;
var
  Engine: TGocciaEngine;
  Lines: TStringList;
  Stage: TGocciaGlobalEvalStage;
  GC: TGarbageCollector;
begin
  GC := TGarbageCollector.Instance;
  Lines := TStringList.Create;
  Engine := nil;
  GC.CollectBeforeEveryReservation := True;
  try
    Engine := TGocciaEngine.Create(TEST_PATH, Lines);
    // The first iteration builds the concrete iterator prototypes.
    Expect<string>(Engine.EvaluateGlobalEval('String([...[1, 2].values()]' +
      '.length)', TEST_PATH, Stage).ToStringLiteral.Value).ToBe('2');
  finally
    GC.CollectBeforeEveryReservation := False;
    Engine.Free;
    Lines.Free;
  end;
end;

{ TOversizedStream }

function TOversizedStream.GetSize: Int64;
begin
  Result := MAX_NATIVE_ALLOCATION_BYTES + 1;
end;

function TOversizedStream.Read(var ABuffer; ACount: Longint): Longint;
begin
  raise EStreamError.Create('an oversized stream is never read');
end;

function TOversizedStream.Seek(const AOffset: Int64;
  AOrigin: TSeekOrigin): Int64;
begin
  Result := 0;
end;

// On a 32-bit target such a count would wrap the array's size to a small
// allocation that the codec then writes past.
procedure THeapSnapshotHardeningTests.TestReservationPastOneAllocationRefuses;
const
  ELEMENT_BYTES = 16;
var
  Table: TGocciaHeapSnapshotBuiltinTable;
  Reader: TGocciaHeapSnapshotReader;
  Refusal: string;
begin
  Table := TGocciaHeapSnapshotBuiltinTable.Create;
  Reader := TGocciaHeapSnapshotReader.Create(Table,
    TGocciaHeapSnapshotLimits.Defaults);
  try
    Refusal := '';
    try
      Reader.ReserveItems(
        MAX_NATIVE_ALLOCATION_BYTES div ELEMENT_BYTES + 1, ELEMENT_BYTES);
    except
      on E: EGocciaHeapSnapshotInvalid do
        Refusal := E.Message;
    end;
    Expect<string>(Refusal).ToBe(
      'restore: more than this target can allocate at once');
  finally
    Reader.Free;
    Table.Free;
  end;
end;

procedure THeapSnapshotHardeningTests.TestInputPastOneAllocationRefuses;
var
  Table: TGocciaHeapSnapshotBuiltinTable;
  Reader: TGocciaHeapSnapshotReader;
  Limits: TGocciaHeapSnapshotLimits;
  Stream: TOversizedStream;
  Refusal: string;
begin
  Limits := TGocciaHeapSnapshotLimits.Defaults;
  Limits.MaxInputBytes := High(Int64);
  Table := TGocciaHeapSnapshotBuiltinTable.Create;
  Reader := TGocciaHeapSnapshotReader.Create(Table, Limits);
  Stream := TOversizedStream.Create;
  try
    Refusal := '';
    try
      Reader.Restore(Stream);
    except
      on E: EGocciaHeapSnapshotInvalid do
        Refusal := E.Message;
    end;
    Expect<string>(Refusal).ToBe(
      'restore: the snapshot is larger than this target can hold');
  finally
    Stream.Free;
    Reader.Free;
    Table.Free;
  end;
end;

function TDriftingSnapshotCodec.Tag: string;
begin
  Result := 'test.drifting';
end;

function TDriftingSnapshotCodec.SchemaVersion: Word;
begin
  Result := 1;
end;

procedure TDriftingSnapshotCodec.Write(const AWriter: TGocciaHeapSnapshotWriter;
  const AObject: TGCManagedObject);
begin
  AWriter.WriteU32(TDriftingSnapshotValue(AObject).Payload);
end;

function TDriftingSnapshotCodec.Allocate(const AReader: TGocciaHeapSnapshotReader):
  TGCManagedObject;
begin
  Result := TDriftingSnapshotValue.Create;
end;

procedure TDriftingSnapshotCodec.Fill(const AReader: TGocciaHeapSnapshotReader;
  const AObject: TGCManagedObject);
begin
  TDriftingSnapshotValue(AObject).Payload := AReader.ReadU32 xor 1;
end;

procedure THeapSnapshotHardeningTests.TestLateBuiltinOrderIsStable;
var
  First, Second: TGocciaHeapSnapshotBuiltinTable;
  A, Z: TGocciaObjectValue;
  Left, Right: TMemoryStream;
  Writer: TGocciaHeapSnapshotWriter;
begin
  First := TGocciaHeapSnapshotBuiltinTable.Create;
  Second := TGocciaHeapSnapshotBuiltinTable.Create;
  Left := TMemoryStream.Create;
  Right := TMemoryStream.Create;
  try
    First.RecordFingerprints;
    Second.RecordFingerprints;
    A := TGocciaObjectValue.Create;
    Z := TGocciaObjectValue.Create;
    First.Add(A, 'a');
    First.Add(Z, 'z');
    Second.Add(Z, 'z');
    Second.Add(A, 'a');
    Writer := TGocciaHeapSnapshotWriter.Create(First);
    try
      Writer.Serialize([nil], Left);
    finally
      Writer.Free;
    end;
    Writer := TGocciaHeapSnapshotWriter.Create(Second);
    try
      Writer.Serialize([nil], Right);
    finally
      Writer.Free;
    end;
    Expect<Int64>(Left.Size).ToBe(Right.Size);
    Expect<Boolean>(CompareMem(Left.Memory, Right.Memory, Left.Size)).ToBe(True);
  finally
    Right.Free;
    Left.Free;
    Second.Free;
    First.Free;
  end;
end;

procedure THeapSnapshotHardeningTests.TestResnapshotDetectsPrivateStateDrift;
var
  Lines: TStringList;
  Engine, Destination: TGocciaEngine;
  Check: TGocciaHeapSnapshotRoundTrip;
  Value: TDriftingSnapshotValue;
begin
  Lines := TStringList.Create;
  Engine := TGocciaEngine.Create(TEST_PATH, Lines);
  Check := TGocciaHeapSnapshotRoundTrip.Create(Engine);
  try
    Value := TDriftingSnapshotValue.Create;
    Value.Payload := 42;
    Check.Observe(Value);
    Destination := TGocciaEngine.Create(TEST_PATH, Lines);
    try
      Check.Verify(Destination);
      Expect<Integer>(Check.Passed).ToBe(0);
      Expect<Integer>(Check.Refused).ToBe(0);
      Expect<Integer>(Check.Failed).ToBe(1);
      Expect<Boolean>(Pos('re-snapshot bytes differ',
        Check.FailureDetails.Text) > 0).ToBe(True);
    finally
      Destination.Free;
    end;
  finally
    Check.Free;
    Engine.Free;
    Lines.Free;
  end;
end;

function THeapSnapshotHardeningTests.CreateRoundTripDestination(
  const ASourceEngine: TGocciaEngine): TGocciaEngine;
begin
  Inc(FFactoryCalls);
  Result := TGocciaEngine.Create(ASourceEngine.SourcePath,
    ASourceEngine.SourceLines);
end;

procedure THeapSnapshotHardeningTests.TestLateFFIPrototypesRoundTrip;
var
  Lines: TStringList;
  Engine: TGocciaEngine;
  Context: TGocciaExecutionContextScope;
  Check: TGocciaHeapSnapshotRoundTrip;
  Descriptor, ArrayDescriptor: TGocciaFFITypeDescriptor;
  Root, Value: TGocciaObjectValue;
begin
  Lines := TStringList.Create;
  Engine := TGocciaEngine.Create(TEST_PATH, Lines);
  Check := TGocciaHeapSnapshotRoundTrip.Create(Engine);
  try
    Context := Engine.ActivateRealmExecutionContext;
    try
      Root := TGocciaObjectValue.Create;
      Check.Observe(Root);
      // Creating these native values builds their shared prototypes after
      // the session baseline. No library is loaded and no address is read.
      Value := TGocciaFFILibraryValue.Create(nil);
      Check.Observe(Value);
      Root.AssignProperty('library', Value.Prototype);
      Root.AssignProperty('nullptr', TGocciaFFIPointerValue.NullPointer);
      Value := TGocciaFFIPointerValue.Create(Pointer(1));
      Check.Observe(Value);
      Root.AssignProperty('pointer', Value.Prototype);
      Descriptor := TGocciaFFITypeDescriptor.CreateScalar(fftI32);
      try
        Value := TGocciaFFITypeDescriptorValue.Create(Descriptor);
        Check.Observe(Value);
        Root.AssignProperty('type', Value.Prototype);
        ArrayDescriptor := TGocciaFFITypeDescriptor.CreateArray(Descriptor, 1);
        try
          Value := TGocciaFFIAggregateValue.Create(ArrayDescriptor);
          Check.Observe(Value);
          Root.AssignProperty('aggregate', Value.Prototype);
        finally
          ArrayDescriptor.ReleaseReference;
        end;
      finally
        Descriptor.ReleaseReference;
      end;
      Value := TGocciaFFIVarArgsValue.Create([], []);
      Check.Observe(Value);
      Root.AssignProperty('varargs', Value.Prototype);
    finally
      Context.Free;
    end;
    Check.Verify(CreateRoundTripDestination);
    Expect<string>(Check.FailureDetails.Text).ToBe('');
    Expect<Integer>(Check.Passed).ToBe(1);
    // Rebuilding ordinary prototypes must not grant codecs to native state.
    Expect<Integer>(Check.Refused).ToBe(5);
    Expect<Integer>(Check.Failed).ToBe(0);
  finally
    Check.Free;
    Engine.Free;
    Lines.Free;
  end;
end;

procedure THeapSnapshotHardeningTests.TestObservedObjectsRoundTrip;
var
  Lines: TStringList;
  Engine: TGocciaEngine;
  Check: TGocciaHeapSnapshotRoundTrip;
  Stage: TGocciaGlobalEvalStage;
  Value: TGocciaValue;
begin
  Lines := TStringList.Create;
  Engine := TGocciaEngine.Create(TEST_PATH, Lines);
  Check := TGocciaHeapSnapshotRoundTrip.Create(Engine);
  try
    Value := Engine.EvaluateGlobalEval('(() => { const shared = { x: 1 }; ' +
      'const root = { a: shared, b: shared, list: [1, , undefined, -0], ' +
      '[Symbol("key")]: shared, frozen: Object.freeze({ x: 2 }) }; ' +
      'root.self = root; return root; })()', TEST_PATH, Stage);
    Check.Observe(Value);
    Check.Observe(Value);
    // Matcher cleanup removes its temporary root after expect returns.
    // The observation must have independent ownership across that removal.
    TGarbageCollector.Instance.AddTempRoot(Value);
    TGarbageCollector.Instance.RemoveTempRoot(Value);
    Check.Observe(TGocciaNumberLiteralValue.Create(1));
    // The checker holds references until idle: later mutations must survive.
    TGocciaObjectValue(Value).AssignProperty('later',
      TGocciaNumberLiteralValue.Create(42));
    Check.Observe(Engine.EvaluateGlobalEval('Reflect.apply(Array, null, ' +
      '[1, 2, 3])', TEST_PATH, Stage));
    Check.Observe(Engine.EvaluateGlobalEval('new Map()', TEST_PATH, Stage));
    TGarbageCollector.Instance.Collect;
    FFactoryCalls := 0;
    Check.Verify(CreateRoundTripDestination);
    Expect<Integer>(Check.Objects).ToBe(3);
    Expect<Integer>(Check.Passed).ToBe(2);
    Expect<Integer>(Check.Refused).ToBe(1);
    Expect<Integer>(Check.Failed).ToBe(0);
    Expect<Integer>(Check.RefusalReasons.Count).ToBe(1);
    Expect<Integer>(FFactoryCalls).ToBe(2);
  finally
    Check.Free;
    Engine.Free;
    Lines.Free;
  end;
end;

procedure THeapSnapshotHardeningTests.TestCanonicalDescriptionDetectsChanges;
const
  ORIGINALS: array[0..3] of string = (
    '(() => { const shared = {}; return { a: shared, b: shared }; })()',
    '[, undefined]',
    'Object.defineProperty({}, "x", { value: 1, writable: false })',
    '({ x: -0 })');
  CHANGED_SOURCES: array[0..3] of string = (
    '({ a: {}, b: {} })',
    '[undefined, undefined]',
    'Object.defineProperty({}, "x", { value: 1, writable: true })',
    '({ x: 0 })');
var
  Lines: TStringList;
  Engine: TGocciaEngine;
  Session: TGocciaHeapSnapshotSession;
  Stage: TGocciaGlobalEvalStage;
  I: Integer;
  Original, Changed, Repeated: string;
  Value: TGocciaValue;
begin
  Lines := TStringList.Create;
  Engine := TGocciaEngine.Create(TEST_PATH, Lines);
  Session := TGocciaHeapSnapshotSession.Create(Engine);
  try
    for I := Low(ORIGINALS) to High(ORIGINALS) do
    begin
      Value := Engine.EvaluateGlobalEval(ORIGINALS[I], TEST_PATH, Stage);
      Original := CanonicalDescription(Value, Session.Table);
      Value := Engine.EvaluateGlobalEval(CHANGED_SOURCES[I], TEST_PATH, Stage);
      Changed := CanonicalDescription(Value, Session.Table);
      Value := Engine.EvaluateGlobalEval(ORIGINALS[I], TEST_PATH, Stage);
      Repeated := CanonicalDescription(Value, Session.Table);
      Expect<Boolean>(Original <> Changed).ToBe(True);
      Expect<string>(Repeated).ToBe(Original);
    end;
    Value := Engine.EvaluateGlobalEval('Object.defineProperty({}, "x", ' +
      '{ get() { throw new Error("description called a getter"); } })',
      TEST_PATH, Stage);
    Expect<Boolean>(CanonicalDescription(Value, Session.Table) <> '').ToBe(True);
  finally
    Session.Free;
    Engine.Free;
    Lines.Free;
  end;
end;

procedure THeapSnapshotHardeningTests.TestObservedRestoreRefusalFails;
var
  Lines: TStringList;
  Engine, Destination: TGocciaEngine;
  Check: TGocciaHeapSnapshotRoundTrip;
  Capture: TCapture;
begin
  Lines := TStringList.Create;
  Lines.Text := 'capture(() => 42);';
  Capture := TCapture.Create;
  Engine := TGocciaEngine.Create(TEST_PATH, Lines);
  Engine.GlobalScope.DefineLexicalBinding('capture',
    TGocciaNativeFunctionValue.Create(Capture.Capture, 'capture', 1), dtConst);
  Check := TGocciaHeapSnapshotRoundTrip.Create(Engine);
  try
    Engine.Execute;
    Check.Observe(Capture.Value);
    // No matching compiled module: a snapshot was written successfully,
    // so the refusal at restore must fail the diagnostic lane.
    Destination := TGocciaEngine.Create(TEST_PATH, Lines);
    try
      Destination.GlobalScope.DefineLexicalBinding('capture',
        TGocciaNativeFunctionValue.Create(Capture.Capture, 'capture', 1), dtConst);
      Check.Verify(Destination);
      Expect<Integer>(Check.Passed).ToBe(0);
      Expect<Integer>(Check.Refused).ToBe(0);
      Expect<Integer>(Check.Failed).ToBe(1);
      Expect<Boolean>(Check.FailureDetails.Count > 0).ToBe(True);
    finally
      Destination.Free;
    end;
  finally
    Check.Free;
    if Assigned(Capture.Value) then
      TGarbageCollector.Instance.RemoveTempRoot(Capture.Value);
    Engine.Free;
    Capture.Free;
    Lines.Free;
  end;
end;

procedure THeapSnapshotHardeningTests.TestLazyNativeNamespacesRestore;

  procedure InstallNamespaces(const AEngine: TGocciaEngine);
  var
    Runtime: TGocciaRuntimeCore;
  begin
    Runtime := AttachRuntime(AEngine);
    Runtime.Install(TGocciaCSVRuntimeExtension.Create);
    Runtime.Install(TGocciaJSON5RuntimeExtension.Create);
    Runtime.Install(TGocciaJSONLRuntimeExtension.Create);
    Runtime.Install(TGocciaTOMLRuntimeExtension.Create);
    Runtime.Install(TGocciaTSVRuntimeExtension.Create);
    Runtime.Install(TGocciaYAMLRuntimeExtension.Create);
  end;

var
  Lines: TStringList;
  Engine, Destination: TGocciaEngine;
  Capture: TCapture;
  Check: TGocciaHeapSnapshotRoundTrip;
  Table: TGocciaHeapSnapshotBuiltinTable;
  Resolver: TGocciaBuiltinPathResolver;
begin
  Lines := TStringList.Create;
  Lines.Text := 'import { parse as csv } from "goccia:csv"; ' +
    'import { parse as json5 } from "goccia:json5"; ' +
    'import { parse as jsonl } from "goccia:jsonl"; ' +
    'import { parse as toml } from "goccia:toml"; ' +
    'import { parse as tsv } from "goccia:tsv"; ' +
    'import { parse as yaml } from "goccia:yaml"; ' +
    'capture([csv, json5, jsonl, toml, tsv, yaml]);';
  Engine := TGocciaEngine.Create(TEST_PATH, Lines);
  Capture := TCapture.Create;
  InstallNamespaces(Engine);
  Engine.GlobalScope.DefineLexicalBinding('capture',
    TGocciaNativeFunctionValue.Create(Capture.Capture, 'capture', 1), dtConst);
  Check := TGocciaHeapSnapshotRoundTrip.Create(Engine);
  try
    Engine.Execute;
    Check.Observe(Capture.Value);
    Destination := TGocciaEngine.Create(TEST_PATH, Lines);
    try
      InstallNamespaces(Destination);
      Destination.GlobalScope.DefineLexicalBinding('capture',
        TGocciaNativeFunctionValue.Create(Capture.Capture, 'capture', 1), dtConst);
      // The destination never compiles or runs these imports. Native values
      // have no bytecode templates and the registered factories do no I/O.
      Check.Verify(Destination);
      Expect<string>(Check.FailureDetails.Text).ToBe('');
      Expect<Integer>(Check.Passed).ToBe(1);
      Expect<Integer>(Check.Refused).ToBe(0);
      Expect<Integer>(Check.Failed).ToBe(0);
      Table := TGocciaHeapSnapshotBuiltinTable.Create;
      Resolver := TGocciaBuiltinPathResolver.Create(Table, Destination);
      try
        Expect<Boolean>(Assigned(Resolver.Resolve('%builtin' +
          HEAP_SNAPSHOT_PATH_SEPARATOR + 'unknown'))).ToBe(False);
      finally
        Resolver.Free;
        Table.Free;
      end;
    finally
      Destination.Free;
    end;
  finally
    Check.Free;
    if Assigned(Capture.Value) then
      TGarbageCollector.Instance.RemoveTempRoot(Capture.Value);
    Engine.Free;
    Capture.Free;
    Lines.Free;
  end;
end;

procedure THeapSnapshotHardeningTests.TestGlobalEvalComputesNaN;
var
  Engine: TGocciaEngine;
  Lines: TStringList;
  Stage: TGocciaGlobalEvalStage;
  Mask: TFPUExceptionMask;
  Value: string;
begin
  // Called by an embedder with the process's own floating-point mode (the
  // test programs run inside the engine's, RunGocciaTests), without the
  // scope THeapSnapshotExerciser enters itself.
  Lines := TStringList.Create;
  Engine := TGocciaEngine.Create(TEST_PATH, Lines);
  Mask := GetExceptionMask;
  try
    SetExceptionMask([exDenormalized, exUnderflow, exPrecision]);
    try
      // Computed when the code runs, not folded when it compiles.
      Value := Engine.EvaluateGlobalEval('((z) => String(z / z) + ' +
        'String(1 / z - 1 / z))(0)', TEST_PATH, Stage).ToStringLiteral.Value;
    finally
      SetExceptionMask(Mask);
    end;
    Expect<string>(Value).ToBe('NaNNaN');
  finally
    Engine.Free;
    Lines.Free;
  end;
end;

begin
  RegisterSnapshotCodec(TDriftingSnapshotValue, TDriftingSnapshotCodec.Create);
  if GetEnvironmentVariable(FULL_ENVIRONMENT) = 'full' then
  begin
    DeepLinks := FULL_LINKS;
    ByteShare := 1;
  end
  else
  begin
    DeepLinks := DEFAULT_LINKS;
    ByteShare := DEFAULT_BYTE_SHARE;
  end;
  TestRunnerProgram.AddSuite(THeapSnapshotHardeningTests.Create(
    'Heap snapshot hardening'));
  RunGocciaTests;
  ExitCode := TestResultToExitCode;
end.
