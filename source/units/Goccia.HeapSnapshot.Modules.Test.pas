program Goccia.HeapSnapshot.Modules.Test;

{$I Goccia.inc}

{ ADR 0148: heap snapshots of a run's modules. A run imports modules from
  files; its snapshot holds every module record the loader caches, and a
  fresh engine over the same files restores them without running any module
  body: it compiles each source module from its path, refuses one whose
  bytecode changed, and caches the restored records as evaluated modules, so
  a later import finds them. Closures over module scopes, live import
  bindings, re-exports, namespace objects and data modules round-trip; state
  outside the records refuses the snapshot, and the reader is fed every
  truncation and thousands of mutants of a real module snapshot. }

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes,
  Generics.Collections,
  SysUtils,

  TestingPascalLibrary,

  Goccia.Arguments.Collection,
  Goccia.Engine,
  Goccia.GarbageCollector,
  Goccia.HeapSnapshot,
  Goccia.HeapSnapshot.Codec,
  Goccia.HeapSnapshot.TestSupport,
  Goccia.InstructionLimit,
  Goccia.MicrotaskQueue,
  Goccia.Modules,
  Goccia.Modules.ContentProvider,
  Goccia.Scope.BindingMap,
  Goccia.SourcePipeline,
  Goccia.TestSetup,
  Goccia.Timeout,
  Goccia.Values.NativeFunction,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

const
  MUTANTS = 10000;

type
  { An engine over a directory of module files, running one of them as its
    entry module, with capture() and tick() in its global scope. }
  TModuleRun = class
  private
    FLines: TStringList;
    FCapture: TCapture;
    FTicks: Integer;
    function Tick(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
  public
    Engine: TGocciaEngine;
    constructor Create(const AEntryPath: string);
    destructor Destroy; override;
    // Evaluates ACheck as global eval code, runs the jobs it queued, then
    // evaluates AAfterJobs (when given) and returns that value as a string.
    function Check(const ACheck: string; const AAfterJobs: string = ''): string;
    property Captured: TCapture read FCapture;
    // How many times module bodies called tick().
    property Ticks: Integer read FTicks;
  end;

  THeapSnapshotModulesTests = class(TTestSuite)
  private
    FDir: string;
    procedure Define(const AName, AText: string);
    function PathOf(const AName: string): string;
    function SnapshotEntry(const AEntry: string; out ARefusal: string;
      const AAccounting: Boolean = True): TBytes;
    // '' when ABytes restored into ARun, else the refusal.
    function RestoreInto(const ARun: TModuleRun; const ABytes: TBytes): string;
    // Restores ABytes in a fresh engine over AEntry and runs the check there;
    // 'refused: ...' when the restore refuses.
    function RoundTripEntry(const AEntry: string; const ABytes: TBytes;
      const ACheck: string; const AAfterJobs: string = ''): string;
    procedure DefineGraph;
  public
    procedure SetupTests; override;
    procedure BeforeAll; override;
    procedure AfterAll; override;

    procedure TestEntryModuleScope;
    procedure TestImportedModulesKeepTheirBindings;
    procedure TestRestoreRunsNoModuleBody;
    procedure TestEveryRecordIsRestored;
    procedure TestDataModulesKeepTheRunsChanges;
    procedure TestVarBindingsRoundTrip;
    procedure TestForwardingChains;
    procedure TestRefusesChangedOrMissingSource;
    procedure TestRefusesAModuleAlreadyLoaded;
    procedure TestRefusedRestoreLeavesNoRecords;
    procedure TestRefusesStateOutsideTheRecords;
    procedure TestRefusesForgedLocalExports;
    procedure TestManyCompactBindingsRestore;
    procedure TestTouchedSourceRunsNoModuleBody;
    procedure TestChangedSourceOverRetainedBytecodeReloads;
    procedure TestChangedEntryFileReloads;
    procedure TestStoppedCacheLeavesNoRecords;
    procedure TestUncheckedChainStopsResolution;
    procedure TestEveryTruncationRefuses;
    procedure TestMutantsRefuseOrRestore;
  end;

{ TModuleRun }

constructor TModuleRun.Create(const AEntryPath: string);
begin
  inherited Create;
  FLines := TStringList.Create;
  FLines.LoadFromFile(AEntryPath);
  FCapture := TCapture.Create;
  Engine := TGocciaEngine.Create(AEntryPath, FLines);
  Engine.SourceType := stModule;
  Engine.ModuleLoader.SetContentProvider(
    TGocciaFileSystemModuleContentProvider.Create, True);
  Engine.GlobalScope.DefineLexicalBinding('capture',
    TGocciaNativeFunctionValue.Create(FCapture.Capture, 'capture', 1), dtConst);
  Engine.GlobalScope.DefineLexicalBinding('tick',
    TGocciaNativeFunctionValue.Create(Tick, 'tick', 0), dtConst);
end;

destructor TModuleRun.Destroy;
begin
  if Assigned(FCapture.Value) then
    TGarbageCollector.Instance.RemoveTempRoot(FCapture.Value);
  Engine.Free;
  FCapture.Free;
  FLines.Free;
  inherited;
end;

function TModuleRun.Tick(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Inc(FTicks);
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TModuleRun.Check(const ACheck, AAfterJobs: string): string;
var
  Stage: TGocciaGlobalEvalStage;
begin
  Result := Engine.EvaluateGlobalEval(ACheck, TEST_PATH,
    Stage).ToStringLiteral.Value;
  if AAfterJobs = '' then
    Exit;
  if Assigned(TGocciaMicrotaskQueue.Instance) then
    TGocciaMicrotaskQueue.Instance.DrainQueue;
  Result := Engine.EvaluateGlobalEval(AAfterJobs, TEST_PATH,
    Stage).ToStringLiteral.Value;
end;

function U32In(const ABytes: TBytes; const AAt: Integer): Cardinal;
begin
  Move(ABytes[AAt], Result, 4);
  Result := LEtoN(Result);
end;

procedure PutU32(var ABytes: TBytes; const AAt: Integer;
  const AValue: Cardinal);
var
  Value: Cardinal;
begin
  Value := NtoLE(AValue);
  Move(Value, ABytes[AAt], 4);
end;

// The index of the ASCII string AText in a snapshot's string table, or -1.
function StringIndexIn(const ABytes: TBytes; const AText: string): Integer;
var
  At, Count, I, J, Len: Integer;
  Same: Boolean;
begin
  Result := -1;
  At := 4 + 2;
  At := At + 4 + 2 * Integer(U32In(ABytes, At));  // build identity
  Count := Integer(U32In(ABytes, At));
  Inc(At, 4);
  for I := 0 to Count - 1 do
  begin
    Len := Integer(U32In(ABytes, At));
    Inc(At, 4);
    if Len = Length(AText) then
    begin
      Same := True;
      for J := 0 to Len - 1 do
        if (ABytes[At + 2 * J] <> Ord(AText[J + 1])) or
           (ABytes[At + 2 * J + 1] <> 0) then
          Same := False;
      if Same then
        Exit(I);
    end;
    Inc(At, 2 * Len);
  end;
end;

// Every string of a snapshot's string table, by its index.
function StringIndexesIn(const ABytes: TBytes): TDictionary<string, Integer>;
var
  At, Count, I, J, Len: Integer;
  Text: string;
begin
  Result := TDictionary<string, Integer>.Create;
  At := 4 + 2;
  At := At + 4 + 2 * Integer(U32In(ABytes, At));  // build identity
  Count := Integer(U32In(ABytes, At));
  Inc(At, 4);
  for I := 0 to Count - 1 do
  begin
    Len := Integer(U32In(ABytes, At));
    Inc(At, 4);
    SetLength(Text, Len);
    for J := 0 to Len - 1 do
      Text[J + 1] := Char(ABytes[At + 2 * J]);
    Result.AddOrSetValue(Text, I);
    Inc(At, 2 * Len);
  end;
end;

{ Where a record's local export AExport of ALocal starts in a snapshot: its
  export name, kind 0, the environment's object reference (a tag and an id)
  and its local name. -1 unless there is exactly one. }
function LocalExportAt(const ABytes: TBytes;
  const AExport, ALocal: Integer): Integer;
var
  At: Integer;
begin
  Result := -1;
  for At := 0 to Length(ABytes) - 14 do
    if (U32In(ABytes, At) = Cardinal(AExport)) and (ABytes[At + 4] = 0) and
       (U32In(ABytes, At + 10) = Cardinal(ALocal)) then
    begin
      if Result >= 0 then
        Exit(-1);
      Result := At;
    end;
end;

{ THeapSnapshotModulesTests }

procedure THeapSnapshotModulesTests.BeforeAll;
begin
  FDir := IncludeTrailingPathDelimiter(GetTempDir(False)) +
    'goccia-heap-snapshot-modules-' + IntToStr(GetProcessID) +
    PathDelim;
  ForceDirectories(FDir);
end;

procedure THeapSnapshotModulesTests.AfterAll;
var
  Search: TSearchRec;
begin
  if FindFirst(FDir + '*', faAnyFile, Search) = 0 then
  begin
    repeat
      if (Search.Attr and faDirectory) = 0 then
        DeleteFile(FDir + Search.Name);
    until FindNext(Search) <> 0;
    FindClose(Search);
  end;
  RemoveDir(FDir);
end;

procedure THeapSnapshotModulesTests.Define(const AName, AText: string);
var
  Lines: TStringList;
begin
  Lines := TStringList.Create;
  try
    Lines.Text := AText;
    Lines.SaveToFile(PathOf(AName));
  finally
    Lines.Free;
  end;
end;

function THeapSnapshotModulesTests.PathOf(const AName: string): string;
begin
  Result := FDir + AName;
end;

function THeapSnapshotModulesTests.SnapshotEntry(const AEntry: string;
  out ARefusal: string; const AAccounting: Boolean): TBytes;
var
  Run: TModuleRun;
  Session: TGocciaHeapSnapshotSession;
  Stream: TBytesStream;
begin
  Result := nil;
  ARefusal := '';
  Session := nil;
  Stream := TBytesStream.Create;
  Run := TModuleRun.Create(PathOf(AEntry));
  try
    Session := TGocciaHeapSnapshotSession.Create(Run.Engine);
    Session.CheckAccounting := AAccounting;
    Run.Engine.Execute;
    try
      Session.Snapshot([Run.Captured.Value], Stream);
      Result := Copy(Stream.Bytes, 0, Stream.Size);
    except
      on E: EGocciaHeapSnapshotRefused do
        ARefusal := E.Message;
    end;
  finally
    Session.Free;
    Run.Free;
    Stream.Free;
  end;
end;

function THeapSnapshotModulesTests.RestoreInto(const ARun: TModuleRun;
  const ABytes: TBytes): string;
var
  Restore: TGocciaHeapSnapshotRestore;
  Stream: TBytesStream;
begin
  Result := '';
  Restore := nil;
  Stream := TBytesStream.Create(ABytes);
  try
    try
      Restore := TGocciaHeapSnapshotRestore.Create(ARun.Engine, Stream,
        TGocciaHeapSnapshotLimits.Defaults);
    except
      on E: EGocciaHeapSnapshotInvalid do
        Exit(E.Message);
    end;
    TGocciaObjectValue(ARun.Engine.Realm.GlobalObject).AssignProperty(
      'restored', TGocciaValue(Restore.Roots[0]));
  finally
    Restore.Free;
    Stream.Free;
  end;
end;

function THeapSnapshotModulesTests.RoundTripEntry(const AEntry: string;
  const ABytes: TBytes; const ACheck, AAfterJobs: string): string;
var
  Run: TModuleRun;
  Refusal: string;
begin
  Run := TModuleRun.Create(PathOf(AEntry));
  try
    Refusal := RestoreInto(Run, ABytes);
    if Refusal <> '' then
      Exit('refused: ' + Refusal);
    Result := Run.Check(ACheck, AAfterJobs);
    // No module body ran in the restoring engine.
    if Run.Ticks <> 0 then
      Result := Result + ' (ticked ' + IntToStr(Run.Ticks) + ')';
  finally
    Run.Free;
  end;
end;

// counter.js holds state; lib.js re-exports from it and, with export *,
// from extra.js; main.js imports all three, a JSON and a text module.
procedure THeapSnapshotModulesTests.DefineGraph;
begin
  Define('counter.js', 'tick(); let count = 0; ' +
    'export const inc = () => { count += 1; return count; }; ' +
    'export { count };');
  Define('extra.js', 'tick(); export const extra = "x";');
  Define('lib.js', 'tick(); export { inc as increment } from "./counter.js"; ' +
    'export * from "./extra.js";');
  Define('data.json', '{ "x": 1, "list": [1, 2, 3] }');
  Define('note.txt', 'a note');
  Define('main.js', 'import { inc, count } from "./counter.js"; ' +
    'import * as ns from "./lib.js"; import data from "./data.json"; ' +
    'import note from "./note.txt"; tick(); inc(); data.x = 5; ' +
    'export const shared = { n: 1 }; ' +
    'capture({ inc, read: () => count, ns, nsAgain: () => ns, data, note, ' +
    'shared, load: () => import("./lib.js"), self: () => import("./main.js") });');
end;

procedure THeapSnapshotModulesTests.SetupTests;
begin
  Test('Closures over an entry module''s scope round-trip and share it',
    TestEntryModuleScope);
  Test('Imported modules keep live bindings, re-exports and namespaces',
    TestImportedModulesKeepTheirBindings);
  Test('A restore runs no module body, and a later import finds the module',
    TestRestoreRunsNoModuleBody);
  Test('Every module record is restored, whatever the roots reach',
    TestEveryRecordIsRestored);
  Test('Data modules keep the values the run changed',
    TestDataModulesKeepTheRunsChanges);
  Test('A module''s var bindings round-trip', TestVarBindingsRoundTrip);
  Test('Re-export chains restore, and one too long is refused',
    TestForwardingChains);
  Test('A module whose source changed or went missing is refused',
    TestRefusesChangedOrMissingSource);
  Test('A module the restoring engine loaded already is refused',
    TestRefusesAModuleAlreadyLoaded);
  Test('A refused restore leaves no module record behind',
    TestRefusedRestoreLeavesNoRecords);
  Test('Loader state outside the records refuses the snapshot',
    TestRefusesStateOutsideTheRecords);
  Test('A local export forged outside its module''s own bindings is refused',
    TestRefusesForgedLocalExports);
  Test('A module scope of many compact bindings restores',
    TestManyCompactBindingsRestore);
  Test('A source file touched after the run is not evaluated again',
    TestTouchedSourceRunsNoModuleBody);
  Test('A changed file over bytecode compiled before is loaded again',
    TestChangedSourceOverRetainedBytecodeReloads);
  Test('An entry file changed after its engine read it is loaded again',
    TestChangedEntryFileReloads);
  Test('A restore stopped while it caches its records leaves none behind',
    TestStoppedCacheLeavesNoRecords);
  Test('A collection during the restore follows no unchecked forwarding chain',
    TestUncheckedChainStopsResolution);
  Test('Every truncation of a module snapshot is refused',
    TestEveryTruncationRefuses);
  Test('Mutants of a module snapshot restore or are refused, never fault',
    TestMutantsRefuseOrRestore);
end;

procedure THeapSnapshotModulesTests.TestEntryModuleScope;
var
  Refusal: string;
  Bytes: TBytes;
begin
  Define('entry.js', 'tick(); const k = 7; let n = 0; ' +
    'const bump = () => { n += 1; return n; }; bump(); bump(); ' +
    'capture({ k: () => k, bump, peek: () => n });');
  Bytes := SnapshotEntry('entry.js', Refusal);
  Expect<string>(Refusal).ToBe('');
  Expect<string>(RoundTripEntry('entry.js', Bytes,
    '[restored.k(), restored.bump(), restored.peek(), restored.bump()].join()'))
    .ToBe('7,3,3,4');
end;

procedure THeapSnapshotModulesTests.TestImportedModulesKeepTheirBindings;
var
  Refusal: string;
  Bytes: TBytes;
begin
  DefineGraph;
  Bytes := SnapshotEntry('main.js', Refusal);
  Expect<string>(Refusal).ToBe('');
  // The import of count is live: it reads counter.js's binding.
  Expect<string>(RoundTripEntry('main.js', Bytes,
    '[restored.read(), restored.inc(), restored.read(), ' +
    'restored.ns.increment === restored.inc, restored.ns.extra, ' +
    'Object.keys(restored.ns).join("+"), restored.nsAgain() === restored.ns, ' +
    'Object.isFrozen(restored.ns), restored.ns[Symbol.toStringTag]].join()'))
    .ToBe('1,2,2,true,x,extra+increment,true,false,Module');
end;

procedure THeapSnapshotModulesTests.TestRestoreRunsNoModuleBody;
var
  Refusal: string;
  Bytes: TBytes;
begin
  DefineGraph;
  Bytes := SnapshotEntry('main.js', Refusal);
  Expect<string>(Refusal).ToBe('');
  // A dynamic import finds the restored, evaluated record: the same
  // namespace, and no body runs (RoundTripEntry reports a tick).
  Expect<string>(RoundTripEntry('main.js', Bytes,
    '(() => { restored.load().then((m) => { restored.out = [m === ' +
    'restored.ns, m.increment()].join(); }); ' +
    'restored.self().then((m) => { restored.out2 = m.shared === ' +
    'restored.shared; }); return ""; })()',
    'restored.out + "," + restored.out2')).ToBe('true,2,true');
end;

procedure THeapSnapshotModulesTests.TestEveryRecordIsRestored;
var
  Refusal: string;
  Bytes: TBytes;
begin
  // side.js is imported for its effect alone: nothing captured reaches it.
  Define('side.js', 'tick();');
  Define('roots.js', 'import "./side.js"; tick(); ' +
    'capture({ load: () => import("./side.js") });');
  Bytes := SnapshotEntry('roots.js', Refusal);
  Expect<string>(Refusal).ToBe('');
  Expect<string>(RoundTripEntry('roots.js', Bytes,
    '(() => { restored.load().then((m) => { restored.out = typeof m; }); ' +
    'return ""; })()', 'restored.out')).ToBe('object');
end;

procedure THeapSnapshotModulesTests.TestDataModulesKeepTheRunsChanges;
var
  Refusal: string;
  Bytes: TBytes;
begin
  DefineGraph;
  Bytes := SnapshotEntry('main.js', Refusal);
  Expect<string>(Refusal).ToBe('');
  // The run changed the JSON module's value: the restore keeps the change
  // rather than reading the file again.
  Define('data.json', '{ "x": 100, "list": [] }');
  Expect<string>(RoundTripEntry('main.js', Bytes,
    '[restored.data.x, restored.data.list.length, restored.note.trim()].join()'))
    .ToBe('5,3,a note');
end;

procedure THeapSnapshotModulesTests.TestVarBindingsRoundTrip;
var
  Refusal: string;
  Bytes: TBytes;
  Run: TModuleRun;
  Session: TGocciaHeapSnapshotSession;
  Stream: TBytesStream;
begin
  Define('vars.js', 'var total = 1; total += 2; ' +
    'capture({ add: (v) => { total += v; return total; } });');
  Bytes := nil;
  Refusal := '';
  Stream := TBytesStream.Create;
  Run := TModuleRun.Create(PathOf('vars.js'));
  Session := nil;
  try
    Run.Engine.Compatibility := [cfVar];
    Session := TGocciaHeapSnapshotSession.Create(Run.Engine);
    Run.Engine.Execute;
    try
      Session.Snapshot([Run.Captured.Value], Stream);
      Bytes := Copy(Stream.Bytes, 0, Stream.Size);
    except
      on E: EGocciaHeapSnapshotRefused do
        Refusal := E.Message;
    end;
  finally
    Session.Free;
    Run.Free;
    Stream.Free;
  end;
  Expect<string>(Refusal).ToBe('');
  Run := TModuleRun.Create(PathOf('vars.js'));
  try
    Run.Engine.Compatibility := [cfVar];
    Expect<string>(RestoreInto(Run, Bytes)).ToBe('');
    Expect<string>(Run.Check('String(restored.add(4))')).ToBe('7');
  finally
    Run.Free;
  end;
end;

procedure THeapSnapshotModulesTests.TestForwardingChains;
const
  SHORT_CHAIN = 64;
  LONG_CHAIN = 1100;

  procedure DefineChain(const APrefix: string; const ALength: Integer);
  var
    I: Integer;
  begin
    for I := 0 to ALength - 1 do
      Define(APrefix + IntToStr(I) + '.js', 'export { v } from "./' + APrefix +
        IntToStr(I + 1) + '.js";');
    Define(APrefix + IntToStr(ALength) + '.js', 'export let v = 1; ' +
      'export const setV = (x) => { v = x; };');
    Define(APrefix + 'main.js', 'import { v } from "./' + APrefix + '0.js"; ' +
      'import { setV } from "./' + APrefix + IntToStr(ALength) + '.js"; ' +
      'capture({ v: () => v, setV });');
  end;

var
  Refusal: string;
  Bytes: TBytes;
begin
  DefineChain('short', SHORT_CHAIN);
  Bytes := SnapshotEntry('shortmain.js', Refusal);
  Expect<string>(Refusal).ToBe('');
  Expect<string>(RoundTripEntry('shortmain.js', Bytes,
    '(() => { const before = restored.v(); restored.setV(9); ' +
    'return before + "," + restored.v(); })()')).ToBe('1,9');
  // A chain longer than a restore lets a resolution follow.
  DefineChain('long', LONG_CHAIN);
  Bytes := SnapshotEntry('longmain.js', Refusal);
  Expect<string>(Refusal).ToBe('');
  Expect<Boolean>(Pos('forwarded through more modules',
    RoundTripEntry('longmain.js', Bytes, '""')) > 0).ToBe(True);
end;

procedure THeapSnapshotModulesTests.TestRefusesChangedOrMissingSource;
var
  Refusal, Outcome: string;
  Bytes: TBytes;
begin
  DefineGraph;
  Bytes := SnapshotEntry('main.js', Refusal);
  Expect<string>(Refusal).ToBe('');
  Define('counter.js', 'tick(); let count = 10; ' +
    'export const inc = () => { count += 1; return count; }; ' +
    'export { count };');
  Outcome := RoundTripEntry('main.js', Bytes, '""');
  Expect<Boolean>(Pos('is not the one the snapshot ran', Outcome) > 0)
    .ToBe(True);
  DeleteFile(PathOf('counter.js'));
  Outcome := RoundTripEntry('main.js', Bytes, '""');
  Expect<Boolean>(Pos('does not compile here', Outcome) > 0).ToBe(True);
end;

procedure THeapSnapshotModulesTests.TestRefusesAModuleAlreadyLoaded;
var
  Refusal: string;
  Bytes: TBytes;
  Run: TModuleRun;
begin
  DefineGraph;
  Bytes := SnapshotEntry('main.js', Refusal);
  Expect<string>(Refusal).ToBe('');
  Run := TModuleRun.Create(PathOf('main.js'));
  try
    Run.Engine.Execute;
    Expect<Boolean>(Pos('loaded already', RestoreInto(Run, Bytes)) > 0)
      .ToBe(True);
  finally
    Run.Free;
  end;
  // Restored once, the records are this engine's: a second restore of the
  // same snapshot is refused too.
  Run := TModuleRun.Create(PathOf('main.js'));
  try
    Expect<string>(RestoreInto(Run, Bytes)).ToBe('');
    Expect<Boolean>(Pos('loaded already', RestoreInto(Run, Bytes)) > 0)
      .ToBe(True);
  finally
    Run.Free;
  end;
end;

procedure THeapSnapshotModulesTests.TestRefusedRestoreLeavesNoRecords;
var
  Refusal: string;
  Bytes, Cut: TBytes;
  Run: TModuleRun;
begin
  DefineGraph;
  Bytes := SnapshotEntry('main.js', Refusal);
  Expect<string>(Refusal).ToBe('');
  Run := TModuleRun.Create(PathOf('main.js'));
  try
    // Cut short of its roots: every record was allocated and filled, then
    // the restore is refused, and the records go with it.
    Cut := Copy(Bytes, 0, Length(Bytes) - 1);
    Expect<Boolean>(RestoreInto(Run, Cut) <> '').ToBe(True);
    TGarbageCollector.Instance.Collect;
    Expect<Boolean>(Run.Engine.ModuleLoader.HasCachedModule(
      PathOf('counter.js'))).ToBe(False);
    Expect<string>(RestoreInto(Run, Bytes)).ToBe('');
    TGarbageCollector.Instance.Collect;
    Expect<string>(Run.Check('String(restored.inc())')).ToBe('2');
  finally
    Run.Free;
  end;
end;

procedure THeapSnapshotModulesTests.TestRefusesStateOutsideTheRecords;
var
  Refusal: string;
  Run: TModuleRun;
  Session: TGocciaHeapSnapshotSession;
  Stream: TBytesStream;
  HostModule: TGocciaModule;
begin
  // import.meta objects are cached outside the heap.
  Define('meta.js', 'const m = import.meta; capture({ f: () => 1 });');
  SnapshotEntry('meta.js', Refusal, False);
  Expect<Boolean>(Pos('import.meta', Refusal) > 0).ToBe(True);
  // A module whose evaluation failed is remembered by the loader.
  Define('fails.js', 'throw new Error("no");');
  Define('caught.js', 'import("./fails.js").catch(() => {}); ' +
    'capture({ f: () => 1 });');
  SnapshotEntry('caught.js', Refusal, False);
  Expect<Boolean>(Pos('a module whose evaluation failed', Refusal) > 0)
    .ToBe(True);
  // A host module's natives can keep state no snapshot sees: refused until
  // host module codecs land.
  Define('host.js', 'import { answer } from "host:thing"; ' +
    'capture({ f: () => answer });');
  Stream := TBytesStream.Create;
  Run := TModuleRun.Create(PathOf('host.js'));
  Session := nil;
  HostModule := TGocciaModule.Create('host:thing');
  try
    HostModule.AddExportValue('answer', TGocciaNumberLiteralValue.Create(42));
    Run.Engine.RegisterHostModule('host:thing', HostModule);
    Session := TGocciaHeapSnapshotSession.Create(Run.Engine);
    Session.CheckAccounting := False;
    Run.Engine.Execute;
    Refusal := '';
    try
      Session.Snapshot([Run.Captured.Value], Stream);
    except
      on E: EGocciaHeapSnapshotRefused do
        Refusal := E.Message;
    end;
    Expect<Boolean>(Pos('a binding of the module host:thing', Refusal) > 0)
      .ToBe(True);
  finally
    Session.Free;
    Run.Free;
    HostModule.Free;
    Stream.Free;
  end;
end;

procedure THeapSnapshotModulesTests.TestRefusesForgedLocalExports;
var
  Refusal: string;
  Bytes, Forged: TBytes;
  Outer, Inner, Me, M, At, MainAt: Integer;
begin
  // loop.js imports its own export as me; main.js has a local export too,
  // so its environment is another module scope.
  Define('loop.js', 'import { outer as me } from "./loop.js"; ' +
    'let inner = 1; export { inner as outer }; ' +
    'export const get = () => me;');
  Define('loopmain.js', 'import { get } from "./loop.js"; ' +
    'export const m = 2; capture({ get });');
  Bytes := SnapshotEntry('loopmain.js', Refusal);
  Expect<string>(Refusal).ToBe('');
  Expect<string>(RoundTripEntry('loopmain.js', Bytes,
    'String(restored.get())')).ToBe('1');
  Outer := StringIndexIn(Bytes, 'outer');
  Inner := StringIndexIn(Bytes, 'inner');
  Me := StringIndexIn(Bytes, 'me');
  M := StringIndexIn(Bytes, 'm');
  Expect<Boolean>((Outer >= 0) and (Inner >= 0) and (Me >= 0) and (M >= 0))
    .ToBe(True);
  At := LocalExportAt(Bytes, Outer, Inner);
  MainAt := LocalExportAt(Bytes, M, M);
  Expect<Boolean>((At >= 0) and (MainAt >= 0)).ToBe(True);
  // Its local name an import binding: reading it would read the export
  // again, without end.
  Forged := Copy(Bytes);
  PutU32(Forged, At + 10, Me);
  Expect<Boolean>(Pos('a local export of no local binding',
    RoundTripEntry('loopmain.js', Forged, 'String(restored.get())')) > 0)
    .ToBe(True);
  // A name the environment does not bind.
  Forged := Copy(Bytes);
  PutU32(Forged, At + 10, Outer);
  Expect<Boolean>(Pos('a local export of no local binding',
    RoundTripEntry('loopmain.js', Forged, 'String(restored.get())')) > 0)
    .ToBe(True);
  // A binding of another module's environment, which this record does not
  // root.
  Forged := Copy(Bytes);
  Move(Bytes[MainAt + 5], Forged[At + 5], 5);
  PutU32(Forged, At + 10, M);
  Expect<Boolean>(Pos('a local export outside its module''s environment',
    RoundTripEntry('loopmain.js', Forged, 'String(restored.get())')) > 0)
    .ToBe(True);
end;

procedure THeapSnapshotModulesTests.TestManyCompactBindingsRestore;
const
  BINDINGS = 20000;
var
  Refusal: string;
  Bytes: TBytes;
  Source: TStringBuilder;
  I: Integer;
begin
  // Each binding holds true: its entry is as small as an entry gets.
  Source := TStringBuilder.Create;
  try
    for I := 0 to BINDINGS - 1 do
      Source.Append('const b').Append(I).Append(' = true; ');
    Source.Append('export const f = () => b0 && b')
      .Append(BINDINGS - 1).Append(';');
    Define('compact.js', Source.ToString);
  finally
    Source.Free;
  end;
  Define('compactmain.js', 'import { f } from "./compact.js"; ' +
    'capture({ f });');
  Bytes := SnapshotEntry('compactmain.js', Refusal, False);
  Expect<string>(Refusal).ToBe('');
  Expect<string>(RoundTripEntry('compactmain.js', Bytes,
    'String(restored.f())')).ToBe('true');
end;

procedure THeapSnapshotModulesTests.TestTouchedSourceRunsNoModuleBody;
var
  Refusal: string;
  Bytes: TBytes;

  procedure Touch(const AName: string);
  begin
    Expect<Integer>(FileSetDate(PathOf(AName),
      DateTimeToFileDate(Now + 1 / 24))).ToBe(0);
  end;

begin
  DefineGraph;
  Bytes := SnapshotEntry('main.js', Refusal);
  Expect<string>(Refusal).ToBe('');
  // The same files, newer: copied to another machine, say. Still the
  // modules the run ran, so a later import evaluates none of them again.
  Touch('main.js');
  Touch('lib.js');
  Touch('counter.js');
  Touch('extra.js');
  Expect<string>(RoundTripEntry('main.js', Bytes,
    '(() => { restored.load().then((m) => { restored.out = [m === ' +
    'restored.ns, m.increment()].join(); }); return ""; })()',
    'restored.out')).ToBe('true,2');
end;

procedure THeapSnapshotModulesTests.TestChangedSourceOverRetainedBytecodeReloads;
var
  Refusal, Counter: string;
  Bytes: TBytes;
  Run: TModuleRun;
begin
  DefineGraph;
  Bytes := SnapshotEntry('main.js', Refusal);
  Expect<string>(Refusal).ToBe('');
  Counter := StringReplace(PathOf('counter.js'), '\', '\\', [rfReplaceAll]);
  Run := TModuleRun.Create(PathOf('main.js'));
  try
    // A refused restore leaves the modules it compiled in the engine.
    Expect<Boolean>(RestoreInto(Run, Copy(Bytes, 0, Length(Bytes) - 1)) <> '')
      .ToBe(True);
    // counter.js changes; the restore uses the bytecode compiled before and
    // does not read the file, so the record keeps the run's time, and the
    // newer file is loaded again on the next import.
    Define('counter.js', 'tick(); let count = 10; ' +
      'export const inc = () => { count += 1; return count; }; ' +
      'export { count };');
    Expect<Integer>(FileSetDate(PathOf('counter.js'),
      DateTimeToFileDate(Now + 1 / 24))).ToBe(0);
    Expect<string>(RestoreInto(Run, Bytes)).ToBe('');
    Expect<string>(Run.Check('(() => { import("' + Counter + '").then(' +
      '(m) => { restored.out = m.count; }); return ""; })()',
      'String(restored.out)')).ToBe('10');
    Expect<Integer>(Run.Ticks).ToBe(1);
  finally
    Run.Free;
  end;
end;

procedure THeapSnapshotModulesTests.TestChangedEntryFileReloads;
var
  Refusal, Main: string;
  Bytes: TBytes;
  Run: TModuleRun;
begin
  DefineGraph;
  Bytes := SnapshotEntry('main.js', Refusal);
  Expect<string>(Refusal).ToBe('');
  Main := StringReplace(PathOf('main.js'), '\', '\\', [rfReplaceAll]);
  // The restoring engine is made with main.js as the run had it; the file
  // changes before the restore, which compiles the engine's own source and
  // does not read the file: the record keeps the run's time, and the newer
  // file is loaded again on the next import.
  Run := TModuleRun.Create(PathOf('main.js'));
  try
    Define('main.js', 'tick(); export const shared = { n: 2 };');
    Expect<Integer>(FileSetDate(PathOf('main.js'),
      DateTimeToFileDate(Now + 1 / 24))).ToBe(0);
    Expect<string>(RestoreInto(Run, Bytes)).ToBe('');
    Expect<string>(Run.Check('(() => { import("' + Main + '").then(' +
      '(m) => { restored.out = m.shared.n; }); return ""; })()',
      'String(restored.out)')).ToBe('2');
    Expect<Integer>(Run.Ticks).ToBe(1);
  finally
    Run.Free;
  end;
end;

procedure THeapSnapshotModulesTests.TestStoppedCacheLeavesNoRecords;
var
  Refusal: string;
  Bytes: TBytes;
  Low, High, Mid, Limit: Int64;

  // Whether the restore completes within ALimit polls; a stopped one must
  // leave no record in the loader.
  function RestoresWithin(const ALimit: Int64): Boolean;
  var
    Run: TModuleRun;
    Outcome: string;
  begin
    Run := TModuleRun.Create(PathOf('main.js'));
    try
      StartInstructionLimit(ALimit);
      try
        try
          Outcome := RestoreInto(Run, Bytes);
        except
          on E: TGocciaInstructionLimitError do
            Outcome := 'stopped';
        end;
      finally
        ClearInstructionLimit;
      end;
      Result := Outcome = '';
      if not Result then
      begin
        TGarbageCollector.Instance.Collect;
        Expect<Boolean>(Run.Engine.ModuleLoader.HasCachedModule(
          PathOf('counter.js'))).ToBe(False);
        Expect<Boolean>(Run.Engine.ModuleLoader.HasCachedModule(
          PathOf('main.js'))).ToBe(False);
      end;
    finally
      Run.Free;
    end;
  end;

begin
  DefineGraph;
  Bytes := SnapshotEntry('main.js', Refusal);
  Expect<string>(Refusal).ToBe('');
  // The fewest polls the restore completes within; the last of them are
  // the loader's, one per key it caches.
  Low := 1;
  High := 1;
  while not RestoresWithin(High) do
  begin
    Low := High + 1;
    High := High * 2;
  end;
  while Low < High do
  begin
    Mid := (Low + High) div 2;
    if RestoresWithin(Mid) then
      High := Mid
    else
      Low := Mid + 1;
  end;
  Expect<Boolean>(High > 16).ToBe(True);
  for Limit := High - 16 to High - 1 do
    Expect<Boolean>(RestoresWithin(Limit)).ToBe(False);
end;

procedure THeapSnapshotModulesTests.TestUncheckedChainStopsResolution;
const
  CHAIN = 60000;
var
  Refusal: string;
  Bytes, Forged: TBytes;
  Source: TStringBuilder;
  Strings: TDictionary<string, Integer>;
  Out: TBytesStream;
  V, At, LenAt, PayloadStart, PayloadEnd, Rec, P, I: Integer;
  Low, High, Mid: Int64;

  procedure PutU8(const AValue: Byte);
  begin
    Out.WriteBuffer(AValue, 1);
  end;

  procedure PutU32Out(const AValue: Cardinal);
  var
    Value: Cardinal;
  begin
    Value := NtoLE(AValue);
    Out.WriteBuffer(Value, 4);
  end;

  // The restore's refusal under a ceiling AMargin bytes above what is live.
  function RestoreUnder(const AMargin: Int64): string;
  var
    Run: TModuleRun;
    GC: TGarbageCollector;
    Previous: Int64;
  begin
    Run := TModuleRun.Create(PathOf('bigmain.js'));
    GC := TGarbageCollector.Instance;
    Previous := GC.MaxBytes;
    try
      GC.Collect;
      GC.MaxBytes := GC.BytesAllocated + AMargin;
      try
        Result := RestoreInto(Run, Forged);
      finally
        GC.MaxBytes := Previous;
      end;
    finally
      Run.Free;
    end;
  end;

  procedure Forward(const AName, ATarget: Integer);
  begin
    PutU32Out(AName);
    PutU8(1);
    PutU32Out(Rec);
    PutU32Out(ATarget);
  end;

begin
  // big.js binds b0 .. b59999, so their names are in the string table.
  Source := TStringBuilder.Create;
  try
    Source.Append('export let v = 0; ');
    for I := 0 to CHAIN - 1 do
      Source.Append('const b').Append(I).Append(' = 0; ');
    Define('big.js', Source.ToString);
  finally
    Source.Free;
  end;
  Define('bigmain.js', 'import { v } from "./big.js"; ' +
    'capture({ v: () => v });');
  Bytes := SnapshotEntry('bigmain.js', Refusal, False);
  Expect<string>(Refusal).ToBe('');
  Strings := StringIndexesIn(Bytes);
  Out := TBytesStream.Create;
  try
    V := Strings['v'];
    // big.js's payload: its time, its environment, one export binding (v),
    // the rest; and bigmain.js's import binding of it names its record.
    At := LocalExportAt(Bytes, V, V);
    Expect<Boolean>(At >= 0).ToBe(True);
    Expect<Integer>(Integer(U32In(Bytes, At - 4))).ToBe(1);
    PayloadStart := At - 4 - 5 - 8;
    LenAt := PayloadStart - 4;
    PayloadEnd := PayloadStart + Integer(U32In(Bytes, LenAt));
    // In the object table, before any record's payload.
    Rec := 0;
    for P := 0 to LenAt - 12 do
      if (U32In(Bytes, P) = Cardinal(V)) and
         (U32In(Bytes, P + 8) = Cardinal(V)) and
         (U32In(Bytes, P + 4) >= 1) and (U32In(Bytes, P + 4) <= 8) then
      begin
        Expect<Integer>(Rec).ToBe(0);
        Rec := Integer(U32In(Bytes, P + 4));
      end;
    Expect<Boolean>(Rec > 0).ToBe(True);
    // Forged: v forwards through b0, b1 ... within big.js's own record, far
    // past what a restore lets a resolution follow, to a local binding.
    Out.WriteBuffer(Bytes[PayloadStart], 8 + 5);
    PutU32Out(1 + CHAIN);
    Forward(V, Strings['b0']);
    for I := 0 to CHAIN - 2 do
      Forward(Strings['b' + IntToStr(I)], Strings['b' + IntToStr(I + 1)]);
    PutU32Out(Strings['b' + IntToStr(CHAIN - 1)]);
    PutU8(0);
    Out.WriteBuffer(Bytes[At + 5], 5);
    PutU32Out(Strings['b' + IntToStr(CHAIN - 1)]);
    Out.WriteBuffer(Bytes[At + 14], PayloadEnd - (At + 14));
    SetLength(Forged, LenAt + 4 + Out.Size + (Length(Bytes) - PayloadEnd));
    Move(Bytes[0], Forged[0], LenAt);
    PutU32(Forged, LenAt, Out.Size);
    Move(Out.Bytes[0], Forged[LenAt + 4], Out.Size);
    Move(Bytes[PayloadEnd], Forged[LenAt + 4 + Out.Size],
      Length(Bytes) - PayloadEnd);
  finally
    Out.Free;
    Strings.Free;
  end;
  Expect<Boolean>(Pos('forwarded through more modules',
    RoundTripEntry('bigmain.js', Forged, '""')) > 0).ToBe(True);
  // Under a memory ceiling the last of the restore's charges, made while it
  // fills the records, does not fit: the collection it makes marks
  // bigmain.js's import binding of v, and resolving it must stop at the
  // unchecked record, not follow 60,000 forwardings on the native stack.
  Low := 0;
  High := 256 * 1024 * 1024;
  while Low < High do
  begin
    Mid := (Low + High) div 2;
    if Pos('forwarded through', RestoreUnder(Mid)) > 0 then
      High := Mid
    else
      Low := Mid + 1;
  end;
  Expect<Boolean>(Pos('forwarded through', RestoreUnder(High)) > 0)
    .ToBe(True);
  Expect<Boolean>(RestoreUnder(High - 1) <> '').ToBe(True);
  Expect<Boolean>(RestoreUnder(High - 64) <> '').ToBe(True);
  Expect<Boolean>(RestoreUnder(High - 127) <> '').ToBe(True);
end;

procedure THeapSnapshotModulesTests.TestEveryTruncationRefuses;
var
  Refusal: string;
  Bytes: TBytes;
  Len, Refused: Integer;
  Run: TModuleRun;
begin
  DefineGraph;
  Bytes := SnapshotEntry('main.js', Refusal);
  Expect<string>(Refusal).ToBe('');
  Refused := 0;
  // A refused restore leaves the engine as it was, so one serves them all.
  Run := TModuleRun.Create(PathOf('main.js'));
  try
    for Len := 0 to Length(Bytes) - 1 do
    begin
      if RestoreInto(Run, Copy(Bytes, 0, Len)) <> '' then
        Inc(Refused);
      if Len mod 256 = 0 then
        TGarbageCollector.Instance.Collect;
    end;
  finally
    Run.Free;
  end;
  Expect<Integer>(Refused).ToBe(Length(Bytes));
end;

{$PUSH}{$Q-}{$R-}
procedure THeapSnapshotModulesTests.TestMutantsRefuseOrRestore;
const
  MUTANT_CHECK_TIMEOUT_MS = 1000;
  // Uses whatever restored, whatever a mutant made of it.
  CHECK = '(() => { const out = []; for (const step of [' +
    '() => restored.inc(), () => restored.read(), ' +
    '() => restored.ns.increment(), () => Object.keys(restored.ns).join(), ' +
    '() => restored.nsAgain() === restored.ns, () => restored.data.x, ' +
    '() => restored.note.trim(), () => restored.shared.n]) { try { ' +
    'out.push(String(step())); } catch (e) { out.push("threw"); } } ' +
    'return out.join("|"); })()';
var
  Refusal, Outcome: string;
  Bytes, Mutant: TBytes;
  Seed: Cardinal;
  I, J, Edits, Restored, Refused: Integer;
  Run: TModuleRun;

  function NextRandom: Cardinal;
  begin
    // xorshift32: the same mutants on every run.
    Seed := Seed xor (Seed shl 13);
    Seed := Seed xor (Seed shr 17);
    Seed := Seed xor (Seed shl 5);
    Result := Seed;
  end;

begin
  DefineGraph;
  Bytes := SnapshotEntry('main.js', Refusal);
  Expect<string>(Refusal).ToBe('');
  Seed := $0DD5EED5;
  Restored := 0;
  Refused := 0;
  Run := TModuleRun.Create(PathOf('main.js'));
  try
    Expect<string>(RestoreInto(Run, Bytes)).ToBe('');
    Expect<string>(Run.Check(CHECK)).ToBe('2|2|3|extra,increment|true|5|' +
      'a note|1');
    for I := 1 to MUTANTS do
    begin
      Mutant := Copy(Bytes);
      Edits := 1 + Integer(NextRandom mod 4);
      for J := 1 to Edits do
        case NextRandom mod 4 of
          0: Mutant[NextRandom mod Cardinal(Length(Mutant))] :=
               Byte(NextRandom);
          1: Mutant[NextRandom mod Cardinal(Length(Mutant))] :=
               Mutant[NextRandom mod Cardinal(Length(Mutant))] xor
               (1 shl (NextRandom mod 8));
          2: Mutant[NextRandom mod Cardinal(Length(Mutant))] := $FF;
          3: Mutant[NextRandom mod Cardinal(Length(Mutant))] := 0;
        end;
      // A restore leaves its records in the engine: each mutant gets a fresh
      // one.
      FreeAndNil(Run);
      Run := TModuleRun.Create(PathOf('main.js'));
      // Every outcome other than a restore or a refusal propagates and fails
      // the test.
      if RestoreInto(Run, Mutant) = '' then
      begin
        Inc(Restored);
        // A mutant can make the check run long: the run's timeout ends it.
        StartExecutionTimeout(MUTANT_CHECK_TIMEOUT_MS);
        try
          try
            Outcome := Run.Check(CHECK);
          except
            on E: TGocciaTimeoutError do
              Outcome := 'timed out';
          end;
        finally
          ClearExecutionTimeout;
        end;
      end
      else
        Inc(Refused);
      if I mod 256 = 0 then
        TGarbageCollector.Instance.Collect;
    end;
  finally
    Run.Free;
  end;
  Expect<Integer>(Restored + Refused).ToBe(MUTANTS);
  Expect<Boolean>(Refused > 0).ToBe(True);
  Expect<Boolean>(Restored > 0).ToBe(True);
end;
{$POP}

begin
  TestRunnerProgram.AddSuite(THeapSnapshotModulesTests.Create(
    'Heap snapshots of modules'));
  RunGocciaTests;
  ExitCode := TestResultToExitCode;
end.
