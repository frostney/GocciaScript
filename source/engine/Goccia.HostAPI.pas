unit Goccia.HostAPI;
{ The CLI host: what GocciaRunner and GocciaTestRunner give a script beyond the language. The API surfaces
  are JS preludes (prelude/host.js, prelude/testing.js) called with the natives they need from here; this unit
  installs them on the thread's realm, keeps what the host reads back, and reports a test file's results.

  The conformance host (goccia --test262) installs none of this: its realm keeps print, eval and the Annex B
  globals the CLIs leave out. }
{$mode delphiunicode}{$H+}
{$I Goccia.Checks.inc}
{$MINFPCONSTPREC 64}

interface

uses
  SysUtils, Classes, Goccia.Core, Goccia.Capabilities, Goccia.ModuleResolver, Goccia.Builtins.Testing.Snapshots;

type
  { an audited read decision of the read policy (the engine's Audit, gckReadFile); False when the audit sink failed }
  TGocciaReadAudit = function(Allow: Boolean; const Subject, Reason: string): Boolean of object;

{ the read capability over this thread's module graph (ADR 0122), as GocciaScript's loader judges it: a request's
  every host candidate, and the file it resolves to, is readable when no deny covers it and it is a literal import
  inside ProjectRoot (outside its .goccia cache), inside the package a literal bare import resolved in, or covered
  by a read grant; a refusal throws PermissionDenied naming the specifier. Off: no judgment (a sandbox's own files,
  the old behaviour). Modules the host loads before the entry are its own. }
procedure SetReadPolicy(On: Boolean; const Caps: TGocciaCapabilities; const ProjectRoot: string;
  Audit: TGocciaReadAudit);
{ while On, what the host loads for a config (its module manifests, globals and host-environment modules) is read
  as the script's: the read policy judges it, and the modules it imports }
procedure SetGuestLoads(On: Boolean);
{ the directory of the config naming what is loaded next ('' none): inside its own tree that needs no grant }
procedure SetGuestScope(const Dir: string);
{ a file a config names, read under the read policy while guest loads are on: False with Err the refusal }
function GuestReadAllowed(const Path: UnicodeString; var Err: UnicodeString): Boolean;
{ the error the host's last failed load threw (a globals module's, a manifest's), taken: undefined for none }
function TakeHostError: TValue;

{ the host's globals on this thread's realm (after EngineInit): the const Goccia, and the conformance host's
  extras removed; TestMode lists the test runner's globals in Goccia.runtimeGlobals. Bare: GocciaScriptLoaderBare's
  surface, the core engine without a runtime (Goccia, DOMException, atob, btoa, PermissionDenied and the conformance
  host's print; no console, performance, timers, web APIs or host modules). AnnexB keeps the conformance host's
  Annex B globals escape and unescape (B.2.1.1, B.2.1.2) in this realm and every realm made after it }
function InstallHostGlobals(ShadowRealm, TestMode: Boolean; const ConfigJson: UnicodeString;
  var Err: UnicodeString; ExposeTimers: Boolean = True; Bare: Boolean = False; AnnexB: Boolean = False): Boolean;
{ the heap limit when none is given: half the memory available to the process, at most 8 GB (700 MB on a 32-bit
  build) }
function DefaultHeapCeiling: Int64;

const
  { console output's object inspection depth (--inspect-depth): the default, and the deepest it may be set }
  DEFAULT_INSPECT_DEPTH = 5;
  MAX_INSPECT_DEPTH = 64;
{ this thread's console inspection depth: ADepth levels, 1 to MAX_INSPECT_DEPTH (0 or less: the default) }
procedure SetInspectDepth(ADepth: Integer);

type
  { a console line with the method that wrote it (log, warn, error, ...), in place of writing it to stdout }
  TConsoleSink = procedure(const Method, Line: UnicodeString) of object;
{ this thread's console sink (nil: lines go to stdout) }
procedure SetConsoleSink(Sink: TConsoleSink);
{ the resolver of this thread's imports: a host's, configured with its import map, aliases and node_modules grant
  (Goccia.Modules.Resolver, not owned); nil for the plain one, which resolves relative and absolute paths only }
procedure SetHostResolver(R: TModuleResolver);
{ the REPL's display of V (host.js replFormat, Goccia.REPL.Formatter's rules), with ANSI colors when Color; False
  with an exception pending when a getter it ran threw }
function ReplFormat(V: TValue; Color: Boolean; var Text: UnicodeString): Boolean;
type
  { a host's refusal of an import by its capability policy (a provider package or node_modules the import
    capability does not grant), raised from the host resolver's grant: the resolution throws the host realm's
    PermissionDenied "<Capability>: <Scope>" with Suggestion, as GocciaScript's ThrowPermissionDenied does }
  EGocciaPermissionDenied = class(Exception)
  public
    Capability, Scope, Suggestion: string;
    constructor CreateDenied(const ACapability, AScope, ASuggestion: string);
  end;
type
  { whether Path is a file of a materialized provider package reached as that package: resolved through its
    import-map entry (PackageRoot its root) or imported by one of its own files (ImportingFile); such a file is
    pinned and verified as it loads, so the import grant covers it without a read grant (ADR 0122) }
  TProviderGraphCheck = function(const Path, ImportingFile, PackageRoot: string): Boolean of object;
{ this thread's provider package check (nil: none) }
procedure SetProviderGraphCheck(Check: TProviderGraphCheck);
{ frees this thread's module resolution state (the plain resolver, the specifiers each module was first imported
  by): an engine's teardown, so the next engine on the thread starts without them }
procedure HostModulesDone;
{ the path the last failed resolution tried (EModuleNotFound's ResolvedCandidatePath; '' for none), taken }
function TakeResolveCandidate: UnicodeString;
{ globals injection, after InstallHostGlobals: Text, a data file's (Ext its extension: .json; .json5 and .jsonc;
  .toml; .yaml and .yml), is parsed by that format's parser into an object whose own enumerable properties become
  const global bindings (Goccia.VM InjectGlobalBinding); False with Err the message when the text does not parse, is
  not an object, or a name is refused }
function InjectDataGlobals(const Text, Ext, Path: UnicodeString; var Err: UnicodeString): Boolean;
{ one global from ValueText read as JSON, or as that string when it is not JSON (--global name=value) }
function InjectInlineGlobal(const Name, ValueText: UnicodeString; var Err: UnicodeString): Boolean;
{ virtual modules (docs/virtual-modules.md), after InstallHostGlobals: a config file's "modules" (a module map, a
  manifest path or an array of them) and "module" (one definition string or an array of them); a manifest file (a
  module map in JSON, JSON5, TOML or YAML); one definition, "address=source", or address= followed by a JSON descriptor. Relative
  addresses and paths resolve from the declaring file's directory (Base for a definition). False with Err. }
function DefineConfigVirtualModules(const ConfigPath: UnicodeString; var Err: UnicodeString): Boolean;
function DefineManifestVirtualModules(const ManifestPath: UnicodeString; var Err: UnicodeString): Boolean;
function DefineInlineVirtualModule(const Definition, Base: UnicodeString; var Err: UnicodeString): Boolean;
type
  { A fresh list of alternating export names and values in the requesting realm. False with a pending error. }
  TNativeModuleFactory = function(var ExportList: TValue): Boolean of object;
function DefineNativeModule(const Specifier: UnicodeString; Factory: TNativeModuleFactory;
  var Err: UnicodeString): Boolean;
type
  { what a module provider answers with: the module's JavaScript source text, or a bytecode file (CompileArtifact's
    bytes, loaded and checked against the engine's settings as an imported .gbc file is) }
  TModuleProviderAnswerKind = (mpaSource, mpaBytecode);
  TModuleProviderAnswer = record
    Kind: TModuleProviderAnswerKind;
    Source: UnicodeString;
    Bytecode: TBytes;
  end;
  { Specifier's module, asked for when it is first imported. False with a pending error. }
  TModuleProvider = function(const Specifier: UnicodeString; var Answer: TModuleProviderAnswer): Boolean of object;
function DefineModuleProvider(const Specifier: UnicodeString; Provider: TModuleProvider;
  var Err: UnicodeString): Boolean;
{ a JavaScript or TypeScript module's exports: each named export a global (a globals module), or its default
  export the module map (a module manifest); the module is evaluated in this engine }
function InjectModuleGlobals(const Path: UnicodeString; var Err: UnicodeString): Boolean;
function DefineModuleManifestVirtualModules(const Path: UnicodeString; var Err: UnicodeString): Boolean;
{ the jobs the script queued, then the timers in due order on the virtual clock, each followed by the jobs it
  queued, until none is left (False: an uncaught exception, in Pending) }
function RunEventLoop: Boolean;
type
  TEmbedderWork = class
  public
    function HasPending: Boolean; virtual; abstract;
    { A zero timeout pumps only ready work. A positive timeout bounds the wait for its next wake. }
    function Pump(TimeoutMs: Cardinal): Boolean; virtual; abstract;
    procedure Discard; virtual; abstract;
    { A test can abandon requests it started without affecting work already pending. }
    function Mark: QWord; virtual; abstract;
    procedure DiscardFrom(AMark: QWord); virtual; abstract;
  end;
procedure SetEmbedderWork(Work: TEmbedderWork);
procedure DiscardEmbedderWork;
{ drops the work a run that failed or aborted left pending, while its limits are still in force: its jobs, the
  rejections it left, its timers and intervals, fetch completions, the embedder's host work, Atomics.waitAsync waits
  and the FinalizationRegistry cleanup and registrations since WeakMark, so none of it runs in the engine's next run. The module evaluations it
  started after ModuleMark fail first (Goccia.Modules FailModuleEvaluationsSince), with the abort's error, or with
  Thrown, the value the run failed with. An entry point that reports only text (HasThrown False) has them fail with
  the thrown value it kept for the host (TakeHostError), else with an Error of FailText. }
procedure DiscardFailedRunWork(WeakMark, ModuleMark: Int64; Thrown: TValue; const FailText: UnicodeString = '';
  HasThrown: Boolean = True);
{ the test framework API (describe, test, expect, ...) as globals of this thread's realm }
function InstallTestingAPI(var Err: UnicodeString): Boolean;
{ the test file's vi.mock directives (Goccia.Vitest.MockHoist): each mock's module, served at the address its
  specifier resolves to ahead of the file there }
procedure HoistTestMocks(const FileName: string; const Source: UnicodeString);
{ runs the suites the test file registered (after InstallTestingAPI) and the jobs they queue; Json their result
  (totalTests, passed, failed, skipped, assertions, failedTests), or False and Err the error that stopped
  them }
function RunTestSuites(var Json, Err: UnicodeString): Boolean;
{ the snapshots of this thread's test file (GocciaScript's Goccia.Builtins.Testing.Snapshots): its host (the .snap
  file and the inline edits) and update mode, before InstallTestingAPI; nil leaves the matchers without snapshots.
  TestSnapshotsDone frees the file's state (its host is reference counted). }
procedure SetTestSnapshots(const Host: IGocciaSnapshotHost; Mode: TGocciaSnapshotUpdateMode);
procedure TestSnapshotsDone;
threadvar
  { --test-timeout: each test's own deadline in milliseconds (0: none), before InstallTestingAPI }
  TestTimeoutMs: Int64;
  { --describe-timeout: each describe callback's deadline in milliseconds (0: none) }
  DescribeTimeoutMs: Int64;
  { the testing library prints its per-test lines (a failure's, a skipped or todo test's) on stdout, as
    GocciaTestRunner's reporter does outside a JSON run }
  TestReporterOn: Boolean;

type
  { one benchmark's result as prelude/microbench.js measured it (Goccia.Builtins.Benchmark's TBenchmarkResult), or
    the error that stopped it }
  TGocciaBenchEntry = record
    Suite, Name: UnicodeString;
    { a failed benchmark's thrown value as Goccia.Embed describes an uncaught one (TGocciaRunResult's fields): its text, and
      for an error its name, message, where it was made and the host's suggestion }
    Failed, ErrorIsError: Boolean;
    Error, ErrorName, ErrorText, ErrorFile, ErrorSuggestion: UnicodeString;
    ErrorLine, ErrorColumn: Integer;
    SummaryScope, BoxplotScope: Integer;
    Iterations: Int64;
    OpsPerSec, MeanMs, VariancePercentage, SetupMs, TeardownMs, MinOpsPerSec, MaxOpsPerSec: Double;
    SampleCount: Integer;
    MinSampleMs, P25Ms, MedianMs, P75Ms, P99Ms, P999Ms, MaxSampleMs: Double;
  end;
  TGocciaBenchEntries = array of TGocciaBenchEntry;
  { the progress line of the benchmark about to be measured (its 1-based index of the total) }
  TGocciaBenchProgress = procedure(const Suite, Name: UnicodeString; Index, Total: Integer);

{ the benchmark API (bench, group, run, summary, boxplot) as this thread's goccia:microbench module; the measurement
  settings from GOCCIA_BENCH_WARMUP, GOCCIA_BENCH_CALIBRATION_MS, GOCCIA_BENCH_CALIBRATION_BATCH and
  GOCCIA_BENCH_ROUNDS, as GocciaBenchmarkRunner reads them }
function InstallBenchmarkAPI(var Err: UnicodeString): Boolean;
{ measures the benchmarks the file registered (after InstallBenchmarkAPI), once each when Deterministic, and runs
  the jobs they queue: each one's result in Entries, their count and the run's duration, or False and Err the error
  that stopped them. A run the script made itself with run() of the same kind is reused. }
function RunBenchmarkSuites(Deterministic: Boolean; out Entries: TGocciaBenchEntries; out Total: Integer;
  out DurationNanoseconds: Int64; var Err: UnicodeString): Boolean;

threadvar
  { the progress line before each benchmark (nil: none), set before InstallBenchmarkAPI }
  BenchProgress: TGocciaBenchProgress;
  { Goccia.Embed's description of a benchmark's thrown value (set by RunBenchmarks) }
  BenchDescribeThrown: procedure(V: TValue; var E: TGocciaBenchEntry);

{ the run's module resolver (an import map, aliases, node_modules, provider packages), else the plain one }
function CurrentResolver: TModuleResolver;

implementation

uses
  Math, MemoryDetection, Generics.Collections, Goccia.Artifact, Goccia.Builtins, Goccia.Instructions, Goccia.VM, Goccia.Typed, Goccia.Modules, Goccia.Snapshot, Goccia.Promise, Goccia.Atomics, Goccia.Eval, Goccia.Weak, Goccia.HeapSnapshot,
  Goccia.Spec.Data, Goccia.Version, Goccia.Platform, Goccia.URL.Parser, Goccia.SourcePipeline, Goccia.Vitest.MockHoist,
  Goccia.AST.Report, Goccia.FileExtensions, TextSemantics, TextEncoding, Goccia.HostSemver, Goccia.DataTree,
  Goccia.YAML, Goccia.TOML, Goccia.URI, Goccia.NativeLimits, Goccia.Modules.Virtual, Goccia.Packages.Address, Goccia.Error.Suggestions,
  Goccia.Fetch, Goccia.Sandbox, TimingUtils, Goccia.ThreadCleanupRegistry, ConsoleStreams, Goccia.EngineFault;

{ FPC 3.3.1 writes a call of a procedural threadvar as a direct call of the variable in its GNU assembler output
  (`call U_$...`, for the AFL build, -Aas): called through a local copy, it is called through a register }
procedure CallBenchProgress(const Suite, Name: UnicodeString; Index, Total: Integer);
var H: TGocciaBenchProgress;
begin
  H := BenchProgress;
  H(Suite, Name, Index, Total);
end;

procedure CallBenchDescribeThrown(V: TValue; var E: TGocciaBenchEntry);
var H: procedure(V: TValue; var E: TGocciaBenchEntry);
begin
  H := BenchDescribeThrown;
  H(V, E);
end;


{$I Goccia.Testing.inc}
{$I Goccia.Microbench.inc}
{$I Goccia.HostPrelude.inc}
{$I Goccia.UrlPrelude.inc}
{$I Goccia.WebPrelude.inc}
{$I Goccia.ModulesPrelude.inc}
{$I Goccia.ConsolePrelude.inc}
{$I Goccia.FetchPrelude.inc}
{$I Goccia.Json5Prelude.inc}
{$I Goccia.DelimitedPrelude.inc}
{$I Goccia.JsonlPrelude.inc}
{$I Goccia.SemverPrelude.inc}
{$I Goccia.AsyncHooksPrelude.inc}
{$I Goccia.EventsPrelude.inc}
{$I Goccia.TimersPrelude.inc}
{$I Goccia.DataTreePrelude.inc}
{$I Goccia.SandboxPrelude.inc}

type
  { Goccia.Modules's resolution as the hoister's resolver }
  TMockResolver = class
    function Resolve(const ASpecifier, AReferrer: string; out AAddress: string): Boolean;
  end;

function TMockResolver.Resolve(const ASpecifier, AReferrer: string; out AAddress: string): Boolean;
begin
  AAddress := string(ResolveModuleSpecifier(UnicodeString(ASpecifier), UnicodeString(AReferrer)));
  Result := AAddress <> '';
end;

threadvar
  { the vi.mock modules: address, then source }
  MockModules: TStringList;
  SharedV, LoadHostModuleV, FindHostModuleV: TValue;

  FireNextV, DiscardTimersV, TimerQueue, TimerHostTasks: TValue;
  EventLoopRootedGen: Integer;
  TestApiV: TValue;
  TestApiRootedGen: Integer;
  ReportJson, ReportErr: UnicodeString;
  ReportDone: Boolean;

function JsonStr(const S: UnicodeString): UnicodeString;
var I: Integer; C: WideChar;
begin
  Result := '"';
  for I := 1 to Length(S) do
  begin
    C := S[I];
    case C of
      '"': Result := Result + '\"';
      '\': Result := Result + '\\';
      #10: Result := Result + '\n';
      #13: Result := Result + '\r';
      #9: Result := Result + '\t';
    else
      if Ord(C) < 32 then Result := Result + '\u' + UnicodeString(IntToHex(Ord(C), 4)) else Result := Result + C;
    end;
  end;
  Result := Result + '"';
end;

function GroupsJson(const Groups: TGocciaFeatureGroups): UnicodeString;
var I, J: Integer;
begin
  Result := '[';
  for I := 0 to High(Groups) do
  begin
    if I > 0 then Result := Result + ',';
    Result := Result + '{"key":' + JsonStr(UnicodeString(Groups[I].Key)) + ',"entries":[';
    for J := 0 to High(Groups[I].Entries) do
    begin
      if J > 0 then Result := Result + ',';
      Result := Result + '{"name":' + JsonStr(UTF8Decode(Groups[I].Entries[J].Name)) + ',"link":' +
        JsonStr(UTF8Decode(Groups[I].Entries[J].Link)) + '}';
    end;
    Result := Result + ']}';
  end;
  Result := Result + ']';
end;

{ the build facts host.js reads: Goccia.version, commit, build, spec and proposal }
function InfoJson: UnicodeString;

  { not BoolToStr(B, True): the RTL fills its shared TrueBoolStrs and FalseBoolStrs on first use, unguarded, so
    engines set up at once on two threads read an empty string or a half-grown array there }
  function GCStressText: UnicodeString;
  begin
    if IsGCStressBuild then Result := 'true' else Result := 'false';
  end;

begin
  Result := '{"version":' + JsonStr(UnicodeString(GetVersion)) + ',"commit":' + JsonStr(UnicodeString(GetCommit)) +
    ',"date":' + JsonStr(UnicodeString(GetBuildDate)) + ',"os":' + JsonStr(UnicodeString(GetBuildOS)) +
    ',"arch":' + JsonStr(UnicodeString(GetBuildArch)) + ',"gcStress":' + GCStressText +
    ',"spec":' + GroupsJson(SpecFeatureGroups) + ',"proposal":' + GroupsJson(ProposalFeatureGroups) + '}';
end;

{ Goccia.gc(): a full collection, the nursery first }
function Host_GC(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
begin
  CollectMinor;
  CollectMajor;
  Res := VUndefined;
  Result := True;
end;

{ Goccia.gc.bytesAllocated: the old generation and the nursery's used part }
function Host_GCBytes(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
begin
  Res := MkNumber(GCOldBytes + (NurseryTop - NurseryStart));
  Result := True;
end;

{ Goccia.gc.suggestedMaxBytes, and the ceiling without --max-memory or goccia.json max-memory: half of the memory
  available to the process (a Linux cgroup's limit, else physical memory; MemoryDetection), at most 8 GB (700 MB on a
  32-bit build) (docs/built-ins.md) }
function DefaultHeapCeiling: Int64;
const Cap = {$IFDEF CPU64}Int64(8) * 1024 * 1024 * 1024{$ELSE}Int64(700) * 1024 * 1024{$ENDIF};
begin
  Result := GetAvailableMemoryBytes div 2;
  if (Result <= 0) or (Result > Cap) then Result := Cap;
end;

function Host_GCSuggestedMaxBytes(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
begin
  Res := MkNumber(DefaultHeapCeiling);
  Result := True;
end;

{ Goccia.gc.maxBytes: the ceiling in force (HeapCeiling) }
function Host_GCMaxBytes(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
begin
  if HeapCeiling <= 0 then HeapCeiling := DefaultHeapCeiling;
  Res := MkNumber(HeapCeiling);
  Result := True;
end;

{ ---- URL (Goccia.URL.Parser) ---- }

function ArgStr(Args: PValue; Argc, I: Integer): UnicodeString;
begin
  if (I <= Argc) and IsString(Args[I]) then Result := StrToUnicode(StrOf(Args[I])) else Result := '';
end;

function U8(const S: UnicodeString): RawByteString;
begin
  Result := UTF8Encode(S);
end;

function FromU8(const S: RawByteString): TValue;
begin
  Result := StrFromUnicode(UTF8Decode(S));
end;

{ the array at the temp root Slot, with V pushed (V rooted by the caller or a primitive) }
procedure PushAt(Slot: Integer; V: TValue);
begin
  PushRoot(V);
  ArrayPush(ObjOf(RootAt(Slot)), RootAt(TempTop - 1));
  PopRoots(1);
end;

{ the URL record as url.js reads it }
function RecordValue(const R: TGocciaURLRecord): TValue;
var Slot: Integer;
begin
  PushRoot(MkPtr(NewArray(0, 12)));
  Slot := TempTop - 1;
  PushAt(Slot, FromU8(R.Scheme));
  PushAt(Slot, FromU8(R.Username));
  PushAt(Slot, FromU8(R.Password));
  PushAt(Slot, FromU8(R.Host));
  PushAt(Slot, MkBool(R.HasNullHost));
  PushAt(Slot, MkInt(R.Port));
  PushAt(Slot, FromU8(SerializePath(R)));
  PushAt(Slot, MkBool(R.HasOpaquePath));
  PushAt(Slot, FromU8(R.Query));
  PushAt(Slot, MkBool(R.HasNullQuery));
  PushAt(Slot, FromU8(R.Fragment));
  PushAt(Slot, MkBool(R.HasNullFragment));
  Result := RootAt(Slot);
  PopRoots(1);
end;

{ urlParse(input[, base]): the record, null when input does not parse, -1 when base does not }
function Host_URLParse(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var R, B: TGocciaURLRecord;
begin
  Result := True;
  if Argc >= 2 then
  begin
    B := ParseURL(U8(ArgStr(Args, Argc, 2)));
    if not B.IsValid then begin Res := MkInt(-1); Exit; end;
    R := ParseURLWithBase(U8(ArgStr(Args, Argc, 1)), B);
  end
  else R := ParseURL(U8(ArgStr(Args, Argc, 1)));
  if R.IsValid then Res := RecordValue(R) else Res := VNull;
end;

{ urlOrigin(scheme, host, hasNullHost, port) }
function Host_URLOrigin(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var R: TGocciaURLRecord;
begin
  R := Default(TGocciaURLRecord);
  R.Scheme := U8(ArgStr(Args, Argc, 1));
  R.Host := U8(ArgStr(Args, Argc, 2));
  R.HasNullHost := (Argc >= 3) and (Args[3] = VTrue);
  if Argc >= 4 then R.Port := Round(PrimitiveToNumber(Args[4])) else R.Port := URL_NULL_PORT;
  Res := FromU8(SerializeOrigin(R));
  Result := True;
end;

{ urlEncode(kind, input, special): kind 0 userinfo, 1 query, 2 fragment, 3 path }
function Host_URLEncode(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var S: RawByteString;
begin
  S := U8(ArgStr(Args, Argc, 2));
  case Round(PrimitiveToNumber(Args[1])) of
    0: S := PercentEncodeForUserinfo(S);
    1: S := PercentEncodeForQuery(S, (Argc >= 3) and (Args[3] = VTrue));
    2: S := PercentEncodeForFragment(S);
  else
    S := PercentEncodeForPath(S);
  end;
  Res := FromU8(S);
  Result := True;
end;

function Host_URLIsSpecial(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
begin
  Res := MkBool(IsSpecialScheme(U8(ArgStr(Args, Argc, 1))));
  Result := True;
end;

function Host_URLDefaultPort(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
begin
  Res := MkInt(DefaultPortForScheme(U8(ArgStr(Args, Argc, 1))));
  Result := True;
end;

{ formParse(input): names and values interleaved }
function Host_FormParse(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var Names, Values: TArray<string>; I, Slot: Integer;
begin
  ParseFormEncoded(U8(ArgStr(Args, Argc, 1)), Names, Values);
  PushRoot(MkPtr(NewArray(0, 2 * Length(Names))));
  Slot := TempTop - 1;
  for I := 0 to High(Names) do
  begin
    PushAt(Slot, FromU8(Names[I]));
    PushAt(Slot, FromU8(Values[I]));
  end;
  Res := RootAt(Slot);
  PopRoots(1);
  Result := True;
end;

function Host_FormSerialize(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
begin
  Res := FromU8(SerializeFormEncoded(U8(ArgStr(Args, Argc, 1)), U8(ArgStr(Args, Argc, 2))));
  Result := True;
end;

{ plainCloneable(v): an ordinary object structuredClone copies property by property (a plain object, an array or
  an unmapped arguments object), not an exotic or platform object it cannot serialize }
function Host_PlainCloneable(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
begin
  Res := MkBool((Argc >= 1) and IsObject(Args[1]) and (ObjOf(Args[1])^.OKind in [okPlain, okArray, okArgumentsOrd]));
  Result := True;
end;

{ viewInfo(view): [length-tracking, out of bounds] of a typed array or a DataView (25.3.1.3 IsViewOutOfBounds:
  the DataView getters answer +0 for one out of bounds, so its clone asks here) }
function Host_ViewInfo(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var O, B: PObj; Tracking, OOB: Boolean; BL: Int64;
begin
  Tracking := False; OOB := False;
  if (Argc >= 1) and IsObject(Args[1]) then
  begin
    O := ObjOf(Args[1]);
    if O^.OKind = okTypedArray then
    begin
      Tracking := O^.Aux2 = MkInt(-1);
      OOB := TAOutOfBounds(O);
    end
    else if O^.OKind = okDataView then
    begin
      Tracking := O^.Aux2 = MkInt(-1);
      B := ObjOf(O^.Aux1);
      if BufferDetached(B) then OOB := True
      else
      begin
        BL := BufferLen(B);
        if Tracking then OOB := O^.Pad > BL else OOB := O^.Pad + IntOf(O^.Aux2) > BL;
      end;
    end;
  end;
  PushRoot(MkPtr(NewArray(0, 2)));
  PushAt(TempTop - 1, MkBool(Tracking));
  PushAt(TempTop - 1, MkBool(OOB));
  Res := RootAt(TempTop - 1);
  PopRoots(1);
  Result := True;
end;

{ the UTF-8 bytes of N code units at P, as TextEncoder encodes them (a lone surrogate: U+FFFD's three) }
function Utf8Count(P: PWideChar; N: Integer): Int64;
var I: Integer; C: Word;
begin
  Result := 0;
  I := 0;
  while I < N do
  begin
    C := Word(P[I]);
    if C < $80 then Inc(Result)
    else if C < $800 then Inc(Result, 2)
    else if (C >= $D800) and (C <= $DBFF) and (I + 1 < N) and (Word(P[I + 1]) >= $DC00) and
      (Word(P[I + 1]) <= $DFFF) then
    begin
      Inc(Result, 4);
      Inc(I);
    end
    else Inc(Result, 3);
    Inc(I);
  end;
end;

{ utf8Length(s): s's bytes in UTF-8 }
function Host_Utf8Length(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var S: PStr;
begin
  if (Argc < 1) or not IsString(Args[1]) then Exit(ThrowTypeError('utf8Length: the argument is not a string'));
  S := StrOf(Args[1]);
  Res := MkNumber(Utf8Count(StrChars(S), S^.Len));
  Result := True;
end;

{ utf8Write(s, bytes, at): s in UTF-8 into the Uint8Array bytes from index at, whole characters while they fit
  (WHATWG Encoding's encodeInto); [the code units read, the bytes written] }
function Host_Utf8Write(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var S: PStr; O: PObj; P: PWideChar; D: PByte; N, I, Units, K: Integer; Room, W, At: Int64; C: Cardinal;
begin
  if (Argc < 3) or not IsString(Args[1]) or not IsObject(Args[2]) or (ObjOf(Args[2])^.OKind <> okTypedArray) or
     (TAElemSize[TAKind(ObjOf(Args[2]))] <> 1) or not IsNumber(Args[3]) then
    Exit(ThrowTypeError('utf8Write: the arguments are not a string, a byte array and an index'));
  S := StrOf(Args[1]);
  O := ObjOf(Args[2]);
  { Web IDL's Uint8Array conversion refuses a view of an immutable ArrayBuffer }
  if ViewImmutable(O) then Exit(ThrowTypeError('Cannot write to a view of an immutable ArrayBuffer'));
  At := Trunc(NumOf(Args[3]));
  if TAOutOfBounds(O) or (At < 0) or (At > TALength(O)) then Room := 0 else Room := TALength(O) - At;
  P := StrChars(S);
  N := S^.Len;
  W := 0;
  I := 0;
  if Room > 0 then
  begin
    D := TAData(O) + At;
    while I < N do
    begin
      C := Word(P[I]);
      Units := 1;
      if (C >= $D800) and (C <= $DBFF) and (I + 1 < N) and (Word(P[I + 1]) >= $DC00) and (Word(P[I + 1]) <= $DFFF) then
      begin
        C := $10000 + ((C - $D800) shl 10) + (Word(P[I + 1]) - $DC00);
        Units := 2;
      end
      else if (C >= $D800) and (C <= $DFFF) then C := $FFFD;
      if C < $80 then K := 1 else if C < $800 then K := 2 else if C < $10000 then K := 3 else K := 4;
      if W + K > Room then Break;
      case K of
        1: D[W] := C;
        2: begin D[W] := $C0 or (C shr 6); D[W + 1] := $80 or (C and 63); end;
        3:
          begin
            D[W] := $E0 or (C shr 12); D[W + 1] := $80 or ((C shr 6) and 63); D[W + 2] := $80 or (C and 63);
          end;
      else
        begin
          D[W] := $F0 or (C shr 18); D[W + 1] := $80 or ((C shr 12) and 63); D[W + 2] := $80 or ((C shr 6) and 63);
          D[W + 3] := $80 or (C and 63);
        end;
      end;
      Inc(W, K);
      Inc(I, Units);
    end;
  end;
  PushRoot(MkPtr(NewArray(0, 2)));
  PushAt(TempTop - 1, MkNumber(I));
  PushAt(TempTop - 1, MkNumber(W));
  Res := RootAt(TempTop - 1);
  PopRoots(1);
  Result := True;
end;

{ isBytecode(f): f is a compiled JS function (a prelude's or a script's), not a native one }
function Host_IsBytecode(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
begin
  Res := MkBool((Argc >= 1) and IsObject(Args[1]) and (ObjOf(Args[1])^.OKind = okFunction));
  Result := True;
end;

{ writeLine(text, method): text and a line break on the method's stream (ConsoleStreams), or to the host's sink }
threadvar
  ConsoleSink: TConsoleSink;

procedure SetConsoleSink(Sink: TConsoleSink);
begin
  ConsoleSink := Sink;
end;

{ writeLine(line, method) }
function Host_WriteLine(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var Sink: TConsoleSink;
begin
  Sink := ConsoleSink;
  if Assigned(Sink) then Sink(ArgStr(Args, Argc, 2), ArgStr(Args, Argc, 1))
  else   { the output's encoder writes an unpaired surrogate as U+FFFD }
    WriteConsoleLine(ConsoleMethodWritesToStderr(string(ArgStr(Args, Argc, 2))), ArgStr(Args, Argc, 1));
  Res := VUndefined;
  Result := True;
end;

{ hrNow(): a monotonic time in milliseconds (an event's timeStamp) }
function Host_HrNow(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
begin
  Res := MkNumber(GetTickCount64);
  Result := True;
end;

type
  TNativeModuleFactories = TDictionary<UnicodeString, TNativeModuleFactory>;
  TModuleProviders = TDictionary<UnicodeString, TModuleProvider>;

threadvar
  NativeModuleFactories: TNativeModuleFactories;
  { the modules embedders supply on first import (DefineModuleProvider): only the callback until then }
  ModuleProviders: TModuleProviders;
  { realm and key of each native factory still running: a load that reaches the same module again before the
    factory returned is refused, because its record is published only after the factory succeeds }
  ActiveNativeFactories: TList<UnicodeString>;

{ a native factory's or a module provider's key: a host module with no source phase (a provider's answer is not
  asked for a source or typed lookup) }
function NativeModuleRegistered(const Key: UnicodeString): Boolean;
var Factories: TNativeModuleFactories; Providers: TModuleProviders;
begin
  Factories := NativeModuleFactories;
  Providers := ModuleProviders;
  Result := ((Factories <> nil) and Factories.ContainsKey(Key)) or ((Providers <> nil) and Providers.ContainsKey(Key));
end;

function NativeModuleWithoutSource(const Key: UnicodeString): Boolean;
begin
  Result := NativeModuleRegistered(Key) and ((MockModules = nil) or (MockModules.IndexOfName(string(Key)) < 0));
end;

{ Goccia.Modules's host hook: native factories and shared.loadHostModule(Key, FilePath) }
function HostModule(const Key, FilePath: UnicodeString; SourceOnly: Boolean; var Exp: TValue): Integer;
var A: array[0..1] of TValue; I: Integer; Factory: TNativeModuleFactory; Factories: TNativeModuleFactories;
  Marker: UnicodeString; Provider: TModuleProvider; Providers: TModuleProviders; Answer: TModuleProviderAnswer;
  Bytes: RawByteString;
begin
  if (FilePath = '') and Assigned(MockModules) then
  begin
    I := MockModules.IndexOfName(string(Key));
    if I >= 0 then
    begin
      Exp := StrFromUnicode(UnicodeString(MockModules.ValueFromIndex[I]));
      Exit(2);
    end;
  end;
  Factories := NativeModuleFactories;
  if (FilePath = '') and (Factories <> nil) and Factories.TryGetValue(Key, Factory) then
  begin
    if SourceOnly then Exit(0);
    Marker := IntToStr(CurRealm) + #0 + Key;
    if ActiveNativeFactories = nil then ActiveNativeFactories := TList<UnicodeString>.Create;
    if ActiveNativeFactories.IndexOf(Marker) >= 0 then
    begin
      ThrowTypeError('The native module ' + Key + ' is still being created');
      Exit(-1);
    end;
    ActiveNativeFactories.Add(Marker);
    try
      Exp := VUndefined;
      if Factory(Exp) then Result := 1
      else
      begin
        if Pending = VUndefined then
          ThrowTypeError('The native module factory for ' + Key + ' failed without an error');
        Result := -1;
      end;
    finally
      I := ActiveNativeFactories.IndexOf(Marker);
      if I >= 0 then ActiveNativeFactories.Delete(I);
    end;
    Exit;
  end;
  Providers := ModuleProviders;
  if (FilePath = '') and (Providers <> nil) and Providers.TryGetValue(Key, Provider) then
  begin
    { a typed or source lookup does not ask the provider (NativeModuleWithoutSource) }
    if SourceOnly then Exit(0);
    { a load that reaches the same module in the same realm while its provider runs is refused, as a native
      factory's is: the record is published only after the answer }
    Marker := IntToStr(CurRealm) + #0 + Key;
    if ActiveNativeFactories = nil then ActiveNativeFactories := TList<UnicodeString>.Create;
    if ActiveNativeFactories.IndexOf(Marker) >= 0 then
    begin
      ThrowTypeError('The provided module ' + Key + ' is still being supplied');
      Exit(-1);
    end;
    ActiveNativeFactories.Add(Marker);
    try
      Answer := Default(TModuleProviderAnswer);
      if not Provider(Key, Answer) then
      begin
        if Pending = VUndefined then
          ThrowTypeError('The module provider for ' + Key + ' failed without an error');
        Exit(-1);
      end;
    finally
      I := ActiveNativeFactories.IndexOf(Marker);
      if I >= 0 then ActiveNativeFactories.Delete(I);
    end;
    if Answer.Kind = mpaBytecode then
    begin
      { the verifier's bound, before the bytes are copied for it }
      if Length(Answer.Bytecode) > GOCCIA_GBC_MAX_FILE_BYTES then
      begin
        SetHostModuleArtifactRefusal(UnicodeString(Format('bytecode file exceeds the %d byte limit',
          [GOCCIA_GBC_MAX_FILE_BYTES])));
        Exp := VUndefined;
        Exit(HOST_MODULE_BYTECODE);
      end;
      SetLength(Bytes, Length(Answer.Bytecode));
      if Length(Bytes) > 0 then Move(Answer.Bytecode[0], Bytes[1], Length(Bytes));
      Answer.Bytecode := nil;
      SetHostModuleArtifact(Bytes);
      Exp := VUndefined;
      Exit(HOST_MODULE_BYTECODE);
    end;
    Exp := StrFromUnicode(Answer.Source);
    Exit(2);
  end;
  if not IsObject(LoadHostModuleV) then Exit(0);
  PushRoot(StrFromUnicode(Key));
  A[1] := StrFromUnicode(FilePath);
  A[0] := RootAt(TempTop - 1);
  PopRoots(1);
  if not CallValue(LoadHostModuleV, SharedV, @A[0], 2, Exp) then Exit(-1);
  if Exp = VUndefined then Result := 0
  else if IsString(Exp) then Result := 2
  else Result := 1;
end;

{ Goccia.Modules's resolution hook: the host's own modules by their keys, a mock by its address, anything else by the
  CLIs' resolver (Goccia.ModuleResolver); each engine thread's own }
threadvar
  HostResolver: TModuleResolver;
  { a host's resolver, in place of HostResolver }
  ProvidedResolver: TModuleResolver;
  ResolveCandidate: UnicodeString;
  ImportSpecifiers: TStringList;   { module key = the first specifier that resolved to it }

procedure SetHostResolver(R: TModuleResolver);
begin
  ProvidedResolver := R;
end;

threadvar
  { the virtual modules whose shadowing of a file was warned about }
  ShadowWarned: TStringList;

threadvar
  EmbedderWork: TEmbedderWork;

function EmbedderWorkPending: Boolean;
var Work: TEmbedderWork;
begin
  Work := EmbedderWork;
  Result := (Work <> nil) and Work.HasPending;
end;

procedure DiscardEmbedderWork;
var Work: TEmbedderWork;
begin
  Work := EmbedderWork;
  if (Work <> nil) and Work.HasPending then Work.Discard;
end;

procedure SetEmbedderWork(Work: TEmbedderWork);
begin
  if EmbedderWork = Work then Exit;
  DiscardEmbedderWork;
  EmbedderWork := Work;
end;

procedure HostModulesDone;
begin
  SetEmbedderWork(nil);
  RealmHostInit := nil;
  FreeAndNil(NativeModuleFactories);
  FreeAndNil(ModuleProviders);
  FreeAndNil(ActiveNativeFactories);
  FreeAndNil(ShadowWarned);
  FreeAndNil(HostResolver);
  FreeAndNil(ImportSpecifiers);
  ProvidedResolver := nil;
  ResolveCandidate := '';
end;

function TakeResolveCandidate: UnicodeString;
begin
  Result := ResolveCandidate;
  ResolveCandidate := '';
end;

{ the pending exception's message (an error's message property, else its string form), taken }
threadvar
  HostErrorV: TValue;
  HostErrorRootedGen: Integer;
  { HostErrorV holds a thrown value (which may be undefined) }
  HostErrorHeld: Boolean;

function TakeHostError: TValue;
begin
  HostErrorHeld := False;
  if not SameEngine(HostErrorRootedGen) then Exit(VUndefined);
  Result := HostErrorV;
  HostErrorV := VUndefined;
end;

{ the pending exception's string form (Error: message), taken; the error itself stays for the host, as
  TakePendingText's does }
function TakePendingString: UnicodeString;
var S: TValue;
begin
  if FirstInEngine(HostErrorRootedGen) then AddRoot(@HostErrorV);
  HostErrorV := Pending;
  HostErrorHeld := True;
  Pending := VUndefined;
  if ToStringV(HostErrorV, S) and IsString(S) then Result := StrToUnicode(StrOf(S))
  else Result := 'uncaught exception';
  Pending := VUndefined;
end;

function TakePendingText: UnicodeString;
var S: TValue;
begin
  { the error itself stays for the host to describe (TakeHostError) }
  if FirstInEngine(HostErrorRootedGen) then AddRoot(@HostErrorV);
  HostErrorV := Pending;
  HostErrorHeld := True;
  PushRoot(Pending);
  Pending := VUndefined;
  if IsObject(RootAt(TempTop - 1)) and GetV(RootAt(TempTop - 1), AtomMessage, S) and IsString(S) then
    Result := StrToUnicode(StrOf(S))
  else if ToStringV(RootAt(TempTop - 1), S) and IsString(S) then Result := StrToUnicode(StrOf(S))
  else Result := 'uncaught exception';
  Pending := VUndefined;
  PopRoots(1);
end;

{ the parse function of a data format: JSON.parse, or the parse of goccia:json5, goccia:toml or goccia:yaml }
function DataParser(const Ext: UnicodeString; var F: TValue): Boolean;
var Key: UnicodeString; Mods, Factory, M: TValue;
begin
  Result := False;
  Key := LowerCase(Ext);
  if Key = '.json' then
  begin
    if not GetV(GlobalObj, AtomOf('JSON'), M) or not IsObject(M) then Exit;
    Result := GetV(M, AtomOf('parse'), F) and IsCallable(F);
    Exit;
  end;
  if (Key = '.json5') or (Key = '.jsonc') then Key := 'goccia:json5'
  else if Key = '.toml' then Key := 'goccia:toml'
  else if (Key = '.yaml') or (Key = '.yml') then Key := 'goccia:yaml'
  else Exit;
  if not IsObject(SharedV) or not GetV(SharedV, AtomOf('hostModules'), Mods) or not IsObject(Mods) or
     not GetV(Mods, AtomOf(Key), Factory) or not IsCallable(Factory) then Exit;
  if not CallValue(Factory, VUndefined, nil, 0, M) or not IsObject(M) then Exit;
  Result := GetV(M, AtomOf('parse'), F) and IsCallable(F);
end;

{ Text parsed as a data file with extension Ext }
function DataValue(const Text, Ext: UnicodeString; var V: TValue): Boolean;
var F, A: TValue;
begin
  if not DataParser(Ext, F) then
  begin
    Pending := VUndefined;
    Exit(ThrowTypeError('Cannot read globals from a ' + Ext + ' file'));
  end;
  PushRoot(F);
  A := StrFromUnicode(Text);
  Result := CallValue(RootAt(TempTop - 1), VUndefined, @A, 1, V);
  PopRoots(1);
end;

function InjectDataGlobals(const Text, Ext, Path: UnicodeString; var Err: UnicodeString): Boolean;
var Obj, Keys, V: TValue; I, N: Integer; Name: UnicodeString;
begin
  Result := False;
  if not DataValue(Text, Ext, Obj) then begin Err := TakePendingText; Exit; end;
  if not IsObject(Obj) or (ObjOf(Obj)^.OKind = okArray) then
  begin
    Err := 'Globals file ' + Path + ' must contain an object';
    Exit;
  end;
  PushRoot(Obj);
  try
    Keys := OwnKeys(ObjOf(RootAt(TempTop - 1)), True);
    PushRoot(Keys);
    try
      N := ArrayLen(ObjOf(Keys));
      for I := 0 to N - 1 do
      begin
        Name := StrToUnicode(StrOf(ObjOf(RootAt(TempTop - 1))^.Elements^.Data[I]));
        if not GetV(RootAt(TempTop - 2), AtomOf(Name), V) or not InjectGlobalBinding(Name, V) then
        begin
          Err := TakePendingText;
          Exit;
        end;
      end;
      Result := True;
    finally
      PopRoots(1);
    end;
  finally
    PopRoots(1);
  end;
end;

function ReadFileUnicode(const Path: UnicodeString; var Text: UnicodeString): Boolean;
var B: RawByteString;
begin
  Result := ReadFileBytes(Path, B);
  if not Result then Exit;
  SetCodePage(B, CP_UTF8, False);
  Text := UTF8DecodeKeepingNoncharacters(B);
  if (Text <> '') and (Text[1] = #$FEFF) then Delete(Text, 1, 1);
end;

{ shared.defineVirtualModules(map, base) }
function DefineVirtualModuleMap(Map: TValue; const Base: UnicodeString; var Err: UnicodeString): Boolean;
var F, R: TValue; A: array[0..1] of TValue; Directory: UnicodeString;
begin
  Result := False;
  PushRoot(Map);
  try
    if not IsObject(SharedV) or not GetV(SharedV, AtomOf('defineVirtualModules'), F) or not IsCallable(F) then
    begin
      Err := 'virtual modules are not available';
      Exit;
    end;
    PushRoot(F);
    Directory := Base;
    { Resolve the declaring directory before folding '..' in a relative address. }
    if not SandboxActive and IsAbsoluteFilePath(Base) then
      Directory := UTF8Decode(CanonicalCapabilityPath(UTF8Encode(Base)));
    A[1] := StrFromUnicode(Directory);
    A[0] := RootAt(TempTop - 2);
    Result := CallValue(RootAt(TempTop - 1), VUndefined, @A[0], 2, R);
    PopRoots(1);
    if not Result then Err := TakePendingText;
  finally
    PopRoots(1);
  end;
end;

function DefineManifestVirtualModules(const ManifestPath: UnicodeString; var Err: UnicodeString): Boolean;
var Text: UnicodeString; V: TValue;
begin
  Result := False;
  if not ReadFileUnicode(ManifestPath, Text) then
  begin
    Err := 'Cannot read the module manifest ' + ManifestPath;
    Exit;
  end;
  if not DataValue(Text, UnicodeString(ExtractFileExt(UTF8Encode(ManifestPath))), V) then
  begin
    Err := TakePendingText;
    Exit;
  end;
  Result := DefineVirtualModuleMap(V, UnicodeString(ExtractFileDir(ExpandFileName(UTF8Encode(ManifestPath)))), Err);
end;

function DefineInlineVirtualModule(const Definition, Base: UnicodeString; var Err: UnicodeString): Boolean;
var Sep: Integer; Address, Content: UnicodeString; Map, D, Parsed: TValue;
begin
  Result := False;
  Sep := Pos('=', Definition);
  if Sep <= 1 then
  begin
    Err := 'Invalid --module value "' + Definition + '"; expected name=source or name={descriptor}.';
    Exit;
  end;
  Address := Copy(Definition, 1, Sep - 1);
  Content := Copy(Definition, Sep + 1, MaxInt);
  PushRoot(MkPtr(NewPlainObject(ObjectProto, 1)));
  try
    if (Content <> '') and (Content[1] = '{') then
    begin
      if not DataValue(Content, '.json', Parsed) then begin Err := TakePendingText; Exit; end;
      D := Parsed;
    end
    else
    begin
      PushRoot(MkPtr(NewPlainObject(ObjectProto, 1)));
      PushRoot(StrFromUnicode(Content));
      DefineOwn(RootAt(TempTop - 2), AtomOf('content'), RootAt(TempTop - 1), pfDefault);
      PopRoots(1);
      D := RootAt(TempTop - 1);
      PopRoots(1);
    end;
    Map := RootAt(TempTop - 1);
    DefineOwn(Map, AtomOf(Address), D, pfDefault);
    Result := DefineVirtualModuleMap(RootAt(TempTop - 1), Base, Err);
  finally
    PopRoots(1);
  end;
end;

threadvar
  GuestLoadsOn: Boolean;   { (GuestLoads, which is declared further down) }
  { the realm the host's globals are in (InstallHostGlobals) }
  HostRealm: Integer;
  { the host keeps escape and unescape (InstallHostGlobals' AnnexB) }
  HostAnnexB: Boolean;

{ a module the host evaluates for itself, or for a config (GuestLoadsOn) in a realm of its own: what it leaves on its
  global object does not reach the script's }
function EvaluateHostModule(const Path: UnicodeString; var NS: TValue): Boolean;
var R, Caller: Integer;
begin
  if not GuestLoadsOn then Exit(EvaluateModuleNamespace(Path, NS));
  if not CreateRealm(R) then Exit(False);
  Caller := CurRealm;
  SwitchRealm(R);
  try
    Result := EvaluateModuleNamespace(Path, NS);
  finally
    SwitchRealm(Caller);
  end;
end;

{ '' when V is data (what JSON carries as it is, at any depth), else what it is; At names where it sits }
function NotData(V: TValue; const At: UnicodeString; var Where: UnicodeString; SeenBase: Integer): UnicodeString;
var T: UnicodeString; O: PObj; P: TValue; Keys, E: TValue; I, N, Self_: Integer; D: Double;
begin
  Result := '';
  Where := At;
  T := TypeOfStr(V);
  if T = 'undefined' then Exit('undefined');
  if T = 'function' then Exit('a function');
  if T = 'bigint' then Exit('a BigInt');
  if T = 'symbol' then Exit('a symbol');
  if T = 'number' then
  begin
    D := NumOf(V);
    if IsNaN(D) or IsInfinite(D) then Exit('a number JSON cannot hold');
    Exit;
  end;
  if not IsObject(V) then Exit;
  for I := SeenBase to TempTop - 1 do
    if RootAt(I) = V then Exit('a circular reference');
  O := ObjOf(V);
  if O^.OKind = okArray then
  begin
    PushRoot(V);
    Self_ := TempTop - 1;
    try
      N := ArrayLen(ObjOf(RootAt(Self_)));
      for I := 0 to N - 1 do
      begin
        if not GetElemV(RootAt(Self_), MkInt(I), E) then begin Pending := VUndefined; Exit('a value that cannot be read'); end;
        Result := NotData(E, At + '[' + UnicodeString(IntToStr(I)) + ']', Where, SeenBase);
        if Result <> '' then Exit;
      end;
    finally
      PopRoots(TempTop - Self_);
    end;
    Exit;
  end;
  P := ProtoOf(O);
  if (O^.OKind <> okPlain) or (IsObject(P) and IsObject(ProtoOf(ObjOf(P)))) then
    Exit('an object that is not a plain object or array');
  PushRoot(V);
  Self_ := TempTop - 1;
  try
    Keys := OwnKeys(ObjOf(RootAt(Self_)), False);
    PushRoot(Keys);
    N := ArrayLen(ObjOf(Keys));
    for I := 0 to N - 1 do
    begin
      E := ObjOf(RootAt(Self_ + 1))^.Elements^.Data[I];
      if not IsString(E) then Continue;
      T := StrToUnicode(StrOf(E));
      if not GetV(RootAt(Self_), AtomOf(T), E) then begin Pending := VUndefined; Exit('a value that cannot be read'); end;
      Result := NotData(E, At + '.' + T, Where, SeenBase);
      if Result <> '' then Exit;
    end;
  finally
    PopRoots(TempTop - Self_);
  end;
end;

function InjectModuleGlobals(const Path: UnicodeString; var Err: UnicodeString): Boolean;
var NS, Keys, V: TValue; I, N: Integer; Name, What, Where: UnicodeString;
begin
  Result := False;
  if not EvaluateHostModule(UnicodeString(ExpandFileName(UTF8Encode(Path))), NS) then
  begin
    Err := TakePendingText;
    Exit;
  end;
  PushRoot(NS);
  try
    Keys := OwnKeys(ObjOf(RootAt(TempTop - 1)), True);
    PushRoot(Keys);
    try
      N := ArrayLen(ObjOf(Keys));
      for I := 0 to N - 1 do
      begin
        Name := StrToUnicode(StrOf(ObjOf(RootAt(TempTop - 1))^.Elements^.Data[I]));
        if Name = 'default' then Continue;
        { code does not cross from a config's globals module: it exports data only }
        if GuestLoadsOn then
        begin
          if not GetV(RootAt(TempTop - 2), AtomOf(Name), V) then begin Err := TakePendingText; Exit; end;
          What := NotData(V, Name, Where, TempTop);
          if What <> '' then
          begin
            HostErrorV := VUndefined;
            HostErrorHeld := False;
            Err := 'export "' + Where + '" is ' + What + '; a config''s globals module may export data only';
            Exit;
          end;
        end;
        if not GetV(RootAt(TempTop - 2), AtomOf(Name), V) or not InjectGlobalBinding(Name, V) then
        begin
          Err := TakePendingText;
          Exit;
        end;
      end;
      Result := True;
    finally
      PopRoots(1);
    end;
  finally
    PopRoots(1);
  end;
end;

function DefineModuleManifestVirtualModules(const Path: UnicodeString; var Err: UnicodeString): Boolean;
var NS, Map: TValue; Abs: UnicodeString;
begin
  Result := False;
  Abs := UnicodeString(ExpandFileName(UTF8Encode(Path)));
  if not EvaluateHostModule(Abs, NS) or not GetV(NS, AtomOf('default'), Map) then
  begin
    Err := TakePendingText;
    Exit;
  end;
  Result := DefineVirtualModuleMap(Map, UnicodeString(ExtractFileDir(UTF8Encode(Abs))), Err);
end;

{ a config's manifest: a JavaScript or TypeScript module's default export, or a data file's map }
threadvar
  { a config's manifest as the config wrote it, and the path it names: a refusal names it as written }
  ManifestWritten, ManifestPath: UnicodeString;
  { the directory of the config whose loads are being judged: what it names inside its own tree needs no grant }
  GuestScopeDir: string;

procedure SetGuestScope(const Dir: string);
begin
  if Dir = '' then GuestScopeDir := '' else GuestScopeDir := CanonicalCapabilityPath(Dir);
end;

function DefineManifestPath(const P, Written: UnicodeString; var Err: UnicodeString): Boolean;
var Ext: string;
begin
  Ext := LowerCase(ExtractFileExt(UTF8Encode(P)));
  { spelled as the loader is asked for it (DefineModuleManifestVirtualModules expands it): a config joins a relative
    manifest with '/', which Windows spells '\' once expanded }
  ManifestPath := UnicodeString(ExpandFileName(UTF8Encode(P)));
  ManifestWritten := Written;
  try
    if (Ext = '.js') or (Ext = '.mjs') or (Ext = '.ts') or (Ext = '.mts') or (Ext = '.jsx') or (Ext = '.tsx') then
      Result := DefineModuleManifestVirtualModules(P, Err)
    else if not GuestReadAllowed(P, Err) then
    begin
      Err := 'PermissionDenied: read: ' + Written;
      Result := False;
    end
    else Result := DefineManifestVirtualModules(P, Err);
  finally
    ManifestPath := '';
    ManifestWritten := '';
  end;
end;

function DefineConfigVirtualModulesAt(const ConfigPath: UnicodeString; Depth: Integer;
  var Err: UnicodeString): Boolean; forward;

{ the configs a config extends ("extends": a path or an array of paths, against its directory), first }
function DefineExtendedConfigs(Cfg: TValue; const Dir: UnicodeString; Depth: Integer; var Err: UnicodeString): Boolean;
var X, E: TValue; I, N: Integer; P: UnicodeString;
begin
  Result := True;
  if not GetV(Cfg, AtomOf('extends'), X) then begin Pending := VUndefined; Exit; end;
  if IsString(X) then
  begin
    P := StrToUnicode(StrOf(X));
    if (P <> '') and not IsAbsoluteFilePath(P) then P := Dir + '/' + P;
    Exit(DefineConfigVirtualModulesAt(UnicodeString(ExpandFileName(UTF8Encode(P))), Depth + 1, Err));
  end;
  if not IsObject(X) or (ObjOf(X)^.OKind <> okArray) then Exit;
  PushRoot(X);
  try
    N := ArrayLen(ObjOf(X));
    for I := 0 to N - 1 do
    begin
      E := ObjOf(RootAt(TempTop - 1))^.Elements^.Data[I];
      if not IsString(E) then Continue;
      P := StrToUnicode(StrOf(E));
      if (P <> '') and not IsAbsoluteFilePath(P) then P := Dir + '/' + P;
      if not DefineConfigVirtualModulesAt(UnicodeString(ExpandFileName(UTF8Encode(P))), Depth + 1, Err) then Exit(False);
    end;
  finally
    PopRoots(1);
  end;
end;

function DefineConfigVirtualModules(const ConfigPath: UnicodeString; var Err: UnicodeString): Boolean;
begin
  Result := DefineConfigVirtualModulesAt(ConfigPath, 0, Err);
end;

function DefineConfigVirtualModulesAt(const ConfigPath: UnicodeString; Depth: Integer;
  var Err: UnicodeString): Boolean;
var Text, Dir, P, W: UnicodeString; Cfg, M, E, S: TValue; I, N: Integer; SavedScope: string;
begin
  Result := False;
  if Depth > 32 then begin Err := 'Circular or too-deeply-nested extends in virtual module config: ' + ConfigPath; Exit; end;
  if not ReadFileUnicode(ConfigPath, Text) then begin Err := 'Cannot read ' + ConfigPath; Exit; end;
  if not DataValue(Text, UnicodeString(ExtractFileExt(UTF8Encode(ConfigPath))), Cfg) then
  begin
    Err := TakePendingText;
    Exit;
  end;
  if not IsObject(Cfg) then Exit(True);
  Dir := UnicodeString(ExtractFileDir(ExpandFileName(UTF8Encode(ConfigPath))));
  PushRoot(Cfg);
  SavedScope := GuestScopeDir;
  try
    { what it extends first: its own definitions override them }
    if not DefineExtendedConfigs(RootAt(TempTop - 1), Dir, Depth, Err) then Exit;
    { judged against the config that names them }
    GuestScopeDir := CanonicalCapabilityPath(UTF8Encode(Dir));
    { "modules": a module map, a manifest path, or an array of manifest paths }
    if GetV(RootAt(TempTop - 1), AtomOf('modules'), M) and (M <> VUndefined) then
    begin
      if IsString(M) then
      begin
        P := StrToUnicode(StrOf(M));
        W := P;
        if (P <> '') and not IsAbsoluteFilePath(P) then P := Dir + '/' + P;
        if not DefineManifestPath(P, W, Err) then Exit;
      end
      else if IsObject(M) and (ObjOf(M)^.OKind = okArray) then
      begin
        PushRoot(M);
        N := ArrayLen(ObjOf(M));
        for I := 0 to N - 1 do
        begin
          E := ObjOf(RootAt(TempTop - 1))^.Elements^.Data[I];
          if not IsString(E) then begin Err := 'Config module manifests must be paths.'; PopRoots(1); Exit; end;
          P := StrToUnicode(StrOf(E));
          W := P;
          if (P <> '') and not IsAbsoluteFilePath(P) then P := Dir + '/' + P;
          if not DefineManifestPath(P, W, Err) then begin PopRoots(1); Exit; end;
        end;
        PopRoots(1);
      end
      else if not DefineVirtualModuleMap(M, Dir, Err) then Exit;
    end
    else Pending := VUndefined;
    { "module": one definition string, or an array of them }
    if GetV(RootAt(TempTop - 1), AtomOf('module'), M) and (M <> VUndefined) then
    begin
      if IsString(M) then
      begin
        if not DefineInlineVirtualModule(StrToUnicode(StrOf(M)), Dir, Err) then Exit;
      end
      else if IsObject(M) and (ObjOf(M)^.OKind = okArray) then
      begin
        PushRoot(M);
        N := ArrayLen(ObjOf(M));
        for I := 0 to N - 1 do
        begin
          S := ObjOf(RootAt(TempTop - 1))^.Elements^.Data[I];
          if not IsString(S) then begin Err := 'Config module definitions must be strings.'; PopRoots(1); Exit; end;
          if not DefineInlineVirtualModule(StrToUnicode(StrOf(S)), Dir, Err) then begin PopRoots(1); Exit; end;
        end;
        PopRoots(1);
      end
      else
      begin
        Err := 'Config module must be a definition string or string array.';
        Exit;
      end;
    end
    else Pending := VUndefined;
    Result := True;
  finally
    GuestScopeDir := SavedScope;
    PopRoots(1);
  end;
end;

function InjectInlineGlobal(const Name, ValueText: UnicodeString; var Err: UnicodeString): Boolean;
var V: TValue;
begin
  if not DataValue(ValueText, '.json', V) then
  begin
    Pending := VUndefined;
    V := StrFromUnicode(ValueText);
  end;
  Result := InjectGlobalBinding(Name, V);
  if not Result then Err := TakePendingText;
end;

{ the registered key of a host module (shared.findHostModule) }
function FindHostModuleKey(const Key: UnicodeString; out Registered: UnicodeString;
  IncludeBareRegistry: Boolean = False): Boolean;
var A: array[0..0] of TValue; R, Lookup: TValue;
begin
  if Assigned(MockModules) and (MockModules.IndexOfName(string(Key)) >= 0) then
  begin
    Registered := Key;
    Exit(True);
  end;
  if NativeModuleRegistered(Key) then
  begin
    Registered := Key;
    Exit(True);
  end;
  Result := False;
  Lookup := FindHostModuleV;
  if IncludeBareRegistry and not IsObject(Lookup) then
    if not GetV(SharedV, AtomOf('findHostModule'), Lookup) then Exit;
  if not IsObject(Lookup) then Exit;
  PushRoot(Lookup);
  try
    A[0] := StrFromUnicode(Key);
    if CallValue(RootAt(TempTop - 1), SharedV, @A[0], 1, R) and IsString(R) then
    begin
      Registered := StrToUnicode(StrOf(R));
      Result := True;
    end;
    Pending := VUndefined;
  finally
    PopRoots(1);
  end;
end;

function IsHostModuleKey(const Key: UnicodeString): Boolean;
var Registered: UnicodeString;
begin
  Result := FindHostModuleKey(Key, Registered);
end;

{ whether an embedder may register Specifier (a native module or a provided one, What in the messages): a virtual
  address outside the runtime's goccia: namespace that no module uses yet }
function CanRegisterHostModule(const Specifier, What: UnicodeString; var Err: UnicodeString): Boolean;
var Registered: UnicodeString;
begin
  Result := False;
  if IsAbsoluteFilePath(UTF8Encode(Specifier)) or not IsVirtualModuleURI(UTF8Encode(Specifier)) or
    (Pos(#0, Specifier) > 0) or (Pos(#1, Specifier) > 0) then
  begin
    Err := What + ' specifier must be a virtual address';
    Exit;
  end;
  { the goccia: namespace is the runtime's, including modules a test or bench host installs later }
  if Copy(Specifier, 1, 7) = 'goccia:' then
  begin
    Err := What + ' specifier must not use the runtime''s goccia: namespace';
    Exit;
  end;
  if FindHostModuleKey(Specifier, Registered, True) then
  begin
    Err := 'Module is already registered: ' + Registered;
    Exit;
  end;
  Result := True;
end;

function DefineNativeModule(const Specifier: UnicodeString; Factory: TNativeModuleFactory;
  var Err: UnicodeString): Boolean;
begin
  Result := False;
  if not Assigned(Factory) then
  begin
    Err := 'Native module factory is required';
    Exit;
  end;
  if not CanRegisterHostModule(Specifier, 'Native module', Err) then Exit;
  if NativeModuleFactories = nil then NativeModuleFactories := TNativeModuleFactories.Create;
  NativeModuleFactories.Add(Specifier, Factory);
  SetHostModuleHook(@HostModule, @IsHostModuleKey, @NativeModuleWithoutSource, IsObject(LoadHostModuleV));
  Err := '';
  Result := True;
end;

function DefineModuleProvider(const Specifier: UnicodeString; Provider: TModuleProvider;
  var Err: UnicodeString): Boolean;
begin
  Result := False;
  if not Assigned(Provider) then
  begin
    Err := 'Module provider is required';
    Exit;
  end;
  if not CanRegisterHostModule(Specifier, 'Provided module', Err) then Exit;
  if ModuleProviders = nil then ModuleProviders := TModuleProviders.Create;
  ModuleProviders.Add(Specifier, Provider);
  SetHostModuleHook(@HostModule, @IsHostModuleKey, @NativeModuleWithoutSource, IsObject(LoadHostModuleV));
  Err := '';
  Result := True;
end;

function CurrentResolver: TModuleResolver;
begin
  Result := ProvidedResolver;
  if Result = nil then
  begin
    if HostResolver = nil then
    begin
      HostResolver := TModuleResolver.Create;
      HostResolver.SetExtensions(ModuleImportExtensions);
    end;
    Result := HostResolver;
  end;
end;

{ a module's key that is a virtual module's address (a URI such as host:pkg/main or https://...), not a path }
function IsVirtualKey(const Key: UnicodeString): Boolean;
begin
  Result := IsVirtualModuleURI(Key) or ((Key <> '') and not IsAbsoluteFilePath(Key) and (Key[1] <> '<') and IsHostModuleKey(Key));
end;

{ ---- the read policy ---- }

type
  TReadDenial = (rdDenied, rdOutsideProject, rdComputedImport);

  { a host candidate the policy refused while the resolver probed it }
  EReadRefused = class(Exception)
  public
    Canonical: string;
    Denial: TReadDenial;
  end;

  TReadPolicy = class
  public
    Caps: TGocciaCapabilities;
    Root: string;
    Audit: TGocciaReadAudit;
    { the request being resolved }
    Spec: string;
    Literal: Boolean;
    PackageDir: string;
    { the importing file and the resolver of the request (the provider package check's) }
    Importer: string;
    Resolver: TModuleResolver;
    function IsProjectGraph(const Canonical: string): Boolean;
    function Verdict(const Canonical: string; Literal_: Boolean; out Denial: TReadDenial): Boolean;
    procedure Probe(const ACandidatePath: string);
  end;

threadvar
  ReadPolicy: TReadPolicy;
  GuestLoads: Boolean;
  ProviderGraph: TProviderGraphCheck;

{ called through a local copy, as the other procedural threadvars (see CallBenchProgress) }
function CallProviderGraph(const Path, ImportingFile, PackageRoot: string): Boolean;
var H: TProviderGraphCheck;
begin
  H := ProviderGraph;
  Result := H(Path, ImportingFile, PackageRoot);
end;

procedure SetProviderGraphCheck(Check: TProviderGraphCheck);
begin
  ProviderGraph := Check;
end;

procedure SetGuestLoads(On: Boolean);
begin
  GuestLoads := On;
  GuestLoadsOn := On;
end;

function GuestReadAllowed(const Path: UnicodeString; var Err: UnicodeString): Boolean;
var Canonical: string; Denial: TReadDenial;
begin
  Result := True;
  if not GuestLoads or (ReadPolicy = nil) then Exit;
  Canonical := CanonicalCapabilityPath(UTF8Encode(Path));
  ReadPolicy.PackageDir := '';
  if ReadPolicy.Verdict(Canonical, True, Denial) then Exit;
  if Assigned(ReadPolicy.Audit) then
    ReadPolicy.Audit(False, Canonical, 'the path is outside the project and no read grant covers it');
  Pending := VUndefined;
  Err := 'PermissionDenied: read: ' + Path;
  Result := False;
end;

procedure SetReadPolicy(On: Boolean; const Caps: TGocciaCapabilities; const ProjectRoot: string;
  Audit: TGocciaReadAudit);
begin
  FreeAndNil(ReadPolicy);
  if not On then Exit;
  ReadPolicy := TReadPolicy.Create;
  ReadPolicy.Caps := Caps;
  { spelled as the paths it is compared with are (CanonicalCapabilityPath): a root from the working directory can be
    a Windows short name (C:\Users\RUNNER~1\...) that the canonical path of a file inside it spells out in full }
  if ProjectRoot <> '' then ReadPolicy.Root := CanonicalCapabilityPath(ProjectRoot) else ReadPolicy.Root := '';
  ReadPolicy.Audit := Audit;
end;

{ inside the project and outside every .goccia directory in it }
function TReadPolicy.IsProjectGraph(const Canonical: string): Boolean;
var Rel: string; I, Start: Integer;
begin
  if (Root = '') or not IsPathWithinScope(Canonical, Root) then Exit(False);
  Rel := Copy(Canonical, Length(Root) + 1, MaxInt);
  Start := 1;
  for I := 1 to Length(Rel) + 1 do
    if (I > Length(Rel)) or (Rel[I] = PathDelim) then
    begin
      if SameText(Copy(Rel, Start, I - Start), PACKAGE_CACHE_DIRECTORY_NAME) then Exit(False);
      Start := I + 1;
    end;
  Result := True;
end;

function TReadPolicy.Verdict(const Canonical: string; Literal_: Boolean; out Denial: TReadDenial): Boolean;
begin
  Denial := rdDenied;
  if Caps.DeniesCanonicalPath(gcRead, Canonical) then Exit(False);
  if Literal_ and IsProjectGraph(Canonical) then Exit(True);
  if Literal_ and GuestLoads and (GuestScopeDir <> '') and IsPathWithinScope(Canonical, GuestScopeDir) then Exit(True);
  if Literal_ and (PackageDir <> '') and IsPathWithinScope(Canonical, PackageDir) then Exit(True);
  if Caps.AllowsCanonicalPath(gcRead, Canonical) then Exit(True);
  if Literal_ then Denial := rdOutsideProject else Denial := rdComputedImport;
  Result := False;
end;

{ each candidate the resolver looks at, before it asks the host whether it exists: a refused one stops the
  resolution, so neither the outcome nor which later candidate loads can depend on files the script may not read }
procedure TReadPolicy.Probe(const ACandidatePath: string);
var Canonical: string; Denial: TReadDenial; E: EReadRefused;
begin
  Canonical := CanonicalCapabilityPath(ACandidatePath);
  { a provider package's own file: confined and verified, the import grant covers it }
  if Assigned(ProviderGraph) and Assigned(Resolver) and
     CallProviderGraph(ACandidatePath, Importer, Resolver.ProbePackageDirectory) and
     not Caps.DeniesCanonicalPath(gcRead, Canonical) then Exit;
  if Verdict(Canonical, Literal, Denial) then Exit;
  E := EReadRefused.Create(Spec);
  E.Canonical := Canonical;
  E.Denial := Denial;
  raise E;
end;

{ the refusal: audited, and a PermissionDenied naming the specifier the script wrote; the host path and the grant
  that would allow it travel only in the host's suggestion (ADR 0108) }
constructor EGocciaPermissionDenied.CreateDenied(const ACapability, AScope, ASuggestion: string);
begin
  inherited Create(ACapability + ': ' + AScope);
  Capability := ACapability;
  Scope := AScope;
  Suggestion := ASuggestion;
end;

{ the resolution's PermissionDenied, made in the host's realm, whose PermissionDenied the CLI installed (a config's
  module runs in a realm of its own) }
function DenyResolution(const Capability, Scope, Suggestion: string): Integer;
var E: TValue; Saved: Integer;
begin
  E := VUndefined;
  Saved := CurRealm;
  if Saved <> HostRealm then SwitchRealm(HostRealm);
  try
    NewPermissionDenied(UnicodeString(Capability), UnicodeString(Scope), UnicodeString(Suggestion), E);
  finally
    if Saved <> HostRealm then SwitchRealm(Saved);
  end;
  SetResolveError(E);
  Result := -2;
end;

function DenyRead(const Spec, Canonical: string; Denial: TReadDenial): Integer;
const
  Reasons: array[TReadDenial] of string = ('read is denied for this path',
    'the path is outside the project and no read grant covers it', 'a computed import specifier needs a read grant');
var Suggestion: string;
begin
  if Assigned(ReadPolicy.Audit) and not ReadPolicy.Audit(False, Canonical, Reasons[Denial]) then
  begin
    SetResolveError(Pending);
    Pending := VUndefined;
    Exit(-2);
  end;
  case Denial of
    rdDenied: Suggestion := Format(SSuggestReadDenied, [Canonical]);
    rdOutsideProject: Suggestion := Format(SSuggestReadNotGranted, [Canonical, ExtractFileDir(Canonical)]);
  else
    Suggestion := Format(SSuggestReadComputed, [Canonical, ExtractFileDir(Canonical)]);
  end;
  Result := DenyResolution('read', Spec, Suggestion);
end;

procedure WarnVirtualShadow(const Key, Path: UnicodeString);
begin
  if not IsAbsoluteFilePath(Path) or not FileExists(UTF8Encode(Path)) then Exit;
  if ShadowWarned = nil then ShadowWarned := TStringList.Create;
  if ShadowWarned.IndexOf(UTF8Encode(Key)) >= 0 then Exit;
  ShadowWarned.Add(UTF8Encode(Key));
  WriteLn(ErrOutput, 'Warning: virtual module "', UTF8Encode(Key), '" shadows module "', UTF8Encode(Path), '".');
end;

function HostResolveSpec(const Spec, Referrer: UnicodeString; out Resolved, RealPath: UnicodeString;
  out VirtualKey: Boolean): Integer;
var Ref: string; Resolver: TModuleResolver; Candidate: UnicodeString; Guard: Boolean; Policy: TReadPolicy;
  SavedGuard: TModuleResolverProbeGuard; Canonical: string; Denial: TReadDenial;
  Literal, ProviderAllowed: Boolean;
  SavedPrefer: Boolean;
begin
  RealPath := '';
  VirtualKey := True;
  { the import() being resolved: its specifier computed or not (the first resolution takes it) }
  Literal := not ImportComputed;
  ImportComputed := False;
  if FindHostModuleKey(Spec, Resolved) then
  begin
    WarnVirtualShadow(Resolved, Spec);
    Exit(1);
  end;
  { a relative specifier in a virtual module resolves against its address, hierarchically }
  if IsRelativeSpecifier(Spec) and (Referrer <> '') and IsVirtualKey(Referrer) then
  begin
    Candidate := ResolveRelativeVirtualAddress(Spec, Referrer);
    if FindHostModuleKey(Candidate, Resolved) then Exit(1);
  end;
  { a registered relative address can name a module even when no file is there }
  if IsRelativeSpecifier(Spec) and (Referrer <> '') and (Referrer[1] <> '<') and not IsVirtualKey(Referrer) and
     not SandboxActive then
  begin
    Candidate := UnicodeString(ExpandFileName(IncludeTrailingPathDelimiter(ExtractFileDir(
      ExpandFileName(UTF8Encode(Referrer)))) + UTF8Encode(Spec)));
    if FindHostModuleKey(Candidate, Resolved) then
    begin
      WarnVirtualShadow(Resolved, Candidate);
      Exit(1);
    end;
  end;
  Resolver := CurrentResolver;
  if (Referrer = '') or (Referrer[1] = '<') or IsVirtualKey(Referrer) then Ref := ''
  { a sandbox's paths are its own, rooted at / (ExpandFileName would put a Windows drive in front) }
  else if SandboxActive then Ref := UTF8Encode(Referrer)
  else Ref := ExpandFileName(UTF8Encode(Referrer));
  { the read policy judges every request while it is on: the run's (a function a host module left behind imports as
    the guest it runs for) and a config's loads; the host's own loads before the run are not judged }
  Policy := ReadPolicy;
  { a virtual module's imports are judged too: a config's or a --module definition runs under the script's
    capability set (docs/virtual-modules.md), so it reaches no host file the script could not }
  Guard := Policy <> nil;
  if Guard then
  begin
    Policy.Spec := UTF8Encode(Spec);
    Policy.Literal := Literal;
    Policy.PackageDir := '';
    Policy.Importer := Ref;
    Policy.Resolver := Resolver;
    SavedGuard := Resolver.ProbeGuard;
    Resolver.ProbeGuard := Policy.Probe;
  end;
  try
    SavedPrefer := Resolver.PreferBytecodeCompanions;
    if ResolvingSourcePhase then Resolver.PreferBytecodeCompanions := False;
    try
      Resolved := UTF8Decode(Resolver.Resolve(UTF8Encode(Spec), Ref));
    finally
      if Guard then Resolver.ProbeGuard := SavedGuard;
      Resolver.PreferBytecodeCompanions := SavedPrefer;
    end;
    { a .gbc companion (--prefer-bytecode) is held to its source's package policy: a CommonJS file's companion is
      refused as the file would be }
    if SameText(ExtractFileExt(UTF8Encode(Resolved)), '.gbc') and (Resolver.LastCompanionSourcePath <> '') then
      Resolver.ValidateCompanionSourcePath(Resolver.LastCompanionSourcePath);
    if FindHostModuleKey(Resolved, Candidate) then
    begin
      WarnVirtualShadow(Candidate, Resolved);
      Resolved := Candidate;
      VirtualKey := True;
    end
    else VirtualKey := IsVirtualModuleURI(Resolved);
    { Pass the path judged by the final read check through to the loader, including on cache hits. }
    if not VirtualKey then RealPath := HostRealFilePath(Resolved);
    if Guard and not VirtualKey then
    begin
      Canonical := RealPath;
      if Canonical = '' then Canonical := CanonicalCapabilityPath(UTF8Encode(Resolved));
      { a literal bare import's package (the directory it resolved in) is part of the graph }
      Policy.PackageDir := '';
      if Literal and (Resolver.ProbePackageDirectory <> '') then
        Policy.PackageDir := CanonicalCapabilityPath(Resolver.ProbePackageDirectory);
      ProviderAllowed := Assigned(ProviderGraph) and
        CallProviderGraph(UTF8Encode(Resolved), Ref, Resolver.LastPackageDirectory) and
        not Policy.Caps.DeniesCanonicalPath(gcRead, Canonical);
      if not ProviderAllowed and not Policy.Verdict(Canonical, Literal, Denial) then
      begin
        if (ManifestPath <> '') and (Spec = ManifestPath) then
          Exit(DenyRead(UTF8Encode(ManifestWritten), Canonical, Denial));
        Exit(DenyRead(UTF8Encode(Spec), Canonical, Denial));
      end;
      if Assigned(Policy.Audit) and not ProviderAllowed and not (Literal and Policy.IsProjectGraph(Canonical)) and
         not Policy.Audit(True, Canonical, 'a read grant covers the path') then
      begin
        SetResolveError(Pending);
        Pending := VUndefined;
        Exit(-2);
      end;
    end;
    { the first specifier a module is imported by names it in its load errors (specifierOf) }
    if ImportSpecifiers = nil then ImportSpecifiers := TStringList.Create;
    if ImportSpecifiers.IndexOfName(UTF8Encode(Resolved)) < 0 then
      ImportSpecifiers.Add(UTF8Encode(Resolved) + '=' + UTF8Encode(Spec));
    Result := 1;
  except
    on E: EReadRefused do
      if (ManifestPath <> '') and (Spec = ManifestPath) then
        Result := DenyRead(UTF8Encode(ManifestWritten), E.Canonical, E.Denial)
      else Result := DenyRead(E.Message, E.Canonical, E.Denial);
    on E: EGocciaPermissionDenied do
      Result := DenyResolution(E.Capability, E.Scope, E.Suggestion);
    on E: Exception do
    begin
      if IsEngineFault(E) then raise;
      Resolved := UTF8Decode(E.Message);
      if E is EModuleNotFound then ResolveCandidate := UTF8Decode(EModuleNotFound(E).ResolvedCandidatePath);
      Result := -1;
    end;
  end;
end;

{ promiseState(p): [0 pending | 1 fulfilled | 2 rejected, the value or reason], or undefined for anything else }
function Host_PromiseState(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var Status: Integer; V: TValue;
begin
  Res := VUndefined;
  Result := True;
  if (Argc < 1) or not IsPromiseValue(Args[1]) then Exit;
  V := VUndefined;
  Status := PromiseStatus(Args[1], V);
  PushRoot(V);
  PushRoot(MkPtr(NewArray(0, 2)));
  ArrayPush(ObjOf(RootAt(TempTop - 1)), MkNumber(Status));
  ArrayPush(ObjOf(RootAt(TempTop - 1)), RootAt(TempTop - 2));
  Res := RootAt(TempTop - 1);
  PopRoots(2);
end;

function ReplFormat(V: TValue; Color: Boolean; var Text: UnicodeString): Boolean;
var F, R: TValue; A: array[0..1] of TValue;
begin
  Text := '';
  if not IsObject(SharedV) or not GetV(SharedV, AtomOf('replFormat'), F) or not IsCallable(F) then
  begin
    Result := ToStringV(V, R) and IsString(R);
    if Result then Text := StrToUnicode(StrOf(R));
    Exit;
  end;
  A[0] := V;
  A[1] := MkBool(Color);
  Result := CallValue(F, VUndefined, @A[0], 2, R);
  if Result and IsString(R) then Text := StrToUnicode(StrOf(R));
end;

{ moduleLoaded(key): whether a module record is keyed key (a virtual module may not replace it) }
function Host_ModuleLoaded(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
begin
  Res := MkBool(ModuleRecordExists(ArgStr(Args, Argc, 1)));
  Result := True;
end;

function Host_NativeModuleRegistered(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
begin
  Res := MkBool(NativeModuleRegistered(ArgStr(Args, Argc, 1)));
  Result := True;
end;

{ readText(path): a file's text, decoded as UTF-8 without a BOM; undefined when it cannot be read }
function Host_ReadText(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var B: RawByteString; Path, FilePath: UnicodeString;
begin
  Res := VUndefined;
  Result := True;
  Path := ArgStr(Args, Argc, 1);
  FilePath := ArgStr(Args, Argc, 2);
  if Path = '' then Exit;
  if not ReadModuleBytes(Path, FilePath, B) then Exit(Pending = VUndefined);
  if (Length(B) >= 3) and (B[1] = #$EF) and (B[2] = #$BB) and (B[3] = #$BF) then Delete(B, 1, 3);
  SetCodePage(B, CP_UTF8, False);
  Res := StrFromUnicode(UTF8DecodeKeepingNoncharacters(B));
end;

{ fileSize(path): a file's size in bytes, -1 when it cannot be read }
{ isAbsolutePath(path): whether path names a file from the root (IsAbsoluteFilePath: a drive on Windows) }
function Host_IsAbsolutePath(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
begin
  if IsAbsoluteFilePath(ArgStr(Args, Argc, 1)) then Res := VTrue else Res := VFalse;
  Result := True;
end;

{ expandPath(dir, rel): on Windows, rel under the directory dir expanded as GocciaScript's resolver spells the paths
  it resolves (ExpandFileName: '\' separators, '.' and '..' folded); '' elsewhere, where the prelude joins with '/' }
function Host_ExpandPath(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
begin
{$IFDEF MSWINDOWS}
  Res := StrFromUnicode(UnicodeString(ExpandFileName(IncludeTrailingPathDelimiter(
    UTF8Encode(ArgStr(Args, Argc, 1))) + UTF8Encode(ArgStr(Args, Argc, 2)))));
{$ELSE}
  Res := StrFromUnicode('');
{$ENDIF}
  Result := True;
end;

{ A virtual file can have no disk counterpart. Resolve its parent directory and keep its configured name. }
function Host_VirtualFileKey(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var Key: UnicodeString; Directory, Path: string;
begin
  Key := ArgStr(Args, Argc, 1);
  if not SandboxActive and IsAbsoluteFilePath(Key) then
  begin
    Path := ExpandFileName(UTF8Encode(Key));
    Directory := CanonicalCapabilityPath(ExtractFileDir(Path));
    Key := UTF8Decode(IncludeTrailingPathDelimiter(Directory) + ExtractFileName(Path));
  end;
  Res := StrFromUnicode(Key);
  Result := True;
end;

function Host_FileSize(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var SR: TSearchRec; Path: string; B: RawByteString;
begin
  Res := MkInt(-1);
  Result := True;
  { a sandbox's file: through the thread's file reader }
  if SandboxActive then
  begin
    if ReadFileBytes(ArgStr(Args, Argc, 1), B) then Res := MkNumber(Length(B));
    Exit;
  end;
  Path := UTF8Encode(ArgStr(Args, Argc, 1));
  if FindFirst(Path, faAnyFile, SR) = 0 then
  begin
    Res := MkNumber(SR.Size);
    FindClose(SR);
  end;
end;

{ setClock(epochMs, monoMs): the mocked clocks (NaN: the real one) }
function Host_SetClock(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var E, M: Double;
begin
  if Argc >= 1 then E := PrimitiveToNumber(Args[1]) else E := NaN;
  if Argc >= 2 then M := PrimitiveToNumber(Args[2]) else M := NaN;
  SetClockOverride(E, M);
  Res := VUndefined;
  Result := True;
end;

{ timeOrigin(): performance.timeOrigin (Goccia.Core TimeOriginInit) }
function Host_TimeOrigin(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var Ms: Double;
begin
  Ms := TimeOriginMs;
  Res := MkNumber(Ms);
  Result := True;
end;

{ realNow(): the real epoch time in milliseconds, whatever is mocked }
function Host_RealNow(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
begin
  Res := MkNumber(GetEpochNanoseconds div 1000000);
  Result := True;
end;

{ asyncGet() and asyncSet(v): the async context (Goccia.Promise) }
function Host_AsyncGet(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
begin
  Res := GetAsyncContext;
  Result := True;
end;

function Host_AsyncSet(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
begin
  if Argc >= 1 then SetAsyncContext(Args[1]) else SetAsyncContext(VUndefined);
  Res := VUndefined;
  Result := True;
end;

{ astParse(source, jsx, module, tokens, expressions, fileName): goccia:ast's parse as JSON text (Goccia.AST.Report);
  a source that does not parse throws its SyntaxError }
function Host_ASTParse(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var Json: string;
  function Flag(I: Integer): Boolean;
  begin
    Result := (Argc >= I) and (Args[I] = VTrue);
  end;
begin
  try
    Json := ASTReportJSON(U8(ArgStr(Args, Argc, 1)), ActivePipelineOptions(False), Flag(2), Flag(3), Flag(4), Flag(5),
      U8(ArgStr(Args, Argc, 6)));
  except
    on E: EGocciaASTSyntaxError do Exit(ThrowError('SyntaxError', UTF8Decode(E.Message)));
  end;
  Res := FromU8(Json);
  Result := True;
end;

{ dataParse(kind, text): a parse as Goccia.DataTree's JSON (DataTreeToJSON), which datatree.js rebuilds: kind 0 YAML
  (YAML.parse), 1 the YAML documents (YAML.parseDocuments), 2 TOML. Text that does not parse throws a SyntaxError,
  nesting past the parsers' depth limit a RangeError, as the CLIs' modules do. }
function Host_DataParse(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var Tree: TGocciaDataTree; YAML: TGocciaYAMLParser; TOML: TGocciaTOMLParser; Text, Json: UnicodeString;
begin
  Text := ArgStr(Args, Argc, 2);
  Tree := nil;
  try
    try
      case Round(PrimitiveToNumber(Args[1])) of
        0, 1:
          begin
            YAML := TGocciaYAMLParser.Create;
            try
              if Round(PrimitiveToNumber(Args[1])) = 0 then Tree := YAML.Parse(Text) else Tree := YAML.ParseDocuments(Text);
            finally
              YAML.Free;
            end;
          end;
      else
        TOML := TGocciaTOMLParser.Create;
        try
          Tree := TOML.Parse(Text);
        finally
          TOML.Free;
        end;
      end;
      Json := DataTreeToJSON(Tree.Root);
    finally
      Tree.Free;
    end;
  except
    on E: EGocciaYAMLParseError do Exit(ThrowError('SyntaxError', UTF8Decode(E.Message)));
    on E: EGocciaTOMLParseError do Exit(ThrowError('SyntaxError', UTF8Decode(E.Message)));
    on E: EGocciaDataDepthError do Exit(ThrowError('RangeError', UTF8Decode(E.Message)));
    on E: ENativeDepthLimit do Exit(ThrowError('RangeError', UTF8Decode(E.Message)));
    on E: EGocciaDataTypeError do Exit(ThrowError('TypeError', UTF8Decode(E.Message)));
    { any other parser failure is still the text's: never let a Pascal exception into the VM }
    on E: Exception do
    begin
      if IsEngineFault(E) then raise;
      Exit(ThrowError('SyntaxError', UTF8Decode(E.Message)));
    end;
  end;
  Res := StrFromUnicode(Json);
  Result := True;
end;

{ RFC 8089: a host path as a file: URL (absolute, percent-encoded), as GocciaScript's import.meta.url has it }
function FileURLOf(const Path: string): string;
var Abs: string;
begin
  Abs := ExpandFileName(Path);
  {$IFDEF MSWINDOWS}
  Abs := StringReplace(Abs, '\', '/', [rfReplaceAll]);
  if (Length(Abs) >= 2) and (Abs[1] = '/') and (Abs[2] = '/') then Exit(FILE_URL_SCHEME + PercentEncodePath(Abs));
  Result := FILE_URL_SCHEME + '///' + PercentEncodePath(Abs);
  {$ELSE}
  Result := FILE_URL_SCHEME + '//' + PercentEncodePath(Abs);
  {$ENDIF}
end;

{ a sandbox path, Path against the directory Base, as the sandbox's file system canonicalizes it
  (TSandboxVirtualFileSystem.Normalize, without its root-clamp audit, since resolving a name reads nothing): '/'-rooted,
  either slash a separator, '.' dropped and '..' stopping at the root; a trailing slash stays. A host path would get a
  Windows drive from ExpandFileName. }
function SandboxPathOf(const Path, Base: string): string;
var Combined, Segment: string; Parts: TStringList; I, Start: Integer;
begin
  Combined := StringReplace(Path, '\', '/', [rfReplaceAll]);
  if (Combined = '') or (Combined[1] <> '/') then
    Combined := StringReplace(Base, '\', '/', [rfReplaceAll]) + '/' + Combined;
  Parts := TStringList.Create;
  try
    Start := 1;
    for I := 1 to Length(Combined) + 1 do
      if (I > Length(Combined)) or (Combined[I] = '/') then
      begin
        Segment := Copy(Combined, Start, I - Start);
        Start := I + 1;
        if Segment = '..' then
        begin
          if Parts.Count > 0 then Parts.Delete(Parts.Count - 1);
        end
        else if (Segment <> '') and (Segment <> '.') then
          Parts.Add(Segment);
      end;
    Result := '';
    for I := 0 to Parts.Count - 1 do Result := Result + '/' + Parts[I];
    { a directory named with a trailing slash keeps it, as a URL does }
    if (Result = '') or (Combined[Length(Combined)] = '/') then Result := Result + '/';
  finally
    Parts.Free;
  end;
end;

{ the file: URL of a module's path: a sandbox's own path, or a host path (FileURLOf) }
function ModuleFileURL(const Path: string): string;
begin
  if SandboxActive then Result := FILE_URL_SCHEME + '//' + PercentEncodePath(SandboxPathOf(Path, '/'))
  else Result := FileURLOf(Path);
end;

{ the directory of a module's path, for a relative specifier: a sandbox's own path, or a host path }
function ModuleDirOf(const Path: string): string;
begin
  if SandboxActive then Result := SandboxPathOf(Path + '/..', '/')
  else Result := IncludeTrailingPathDelimiter(ExtractFilePath(ExpandFileName(Path)));
end;

threadvar
  InspectDepth: Integer;   { 0: DEFAULT_INSPECT_DEPTH }

procedure SetInspectDepth(ADepth: Integer);
begin
  if ADepth <= 0 then InspectDepth := 0
  else if ADepth > MAX_INSPECT_DEPTH then InspectDepth := MAX_INSPECT_DEPTH
  else InspectDepth := ADepth;
end;

{ inspectDepth(): how many levels deep console output renders objects (console.js) }
function Host_InspectDepth(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
begin
  if InspectDepth = 0 then Res := MkNumber(DEFAULT_INSPECT_DEPTH) else Res := MkNumber(InspectDepth);
  Result := True;
end;

{ fileURL(path): the file: URL of a module's path }
function Host_FileURL(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
begin
  { a virtual module's address is its URL already }
  if IsVirtualModuleURI(ArgStr(Args, Argc, 1)) then Res := StrFromUnicode(ArgStr(Args, Argc, 1))
  else Res := StrFromUnicode(UTF8Decode(ModuleFileURL(UTF8Encode(ArgStr(Args, Argc, 1)))));
  Result := True;
end;

{ metaResolve(modulePath, argc, specifier): import.meta.resolve(specifier) of the module at modulePath, as
  GocciaScript resolves it: an absolute specifier as it is, any other against the module's directory, as a file:
  URL (the file need not exist) }
function Host_MetaResolve(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var Spec, Path, RefPath: string; S: TValue; Ref, Registered: UnicodeString;
begin
  if (Argc < 2) or (PrimitiveToNumber(Args[2]) < 1) then
    Exit(ThrowTypeError('import.meta.resolve requires a specifier argument'));
  if (Argc < 3) or not ToStringV(Args[3], S) then Exit(False);
  Spec := UTF8Encode(StrToUnicode(StrOf(S)));
  Ref := ArgStr(Args, Argc, 1);
  Result := True;
  { a host module (a virtual one too) by its key; a relative specifier in a virtual module against its address }
  if FindHostModuleKey(UnicodeString(Spec), Registered) then
  begin
    Res := StrFromUnicode(Registered);
    Exit;
  end;
  if IsVirtualKey(Ref) and IsRelativeSpecifier(Spec) then
  begin
    Res := StrFromUnicode(ResolveRelativeVirtualAddress(UnicodeString(Spec), Ref));
    Exit;
  end;
  { a bare specifier through the run's resolver (its aliases and import map); the file need not exist }
  if not IsRelativeSpecifier(Spec) and (Spec <> '') and (Spec[1] <> '/') and (Spec[1] <> '\') and
     not ((Length(Spec) >= 2) and (Spec[2] = ':')) then
  begin
    if IsVirtualKey(Ref) or (Ref = '') or (Ref[1] = '<') then RefPath := ''
    { a sandbox's paths are its own, as HostResolveSpec has them }
    else if SandboxActive then RefPath := UTF8Encode(Ref)
    else RefPath := ExpandFileName(UTF8Encode(Ref));
    try
      Path := CurrentResolver.Resolve(Spec, RefPath);
      if IsVirtualModuleURI(UnicodeString(Path)) then Res := StrFromUnicode(UnicodeString(Path))
      else Res := StrFromUnicode(UTF8Decode(ModuleFileURL(Path)));
      Exit;
    except
      on E: Exception do
      begin
        if IsEngineFault(E) then raise;
      end;
    end;
  end;
  if SandboxActive and not IsVirtualKey(Ref) then
    Path := SandboxPathOf(Spec, ModuleDirOf(UTF8Encode(Ref)))
  else if (Spec <> '') and ((Spec[1] = '/') or (Spec[1] = '\') or ((Length(Spec) >= 2) and (Spec[2] = ':'))) then
    Path := ExpandFileName(Spec)
  else if IsVirtualKey(Ref) then
    Path := ExpandFileName(Spec)
  else
    Path := ExpandFileName(ModuleDirOf(UTF8Encode(Ref)) + Spec);
  Res := StrFromUnicode(UTF8Decode(ModuleFileURL(Path)));
end;

{ Goccia.Modules's import.meta hook: shared.initImportMeta(meta, path) (modules.js) }
function HostInitImportMeta(Meta: TValue; const Path: UnicodeString): Boolean;
var A: array[0..1] of TValue; F, R: TValue;
begin
  Result := True;
  if not IsObject(SharedV) then Exit;
  PushRoot(Meta);
  A[1] := StrFromUnicode(Path);
  PushRoot(A[1]);
  if not GetV(SharedV, AtomOf('initImportMeta'), F) or not IsCallable(F) then begin PopRoots(2); Exit; end;
  A[0] := RootAt(TempTop - 2);
  A[1] := RootAt(TempTop - 1);
  Result := CallValue(F, SharedV, @A[0], 2, R);
  PopRoots(2);
end;

{ specifierOf(key): the specifier the module at key was first imported by (key itself when none was recorded) }
function Host_SpecifierOf(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var Key: string; I: Integer;
begin
  Key := UTF8Encode(ArgStr(Args, Argc, 1));
  I := -1;
  if ImportSpecifiers <> nil then I := ImportSpecifiers.IndexOfName(Key);
  if I >= 0 then Res := StrFromUnicode(UTF8Decode(ImportSpecifiers.ValueFromIndex[I]))
  else Res := StrFromUnicode(UTF8Decode(Key));
  Result := True;
end;

{ Goccia.Modules's JSON module exports hook: shared.jsonExports(value) (modules.js) }
function HostJSONExports(V: TValue; var Exp: TValue): Boolean;
var F: TValue; A: array[0..0] of TValue;
begin
  Exp := VUndefined;
  Result := True;
  if not IsObject(SharedV) then Exit;
  PushRoot(V);
  if not GetV(SharedV, AtomOf('jsonExports'), F) or not IsCallable(F) then begin PopRoots(1); Exit; end;
  A[0] := RootAt(TempTop - 1);
  Result := CallValue(F, SharedV, @A[0], 1, Exp);
  PopRoots(1);
end;

{ vitestSource(moduleOnly): the vitest compatibility shim's module source (Goccia.VitestShim.inc), with MODULE_ONLY
  set for the module-only opt-in. The hosts that run a test file (GocciaTestRunner, goccia --test) hoist vi.mock and
  vi.unmock out of it before it is parsed; the module-only opt-in has no such pass, and its shim refuses them }
function VitestShimSource(ModuleOnly: Boolean): UnicodeString;
const
  LB = #10;
  Flags: array[Boolean] of UnicodeString = ('const MODULE_ONLY = false;' + LB, 'const MODULE_ONLY = true;' + LB);

  function ShimSource: UnicodeString;
  begin
    {$I Goccia.VitestShim.inc}
  end;

begin
  Result := Flags[ModuleOnly] + ShimSource;
end;

function Host_VitestSource(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
begin
  Res := StrFromUnicode(VitestShimSource((Argc >= 1) and (Args[1] = VTrue)));
  Result := True;
end;

threadvar
  { InstallTestingAPI installs goccia:test module-only (GocciaRunner's import of it): no testing globals, and the
    job errors keep failing the run outside runTests }
  TestModuleOnly: Boolean;
  { the engine whose goccia:test is installed (FirstInEngine's generation) }
  TestModuleGen: Integer;

{ n.testingModule(): goccia:test outside the test runner, the testing API installed on its first import, module-only;
  its exports }
function Host_TestingModule(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var Err: UnicodeString;
begin
  if not SameEngine(TestModuleGen) then
  begin
    TestModuleOnly := True;
    try
      Result := InstallTestingAPI(Err);
    finally
      TestModuleOnly := False;
    end;
    if not Result then Exit(ThrowError('Error', 'goccia:test could not be installed: ' + Err));
    FirstInEngine(TestModuleGen);
  end;
  Result := GetV(TestApiV, AtomOf('api'), Res);
end;

{ ---- installing the preludes ---- }

{ calls the prelude's function value (Source's completion) with the Argc temp roots from Base; Res its result }
function CallPrelude(const Name, Source: UnicodeString; Base, Argc: Integer; var Res: TValue;
  var Err: UnicodeString): Boolean;
var T: PFuncTemplate; Clo, Fn, S: TValue; A: array[0..15] of TValue; I: Integer;
begin
  Result := False;
  if not CompilePrelude(Name, Source, T, Err) then Exit;
  if MakeScriptClosure(T, Clo) and CallValue(Clo, VUndefined, nil, 0, Fn) then
  begin
    PushRoot(Fn);
    for I := 0 to Argc - 1 do A[I] := RootAt(Base + I);
    Result := CallValue(RootAt(TempTop - 1), VUndefined, @A[0], Argc, Res);
    PopRoots(1);
  end;
  if not Result then
  begin
    if ToStringV(Pending, S) and IsString(S) then Err := StrToUnicode(StrOf(S)) else Err := Name + ' failed';
    Pending := VUndefined;
  end;
end;

{ the natives object the host preludes share }
function HostNatives: TValue;
begin
  PushRoot(MkPtr(NewPlainObject(ObjectProto, 16)));
  DefineNative(RootAt(TempTop - 1), 'gc', Host_GC, 0);
  DefineNative(RootAt(TempTop - 1), 'gcBytes', Host_GCBytes, 0);
  DefineNative(RootAt(TempTop - 1), 'gcMaxBytes', Host_GCMaxBytes, 0);
  DefineNative(RootAt(TempTop - 1), 'gcSuggestedMaxBytes', Host_GCSuggestedMaxBytes, 0);
  DefineNative(RootAt(TempTop - 1), 'urlParse', Host_URLParse, 2);
  DefineNative(RootAt(TempTop - 1), 'urlOrigin', Host_URLOrigin, 4);
  DefineNative(RootAt(TempTop - 1), 'urlEncode', Host_URLEncode, 3);
  DefineNative(RootAt(TempTop - 1), 'isSpecial', Host_URLIsSpecial, 1);
  DefineNative(RootAt(TempTop - 1), 'defaultPort', Host_URLDefaultPort, 1);
  DefineNative(RootAt(TempTop - 1), 'formParse', Host_FormParse, 1);
  DefineNative(RootAt(TempTop - 1), 'formSerialize', Host_FormSerialize, 2);
  DefineNative(RootAt(TempTop - 1), 'plainCloneable', Host_PlainCloneable, 1);
  DefineNative(RootAt(TempTop - 1), 'viewInfo', Host_ViewInfo, 1);
  DefineNative(RootAt(TempTop - 1), 'utf8Length', Host_Utf8Length, 1);
  DefineNative(RootAt(TempTop - 1), 'utf8Write', Host_Utf8Write, 3);
  DefineNative(RootAt(TempTop - 1), 'isBytecode', Host_IsBytecode, 1);
  DefineNative(RootAt(TempTop - 1), 'hrNow', Host_HrNow, 0);
  DefineNative(RootAt(TempTop - 1), 'writeLine', Host_WriteLine, 1);
  DefineNative(RootAt(TempTop - 1), 'semver', Host_Semver, 6);
  DefineNative(RootAt(TempTop - 1), 'setClock', Host_SetClock, 2);
  DefineNative(RootAt(TempTop - 1), 'realNow', Host_RealNow, 0);
  DefineNative(RootAt(TempTop - 1), 'timeOrigin', Host_TimeOrigin, 0);
  DefineNative(RootAt(TempTop - 1), 'asyncGet', Host_AsyncGet, 0);
  DefineNative(RootAt(TempTop - 1), 'asyncSet', Host_AsyncSet, 1);
  DefineNative(RootAt(TempTop - 1), 'vitestSource', Host_VitestSource, 1);
  DefineNative(RootAt(TempTop - 1), 'testingModule', Host_TestingModule, 0);
  DefineNative(RootAt(TempTop - 1), 'astParse', Host_ASTParse, 6);
  DefineNative(RootAt(TempTop - 1), 'dataParse', Host_DataParse, 2);
  DefineNative(RootAt(TempTop - 1), 'fileURL', Host_FileURL, 1);
  DefineNative(RootAt(TempTop - 1), 'inspectDepth', Host_InspectDepth, 0);
  DefineNative(RootAt(TempTop - 1), 'specifierOf', Host_SpecifierOf, 1);
  DefineNative(RootAt(TempTop - 1), 'metaResolve', Host_MetaResolve, 3);
  DefineNative(RootAt(TempTop - 1), 'readText', Host_ReadText, 2);
  DefineNative(RootAt(TempTop - 1), 'fileSize', Host_FileSize, 1);
  DefineNative(RootAt(TempTop - 1), 'isAbsolutePath', Host_IsAbsolutePath, 1);
  DefineNative(RootAt(TempTop - 1), 'expandPath', Host_ExpandPath, 2);
  DefineNative(RootAt(TempTop - 1), 'virtualFileKey', Host_VirtualFileKey, 1);
  DefineNative(RootAt(TempTop - 1), 'moduleLoaded', Host_ModuleLoaded, 1);
  DefineNative(RootAt(TempTop - 1), 'nativeModuleRegistered', Host_NativeModuleRegistered, 1);
  DefineNative(RootAt(TempTop - 1), 'promiseState', Host_PromiseState, 1);
  DefineFetchNatives(RootAt(TempTop - 1));
  if SandboxActive then DefineSandboxNatives(RootAt(TempTop - 1));
  Result := RootAt(TempTop - 1);
  PopRoots(1);
end;

{ a new realm of a CLI host: the conformance host's extras left out, as host.js leaves them out of the first realm
  (eval stays off: no flag, config key or engine setting installs it; escape and unescape stay under compat-annex-b) }
procedure CLIRealmInit;
begin
  DeleteIfConfigurable(ObjOf(GlobalObj), AtomOf('print'));
  DeleteIfConfigurable(ObjOf(GlobalObj), AtomOf('eval'));
  if not HostAnnexB then
  begin
    DeleteIfConfigurable(ObjOf(GlobalObj), AtomOf('escape'));
    DeleteIfConfigurable(ObjOf(GlobalObj), AtomOf('unescape'));
  end;
end;

function InstallHostGlobals(ShadowRealm, TestMode: Boolean; const ConfigJson: UnicodeString;
  var Err: UnicodeString; ExposeTimers: Boolean; Bare: Boolean; AnnexB: Boolean): Boolean;
var Goccia, R: TValue; Base: Integer; T: PFuncTemplate;
begin
  if HeapCeiling <= 0 then HeapCeiling := DefaultHeapCeiling;
  RealmHostInit := @CLIRealmInit;
  HostRealm := CurRealm;
  HostAnnexB := AnnexB;
  if FirstInEngine(EventLoopRootedGen) then
  begin
    AddRoot(@FireNextV); AddRoot(@SharedV); AddRoot(@LoadHostModuleV); AddRoot(@FindHostModuleV);
    AddRoot(@DiscardTimersV);
    AddRoot(@TimerQueue); AddRoot(@TimerHostTasks);
  end;
  FireNextV := VUndefined;
  DiscardTimersV := VUndefined;
  TimerQueue := VUndefined;
  TimerHostTasks := VUndefined;
  SharedV := VUndefined;
  LoadHostModuleV := VUndefined;
  FindHostModuleV := VUndefined;
  Base := TempTop;
  PushRoot(GlobalObj);
  PushRoot(HostNatives);
  { the preludes' shared internals, runtimeGlobals the names they install }
  PushRoot(MkPtr(NewPlainObject(ObjectProto, 8)));
  PushRoot(MkPtr(NewArray(0, 16)));
  DefineOwn(RootAt(Base + 2), AtomOf('runtimeGlobals'), RootAt(TempTop - 1), pfHidden);
  PopRoots(1);
  { host.js keeps escape and unescape (compat-annex-b) }
  if AnnexB then DefineOwn(RootAt(Base + 2), AtomOf('annexB'), VTrue, pfDefault);
  PushRoot(StrFromUnicode(InfoJson));
  PushRoot(MkBool(TestMode));
  PushRoot(MkBool(ShadowRealm));
  PushRoot(StrFromUnicode(ConfigJson));
  { (G, n, shared), then host.js's (G, n, shared, infoJson, testMode, shadowRealm, configJson), in the order the
    CLIs' runtime extensions install (Goccia.runtimeGlobals lists their names in it) }
  if Bare then
  begin
    { host.js trims the global object to the bare surface (shared.bare) }
    DefineOwn(RootAt(Base + 2), AtomOf('bare'), VTrue, pfDefault);
    Result := CallPrelude('<goccia:modules>', ModulesSrc, Base, 3, R, Err) and
      CallPrelude('<goccia:web>', WebSrc, Base, 3, R, Err) and
      CallPrelude('<goccia:host>', HostSrc, Base, 7, Goccia, Err);
    { import resolution and import.meta as the CLIs have them; no host modules (goccia:*) and no timers }
    if Result then
    begin
      SharedV := RootAt(Base + 2);
      ImportMetaInitHook := @HostInitImportMeta;
      SetHostResolveHook(@HostResolveSpec);
    end;
    PopRoots(TempTop - Base);
    if not Result then Exit;
    PushRoot(Goccia);
    DefineOwn(GlobalObj, AtomOf('Goccia'), Goccia, pfHidden);
    PopRoots(1);
    Result := ParseAndCompile('const Goccia = globalThis.Goccia;', '<goccia:host>', False, T, Err, 1) and RunScript(T);
    if not Result and (Err = '') then Err := 'Goccia binding failed';
    Exit;
  end;
  { the timer globals and goccia:timers, or only the queue (ExposeTimers False): known to the host prelude too }
  if not ExposeTimers then DefineOwn(RootAt(Base + 2), AtomOf('exposeTimers'), VFalse, pfDefault);
  Result := CallPrelude('<goccia:modules>', ModulesSrc, Base, 3, R, Err) and
    CallPrelude('<goccia:console>', ConsoleSrc, Base, 3, R, Err) and
    CallPrelude('<goccia:url>', UrlSrc, Base, 3, R, Err) and CallPrelude('<goccia:web>', WebSrc, Base, 3, R, Err) and
    CallPrelude('<goccia:events>', EventsSrc, Base, 3, R, Err) and
    CallPrelude('<goccia:fetch>', FetchSrc, Base, 3, R, Err) and
    CallPrelude('<goccia:json5>', Json5Src, Base, 3, R, Err) and
    CallPrelude('<goccia:csv>', DelimitedSrc, Base, 3, R, Err) and
    CallPrelude('<goccia:jsonl>', JsonlSrc, Base, 3, R, Err) and
    CallPrelude('<goccia:yaml>', DataTreeSrc, Base, 3, R, Err) and
    CallPrelude('<goccia:semver>', SemverSrc, Base, 3, R, Err) and
    CallPrelude('<goccia:host>', HostSrc, Base, 7, Goccia, Err);
  if Result then
  begin
    PushRoot(Goccia);
    Result := CallPrelude('<goccia:timers>', TimersSrc, Base, 3, R, Err) and
      CallPrelude('<node:async_hooks>', AsyncHooksSrc, Base, 3, R, Err) and
      { sandbox mode's fs, goccia and goccia:search over the thread's virtual file system }
      (not SandboxActive or CallPrelude('<goccia:sandbox>', SandboxSrc, Base, 3, R, Err));
    Goccia := RootAt(TempTop - 1);
    { shared.timers.fireNext, for the event loop }
    if Result and GetV(RootAt(Base + 2), AtomOf('timers'), R) and IsObject(R) then
    begin
      PushRoot(R);
      Result := GetV(RootAt(TempTop - 1), AtomOf('fireNext'), FireNextV) and
        GetV(RootAt(TempTop - 1), AtomOf('discard'), DiscardTimersV) and
        GetV(RootAt(TempTop - 1), AtomOf('queue'), TimerQueue) and
        GetV(RootAt(TempTop - 1), AtomOf('hostTasks'), TimerHostTasks);
      PopRoots(1);
    end;
    SharedV := RootAt(Base + 2);
    if Result then Result := GetV(SharedV, AtomOf('loadHostModule'), LoadHostModuleV) and
      GetV(SharedV, AtomOf('findHostModule'), FindHostModuleV);
    if Result then
    begin
      SetHostModuleHook(@HostModule, @IsHostModuleKey, @NativeModuleWithoutSource, IsObject(LoadHostModuleV));
      ImportMetaInitHook := @HostInitImportMeta;
      JSONExportsHook := @HostJSONExports;
      SetHostResolveHook(@HostResolveSpec);
    end;
  end;
  PopRoots(TempTop - Base);
  if not Result then Exit;
  { Goccia: a const binding of the global scope, mirrored on the global object as the CLIs mirror theirs }
  DefineOwn(GlobalObj, AtomOf('Goccia'), Goccia, pfHidden);
  Result := ParseAndCompile('const Goccia = globalThis.Goccia;', '<goccia:host>', False, T, Err, 1) and RunScript(T);
  if not Result and (Err = '') then Err := 'Goccia binding failed';
end;

threadvar
  SnapshotState: TGocciaSnapshotState;

procedure SetTestSnapshots(const Host: IGocciaSnapshotHost; Mode: TGocciaSnapshotUpdateMode);
begin
  FreeAndNil(SnapshotState);
  if Host <> nil then SnapshotState := TGocciaSnapshotState.Create(Host, Mode);
end;

procedure TestSnapshotsDone;
begin
  FreeAndNil(SnapshotState);
end;

{ [passed, failure message] }
function SnapshotOutcome(Passed: Boolean; const Message: string; var Res: TValue): Boolean;
begin
  PushRoot(MkPtr(NewArray(0, 2)));
  ArrayPush(ObjOf(RootAt(TempTop - 1)), MkBool(Passed));
  PushRoot(StrFromUnicode(UTF8Decode(Message)));
  ArrayPush(ObjOf(RootAt(TempTop - 2)), RootAt(TempTop - 1));
  PopRoots(1);
  Res := RootAt(TempTop - 1);
  PopRoots(1);
  Result := True;
end;

{ a snapshot state's failure (its file unreadable or malformed) as the matcher's error }
function SnapshotFailed(const E: Exception): Boolean;
begin
  Result := ThrowError('Error', UnicodeString(UTF8Decode(E.Message)));
end;

{ snapshot.available() }
function Snap_Available(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
begin
  Res := MkBool(SnapshotState <> nil);
  Result := True;
end;

{ snapshot.matchExternal(testName, hint, received, propertyMatched) }
function Snap_MatchExternal(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var Msg: string; Passed: Boolean;
begin
  if SnapshotState = nil then Exit(ThrowError('Error', 'toMatchSnapshot is only available when a snapshot host is installed'));
  try
    Passed := SnapshotState.MatchExternal(U8(ArgStr(Args, Argc, 1)), U8(ArgStr(Args, Argc, 2)),
      U8(ArgStr(Args, Argc, 3)), (Argc >= 4) and ToBool(Args[4]), Msg);
  except
    on E: Exception do
    begin
      if IsEngineFault(E) then raise;
      Exit(SnapshotFailed(E));
    end;
  end;
  Result := SnapshotOutcome(Passed, Msg, Res);
end;

{ snapshot.matchInline(testName, hint, received, inline, hasInline, propertyMatched, argumentIndex): the call site is
  the user's frame that called the matcher }
function Snap_MatchInline(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var Msg: string; Passed: Boolean; Src: UnicodeString; Line, Col, ArgIndex: Integer;
begin
  if SnapshotState = nil then
    Exit(ThrowError('Error', 'toMatchInlineSnapshot is only available when a snapshot host is installed'));
  Src := '';
  Line := 0;
  Col := 0;
  CallSiteNow(Src, Line, Col);
  ArgIndex := 0;
  if (Argc >= 7) and IsNumber(Args[7]) then ArgIndex := Trunc(NumOf(Args[7]));
  try
    Passed := SnapshotState.MatchInline(U8(ArgStr(Args, Argc, 1)), U8(ArgStr(Args, Argc, 2)),
      U8(ArgStr(Args, Argc, 3)), U8(ArgStr(Args, Argc, 4)), (Argc >= 5) and ToBool(Args[5]),
      (Argc >= 6) and ToBool(Args[6]), U8(Src), Line, Col, ArgIndex, Msg);
  except
    on E: Exception do
    begin
      if IsEngineFault(E) then raise;
      Exit(SnapshotFailed(E));
    end;
  end;
  Result := SnapshotOutcome(Passed, Msg, Res);
end;

{ snapshot.preserve(testName): a skipped or todo test keeps its snapshots }
function Snap_Preserve(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
begin
  if SnapshotState <> nil then SnapshotState.PreserveTestSnapshots(U8(ArgStr(Args, Argc, 1)));
  Res := VUndefined;
  Result := True;
end;

{ snapshot.finish(processUnchecked): the file's snapshots written and the obsolete ones handled; the errors, a list
  of strings }
function Snap_Finish(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var Errors: TStringList; I: Integer;
begin
  Errors := TStringList.Create;
  try
    if SnapshotState <> nil then
      try
        SnapshotState.Finish(Errors, (Argc < 1) or ToBool(Args[1]));
      except
        on E: Exception do
        begin
          if IsEngineFault(E) then raise;
          Errors.Add('Snapshot finalization failed: ' + E.Message);
        end;
      end;
    PushRoot(MkPtr(NewArray(0, Errors.Count)));
    for I := 0 to Errors.Count - 1 do
    begin
      PushRoot(StrFromUnicode(UTF8Decode(Errors[I])));
      ArrayPush(ObjOf(RootAt(TempTop - 2)), RootAt(TempTop - 1));
      PopRoots(1);
    end;
    Res := RootAt(TempTop - 1);
    PopRoots(1);
  finally
    Errors.Free;
  end;
  Result := True;
end;

function RunEventLoopIn(Intervals: Boolean): Boolean; forward;

{ deadline.callWithin(fn, ms): a test's body under its own deadline (--test-timeout), the work it leaves pending run
  here too, so a wait (a fetch) counts against it; ['ok', the promise or value fn returned], ['threw', the error], or
  ['timeout'] once the deadline ended it. A file deadline sooner than ms stays the one in force, and ends the run. }
function Test_CallWithin(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var Ms, Outer, Elapsed: Int64; NoOuter, Binding, Ok: Boolean; Start: QWord; R, E: TValue; Status: UnicodeString;
  Mark: Integer; HostMark: QWord; Work: TEmbedderWork;
begin
  if (Argc < 1) or not IsCallable(Args[1]) then Exit(ThrowTypeError('callWithin needs a function'));
  Ms := 0;
  if (Argc >= 2) and IsNumber(Args[2]) then Ms := Trunc(NumOf(Args[2]));
  Outer := LimitRemainingMs;
  NoOuter := Outer = 0;
  Binding := (Ms > 0) and (NoOuter or (Ms < Outer));
  if Binding then LimitsSetDeadline(Ms);
  Mark := FetchMark;
  Work := EmbedderWork;
  HostMark := 0;
  if Work <> nil then HostMark := Work.Mark;
  Start := GetTickCount64;
  R := VUndefined;
  Ok := CallValue(Args[1], VUndefined, nil, 0, R);
  PushRoot(R);
  if Ok then Ok := RunEventLoopIn(False);
  Elapsed := Int64(GetTickCount64 - Start);
  E := VUndefined;
  if not Ok and Binding and (AbortKind = 1) then
  begin
    { the timer stops first: a deadline seen before its thread fired it must not fire into what runs next }
    LimitsDropTimer;
    ClearTimeoutAbort;
    Pending := VUndefined;
    { the requests the body started are not waited for after it: their promises stay pending }
    FetchCancelFrom(Mark);
    if (Work <> nil) and (Work = EmbedderWork) then Work.DiscardFrom(HostMark);
    Status := 'timeout';
  end
  else if not Ok then
  begin
    if Aborting then
    begin
      { another limit ended the run: it goes on unwinding }
      PopRoots(1);
      if Binding and NoOuter then LimitsDropTimer;
      Exit(False);
    end;
    E := Pending;
    Pending := VUndefined;
    Status := 'threw';
  end
  else
  begin
    { a deadline that fired as the body finished is not a timeout }
    if Binding then ClearTimeoutAbort;
    Status := 'ok';
  end;
  if Binding then
    if NoOuter then LimitsDropTimer
    else if Outer - Elapsed > 0 then LimitsSetDeadline(Outer - Elapsed)
    else LimitsSetDeadline(1);
  PushRoot(E);
  PushRoot(MkPtr(NewArray(0, 2)));
  PushRoot(StrFromUnicode(Status));
  ArrayPush(ObjOf(RootAt(TempTop - 2)), RootAt(TempTop - 1));
  PopRoots(1);
  if Status = 'ok' then ArrayPush(ObjOf(RootAt(TempTop - 1)), RootAt(TempTop - 3))
  else if Status = 'threw' then ArrayPush(ObjOf(RootAt(TempTop - 1)), RootAt(TempTop - 2));
  Res := RootAt(TempTop - 1);
  PopRoots(3);
  Result := True;
end;

{ work.mark(): the rejection mark of the unit starting (Goccia.Promise RejectionMark) }
function Work_Mark(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
begin
  Res := MkNumber(RejectionMark);
  Result := True;
end;

{ work.take(mark): the promises rejected without a handler since the mark, no longer tracked for the run }
function Work_Take(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var From: Integer;
begin
  From := 0;
  if (Argc >= 1) and IsNumber(Args[1]) then From := Trunc(NumOf(Args[1]));
  Res := TakeRejections(From);
  Result := True;
end;

{ work.unhandled(list): [true, reason] for the first promise of the list that still has no handler, else [false] }
function Work_Unhandled(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var Reason: TValue; Found: Boolean;
begin
  Reason := VUndefined;
  Found := (Argc >= 1) and FirstUnhandledOf(Args[1], Reason);
  PushRoot(Reason);
  PushRoot(MkPtr(NewArray(0, 2)));
  ArrayPush(ObjOf(RootAt(TempTop - 1)), MkBool(Found));
  if Found then ArrayPush(ObjOf(RootAt(TempTop - 1)), RootAt(TempTop - 2));
  Res := RootAt(TempTop - 1);
  PopRoots(2);
  Result := True;
end;

{ work.jobErrors(): what the queued callbacks threw since the last call, an array }
function Work_JobErrors(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
begin
  Res := TakeJobErrors;
  if not IsObject(Res) then Res := MkPtr(NewArray(0, 0));
  Result := True;
end;

{ work.collected(): the suites are collected; from now on a job that throws is only recorded }
function Work_Collected(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
begin
  JobErrorsDropWork := False;
  Res := VUndefined;
  Result := True;
end;

{ work.discard(): the queued jobs dropped (what a failed unit left must not run in the next one) }
function Work_Discard(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
begin
  DiscardJobs;
  Res := VUndefined;
  Result := True;
end;

{ deadline.settle(p) (runTests of goccia:test outside the test runner): the jobs and timers run until none is left,
  the job errors a test leaves attributed to its unit as the test runner attributes them; p's value, or its
  rejection thrown }
function Test_Settle(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var P, V: TValue; SavedToHost, SavedDrop, Ok: Boolean;
begin
  if Argc >= 1 then P := Args[1] else P := VUndefined;
  if not IsPromiseValue(P) then begin Res := P; Exit(True); end;
  PushRoot(P);
  SavedToHost := JobErrorsToHost;
  SavedDrop := JobErrorsDropWork;
  JobErrorsToHost := True;
  JobErrorsDropWork := False;
  Ok := RunEventLoopIn(True);
  JobErrorsToHost := SavedToHost;
  JobErrorsDropWork := SavedDrop;
  P := RootAt(TempTop - 1);
  PopRoots(1);
  if not Ok then Exit(False);
  case PromiseStatus(P, V) of
    1: begin Res := V; Result := True; end;
    2: begin Pending := V; Result := False; end;
  else
    Result := ThrowError('Error', 'runTests did not finish: a test waits on work that never comes');
  end;
end;

{ deadline.clock(): nanoseconds on the monotonic clock (runTests' duration) }
function Test_Clock(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
begin
  Res := MkNumber(GetNanoseconds);
  Result := True;
end;

{ deadline.callTest(fn): a test function called with --max-stack counted from its own frame }
function Test_CallTest(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
begin
  if (Argc < 1) or not IsCallable(Args[1]) then Exit(ThrowTypeError('callTest needs a function'));
  Res := VUndefined;
  Result := CallRebased(Args[1], VUndefined, Res);
end;

function InstallTestingAPI(var Err: UnicodeString): Boolean;
var Base: Integer;
begin
  if NativeModuleRegistered('goccia:test') then
  begin
    Err := 'Module is already registered: goccia:test';
    Exit(False);
  end;
  if FirstInEngine(TestApiRootedGen) then AddRoot(@TestApiV);
  Base := TempTop;
  { shared.snapshot: the snapshot natives the matchers call }
  PushRoot(MkPtr(NewPlainObject(ObjectProto, 8)));
  DefineNative(RootAt(TempTop - 1), 'available', Snap_Available, 0);
  DefineNative(RootAt(TempTop - 1), 'matchExternal', Snap_MatchExternal, 4);
  DefineNative(RootAt(TempTop - 1), 'matchInline', Snap_MatchInline, 7);
  DefineNative(RootAt(TempTop - 1), 'preserve', Snap_Preserve, 1);
  DefineNative(RootAt(TempTop - 1), 'finish', Snap_Finish, 1);
  { --heap-snapshots: expect() hands what it is given to the round trip (Goccia.HeapSnapshot) }
  if HeapObserving then DefineNative(RootAt(TempTop - 1), 'observe', SnapshotObserve, 1);
  DefineOwn(SharedV, AtomOf('snapshot'), RootAt(TempTop - 1), pfDefault);
  PopRoots(1);
  { shared.deadline: the per-test timeout and the runner's settings }
  PushRoot(MkPtr(NewPlainObject(ObjectProto, 4)));
  DefineNative(RootAt(TempTop - 1), 'callWithin', Test_CallWithin, 2);
  DefineOwn(RootAt(TempTop - 1), AtomOf('testTimeoutMs'), MkNumber(TestTimeoutMs), pfDefault);
  DefineOwn(RootAt(TempTop - 1), AtomOf('describeTimeoutMs'), MkNumber(DescribeTimeoutMs), pfDefault);
  DefineOwn(RootAt(TempTop - 1), AtomOf('reporter'), MkBool(TestReporterOn), pfDefault);
  DefineNative(RootAt(TempTop - 1), 'print', Host_WriteLine, 1);
  DefineNative(RootAt(TempTop - 1), 'callTest', Test_CallTest, 1);
  DefineNative(RootAt(TempTop - 1), 'settle', Test_Settle, 1);
  DefineNative(RootAt(TempTop - 1), 'clock', Test_Clock, 0);
  DefineOwn(SharedV, AtomOf('deadline'), RootAt(TempTop - 1), pfDefault);
  PopRoots(1);
  { shared.work: the host work each unit leaves (rejections, job errors), attributed to it }
  PushRoot(MkPtr(NewPlainObject(ObjectProto, 8)));
  DefineNative(RootAt(TempTop - 1), 'mark', Work_Mark, 0);
  DefineNative(RootAt(TempTop - 1), 'take', Work_Take, 1);
  DefineNative(RootAt(TempTop - 1), 'unhandled', Work_Unhandled, 1);
  DefineNative(RootAt(TempTop - 1), 'jobErrors', Work_JobErrors, 0);
  DefineNative(RootAt(TempTop - 1), 'discard', Work_Discard, 0);
  DefineNative(RootAt(TempTop - 1), 'collected', Work_Collected, 0);
  DefineOwn(SharedV, AtomOf('work'), RootAt(TempTop - 1), pfDefault);
  PopRoots(1);
  DefineOwn(SharedV, AtomOf('moduleOnly'), MkBool(TestModuleOnly), pfDefault);
  { a test file's queued callbacks that throw fail the unit that queued them, not the run (module-only: only while
    runTests runs, deadline.settle) }
  if not TestModuleOnly then
  begin
    JobErrorsToHost := True;
    JobErrorsDropWork := True;
  end;
  PushRoot(GlobalObj);
  PushRoot(SharedV);
  Result := CallPrelude('<goccia:test>', TestingSrc, Base, 2, TestApiV, Err);
  PopRoots(2);
end;

{ report's done(json, error) }
function Test_Done(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var S: TValue;
begin
  ReportDone := True;
  if (Argc >= 1) and IsString(Args[1]) then ReportJson := StrToUnicode(StrOf(Args[1]))
  else if (Argc >= 2) and ToStringV(Args[2], S) and IsString(S) then ReportErr := StrToUnicode(StrOf(S))
  else ReportErr := 'the test run failed';
  Res := VUndefined;
  Result := True;
end;

procedure HoistTestMocks(const FileName: string; const Source: UnicodeString);
var Lines: TStringList; R: TMockResolver; Mocks: TGocciaMockModules; I: Integer;
begin
  if (Pos('vi.mock', Source) = 0) and (Pos('vi.unmock', Source) = 0) and (Pos('vitest.mock', Source) = 0) and
     (Pos('vitest.unmock', Source) = 0) then Exit;
  Lines := CreateFileTextLines(NormalizeNewlinesToLF(UTF8Encode(Source)));
  R := TMockResolver.Create;
  try
    Mocks := CollectModuleMocks(Lines, ExpandFileName(FileName), [ppJSX], R.Resolve);
  finally
    R.Free;
    Lines.Free;
  end;
  if Length(Mocks) = 0 then Exit;
  if not Assigned(MockModules) then
  begin
    MockModules := TStringList.Create;
    MockModules.NameValueSeparator := #1;
  end;
  for I := 0 to High(Mocks) do MockModules.Add(Mocks[I].Address + #1 + Mocks[I].Source);
end;

function PumpEmbedderWork(TimeoutMs: Cardinal): Boolean;
var Work: TEmbedderWork; Queued: Integer;
begin
  Work := EmbedderWork;
  { the deadline is read here too, not only from the timer thread's flag, which it sets once it is scheduled }
  if (Interrupt <> 0) or LimitDeadlinePassed then Exit(AbortInterrupted);
  Queued := PendingJobCount;
  Result := Work.Pump(TimeoutMs);
  { a pump that failed, or during which the deadline passed, delivered nothing the run goes on with: the reactions
    its results queued do not run, as a job taken after an abort does not (a timed-out test's continuation stays
    stopped, and the next run on the engine does not see them) }
  if not Result then DiscardJobsAfter(Queued);
  if (Interrupt <> 0) or LimitDeadlinePassed then
  begin
    DiscardJobsAfter(Queued);
    Result := AbortInterrupted;
  end;
end;

function EmbedderWorkWait: Cardinal;
const MaxWaitMs = 10;
var Remaining: Int64; Due, Now_: QWord;
begin
  Result := MaxWaitMs;
  Remaining := LimitRemainingMs;
  if (Remaining > 0) and (Remaining < Result) then Result := Remaining;
  if not AsyncWaitsPending then Exit;
  Due := NextAsyncDeadline;
  Now_ := GetTickCount64;
  if Due <= Now_ then Exit(0);
  if Due - Now_ < Result then Result := Due - Now_;
end;

function RunEventLoopIn(Intervals: Boolean): Boolean;
var R: TValue; A: array[0..1] of TValue; Due, Cap: QWord; HostPending, TimersIdle: Boolean; IdleCounter: Int64;
begin
  { set once the timers had nothing runnable while host work waits, with the instruction counter then: under an
    instruction limit, idle turns do not run the timers' bytecode again until some guest code (a job, a getter a pump
    ran, a timer's own work) has run and moved the counter }
  TimersIdle := False;
  IdleCounter := 0;
  while True do
  begin
    if not RunMicrotasks then Exit(False);
    HostPending := EmbedderWorkPending;
    if HostPending then
    begin
      if not PumpEmbedderWork(0) or not RunMicrotasks then Exit(False);
      if not EmbedderWorkPending then Continue;
      { while an embedder can make progress, take only ready built-in work before returning to it }
      PumpAsyncWaits;
      if not RunMicrotasks then Exit(False);
      if FetchPending and (not FetchPump(True, GetTickCount64) or not RunMicrotasks) then Exit(False);
      if not EmbedderWorkPending then Continue;
    end
    { async waits and fetches both pending: the waits that fell due settle, then the fetches are waited for until
      the next wait's timeout, so neither holds the other up }
    else if FetchPending and AsyncWaitsPending then
    begin
      PumpAsyncWaits;
      if not RunMicrotasks then Exit(False);
      if EmbedderWorkPending then Continue;
      if not FetchPending then Continue;
      Due := NextAsyncDeadline;
      Cap := GetTickCount64 + 50;
      if Due > Cap then Due := Cap;
      if not FetchPump(True, Due) then Exit(False);
      Continue;
    end
    else
    begin
      if not RunHostLoop(@EmbedderWorkPending) then Exit(False);
      if EmbedderWorkPending then Continue;
    end;
    { a FinalizationRegistry's cleanup: one job (one dead cell) at a time, the microtasks it queues before the
      next (9.13 HostEnqueueFinalizationRegistryCleanupJob) }
    if WeakCleanupPending then
    begin
      ClearKeptObjects;
      if not RunWeakCleanup then Exit(False);
      Continue;
    end;
    { the fetches that finished settle first; while any is in flight the loop waits for it before the timers, which
      do not keep real time }
    if FetchPending then
    begin
      if EmbedderWorkPending then
      begin
        if not PumpEmbedderWork(EmbedderWorkWait) then Exit(False);
      end
      else if not FetchPump(True) then Exit(False);
      Continue;
    end;
    if TimersIdle and not (InstrCounted and (InstrLeft = IdleCounter)) then TimersIdle := False;
    if IsObject(FireNextV) and (not HostPending or JobErrorDropsTimers or
      (IsObject(TimerHostTasks) and (ArrayLen(ObjOf(TimerHostTasks)) > 0)) or
      (not TimersIdle and IsObject(TimerQueue) and (MapCount(TimerQueue) > 0))) then
    begin
      { a test file's job that threw took the pending timers with it }
      if JobErrorDropsTimers then
      begin
        JobErrorDropsTimers := False;
        if IsCallable(DiscardTimersV) and not CallValue(DiscardTimersV, VUndefined, nil, 0, R) then Exit(False);
      end;
      ClearKeptObjects;
      { after the test report, the work the tests left still runs: its timers, not its intervals }
      A[0] := MkBool(Intervals and not ReportDone);
      { real outstanding host work comes before virtual time, as an in-flight fetch does (ADR 0113): while it waits,
        only a timer already due runs }
      A[1] := MkBool(HostPending);
      if not CallValue(FireNextV, VUndefined, @A[0], 2, R) then Exit(False);
      if R = VTrue then Continue;
      { only intervals or fake timers wait: nothing changes for them until guest code runs }
      TimersIdle := HostPending and InstrCounted;
      IdleCounter := InstrLeft;
    end;
    if EmbedderWorkPending then
    begin
      if not PumpEmbedderWork(EmbedderWorkWait) then Exit(False);
      Continue;
    end;
    if not IsObject(FireNextV) then Exit(True);
    { nothing left to run: while a FinalizationRegistry cell still waits on its target, the host collects once
      (it may at any time, 9.13), so a target that died while nothing allocated still has its cleanup run (a
      sandbox caller's, whose nested run, an engine of its own, collects nothing of the caller's) }
    if not WeakCellsWaiting then Exit(True);
    CollectMinor;
    CollectMajor;
    if not WeakCleanupPending then Exit(True);
  end;
end;

function RunEventLoop: Boolean;
begin
  Result := RunEventLoopIn(False);
end;

procedure DiscardFailedRunWork(WeakMark, ModuleMark: Int64; Thrown: TValue; const FailText: UnicodeString;
  HasThrown: Boolean);
begin
  if ModuleEvaluationsPendingSince(ModuleMark) then
  begin
    { an abort's own error: a job that met the limit may have taken the one it threw. Pending stays the run's. }
    if AbortKind = 3 then Thrown := MakeError(RangeErrorProto, AbortErrorMessage)
    else if AbortKind <> 0 then Thrown := MakeError(ErrorProto, AbortErrorMessage)
    else if not HasThrown then
      if HostErrorHeld and SameEngine(HostErrorRootedGen) then Thrown := HostErrorV
      else Thrown := MakeError(ErrorProto, FailText);
    FailModuleEvaluationsSince(ModuleMark, Thrown);
  end;
  DiscardEmbedderWork;
  DiscardJobs;
  DiscardRejections;
  TakeJobErrors;
  JobErrorDropsTimers := False;
  if SameEngine(EventLoopRootedGen) then
  begin
    if IsObject(TimerQueue) then DiscardMapEntries(TimerQueue);
    if IsObject(TimerHostTasks) then ArraySetLength(ObjOf(TimerHostTasks), 0);
  end;
  DiscardFetches;
  DiscardAsyncWaits;
  DiscardWeakCleanup(WeakMark);
end;

function RunTestSuites(var Json, Err: UnicodeString): Boolean;
var A: array[0..0] of TValue; Report, R: TValue;
begin
  ReportJson := ''; ReportErr := ''; ReportDone := False;
  if not GetV(TestApiV, AtomOf('report'), Report) then Exit(False);
  { report is read again from its root after the done function's allocation (a collection moves it) }
  PushRoot(Report);
  PushRoot(NewNativeFunction(Test_Done, 'done', 2));
  A[0] := RootAt(TempTop - 1);
  Report := RootAt(TempTop - 2);
  Result := CallValue(Report, TestApiV, @A[0], 1, R) and RunEventLoopIn(True);
  PopRoots(2);
  if not Result then
  begin
    Err := TakePendingString;
    Exit;
  end;
  if not ReportDone then Exit(False);
  Json := ReportJson;
  Err := ReportErr;
  Result := ReportErr = '';
end;

{ ---- the benchmark API (prelude/microbench.js) ---- }

{ n.pendingJobs(): the jobs queued and not yet run, so a synchronous batch drains the jobs its calls queue instead of
  holding all of them until it ends }
function Bench_PendingJobs(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
begin
  Res := MkNumber(PendingJobCount);
  Result := True;
end;

threadvar
  BenchApiV: TValue;
  BenchApiRootedGen: Integer;
  { the clock's origin: now() counts from the install, so its nanoseconds stay exact in a double }
  BenchClockBase: Int64;
  BenchEntries: TGocciaBenchEntries;
  BenchEntryCount: Integer;
  BenchTotal: Integer;
  BenchDuration: Int64;

{ a setting as GocciaBenchmarkRunner reads it: the variable's integer, else the default }
function BenchEnvInt(const Name: string; Default: Integer): Integer;
begin
  if not TryStrToInt(GetEnvironmentVariable(Name), Result) then Result := Default;
end;

function ArgNum(Args: PValue; Argc, I: Integer): Double;
begin
  if (I <= Argc) and IsNumber(Args[I]) then Result := NumOf(Args[I]) else Result := 0;
end;

{ now(): nanoseconds on the monotonic clock since the API was installed }
function Bench_Now(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
begin
  Res := MkNumber(GetNanoseconds - BenchClockBase);
  Result := True;
end;

{ forget(): the promises the bodies left rejected without a handler, no longer tracked. A benchmark measures and does
  not assert, so nothing would ever take them (Goccia.Builtins.Benchmark SettleBatch) }
function Bench_Forget(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
begin
  TakeRejections(0);
  Res := VUndefined;
  Result := True;
end;

{ progress(suite, name, index, total): the host's progress line }
function Bench_Progress(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
begin
  if Assigned(BenchProgress) then
    CallBenchProgress(ArgStr(Args, Argc, 1), ArgStr(Args, Argc, 2), Trunc(ArgNum(Args, Argc, 3)),
      Trunc(ArgNum(Args, Argc, 4)));
  Res := VUndefined;
  Result := True;
end;

{ entry(suite, name, summaryScope, boxplotScope, failed, thrown, iterations, opsPerSec, meanMs, variancePercentage,
  setupMs, teardownMs, minOpsPerSec, maxOpsPerSec, sampleCount, minSampleMs, p25Ms, medianMs, p75Ms, p99Ms,
  p999Ms, maxSampleMs): one result (for a benchmark that failed, what it threw and no numbers) }
function Bench_Entry(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var E: TGocciaBenchEntry;
begin
  E := Default(TGocciaBenchEntry);
  E.Suite := ArgStr(Args, Argc, 1);
  E.Name := ArgStr(Args, Argc, 2);
  E.SummaryScope := Trunc(ArgNum(Args, Argc, 3));
  E.BoxplotScope := Trunc(ArgNum(Args, Argc, 4));
  E.Failed := (Argc >= 5) and (Args[5] = VTrue);
  if E.Failed then
  begin
    if (Argc >= 6) and Assigned(BenchDescribeThrown) then
      CallBenchDescribeThrown(Args[6], E);
    if E.Error = '' then
      E.Error := 'benchmark failed';
  end
  else
  begin
    E.Iterations := Trunc(ArgNum(Args, Argc, 7));
    E.OpsPerSec := ArgNum(Args, Argc, 8);
    E.MeanMs := ArgNum(Args, Argc, 9);
    E.VariancePercentage := ArgNum(Args, Argc, 10);
    E.SetupMs := ArgNum(Args, Argc, 11);
    E.TeardownMs := ArgNum(Args, Argc, 12);
    E.MinOpsPerSec := ArgNum(Args, Argc, 13);
    E.MaxOpsPerSec := ArgNum(Args, Argc, 14);
    E.SampleCount := Trunc(ArgNum(Args, Argc, 15));
    E.MinSampleMs := ArgNum(Args, Argc, 16);
    E.P25Ms := ArgNum(Args, Argc, 17);
    E.MedianMs := ArgNum(Args, Argc, 18);
    E.P75Ms := ArgNum(Args, Argc, 19);
    E.P99Ms := ArgNum(Args, Argc, 20);
    E.P999Ms := ArgNum(Args, Argc, 21);
    E.MaxSampleMs := ArgNum(Args, Argc, 22);
  end;
  if BenchEntryCount = Length(BenchEntries) then
    SetLength(BenchEntries, 2 * BenchEntryCount + 8);
  BenchEntries[BenchEntryCount] := E;
  Inc(BenchEntryCount);
  Res := VUndefined;
  Result := True;
end;

{ report's done(total, durationNanoseconds) or done(undefined, undefined, error) }
function Bench_Done(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
begin
  ReportDone := True;
  if (Argc >= 1) and IsNumber(Args[1]) then
  begin
    BenchTotal := Trunc(NumOf(Args[1]));
    BenchDuration := Trunc(ArgNum(Args, Argc, 2));
  end
  else
  begin
    ReportErr := ArgStr(Args, Argc, 3);
    if ReportErr = '' then ReportErr := 'the benchmark run failed';
  end;
  Res := VUndefined;
  Result := True;
end;

function InstallBenchmarkAPI(var Err: UnicodeString): Boolean;
var Base, Rounds: Integer;
begin
  if NativeModuleRegistered('goccia:microbench') then
  begin
    Err := 'Module is already registered: goccia:microbench';
    Exit(False);
  end;
  if FirstInEngine(BenchApiRootedGen) then AddRoot(@BenchApiV);
  BenchClockBase := GetNanoseconds;
  BenchEntries := nil;
  BenchEntryCount := 0;
  Rounds := BenchEnvInt('GOCCIA_BENCH_ROUNDS', 7);
  if Rounds > 50 then Rounds := 50;
  if Rounds < 1 then Rounds := 1;
  Base := TempTop;
  PushRoot(GlobalObj);
  PushRoot(SharedV);
  PushRoot(MkPtr(NewPlainObject(ObjectProto, 16)));
  DefineNative(RootAt(TempTop - 1), 'now', Bench_Now, 0);
  DefineNative(RootAt(TempTop - 1), 'collect', Host_GC, 0);
  DefineNative(RootAt(TempTop - 1), 'forget', Bench_Forget, 0);
  DefineNative(RootAt(TempTop - 1), 'discardJobs', Work_Discard, 0);
  DefineNative(RootAt(TempTop - 1), 'pendingJobs', Bench_PendingJobs, 0);
  DefineNative(RootAt(TempTop - 1), 'progress', Bench_Progress, 4);
  DefineNative(RootAt(TempTop - 1), 'entry', Bench_Entry, 22);
  DefineOwn(RootAt(TempTop - 1), AtomOf('progressOn'), MkBool(Assigned(BenchProgress)), pfDefault);
  DefineOwn(RootAt(TempTop - 1), AtomOf('warmup'), MkNumber(BenchEnvInt('GOCCIA_BENCH_WARMUP', 5)), pfDefault);
  DefineOwn(RootAt(TempTop - 1), AtomOf('calibrationMs'),
    MkNumber(BenchEnvInt('GOCCIA_BENCH_CALIBRATION_MS', 200)), pfDefault);
  DefineOwn(RootAt(TempTop - 1), AtomOf('calibrationBatch'),
    MkNumber(BenchEnvInt('GOCCIA_BENCH_CALIBRATION_BATCH', 5)), pfDefault);
  DefineOwn(RootAt(TempTop - 1), AtomOf('rounds'), MkNumber(Rounds), pfDefault);
  Result := CallPrelude('<goccia:microbench>', MicrobenchSrc, Base, 3, BenchApiV, Err);
  PopRoots(3);
end;

function RunBenchmarkSuites(Deterministic: Boolean; out Entries: TGocciaBenchEntries; out Total: Integer;
  out DurationNanoseconds: Int64; var Err: UnicodeString): Boolean;
var A: array[0..1] of TValue; Report, R: TValue;
begin
  Entries := nil;
  Total := 0;
  DurationNanoseconds := 0;
  ReportJson := ''; ReportErr := ''; ReportDone := False;
  BenchEntryCount := 0;
  BenchTotal := 0;
  BenchDuration := 0;
  if not GetV(BenchApiV, AtomOf('report'), Report) then Exit(False);
  PushRoot(NewNativeFunction(Bench_Done, 'done', 3));
  A[0] := MkBool(Deterministic);
  A[1] := RootAt(TempTop - 1);
  Result := CallValue(Report, BenchApiV, @A[0], 2, R) and RunEventLoop;
  PopRoots(1);
  if not Result then
  begin
    Err := TakePendingString;
    Exit;
  end;
  if not ReportDone then
  begin
    Err := 'the benchmarks did not finish';
    Exit(False);
  end;
  Err := ReportErr;
  Result := ReportErr = '';
  Entries := Copy(BenchEntries, 0, BenchEntryCount);
  Total := BenchTotal;
  DurationNanoseconds := BenchDuration;
end;

{ the managed threadvars of this unit, released when the thread ends (Goccia.ThreadCleanupRegistry): FPC does not
  finalize a threadvar, and every engine thread leaves its strings, arrays and objects behind otherwise }
procedure GocciaHostAPIThreadvarCleanup;
begin
  BenchEntries := nil;
  ReadPolicy.Free;
  ReadPolicy := nil;
  MockModules.Free;
  MockModules := nil;
  ReportJson := '';
  ReportErr := '';
  ResolveCandidate := '';
  ImportSpecifiers.Free;
  ImportSpecifiers := nil;
  ShadowWarned.Free;
  ShadowWarned := nil;
  ManifestWritten := '';
  ManifestPath := '';
  GuestScopeDir := '';
end;

initialization
  RegisterThreadvarCleanup(@GocciaHostAPIThreadvarCleanup);
  { a module load failure records the path its missing module resolved to }
  TakeLoadFailureResolved := @TakeResolveCandidate;
  { SysUtils fills TrueBoolStrs and FalseBoolStrs on the first BoolToStr(B, True), without a lock: fill them here, on
    the main thread before any engine thread starts, so every later call only reads them }
  BoolToStr(True, True);
finalization
  HostModulesDone;
end.
