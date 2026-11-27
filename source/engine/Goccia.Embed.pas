unit Goccia.Embed;
{ The engine as a host embeds it: one TGocciaEngine per thread at a time (its state is the thread's, Goccia.Engine).
  Create builds the engine; the settings (language profile, source kind, host config, limits) apply to the run
  that follows; Run compiles and runs one entry file as a script or a module, then its jobs and timers (the
  event loop), and reports how it ended. The command-line hosts (goccia.dpr, and the apps through the host engine)
  build on this instead of on the engine's units. }
{$mode delphiunicode}{$H+}
{$I Goccia.Checks.inc}

interface

uses
  SysUtils, Goccia.Capabilities, Goccia.CapabilityAudit, Goccia.HostEnvironment, Goccia.ModuleResolver, Goccia.Core, Goccia.HostAPI, Goccia.Modules, Goccia.VM,
  Goccia.Instructions, Goccia.InstructionBudget, Goccia.Artifact;

type
  { a console line and the method that wrote it (Goccia.HostAPI TConsoleSink) }
  TGocciaConsoleEvent = TConsoleSink;
  TGocciaNativeModuleFactory = TNativeModuleFactory;
  TGocciaEmbedderWork = TEmbedderWork;
  { how the entry's completion value is reported: not at all, as String(value) (--print), or as JSON (a CLI's JSON
    report: JSON.stringify's text, with undefined as null and a function or what JSON leaves out as its quoted
    String form) }
  TGocciaResultForm = (grfNone, grfString, grfJSON, grfSandbox, grfREPL, grfREPLColor);
  { a warning of the entry's parse (warning-unsupported-features), given to the host before the entry runs }
  TGocciaWarningEvent = procedure(const Message, Suggestion: UnicodeString; Line, Column: Integer) of object;
  { a warning of an imported module's parse, given as the module compiles (FileName the module's path) }
  TGocciaModuleWarningEvent = procedure(const FileName, Message, Suggestion: UnicodeString; Line, Column: Integer)
    of object;

  { how the entry is compiled: a script, a module, or as the source shows (a top-level import or export
    declaration makes a module); a .mjs file is a module unless the kind says otherwise }
  TGocciaSourceKind = (gskDetect, gskScript, gskModule);
  { a run ended by a spent limit (Goccia.VM LimitsArm) rather than by the script }
  TGocciaAbort = (gaNone, gaTimeout, gaInstructionLimit,
    { eight memory refusals in a row (MaxMemory) freed nothing; ErrorText says how much }
    gaMemoryLimit,
    { the host ended the run, past every handler: AuditSink could not record a decision (ErrorText says why) }
    gaHostAbort);

  { the engine's heap now (Memory): what a CLI's JSON report shows under memory.gc }
  TGocciaMemoryStats = record
    { the heap's bytes, garbage included until a collection frees it; the most it has held }
    LiveBytes, PeakLiveBytes: Int64;
    { every byte the heap has allocated since the engine was made }
    AllocatedBytes: Int64;
    { the heap limit (MaxMemory, else the default ceiling) }
    LimitBytes: Int64;
    Collections: Integer;
  end;

  { a heap snapshot round trip's object (TGocciaEngine.HeapSnapshotObserved): what the source engine wrote of it and
    how it described it, or why it refused }
  TGocciaHeapRoot = record
    Data: TBytes;
    Description: UnicodeString;
    Refusal: UnicodeString;
  end;
  { a restored object's outcome (TGocciaEngine.HeapSnapshotVerify) }
  TGocciaHeapVerdict = (ghvPassed, ghvRefused, ghvFailed);

  TGocciaRunResult = record
    Ok: Boolean;
    Aborted: TGocciaAbort;
    { the entry was compiled: False for a syntax error, whose message is in ErrorMessage }
    Compiled: Boolean;
    IsModule: Boolean;
    { the uncaught error (ToString), or the limit's message when Aborted }
    ErrorMessage: UnicodeString;
    { the error in parts, for a host's diagnostic: its name and message (ErrorIsError: the thrown value was an
      object with a string name or message, or a syntax error; otherwise ErrorMessage is the thrown value as a
      string), where it was made (the link declaration, first engine-recorded stack frame, or where the source
      was rejected; ErrorLine 0 when unknown) and the parser's suggestion }
    ErrorIsError: Boolean;
    ErrorName, ErrorText, ErrorFile, ErrorSuggestion: UnicodeString;
    ErrorLine, ErrorColumn: Integer;
    { a static import that failed to load: the path its resolution tried, for the host's "Resolved to" line }
    ErrorResolvedPath: UnicodeString;
    { nanoseconds spent parsing and compiling the entry (lexing included), and running it with its event loop }
    CompileNanoseconds, ExecuteNanoseconds: Int64;
    { Dispatch and native work charged while running this entry and its event loop; with a shared
      InstructionBudget attached, the job's count so far instead (do not add those up across engines) }
    HasInstructionCount: Boolean;
    InstructionCount: QWord;
    { ResultForm: the entry's completion value as String(value) or JSON (a module's too: the value of its last
      expression statement), converted after the event loop under the run's limits; a conversion that throws fails
      the run }
    HasResult: Boolean;
    ResultText: UnicodeString;
  end;

  TGocciaEngine = class
  private
    FProfile: TLangProfile;
    FSourceKind: TGocciaSourceKind;
    FConfigJson: UnicodeString;
    FTestMode: Boolean;
    FBenchMode: Boolean;
    FHeapSnapshots: Boolean;
    FTimeoutMs, FMaxInstructions, FMaxMemory: Int64;
    FMeasureInstructions: Boolean;
    FInstructionBudget: IGocciaInstructionBudget;
    FSetupAborted: TGocciaAbort;
    FMaxFrames: Integer;
    FHostReady: Boolean;
    FExposeTimers: Boolean;
    FBare: Boolean;
    FResultForm: TGocciaResultForm;
    FOnConsole: TGocciaConsoleEvent;
    FResolver: TModuleResolver;
    FModuleContentCheck: TModuleContentCheck;
    FProviderGraph: TProviderGraphCheck;
    FOnWarning: TGocciaWarningEvent;
    FOnModuleWarning: TGocciaModuleWarningEvent;
    FDeterministic: Boolean;
    { a deterministic run's Math.random streams: realm 0 seeded as GocciaScript's deterministic profile is, each
      other realm a fork of it in the order the realms first draw }
    FRandoms: array of IGocciaHostRandom;
    FRandomForks: UInt64;
    FFailOnUnhandledRejection: Boolean;
    { --host-environment: the provider module ('' none), its four exports once loaded (epochNanoseconds,
      monotonicNanoseconds, timeZoneIdentifier, random; rooted), each realm's random stream id (set: first drawn),
      and whether a clock or random export is running (a provider that reads its own source is refused) }
    FHostEnvironment: UnicodeString;
    FEnvLoaded: Boolean;
    FEnvFns: array[0..3] of TValue;
    FEnvStreams: array of UInt64;
    FEnvStreamSet: array of Boolean;
    FEnvClockCalling, FEnvRandomCalling: Boolean;
    { the module records before the run's entry: those are the host's (globals, manifests, the provider) }
    FModuleMark: Integer;
    { registrations made during the entry and its test or benchmark suites }
    FWeakCleanupMark: Int64;
    FAuditSink: TGocciaCapabilityAuditSink;
    { the capability set FFI.open judges libraries by (Capabilities); FFI is installed when it grants ffi }
    FCapabilities: TGocciaCapabilities;
    FHasCapabilities: Boolean;
    FFetchMaxResponseBytes: Integer;
    FInspectDepth: Integer;
    FCoverage: Boolean;
    FRandomRoot: IGocciaHostRandom;
    FProfileOpcodes, FProfileFunctions: Boolean;
    FJSModuleSources: Boolean;
    FProjectRoot: string;
    FHostEnvironmentGuest: Boolean;
    { the entry's file name (Run): an audited decision made outside every script frame is the entry's }
    FRunFile: UnicodeString;
    { RunArtifact's entry: the bytecode file's bytes, run instead of compiling Run's source }
    FArtifactEntry: Boolean;
    FArtifactBytes: RawByteString;
    { Goccia.Modules' TArtifactCheck: a bytecode module's profile against this engine's settings }
    function CheckArtifact(const A: TGocciaArtifact; var Err: UnicodeString): Boolean;
    { the artifact entry loaded: a Script's template in T, or a Module provided under its logical address
      (EntryKey); False with R's error fields set }
    function LoadEntryArtifact(const FileName: string; var T: PFuncTemplate; var EntryKey: UnicodeString;
      var R: TGocciaRunResult): Boolean;
    function NextRandom(Realm: Integer; out D: Double): Boolean;
    function AuditRead(Allow: Boolean; const Subject, Reason: string): Boolean;
    function HostReady(var Err: UnicodeString): Boolean;
    { Goccia.VM's AuditHook: the script's decisions, located at its call site }
    function AuditFromScript(Kind: TAuditKind; Allow: Boolean; const Subject, Reason: UnicodeString): Boolean;
    procedure SetCapabilities(const Value: TGocciaCapabilities);
    function ProviderRandom(Realm: Integer; out D: Double): Boolean;
    function ProviderClock(Kind: Integer; out Ns: Int64): Boolean;
    function LoadHostEnvironment: Boolean;
    { a run that ended before its entry ran: the shared budget's count, so the setup work it charged is reported }
    procedure ReportSharedCount(var R: TGocciaRunResult);
    function InstallHost(var Err: UnicodeString): Boolean;
    procedure SetHeapSnapshots(V: Boolean);
    procedure SetOnModuleWarning(const Value: TGocciaModuleWarningEvent);
  public
    { builds this thread's engine; one at a time per thread }
    constructor Create;
    { frees this thread's engine: values and closures from it are invalid afterwards }
    destructor Destroy; override;
    { the host's globals and modules (console, timers, URL, goccia:*, ...), installed before the first run: the
      ShadowRealm switch and ConfigJson (the host config as a JSON object of strings, goccia.json's keys) are
      read then. Run installs them when the host has not. }
    function PrepareHost(var Err: UnicodeString): Boolean;
    function Run(const FileName: string; const Source: UnicodeString): TGocciaRunResult;
    { Run for a bytecode file (a .gbc entry, Goccia.Artifact): its recorded goal unless SourceKind names one (a
      conflicting one is an error), its profile checked against the settings before anything of it runs, a
      Module's imports linked as a source module's }
    function RunArtifact(const FileName: string; const Bytes: RawByteString): TGocciaRunResult;
    { the Bundler's compile: Source compiled as Run would compile it (SourceKind, Profile, JSModuleSources), every
      function with its code, and written as a bytecode file (Goccia.Artifact) without running anything. False with
      R's error fields set for source that does not compile or a template the format cannot hold }
    function CompileArtifact(const FileName: string; const Source: UnicodeString; out Bytes: TBytes;
      var R: TGocciaRunResult): Boolean;
    { test mode, after Run: the file's suites (Goccia.HostAPI RunTestSuites) under the limits Run armed, which stay in force
      from the file's start; Json their report, else Err why they did not finish and Aborted the limit that ended
      them. The limits are disarmed after. }
    function RunTests(var Json, Err: UnicodeString; var Aborted: TGocciaAbort): Boolean;
    { after RunTests: a rejection still without a handler once the work the tests left has run (its text), which
      fails the file; '' for none }
    function LateRejection: UnicodeString;
    { test mode: the limits Run left armed disarmed, when RunTests does not follow }
    procedure EndTests;
    { benchmark mode, after Run: the file's benchmarks (Goccia.HostAPI RunBenchmarkSuites, once each when
      Deterministic) under the limits Run armed, which stay in force from the file's start, and the profile when
      one is recorded; their results, else Err why they did not finish and Aborted the limit that ended them. The
      limits are disarmed after (EndTests when RunBenchmarks does not follow). }
    function RunBenchmarks(Deterministic: Boolean; out Entries: TGocciaBenchEntries; out Total: Integer;
      out DurationNanoseconds: Int64; var Err: UnicodeString; var Aborted: TGocciaAbort): Boolean;
    { with HeapSnapshots, after the file's tests: each distinct object expect() was given, in the order first
      given, written as a heap snapshot (Goccia.HeapSnapshot) and described, or why it cannot be; Observations counts
      every object given, repeats included }
    function HeapSnapshotObserved(out Observations: Integer): TArray<TGocciaHeapRoot>;
    { a fresh engine, set up as the one that wrote Root was (globals, testing API, settings) and not run: restores
      Root, writes it again and describes it. Passed when both match the source's; Refused (Detail why) when this
      engine cannot restore it, a built-in the run changed say; Failed (Detail) when they differ }
    function HeapSnapshotVerify(const Root: TGocciaHeapRoot; const FileName: string; const Source: UnicodeString;
      out Detail: UnicodeString): TGocciaHeapVerdict;
    { whether the run's entry loaded a module from Path (an absolute path), not the host before it: a host shows a
      code frame of such a file only }
    function ModuleLoaded(const Path: UnicodeString): Boolean;
    function ModuleSourcePath(const Path: UnicodeString): UnicodeString;
    { the engine's heap now; call it on the engine's thread }
    function Memory: TGocciaMemoryStats;
    { a deterministic run's stream for a child engine (a sandbox's nested run), forked from realm 0's as a realm's
      is; nil when the run is not deterministic. Call it on the engine's thread. }
    function ForkRandom: IGocciaHostRandom;
    { a decision of the host's own (a node_modules grant) to AuditSink, at the script's call site, during a run on
      the engine's thread: False when the sink raised, the run then ending (gaHostAbort) }
    function Audit(Kind: TGocciaCapabilityKind; Allow: Boolean; const Subject, Reason: string): Boolean;
    { globals injection before Run (Goccia.HostAPI): a data file's globals, and one --global name=value }
    function InjectDataGlobals(const Text, Ext, Path: UnicodeString; var Err: UnicodeString): Boolean;
    function InjectInlineGlobal(const Name, ValueText: UnicodeString; var Err: UnicodeString): Boolean;
    { virtual modules before Run (Goccia.HostAPI) }
    function DefineConfigVirtualModules(const ConfigPath: UnicodeString; var Err: UnicodeString): Boolean;
    function DefineManifestVirtualModules(const ManifestPath: UnicodeString; var Err: UnicodeString): Boolean;
    function DefineInlineVirtualModule(const Definition, Base: UnicodeString; var Err: UnicodeString): Boolean;
    { Registers a native factory for an import, with the same lifetime as this engine. }
    function DefineNativeModule(const Specifier: UnicodeString; Factory: TGocciaNativeModuleFactory;
      var Err: UnicodeString): Boolean;
    { Installs a work source for this engine. The caller keeps it alive until it is removed or the engine ends. }
    procedure SetHostWork(Work: TGocciaEmbedderWork);
    function InjectModuleGlobals(const Path: UnicodeString; var Err: UnicodeString): Boolean;
    function DefineModuleManifestVirtualModules(const Path: UnicodeString; var Err: UnicodeString): Boolean;
    property Profile: TLangProfile read FProfile write FProfile;
    property SourceKind: TGocciaSourceKind read FSourceKind write FSourceKind;
    property ConfigJson: UnicodeString read FConfigJson write FConfigJson;
    { the test framework's host: no process exit on an uncaught error, the testing API's globals }
    property TestMode: Boolean read FTestMode write FTestMode;
    { the benchmark runner's host: the limits Run arms stay armed for RunBenchmarks }
    property BenchMode: Boolean read FBenchMode write FBenchMode;
    { the TestRunner's --heap-snapshots: built-ins named before the entry compiles, and expect() keeping what it
      is given for HeapSnapshotObserved (set before InstallTestingAPI and Run) }
    property HeapSnapshots: Boolean read FHeapSnapshots write SetHeapSnapshots;
    { setTimeout and the other timer globals, and goccia:timers (True by default); without them the timer queue
      still serves the host's own users (AbortSignal.timeout) }
    property ExposeTimers: Boolean read FExposeTimers write FExposeTimers;
    { GocciaScriptLoaderBare's engine, the core without a runtime: the global object of Goccia.HostAPI's bare host
      (print, Goccia, no console or web APIs) and no module content provider, so an import that finds its file fails
      with ERR_MODULE_LOADING_UNSUPPORTED }
    property Bare: Boolean read FBare write FBare;
    { report the entry's completion value (TGocciaRunResult.ResultText) }
    property ResultForm: TGocciaResultForm read FResultForm write FResultForm;
    { console lines go here instead of to stdout (nil: stdout) }
    property OnConsole: TGocciaConsoleEvent read FOnConsole write FOnConsole;
    { the run's import resolution (an import map, aliases, node_modules), not owned; nil for relative and absolute
      paths only }
    property Resolver: TModuleResolver read FResolver write FResolver;
    { each module file's bytes before they load (the resolver's provider packages checked against their pins);
      nil for none }
    property ModuleContentCheck: TModuleContentCheck read FModuleContentCheck write FModuleContentCheck;
    { whether a file is a provider package's, reached as that package (the read policy admits it); nil for none }
    property ProviderGraph: TProviderGraphCheck read FProviderGraph write FProviderGraph;
    property OnWarning: TGocciaWarningEvent read FOnWarning write FOnWarning;
    { an imported module's parse warnings, from setup loads (virtual modules, globals) and the run alike; set on
      the engine's thread }
    property OnModuleWarning: TGocciaModuleWarningEvent read FOnModuleWarning write SetOnModuleWarning;
    { GocciaScript's deterministic profile (--deterministic): the epoch and monotonic clocks at 0, the time zone
      UTC, Math.random a seeded stream; set before the first run or PrepareHost }
    property Deterministic: Boolean read FDeterministic write FDeterministic;
    { a deterministic engine's Math.random stream for realm 0 (a parent's ForkRandom), instead of the profile's
      seed }
    property RandomRoot: IGocciaHostRandom read FRandomRoot write FRandomRoot;
    { GocciaScript's JavaScript host environment (--host-environment): the module at this path, evaluated before
      the first run's entry, exports the clocks, time zone and random streams the script reads (docs/
      host-environment.md); its time zone is read once, then. Not with Deterministic }
    property HostEnvironment: UnicodeString read FHostEnvironment write FHostEnvironment;
    { the capability decisions of the run (GocciaScript's capability audit, docs/capability-audit.md): the Function
      constructor's, a ShadowRealm's, an eval's, and what the host reports through Audit, each at the script's call
      site. A sink that raises ends the run past every handler (gaHostAbort). Set before Run }
    property AuditSink: TGocciaCapabilityAuditSink read FAuditSink write FAuditSink;
    { the run's capability set (ADR 0122): with the ffi capability the FFI global is installed, and FFI.open judges
      each library by its scopes. Set before the host is prepared; without it there is no FFI }
    property Capabilities: TGocciaCapabilities read FCapabilities write SetCapabilities;
    { fetch()'s response-body ceiling in bytes (--max-fetch-bytes); 0 for the default (8 MiB) }
    property FetchMaxResponseBytes: Integer read FFetchMaxResponseBytes write FFetchMaxResponseBytes;
    { how many levels deep console output renders objects (--inspect-depth), 1 to 64; 0 for the default (5) }
    property InspectDepth: Integer read FInspectDepth write FInspectDepth;
    { coverage of the run's code (Goccia.Coverage): its lines, branches and functions recorded in the calling
      thread's TGocciaCoverageTracker, which the host creates and enables; the run takes the interpreter only }
    property Coverage: Boolean read FCoverage write FCoverage;
    { the runtime profile of the run (--profile): opcode counts and the scalar fast path, and function calls, times
      and allocations, recorded in the calling thread's TGocciaProfiler (Goccia.Profile), which the host creates or
      adopts; the run takes the interpreter only }
    property ProfileOpcodes: Boolean read FProfileOpcodes write FProfileOpcodes;
    { GocciaScript's experimental JavaScript module source objects (--experimental-js-module-source): a source
      phase import of a source text module gives one }
    property JSModuleSources: Boolean read FJSModuleSources write FJSModuleSources;
    { the read capability's module-graph exemption (ADR 0122): a literal import inside this directory (canonical) needs
      no read grant (by default the nearest goccia config's directory above the entry, else the entry's). With Capabilities set (and no sandbox) the run's module reads are judged by them (Goccia.HostAPI
      SetReadPolicy) }
    property ProjectRoot: string read FProjectRoot write FProjectRoot;
    { what the host loads for a config between these (its manifests and globals modules) is read under the run's
      read capability, as the script's own reads are }
    { the error the host's last failed load threw (a globals module's, a manifest's) described in R as a run's:
      False when it threw none }
    function DescribeHostError(var R: TGocciaRunResult): Boolean;
    procedure BeginGuestLoads;
    procedure EndGuestLoads;
    { the config naming what the next guest load reads: its own tree needs no grant }
    procedure GuestScope(const Dir: string);
    { whether a file a config names may be read (under BeginGuestLoads): False with Err the refusal }
    function GuestReadAllowed(const Path: UnicodeString; var Err: UnicodeString): Boolean;
    { the host environment's module is a config's (read as the script's), not the command line's }
    property HostEnvironmentGuest: Boolean read FHostEnvironmentGuest write FHostEnvironmentGuest;
    property ProfileFunctions: Boolean read FProfileFunctions write FProfileFunctions;
    { a promise rejected with no handler that still has none when the run ends fails the run, its reason the
      uncaught error (GocciaRunner) }
    property FailOnUnhandledRejection: Boolean read FFailOnUnhandledRejection write FFailOnUnhandledRejection;
    { 0 for none }
    property TimeoutMs: Int64 read FTimeoutMs write FTimeoutMs;
    { the limit that stopped the last InjectModuleGlobals (gaNone: none), which the host reports as it reports a
      run's }
    property SetupAborted: TGocciaAbort read FSetupAborted;
    property MaxInstructions: Int64 read FMaxInstructions write FMaxInstructions;
    { Count instructions when MaxInstructions is zero. Capped runs always report a count. }
    property MeasureInstructions: Boolean read FMeasureInstructions write FMeasureInstructions;
    { Engines sharing a job attach the same budget before Run. While one is attached it provides both the limit and
      the count, and MaxInstructions and MeasureInstructions are ignored; setup work (a globals module, the host
      environment) is charged to it too. }
    property InstructionBudget: IGocciaInstructionBudget read FInstructionBudget write FInstructionBudget;
    property MaxFrames: Integer read FMaxFrames write FMaxFrames;
    { the heap limit in bytes, set as the running thread's HeapCeiling when this engine runs (each thread has its
      own); 0 for the default (Goccia.HostAPI DefaultHeapCeiling) }
    property MaxMemory: Int64 read FMaxMemory write FMaxMemory;
  end;

{ the recommended language profile: restricted, every compatibility switch off }
function RecommendedProfile: TLangProfile;
{ the read capability's project for an entry (ADR 0122): the nearest goccia config's directory above it, else its
  own directory, canonical }
function DiscoverProjectRoot(const EntryFileName: string): string;

implementation

uses
  Math, TimingUtils, BigInteger, Goccia.Temporal.TimeZone, Goccia.Engine, Goccia.Eval,
  Goccia.Promise, Goccia.Builtins, Goccia.BigIntV, Goccia.ObjModel, Goccia.FFI, Goccia.Fetch, Goccia.Compiler, Goccia.Sandbox, Goccia.Profile, Goccia.Profiler.Compile, Goccia.Weak,
  Goccia.HeapSnapshot, Goccia.Bytecode.Profile;

threadvar
  { the built-in JSON.stringify, taken when the host is installed: a script that replaces it does not change a
    report }
  StringifyFn: TValue;
  StringifyRootedGen: Integer;

function JSONQuoted(const S: UnicodeString): UnicodeString;
var I: Integer; C: WideChar;
begin
  Result := '"';
  for I := 1 to Length(S) do
  begin
    C := S[I];
    case C of
      '"': Result := Result + '\"';
      '\': Result := Result + '\\';
      #8: Result := Result + '\b';
      #9: Result := Result + '\t';
      #10: Result := Result + '\n';
      #12: Result := Result + '\f';
      #13: Result := Result + '\r';
    else
      if Ord(C) < 32 then Result := Result + '\u' + UnicodeString(LowerCase(IntToHex(Ord(C), 4)))
      else Result := Result + C;
    end;
  end;
  Result := Result + '"';
end;

{ the JSON report's text of a completion value JSON leaves out: String(value) (22.1.1.1), so a symbol is its
  SymbolDescriptiveString (20.4.3.4.1) where ToString would throw, and the report's result is always there }
function CompletionString(V: TValue; var S: TValue): Boolean;
var D: TValue;
begin
  if IsPtr(V) and (PCell(PtrOf(V))^.Kind = ckSymbol) then
  begin
    D := SymbolDescription(V);
    if D = VUndefined then S := StrFromUnicode('Symbol()')
    else S := StrFromUnicode('Symbol(' + StrToUnicode(StrOf(D)) + ')');
    Exit(True);
  end;
  Result := ToStringV(V, S);
end;

{ the report forms of the value at the top of the root stack; False with an exception pending when a conversion threw }
function ResultText(Form: TGocciaResultForm; var Text: UnicodeString): Boolean;
var V, S: TValue;
begin
  V := RootAt(TempTop - 1);
  if Form = grfString then
  begin
    { ToString: a Symbol, which has no string form, throws its TypeError (docs/build-system.md, --print) }
    Result := ToStringV(V, S) and IsString(S);
    if Result then Text := StrToUnicode(StrOf(S));
    Exit;
  end;
  { a sandbox child's value, for the calling engine to decode }
  if Form = grfSandbox then Exit(EncodeSandboxValue(V, Text));
  { the REPL's display of an input's value }
  if Form in [grfREPL, grfREPLColor] then Exit(ReplFormat(V, Form = grfREPLColor, Text));
  { JSON }
  if V = VUndefined then begin Text := 'null'; Exit(True); end;
  if not IsCallable(V) and IsObject(StringifyFn) then
  begin
    if not CallValue(StringifyFn, VUndefined, @V, 1, S) then Exit(False);
    if IsString(S) then
    begin
      Text := StrToUnicode(StrOf(S));
      { what JSON leaves out comes back null too: only null, booleans and numbers keep it }
      V := RootAt(TempTop - 1);
      if (Text <> 'null') or (V = VNull) or (V = VTrue) or (V = VFalse) or IsNumber(V) then Exit(True);
    end;
  end;
  V := RootAt(TempTop - 1);
  Result := CompletionString(V, S) and IsString(S);
  if Result then Text := JSONQuoted(StrToUnicode(StrOf(S)));
end;

{ "    at name (source:line:column)": the first located frame line of a stack text (a built-in's "at name (native)"
  line before it names the built-in that raised the error, and has no location) }
procedure FirstFrameOf(const Stack: UnicodeString; var R: TGocciaRunResult);
var P, Open, Close, C1, C2: Integer; Loc: UnicodeString;
begin
  P := Pos(#10'    at ', Stack);
  while True do
  begin
    if P = 0 then Exit;
    Close := P + 1;
    while (Close <= Length(Stack)) and (Stack[Close] <> #10) do Inc(Close);
    if Copy(Stack, Close - 9, 9) <> ' (native)' then Break;
    { the next frame line, if any }
    if (Close > Length(Stack)) or (Copy(Stack, Close, 8) <> #10'    at ') then Exit;
    P := Close;
  end;
  Dec(Close);
  if (Close <= P) or (Stack[Close] <> ')') then Exit;
  Open := Close;
  while (Open > P) and (Stack[Open] <> '(') do Dec(Open);
  if Open <= P then Exit;
  Loc := Copy(Stack, Open + 1, Close - Open - 1);
  C2 := Length(Loc);
  while (C2 > 0) and (Loc[C2] <> ':') do Dec(C2);
  C1 := C2 - 1;
  while (C1 > 0) and (Loc[C1] <> ':') do Dec(C1);
  if C1 <= 0 then Exit;
  R.ErrorFile := Copy(Loc, 1, C1 - 1);
  R.ErrorLine := StrToIntDef(string(Copy(Loc, C1 + 1, C2 - C1 - 1)), 0);
  R.ErrorColumn := StrToIntDef(string(Copy(Loc, C2 + 1, MaxInt)), 0);
end;

{ an own or inherited string data property, read without running script: the report is made after the run, so an
  accessor, a Proxy (its traps) or a module namespace (a deferred one evaluates its module) ends the walk unread,
  as GocciaRunner's TryGetStringDataProperty does }
function StringDataProperty(V: TValue; Key: PStr; out Text: UnicodeString): Boolean;
var D: TPropDesc; Found: Boolean;
begin
  Result := False;
  Text := '';
  while IsObject(V) do
  begin
    if ObjOf(V)^.OKind in [okProxy, okModuleNS] then Exit;
    if not GetOwnPropertyV(V, Key, D, Found) then
    begin
      Pending := VUndefined;
      Exit;
    end;
    if Found then
    begin
      if D.HasValue and IsString(D.Value) then
      begin
        Text := StrToUnicode(StrOf(D.Value));
        Result := True;
      end;
      Exit;
    end;
    V := ProtoOf(ObjOf(V));
  end;
end;

{ the uncaught value as text without running script: a primitive's string, a symbol's descriptive string, an
  object with a string name or message as Error.prototype.toString joins them (20.5.3.4), any other object as
  "[object Tag]" (GocciaRunner's ThrownValueFallbackText) }
function ThrownText(V: TValue): UnicodeString;
const
  okDate = 18; { Goccia.Date's }
var S, D: TValue; Tag, N, M: UnicodeString; HasN, HasM: Boolean;
begin
  if IsPtr(V) and (PCell(PtrOf(V))^.Kind = ckSymbol) then
  begin
    D := SymbolDescription(V);
    if D = VUndefined then Exit('Symbol()');
    Exit('Symbol(' + StrToUnicode(StrOf(D)) + ')');
  end;
  if IsObject(V) then
  begin
    if ObjOf(V)^.OKind = okProxy then Exit('[object Proxy]');
    HasN := StringDataProperty(V, AtomOf('name'), N);
    HasM := StringDataProperty(V, AtomOf('message'), M);
    if HasN or HasM then
    begin
      if not HasN then N := 'Error';
      if N = '' then Exit(M);
      if M = '' then Exit(N);
      Exit(N + ': ' + M);
    end;
    if not StringDataProperty(V, SymKey(SymToStringTagW), Tag) then
      case ObjOf(V)^.OKind of
        okArray: Tag := 'Array';
        okArguments, okArgumentsOrd: Tag := 'Arguments';
        okFunction, okNative, okBound: Tag := 'Function';
        okError: Tag := 'Error';
        okBoolean: Tag := 'Boolean';
        okNumber: Tag := 'Number';
        okStringObj: Tag := 'String';
        okDate: Tag := 'Date';
      else Tag := 'Object';
      end;
    Exit('[object ' + Tag + ']');
  end;
  Result := '';
  if ToStringV(V, S) and IsString(S) then Result := StrToUnicode(StrOf(S))
  else if (AbortKind <> 0) and IsObject(Pending) then Result := ThrownText(Pending);
  Pending := VUndefined;
end;

{ the uncaught value in parts (R.ErrorMessage already holds its text), from its string data properties only }
function DescribeThrown(V: TValue; var R: TGocciaRunResult): Boolean;
var Stack, Text: UnicodeString;
begin
  Result := False;
  if not IsObject(V) then Exit;
  PushRoot(V);
  try
    if StringDataProperty(RootAt(TempTop - 1), AtomOf('name'), Text) then
    begin
      R.ErrorIsError := True;
      R.ErrorName := Text;
    end;
    if StringDataProperty(RootAt(TempTop - 1), AtomOf('message'), Text) then
    begin
      R.ErrorIsError := True;
      R.ErrorText := Text;
    end;
    { located only from the provenance the engine recorded when it made the error, never from a thrown value's
      own stack string, which can name any file }
    Result := ErrorSourceLocation(RootAt(TempTop - 1), R.ErrorFile, R.ErrorLine, R.ErrorColumn);
    if not Result and ErrorStackProvenance(RootAt(TempTop - 1), Stack) then FirstFrameOf(Stack, R);
    { the host's suggestion for the error it threw (a PermissionDenied's grant) }
    if ErrorHostSuggestion(RootAt(TempTop - 1), Text) then R.ErrorSuggestion := Text;
  finally
    PopRoots(1);
  end;
  Pending := VUndefined;
end;

function RecommendedProfile: TLangProfile;
begin
  Result := Default(TLangProfile);
  Result.Restricted := True;
end;

constructor TGocciaEngine.Create;
begin
  inherited Create;
  FProfile := RecommendedProfile;
  FConfigJson := '{}';
  FExposeTimers := True;
  EngineInit;
end;

function TGocciaEngine.NextRandom(Realm: Integer; out D: Double): Boolean;
begin
  if Realm >= Length(FRandoms) then SetLength(FRandoms, Realm + 1);
  if FRandoms[Realm] = nil then
  begin
    Inc(FRandomForks);
    FRandoms[Realm] := FRandoms[0].Fork(FRandomForks);
  end;
  D := FRandoms[Realm].NextDouble;
  Result := True;
end;

const
  STREAM_HASH_INCREMENT = UInt64($9E3779B97F4A7C15);
  STREAM_HASH_MULTIPLIER = UInt64($BF58476D1CE4E5B9);

{$PUSH}{$Q-}{$R-}
{ a child stream's id from its parent's and the child's number (Goccia.HostEnvironment.JavaScript's
  DeriveStreamId: a realm's stream is the provider's own fork of the engine's) }
function DeriveStreamId(Parent, Child: UInt64): UInt64;
begin
  Result := (Parent xor STREAM_HASH_INCREMENT) + Child;
  Result := (Result xor (Result shr 30)) * STREAM_HASH_MULTIPLIER;
  Result := Result xor (Result shr 27);
end;
{$POP}

function BigFromQWord(Q: UInt64): TBigInteger;
begin
  if Q shr 32 = 0 then Exit(TBigInteger.FromInt64(Int64(Q)));
  Result := TBigInteger.FromInt64(Int64(Q shr 32)).Multiply(TBigInteger.FromInt64($100000000)).Add(
    TBigInteger.FromInt64(Int64(Q and $FFFFFFFF)));
end;

{ Math.random through the provider's random(streamId): realm 0 stream 0n, each other realm a fork in the order
  the realms first draw }
function TGocciaEngine.ProviderRandom(Realm: Integer; out D: Double): Boolean;
var A: array[0..0] of TValue; R: TValue;
begin
  D := 0;
  if FEnvRandomCalling then
    Exit(ThrowTypeError('Host environment export "random" cannot call Math.random recursively'));
  if Realm >= Length(FEnvStreams) then
  begin
    SetLength(FEnvStreams, Realm + 1);
    SetLength(FEnvStreamSet, Realm + 1);
  end;
  if not FEnvStreamSet[Realm] then
  begin
    if Realm > 0 then
    begin
      Inc(FRandomForks);
      FEnvStreams[Realm] := DeriveStreamId(0, FRandomForks);
    end;
    FEnvStreamSet[Realm] := True;
  end;
  A[0] := BigToValue(BigFromQWord(FEnvStreams[Realm]));
  FEnvRandomCalling := True;
  try
    Result := CallValue(FEnvFns[3], VUndefined, @A[0], 1, R);
  finally
    FEnvRandomCalling := False;
  end;
  if not Result then Exit;
  if not IsNumber(R) then Exit(ThrowTypeError('Host environment export "random" must return a Number'));
  D := NumOf(R);
  if IsNan(D) or IsInfinite(D) or (D < 0) or (D >= 1) then
    Exit(ThrowError('RangeError', 'Host environment export "random" must return a finite Number greater ' +
      'than or equal to 0 and less than 1'));
end;

{ the provider's epochNanoseconds() (Kind 0) or monotonicNanoseconds() (1): an Int64-range BigInt or a safe
  integer Number }
function TGocciaEngine.ProviderClock(Kind: Integer; out Ns: Int64): Boolean;
var Name: UnicodeString; R: TValue; B: TBigInteger; D: Double;
begin
  Ns := 0;
  if Kind = 0 then Name := 'epochNanoseconds' else Name := 'monotonicNanoseconds';
  if FEnvClockCalling then
    Exit(ThrowTypeError('Host environment export "' + Name + '" cannot call a host time source recursively'));
  FEnvClockCalling := True;
  try
    Result := CallValue(FEnvFns[Kind], VUndefined, nil, 0, R);
  finally
    FEnvClockCalling := False;
  end;
  if not Result then Exit;
  if IsBigInt(R) then
  begin
    B := ValueToBig(R);
    if (B.Compare(TBigInteger.FromInt64(Low(Int64))) < 0) or (B.Compare(TBigInteger.FromInt64(High(Int64))) > 0) then
      Exit(ThrowError('RangeError', 'Host environment export "' + Name + '" must return an Int64-range BigInt'));
    Ns := B.ToInt64;
    Exit(True);
  end;
  if IsNumber(R) then
  begin
    D := NumOf(R);
    if IsNan(D) or IsInfinite(D) or (Frac(D) <> 0) or (Abs(D) > 9007199254740991.0) then
      Exit(ThrowError('RangeError', 'Host environment export "' + Name +
        '" must return a safe integer Number or Int64-range BigInt'));
    Ns := Trunc(D);
    Exit(True);
  end;
  Result := ThrowTypeError('Host environment export "' + Name + '" must return a Number or BigInt');
end;

function TGocciaEngine.DefineNativeModule(const Specifier: UnicodeString; Factory: TGocciaNativeModuleFactory;
  var Err: UnicodeString): Boolean;
begin
  Result := HostReady(Err) and Goccia.HostAPI.DefineNativeModule(Specifier, Factory, Err);
end;

{ the provider module evaluated, its exports checked and taken, the clocks and Math.random routed through them,
  performance's time origin taken from them, then the time zone read; False with the error pending }
function TGocciaEngine.LoadHostEnvironment: Boolean;
const NAMES: array[0..3] of UnicodeString = ('epochNanoseconds', 'monotonicNanoseconds', 'timeZoneIdentifier',
  'random');
var NS, V, R: TValue; I: Integer; Z, C: string;
begin
  Result := False;
  if not EvaluateModuleNamespace(UnicodeString(ExpandFileName(UTF8Encode(FHostEnvironment))), NS) then Exit;
  PushRoot(NS);
  try
    for I := 0 to 3 do
    begin
      if not GetV(RootAt(TempTop - 1), AtomOf(NAMES[I]), V) then Exit;
      if not IsCallable(V) then
        Exit(ThrowTypeError('Host environment module must export callable "' + NAMES[I] + '"'));
      FEnvFns[I] := V;
    end;
  finally
    PopRoots(1);
  end;
  AddRootRange(@FEnvFns[0], 4);
  FEnvLoaded := True;
  FRandomForks := 0;
  SetLength(FEnvStreams, 0);
  SetLength(FEnvStreamSet, 0);
  HostClock := ProviderClock;
  HostRandom := ProviderRandom;
  if not TimeOriginInit then Exit;
  { the default time zone, read once (Date's local time, Temporal.Now, Intl), as GocciaScript canonicalizes it }
  FEnvClockCalling := True;
  try
    if not CallValue(FEnvFns[2], VUndefined, nil, 0, R) then Exit;
  finally
    FEnvClockCalling := False;
  end;
  if not IsString(R) then Exit(ThrowTypeError('Host environment export "timeZoneIdentifier" must return a string'));
  Z := StrToUnicode(StrOf(R));
  if TimeZoneIdentifiersEqual(Z, 'UTC') then Z := 'UTC'
  else if TryCanonicalizeTimeZoneIdentifierCase(Z, C) and IsValidTimeZone(C) then Z := C
  else if not IsValidTimeZone(Z) then
    Exit(ThrowError('RangeError', 'Host environment export "timeZoneIdentifier" returned an unsupported time ' +
      'zone: ' + Z));
  TimeZoneOverride := Z;
  Result := True;
end;

procedure TGocciaEngine.ReportSharedCount(var R: TGocciaRunResult);
begin
  if FInstructionBudget = nil then Exit;
  R.HasInstructionCount := True;
  R.InstructionCount := FInstructionBudget.Count;
end;

procedure TGocciaEngine.SetCapabilities(const Value: TGocciaCapabilities);
begin
  FCapabilities := Value;
  FHasCapabilities := True;
end;

function TGocciaEngine.ModuleLoaded(const Path: UnicodeString): Boolean;
begin
  Result := ModuleSourcePath(Path) <> '';
end;

function TGocciaEngine.ModuleSourcePath(const Path: UnicodeString): UnicodeString;
begin
  Result := ModuleSourcePathSince(Path, FModuleMark);
end;

function TGocciaEngine.Memory: TGocciaMemoryStats;
begin
  Result.LiveBytes := HeapUsedBytes;
  Result.PeakLiveBytes := HeapPeakBytes;
  Result.AllocatedBytes := HeapAllocatedBytes;
  if FMaxMemory > 0 then Result.LimitBytes := FMaxMemory else Result.LimitBytes := DefaultHeapCeiling;
  Result.Collections := GCMinorCount + GCMajorCount;
end;

function TGocciaEngine.Audit(Kind: TGocciaCapabilityKind; Allow: Boolean; const Subject, Reason: string): Boolean;
var Event: TGocciaCapabilityAuditEvent; Src: UnicodeString; Line, Col: Integer; Found: Boolean;
begin
  if not Assigned(FAuditSink) then Exit(True);
  Event := Default(TGocciaCapabilityAuditEvent);
  Event.Kind := Kind;
  if Allow then Event.Decision := gcdAllow else Event.Decision := gcdDeny;
  Event.Subject := Subject;
  Event.Reason := Reason;
  { a static import's declaration while its module links, else the running script's call site; the entry's file
    reads as the host named it }
  Found := AuditSiteOverride(Src, Line, Col) or ImportSiteNow(Src, Line, Col) or CallSiteNow(Src, Line, Col);
  if Found and (Src = ExpandFileName(FRunFile)) then Src := FRunFile;
  if Found then
  begin
    Event.Source.FilePath := Src;
    Event.Source.Line := Line;
    Event.Source.Column := Col;
  end
  else
    Event.Source.FilePath := FRunFile;
  try
    FAuditSink(Event);
  except
    on E: Exception do
      Exit(HostAbort('capability audit delivery failed: ' + E.Message));
  end;
  Result := True;
end;

function TGocciaEngine.AuditFromScript(Kind: TAuditKind; Allow: Boolean; const Subject, Reason: UnicodeString): Boolean;
const
  Kinds: array[TAuditKind] of TGocciaCapabilityKind = (gckFunctionConstructor, gckShadowRealm, gckEval, gckFFIOpen,
    gckNetFetch, gckNetDispatch);
begin
  Result := Audit(Kinds[Kind], Allow, Subject, Reason);
end;

function TGocciaEngine.DescribeHostError(var R: TGocciaRunResult): Boolean;
var V: TValue;
begin
  R := Default(TGocciaRunResult);
  V := TakeHostError;
  if V = VUndefined then Exit(False);
  PushRoot(V);
  R.ErrorMessage := ThrownText(RootAt(TempTop - 1));
  DescribeThrown(RootAt(TempTop - 1), R);
  PopRoots(1);
  Pending := VUndefined;
  Result := True;
end;

procedure TGocciaEngine.BeginGuestLoads;
begin
  SetReadPolicy(FHasCapabilities and not SandboxActive, FCapabilities, FProjectRoot, AuditRead);
  SetGuestLoads(True);
end;

procedure TGocciaEngine.GuestScope(const Dir: string);
begin
  SetGuestScope(Dir);
end;

procedure TGocciaEngine.EndGuestLoads;
begin
  SetGuestScope('');
  SetGuestLoads(False);
  SetReadPolicy(False, TGocciaCapabilities.None, '', nil);
end;

function TGocciaEngine.GuestReadAllowed(const Path: UnicodeString; var Err: UnicodeString): Boolean;
begin
  Result := Goccia.HostAPI.GuestReadAllowed(Path, Err);
end;

function TGocciaEngine.AuditRead(Allow: Boolean; const Subject, Reason: string): Boolean;
begin
  Result := Audit(gckReadFile, Allow, Subject, Reason);
end;

procedure TGocciaEngine.SetOnModuleWarning(const Value: TGocciaModuleWarningEvent);
begin
  FOnModuleWarning := Value;
  ModuleWarningHook := Value;
end;

destructor TGocciaEngine.Destroy;
begin
  ModuleWarningHook := nil;
  SetReadPolicy(False, TGocciaCapabilities.None, '', nil);
  SetModuleLoadingUnsupported(False);
  CompileCoverage := False;
  CoverageOn := False;
  AuditHook := nil;
  HostRandom := nil;
  HostClock := nil;
  TimeZoneOverride := '';
  if FDeterministic then SetClockOverride(NaN, NaN);
  LimitsDisarm;
  SetMaxFrames(0);
  HostModulesDone;
  FetchDone;
  FFIDone;
  SnapshotDone;
  HeapObserving := False;
  EngineDone;
  inherited Destroy;
end;

{ an entry's module key: a host file's absolute path, or a sandbox path as it is (the sandbox's paths are its own,
  rooted at /: ExpandFileName would make /entry.mjs C:\entry.mjs on Windows, and its imports would resolve there) }
function EntryKeyOf(const FileName: string): UnicodeString;
begin
  if SandboxActive then Result := UnicodeString(FileName)
  else Result := UnicodeString(ExpandFileName(FileName));
end;

{ the read capability's project (ADR 0122), as GocciaScript's engine finds it: the directory of the nearest goccia
  config above the entry, or the entry's own directory }
function DiscoverProjectRoot(const EntryFileName: string): string;
const
  NAMES: array[0..2] of string = ('goccia.json', 'goccia.json5', 'goccia.toml');
var EntryDirectory, Directory, Parent: string; I: Integer;
begin
  EntryDirectory := ExcludeTrailingPathDelimiter(ExtractFilePath(ExpandFileName(EntryFileName)));
  if EntryDirectory = '' then EntryDirectory := GetCurrentDir;
  Directory := EntryDirectory;
  while Directory <> '' do
  begin
    for I := Low(NAMES) to High(NAMES) do
      if FileExists(IncludeTrailingPathDelimiter(Directory) + NAMES[I]) then
        Exit(CanonicalCapabilityPath(Directory));
    Parent := ExtractFileDir(Directory);
    if (Parent = '') or (Parent = Directory) then Break;
    Directory := Parent;
  end;
  Result := CanonicalCapabilityPath(EntryDirectory);
end;

function TGocciaEngine.ForkRandom: IGocciaHostRandom;
begin
  if Length(FRandoms) = 0 then Exit(nil);
  Inc(FRandomForks);
  Result := FRandoms[0].Fork(FRandomForks);
end;

function TGocciaEngine.InstallHost(var Err: UnicodeString): Boolean;
var J: TValue;
begin
  { before the host's globals: performance.timeOrigin is the clock when they install }
  if FDeterministic then
  begin
    SetClockOverride(TGocciaHostEnvironment.DeterministicEpochNanoseconds / 1000000,
      TGocciaHostEnvironment.DeterministicMonotonicNanoseconds / 1000000);
    TimeZoneOverride := TGocciaHostEnvironment.DeterministicTimeZone;
    SetLength(FRandoms, 1);
    if FRandomRoot <> nil then FRandoms[0] := FRandomRoot
    else FRandoms[0] := TGocciaSeededHostRandom.Create(TGocciaHostEnvironment.DeterministicSeed);
    FRandomForks := 0;
    HostRandom := NextRandom;
    { the time origin under the profile's clocks (0) }
    TimeOriginInit;
  end
  else
    SeedMathRandom(QWord(GetTickCount64) xor (QWord(GetProcessID) shl 32) xor QWord(PtrUInt(Self)));
  { fetch() judges destinations by the capability set (none without one) }
  if FHasCapabilities then SetFetchPolicy(FCapabilities, FFetchMaxResponseBytes)
  else SetFetchPolicy(TGocciaCapabilities.None, FFetchMaxResponseBytes);
  SetInspectDepth(FInspectDepth);
  SetModuleLoadingUnsupported(FBare);
  Result := InstallHostGlobals(FProfile.ShadowRealm, FTestMode, FConfigJson, Err, FExposeTimers, FBare,
    not FProfile.Restricted or FProfile.AnnexB);
  if Result and FHasCapabilities and FCapabilities.Grants(gcFFI) then Result := InstallFFI(FCapabilities, Err);
  FHostReady := Result;
  if not Result then Exit;
  if FirstInEngine(StringifyRootedGen) then AddRoot(@StringifyFn);
  StringifyFn := VUndefined;
  if GetV(GlobalObj, AtomOf('JSON'), J) and IsObject(J) and GetV(J, AtomOf('stringify'), J) and IsCallable(J) then
    StringifyFn := J;
  Pending := VUndefined;
end;

{ the host prepared for a load of its own before the run, under the script's language profile (a module it evaluates
  is compiled as the script's code is, its errors with stacks) }
function TGocciaEngine.HostReady(var Err: UnicodeString): Boolean;
begin
  Result := PrepareHost(Err);
  if Result then SetActiveProfile(FProfile);
end;

function TGocciaEngine.PrepareHost(var Err: UnicodeString): Boolean;
begin
  if FHostReady then Exit(True);
  Result := InstallHost(Err);
end;

procedure TGocciaEngine.SetHostWork(Work: TGocciaEmbedderWork);
begin
  SetEmbedderWork(Work);
end;

function TGocciaEngine.InjectDataGlobals(const Text, Ext, Path: UnicodeString; var Err: UnicodeString): Boolean;
begin
  TakeHostError;
  Result := HostReady(Err) and Goccia.HostAPI.InjectDataGlobals(Text, Ext, Path, Err);
end;

function TGocciaEngine.InjectInlineGlobal(const Name, ValueText: UnicodeString; var Err: UnicodeString): Boolean;
begin
  TakeHostError;
  Result := HostReady(Err) and Goccia.HostAPI.InjectInlineGlobal(Name, ValueText, Err);
end;

function TGocciaEngine.DefineConfigVirtualModules(const ConfigPath: UnicodeString; var Err: UnicodeString): Boolean;
begin
  TakeHostError;
  Result := HostReady(Err) and Goccia.HostAPI.DefineConfigVirtualModules(ConfigPath, Err);
end;

function TGocciaEngine.DefineManifestVirtualModules(const ManifestPath: UnicodeString; var Err: UnicodeString): Boolean;
begin
  TakeHostError;
  Result := HostReady(Err) and Goccia.HostAPI.DefineManifestVirtualModules(ManifestPath, Err);
end;

function TGocciaEngine.DefineInlineVirtualModule(const Definition, Base: UnicodeString; var Err: UnicodeString): Boolean;
begin
  TakeHostError;
  Result := HostReady(Err) and Goccia.HostAPI.DefineInlineVirtualModule(Definition, Base, Err);
end;

function TGocciaEngine.InjectModuleGlobals(const Path: UnicodeString; var Err: UnicodeString): Boolean;
var WeakMark: Int64;
begin
  TakeHostError;
  if not HostReady(Err) then Exit(False);
  { a globals module's code runs under the run's limits, as the entry's does }
  FSetupAborted := gaNone;
  WeakMark := WeakCleanupMark;
  LimitsArm(FTimeoutMs, FMaxInstructions, False, FInstructionBudget);
  Result := False;
  try
    Result := Goccia.HostAPI.InjectModuleGlobals(Path, Err);
  finally
    case LimitAbortKind of
      1: FSetupAborted := gaTimeout;
      2: FSetupAborted := gaInstructionLimit;
      3: FSetupAborted := gaMemoryLimit;
      4: FSetupAborted := gaHostAbort;
    end;
    if FSetupAborted <> gaNone then DiscardAbortedRunWork(WeakMark)
    else if not Result then DiscardEmbedderWork;
    LimitsDisarm;
  end;
end;

function TGocciaEngine.DefineModuleManifestVirtualModules(const Path: UnicodeString; var Err: UnicodeString): Boolean;
begin
  TakeHostError;
  Result := HostReady(Err) and Goccia.HostAPI.DefineModuleManifestVirtualModules(Path, Err);
end;

function TGocciaEngine.CompileArtifact(const FileName: string; const Source: UnicodeString; out Bytes: TBytes;
  var R: TGocciaRunResult): Boolean;
var Kind: Integer; T: PFuncTemplate; IsModule, Unr: Boolean; Err, ErrName, ModuleSource: UnicodeString; A: TGocciaArtifact;
begin
  Result := False;
  Bytes := nil;
  R := Default(TGocciaRunResult);
  SetActiveProfile(FProfile);
  case FSourceKind of
    gskScript: Kind := 1;
    gskModule: Kind := 2;
  else
    if LowerCase(ExtractFileExt(FileName)) = '.mjs' then Kind := 2 else Kind := 0;
  end;
  A := Default(TGocciaArtifact);
  T := nil;
  Err := '';
  SetEagerCompile(True);
  SetArtifactCompile(True);
  try
    { a Script keeps its completion value, so a runner can report it whatever its options }
    if not HostCompileEntry(Source, UnicodeString(FileName), Kind, T, IsModule, Err, True) and not IsModule then
    begin
      R.ErrorName := ParseErrorName;
      R.ErrorLine := ParseErrorLine;
      R.ErrorColumn := ParseErrorColumn;
      R.ErrorSuggestion := ParseErrorSuggestion;
      R.ErrorText := Err;
      R.ErrorMessage := Err;
      R.ErrorIsError := True;
      R.ErrorFile := UnicodeString(FileName);
      Exit;
    end;
    if IsModule then
    begin
      ModuleReturnLast := True;
      try
        ModuleSource := Source;
        if (ModuleSource <> '') and (ModuleSource[1] = #$FEFF) then Delete(ModuleSource, 1, 1);
        if not ParseAndCompileModule(ModuleSource, UnicodeString(FileName), 0, T, A.Info, Err, ErrName) then
        begin
          R.ErrorName := ErrName;
          R.ErrorLine := ParseErrorLine;
          R.ErrorColumn := ParseErrorColumn;
          R.ErrorSuggestion := ParseErrorSuggestion;
          R.ErrorText := Err;
          R.ErrorMessage := Err;
          R.ErrorIsError := True;
          R.ErrorFile := UnicodeString(FileName);
          Exit;
        end;
      finally
        ModuleReturnLast := False;
      end;
    end
    else A.ScriptImports := ScriptImportSource;
    R.Compiled := True;
    R.IsModule := IsModule;
    A.SourcePath := UnicodeString(FileName);
    A.Profile := CurrentArtifactProfile(IsModule, FJSModuleSources, Unr);
    A.Unrestricted := Unr;
    A.Top := T;
    try
      Bytes := SaveArtifact(A);
      Result := True;
    except
      on E: Exception do
      begin
        R.ErrorName := 'Error';
        R.ErrorText := UnicodeString(E.Message);
        R.ErrorMessage := R.ErrorText;
        R.ErrorIsError := True;
        R.ErrorFile := UnicodeString(FileName);
      end;
    end;
  finally
    SetEagerCompile(False);
    SetArtifactCompile(False);
    A.Info.Free;
  end;
end;

function TGocciaEngine.CheckArtifact(const A: TGocciaArtifact; var Err: UnicodeString): Boolean;
var P: TGocciaCompileProfile; Unr: Boolean;
begin
  SetActiveProfile(FProfile);
  P := CurrentArtifactProfile(A.Profile.SourceType = bstModule, FJSModuleSources, Unr);
  Result := P.Matches(A.Profile) and (Unr = A.Unrestricted);
  if not Result then Err := 'Bytecode compile profile does not match the engine';
end;

function TGocciaEngine.LoadEntryArtifact(const FileName: string; var T: PFuncTemplate; var EntryKey: UnicodeString;
  var R: TGocciaRunResult): Boolean;
var Src, Err: UnicodeString; A: TGocciaArtifact;
  procedure Failed(const Msg: UnicodeString);
  begin
    R.ErrorName := 'Error';
    R.ErrorText := Msg;
    R.ErrorMessage := Msg;
    R.ErrorIsError := True;
    R.ErrorFile := UnicodeString(FileName);
  end;
begin
  Result := False;
  Err := '';
  if not ArtifactSourcePath(PByte(FArtifactBytes), Length(FArtifactBytes), Src, Err) then
  begin
    Failed(Err);
    Exit;
  end;
  EntryKey := ArtifactModulePath(EntryKeyOf(FileName), Src);
  { the goal: the recorded one, unless the host names one }
  if PByte(FArtifactBytes) = nil then Exit;
  if not LoadArtifact(PByte(FArtifactBytes), Length(FArtifactBytes), EntryKey, 0, A, Err) then
  begin
    Failed(Err);
    Exit;
  end;
  if ((FSourceKind = gskScript) and (A.Profile.SourceType <> bstScript)) or
     ((FSourceKind = gskModule) and (A.Profile.SourceType <> bstModule)) then
  begin
    A.Info.Free;
    Failed('Bytecode compile profile does not match the requested source type');
    Exit;
  end;
  if not CheckArtifact(A, Err) then
  begin
    A.Info.Free;
    Failed(Err);
    Exit;
  end;
  R.IsModule := A.Info <> nil;
  if R.IsModule then
  begin
    { the module loads again under its record (Goccia.Modules LoadArtifactRecord, with its own module id); this
      read only settled the goal and the checks }
    A.Info.Free;
    SetProvidedArtifact(EntryKey, EntryKeyOf(FileName), FArtifactBytes);
  end
  else
  begin
    T := A.Top;
    ScriptImportSource := A.ScriptImports;
  end;
  Result := True;
end;

function TGocciaEngine.RunArtifact(const FileName: string; const Bytes: RawByteString): TGocciaRunResult;
begin
  FArtifactEntry := True;
  FArtifactBytes := Bytes;
  try
    Result := Run(FileName, '');
  finally
    FArtifactEntry := False;
    FArtifactBytes := '';
    SetProvidedArtifact('', '', '');
  end;
end;

function TGocciaEngine.Run(const FileName: string; const Source: UnicodeString): TGocciaRunResult;
var Kind, Phase, I, FailLine, FailColumn, ParseLine, ParseColumn: Integer; T: PFuncTemplate;
  Err, FailPath, ParsePath, ParseName, ParseSuggestion, EntryKey: UnicodeString;
  S, Thrown, Completion, Imports: TValue; Start: Int64;
  Compiled, HasErrorLocation: Boolean; EnvAbort: Integer;
begin
  Result := Default(TGocciaRunResult);
  EntryKey := EntryKeyOf(FileName);
  Err := '';
  if not PrepareHost(Err) then
  begin
    Result.ErrorMessage := Err;
    { work the host source holds from before does not outlive the failed run }
    DiscardEmbedderWork;
    ReportSharedCount(Result);
    Exit;
  end;
  FWeakCleanupMark := WeakCleanupMark;
  if FMaxMemory > 0 then HeapCeiling := FMaxMemory else HeapCeiling := DefaultHeapCeiling;
  NurseryFitCeiling;
  SetActiveProfile(FProfile);
  SetConsoleSink(FOnConsole);
  SetJSModuleSources(FJSModuleSources);
  { the project the entry is in, unless the host named one }
  if (FProjectRoot = '') and FHasCapabilities and not SandboxActive then FProjectRoot := DiscoverProjectRoot(FileName);
  { the read policy judges a config's host-environment module as the script's reads, not the command line's }
  SetReadPolicy(FHostEnvironmentGuest and FHasCapabilities and not SandboxActive, FCapabilities, FProjectRoot,
    AuditRead);
  { coverage: the run's code compiled with a line for every source line, and run through the coverage hook }
  CompileCoverage := FCoverage;
  CoverageOn := FCoverage;
  FRunFile := FileName;
  if Assigned(FAuditSink) then AuditHook := AuditFromScript else AuditHook := nil;
  SetHostResolver(FResolver);
  SetModuleContentCheck(FModuleContentCheck);
  SetProviderGraphCheck(FProviderGraph);
  SetArtifactCheck(CheckArtifact);
  ClearLoadFailure;
  TakeResolveCandidate;
  { the JavaScript host environment, before the entry: a provider that does not load fails the run with its error }
  if FHostEnvironmentGuest then SetGuestLoads(True);
  try
    EnvAbort := 0;
    if (FHostEnvironment <> '') and not FEnvLoaded then
    begin
      { the provider module's code runs under the run's limits, as the entry's does }
      LimitsArm(FTimeoutMs, FMaxInstructions, False, FInstructionBudget);
      Compiled := False;
      try
        Compiled := LoadHostEnvironment;
      finally
        EnvAbort := AbortKind;
        if EnvAbort <> 0 then DiscardAbortedRunWork(FWeakCleanupMark)
        else if not Compiled then DiscardEmbedderWork;
        LimitsDisarm;
      end;
    end
    else Compiled := True;
  finally
    SetGuestLoads(False);
  end;
  SetReadPolicy(FHasCapabilities and not SandboxActive, FCapabilities, FProjectRoot, AuditRead);
  if not Compiled or (EnvAbort <> 0) then
  begin
    PushRoot(Pending);
    Pending := VUndefined;
    Result.ErrorMessage := ThrownText(RootAt(TempTop - 1));
    if Result.ErrorMessage = '' then Result.ErrorMessage := 'uncaught exception';
    Pending := VUndefined;
    case EnvAbort of
      1: Result.Aborted := gaTimeout;
      2: Result.Aborted := gaInstructionLimit;
      3: Result.Aborted := gaMemoryLimit;
      4: Result.Aborted := gaHostAbort;
    end;
    ReportSharedCount(Result);
    DescribeThrown(RootAt(TempTop - 1), Result);
    PopRoots(1);
    Pending := VUndefined;
    SetHostResolver(nil);
    SetModuleContentCheck(nil);
    SetArtifactCheck(nil);
    SetProviderGraphCheck(nil);
    Exit;
  end;
  FModuleMark := ModuleCount;
  SetEntryModuleMark(FModuleMark);
  case FSourceKind of
    gskScript: Kind := 1;
    gskModule: Kind := 2;
  else
    if LowerCase(ExtractFileExt(FileName)) = '.mjs' then Kind := 2 else Kind := 0;
  end;
  T := nil;
  { the built-ins a heap snapshot names, as the engine has them before the entry's code }
  if FHeapSnapshots then SnapshotNameBuiltins;
  Start := GetNanoseconds;
  { an opcode or compile profile compiles the entry as it would be compiled to keep its value, whatever the report's
    form, so the code it counts does not depend on --output or --print }
  if FArtifactEntry then
  begin
    ScriptImportSource := '';
    Compiled := LoadEntryArtifact(FileName, T, EntryKey, Result);
    Result.CompileNanoseconds := GetNanoseconds - Start;
    if not Compiled then
    begin
      { work a loaded host environment started does not outlive the failed run }
      DiscardEmbedderWork;
      ReportSharedCount(Result);
      SetHostResolver(nil);
      SetModuleContentCheck(nil);
      SetArtifactCheck(nil);
      SetProviderGraphCheck(nil);
      Exit;
    end;
  end
  else
    Compiled := HostCompileEntry(Source, UnicodeString(FileName), Kind, T, Result.IsModule, Err,
      (FResultForm <> grfNone) or FProfileOpcodes or (TGocciaCompileProfiler.Instance <> nil));
  Result.CompileNanoseconds := GetNanoseconds - Start;
  if not Compiled then
  begin
    Result.ErrorMessage := Err;
    Result.ErrorIsError := True;
    Result.ErrorName := ParseErrorName;
    Result.ErrorText := Err;
    Result.ErrorFile := UnicodeString(FileName);
    Result.ErrorLine := ParseErrorLine;
    Result.ErrorColumn := ParseErrorColumn;
    Result.ErrorSuggestion := ParseErrorSuggestion;
    DiscardEmbedderWork;
    ReportSharedCount(Result);
    Exit;
  end;
  Result.Compiled := True;
  if FHeapSnapshots then
    if Result.IsModule then SnapshotSetEntry(UnicodeString(FileName), Source, Kind)
    else SnapshotNoteEntry(T, UnicodeString(FileName), Source, Kind);
  if Assigned(FOnWarning) then
    for I := 0 to EntryWarningCount - 1 do
      FOnWarning(EntryWarningMessages[I], EntryWarningSuggestions[I], EntryWarningLines[I], EntryWarningColumns[I]);
  SetMaxFrames(FMaxFrames);
  if FProfileOpcodes or FProfileFunctions then ProfileStart(FProfileOpcodes, FProfileFunctions);
  LimitsArm(FTimeoutMs, FMaxInstructions, FMeasureInstructions, FInstructionBudget);
  Start := GetNanoseconds;
  { the completion value, rooted through the event loop }
  PushRoot(VUndefined);
  try
    if Result.IsModule then
    begin
      Phase := 0;
      { the entry's source as given (stdin has no file to read), compiled to keep its last value when reported }
      if not FArtifactEntry then SetProvidedSource(EntryKey, Source)
      else SetProvidedSource(EntryKey, '');
      SetEntryCompletionWanted((FResultForm <> grfNone) or FProfileOpcodes or (TGocciaCompileProfiler.Instance <> nil));
      ModuleWarningEntry := EntryKey;
      try
        Result.Ok := RunModuleFile(EntryKey, Phase) and RunEventLoop;
      finally
        ModuleWarningEntry := '';
      end;
      Completion := EntryCompletion;
      PopRoots(1);
      PushRoot(Completion);
      SetEntryCompletionWanted(False);
    end
    else
    begin
      Completion := VUndefined;
      Result.Ok := True;
      { an explicit Script's imports: their graph linked and evaluated first, then bound as its globals }
      if ScriptImportSource <> '' then
      begin
        Result.Ok := StartScriptImports(EntryKey, ScriptImportSource, Imports);
        if Result.Ok then
        begin
          PushRoot(Imports);
          if PromiseStatus(Imports, Completion) = 0 then Result.Ok := RunEventLoop;
          Result.Ok := Result.Ok and FinishScriptImports(EntryKey, RootAt(TempTop - 1));
          PopRoots(1);
        end;
        Completion := VUndefined;
      end;
      if Result.Ok then Result.Ok := RunScriptRes(T, Completion);
      { nothing allocates between the run's return and the push }
      PopRoots(1);
      PushRoot(Completion);
      Result.Ok := Result.Ok and RunEventLoop;
    end;
    if Result.Ok and FFailOnUnhandledRejection and TakeUnhandledRejection(Thrown) then
    begin
      Pending := Thrown;
      Result.Ok := False;
    end;
    { what a run left rejected goes with it, reported or not: a later run on this engine (a REPL's next input) is
      not blamed for it }
    if FFailOnUnhandledRejection then DiscardRejections;
    { an entry module still waiting once nothing is left to settle it failed (GocciaScript's await of a promise
      that never settles); a rejection nothing handled comes first }
    if Result.Ok and Result.IsModule and EntryEvaluationPending then
    begin
      Pending := MakeError(TypeErrorProto, 'await: Promise did not settle');
      Result.Ok := False;
    end;
    if Result.Ok and (FResultForm <> grfNone) then
    begin
      Result.Ok := ResultText(FResultForm, Result.ResultText);
      Result.HasResult := Result.Ok;
    end;
    Result.ExecuteNanoseconds := GetNanoseconds - Start;
    if not Result.Ok or (AbortKind <> 0) then
    begin
      Result.Ok := False;
      Thrown := Pending;
      PushRoot(Thrown);
      Result.ErrorMessage := ThrownText(RootAt(TempTop - 1));
      if Result.ErrorMessage = '' then Result.ErrorMessage := 'uncaught exception';
      Pending := VUndefined;
      { the limits stay armed until the finally: a spent one refuses every call, so its error is not inspected }
      HasErrorLocation := (AbortKind = 0) and DescribeThrown(RootAt(TempTop - 1), Result);
      PopRoots(1);
      Pending := VUndefined;
      { a static import that did not load: GocciaScript's RuntimeError at the declaration }
      if not HasErrorLocation and LoadFailure(FailPath, FailLine, FailColumn) then
      begin
        Result.ErrorIsError := True;
        if ModuleParseFailure(ParsePath, ParseName, ParseLine, ParseColumn, ParseSuggestion) then
        begin
          { an imported module that did not parse: its SyntaxError where it is, as GocciaRunner reports it }
          Result.ErrorName := ParseName;
          Result.ErrorFile := ParsePath;
          Result.ErrorLine := ParseLine;
          Result.ErrorColumn := ParseColumn;
          Result.ErrorSuggestion := ParseSuggestion;
          TakeResolveCandidate;
        end
        else
        begin
          { a refused read stays the PermissionDenied it is, located at the declaration }
          if Result.ErrorName <> 'PermissionDenied' then Result.ErrorName := 'RuntimeError';
          Result.ErrorFile := FailPath;
          Result.ErrorLine := FailLine;
          Result.ErrorColumn := FailColumn;
          Result.ErrorResolvedPath := TakeResolveCandidate;
        end;
      end;
    end;
    Result.HasInstructionCount := ReadInstructionCount(Result.InstructionCount);
    case AbortKind of
      1: Result.Aborted := gaTimeout;
      2: Result.Aborted := gaInstructionLimit;
      3:
        begin
          Result.Aborted := gaMemoryLimit;
          Result.ErrorName := 'RangeError';
          Result.ErrorText := MemoryLimitMessage;
        end;
      4:
        begin
          Result.Aborted := gaHostAbort;
          Result.ErrorName := 'Error';
          Result.ErrorText := HostAbortMessage;
        end;
    end;
  finally
    if AbortKind <> 0 then DiscardAbortedRunWork(FWeakCleanupMark)
    else if not Result.Ok then DiscardEmbedderWork;
    ProfileStop;
    SetProvidedSource('', '');
    SetHostResolver(nil);
    SetModuleContentCheck(nil);
    SetArtifactCheck(nil);
    SetProviderGraphCheck(nil);
    PopRoots(1);
    { a test or benchmark file's limits cover its suites too (RunTests and RunBenchmarks disarm them) }
    if (AbortKind <> 0) or (not FTestMode and not FBenchMode) then LimitsDisarm
    else StopInstructionMeasurement;
  end;
end;

function TGocciaEngine.RunTests(var Json, Err: UnicodeString; var Aborted: TGocciaAbort): Boolean;
begin
  Aborted := gaNone;
  Json := '';
  Err := '';
  { the tests import as the file did: its resolver (import map, node_modules, providers) and their checks }
  SetHostResolver(FResolver);
  SetModuleContentCheck(FModuleContentCheck);
  SetArtifactCheck(CheckArtifact);
  SetProviderGraphCheck(FProviderGraph);
  Result := False;
  try
    Result := RunTestSuites(Json, Err);
    case AbortKind of
      1: Aborted := gaTimeout;
      2: Aborted := gaInstructionLimit;
      3:
        begin
          Aborted := gaMemoryLimit;
          Err := MemoryLimitMessage;
        end;
      4:
        begin
          Aborted := gaHostAbort;
          Err := HostAbortMessage;
        end;
    end;
    if Aborted <> gaNone then Result := False;
  finally
    if AbortKind <> 0 then DiscardAbortedRunWork(FWeakCleanupMark)
    else if not Result then DiscardEmbedderWork;
    SetHostResolver(nil);
    SetModuleContentCheck(nil);
    SetArtifactCheck(nil);
    SetProviderGraphCheck(nil);
    LimitsDisarm;
  end;
end;

procedure TGocciaEngine.EndTests;
begin
  if AbortKind <> 0 then DiscardAbortedRunWork(FWeakCleanupMark);
  LimitsDisarm;
end;

{ Goccia.HostAPI BenchDescribeThrown: what a benchmark threw, as an uncaught value of a run is described }
procedure DescribeBenchThrown(V: TValue; var E: TGocciaBenchEntry);
var R: TGocciaRunResult;
begin
  R := Default(TGocciaRunResult);
  R.ErrorMessage := ThrownText(V);
  DescribeThrown(V, R);
  E.Error := R.ErrorMessage;
  E.ErrorIsError := R.ErrorIsError;
  E.ErrorName := R.ErrorName;
  E.ErrorText := R.ErrorText;
  E.ErrorFile := R.ErrorFile;
  E.ErrorSuggestion := R.ErrorSuggestion;
  E.ErrorLine := R.ErrorLine;
  E.ErrorColumn := R.ErrorColumn;
end;

function TGocciaEngine.RunBenchmarks(Deterministic: Boolean; out Entries: TGocciaBenchEntries; out Total: Integer;
  out DurationNanoseconds: Int64; var Err: UnicodeString; var Aborted: TGocciaAbort): Boolean;
begin
  BenchDescribeThrown := @DescribeBenchThrown;
  Aborted := gaNone;
  Err := '';
  { the benchmarks import as the file did: its resolver and their checks }
  SetHostResolver(FResolver);
  SetModuleContentCheck(FModuleContentCheck);
  SetArtifactCheck(CheckArtifact);
  SetProviderGraphCheck(FProviderGraph);
  if FProfileOpcodes or FProfileFunctions then ProfileStart(FProfileOpcodes, FProfileFunctions);
  Result := False;
  try
    Result := RunBenchmarkSuites(Deterministic, Entries, Total, DurationNanoseconds, Err);
    case AbortKind of
      1: Aborted := gaTimeout;
      2: Aborted := gaInstructionLimit;
      3:
        begin
          Aborted := gaMemoryLimit;
          Err := MemoryLimitMessage;
        end;
      4:
        begin
          Aborted := gaHostAbort;
          Err := HostAbortMessage;
        end;
    end;
    if Aborted <> gaNone then Result := False;
  finally
    if AbortKind <> 0 then DiscardAbortedRunWork(FWeakCleanupMark)
    else if not Result then DiscardEmbedderWork;
    ProfileStop;
    SetHostResolver(nil);
    SetModuleContentCheck(nil);
    SetArtifactCheck(nil);
    SetProviderGraphCheck(nil);
    LimitsDisarm;
  end;
end;

procedure TGocciaEngine.SetHeapSnapshots(V: Boolean);
begin
  FHeapSnapshots := V;
  HeapObserving := V;
end;

function TGocciaEngine.HeapSnapshotObserved(out Observations: Integer): TArray<TGocciaHeapRoot>;
var I, N: Integer; V: TValue;
begin
  Observations := SnapshotObservations;
  N := SnapshotObservedCount;
  SetLength(Result, N);
  for I := 0 to N - 1 do
  begin
    V := SnapshotObservedAt(I);
    if SnapshotWrite(V, Result[I].Data, Result[I].Refusal) then
      { after writing: describing reads properties, and reading a function's prototype makes it }
      Result[I].Description := SnapshotDescribe(SnapshotObservedAt(I));
  end;
end;

function TGocciaEngine.HeapSnapshotVerify(const Root: TGocciaHeapRoot; const FileName: string; const Source: UnicodeString;
  out Detail: UnicodeString): TGocciaHeapVerdict;
var Err, Refusal: UnicodeString; Kind, Base: Integer; V: TValue; Again: TBytes;
begin
  Detail := '';
  Err := '';
  if not PrepareHost(Err) then
  begin
    Detail := 'the destination engine could not be set up: ' + Err;
    Exit(ghvFailed);
  end;
  if FMaxMemory > 0 then HeapCeiling := FMaxMemory else HeapCeiling := DefaultHeapCeiling;
  NurseryFitCeiling;
  SetActiveProfile(FProfile);
  SetJSModuleSources(FJSModuleSources);
  if (FProjectRoot = '') and FHasCapabilities and not SandboxActive then FProjectRoot := DiscoverProjectRoot(FileName);
  SetReadPolicy(FHasCapabilities and not SandboxActive, FCapabilities, FProjectRoot, AuditRead);
  SetHostResolver(FResolver);
  SetModuleContentCheck(FModuleContentCheck);
  SetArtifactCheck(CheckArtifact);
  SetProviderGraphCheck(FProviderGraph);
  Base := TempTop;
  try
    case FSourceKind of
      gskScript: Kind := 1;
      gskModule: Kind := 2;
    else
      if LowerCase(ExtractFileExt(FileName)) = '.mjs' then Kind := 2 else Kind := 0;
    end;
    SnapshotNameBuiltins;
    SnapshotSetEntry(UnicodeString(FileName), Source, Kind);
    if not SnapshotRestore(Root.Data, V, Refusal) then
    begin
      Detail := Refusal;
      Exit(ghvRefused);
    end;
    PushRoot(V);
    if not SnapshotWrite(RootAt(TempTop - 1), Again, Refusal) then
    begin
      Detail := 'the restored object refused a snapshot: ' + Refusal;
      Exit(ghvFailed);
    end;
    if (Length(Again) <> Length(Root.Data)) or
       ((Length(Again) > 0) and not CompareMem(@Again[0], @Root.Data[0], Length(Again))) then
    begin
      Detail := 're-snapshot bytes differ';
      Exit(ghvFailed);
    end;
    if SnapshotDescribe(RootAt(TempTop - 1)) <> Root.Description then
    begin
      Detail := 'canonical descriptions differ';
      Exit(ghvFailed);
    end;
    Result := ghvPassed;
  finally
    PopRoots(TempTop - Base);
    Pending := VUndefined;
    SetHostResolver(nil);
    SetModuleContentCheck(nil);
    SetArtifactCheck(nil);
    SetProviderGraphCheck(nil);
  end;
end;

function TGocciaEngine.LateRejection: UnicodeString;
var Reason: TValue;
begin
  Result := '';
  Reason := VUndefined;
  if not TakeUnhandledRejection(Reason) then Exit;
  PushRoot(Reason);
  Result := ThrownText(RootAt(TempTop - 1));
  PopRoots(1);
end;

end.
