unit G2.Embed;
{ The engine as a host embeds it: one TG2Engine per thread at a time (its state is the thread's, G2.Engine).
  Create builds the engine; the settings (language profile, source kind, host config, limits) apply to the run
  that follows; Run compiles and runs one entry file as a script or a module, then its jobs and timers (the
  event loop), and reports how it ended. The command-line hosts (g2.dpr, and the apps through the host engine)
  build on this instead of on the engine's units. }
{$mode delphiunicode}{$H+}
{$I G2.Checks.inc}

interface

uses
  SysUtils, Goccia.Capabilities, Goccia.CapabilityAudit, Goccia.HostEnvironment, Goccia.ModuleResolver, G2.Core, G2.HostAPI, G2.VM;

type
  { a console line and the method that wrote it (G2.HostAPI TConsoleSink) }
  TG2ConsoleEvent = TConsoleSink;
  { how the entry's completion value is reported: not at all, as String(value) (--print), or as JSON (a CLI's JSON
    report: JSON.stringify's text, with undefined as null and a function or what JSON leaves out as its quoted
    String form) }
  TG2ResultForm = (g2rfNone, g2rfString, g2rfJSON);
  { a warning of the entry's parse (warning-unsupported-features), given to the host before the entry runs }
  TG2WarningEvent = procedure(const Message, Suggestion: UnicodeString; Line, Column: Integer) of object;

  { how the entry is compiled: a script, a module, or as the source shows (a top-level import or export
    declaration makes a module); a .mjs file is a module unless the kind says otherwise }
  TG2SourceKind = (g2skDetect, g2skScript, g2skModule);
  { a run ended by a spent limit (G2.VM LimitsArm) rather than by the script }
  TG2Abort = (g2aNone, g2aTimeout, g2aInstructionLimit,
    { a full collection left more live than the memory limit (MaxMemory); ErrorText says how much }
    g2aMemoryLimit,
    { the host ended the run, past every handler: AuditSink could not record a decision (ErrorText says why) }
    g2aHostAbort);

  { the engine's heap now (Memory): what a CLI's JSON report shows under memory.gc }
  TG2MemoryStats = record
    { the heap's bytes, garbage included until a collection frees it; the most it has held }
    LiveBytes, PeakLiveBytes: Int64;
    { every byte the heap has allocated since the engine was made }
    AllocatedBytes: Int64;
    { the heap limit (MaxMemory, else the default ceiling) }
    LimitBytes: Int64;
    Collections: Integer;
  end;

  TG2RunResult = record
    Ok: Boolean;
    Aborted: TG2Abort;
    { the entry was compiled: False for a syntax error, whose message is in ErrorMessage }
    Compiled: Boolean;
    IsModule: Boolean;
    { the uncaught error (ToString), or the limit's message when Aborted }
    ErrorMessage: UnicodeString;
    { the error in parts, for a host's diagnostic: its name and message (ErrorIsError: the thrown value was an
      object with a string name or message, or a syntax error; otherwise ErrorMessage is the thrown value as a
      string), where it was made (the first frame of its stack, or where the source was rejected; ErrorLine 0
      when unknown) and the parser's suggestion }
    ErrorIsError: Boolean;
    ErrorName, ErrorText, ErrorFile, ErrorSuggestion: UnicodeString;
    ErrorLine, ErrorColumn: Integer;
    { a static import that failed to load: the path its resolution tried, for the host's "Resolved to" line }
    ErrorResolvedPath: UnicodeString;
    { nanoseconds spent parsing and compiling the entry (lexing included), and running it with its event loop }
    CompileNanoseconds, ExecuteNanoseconds: Int64;
    { ResultForm: the entry's completion value as String(value) or JSON (undefined for a module), converted after
      the event loop under the run's limits; a conversion that throws fails the run }
    HasResult: Boolean;
    ResultText: UnicodeString;
  end;

  TG2Engine = class
  private
    FProfile: TLangProfile;
    FSourceKind: TG2SourceKind;
    FConfigJson: UnicodeString;
    FTestMode: Boolean;
    FTimeoutMs, FMaxInstructions, FMaxMemory: Int64;
    FMaxFrames: Integer;
    FHostReady: Boolean;
    FExposeTimers: Boolean;
    FResultForm: TG2ResultForm;
    FOnConsole: TG2ConsoleEvent;
    FResolver: TModuleResolver;
    FOnWarning: TG2WarningEvent;
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
    FAuditSink: TGocciaCapabilityAuditSink;
    { the capability set FFI.open judges libraries by (Capabilities); FFI is installed when it grants ffi }
    FCapabilities: TGocciaCapabilities;
    FHasCapabilities: Boolean;
    FFetchMaxResponseBytes: Integer;
    FCoverage: Boolean;
    { the entry's file name (Run): an audited decision made outside every script frame is the entry's }
    FRunFile: UnicodeString;
    function NextRandom(Realm: Integer; out D: Double): Boolean;
    { G2.VM's AuditHook: the script's decisions, located at its call site }
    function AuditFromScript(Kind: TAuditKind; Allow: Boolean; const Subject, Reason: UnicodeString): Boolean;
    procedure SetCapabilities(const Value: TGocciaCapabilities);
    function ProviderRandom(Realm: Integer; out D: Double): Boolean;
    function ProviderClock(Kind: Integer; out Ns: Int64): Boolean;
    function LoadHostEnvironment: Boolean;
    function InstallHost(var Err: UnicodeString): Boolean;
  public
    { builds this thread's engine; one at a time per thread }
    constructor Create;
    { frees this thread's engine: values and closures from it are invalid afterwards }
    destructor Destroy; override;
    { the host's globals and modules (console, timers, URL, goccia:*, ...), installed before the first run: the
      ShadowRealm switch and ConfigJson (the host config as a JSON object of strings, goccia.json's keys) are
      read then. Run installs them when the host has not. }
    function PrepareHost(var Err: UnicodeString): Boolean;
    function Run(const FileName: string; const Source: UnicodeString): TG2RunResult;
    { whether the run's entry loaded a module from Path (an absolute path), not the host before it: a host shows a
      code frame of such a file only }
    function ModuleLoaded(const Path: UnicodeString): Boolean;
    { the engine's heap now; call it on the engine's thread }
    function Memory: TG2MemoryStats;
    { a decision of the host's own (a node_modules grant) to AuditSink, at the script's call site, during a run on
      the engine's thread: False when the sink raised, the run then ending (g2aHostAbort) }
    function Audit(Kind: TGocciaCapabilityKind; Allow: Boolean; const Subject, Reason: string): Boolean;
    { globals injection before Run (G2.HostAPI): a data file's globals, and one --global name=value }
    function InjectDataGlobals(const Text, Ext, Path: UnicodeString; var Err: UnicodeString): Boolean;
    function InjectInlineGlobal(const Name, ValueText: UnicodeString; var Err: UnicodeString): Boolean;
    { virtual modules before Run (G2.HostAPI) }
    function DefineConfigVirtualModules(const ConfigPath: UnicodeString; var Err: UnicodeString): Boolean;
    function DefineManifestVirtualModules(const ManifestPath: UnicodeString; var Err: UnicodeString): Boolean;
    function DefineInlineVirtualModule(const Definition, Base: UnicodeString; var Err: UnicodeString): Boolean;
    function InjectModuleGlobals(const Path: UnicodeString; var Err: UnicodeString): Boolean;
    function DefineModuleManifestVirtualModules(const Path: UnicodeString; var Err: UnicodeString): Boolean;
    property Profile: TLangProfile read FProfile write FProfile;
    property SourceKind: TG2SourceKind read FSourceKind write FSourceKind;
    property ConfigJson: UnicodeString read FConfigJson write FConfigJson;
    { the test framework's host: no process exit on an uncaught error, the testing API's globals }
    property TestMode: Boolean read FTestMode write FTestMode;
    { setTimeout and the other timer globals, and goccia:timers (True by default); without them the timer queue
      still serves the host's own users (AbortSignal.timeout) }
    property ExposeTimers: Boolean read FExposeTimers write FExposeTimers;
    { report the entry's completion value (TG2RunResult.ResultText) }
    property ResultForm: TG2ResultForm read FResultForm write FResultForm;
    { console lines go here instead of to stdout (nil: stdout) }
    property OnConsole: TG2ConsoleEvent read FOnConsole write FOnConsole;
    { the run's import resolution (an import map, aliases, node_modules), not owned; nil for relative and absolute
      paths only }
    property Resolver: TModuleResolver read FResolver write FResolver;
    property OnWarning: TG2WarningEvent read FOnWarning write FOnWarning;
    { GocciaScript's deterministic profile (--deterministic): the epoch and monotonic clocks at 0, the time zone
      UTC, Math.random a seeded stream; set before the first run or PrepareHost }
    property Deterministic: Boolean read FDeterministic write FDeterministic;
    { GocciaScript's JavaScript host environment (--host-environment): the module at this path, evaluated before
      the first run's entry, exports the clocks, time zone and random streams the script reads (docs/
      host-environment.md); its time zone is read once, then. Not with Deterministic }
    property HostEnvironment: UnicodeString read FHostEnvironment write FHostEnvironment;
    { the capability decisions of the run (GocciaScript's capability audit, docs/capability-audit.md): the Function
      constructor's, a ShadowRealm's, an eval's, and what the host reports through Audit, each at the script's call
      site. A sink that raises ends the run past every handler (g2aHostAbort). Set before Run }
    property AuditSink: TGocciaCapabilityAuditSink read FAuditSink write FAuditSink;
    { the run's capability set (ADR 0122): with the ffi capability the FFI global is installed, and FFI.open judges
      each library by its scopes. Set before the host is prepared; without it there is no FFI }
    property Capabilities: TGocciaCapabilities read FCapabilities write SetCapabilities;
    { fetch()'s response-body ceiling in bytes (--max-fetch-bytes); 0 for the default (8 MiB) }
    property FetchMaxResponseBytes: Integer read FFetchMaxResponseBytes write FFetchMaxResponseBytes;
    { coverage of the run's code (Goccia.Coverage): its lines, branches and functions recorded in the calling
      thread's TGocciaCoverageTracker, which the host creates and enables; the run takes the interpreter only }
    property Coverage: Boolean read FCoverage write FCoverage;
    { a promise rejected with no handler that still has none when the run ends fails the run, its reason the
      uncaught error (GocciaRunner) }
    property FailOnUnhandledRejection: Boolean read FFailOnUnhandledRejection write FFailOnUnhandledRejection;
    { 0 for none }
    property TimeoutMs: Int64 read FTimeoutMs write FTimeoutMs;
    property MaxInstructions: Int64 read FMaxInstructions write FMaxInstructions;
    property MaxFrames: Integer read FMaxFrames write FMaxFrames;
    { the heap limit in bytes (HeapCeiling, for every engine of the process); 0 for the default
      (G2.HostAPI DefaultHeapCeiling) }
    property MaxMemory: Int64 read FMaxMemory write FMaxMemory;
  end;

{ the recommended language profile: restricted, every compatibility switch off }
function RecommendedProfile: TLangProfile;

implementation

uses
  Math, TimingUtils, BigInteger, Goccia.Temporal.TimeZone, G2.Bytecode, G2.Engine, G2.Eval, G2.Modules,
  G2.Promise, G2.Builtins, G2.BigIntV, G2.ObjModel, G2.FFI, G2.Fetch, G2.Compiler;

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

{ the report forms of the value at the top of the root stack; False with an exception pending when a conversion threw }
function ResultText(Form: TG2ResultForm; var Text: UnicodeString): Boolean;
var V, S: TValue;
begin
  V := RootAt(TempTop - 1);
  if Form = g2rfString then
  begin
    Result := ToStringV(V, S) and IsString(S);
    if Result then Text := StrToUnicode(StrOf(S));
    Exit;
  end;
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
  Result := ToStringV(V, S) and IsString(S);
  if Result then Text := JSONQuoted(StrToUnicode(StrOf(S)));
end;

{ "    at name (source:line:column)": the first frame line of a stack text }
procedure FirstFrameOf(const Stack: UnicodeString; var R: TG2RunResult);
var P, Open, Close, C1, C2: Integer; Loc: UnicodeString;
begin
  P := Pos(#10'    at ', Stack);
  if P = 0 then Exit;
  Close := P + 1;
  while (Close <= Length(Stack)) and (Stack[Close] <> #10) do Inc(Close);
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
  okDate = 18; { G2.Date's }
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
  if ToStringV(V, S) and IsString(S) then Result := StrToUnicode(StrOf(S));
  Pending := VUndefined;
end;

{ the uncaught value in parts (R.ErrorMessage already holds its text), from its string data properties only }
procedure DescribeThrown(V: TValue; var R: TG2RunResult);
var Stack, Text: UnicodeString;
begin
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
    if ErrorStackProvenance(RootAt(TempTop - 1), Stack) then FirstFrameOf(Stack, R);
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

constructor TG2Engine.Create;
begin
  inherited Create;
  FProfile := RecommendedProfile;
  FConfigJson := '{}';
  FExposeTimers := True;
  EngineInit;
end;

function TG2Engine.NextRandom(Realm: Integer; out D: Double): Boolean;
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
function TG2Engine.ProviderRandom(Realm: Integer; out D: Double): Boolean;
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
function TG2Engine.ProviderClock(Kind: Integer; out Ns: Int64): Boolean;
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

{ the provider module evaluated, its exports checked and taken, the clocks and Math.random routed through them,
  performance's time origin taken from them, then the time zone read; False with the error pending }
function TG2Engine.LoadHostEnvironment: Boolean;
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

procedure TG2Engine.SetCapabilities(const Value: TGocciaCapabilities);
begin
  FCapabilities := Value;
  FHasCapabilities := True;
end;

function TG2Engine.ModuleLoaded(const Path: UnicodeString): Boolean;
begin
  Result := ModuleLoadedSince(Path, FModuleMark);
end;

function TG2Engine.Memory: TG2MemoryStats;
begin
  Result.LiveBytes := HeapUsedBytes;
  Result.PeakLiveBytes := HeapPeakBytes;
  Result.AllocatedBytes := HeapAllocatedBytes;
  if FMaxMemory > 0 then Result.LimitBytes := FMaxMemory else Result.LimitBytes := DefaultHeapCeiling;
  Result.Collections := GCMinorCount + GCMajorCount;
end;

function TG2Engine.Audit(Kind: TGocciaCapabilityKind; Allow: Boolean; const Subject, Reason: string): Boolean;
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

function TG2Engine.AuditFromScript(Kind: TAuditKind; Allow: Boolean; const Subject, Reason: UnicodeString): Boolean;
const
  Kinds: array[TAuditKind] of TGocciaCapabilityKind = (gckFunctionConstructor, gckShadowRealm, gckEval, gckFFIOpen,
    gckNetFetch, gckNetDispatch);
begin
  Result := Audit(Kinds[Kind], Allow, Subject, Reason);
end;

destructor TG2Engine.Destroy;
begin
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
  EngineDone;
  inherited Destroy;
end;

function TG2Engine.InstallHost(var Err: UnicodeString): Boolean;
var J: TValue;
begin
  { before the host's globals: performance.timeOrigin is the clock when they install }
  if FDeterministic then
  begin
    SetClockOverride(TGocciaHostEnvironment.DeterministicEpochNanoseconds / 1000000,
      TGocciaHostEnvironment.DeterministicMonotonicNanoseconds / 1000000);
    TimeZoneOverride := TGocciaHostEnvironment.DeterministicTimeZone;
    SetLength(FRandoms, 1);
    FRandoms[0] := TGocciaSeededHostRandom.Create(TGocciaHostEnvironment.DeterministicSeed);
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
  Result := InstallHostGlobals(FProfile.ShadowRealm, FTestMode, FConfigJson, Err, FExposeTimers);
  if Result and FHasCapabilities and FCapabilities.Grants(gcFFI) then Result := InstallFFI(FCapabilities, Err);
  FHostReady := Result;
  if not Result then Exit;
  if FirstInEngine(StringifyRootedGen) then AddRoot(@StringifyFn);
  StringifyFn := VUndefined;
  if GetV(GlobalObj, AtomOf('JSON'), J) and IsObject(J) and GetV(J, AtomOf('stringify'), J) and IsCallable(J) then
    StringifyFn := J;
  Pending := VUndefined;
end;

function TG2Engine.PrepareHost(var Err: UnicodeString): Boolean;
begin
  if FHostReady then Exit(True);
  Result := InstallHost(Err);
end;

function TG2Engine.InjectDataGlobals(const Text, Ext, Path: UnicodeString; var Err: UnicodeString): Boolean;
begin
  Result := PrepareHost(Err) and G2.HostAPI.InjectDataGlobals(Text, Ext, Path, Err);
end;

function TG2Engine.InjectInlineGlobal(const Name, ValueText: UnicodeString; var Err: UnicodeString): Boolean;
begin
  Result := PrepareHost(Err) and G2.HostAPI.InjectInlineGlobal(Name, ValueText, Err);
end;

function TG2Engine.DefineConfigVirtualModules(const ConfigPath: UnicodeString; var Err: UnicodeString): Boolean;
begin
  Result := PrepareHost(Err) and G2.HostAPI.DefineConfigVirtualModules(ConfigPath, Err);
end;

function TG2Engine.DefineManifestVirtualModules(const ManifestPath: UnicodeString; var Err: UnicodeString): Boolean;
begin
  Result := PrepareHost(Err) and G2.HostAPI.DefineManifestVirtualModules(ManifestPath, Err);
end;

function TG2Engine.DefineInlineVirtualModule(const Definition, Base: UnicodeString; var Err: UnicodeString): Boolean;
begin
  Result := PrepareHost(Err) and G2.HostAPI.DefineInlineVirtualModule(Definition, Base, Err);
end;

function TG2Engine.InjectModuleGlobals(const Path: UnicodeString; var Err: UnicodeString): Boolean;
begin
  Result := PrepareHost(Err) and G2.HostAPI.InjectModuleGlobals(Path, Err);
end;

function TG2Engine.DefineModuleManifestVirtualModules(const Path: UnicodeString; var Err: UnicodeString): Boolean;
begin
  Result := PrepareHost(Err) and G2.HostAPI.DefineModuleManifestVirtualModules(Path, Err);
end;

function TG2Engine.Run(const FileName: string; const Source: UnicodeString): TG2RunResult;
var Kind, Phase, I, FailLine, FailColumn: Integer; T: PFuncTemplate; Err, FailPath: UnicodeString;
  S, Thrown, Completion: TValue; Start: Int64;
  Compiled: Boolean;
begin
  Result := Default(TG2RunResult);
  Err := '';
  if not PrepareHost(Err) then
  begin
    Result.ErrorMessage := Err;
    Exit;
  end;
  if FMaxMemory > 0 then HeapCeiling := FMaxMemory else HeapCeiling := DefaultHeapCeiling;
  SetActiveProfile(FProfile);
  SetConsoleSink(FOnConsole);
  { coverage: the run's code compiled with a line for every source line, and run through the coverage hook }
  CompileCoverage := FCoverage;
  CoverageOn := FCoverage;
  FRunFile := FileName;
  if Assigned(FAuditSink) then AuditHook := AuditFromScript else AuditHook := nil;
  SetHostResolver(FResolver);
  ClearLoadFailure;
  TakeResolveCandidate;
  { the JavaScript host environment, before the entry: a provider that does not load fails the run with its error }
  if (FHostEnvironment <> '') and not FEnvLoaded and not LoadHostEnvironment then
  begin
    PushRoot(Pending);
    Pending := VUndefined;
    Result.ErrorMessage := ThrownText(RootAt(TempTop - 1));
    if Result.ErrorMessage = '' then Result.ErrorMessage := 'uncaught exception';
    Pending := VUndefined;
    DescribeThrown(RootAt(TempTop - 1), Result);
    PopRoots(1);
    Pending := VUndefined;
    SetHostResolver(nil);
    Exit;
  end;
  FModuleMark := ModuleCount;
  SetEntryModuleMark(FModuleMark);
  case FSourceKind of
    g2skScript: Kind := 1;
    g2skModule: Kind := 2;
  else
    if LowerCase(ExtractFileExt(FileName)) = '.mjs' then Kind := 2 else Kind := 0;
  end;
  T := nil;
  Start := GetNanoseconds;
  Compiled := HostCompileEntry(Source, UnicodeString(FileName), Kind, T, Result.IsModule, Err,
    FResultForm <> g2rfNone);
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
    Exit;
  end;
  Result.Compiled := True;
  if Assigned(FOnWarning) then
    for I := 0 to EntryWarningCount - 1 do
      FOnWarning(EntryWarningMessages[I], EntryWarningSuggestions[I], EntryWarningLines[I], EntryWarningColumns[I]);
  SetMaxFrames(FMaxFrames);
  LimitsArm(FTimeoutMs, FMaxInstructions);
  Start := GetNanoseconds;
  { the completion value, rooted through the event loop }
  PushRoot(VUndefined);
  try
    if Result.IsModule then
    begin
      Phase := 0;
      { the entry's source as given (stdin has no file to read), compiled to keep its last value when reported }
      SetProvidedSource(UnicodeString(ExpandFileName(FileName)), Source);
      SetEntryCompletionWanted(FResultForm <> g2rfNone);
      Result.Ok := RunModuleFile(UnicodeString(ExpandFileName(FileName)), Phase) and RunEventLoop;
      Completion := EntryCompletion;
      PopRoots(1);
      PushRoot(Completion);
      SetEntryCompletionWanted(False);
    end
    else
    begin
      Completion := VUndefined;
      Result.Ok := RunScriptRes(T, Completion);
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
    if Result.Ok and (FResultForm <> g2rfNone) then
    begin
      Result.Ok := ResultText(FResultForm, Result.ResultText);
      Result.HasResult := Result.Ok;
    end;
    Result.ExecuteNanoseconds := GetNanoseconds - Start;
    case AbortKind of
      1: Result.Aborted := g2aTimeout;
      2: Result.Aborted := g2aInstructionLimit;
      3:
        begin
          Result.Aborted := g2aMemoryLimit;
          Result.ErrorName := 'RangeError';
          Result.ErrorText := MemoryLimitMessage;
        end;
      4:
        begin
          Result.Aborted := g2aHostAbort;
          Result.ErrorName := 'Error';
          Result.ErrorText := HostAbortMessage;
        end;
    end;
    if not Result.Ok or (Result.Aborted <> g2aNone) then
    begin
      Result.Ok := False;
      Thrown := Pending;
      PushRoot(Thrown);
      Result.ErrorMessage := ThrownText(RootAt(TempTop - 1));
      if Result.ErrorMessage = '' then Result.ErrorMessage := 'uncaught exception';
      Pending := VUndefined;
      { the limits stay armed until the finally: a spent one refuses every call, so its error is not inspected }
      if Result.Aborted = g2aNone then DescribeThrown(RootAt(TempTop - 1), Result);
      PopRoots(1);
      Pending := VUndefined;
      { a static import that did not load: GocciaScript's RuntimeError at the declaration }
      if LoadFailure(FailPath, FailLine, FailColumn) then
      begin
        Result.ErrorIsError := True;
        Result.ErrorName := 'RuntimeError';
        Result.ErrorFile := FailPath;
        Result.ErrorLine := FailLine;
        Result.ErrorColumn := FailColumn;
        Result.ErrorResolvedPath := TakeResolveCandidate;
      end;
    end;
  finally
    SetProvidedSource('', '');
    SetHostResolver(nil);
    PopRoots(1);
    LimitsDisarm;
  end;
end;

end.
