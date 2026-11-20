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
  SysUtils, Goccia.HostEnvironment, Goccia.ModuleResolver, G2.Core, G2.HostAPI;

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
    g2aMemoryLimit);

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
    function NextRandom(Realm: Integer): Double;
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
  Math, TimingUtils, G2.Bytecode, G2.VM, G2.Engine, G2.Eval, G2.Modules, G2.Promise, G2.Builtins;

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

{ the uncaught value in parts (R.ErrorMessage already holds its string form) }
procedure DescribeThrown(V: TValue; var R: TG2RunResult);
var N, M, St: TValue;
begin
  if not IsObject(V) then Exit;
  PushRoot(V);
  try
    if GetV(RootAt(TempTop - 1), AtomOf('name'), N) and IsString(N) then
    begin
      R.ErrorIsError := True;
      R.ErrorName := StrToUnicode(StrOf(N));
    end;
    if GetV(RootAt(TempTop - 1), AtomOf('message'), M) and IsString(M) then
    begin
      R.ErrorIsError := True;
      R.ErrorText := StrToUnicode(StrOf(M));
    end;
    if GetV(RootAt(TempTop - 1), AtomOf('stack'), St) and IsString(St) then
      FirstFrameOf(StrToUnicode(StrOf(St)), R);
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

function TG2Engine.NextRandom(Realm: Integer): Double;
begin
  if Realm >= Length(FRandoms) then SetLength(FRandoms, Realm + 1);
  if FRandoms[Realm] = nil then
  begin
    Inc(FRandomForks);
    FRandoms[Realm] := FRandoms[0].Fork(FRandomForks);
  end;
  Result := FRandoms[Realm].NextDouble;
end;

destructor TG2Engine.Destroy;
begin
  HostRandom := nil;
  TimeZoneOverride := '';
  if FDeterministic then SetClockOverride(NaN, NaN);
  LimitsDisarm;
  SetMaxFrames(0);
  HostModulesDone;
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
  end
  else
    SeedMathRandom(QWord(GetTickCount64) xor (QWord(GetProcessID) shl 32) xor QWord(PtrUInt(Self)));
  Result := InstallHostGlobals(FProfile.ShadowRealm, FTestMode, FConfigJson, Err, FExposeTimers);
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
  SetHostResolver(FResolver);
  ClearLoadFailure;
  TakeResolveCandidate;
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
    end;
    if not Result.Ok or (Result.Aborted <> g2aNone) then
    begin
      Result.Ok := False;
      Thrown := Pending;
      PushRoot(Thrown);
      if ToStringV(RootAt(TempTop - 1), S) and IsString(S) then Result.ErrorMessage := StrToUnicode(StrOf(S));
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
