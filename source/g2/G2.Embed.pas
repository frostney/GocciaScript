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
  SysUtils, G2.Core;

type
  { how the entry is compiled: a script, a module, or as the source shows (a top-level import or export
    declaration makes a module); a .mjs file is a module unless the kind says otherwise }
  TG2SourceKind = (g2skDetect, g2skScript, g2skModule);
  { a run ended by a spent limit (G2.VM LimitsArm) rather than by the script }
  TG2Abort = (g2aNone, g2aTimeout, g2aInstructionLimit);

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
    { nanoseconds spent parsing and compiling the entry (lexing included), and running it with its event loop }
    CompileNanoseconds, ExecuteNanoseconds: Int64;
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
    property Profile: TLangProfile read FProfile write FProfile;
    property SourceKind: TG2SourceKind read FSourceKind write FSourceKind;
    property ConfigJson: UnicodeString read FConfigJson write FConfigJson;
    { the test framework's host: no process exit on an uncaught error, the testing API's globals }
    property TestMode: Boolean read FTestMode write FTestMode;
    { setTimeout and the other timer globals, and goccia:timers (True by default); without them the timer queue
      still serves the host's own users (AbortSignal.timeout) }
    property ExposeTimers: Boolean read FExposeTimers write FExposeTimers;
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
  TimingUtils, G2.Bytecode, G2.VM, G2.Engine, G2.Eval, G2.Modules, G2.HostAPI;

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

destructor TG2Engine.Destroy;
begin
  LimitsDisarm;
  SetMaxFrames(0);
  EngineDone;
  inherited Destroy;
end;

function TG2Engine.InstallHost(var Err: UnicodeString): Boolean;
begin
  Result := InstallHostGlobals(FProfile.ShadowRealm, FTestMode, FConfigJson, Err, FExposeTimers);
  FHostReady := Result;
end;

function TG2Engine.PrepareHost(var Err: UnicodeString): Boolean;
begin
  if FHostReady then Exit(True);
  Result := InstallHost(Err);
end;

function TG2Engine.Run(const FileName: string; const Source: UnicodeString): TG2RunResult;
var Kind, Phase: Integer; T: PFuncTemplate; Err: UnicodeString; S, Thrown: TValue; Start: Int64; Compiled: Boolean;
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
  case FSourceKind of
    g2skScript: Kind := 1;
    g2skModule: Kind := 2;
  else
    if LowerCase(ExtractFileExt(FileName)) = '.mjs' then Kind := 2 else Kind := 0;
  end;
  T := nil;
  Start := GetNanoseconds;
  Compiled := HostCompileEntry(Source, UnicodeString(FileName), Kind, T, Result.IsModule, Err);
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
  SetMaxFrames(FMaxFrames);
  LimitsArm(FTimeoutMs, FMaxInstructions);
  Start := GetNanoseconds;
  try
    if Result.IsModule then
    begin
      Phase := 0;
      Result.Ok := RunModuleFile(UnicodeString(ExpandFileName(FileName)), Phase) and RunEventLoop;
    end
    else Result.Ok := RunScript(T) and RunEventLoop;
    Result.ExecuteNanoseconds := GetNanoseconds - Start;
    case AbortKind of
      1: Result.Aborted := g2aTimeout;
      2: Result.Aborted := g2aInstructionLimit;
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
    end;
  finally
    LimitsDisarm;
  end;
end;

end.
