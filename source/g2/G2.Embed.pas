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
  end;

  TG2Engine = class
  private
    FProfile: TLangProfile;
    FSourceKind: TG2SourceKind;
    FConfigJson: UnicodeString;
    FTestMode: Boolean;
    FTimeoutMs, FMaxInstructions: Int64;
    FMaxFrames: Integer;
    FHostReady: Boolean;
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
    { 0 for none }
    property TimeoutMs: Int64 read FTimeoutMs write FTimeoutMs;
    property MaxInstructions: Int64 read FMaxInstructions write FMaxInstructions;
    property MaxFrames: Integer read FMaxFrames write FMaxFrames;
  end;

{ the recommended language profile: restricted, every compatibility switch off }
function RecommendedProfile: TLangProfile;

implementation

uses
  G2.Bytecode, G2.VM, G2.Engine, G2.Eval, G2.Modules, G2.HostAPI;

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
  Result := InstallHostGlobals(FProfile.ShadowRealm, FTestMode, FConfigJson, Err);
  FHostReady := Result;
end;

function TG2Engine.PrepareHost(var Err: UnicodeString): Boolean;
begin
  if FHostReady then Exit(True);
  Result := InstallHost(Err);
end;

function TG2Engine.Run(const FileName: string; const Source: UnicodeString): TG2RunResult;
var Kind, Phase: Integer; T: PFuncTemplate; Err: UnicodeString; S: TValue;
begin
  Result := Default(TG2RunResult);
  Err := '';
  if not PrepareHost(Err) then
  begin
    Result.ErrorMessage := Err;
    Exit;
  end;
  SetActiveProfile(FProfile);
  case FSourceKind of
    g2skScript: Kind := 1;
    g2skModule: Kind := 2;
  else
    if LowerCase(ExtractFileExt(FileName)) = '.mjs' then Kind := 2 else Kind := 0;
  end;
  T := nil;
  if not HostCompileEntry(Source, UnicodeString(FileName), Kind, T, Result.IsModule, Err) then
  begin
    Result.ErrorMessage := Err;
    Exit;
  end;
  Result.Compiled := True;
  SetMaxFrames(FMaxFrames);
  LimitsArm(FTimeoutMs, FMaxInstructions);
  try
    if Result.IsModule then
    begin
      Phase := 0;
      Result.Ok := RunModuleFile(UnicodeString(ExpandFileName(FileName)), Phase) and RunEventLoop;
    end
    else Result.Ok := RunScript(T) and RunEventLoop;
    case AbortKind of
      1: Result.Aborted := g2aTimeout;
      2: Result.Aborted := g2aInstructionLimit;
    end;
    if not Result.Ok or (Result.Aborted <> g2aNone) then
    begin
      Result.Ok := False;
      if ToStringV(Pending, S) and IsString(S) then Result.ErrorMessage := StrToUnicode(StrOf(S));
      if Result.ErrorMessage = '' then Result.ErrorMessage := 'uncaught exception';
      Pending := VUndefined;
    end;
  finally
    LimitsDisarm;
  end;
end;

end.
