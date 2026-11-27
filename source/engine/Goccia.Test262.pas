unit Goccia.Test262;
{ test262's host, under GocciaScript's grading contract 4 (docs/test262.md): the hidden GocciaHost object that
  $262.js (source/engine/prelude/test262host.js, compiled in) turns into $262 (evalScript, gc, detachArrayBuffer, createRealm, IsHTMLDDA,
  AbstractModuleSource, the agent API), and one test run in this process. Each harness file runs as its own
  non-strict global Script in one realm, then the test file: a Script, prefixed with '"use strict";' and a newline
  in a strict run, or a module. GocciaTest262Runner forks a process per run and calls RunTest262Case in it;
  `goccia --test262` runs one and prints its outcome. }
{$mode delphiunicode}{$H+}
{$I Goccia.Checks.inc}

interface

type
  { how a run ended. Kind: ok; parse (the parser or the module linker rejected the test before evaluation: an
    early error, Name its constructor's name); throw (a value escaped evaluation, Name its constructor's name read
    from data properties only, so no getter runs); harness (a harness file could not be read, compiled or run:
    FileName names it); compile (the engine's compiler refused the parsed test); crash (a Pascal exception in the engine,
    Name its class) }
  TGocciaT262Outcome = record
    Kind, Name, Message, FileName: UnicodeString;
    { a thrown error's provenance, "    at source:line:column" where it was made, or '' }
    Site: UnicodeString;
  end;

{ the hidden GocciaHost object of the running realm }
procedure InstallHostHooks;
{ the engine of a test262 run in this process: GOCCIA_NURSERY_KB and GOCCIA_GC_VERIFY (scripts/engine/gcstress.sh's
  settings), the agent host (CannotBlock: the main agent may not suspend, test262's CanBlockIsFalse), GocciaHost with
  the agent API moved into it (so $262 stays the only harness global) and print, which records into PrintText }
procedure InitTest262Engine(CannotBlock: Boolean);
{ the engine InitTest262Engine made, ended: a thread that runs another case after it makes a new one }
procedure DoneTest262Engine;
{ --deterministic for the engine InitTest262Engine made: time frozen at the epoch in UTC, and Math.random from the
  portable seeded stream (Goccia.HostEnvironment), a realm $262.createRealm makes forking its creator's stream }
procedure SetTest262Deterministic;
{ one run on the engine InitTest262Engine made: the harness files in order, then TestFile }
function RunTest262Case(const Harness: array of string; const TestFile: string; Strict, IsModule: Boolean): TGocciaT262Outcome;
{ what the run's print calls printed, a line each }
function Test262PrintText: UnicodeString;
{ a UTF-8 source file (a BOM dropped); False when it cannot be read }
function ReadTest262Source(const Path: string; var Src: UnicodeString): Boolean;

implementation

uses
  Classes, SysUtils, Goccia.Core, Goccia.Instructions, Goccia.Compiler, Goccia.VM, Goccia.Eval, Goccia.Promise, Goccia.Atomics, Goccia.Agent,
  Goccia.Engine, Goccia.TypedArrays, Goccia.Modules, Goccia.Builtins, Goccia.ThreadCleanupRegistry, Goccia.HostEnvironment,
  TextEncoding, Goccia.EngineFault;

{$I Goccia.Test262Host.inc}

threadvar
  PrintBuf: UnicodeString;   { (per thread: a runner without fork runs cases on several threads) }

type
  { the realms' Math.random streams under --deterministic: realm 0 the seeded root, a created realm's a fork of its
    creator's (the host environment of the new realm forks the creator's stream, and its Math.random forks that, as
    the old engine's test262 host did, so runs keep their values) }
  TT262Randoms = class
    Streams: array of IGocciaHostRandom;
    Forks: array of Integer;
    function Next(Realm: Integer; out D: Double): Boolean;
    procedure Created(Creator, Realm: Integer);
  end;

threadvar
  T262Randoms: TT262Randoms;

function TT262Randoms.Next(Realm: Integer; out D: Double): Boolean;
begin
  if (Realm >= Length(Streams)) or (Streams[Realm] = nil) then Created(0, Realm);
  D := Streams[Realm].NextDouble;
  Result := True;
end;

procedure TT262Randoms.Created(Creator, Realm: Integer);
begin
  if Realm >= Length(Streams) then
  begin
    SetLength(Streams, Realm + 1);
    SetLength(Forks, Realm + 1);
  end;
  Inc(Forks[Creator]);
  Streams[Realm] := Streams[Creator].Fork(Forks[Creator]).Fork(1);
end;

procedure SetTest262Deterministic;
var Rs: TT262Randoms;
begin
  SetClockOverride(TGocciaHostEnvironment.DeterministicEpochNanoseconds / 1000000,
    TGocciaHostEnvironment.DeterministicMonotonicNanoseconds / 1000000);
  TimeZoneOverride := TGocciaHostEnvironment.DeterministicTimeZone;
  T262Randoms.Free;
  T262Randoms := nil;
  T262Randoms := TT262Randoms.Create;
  SetLength(T262Randoms.Streams, 1);
  SetLength(T262Randoms.Forks, 1);
  T262Randoms.Streams[0] := TGocciaSeededHostRandom.Create(TGocciaHostEnvironment.DeterministicSeed);
  Rs := T262Randoms;
  HostRandom := Rs.Next;
end;

function Test262PrintText: UnicodeString;
begin
  Result := PrintBuf;
end;

function ReadTest262Source(const Path: string; var Src: UnicodeString): Boolean;
var FS: TFileStream; Raw: RawByteString;
begin
  Result := False;
  try
    FS := TFileStream.Create(Path, fmOpenRead or fmShareDenyWrite);
    try
      SetLength(Raw, FS.Size);
      if FS.Size > 0 then FS.ReadBuffer(Raw[1], FS.Size);
    finally
      FS.Free;
    end;
  except
    on E: Exception do
    begin
      if IsEngineFault(E) then raise;
      Exit;
    end;
  end;
  if (Length(Raw) >= 3) and (Raw[1] = #$EF) and (Raw[2] = #$BB) and (Raw[3] = #$BF) then Delete(Raw, 1, 3);
  Src := UTF8DecodeKeepingNoncharacters(Raw);
  Result := True;
end;

{ Key along O's prototype chain as a data property; False at an accessor, a proxy or the end of the chain }
function DataProp(O: TValue; Key: PStr; var V: TValue): Boolean;
var P: PObj; S: Integer;
begin
  while IsObject(O) do
  begin
    P := ObjOf(O);
    if P^.OKind = okProxy then Exit(False);
    S := ShapeLookup(P^.Shape, Key);
    if S >= 0 then
    begin
      if (P^.Shape^.PFlags[S] and pfAccessor) <> 0 then Exit(False);
      V := GetSlot(P, S);
      Exit(True);
    end;
    O := ProtoOf(P);
  end;
  Result := False;
end;




{ GocciaHost.detachArrayBuffer(buffer): $262.detachArrayBuffer (25.1.3.5 DetachArrayBuffer) }
function Host_HTMLDDA(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
begin
  Res := VNull;
  Result := True;
end;

function Host_Detach(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
begin
  if Argc < 1 then Exit(DetachArrayBufferV(VUndefined));
  Result := DetachArrayBufferV(Args[1]);
  Res := VNull;
end;

{ GocciaHost.evalScript(source): $262.evalScript, a global Script run in this realm }
function Host_EvalScript(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var T: PFuncTemplate; Err: UnicodeString; Clo, S: TValue;
begin
  if Argc >= 1 then S := Args[1] else S := VUndefined;
  if not ToStringV(S, S) then Exit(False);
  { the Script's completion value is the result (ScriptEvaluation step 13) }
  if not ParseAndCompile(StrToUnicode(StrOf(S)), '<evalScript>', True, T, Err, 1) then
  begin
    if CompileFailKind = 1 then Exit(ThrowError(ParseErrorName, Err));
    Exit(ThrowError('SyntaxError', Err));
  end;
  if not MakeScriptClosure(T, Clo) then Exit(False);
  Result := CallValue(Clo, GlobalObj, nil, 0, Res);
end;

function Host_GC(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
begin
  { a full collection, as the allocator runs one: the nursery first }
  CollectMinor;
  CollectMajor;
  Res := VUndefined;
  Result := True;
end;

var
  { the $262.js harness file, which createRealm runs in each new realm }
  HostHarnessSrc: UnicodeString;

function Host_CreateRealm(Args: PValue; Argc: Integer; var Res: TValue): Boolean; forward;

{ GocciaHost.pumpAsyncWaits(): a timer tick of the host's event loop settles the Atomics.waitAsync waits that are due }
function Host_PumpAsyncWaits(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
begin
  PumpAsyncWaits;
  Res := VUndefined;
  Result := True;
end;

{ the hidden GocciaHost object $262.js reads, in the running realm }
procedure InstallHostHooks;
var H: TValue;
begin
  H := MkPtr(NewPlainObject(ObjectProto, 8));
  DefineOwn(GlobalObj, AtomOf('GocciaHost'), H, pfHidden);
  { H is re-read after each allocation: it may move }
  GetV(GlobalObj, AtomOf('GocciaHost'), H);
  DefineNative(H, 'evalScript', Host_EvalScript, 1);
  GetV(GlobalObj, AtomOf('GocciaHost'), H);
  DefineNative(H, 'gc', Host_GC, 0);
  GetV(GlobalObj, AtomOf('GocciaHost'), H);
  DefineNative(H, 'detachArrayBuffer', Host_Detach, 1);
  GetV(GlobalObj, AtomOf('GocciaHost'), H);
  DefineNative(H, 'createRealm', Host_CreateRealm, 0);
  GetV(GlobalObj, AtomOf('GocciaHost'), H);
  DefineNative(H, 'pumpAsyncWaits', Host_PumpAsyncWaits, 0);
  { B.3.6 $262.IsHTMLDDA: a callable object that returns null and emulates undefined }
  GetV(GlobalObj, AtomOf('GocciaHost'), H);
  DefineNative(H, 'IsHTMLDDA', Host_HTMLDDA, 0);
  GetV(GlobalObj, AtomOf('GocciaHost'), H);
  GetV(H, AtomOf('IsHTMLDDA'), H);
  PCell(PtrOf(H))^.Flags := PCell(PtrOf(H))^.Flags or cfEmulatesUndefined;
  ObjOf(H)^.OFlags := ObjOf(H)^.OFlags and not 32;   { not a constructor }  { $262.AbstractModuleSource: the realm's %AbstractModuleSource% }
  PushRoot(AbstractModuleSourceIntrinsic);
  GetV(GlobalObj, AtomOf('GocciaHost'), H);
  DefineOwn(H, AtomOf('AbstractModuleSource'), RootAt(TempTop - 1), pfHidden);
  PopRoots(1);
end;

{ GocciaHost.createRealm(): $262.createRealm, the $262 object of a new realm }
function Host_CreateRealm(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var R, Old: Integer; T: PFuncTemplate; Err: UnicodeString; Clo: TValue;
begin
  if HostHarnessSrc = '' then Exit(ThrowTypeError('createRealm needs the $262.js harness'));
  Old := CurRealm;
  if not CreateRealm(R) then Exit(False);
  if T262Randoms <> nil then T262Randoms.Created(Old, R);
  SwitchRealm(R);
  InstallHostHooks;
  Result := ParseAndCompile(HostHarnessSrc, '$262.js', False, T, Err, 1);
  if not Result then ThrowError('SyntaxError', Err)
  else Result := MakeScriptClosure(T, Clo) and CallValue(Clo, GlobalObj, nil, 0, Res) and
    GetV(GlobalObj, AtomOf('$262'), Res);
  SwitchRealm(Old);
end;

{ print(...): the arguments as strings, joined by spaces, a line in PrintText (doneprintHandle.js reports $DONE's
  outcome through it) }
function Host_Print(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var I: Integer; S: TValue; Line: UnicodeString;
begin
  Line := '';
  for I := 1 to Argc do
  begin
    if not ToStringV(Args[I], S) then Exit(False);
    if I > 1 then Line := Line + ' ';
    Line := Line + StrToUnicode(StrOf(S));
  end;
  PrintBuf := PrintBuf + Line + #10;
  Res := VUndefined;
  Result := True;
end;

procedure InitTest262Engine(CannotBlock: Boolean);
var H, Agent: TValue;
begin
  GCVerify := GetEnvironmentVariable('GOCCIA_GC_VERIFY') <> '';
  if GetEnvironmentVariable('GOCCIA_NURSERY_KB') <> '' then EngineInit(StrToInt(GetEnvironmentVariable('GOCCIA_NURSERY_KB')) * 1024)
  else EngineInit;
  AgentCannotSuspend := CannotBlock;
  InitAgentHost;
  InstallHostHooks;
  if GetV(GlobalObj, AtomOf('$262agent'), Agent) then
  begin
    GetV(GlobalObj, AtomOf('GocciaHost'), H);
    DefineOwn(H, AtomOf('agent'), Agent, pfDefault);
    DeleteProperty(ObjOf(GlobalObj), AtomOf('$262agent'));
  end;
  PrintBuf := '';
  DefineNative(GlobalObj, 'print', Host_Print, 1);
  { print is the host's, not a global binding a test declares: hidden, as console.log was before }
  GetV(GlobalObj, AtomOf('print'), H);
  DeleteProperty(ObjOf(GlobalObj), AtomOf('print'));
  DefineOwn(GlobalObj, AtomOf('print'), H, pfHidden);
end;

procedure DoneTest262Engine;
begin
  LimitsDisarm;
  EngineDone;
  if T262Randoms <> nil then
  begin
    HostRandom := nil;
    T262Randoms.Free;
    T262Randoms := nil;
  end;
end;

type
  { the run's outcome, raised to leave RunTest262Case from any depth }
  ET262Outcome = class(Exception)
    O: TGocciaT262Outcome;
  end;

procedure Done(const Kind, Name, Msg, F: UnicodeString; const Site: UnicodeString = '');
var E: ET262Outcome;
begin
  E := ET262Outcome.Create('test262 outcome');
  E.O.Kind := Kind;
  E.O.Name := Name;
  E.O.Message := Copy(Msg, 1, 400);
  E.O.FileName := F;
  E.O.Site := Site;
  raise E;
end;

{ the escaped value E's constructor name (data properties only) and message }
procedure Thrown(E: TValue; const Kind: UnicodeString);
var C, N, M, S: TValue; Name, Msg, Site: UnicodeString;
begin
  Name := ''; Msg := ''; Site := '';
  if not ErrorStackProvenance(E, Site) then Site := '';
  if IsObject(E) then
  begin
    if DataProp(E, AtomConstructor, C) and IsObject(C) then
    begin
      { The engine keeps a function's own name outside its shape (read through GetV, no getter involved) }
      if IsCallable(C) and (ShapeLookup(ObjOf(C)^.Shape, AtomName) < 0) then
      begin
        if GetV(C, AtomName, N) and IsString(N) then Name := StrToUnicode(StrOf(N));
        Pending := VUndefined;
      end
      else if DataProp(C, AtomName, N) and IsString(N) then Name := StrToUnicode(StrOf(N));
    end;
    if DataProp(E, AtomMessage, M) and IsString(M) then Msg := StrToUnicode(StrOf(M));
  end
  else
  begin
    Pending := VUndefined;
    if ToStringV(E, S) and IsString(S) then Msg := StrToUnicode(StrOf(S));
  end;
  Done(Kind, Name, Msg, '', Site);
end;

procedure RunCase(const Harness: array of string; const TestFile: string; Strict, IsModule: Boolean);
var I, Phase: Integer; Src, Err: UnicodeString; T: PFuncTemplate; H: TValue;
begin
  for I := 0 to High(Harness) do
  begin
    { $262.js is the host's own, compiled in (Goccia.Test262Host.inc); the others are the suite's files }
    if ExtractFileName(Harness[I]) = '$262.js' then
    begin
      Src := Test262HostSrc;
      HostHarnessSrc := Src;
    end
    else if not ReadTest262Source(Harness[I], Src) then
      Done('harness', '', 'cannot read', UnicodeString(Harness[I]));
    if not ParseAndCompile(Src, UnicodeString(Harness[I]), False, T, Err, 1) then
      Done('harness', '', Err, UnicodeString(Harness[I]));
    if not (RunScript(T) and RunMicrotasks) then
    begin
      Err := '';
      if IsObject(Pending) and DataProp(Pending, AtomMessage, H) and IsString(H) then Err := StrToUnicode(StrOf(H));
      Done('harness', '', Err, UnicodeString(Harness[I]));
    end;
  end;
  if IsModule then
  begin
    { a module test: parse and link errors are early (resolution) errors, the rest are evaluation errors }
    if not RunModuleFile(UnicodeString(TestFile), Phase) then
    begin
      if Phase = 0 then Thrown(Pending, 'parse');
      Thrown(Pending, 'throw');
    end;
    if not (RunMicrotasks and RunHostLoop) then Thrown(Pending, 'throw');
    Done('ok', '', '', '');
  end;
  if not ReadTest262Source(TestFile, Src) then Done('harness', '', 'cannot read test', UnicodeString(TestFile));
  if Strict then Src := '"use strict";'#10 + Src;
  if not ParseAndCompile(Src, UnicodeString(TestFile), False, T, Err, 1) then
  begin
    if CompileFailKind = 1 then Done('parse', ParseErrorName, Err, '');
    Done('compile', '', Err, '');
  end;
  if not (RunScript(T) and RunMicrotasks and RunHostLoop) then Thrown(Pending, 'throw');
  Done('ok', '', '', '');
end;

function RunTest262Case(const Harness: array of string; const TestFile: string; Strict, IsModule: Boolean): TGocciaT262Outcome;
begin
  Result := Default(TGocciaT262Outcome);
  { a Pascal exception in the engine (an engine bug) grades the run as a failure, as upstream's runner does; only a
    signal the runtime cannot turn into an exception kills the process (wrapper-infra) }
  try
    RunCase(Harness, TestFile, Strict, IsModule);
  except
    on E: ET262Outcome do Result := E.O;
    { fault-guard: the crash outcome names the exception's class, and on POSIX each case runs in a forked worker of its
      own (docs/test262.md) }
    on E: Exception do
    begin
      Result.Kind := 'crash';
      Result.Name := UnicodeString(E.ClassName);
      Result.Message := UnicodeString(E.Message);
    end;
  end;
end;

{ the managed threadvars of this unit, released when the thread ends (Goccia.ThreadCleanupRegistry): FPC does not
  finalize a threadvar, and every engine thread leaves its strings, arrays and objects behind otherwise }
procedure GocciaTest262ThreadvarCleanup;
begin
  PrintBuf := '';
  T262Randoms.Free;
  T262Randoms := nil;
end;

initialization
  RegisterThreadvarCleanup(@GocciaTest262ThreadvarCleanup);
end.
