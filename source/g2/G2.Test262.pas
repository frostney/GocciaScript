unit G2.Test262;
{ test262's host on G2, under GocciaScript's grading contract 4 (docs/test262.md): the hidden G2Host object that
  $262.js (source/g2/prelude/test262host.js, compiled in) turns into $262 (evalScript, gc, detachArrayBuffer, createRealm, IsHTMLDDA,
  AbstractModuleSource, the agent API), and one test run in this process. Each harness file runs as its own
  non-strict global Script in one realm, then the test file: a Script, prefixed with '"use strict";' and a newline
  in a strict run, or a module. GocciaTest262Runner (G2) forks a process per run and calls RunTest262Case in it;
  `g2 --test262` runs one and prints its outcome. }
{$mode delphiunicode}{$H+}
{$I G2.Checks.inc}

interface

type
  { how a run ended. Kind: ok; parse (the parser or the module linker rejected the test before evaluation: an
    early error, Name its constructor's name); throw (a value escaped evaluation, Name its constructor's name read
    from data properties only, so no getter runs); harness (a harness file could not be read, compiled or run:
    FileName names it); compile (G2's compiler refused the parsed test); crash (a Pascal exception in the engine,
    Name its class) }
  TG2T262Outcome = record
    Kind, Name, Message, FileName: UnicodeString;
    { a thrown error's provenance, "    at source:line:column" where it was made, or '' }
    Site: UnicodeString;
  end;

{ the hidden G2Host object of the running realm }
procedure InstallHostHooks;
{ the engine of a test262 run in this process: G2_NURSERY_KB and G2_GC_VERIFY (scripts/g2/gcstress.sh's
  settings), the agent host (CannotBlock: the main agent may not suspend, test262's CanBlockIsFalse), G2Host with
  the agent API moved into it (so $262 stays the only harness global) and print, which records into PrintText }
procedure InitTest262Engine(CannotBlock: Boolean);
{ the engine InitTest262Engine made, ended: a thread that runs another case after it makes a new one }
procedure DoneTest262Engine;
{ one run on the engine InitTest262Engine made: the harness files in order, then TestFile }
function RunTest262Case(const Harness: array of string; const TestFile: string; Strict, IsModule: Boolean): TG2T262Outcome;
{ what the run's print calls printed, a line each }
function Test262PrintText: UnicodeString;
{ a UTF-8 source file (a BOM dropped); False when it cannot be read }
function ReadTest262Source(const Path: string; var Src: UnicodeString): Boolean;

implementation

uses
  Classes, SysUtils, G2.Core, G2.Bytecode, G2.Compiler, G2.VM, G2.Eval, G2.Promise, G2.Atomics, G2.Agent,
  G2.Engine, G2.TypedArrays, G2.Modules, G2.Builtins, Goccia.ThreadCleanupRegistry;

{$I G2.Test262Host.inc}

threadvar
  PrintBuf: UnicodeString;   { (per thread: a runner without fork runs cases on several threads) }

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
    Exit;
  end;
  if (Length(Raw) >= 3) and (Raw[1] = #$EF) and (Raw[2] = #$BB) and (Raw[3] = #$BF) then Delete(Raw, 1, 3);
  Src := UTF8Decode(Raw);
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




{ G2Host.detachArrayBuffer(buffer): $262.detachArrayBuffer (25.1.3.5 DetachArrayBuffer) }
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

{ G2Host.evalScript(source): $262.evalScript, a global Script run in this realm }
function Host_EvalScript(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var T: PFuncTemplate; Err: UnicodeString; Clo, S: TValue;
begin
  if Argc >= 1 then S := Args[1] else S := VUndefined;
  if not ToStringV(S, S) then Exit(False);
  { the Script's completion value is the result (ScriptEvaluation step 13) }
  if not ParseAndCompile(StrToUnicode(StrOf(S)), '<evalScript>', True, T, Err, 1) then
  begin
    if CompileFailKind = 1 then Exit(ThrowError(ParseErrorName, Err));
    Exit(ThrowError('SyntaxError', 'G2 compile: ' + Err));
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

{ G2Host.pumpAsyncWaits(): a timer tick of the host's event loop settles the Atomics.waitAsync waits that are due }
function Host_PumpAsyncWaits(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
begin
  PumpAsyncWaits;
  Res := VUndefined;
  Result := True;
end;

{ the hidden G2Host object $262.js reads, in the running realm }
procedure InstallHostHooks;
var H: TValue;
begin
  H := MkPtr(NewPlainObject(ObjectProto, 8));
  DefineOwn(GlobalObj, AtomOf('G2Host'), H, pfHidden);
  { H is re-read after each allocation: it may move }
  GetV(GlobalObj, AtomOf('G2Host'), H);
  DefineNative(H, 'evalScript', Host_EvalScript, 1);
  GetV(GlobalObj, AtomOf('G2Host'), H);
  DefineNative(H, 'gc', Host_GC, 0);
  GetV(GlobalObj, AtomOf('G2Host'), H);
  DefineNative(H, 'detachArrayBuffer', Host_Detach, 1);
  GetV(GlobalObj, AtomOf('G2Host'), H);
  DefineNative(H, 'createRealm', Host_CreateRealm, 0);
  GetV(GlobalObj, AtomOf('G2Host'), H);
  DefineNative(H, 'pumpAsyncWaits', Host_PumpAsyncWaits, 0);
  { B.3.6 $262.IsHTMLDDA: a callable object that returns null and emulates undefined }
  GetV(GlobalObj, AtomOf('G2Host'), H);
  DefineNative(H, 'IsHTMLDDA', Host_HTMLDDA, 0);
  GetV(GlobalObj, AtomOf('G2Host'), H);
  GetV(H, AtomOf('IsHTMLDDA'), H);
  PCell(PtrOf(H))^.Flags := PCell(PtrOf(H))^.Flags or cfEmulatesUndefined;
  ObjOf(H)^.OFlags := ObjOf(H)^.OFlags and not 32;   { not a constructor }  { $262.AbstractModuleSource: the realm's %AbstractModuleSource% }
  PushRoot(AbstractModuleSourceIntrinsic);
  GetV(GlobalObj, AtomOf('G2Host'), H);
  DefineOwn(H, AtomOf('AbstractModuleSource'), RootAt(TempTop - 1), pfHidden);
  PopRoots(1);
end;

{ G2Host.createRealm(): $262.createRealm, the $262 object of a new realm }
function Host_CreateRealm(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var R, Old: Integer; T: PFuncTemplate; Err: UnicodeString; Clo: TValue;
begin
  if HostHarnessSrc = '' then Exit(ThrowTypeError('createRealm needs the $262.js harness'));
  if not CreateRealm(R) then Exit(False);
  Old := CurRealm;
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
  GCVerify := GetEnvironmentVariable('G2_GC_VERIFY') <> '';
  if GetEnvironmentVariable('G2_NURSERY_KB') <> '' then EngineInit(StrToInt(GetEnvironmentVariable('G2_NURSERY_KB')) * 1024)
  else EngineInit;
  AgentCannotSuspend := CannotBlock;
  InitAgentHost;
  InstallHostHooks;
  if GetV(GlobalObj, AtomOf('$262agent'), Agent) then
  begin
    GetV(GlobalObj, AtomOf('G2Host'), H);
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
end;

type
  { the run's outcome, raised to leave RunTest262Case from any depth }
  ET262Outcome = class(Exception)
    O: TG2T262Outcome;
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
      { G2 keeps a function's own name outside its shape (read through GetV, no getter involved) }
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
    { $262.js is the host's own, compiled in (G2.Test262Host.inc); the others are the suite's files }
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

function RunTest262Case(const Harness: array of string; const TestFile: string; Strict, IsModule: Boolean): TG2T262Outcome;
begin
  Result := Default(TG2T262Outcome);
  { a Pascal exception in the engine (an engine bug) grades the run as a failure, as upstream's runner does; only a
    signal the runtime cannot turn into an exception kills the process (wrapper-infra) }
  try
    RunCase(Harness, TestFile, Strict, IsModule);
  except
    on E: ET262Outcome do Result := E.O;
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
procedure G2Test262ThreadvarCleanup;
begin
  PrintBuf := '';
end;

initialization
  RegisterThreadvarCleanup(@G2Test262ThreadvarCleanup);
end.
