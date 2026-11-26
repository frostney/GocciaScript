unit Goccia.Engine;
{ One engine per thread.

  Every piece of engine state (heap, atoms, shapes, stacks, builtins, the job
  queue) is a threadvar, so a thread that calls EngineInit has an engine of
  its own and shares nothing with the others: values never cross threads.
  With FPC's native ELF TLS (the -dtls_threadvars toolchain) a threadvar costs
  about what a global does: single-thread runs execute 0-0.7% more
  instructions than with globals. }
{$mode delphiunicode}{$H+}
{$I Goccia.Checks.inc}
{$MINFPCONSTPREC 64}

interface

uses
  SysUtils, Classes, Math, Goccia.Core, Goccia.Instructions, Goccia.VM;

var
  { the nursery an engine gets when its host names none (TGocciaEngine, EngineInit without one): a host's GC stress
    setting (goccia --test with GOCCIA_NURSERY_KB) }
  EngineNurseryBytes: SizeInt = 8 * 1024 * 1024;
{ builds this thread's engine: heap, VM, builtins and the JS preludes; NurseryBytes 0 EngineNurseryBytes }
procedure EngineInit(NurseryBytes: SizeInt = 0);
{ a realm's intrinsics and global object (the builtins' Init procedures, after HeapInit and VMInit) }
procedure InitIntrinsics;
{ frees this thread's engine; values, templates and closures from it are invalid afterwards }
procedure EngineDone;
{ parses, compiles and runs Source as a script, then the microtasks it queued; Err gets an uncaught error's
  message }
function EngineRunScript(const Source, FileName: UnicodeString; var Err: UnicodeString): Boolean;

implementation

uses
  Goccia.Lazy, Goccia.Builtins, Goccia.Builtins2, Goccia.RegExpV, Goccia.Eval, Goccia.Builtins3, Goccia.Promise, Goccia.Builtins4, Goccia.Atomics, Goccia.ObjModel, Goccia.ArrayProto, Goccia.IterHelpers, Goccia.TypedArrays, Goccia.Disposal, Goccia.Modules, Goccia.Builtins5, Goccia.Temporal, Goccia.JSON, Goccia.Date, Goccia.Intl, Goccia.ShadowRealm, Goccia.Weak, Goccia.NativeLimits;

procedure EngineInit(NurseryBytes: SizeInt);
begin
  { the shared parsers' nesting guard refuses with a plain exception on this thread, which the engine throws }
  NativeErrorsPlain := True;
  { JS arithmetic produces NaN and Infinity instead of trapping; the masks are per thread, and a new thread
    starts with the exceptions enabled }
  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);
  if NurseryBytes <= 0 then NurseryBytes := EngineNurseryBytes;
  HeapInit(NurseryBytes);
  VMInit;
  InitIntrinsics;
  { performance's time origin: the system's clocks (a host that mocks or provides them takes it again) }
  HostClock := nil;
  TimeOriginInit;
end;

procedure InitIntrinsics;
begin
  IntrinsicsInit := True;
  InitBuiltins;
  InitBuiltins2;
  InitRegExp;
  InitEval;
  InitPromise;
  InitBuiltins3;
  InitBuiltins4;
  InitAtomics;
  InitObjModel;
  InitArrayProto;
  InitIterHelpers;
  InitTypedArrays;
  InitDisposal;
  InitModules;
  InitBuiltins5;
  InitTemporal;
  InitJSON;
  InitDate;
  InitIntl;
  InitShadowRealm;
  RecordTypedArrayGuard;
  IntrinsicsInit := False;
  RealmInitProc := @InitIntrinsics;
end;

procedure EngineDone;
begin
  WeakDone;
  ModulesDone;
  VMDone;
  HeapDone;
  FreeAllTemplates;
  FreeLazySources;
  RegExpDone;
end;

function EngineRunScript(const Source, FileName: UnicodeString; var Err: UnicodeString): Boolean;
var T: PFuncTemplate; S: TValue;
begin
  Err := '';
  if not ParseAndCompile(Source, FileName, False, T, Err, 1) then Exit(False);
  Result := RunScript(T) and RunMicrotasks and RunHostLoop;
  if not Result then
  begin
    if ToStringV(Pending, S) and IsString(S) then Err := StrToUnicode(StrOf(S)) else Err := 'uncaught exception';
    Pending := VUndefined;
  end;
end;

end.
