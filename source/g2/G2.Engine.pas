unit G2.Engine;
{ One engine per thread.

  Every piece of engine state (heap, atoms, shapes, stacks, builtins, the job
  queue) is a threadvar, so a thread that calls EngineInit has an engine of
  its own and shares nothing with the others: values never cross threads.
  With FPC's native ELF TLS (the -dtls_threadvars toolchain) a threadvar costs
  about what a global does: single-thread runs execute 0-0.7% more
  instructions than with globals. }
{$mode delphiunicode}{$H+}

interface

uses
  SysUtils, Classes, Math, G2.Core, G2.Bytecode, G2.VM;

{ builds this thread's engine: heap, VM, builtins and the JS preludes }
procedure EngineInit(NurseryBytes: SizeInt = 8 * 1024 * 1024);
{ frees this thread's engine; values, templates and closures from it are invalid afterwards }
procedure EngineDone;
{ parses, compiles and runs Source as a script, then the microtasks it queued; Err gets an uncaught error's
  message }
function EngineRunScript(const Source, FileName: UnicodeString; var Err: UnicodeString): Boolean;

implementation

uses
  G2.Builtins, G2.Builtins2, G2.RegExpV, G2.Eval, G2.Builtins3, G2.Promise, G2.Builtins4, G2.Atomics, G2.ObjModel, G2.ArrayProto, G2.IterHelpers, G2.TypedArrays, G2.Disposal, G2.Modules, G2.Builtins5;

procedure EngineInit(NurseryBytes: SizeInt);
begin
  { JS arithmetic produces NaN and Infinity instead of trapping; the masks are per thread, and a new thread
    starts with the exceptions enabled }
  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);
  HeapInit(NurseryBytes);
  VMInit;
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
end;

procedure EngineDone;
begin
  VMDone;
  HeapDone;
  FreeAllTemplates;
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
