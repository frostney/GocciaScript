unit Goccia.FaultInjection;

{ Real engine faults on request, for the tests of how every application stops
  on one (scripts/test-engine-faults.ts, docs/adr/0196-every-application-stops-on-an-engine-fault.md).

  Only a build with -dGOCCIA_FAULT_INJECTION has any of this: the CI's fault
  build sets it through build.pas's variant mechanism,

      GOCCIA_VARIANT=FaultInjection GOCCIA_FPC_FLAGS=-dGOCCIA_FAULT_INJECTION ./build.pas runner ...

  which writes build/GocciaRunnerFaultInjection and so on. No shipped target
  sets the define, so a shipped binary has neither the guest global nor the
  environment variable below, and the fault job checks that the plain binaries
  carry neither name.

  Two ways to ask for a fault:

    __gocciaInjectFault(kind)    a global every realm of an injection build
                                 has: faults inside the guest code calling it,
                                 on the thread running that code
    GOCCIA_FAULT_INJECTION=kind@site
                                 faults when the process reaches the named
                                 site: "startup", before an application does
                                 any work, on its main thread; "compile", when
                                 the bundler's engine compiles an entry; or
                                 "compile-entry", inside the handler that makes
                                 a failure to compile a script's entry its
                                 SyntaxError (Goccia.Eval)

  The kinds, each a real fault raised the way the engine's own bugs raise it:

    access-violation   a write through a nil pointer: a hardware fault the
                       platform delivers (SIGSEGV, or an access-violation SEH
                       exception on Windows) and FPC turns into EAccessViolation
    assertion          a failed Assert, compiled in here whatever the build's
                       own assertion setting: EAssertionFailed
    invalid-pointer    run-time error 204 through the RTL's internal-error path
                       (RaiseRuntimeError), as the heap manager reports a
                       pointer it does not own and as the engine's own check
                       does when a native's root bookkeeping is off
                       (Goccia.Core): EInvalidPointer
    stack-overflow     run-time error 202 the same way, the RTL's report of a
                       native stack that ran out: EStackOverflow. FPC 3.3.1 on
                       x86-64 Linux has no stack check that raises it (a real
                       overflow ends the process by SIGSEGV before any handler
                       runs), so this kind raises it the way the RTL's check
                       does where it has one }

{$I Goccia.inc}
{$C+}

interface

{$IFDEF GOCCIA_FAULT_INJECTION}
const
  FAULT_INJECTION_VARIABLE = 'GOCCIA_FAULT_INJECTION';

{ Raises the fault AKind names; an unknown kind is an ordinary exception, so a
  test that misspells one fails as an ordinary error instead of passing. }
procedure CauseInjectedFault(const AKind: string);

{ Causes the fault GOCCIA_FAULT_INJECTION asks for when it names ASite;
  otherwise returns. }
procedure InjectFaultAt(const ASite: string);
{$ENDIF}

implementation

{$IFDEF GOCCIA_FAULT_INJECTION}
uses
  SysUtils,

  Goccia.EngineFault;

var
  { a nil pointer the optimizer cannot see through }
  GNowhere: PLongInt = nil;

procedure CauseInjectedFault(const AKind: string);
begin
  if AKind = 'access-violation' then
    GNowhere^ := 1
  else if AKind = 'assertion' then
    Assert(False, 'injected assertion')
  else if AKind = 'invalid-pointer' then
    RaiseRuntimeError(204)
  else if AKind = 'stack-overflow' then
    RaiseRuntimeError(202)
  else
    raise Exception.CreateFmt('unknown fault kind: %s', [AKind]);
end;

procedure InjectFaultAt(const ASite: string);
var
  Request: string;
  At: Integer;
begin
  Request := GetEnvironmentVariable(FAULT_INJECTION_VARIABLE);
  At := Pos('@', Request);
  if (At > 0) and (Copy(Request, At + 1, Length(Request)) = ASite) then
    CauseInjectedFault(Copy(Request, 1, At - 1));
end;
{$ENDIF}

end.
