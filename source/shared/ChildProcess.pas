unit ChildProcess;

{$I Shared.inc}

{ FPC's TProcess on Unix forks and execs without closing the descriptors the
  child inherits, and the RTL opens files and TProcess creates pipes without
  close-on-exec. Every descriptor the parent has open at the fork, in any of
  its threads, stays open in the child until it exits:

  - a TFileStream's flock goes with it, so the parent's next exclusive open of
    that file fails with EAGAIN ("Try again") while the child runs;
  - the parent's end of another child's pipe goes with it, so a child reading
    its standard input sees end of file only once every later child has
    exited too.

  TChildProcess closes every descriptor above standard error in the child,
  after TProcess has set up the child's standard input, output and error and
  before it execs the program. Between the fork and that close the child still
  holds them, so a lock can refuse the parent for that moment, but no longer
  for the child's lifetime. Using this unit also makes it the class that
  Process.RunCommand and its variants create (DefaultTProcess), so every
  process a program starts through them gets the same treatment. Windows
  needs nothing: TProcess there makes only the child's ends of its pipes
  inheritable, and the RTL opens files non-inheritable. }

interface

uses
  Classes,
  Process;

type
  TChildProcess = class(TProcess)
  {$IFDEF UNIX}
  private
    procedure CloseInheritedDescriptors(ASender: TObject);
  {$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

{$IFDEF UNIX}
uses
  {$IFDEF LINUX}Syscall,{$ENDIF}
  BaseUnix;

const
  FIRST_INHERITED_DESCRIPTOR = 3;
  {$IFDEF LINUX}
  // close_range(2), Linux 5.9 and later; the same number on every
  // architecture, since it was added after the syscall tables were unified.
  SYSCALL_NR_CLOSE_RANGE = 436;
  {$ENDIF}
  // How far the fallback closes when the descriptor limit is unlimited.
  UNLIMITED_DESCRIPTOR_FALLBACK = 65536;
{$ENDIF}

constructor TChildProcess.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  {$IFDEF UNIX}
  OnForkEvent := CloseInheritedDescriptors;
  {$ENDIF}
end;

{$IFDEF UNIX}
{ Runs in the forked child, so it makes system calls only: no allocation and
  no lock another thread of the parent may have held at the fork. }
procedure TChildProcess.CloseInheritedDescriptors(ASender: TObject);
var
  Limit: TRLimit;
  Last, Descriptor: cint;
begin
  {$IFDEF LINUX}
  if Do_SysCall(SYSCALL_NR_CLOSE_RANGE, TSysParam(FIRST_INHERITED_DESCRIPTOR),
       TSysParam(High(Cardinal)), 0) = 0 then
    Exit;
  {$ENDIF}
  // No close_range (Linux before 5.9, or another Unix): a descriptor is below
  // the soft limit unless the limit was lowered after it was opened. An
  // unlimited soft limit (RLIM_INFINITY, all ones on Linux and 2^63 - 1 on
  // Darwin and the BSDs) is at least High(cint).
  Last := UNLIMITED_DESCRIPTOR_FALLBACK - 1;
  if (FpGetRLimit(RLIMIT_NOFILE, @Limit) = 0) and
     (Limit.rlim_cur < rlim_t(High(cint))) then
    Last := cint(Limit.rlim_cur) - 1;
  for Descriptor := FIRST_INHERITED_DESCRIPTOR to Last do
    FpClose(Descriptor);
end;
{$ENDIF}

initialization
  DefaultTProcess := TChildProcess;

end.
