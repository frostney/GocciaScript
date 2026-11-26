unit ChildProcess;

{$I Shared.inc}

{ FPC's TProcess on Unix forks and execs without closing the descriptors the
  child inherits, and the RTL opens files and TProcess creates pipes without
  close-on-exec (FD_CLOEXEC). Every such descriptor the parent has open at the
  fork, in any of its threads, stays open in the child until it exits;
  docs/contributing/tooling.md ("Starting a Child Process on Unix") describes
  what that breaks.

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
    FDescriptorsInUse: LongInt;
    procedure CloseInheritedDescriptors(ASender: TObject);
  protected
    { Closes every descriptor from AFirst up in one call and returns True, or
      returns False where the system has no such call, and the child then
      closes descriptors one by one. It is close_range(2) on Linux 5.9 and
      later on the architectures whose system call number is known here. It
      runs in the forked child, so an override makes system calls only. }
    function CloseDescriptorsFrom(const AFirst: LongInt): Boolean; virtual;
  {$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    {$IFDEF UNIX}
    procedure Execute; override;
    {$ENDIF}
  end;

implementation

{$IF DEFINED(LINUX) AND (DEFINED(CPUX86_64) OR DEFINED(CPUI386) OR DEFINED(CPUAARCH64))}
  {$DEFINE GOCCIA_CHILD_PROCESS_CLOSE_RANGE}
{$IFEND}

{$IFDEF UNIX}
uses
  BaseUnix,
  {$IFDEF GOCCIA_CHILD_PROCESS_CLOSE_RANGE}
  Syscall,
  {$ENDIF}
  SysUtils;

const
  FIRST_INHERITED_DESCRIPTOR = 3;
  // How far the one-by-one close goes, at least, when the descriptor limit is
  // unlimited or unknown, or the parent's descriptors could not be listed.
  UNBOUNDED_DESCRIPTOR_FALLBACK = 65536;
  // FDescriptorsInUse when the listing failed.
  DESCRIPTORS_UNLISTED = -1;
  { The directory naming each open descriptor of the reading process. Linux's
    /proc/self/fd and Darwin's /dev/fd name every one. FreeBSD's /dev/fd names
    only 0 to 2 unless fdescfs is mounted, and other systems' may name fewer,
    so there the listing only raises the bound the descriptor limit sets. }
  {$IFDEF LINUX}
  OPEN_DESCRIPTOR_DIRECTORY = '/proc/self/fd';
  {$ELSE}
  OPEN_DESCRIPTOR_DIRECTORY = '/dev/fd';
  {$ENDIF}

{$IFDEF GOCCIA_CHILD_PROCESS_CLOSE_RANGE}
const
  // close_range(2). x86-64, i386 and AArch64 share the number; other ABIs,
  // such as MIPS, offset theirs and take the one-by-one close.
  SYSCALL_NR_CLOSE_RANGE = 436;
{$ENDIF}

{ One above the highest descriptor the process has open, or
  DESCRIPTORS_UNLISTED when its descriptors cannot be listed. The entries are
  read as names only: SysUtils.FindFirst would also look up each entry's
  metadata and skip one whose lookup fails. }
function DescriptorsInUseBound: LongInt;
var
  Directory: PDir;
  Entry: PDirent;
  Descriptor: LongInt;
begin
  Directory := FpOpendir(OPEN_DESCRIPTOR_DIRECTORY);
  if Directory = nil then
    Exit(DESCRIPTORS_UNLISTED);
  Result := 0;
  try
    Entry := FpReaddir(Directory^);
    while Entry <> nil do
    begin
      Descriptor := StrToIntDef(string(PAnsiChar(@Entry^.d_name[0])), -1);
      if Descriptor >= Result then
        Result := Descriptor + 1;
      Entry := FpReaddir(Directory^);
    end;
  finally
    FpClosedir(Directory^);
  end;
end;
{$ENDIF}

constructor TChildProcess.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  {$IFDEF UNIX}
  OnForkEvent := CloseInheritedDescriptors;
  {$ENDIF}
end;

{$IFDEF UNIX}
{ A descriptor opened before the soft descriptor limit was lowered can lie
  above the limit, so the one-by-one close goes up to the highest descriptor
  the parent has open as well. It is listed here, before the fork, since the
  child may not allocate. A descriptor another thread opens after the listing
  is below the limit in force when it was opened, which the child reads. }
procedure TChildProcess.Execute;
begin
  FDescriptorsInUse := DescriptorsInUseBound;
  inherited Execute;
end;

function TChildProcess.CloseDescriptorsFrom(const AFirst: LongInt): Boolean;
begin
  {$IFDEF GOCCIA_CHILD_PROCESS_CLOSE_RANGE}
  Result := Do_SysCall(SYSCALL_NR_CLOSE_RANGE, TSysParam(AFirst),
    TSysParam(High(Cardinal)), 0) = 0;
  {$ELSE}
  Result := False;
  {$ENDIF}
end;

{ Runs in the forked child, so it makes system calls only: no allocation and
  no lock another thread of the parent may have held at the fork. }
procedure TChildProcess.CloseInheritedDescriptors(ASender: TObject);
var
  Limit: TRLimit;
  Last, Descriptor: cint;
begin
  if CloseDescriptorsFrom(FIRST_INHERITED_DESCRIPTOR) then
    Exit;
  // An unlimited soft limit (RLIM_INFINITY, all ones on Linux and 2^63 - 1 on
  // Darwin and the BSDs) is at least High(cint).
  if (FpGetRLimit(RLIMIT_NOFILE, @Limit) = 0) and
     (QWord(Limit.rlim_cur) < QWord(High(cint))) then
    Last := cint(Limit.rlim_cur) - 1
  else
    Last := UNBOUNDED_DESCRIPTOR_FALLBACK - 1;
  if (FDescriptorsInUse = DESCRIPTORS_UNLISTED) and
     (Last < UNBOUNDED_DESCRIPTOR_FALLBACK - 1) then
    Last := UNBOUNDED_DESCRIPTOR_FALLBACK - 1;
  if FDescriptorsInUse - 1 > Last then
    Last := FDescriptorsInUse - 1;
  for Descriptor := FIRST_INHERITED_DESCRIPTOR to Last do
    FpClose(Descriptor);
end;
{$ENDIF}

initialization
  DefaultTProcess := TChildProcess;

end.
