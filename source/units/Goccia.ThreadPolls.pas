{ One thread-local word that says whether any of the checks a value allocation
  and a bytecode call make is armed on the calling thread.

  Each of those checks keeps its own state behind a thread variable, and every
  reference to a thread variable is a lookup (a pthread_getspecific call on
  POSIX targets). A script that runs without a timeout, without an instruction
  limit and without the allocation profiler made four of them per allocated
  value to learn that it had nothing to do. The flags below say whether each
  of the three is armed, and a hot path reads the word once:

    if GThreadPolls.Any <> 0 then
      <the checks, out of line>;

  The first two flags are mirrors, never the source of truth. Each must change
  in the same procedure that changes the state it mirrors:

    TimeoutArmed            Goccia.Timeout: a scope with a deadline is active
                            (the soonest deadline is not 0).
    InstructionLimitActive  Goccia.InstructionLimit: the state's Active flag.

  The third has no other owner:

    ProfilingAllocations    Set by the VM around a native entry while the
                            function profiler is on, and restored on the way
                            out.

  The thread variable is declared in the interface on purpose: other units
  reference it directly, where an accessor function would add a call to the
  lookup (FPC does not inline TGarbageCollector.Instance or CurrentRealm into
  other units). The address of the record is
  stable for the life of its thread, so a VM may keep it between its
  outermost entry and the return of that entry. }

unit Goccia.ThreadPolls;

{$I Goccia.inc}

interface

type
  TGocciaThreadPolls = packed record
    case Boolean of
      False: (
        TimeoutArmed: Boolean;
        InstructionLimitActive: Boolean;
        ProfilingAllocations: Boolean;
        Reserved: Boolean);
      True: (
        Any: UInt32);
  end;
  PGocciaThreadPolls = ^TGocciaThreadPolls;

threadvar
  GThreadPolls: TGocciaThreadPolls;

implementation

end.
