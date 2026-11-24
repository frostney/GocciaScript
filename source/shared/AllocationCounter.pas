unit AllocationCounter;

{$I Shared.inc}

// Counts heap allocations so a unit test can hold a code path to the
// allocations it should make. Allocation churn costs little in an ordinary
// build and a great deal under an instrumented heap (heaptrc, Valgrind), where
// every block carries bookkeeping; a count catches it in an ordinary test run.
//
// Every thread's allocations are counted: count only while no other thread
// allocates.

interface

procedure StartCountingAllocations;

// Stops counting and returns the number of blocks allocated or reallocated
// since StartCountingAllocations.
function StopCountingAllocations: Int64;

// The bytes the blocks of the last count asked for: the size of each GetMem
// and AllocMem and the new size of each ReAllocMem. A string grown by one
// append at a time asks for its whole length again on every append, so these
// grow with the square of its length while the count of blocks grows only
// with the number of appends.
function CountedAllocationBytes: Int64;

implementation

uses
  SysUtils;

var
  PreviousManager: TMemoryManager;
  AllocationCount: Int64;
  AllocationBytes: Int64;
  Counting: Boolean;

function CountingGetMem(ASize: PtrUInt): Pointer;
begin
  Inc(AllocationCount);
  Inc(AllocationBytes, ASize);
  Result := PreviousManager.GetMem(ASize);
end;

function CountingAllocMem(ASize: PtrUInt): Pointer;
begin
  Inc(AllocationCount);
  Inc(AllocationBytes, ASize);
  Result := PreviousManager.AllocMem(ASize);
end;

function CountingReAllocMem(var APointer: Pointer; ASize: PtrUInt): Pointer;
begin
  Inc(AllocationCount);
  Inc(AllocationBytes, ASize);
  Result := PreviousManager.ReAllocMem(APointer, ASize);
end;

procedure StartCountingAllocations;
var
  CountingManager: TMemoryManager;
begin
  if Counting then
    raise Exception.Create('StartCountingAllocations is already counting');
  GetMemoryManager(PreviousManager);
  CountingManager := PreviousManager;
  CountingManager.GetMem := @CountingGetMem;
  CountingManager.AllocMem := @CountingAllocMem;
  CountingManager.ReAllocMem := @CountingReAllocMem;
  AllocationCount := 0;
  AllocationBytes := 0;
  Counting := True;
  SetMemoryManager(CountingManager);
end;

function StopCountingAllocations: Int64;
begin
  if not Counting then
    raise Exception.Create('StopCountingAllocations is not counting');
  SetMemoryManager(PreviousManager);
  Counting := False;
  Result := AllocationCount;
end;

function CountedAllocationBytes: Int64;
begin
  Result := AllocationBytes;
end;

end.
