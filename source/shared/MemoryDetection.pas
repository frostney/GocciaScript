unit MemoryDetection;

{$I Shared.inc}

interface

// Returns the effective memory available to this process in bytes.
// On Linux inside a container, returns the cgroup limit when set;
// otherwise returns host physical RAM. Returns 0 on detection failure.
function GetAvailableMemoryBytes: Int64;

implementation

{$IFDEF UNIX}
function libc_sysconf(Name: Integer): Int64; cdecl; external 'c' name 'sysconf';
{$ENDIF}

// Windows: GlobalMemoryStatusEx with MEMORYSTATUSEX is declared inline
// because the standard FPC 3.2.2 Windows unit only provides the older
// GlobalMemoryStatus/TMemoryStatus API. Microsoft documents that
// GlobalMemoryStatus caps dwTotalPhys at 2 GB on x86 systems with
// 2-4 GB of RAM. We use GlobalMemoryStatusEx which has 64-bit fields
// (DWORDLONG) that report correctly on all systems.
{$IFDEF MSWINDOWS}
type
  DWORDLONG = QWord;
  TMemoryStatusEx = record
    dwLength: LongWord;
    dwMemoryLoad: LongWord;
    ullTotalPhys: DWORDLONG;
    ullAvailPhys: DWORDLONG;
    ullTotalPageFile: DWORDLONG;
    ullAvailPageFile: DWORDLONG;
    ullTotalVirtual: DWORDLONG;
    ullAvailVirtual: DWORDLONG;
    ullAvailExtendedVirtual: DWORDLONG;
  end;

function GlobalMemoryStatusEx(var ALpBuffer: TMemoryStatusEx): LongBool;
  stdcall; external 'kernel32.dll' name 'GlobalMemoryStatusEx';
{$ENDIF}

function GetPhysicalMemoryBytes: Int64;
{$IFDEF UNIX}
const
  {$IFDEF DARWIN}
  SC_PHYS_PAGES = 200;
  SC_PAGESIZE   = 29;
  {$ELSE}
  SC_PHYS_PAGES = 85;  { Linux }
  SC_PAGESIZE   = 30;
  {$ENDIF}
var
  Pages, PageSize: Int64;
{$ENDIF}
{$IFDEF MSWINDOWS}
var
  MemStatus: TMemoryStatusEx;
{$ENDIF}
begin
  {$IFDEF UNIX}
  Pages := libc_sysconf(SC_PHYS_PAGES);
  PageSize := libc_sysconf(SC_PAGESIZE);
  if (Pages > 0) and (PageSize > 0) then
    Result := Pages * PageSize
  else
    Result := 0;
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  FillChar(MemStatus, SizeOf(MemStatus), 0);
  MemStatus.dwLength := SizeOf(MemStatus);
  if GlobalMemoryStatusEx(MemStatus) then
    Result := Int64(MemStatus.ullTotalPhys)
  else
    Result := 0;
  {$ENDIF}
end;

// Linux cgroup memory limit detection for container-aware defaults.
// Inside a container the host physical RAM is visible via sysconf, which
// overstates the memory actually available to the process.  Check cgroup
// v2 and v1 limits first and fall back to physical RAM when no valid
// cgroup limit is found.
{$IFDEF LINUX}
const
  CGROUP_V2_MEMORY_MAX = '/sys/fs/cgroup/memory.max';
  CGROUP_V1_MEMORY_LIMIT = '/sys/fs/cgroup/memory/memory.limit_in_bytes';
  CGROUP_UNLIMITED = 'max';

function TryReadCgroupMemoryLimit(const APath: string;
  out ALimit: Int64): Boolean;
var
  F: TextFile;
  Line: string;
  Code: Integer;
  Err: Integer;
begin
  Result := False;
  ALimit := 0;
  {$I-}
  AssignFile(F, APath);
  Reset(F);
  if IOResult <> 0 then
    Exit;
  ReadLn(F, Line);
  Err := IOResult;
  CloseFile(F);
  Err := Err or IOResult;
  {$I+}
  if Err <> 0 then
    Exit;
  if (Line = '') or (Line = CGROUP_UNLIMITED) then
    Exit;
  Val(Line, ALimit, Code);
  if (Code <> 0) or (ALimit <= 0) then
  begin
    ALimit := 0;
    Exit;
  end;
  Result := True;
end;

function GetCgroupMemoryLimitBytes: Int64;
begin
  if TryReadCgroupMemoryLimit(CGROUP_V2_MEMORY_MAX, Result) then
    Exit;
  if TryReadCgroupMemoryLimit(CGROUP_V1_MEMORY_LIMIT, Result) then
    Exit;
  Result := 0;
end;
{$ENDIF}

function GetAvailableMemoryBytes: Int64;
begin
  {$IFDEF LINUX}
  Result := GetCgroupMemoryLimitBytes;
  if Result > 0 then
    Exit;
  {$ENDIF}
  Result := GetPhysicalMemoryBytes;
end;

end.
