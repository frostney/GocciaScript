unit HostFileLock;

{ An OS-level exclusive lock on a lock file: `flock` on POSIX, `LockFileEx`
  on Windows. The system releases it when its holder exits, crashed or not,
  so a lock never outlives its holder and nothing has to guess whether one
  is stale. The lock file itself stays in place. }

{$I Shared.inc}

interface

uses
  {$IF DEFINED(UNIX) AND NOT DEFINED(LAKON)}
  UnixType,
  {$ELSEIF DEFINED(MSWINDOWS)}
  Windows,
  {$IFEND}
  SysUtils;

type
  THostFileLock = record
    {$IF DEFINED(UNIX) AND NOT DEFINED(LAKON)}
    Handle: cint;
    {$ELSEIF DEFINED(MSWINDOWS)}
    Handle: THandle;
    {$IFEND}
    Held: Boolean;
  end;

  THostFileLockResult = (hflAcquired, hflHeld, hflFailed);

{ Tries once to take the lock on ALockPath, creating the file with AMode
  (POSIX mode bits) when it is missing, without waiting. hflHeld when
  another holder has it; hflFailed, with AError, when the file cannot be
  opened or is a symbolic link, or the host has no file locks. }
function TryAcquireHostFileLock(const ALockPath: string;
  const AMode: Cardinal; out ALock: THostFileLock;
  out AError: string): THostFileLockResult;

procedure ReleaseHostFileLock(var ALock: THostFileLock);

{$IF DEFINED(UNIX) AND NOT DEFINED(LAKON)}
type
  THostFlock = function(AHandle, AOperation: cint): cint;

var
  { Test seam: the flock(2) call, so a test can make it fail the way a
    filesystem without flock does. nil selects fpFlock. }
  HostFileLockFlock: THostFlock = nil;
{$ELSEIF DEFINED(MSWINDOWS)}
type
  THostLockFileEx = function(AHandle: THandle; AFlags: DWORD;
    var AOverlapped: TOverlapped): BOOL;

var
  { Test seam: the LockFileEx call, so a test can make it fail the way a
    volume without byte-range locks does. nil selects LockFileEx. }
  HostFileLockLockFileEx: THostLockFileEx = nil;
{$IFEND}

implementation

uses
  {$IF DEFINED(UNIX) AND NOT DEFINED(LAKON)}
  BaseUnix,
  Unix,
  {$IFEND}
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}

  FileUtils,
  TextEncoding;

function TryAcquireHostFileLock(const ALockPath: string;
  const AMode: Cardinal; out ALock: THostFileLock;
  out AError: string): THostFileLockResult;
{$IF DEFINED(UNIX) AND NOT DEFINED(LAKON)}
var
  PathBytes: TBytes;
  Error, ErrorOffset: Integer;
  Status: cint;
begin
  ALock := Default(THostFileLock);
  AError := '';
  if not TryEncodeUTF8NullTerminated(ALockPath, PathBytes, ErrorOffset) then
  begin
    AError := 'cannot encode the lock file path ' + ALockPath;
    Exit(hflFailed);
  end;
  { O_NOFOLLOW in the open itself: a link planted at the lock's name, before
    or during the call, fails the open instead of touching its target. }
  ALock.Handle := fpOpen(PAnsiChar(@PathBytes[0]), O_RDWR or O_CREAT or
    HOST_O_NOFOLLOW or HOST_O_CLOEXEC, AMode);
  if ALock.Handle < 0 then
  begin
    Error := fpgeterrno;
    if HostPathIsSymlink(ALockPath) then
      AError := ALockPath + ' is a symbolic link'
    else
      AError := Format('cannot open %s: %s', [ALockPath,
        SysErrorMessage(Error)]);
    Exit(hflFailed);
  end;
  if Assigned(HostFileLockFlock) then
    Status := HostFileLockFlock(ALock.Handle, LOCK_EX or LOCK_NB)
  else
    Status := fpFlock(ALock.Handle, LOCK_EX or LOCK_NB);
  if Status <> 0 then
  begin
    Error := fpgeterrno;
    fpClose(ALock.Handle);
    { Only another holder is "held"; any other failure, such as a
      filesystem without flock, fails straight away. }
    if (Error = ESysEWOULDBLOCK) or (Error = ESysEAGAIN) then
      Exit(hflHeld);
    AError := Format('cannot lock %s: %s', [ALockPath,
      SysErrorMessage(Error)]);
    Exit(hflFailed);
  end;
  ALock.Held := True;
  Result := hflAcquired;
end;
{$ELSEIF DEFINED(MSWINDOWS)}
var
  Overlapped: TOverlapped;
  Locked: BOOL;
  Error: DWORD;
begin
  ALock := Default(THostFileLock);
  AError := '';
  ALock.Handle := CreateFileW(PWideChar(UnicodeString(ALockPath)),
    GENERIC_READ or GENERIC_WRITE,
    FILE_SHARE_READ or FILE_SHARE_WRITE or FILE_SHARE_DELETE, nil,
    OPEN_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);
  if ALock.Handle = INVALID_HANDLE_VALUE then
  begin
    AError := Format('cannot open %s: %s', [ALockPath,
      SysErrorMessage(GetLastError)]);
    Exit(hflFailed);
  end;
  FillChar(Overlapped, SizeOf(Overlapped), 0);
  if Assigned(HostFileLockLockFileEx) then
    Locked := HostFileLockLockFileEx(ALock.Handle, LOCKFILE_EXCLUSIVE_LOCK or
      LOCKFILE_FAIL_IMMEDIATELY, Overlapped)
  else
    Locked := LockFileEx(ALock.Handle, LOCKFILE_EXCLUSIVE_LOCK or
      LOCKFILE_FAIL_IMMEDIATELY, 0, 1, 0, Overlapped);
  if not Locked then
  begin
    Error := GetLastError;
    CloseHandle(ALock.Handle);
    { Only another holder is "held"; any other failure, such as a volume
      without byte-range locks, fails straight away. }
    if Error = ERROR_LOCK_VIOLATION then
      Exit(hflHeld);
    AError := Format('cannot lock %s: %s', [ALockPath,
      SysErrorMessage(Error)]);
    Exit(hflFailed);
  end;
  ALock.Held := True;
  Result := hflAcquired;
end;
{$ELSE}
begin
  ALock := Default(THostFileLock);
  AError := 'this build has no file locks';
  Result := hflFailed;
end;
{$IFEND}

procedure ReleaseHostFileLock(var ALock: THostFileLock);
{$IF DEFINED(UNIX) AND NOT DEFINED(LAKON)}
begin
  if not ALock.Held then
    Exit;
  fpFlock(ALock.Handle, LOCK_UN);
  fpClose(ALock.Handle);
  ALock.Held := False;
end;
{$ELSEIF DEFINED(MSWINDOWS)}
var
  Overlapped: TOverlapped;
begin
  if not ALock.Held then
    Exit;
  FillChar(Overlapped, SizeOf(Overlapped), 0);
  UnlockFileEx(ALock.Handle, 0, 1, 0, Overlapped);
  CloseHandle(ALock.Handle);
  ALock.Held := False;
end;
{$ELSE}
begin
  ALock.Held := False;
end;
{$IFEND}

end.
