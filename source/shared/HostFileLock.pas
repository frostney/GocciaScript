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
  ErrorOffset: Integer;
begin
  ALock := Default(THostFileLock);
  AError := '';
  { A link planted at the lock's name would make the open touch its target. }
  if HostPathIsSymlink(ALockPath) then
  begin
    AError := ALockPath + ' is a symbolic link';
    Exit(hflFailed);
  end;
  if not TryEncodeUTF8NullTerminated(ALockPath, PathBytes, ErrorOffset) then
  begin
    AError := 'cannot encode the lock file path ' + ALockPath;
    Exit(hflFailed);
  end;
  ALock.Handle := fpOpen(PAnsiChar(@PathBytes[0]), O_RDWR or O_CREAT, AMode);
  if ALock.Handle < 0 then
  begin
    AError := Format('cannot open %s: %s', [ALockPath,
      SysErrorMessage(fpgeterrno)]);
    Exit(hflFailed);
  end;
  if fpFlock(ALock.Handle, LOCK_EX or LOCK_NB) <> 0 then
  begin
    fpClose(ALock.Handle);
    Exit(hflHeld);
  end;
  ALock.Held := True;
  Result := hflAcquired;
end;
{$ELSEIF DEFINED(MSWINDOWS)}
var
  Overlapped: TOverlapped;
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
  if not LockFileEx(ALock.Handle, LOCKFILE_EXCLUSIVE_LOCK or
     LOCKFILE_FAIL_IMMEDIATELY, 0, 1, 0, Overlapped) then
  begin
    CloseHandle(ALock.Handle);
    Exit(hflHeld);
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
