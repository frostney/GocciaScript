unit FileUtils;

{$I Shared.inc}

interface

uses
  {$IFDEF UNIX}BaseUnix,{$ENDIF}
  {$IFDEF MSWINDOWS}Windows,{$ENDIF}
  Classes,
  SysUtils;

{$IF DEFINED(UNIX) AND NOT DEFINED(LAKON)}
const
  { open(2)/openat(2) flags and *at(2) arguments FPC 3.2.2's BaseUnix does
    not declare on every host, or declares wrongly: on Linux it gives every
    CPU but SPARC and MIPS the x86 O_DIRECTORY/O_NOFOLLOW values, but arm,
    aarch64 and powerpc use their own (arch/*/include/uapi/asm/fcntl.h),
    where $20000 is O_LARGEFILE and would make O_NOFOLLOW a no-op. Linux's
    AT_* values are the same on every architecture (linux/fcntl.h).
    Darwin's come from the macOS SDK / xnu bsd/sys/fcntl.h, FreeBSD's from
    sys/sys/fcntl.h. }
  {$IF DEFINED(LINUX)}
  {$IF DEFINED(CPUARM) OR DEFINED(CPUAARCH64) OR DEFINED(CPUPOWERPC) OR
    DEFINED(CPUPOWERPC64)}
  HOST_O_NOFOLLOW = $8000;
  HOST_O_DIRECTORY = $4000;
  {$ELSE}
  HOST_O_NOFOLLOW = BaseUnix.O_NOFOLLOW;
  HOST_O_DIRECTORY = BaseUnix.O_DIRECTORY;
  {$IFEND}
  { The same on every Linux architecture this project builds for. }
  HOST_O_CLOEXEC = $80000;
  HOST_AT_FDCWD = -100;
  HOST_AT_SYMLINK_NOFOLLOW = $100;
  HOST_AT_REMOVEDIR = $200;
  {$ELSEIF DEFINED(DARWIN)}
  HOST_O_NOFOLLOW = $0100;
  HOST_O_DIRECTORY = $100000;
  HOST_O_CLOEXEC = $1000000;
  HOST_AT_FDCWD = -2;
  HOST_AT_SYMLINK_NOFOLLOW = $20;
  HOST_AT_REMOVEDIR = $80;
  {$ELSEIF DEFINED(FREEBSD)}
  HOST_O_NOFOLLOW = $0100;
  HOST_O_DIRECTORY = $20000;
  HOST_O_CLOEXEC = $100000;
  HOST_AT_FDCWD = -100;
  HOST_AT_SYMLINK_NOFOLLOW = $200;
  HOST_AT_REMOVEDIR = $800;
  {$ELSE}
    {$ERROR Declare the open(2) and *at(2) constants for this host in FileUtils}
  {$IFEND}
{$IFEND}

function FindAllFiles(const ADirectory: string; const AFileExtension: string): TStringList; overload;
function FindAllFiles(const ADirectory: string; const AFileExtensions: array of string): TStringList; overload;

{ FindAllFiles, but subdirectories whose name appears in
  AExcludedDirectoryNames are not descended into. Case-sensitive, matching how
  the names it excludes are spelled on disk. }
function FindAllFilesExcludingDirectories(const ADirectory: string;
  const AFileExtensions: array of string;
  const AExcludedDirectoryNames: array of string): TStringList;
{ True when APath is rooted rather than interpreted against a working
  directory. The test is platform-specific because the spellings are: on UNIX
  only a leading '/' roots a path, and a backslash is an ordinary filename
  character; on Windows a UNC prefix, a leading separator, or a drive letter
  *followed by a separator* does, while the drive-relative `C:packages` is
  resolved against that drive's working directory and is therefore not
  absolute.
  (Several units still carry private copies of this predating the shared one;
  they are unchanged here rather than refactored in passing.) }
function IsAbsoluteHostPath(const APath: string): Boolean;
function ExpandHostFileName(const APath: string): string;
function HostDirectoryExists(const APath: string): Boolean;
function HostFileExists(const APath: string): Boolean;

{ True when APath itself is a symbolic link (UNIX) or a reparse
  point / junction (Windows). Does not follow the link. }
function HostPathIsSymlink(const APath: string): Boolean;

{ True when APath itself is a regular file: not a directory, and not a
  symbolic link or reparse point, which is not followed. }
function HostPathIsRegularFile(const APath: string): Boolean;

{ Every byte of the open file AHandle, read from its start through that
  handle, so the bytes read are those of the file the handle pins. False
  with AError when the handle cannot be read. }
function ReadHostHandleBytes(const AHandle: THandle; out ABytes: TBytes;
  out AError: string): Boolean;

{ APath with every symbolic link along it resolved to the file it physically
  names, or '' when the host cannot answer.

  ExpandHostFileName only normalizes a *spelling*: it collapses `.` and `..`
  and makes the path absolute, but it never touches the filesystem, so a path
  that normalizes inside a directory can still resolve outside it through a
  symlinked component. This resolves the links, which is what a containment
  guarantee has to be phrased in.

  '' means "unknown", never "root", and a caller must decide for itself what an
  unknown means. It is returned when the path does not exist (POSIX
  `realpath` and the Windows handle open both require it to), when the name
  cannot be encoded for the host, and on builds with no canonicalization
  available — currently the Lakon/WASI lane, whose filesystem is the virtual
  one in SandboxVirtualFileSystem and has no symbolic links at all. }
function CanonicalHostPath(const APath: string): string;

{$IFDEF MSWINDOWS}
{ Opens the existing file APath for reading and pins it: it is opened
  without FILE_SHARE_WRITE or FILE_SHARE_DELETE, so while AHandle is held
  the file cannot be written, renamed, or deleted, and no directory on its
  path can be renamed or replaced. AFinalPath is the opened file's path as
  GetFinalPathNameByHandleW reports it, in CanonicalHostPath's spelling.
  False, with AHandle closed, when the file cannot be opened or its path
  cannot be read. Release the pin with ClosePinnedHostFile. }
function OpenPinnedHostFile(const APath: string; out AHandle: THandle;
  out AFinalPath: string): Boolean;
procedure ClosePinnedHostFile(const AHandle: THandle);
{$ENDIF}

{ Read an entire file as strict UTF-8 source text. No BOM stripping or
  newline normalization is performed. Invalid UTF-8 raises EConvertError. }
function ReadUTF8FileText(const APath: string): string;
procedure WriteUTF8FileText(const APath, AText: string);

{ Read an entire file as raw bytes, preserving every byte exactly
  (NUL bytes, non-UTF-8 sequences, and original newlines). }
function ReadFileBytes(const APath: string): TBytes;
{ As ReadFileBytes, for a file other processes replace while it is read (the
  trust store). On Windows the file is opened sharing read, write, and
  delete, so a concurrent ReplaceHostFile can rename over it while it is open,
  and a sharing violation from some other tool holding it is retried briefly.
  Elsewhere this is ReadFileBytes: POSIX has no share modes, and a rename
  over an open file leaves the reader with the file it opened. }
function ReadSharedHostFileBytes(const APath: string): TBytes;

{ Replace APath's contents with ABytes so that APath afterwards holds either
  the new bytes or exactly what it held before, never neither.

  The bytes go to ATemporaryPath first, which must be in APath's directory so
  the final rename stays on one filesystem. The temporary is created
  exclusively: a symbolic link at that name is refused rather than followed,
  so a link planted beside the target cannot redirect the write. A regular
  file there is a leftover from an interrupted write and is removed first.
  On POSIX and Windows the temporary is flushed to disk and then replaces
  APath in one step — rename(2) on POSIX, MoveFileExW with
  MOVEFILE_REPLACE_EXISTING on Windows — without the original being deleted
  beforehand. On Windows the rename uses POSIX semantics where the system
  has them (Windows 10 1607 and later, NTFS), so it succeeds while readers
  that share delete have the old file open; a replace refused with a sharing
  violation or access denied (a reader without delete sharing, an antivirus
  scan, an indexer) is retried in short steps for up to two seconds. The Lakon/WASI lane writes its in-memory filesystem, which has
  nothing to flush, and replaces with a rename.

  Returns False with AError describing the failure; the temporary is removed
  whenever the replacement did not happen. }
function ReplaceHostFile(const APath, ATemporaryPath: string;
  const ABytes: TBytes; out AError: string): Boolean; overload;
{ As ReplaceHostFile, creating the temporary with APermissions (POSIX mode
  bits, narrowed by the umask) instead of 0666, so the file is never more
  readable than intended, not even before the rename. Ignored where the host
  has no POSIX modes. }
function ReplaceHostFile(const APath, ATemporaryPath: string;
  const ABytes: TBytes; const APermissions: Cardinal;
  out AError: string): Boolean; overload;

type
  { Which directory a path named when it was recorded: POSIX device and inode
    (InodeHigh is 0). On Windows, the 64-bit volume serial number and the
    128-bit file ID from GetFileInformationByHandleEx(FileIdInfo), its low
    half in Inode and its high half in InodeHigh. Known is False where the
    host cannot say (Lakon/WASI). Unavailable is True for a Windows directory
    whose file ID the call would not give: such a directory matches no
    identity, and nothing is written under it. }
  THostDirectoryIdentity = record
    Known: Boolean;
    Unavailable: Boolean;
    Device: QWord;
    Inode: QWord;
    InodeHigh: QWord;
  end;

{ The identity of the directory at APath, following links along it. False
  when APath is not a directory. A Windows directory whose file ID cannot be
  read is still a directory: the answer is True with AIdentity.Unavailable. }
function TryHostDirectoryIdentity(const APath: string;
  out AIdentity: THostDirectoryIdentity): Boolean;

{ AIdentity is AExpected, or AExpected is not known. An Unavailable identity
  on either side is never the same. }
function SameDirectoryIdentity(const AIdentity,
  AExpected: THostDirectoryIdentity): Boolean;

{ Why nothing may be written under ADirectory, recorded as AIdentity, or ''
  when its identity does not stand in the way. }
function HostDirectoryIdentityProblem(const ADirectory: string;
  const AIdentity: THostDirectoryIdentity): string;

{ The identity of the directory ARoute leads to under ARoot, which must still
  be the directory recorded as ARootIdentity. ARoute ('' for ARoot itself) is
  walked without following a symbolic link, as ReplaceHostFileBeneath walks
  it, so the answer is False, with AError saying why, when anything on the
  way was replaced by a link or is not a directory. }
function TryHostDirectoryIdentityBeneath(const ARoot: string;
  const ARootIdentity: THostDirectoryIdentity; const ARoute: string;
  out AIdentity: THostDirectoryIdentity; out AError: string): Boolean;

{ ReplaceHostFile for ARelativePath under the directory ARoot, which must
  still be the directory recorded as ARootIdentity: a root replaced since (a
  different directory, or a link to one) is refused. Every directory between
  the root and the file is opened without following a symbolic link, and
  created when missing, and the file itself must not be a link, so nothing
  swapped in after the root was recorded can carry the write elsewhere. On
  POSIX the walk and the write go through directory descriptors, so no name
  is looked up twice; on Windows each component is checked for a reparse
  point before the write. ARelativePath uses PathDelim. The bytes go first to
  the file's name plus ATemporarySuffix, which, like ReplaceHostFile's
  temporary, is refused when it is a link and replaced when it is a leftover
  file. }
function ReplaceHostFileBeneath(const ARoot: string;
  const ARootIdentity: THostDirectoryIdentity;
  const ARelativePath, ATemporarySuffix: string; const ABytes: TBytes;
  out AError: string): Boolean;

{ Deletes ARelativePath under the directory ARoot and everything below it,
  following no symbolic link: a link met anywhere is removed itself, never
  its target. On POSIX every directory is opened without following a link
  and entries are removed relative to it (`unlinkat`). On Windows every
  object is opened as itself (a reparse point too) and held without delete
  sharing, so nothing on the way can be renamed or replaced; its attributes
  are read from the handle, and it is deleted through that handle. True when nothing is left, including when the path
  did not exist; False with AError otherwise. }
function RemoveHostTreeBeneath(const ARoot, ARelativePath: string;
  out AError: string): Boolean;

{ The POSIX permission bits of the file APath. False where the host has no
  POSIX modes, or the file cannot be examined. }
function TryHostFileMode(const APath: string; out AMode: Cardinal): Boolean;

implementation

uses
  {$IF DEFINED(UNIX) AND NOT DEFINED(LAKON)}InitC,{$ENDIF}
  TextEncoding;

{$IFDEF MSWINDOWS}
const
  { A transient hold on a file by another process (a reader, antivirus, an
    indexer) is waited out in these steps, for at most this many. }
  SHARING_RETRY_MILLISECONDS = 50;
  SHARING_RETRY_ATTEMPTS = 40;

function IsTransientSharingError(const AError: DWORD): Boolean;
begin
  Result := (AError = ERROR_SHARING_VIOLATION) or
    (AError = ERROR_ACCESS_DENIED);
end;

const
  { FILE_INFO_BY_HANDLE_CLASS FileRenameInfoEx and its flags (winbase.h,
    Windows 10 1607 and later), missing from FPC 3.2.2's Windows unit. }
  FILE_RENAME_INFO_EX_CLASS = 22;
  FILE_RENAME_FLAG_REPLACE_IF_EXISTS = $00000001;
  FILE_RENAME_FLAG_POSIX_SEMANTICS = $00000002;
  DELETE_ACCESS = $00010000;

type
  {$PUSH}{$PACKRECORDS C}
  { FILE_RENAME_INFO with its Flags member (winbase.h). }
  TFileRenameInfoEx = record
    Flags: DWORD;
    RootDirectory: THandle;
    FileNameLength: DWORD;
    FileName: array[0..0] of WideChar;
  end;
  {$POP}
  PFileRenameInfoEx = ^TFileRenameInfoEx;

function SetFileInformationByHandle(AFile: THandle; AInformationClass: DWORD;
  AInformation: Pointer; ABufferSize: DWORD): BOOL;
  stdcall; external 'kernel32.dll' name 'SetFileInformationByHandle';

{ Renames ASource over ATarget with POSIX semantics: the name is replaced
  even while other handles have the old file open, as long as they share
  delete, which is how every reader of a replaced file opens it. Plain
  MoveFileExW refuses that with access denied until the last handle closes.
  The path is given in its verbatim \\?\ form, as the call requires a
  full path. Returns False with GetLastError set; ERROR_INVALID_PARAMETER,
  ERROR_NOT_SUPPORTED, or ERROR_INVALID_FUNCTION means the system or file
  system cannot do it (before Windows 10 1607, FAT). }
function PosixRenameReplacing(const ASource, ATarget: string): Boolean;
var
  Handle: THandle;
  Target: UnicodeString;
  Info: PFileRenameInfoEx;
  Size: SizeInt;
  LastError: DWORD;
begin
  Target := UnicodeString(StringReplace(ExpandFileName(ATarget), '/', '\',
    [rfReplaceAll]));
  if Copy(Target, 1, 4) <> '\\?\' then
  begin
    if Copy(Target, 1, 2) = '\\' then
      Target := '\\?\UNC\' + Copy(Target, 3, MaxInt)
    else
      Target := '\\?\' + Target;
  end;
  Handle := CreateFileW(PWideChar(UnicodeString(ASource)), DELETE_ACCESS,
    FILE_SHARE_READ or FILE_SHARE_WRITE or FILE_SHARE_DELETE, nil,
    OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
  if Handle = INVALID_HANDLE_VALUE then
    Exit(False);
  Size := SizeOf(TFileRenameInfoEx) + Length(Target) * SizeOf(WideChar);
  GetMem(Info, Size);
  try
    FillChar(Info^, Size, 0);
    Info^.Flags := FILE_RENAME_FLAG_REPLACE_IF_EXISTS or
      FILE_RENAME_FLAG_POSIX_SEMANTICS;
    Info^.RootDirectory := 0;
    Info^.FileNameLength := Length(Target) * SizeOf(WideChar);
    Move(PWideChar(Target)^, Info^.FileName[0],
      Length(Target) * SizeOf(WideChar));
    Result := SetFileInformationByHandle(Handle, FILE_RENAME_INFO_EX_CLASS,
      Info, DWORD(Size));
    LastError := GetLastError;
  finally
    FreeMem(Info);
    CloseHandle(Handle);
  end;
  if not Result then
    SetLastError(LastError);
end;

function IsRenameUnsupported(const AError: DWORD): Boolean;
begin
  Result := (AError = ERROR_INVALID_PARAMETER) or
    (AError = ERROR_NOT_SUPPORTED) or (AError = ERROR_INVALID_FUNCTION);
end;
{$ENDIF}

function IsAbsoluteHostPath(const APath: string): Boolean;
{$IFDEF UNIX}
begin
  { A backslash is an ordinary filename character here, so `\packages` is a
    relative path, not a rooted one. }
  Result := (Length(APath) > 0) and (APath[1] = '/');
end;
{$ELSE}
begin
  if Length(APath) = 0 then
    Exit(False);
  { A UNC path is rooted at the share. }
  if (Copy(APath, 1, 2) = '\\') or (Copy(APath, 1, 2) = '//') then
    Exit(True);
  { A leading separator with no drive is root-relative rather than fully
    qualified, but it is still rooted: it is not interpreted against the
    working directory. }
  if (APath[1] = '\') or (APath[1] = '/') then
    Exit(True);
  { `C:\x` is rooted; `C:x` is drive-*relative* — resolved against that
    drive's own working directory — so only the separator form counts. }
  Result := (Length(APath) >= 3) and
    (APath[2] = ':') and
    ((APath[3] = '\') or (APath[3] = '/')) and
    (UpCase(APath[1]) >= 'A') and (UpCase(APath[1]) <= 'Z');
end;
{$ENDIF}

function ExpandHostFileName(const APath: string): string;
begin
  Result := ExpandFileName(APath);
end;

function HostDirectoryExists(const APath: string): Boolean;
begin
  Result := DirectoryExists(APath);
end;

function HostFileExists(const APath: string): Boolean;
begin
  Result := FileExists(APath);
end;

function HostPathIsSymlink(const APath: string): Boolean;
{$IFDEF UNIX}
var
  Info: Stat;
  ErrorOffset: Integer;
  PathBytes: TBytes;
begin
  if not TryEncodeUTF8NullTerminated(APath, PathBytes, ErrorOffset) then
    Exit(False);
  Result := (fpLStat(PAnsiChar(@PathBytes[0]), Info) = 0) and
    fpS_ISLNK(Info.st_mode);
end;
{$ELSE}
var
  Attr: LongInt;
begin
  Attr := FileGetAttr(APath);
  Result := (Attr <> -1) and ((Attr and faSymLink) <> 0);
end;
{$ENDIF}

function HostPathIsRegularFile(const APath: string): Boolean;
{$IFDEF UNIX}
var
  Info: Stat;
  ErrorOffset: Integer;
  PathBytes: TBytes;
begin
  Result := TryEncodeUTF8NullTerminated(APath, PathBytes, ErrorOffset) and
    (fpLStat(PAnsiChar(@PathBytes[0]), Info) = 0) and
    fpS_ISREG(Info.st_mode);
end;
{$ELSE}
var
  Attributes: LongInt;
begin
  Attributes := FileGetAttr(APath);
  Result := (Attributes <> -1) and
    ((Attributes and (faDirectory or faSymLink)) = 0);
end;
{$ENDIF}

function ReadHostHandleBytes(const AHandle: THandle; out ABytes: TBytes;
  out AError: string): Boolean;
var
  Size, Offset: Int64;
  Count: LongInt;
begin
  ABytes := nil;
  AError := '';
  Size := FileSeek(AHandle, Int64(0), fsFromEnd);
  if (Size < 0) or (FileSeek(AHandle, Int64(0), fsFromBeginning) <> 0) then
  begin
    AError := SysErrorMessage(GetLastOSError);
    Exit(False);
  end;
  SetLength(ABytes, Size);
  Offset := 0;
  while Offset < Size do
  begin
    Count := FileRead(AHandle, ABytes[Offset], Size - Offset);
    if Count < 0 then
    begin
      AError := SysErrorMessage(GetLastOSError);
      Exit(False);
    end;
    if Count = 0 then
    begin
      SetLength(ABytes, Offset);
      Break;
    end;
    Inc(Offset, Count);
  end;
  Result := True;
end;

{$IF DEFINED(UNIX) AND NOT DEFINED(LAKON)}
{ POSIX.1-2008 realpath(3). The two-argument form is used rather than the
  malloc'ing one so no libc `free` has to be bound as well; POSIX requires the
  caller's buffer to hold PATH_MAX bytes, which HOST_PATH_MAX_BYTES is (Linux's
  value — macOS and the BSDs cap lower). }
function HostRealPath(APath: PAnsiChar; AResolved: PAnsiChar): PAnsiChar;
  cdecl; external 'c' name 'realpath';
{$ENDIF}

{$IFDEF MSWINDOWS}
{ FPC 3.2.2's Windows unit stops at the pre-Vista path API, so the one call
  that follows reparse points has to be declared here. FILE_NAME_NORMALIZED
  ($0) plus VOLUME_NAME_DOS ($0) is the drive-letter spelling; the result still
  carries a `\\?\` (or `\\?\UNC\`) prefix, which the caller strips. }
function GetFinalPathNameByHandleW(AFile: THandle; APath: PWideChar;
  APathLength, AFlags: DWORD): DWORD;
  stdcall; external 'kernel32.dll' name 'GetFinalPathNameByHandleW';

const
  { Missing from FPC 3.2.2's Windows unit for the same reason. }
  MOVEFILE_WRITE_THROUGH = $00000008;
{$ENDIF}

{$IFDEF MSWINDOWS}
{ The path of an open file as GetFinalPathNameByHandleW reports it, with the
  `\\?\` (or `\\?\UNC\`) prefix removed; '' when it cannot be read. }
function FinalPathOfHandle(const AHandle: THandle): string;
const
  DEVICE_PATH_PREFIX = '\\?\';
  DEVICE_UNC_PATH_PREFIX = '\\?\UNC\';
var
  Buffer: array of WideChar;
  Needed: DWORD;
  WidePath: UnicodeString;
begin
  Result := '';
  Needed := GetFinalPathNameByHandleW(AHandle, nil, 0, 0);
  if Needed = 0 then
    Exit;
  { The probing call reports the length *including* the terminator and the
    filling one reports it without, so a buffer of that size always holds the
    answer. A second call that asks for more than it fits means the file was
    renamed between the two, and an unknown beats a truncated path. }
  SetLength(Buffer, Needed + 1);
  Needed := GetFinalPathNameByHandleW(AHandle, @Buffer[0], Needed, 0);
  if (Needed = 0) or (Needed > DWORD(Length(Buffer) - 1)) then
    Exit;
  SetString(WidePath, PWideChar(@Buffer[0]), Integer(Needed));
  Result := string(WidePath);
  if Copy(Result, 1, Length(DEVICE_UNC_PATH_PREFIX)) =
     DEVICE_UNC_PATH_PREFIX then
    Result := '\\' + Copy(Result, Length(DEVICE_UNC_PATH_PREFIX) + 1, MaxInt)
  else if Copy(Result, 1, Length(DEVICE_PATH_PREFIX)) = DEVICE_PATH_PREFIX then
    Result := Copy(Result, Length(DEVICE_PATH_PREFIX) + 1, MaxInt);
end;

function OpenPinnedHostFile(const APath: string; out AHandle: THandle;
  out AFinalPath: string): Boolean;
var
  WidePath: UnicodeString;
begin
  AFinalPath := '';
  WidePath := UnicodeString(APath);
  { FILE_SHARE_READ only: the platform loader may read the file while it is
    pinned, but nobody may write, rename, or delete it, which also keeps
    every directory on its path from being renamed. }
  AHandle := CreateFileW(PWideChar(WidePath), GENERIC_READ, FILE_SHARE_READ,
    nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
  if AHandle = INVALID_HANDLE_VALUE then
    Exit(False);
  AFinalPath := FinalPathOfHandle(AHandle);
  if AFinalPath = '' then
  begin
    CloseHandle(AHandle);
    AHandle := INVALID_HANDLE_VALUE;
    Exit(False);
  end;
  Result := True;
end;

procedure ClosePinnedHostFile(const AHandle: THandle);
begin
  if AHandle <> INVALID_HANDLE_VALUE then
    CloseHandle(AHandle);
end;
{$ENDIF}

function CanonicalHostPath(const APath: string): string;
{$IF DEFINED(UNIX) AND NOT DEFINED(LAKON)}
const
  HOST_PATH_MAX_BYTES = 4096;
var
  Buffer: array[0..HOST_PATH_MAX_BYTES - 1] of AnsiChar;
  PathBytes, ResolvedBytes: TBytes;
  ErrorOffset, Length_: Integer;
begin
  Result := '';
  if APath = '' then
    Exit;
  if not TryEncodeUTF8NullTerminated(APath, PathBytes, ErrorOffset) then
    Exit;
  FillChar(Buffer[0], SizeOf(Buffer), 0);
  if HostRealPath(PAnsiChar(@PathBytes[0]), @Buffer[0]) = nil then
    Exit;
  Length_ := 0;
  while (Length_ < SizeOf(Buffer)) and (Buffer[Length_] <> #0) do
    Inc(Length_);
  SetLength(ResolvedBytes, Length_);
  if Length_ > 0 then
    Move(Buffer[0], ResolvedBytes[0], Length_);
  { A path the host handed back is bytes, and the host does not promise they
    are UTF-8. A name this process cannot represent is one it cannot compare
    either, so it stays "unknown" rather than becoming a lossy string. }
  if not TryDecodeUTF8(ResolvedBytes, Result, ErrorOffset) then
    Result := '';
end;
{$ELSE}
{$IFDEF MSWINDOWS}
var
  Handle: THandle;
begin
  Result := '';
  if APath = '' then
    Exit;
  { FILE_FLAG_BACKUP_SEMANTICS is what lets a *directory* be opened at all, and
    zero desired access asks only for the metadata this needs — no read rights,
    so an unreadable file still canonicalizes. Every share mode is granted so
    the probe never blocks whoever else has the file open. }
  Handle := CreateFileW(PWideChar(APath), 0,
    FILE_SHARE_READ or FILE_SHARE_WRITE or FILE_SHARE_DELETE, nil,
    OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS, 0);
  if Handle = INVALID_HANDLE_VALUE then
    Exit;
  try
    Result := FinalPathOfHandle(Handle);
  finally
    CloseHandle(Handle);
  end;
end;
{$ELSE}
begin
  { No canonicalization on this lane. Callers fall back to their lexical check;
    see the interface comment. }
  Result := '';
end;
{$ENDIF}
{$ENDIF}

function MatchesExtension(const AName: string; const AExtensions: array of string): Boolean;
var
  Ext: string;
  I: Integer;
begin
  Ext := ExtractFileExt(AName);
  for I := Low(AExtensions) to High(AExtensions) do
    if Ext = AExtensions[I] then
      Exit(True);
  Result := False;
end;

function MatchesExcludedDirectory(const AName: string;
  const AExcludedDirectoryNames: array of string): Boolean;
var
  I: Integer;
begin
  for I := Low(AExcludedDirectoryNames) to High(AExcludedDirectoryNames) do
    if AName = AExcludedDirectoryNames[I] then
      Exit(True);
  Result := False;
end;

function FindAllFilesExcludingDirectories(const ADirectory: string;
  const AFileExtensions: array of string;
  const AExcludedDirectoryNames: array of string): TStringList;
var
  SearchRec: TSearchRec;
  Files: TStringList;
  SubdirFiles: TStringList;
  Dir: string;
begin
  Files := TStringList.Create;
  Dir := ExcludeTrailingPathDelimiter(ADirectory);

  if FindFirst(Dir + PathDelim + '*', faAnyFile, SearchRec) = 0 then
  begin
    repeat
      if (SearchRec.Attr and faDirectory) = faDirectory then
      begin
        if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') and
           (not MatchesExcludedDirectory(SearchRec.Name,
              AExcludedDirectoryNames)) then
        begin
          SubdirFiles := FindAllFilesExcludingDirectories(
            Dir + PathDelim + SearchRec.Name, AFileExtensions,
            AExcludedDirectoryNames);
          try
            Files.AddStrings(SubdirFiles);
          finally
            SubdirFiles.Free;
          end;
        end;
      end;

      if MatchesExtension(SearchRec.Name, AFileExtensions) then
        Files.Add(Dir + PathDelim + SearchRec.Name);
    until FindNext(SearchRec) <> 0;
  end;
  FindClose(SearchRec);
  Files.Sort;
  Result := Files;
end;

function FindAllFiles(const ADirectory: string; const AFileExtensions: array of string): TStringList;
var
  NoExclusions: array[0..0] of string;
begin
  { An empty open array literal is not spellable here, so a single entry no
    directory name can equal stands in for "exclude nothing". }
  NoExclusions[0] := '';
  Result := FindAllFilesExcludingDirectories(ADirectory, AFileExtensions,
    NoExclusions);
end;

function FindAllFiles(const ADirectory: string; const AFileExtension: string): TStringList;
var
  // A named array rather than the bracket-constructor argument:
  // context-typed constructor arguments refuse OVERLOAD resolution
  // under Lakon (its documented minimal-overload boundary), and the
  // explicit form is identical native code.
  Extensions: array[0..0] of string;
begin
  Extensions[0] := AFileExtension;
  Result := FindAllFiles(ADirectory, Extensions);
end;

function ReplaceHostFile(const APath, ATemporaryPath: string;
  const ABytes: TBytes; const APermissions: Cardinal;
  out AError: string): Boolean; overload;
{$IF DEFINED(UNIX) AND NOT DEFINED(LAKON)}
var
  TemporaryBytes, PathBytes: TBytes;
  ErrorOffset: Integer;
  Handle: cint;
  Offset: SizeInt;
  Written: TSsize;
  Created: Boolean;
begin
  Result := False;
  AError := '';
  Created := False;
  if not TryEncodeUTF8NullTerminated(ATemporaryPath, TemporaryBytes,
       ErrorOffset) or
     not TryEncodeUTF8NullTerminated(APath, PathBytes, ErrorOffset) then
  begin
    AError := 'path cannot be encoded for the host';
    Exit;
  end;
  if HostPathIsSymlink(ATemporaryPath) then
  begin
    AError := ATemporaryPath + ' is a symlink';
    Exit;
  end;
  if FileExists(ATemporaryPath) then
    DeleteFile(ATemporaryPath);

  Handle := fpOpen(PAnsiChar(@TemporaryBytes[0]),
    O_WRONLY or O_CREAT or O_EXCL, APermissions);
  if Handle < 0 then
  begin
    AError := SysErrorMessage(fpgeterrno);
    Exit;
  end;
  Created := True;
  try
    Offset := 0;
    while Offset < Length(ABytes) do
    begin
      Written := fpWrite(Handle, ABytes[Offset], Length(ABytes) - Offset);
      if Written < 0 then
      begin
        if fpgeterrno = ESysEINTR then
          Continue;
        AError := SysErrorMessage(fpgeterrno);
        Break;
      end;
      Inc(Offset, Written);
    end;
    // On disk before the rename makes it the file, or a crash can leave the
    // replaced name holding an empty file.
    if (AError = '') and not FileFlush(Handle) then
      AError := SysErrorMessage(fpgeterrno);
  finally
    if fpClose(Handle) <> 0 then
      if AError = '' then
        AError := SysErrorMessage(fpgeterrno);
  end;

  if AError = '' then
  begin
    if fpRename(PAnsiChar(@TemporaryBytes[0]), PAnsiChar(@PathBytes[0])) = 0 then
      Result := True
    else
      AError := SysErrorMessage(fpgeterrno);
  end;
  if Created and not Result then
    fpUnlink(PAnsiChar(@TemporaryBytes[0]));
end;
{$ELSEIF DEFINED(MSWINDOWS)}
var
  Handle: THandle;
  Offset: SizeInt;
  Written, LastError: DWORD;
  Attempt: Integer;
  PosixRename: Boolean;
begin
  Result := False;
  AError := '';
  if HostPathIsSymlink(ATemporaryPath) then
  begin
    AError := ATemporaryPath + ' is a symlink';
    Exit;
  end;
  if FileExists(ATemporaryPath) then
    DeleteFile(ATemporaryPath);

  { CREATE_NEW fails on any existing name, a reparse point included, so the
    write cannot be redirected between the check above and this open. }
  Handle := CreateFileW(PWideChar(ATemporaryPath), GENERIC_WRITE, 0, nil,
    CREATE_NEW, FILE_ATTRIBUTE_NORMAL, 0);
  if Handle = INVALID_HANDLE_VALUE then
  begin
    AError := SysErrorMessage(GetLastError);
    Exit;
  end;
  try
    Offset := 0;
    while Offset < Length(ABytes) do
    begin
      if not WriteFile(Handle, ABytes[Offset], Length(ABytes) - Offset,
           Written, nil) then
      begin
        AError := SysErrorMessage(GetLastError);
        Break;
      end;
      Inc(Offset, Written);
    end;
    if (AError = '') and not FileFlush(Handle) then
      AError := SysErrorMessage(GetLastError);
  finally
    CloseHandle(Handle);
  end;

  if AError = '' then
  begin
    { A POSIX-semantics rename replaces the name even while readers have
      the old file open (they share delete); where the system cannot do
      that, MoveFileExW. Either is retried briefly on a transient hold. }
    PosixRename := True;
    Attempt := 0;
    repeat
      if PosixRename then
      begin
        if PosixRenameReplacing(ATemporaryPath, APath) then
        begin
          Result := True;
          Break;
        end;
        LastError := GetLastError;
        if IsRenameUnsupported(LastError) then
        begin
          PosixRename := False;
          Continue;
        end;
      end
      else
      begin
        if MoveFileExW(PWideChar(ATemporaryPath), PWideChar(APath),
             MOVEFILE_REPLACE_EXISTING or MOVEFILE_WRITE_THROUGH) then
        begin
          Result := True;
          Break;
        end;
        LastError := GetLastError;
      end;
      if not IsTransientSharingError(LastError) or
         (Attempt >= SHARING_RETRY_ATTEMPTS) then
      begin
        AError := SysErrorMessage(LastError);
        Break;
      end;
      Inc(Attempt);
      SysUtils.Sleep(SHARING_RETRY_MILLISECONDS);
    until False;
  end;
  if not Result then
    DeleteFile(ATemporaryPath);
end;
{$ELSE}
{ The Lakon/WASI lane's filesystem is the virtual one, with no symbolic links
  and a rename that replaces. }
var
  Stream: TFileStream;
begin
  Result := False;
  AError := '';
  try
    Stream := TFileStream.Create(ATemporaryPath, fmCreate);
    try
      if Length(ABytes) > 0 then
        Stream.WriteBuffer(ABytes[0], Length(ABytes));
    finally
      Stream.Free;
    end;
    Result := RenameFile(ATemporaryPath, APath);
    if not Result then
      AError := 'rename failed';
  except
    on E: Exception do
      AError := E.Message;
  end;
  if not Result then
    DeleteFile(ATemporaryPath);
end;
{$ENDIF}

function ReplaceHostFile(const APath, ATemporaryPath: string;
  const ABytes: TBytes; out AError: string): Boolean;
const
  DEFAULT_FILE_PERMISSIONS = &666;
begin
  Result := ReplaceHostFile(APath, ATemporaryPath, ABytes,
    DEFAULT_FILE_PERMISSIONS, AError);
end;

{$IFDEF LAKON}

// The Lakon/WASI file lane ignores share flags on its single-process lane.

function ReadFileBytes(const APath: string): TBytes;
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(APath, fmOpenRead);
  try
    SetLength(Result, Stream.Size);
    if Length(Result) > 0 then
      Stream.ReadBuffer(Result[0], Length(Result));
  finally
    Stream.Free;
  end;
end;

function ReadUTF8FileText(const APath: string): string;
var
  Bytes: TBytes;
  ErrorOffset: Integer;
begin
  Bytes := ReadFileBytes(APath);
  if not TryDecodeUTF8(Bytes, Result, ErrorOffset) then
    raise EConvertError.CreateFmt('Invalid UTF-8 at byte %d in file "%s"',
      [ErrorOffset, APath]);
end;

{$ELSE}

function ReadUTF8FileText(const APath: string): string;
var
  Bytes: TBytes;
  ErrorOffset: Integer;
begin
  Bytes := ReadFileBytes(APath);
  if not TryDecodeUTF8(Bytes, Result, ErrorOffset) then
    raise EConvertError.CreateFmt('Invalid UTF-8 at byte %d in file "%s"',
      [ErrorOffset, APath]);
end;

function ReadFileBytes(const APath: string): TBytes;
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(APath, fmOpenRead or fmShareDenyWrite);
  try
    SetLength(Result, Stream.Size);
    if Length(Result) > 0 then
      Stream.ReadBuffer(Result[0], Length(Result));
  finally
    Stream.Free;
  end;
end;

{$ENDIF}

procedure WriteUTF8FileText(const APath, AText: string);
var
  Bytes: TBytes;
  ErrorOffset: Integer;
  Stream: TFileStream;
begin
  if not TryEncodeUTF8(AText, Bytes, ErrorOffset) then
    raise EConvertError.CreateFmt(
      'Cannot encode lone UTF-16 surrogate at code-unit %d in file "%s"',
      [ErrorOffset, APath]);
  Stream := TFileStream.Create(APath, fmCreate);
  try
    if Length(Bytes) > 0 then
      Stream.WriteBuffer(Bytes[0], Length(Bytes));
  finally
    Stream.Free;
  end;
end;


{ ── Writes beneath a recorded directory ─────────────────────────── }

const
  BENEATH_DIRECTORY_MODE = $1FF;
  BENEATH_FILE_MODE = $1B6;

{$IF DEFINED(UNIX) AND NOT DEFINED(LAKON)}
{ POSIX.1-2008 *at() calls, so each step is relative to a descriptor the
  walk already holds. `openat` is variadic in its mode, which only O_CREAT
  reads. }
function HostOpenAt(ADirectory: cint; APath: PAnsiChar; AFlags: cint): cint;
  cdecl; varargs; external 'c' name 'openat';
function HostMkdirAt(ADirectory: cint; APath: PAnsiChar;
  AMode: TMode): cint; cdecl; external 'c' name 'mkdirat';
function HostRenameAt(AFromDirectory: cint; AFrom: PAnsiChar;
  AToDirectory: cint; ATo: PAnsiChar): cint; cdecl;
  external 'c' name 'renameat';
function HostUnlinkAt(ADirectory: cint; APath: PAnsiChar;
  AFlags: cint): cint; cdecl; external 'c' name 'unlinkat';
{$ENDIF}

{$IFDEF MSWINDOWS}
type
  { FILE_ID_INFO: a 64-bit volume serial number and a 128-bit file ID. }
  THostFileIdInfo = record
    VolumeSerialNumber: QWord;
    FileId: array[0..15] of Byte;
  end;

const
  { FileIdInfo in FILE_INFO_BY_HANDLE_CLASS. }
  HOST_FILE_ID_INFO_CLASS = 18;

function HostGetFileInformationByHandleEx(AFile: THandle;
  AInformationClass: DWORD; AInformation: Pointer;
  ABufferSize: DWORD): BOOL; stdcall;
  external 'kernel32.dll' name 'GetFileInformationByHandleEx';
{$ENDIF}

function TryHostDirectoryIdentity(const APath: string;
  out AIdentity: THostDirectoryIdentity): Boolean;
{$IF DEFINED(UNIX) AND NOT DEFINED(LAKON)}
var
  Info: Stat;
  PathBytes: TBytes;
  ErrorOffset: Integer;
begin
  AIdentity := Default(THostDirectoryIdentity);
  Result := False;
  if not TryEncodeUTF8NullTerminated(APath, PathBytes, ErrorOffset) then
    Exit;
  if (fpStat(PAnsiChar(@PathBytes[0]), Info) <> 0) or
     not fpS_ISDIR(Info.st_mode) then
    Exit;
  AIdentity.Known := True;
  AIdentity.Device := QWord(Info.st_dev);
  AIdentity.Inode := QWord(Info.st_ino);
  Result := True;
end;
{$ELSEIF DEFINED(MSWINDOWS)}
var
  Handle: THandle;
  Information: THostFileIdInfo;
begin
  AIdentity := Default(THostDirectoryIdentity);
  Result := False;
  if not DirectoryExists(APath) then
    Exit;
  { Opened for no access at all, only to ask which file it is; backup
    semantics lets a directory be opened. }
  Handle := CreateFileW(PWideChar(UnicodeString(APath)), 0,
    FILE_SHARE_READ or FILE_SHARE_WRITE or FILE_SHARE_DELETE, nil,
    OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS, 0);
  if Handle = INVALID_HANDLE_VALUE then
    Exit;
  try
    Result := True;
    { The 64-bit file index BY_HANDLE_FILE_INFORMATION gives is not unique
      on ReFS, so there is no falling back to it: a directory without a
      FileIdInfo answer is marked Unavailable and nothing is written under
      it. }
    Information := Default(THostFileIdInfo);
    if not HostGetFileInformationByHandleEx(Handle, HOST_FILE_ID_INFO_CLASS,
       @Information, SizeOf(Information)) then
    begin
      AIdentity.Unavailable := True;
      Exit;
    end;
    AIdentity.Known := True;
    AIdentity.Device := Information.VolumeSerialNumber;
    Move(Information.FileId[0], AIdentity.Inode, SizeOf(AIdentity.Inode));
    Move(Information.FileId[8], AIdentity.InodeHigh,
      SizeOf(AIdentity.InodeHigh));
  finally
    CloseHandle(Handle);
  end;
end;
{$ELSE}
begin
  AIdentity := Default(THostDirectoryIdentity);
  Result := DirectoryExists(APath);
end;
{$ENDIF}

function SameDirectoryIdentity(const AIdentity,
  AExpected: THostDirectoryIdentity): Boolean;
begin
  if AIdentity.Unavailable or AExpected.Unavailable then
    Exit(False);
  Result := (not AExpected.Known) or (AIdentity.Known and
    (AIdentity.Device = AExpected.Device) and
    (AIdentity.Inode = AExpected.Inode) and
    (AIdentity.InodeHigh = AExpected.InodeHigh));
end;

function HostDirectoryIdentityProblem(const ADirectory: string;
  const AIdentity: THostDirectoryIdentity): string;
begin
  if AIdentity.Unavailable then
    Result := 'Windows gives no file ID for ' + ADirectory +
      ' (GetFileInformationByHandleEx FileIdInfo failed), so it cannot be ' +
      'checked for replacement; refusing to write under it'
  else
    Result := '';
end;

function SplitRelativeHostPath(const APath: string): TStringList;
var
  Part: string;
  I, Start: Integer;
begin
  Result := TStringList.Create;
  Start := 1;
  for I := 1 to Length(APath) + 1 do
    if (I > Length(APath)) or (APath[I] = PathDelim) or
       (APath[I] = '/') then
    begin
      Part := Copy(APath, Start, I - Start);
      if Part <> '' then
        Result.Add(Part);
      Start := I + 1;
    end;
end;

function ReplaceHostFileBeneath(const ARoot: string;
  const ARootIdentity: THostDirectoryIdentity;
  const ARelativePath, ATemporarySuffix: string; const ABytes: TBytes;
  out AError: string): Boolean;
{$IF DEFINED(UNIX) AND NOT DEFINED(LAKON)}
var
  Parts: TStringList;
  Directory, Next, Handle: cint;
  Info: Stat;
  NameBytes, TemporaryBytes: TBytes;
  ErrorOffset, I: Integer;
  Leaf: string;
  Offset: SizeInt;
  Written: TSsize;
  Created: Boolean;

  function Encode(const AText: string; out ABytes: TBytes): Boolean;
  begin
    Result := TryEncodeUTF8NullTerminated(AText, ABytes, ErrorOffset);
    if not Result then
      AError := 'path cannot be encoded for the host';
  end;

begin
  Result := False;
  AError := '';
  Created := False;
  Parts := SplitRelativeHostPath(ARelativePath);
  Directory := -1;
  try
    for I := 0 to Parts.Count - 1 do
      if (Parts[I] = '.') or (Parts[I] = '..') then
      begin
        AError := 'the path climbs out of ' + ARoot;
        Exit;
      end;
    if Parts.Count = 0 then
    begin
      AError := 'no file name';
      Exit;
    end;
    AError := HostDirectoryIdentityProblem(ARoot, ARootIdentity);
    if AError <> '' then
      Exit;
    if not Encode(ARoot, NameBytes) then
      Exit;
    Directory := fpOpen(PAnsiChar(@NameBytes[0]),
      O_RDONLY or HOST_O_DIRECTORY or HOST_O_NOFOLLOW);
    if Directory < 0 then
    begin
      AError := ARoot + ' is no longer the copied directory (' +
        SysErrorMessage(fpgeterrno) + ')';
      Exit;
    end;
    if ARootIdentity.Known and ((fpFStat(Directory, Info) <> 0) or
       (QWord(Info.st_dev) <> ARootIdentity.Device) or
       (QWord(Info.st_ino) <> ARootIdentity.Inode)) then
    begin
      AError := ARoot + ' was replaced after it was copied';
      Exit;
    end;

    for I := 0 to Parts.Count - 2 do
    begin
      if not Encode(Parts[I], NameBytes) then
        Exit;
      Next := HostOpenAt(Directory, PAnsiChar(@NameBytes[0]),
        O_RDONLY or HOST_O_DIRECTORY or HOST_O_NOFOLLOW);
      if (Next < 0) and (fpgetCerrno = ESysENOENT) then
      begin
        HostMkdirAt(Directory, PAnsiChar(@NameBytes[0]),
          BENEATH_DIRECTORY_MODE);
        Next := HostOpenAt(Directory, PAnsiChar(@NameBytes[0]),
          O_RDONLY or HOST_O_DIRECTORY or HOST_O_NOFOLLOW);
      end;
      if Next < 0 then
      begin
        AError := Parts[I] + ' is a symbolic link or not a directory';
        Exit;
      end;
      fpClose(Directory);
      Directory := Next;
    end;

    Leaf := Parts[Parts.Count - 1];
    if not Encode(Leaf, NameBytes) or
       not Encode(Leaf + ATemporarySuffix, TemporaryBytes) then
      Exit;
    Handle := HostOpenAt(Directory, PAnsiChar(@NameBytes[0]),
      O_RDONLY or HOST_O_NOFOLLOW or O_NONBLOCK);
    if Handle >= 0 then
    begin
      if (fpFStat(Handle, Info) = 0) and not fpS_ISREG(Info.st_mode) then
      begin
        fpClose(Handle);
        AError := 'the target is not a regular file';
        Exit;
      end;
      fpClose(Handle);
    end
    else if fpgetCerrno <> ESysENOENT then
    begin
      AError := 'the target is a symbolic link or cannot be opened';
      Exit;
    end;

    { A link at the temporary's name is refused; a leftover file from an
      interrupted write is removed. }
    Handle := HostOpenAt(Directory, PAnsiChar(@TemporaryBytes[0]),
      O_RDONLY or HOST_O_NOFOLLOW or O_NONBLOCK);
    if Handle >= 0 then
    begin
      fpClose(Handle);
      HostUnlinkAt(Directory, PAnsiChar(@TemporaryBytes[0]), 0);
    end
    else if fpgetCerrno <> ESysENOENT then
    begin
      AError := IncludeTrailingPathDelimiter(ARoot) + ARelativePath +
        ATemporarySuffix + ' is a symlink';
      Exit;
    end;
    Handle := HostOpenAt(Directory, PAnsiChar(@TemporaryBytes[0]),
      O_WRONLY or O_CREAT or O_EXCL or HOST_O_NOFOLLOW,
      cint(BENEATH_FILE_MODE));
    if Handle < 0 then
    begin
      AError := SysErrorMessage(fpgetCerrno);
      Exit;
    end;
    Created := True;
    try
      Offset := 0;
      while Offset < Length(ABytes) do
      begin
        Written := fpWrite(Handle, ABytes[Offset], Length(ABytes) - Offset);
        if Written < 0 then
        begin
          if fpgeterrno = ESysEINTR then
            Continue;
          AError := SysErrorMessage(fpgeterrno);
          Break;
        end;
        Inc(Offset, Written);
      end;
      if (AError = '') and not FileFlush(Handle) then
        AError := SysErrorMessage(fpgeterrno);
    finally
      if (fpClose(Handle) <> 0) and (AError = '') then
        AError := SysErrorMessage(fpgeterrno);
    end;
    if AError = '' then
    begin
      if HostRenameAt(Directory, PAnsiChar(@TemporaryBytes[0]), Directory,
         PAnsiChar(@NameBytes[0])) = 0 then
        Result := True
      else
        AError := SysErrorMessage(fpgetCerrno);
    end;
    if Created and not Result then
      HostUnlinkAt(Directory, PAnsiChar(@TemporaryBytes[0]), 0);
  finally
    if Directory >= 0 then
      fpClose(Directory);
    Parts.Free;
  end;
end;
{$ELSE}
var
  Parts: TStringList;
  Path: string;
  I: Integer;
  Identity: THostDirectoryIdentity;
begin
  Result := False;
  AError := '';
  Parts := SplitRelativeHostPath(ARelativePath);
  try
    if Parts.Count = 0 then
    begin
      AError := 'no file name';
      Exit;
    end;
    AError := HostDirectoryIdentityProblem(ARoot, ARootIdentity);
    if AError <> '' then
      Exit;
    Path := ExcludeTrailingPathDelimiter(ARoot);
    if HostPathIsSymlink(Path) or not DirectoryExists(Path) then
    begin
      AError := ARoot + ' is no longer the copied directory';
      Exit;
    end;
    if not TryHostDirectoryIdentity(Path, Identity) then
    begin
      AError := ARoot + ' was replaced after it was copied';
      Exit;
    end;
    AError := HostDirectoryIdentityProblem(ARoot, Identity);
    if AError <> '' then
      Exit;
    if not SameDirectoryIdentity(Identity, ARootIdentity) then
    begin
      AError := ARoot + ' was replaced after it was copied';
      Exit;
    end;
    for I := 0 to Parts.Count - 1 do
    begin
      if (Parts[I] = '.') or (Parts[I] = '..') then
      begin
        AError := 'the path climbs out of ' + ARoot;
        Exit;
      end;
      Path := Path + PathDelim + Parts[I];
      if HostPathIsSymlink(Path) then
      begin
        AError := Parts[I] + ' is a symbolic link';
        Exit;
      end;
      if I < Parts.Count - 1 then
      begin
        if not DirectoryExists(Path) and not CreateDir(Path) then
        begin
          AError := 'cannot create ' + Path;
          Exit;
        end;
        if HostPathIsSymlink(Path) or not DirectoryExists(Path) then
        begin
          AError := Parts[I] + ' is a symbolic link or not a directory';
          Exit;
        end;
      end;
    end;
    Result := ReplaceHostFile(Path, Path + ATemporarySuffix, ABytes, AError);
  finally
    Parts.Free;
  end;
end;
{$ENDIF}


function TryHostDirectoryIdentityBeneath(const ARoot: string;
  const ARootIdentity: THostDirectoryIdentity; const ARoute: string;
  out AIdentity: THostDirectoryIdentity; out AError: string): Boolean;
{$IF DEFINED(UNIX) AND NOT DEFINED(LAKON)}
var
  Parts: TStringList;
  Directory, Next: cint;
  Info: Stat;
  NameBytes: TBytes;
  ErrorOffset, I: Integer;
begin
  Result := False;
  AError := '';
  AIdentity := Default(THostDirectoryIdentity);
  Parts := SplitRelativeHostPath(ARoute);
  Directory := -1;
  try
    AError := HostDirectoryIdentityProblem(ARoot, ARootIdentity);
    if AError <> '' then
      Exit;
    if not TryEncodeUTF8NullTerminated(ARoot, NameBytes, ErrorOffset) then
    begin
      AError := 'path cannot be encoded for the host';
      Exit;
    end;
    Directory := fpOpen(PAnsiChar(@NameBytes[0]),
      O_RDONLY or HOST_O_DIRECTORY or HOST_O_NOFOLLOW);
    if (Directory < 0) or (fpFStat(Directory, Info) <> 0) or
       (ARootIdentity.Known and
        ((QWord(Info.st_dev) <> ARootIdentity.Device) or
         (QWord(Info.st_ino) <> ARootIdentity.Inode))) then
    begin
      AError := ARoot + ' was replaced';
      Exit;
    end;
    for I := 0 to Parts.Count - 1 do
    begin
      if (Parts[I] = '.') or (Parts[I] = '..') or
         not TryEncodeUTF8NullTerminated(Parts[I], NameBytes, ErrorOffset) then
      begin
        AError := 'the route climbs out of ' + ARoot;
        Exit;
      end;
      Next := HostOpenAt(Directory, PAnsiChar(@NameBytes[0]),
        O_RDONLY or HOST_O_DIRECTORY or HOST_O_NOFOLLOW);
      if Next < 0 then
      begin
        AError := Parts[I] + ' is a symbolic link or not a directory';
        Exit;
      end;
      fpClose(Directory);
      Directory := Next;
    end;
    if fpFStat(Directory, Info) <> 0 then
    begin
      AError := SysErrorMessage(fpgeterrno);
      Exit;
    end;
    AIdentity.Known := True;
    AIdentity.Device := QWord(Info.st_dev);
    AIdentity.Inode := QWord(Info.st_ino);
    Result := True;
  finally
    if Directory >= 0 then
      fpClose(Directory);
    Parts.Free;
  end;
end;
{$ELSE}
var
  Parts: TStringList;
  Path: string;
  I: Integer;
begin
  Result := False;
  AError := '';
  AIdentity := Default(THostDirectoryIdentity);
  Parts := SplitRelativeHostPath(ARoute);
  try
    AError := HostDirectoryIdentityProblem(ARoot, ARootIdentity);
    if AError <> '' then
      Exit;
    Path := ExcludeTrailingPathDelimiter(ARoot);
    if HostPathIsSymlink(Path) or
       not TryHostDirectoryIdentity(Path, AIdentity) then
    begin
      AError := ARoot + ' was replaced';
      Exit;
    end;
    AError := HostDirectoryIdentityProblem(ARoot, AIdentity);
    if AError <> '' then
      Exit;
    if not SameDirectoryIdentity(AIdentity, ARootIdentity) then
    begin
      AError := ARoot + ' was replaced';
      Exit;
    end;
    for I := 0 to Parts.Count - 1 do
    begin
      Path := Path + PathDelim + Parts[I];
      if (Parts[I] = '.') or (Parts[I] = '..') or HostPathIsSymlink(Path) or
         not DirectoryExists(Path) then
      begin
        AError := Parts[I] + ' is a symbolic link or not a directory';
        Exit;
      end;
    end;
    Result := TryHostDirectoryIdentity(Path, AIdentity);
  finally
    Parts.Free;
  end;
end;
{$ENDIF}

function ReadSharedHostFileBytes(const APath: string): TBytes;
{$IFDEF MSWINDOWS}
var
  Handle: THandle;
  LastError: DWORD;
  Attempt: Integer;
  ReadError: string;
begin
  Attempt := 0;
  repeat
    Handle := CreateFileW(PWideChar(UnicodeString(APath)), GENERIC_READ,
      FILE_SHARE_READ or FILE_SHARE_WRITE or FILE_SHARE_DELETE, nil,
      OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
    if Handle <> INVALID_HANDLE_VALUE then
      Break;
    LastError := GetLastError;
    if not IsTransientSharingError(LastError) or
       (Attempt >= SHARING_RETRY_ATTEMPTS) then
      raise EFOpenError.CreateFmt('Unable to open file "%s": %s',
        [APath, SysErrorMessage(LastError)]);
    Inc(Attempt);
    SysUtils.Sleep(SHARING_RETRY_MILLISECONDS);
  until False;
  try
    if not ReadHostHandleBytes(Handle, Result, ReadError) then
      raise EReadError.CreateFmt('Unable to read file "%s": %s',
        [APath, ReadError]);
  finally
    CloseHandle(Handle);
  end;
end;
{$ELSE}
begin
  Result := ReadFileBytes(APath);
end;
{$ENDIF}

function TryHostFileMode(const APath: string; out AMode: Cardinal): Boolean;
{$IF DEFINED(UNIX) AND NOT DEFINED(LAKON)}
const
  PERMISSION_BITS = &7777;
var
  Info: Stat;
  PathBytes: TBytes;
  ErrorOffset: Integer;
begin
  AMode := 0;
  Result := TryEncodeUTF8NullTerminated(APath, PathBytes, ErrorOffset) and
    (fpStat(PAnsiChar(@PathBytes[0]), Info) = 0);
  if Result then
    AMode := Info.st_mode and PERMISSION_BITS;
end;
{$ELSE}
begin
  AMode := 0;
  Result := False;
end;
{$IFEND}

function RemoveHostTreeBeneath(const ARoot, ARelativePath: string;
  out AError: string): Boolean;
{$IF DEFINED(UNIX) AND NOT DEFINED(LAKON)}
var
  Parts: TStringList;
  Directory, Next: cint;
  NameBytes: TBytes;
  ErrorOffset, I: Integer;
  ListingPath: string;

  function Encode(const AText: string; out ABytes: TBytes): Boolean;
  begin
    Result := TryEncodeUTF8NullTerminated(AText, ABytes, ErrorOffset);
    if not Result then
      AError := 'path cannot be encoded for the host';
  end;

  { Removes AName in the directory AParent. The names below a directory are
    listed by path, which may name something else if a link was swapped in,
    but every removal is relative to the descriptor opened without following
    a link, so nothing outside the tree can be removed. }
  function RemoveEntry(const AParent: cint; const AName,
    APath: string): Boolean;
  var
    Child: cint;
    Entry: PDirent;
    Listing: PDir;
    Names: TStringList;
    Bytes: TBytes;
    J: Integer;
  begin
    Result := False;
    if not Encode(AName, Bytes) then
      Exit;
    Child := HostOpenAt(AParent, PAnsiChar(@Bytes[0]),
      O_RDONLY or HOST_O_DIRECTORY or HOST_O_NOFOLLOW);
    if Child < 0 then
    begin
      { A file or a link: removed itself. }
      if (HostUnlinkAt(AParent, PAnsiChar(@Bytes[0]), 0) <> 0) and
         (fpgetCerrno <> ESysENOENT) then
      begin
        AError := Format('cannot remove %s: %s', [APath,
          SysErrorMessage(fpgetCerrno)]);
        Exit;
      end;
      Exit(True);
    end;
    Names := TStringList.Create;
    try
      Listing := fpOpenDir(APath);
      if Assigned(Listing) then
      try
        repeat
          Entry := fpReadDir(Listing^);
          if Assigned(Entry) and (StrPas(PAnsiChar(@Entry^.d_name[0])) <> '.')
             and (StrPas(PAnsiChar(@Entry^.d_name[0])) <> '..') then
            Names.Add(StrPas(PAnsiChar(@Entry^.d_name[0])));
        until not Assigned(Entry);
      finally
        fpCloseDir(Listing^);
      end;
      for J := 0 to Names.Count - 1 do
        if not RemoveEntry(Child, Names[J], APath + PathDelim + Names[J]) then
          Exit;
    finally
      Names.Free;
      fpClose(Child);
    end;
    if (HostUnlinkAt(AParent, PAnsiChar(@Bytes[0]), HOST_AT_REMOVEDIR) <> 0)
       and (fpgetCerrno <> ESysENOENT) then
    begin
      AError := Format('cannot remove %s: %s', [APath,
        SysErrorMessage(fpgetCerrno)]);
      Exit;
    end;
    Result := True;
  end;

begin
  Result := False;
  AError := '';
  Parts := SplitRelativeHostPath(ARelativePath);
  Directory := -1;
  try
    if Parts.Count = 0 then
    begin
      AError := 'no path below ' + ARoot;
      Exit;
    end;
    for I := 0 to Parts.Count - 1 do
      if (Parts[I] = '.') or (Parts[I] = '..') then
      begin
        AError := 'the path climbs out of ' + ARoot;
        Exit;
      end;
    if not Encode(ARoot, NameBytes) then
      Exit;
    Directory := fpOpen(PAnsiChar(@NameBytes[0]),
      O_RDONLY or HOST_O_DIRECTORY or HOST_O_NOFOLLOW);
    if Directory < 0 then
    begin
      AError := ARoot + ' is a symbolic link or not a directory';
      Exit;
    end;
    ListingPath := ExcludeTrailingPathDelimiter(ARoot);
    for I := 0 to Parts.Count - 2 do
    begin
      if not Encode(Parts[I], NameBytes) then
        Exit;
      Next := HostOpenAt(Directory, PAnsiChar(@NameBytes[0]),
        O_RDONLY or HOST_O_DIRECTORY or HOST_O_NOFOLLOW);
      if Next < 0 then
      begin
        if fpgetCerrno = ESysENOENT then
          Exit(True);
        AError := Parts[I] + ' is a symbolic link or not a directory';
        Exit;
      end;
      fpClose(Directory);
      Directory := Next;
      ListingPath := ListingPath + PathDelim + Parts[I];
    end;
    Result := RemoveEntry(Directory, Parts[Parts.Count - 1],
      ListingPath + PathDelim + Parts[Parts.Count - 1]);
  finally
    if Directory >= 0 then
      fpClose(Directory);
    Parts.Free;
  end;
end;
{$ELSEIF DEFINED(MSWINDOWS)}
const
  { CreateFileW flags and FILE_INFO_BY_HANDLE_CLASS values (winbase.h). }
  OPEN_REPARSE_POINT_FLAG = $00200000;
  BACKUP_SEMANTICS_FLAG = $02000000;
  FILE_DISPOSITION_INFO_CLASS = 4;
  FILE_DISPOSITION_INFO_EX_CLASS = 21;
  FILE_DISPOSITION_FLAG_DELETE = $00000001;
  FILE_DISPOSITION_FLAG_POSIX_SEMANTICS = $00000002;
  FILE_DISPOSITION_FLAG_IGNORE_READONLY_ATTRIBUTE = $00000010;
  { FILE_LIST_DIRECTORY or FILE_READ_ATTRIBUTES; DELETE for what is
    removed. }
  HOLD_ACCESS = $00000001 or $00000080;
  ENTRY_ACCESS = HOLD_ACCESS or DELETE_ACCESS;
var
  Held: array of THandle;
  Parts: TStringList;
  Path: string;
  Handle: THandle;
  Attributes: DWORD;
  I: Integer;

  { Opens APath itself, a reparse point as itself, and holds it: without
    FILE_SHARE_DELETE nobody can rename, delete, or replace it, or any
    directory on its path, while the handle is open. }
  function OpenHeld(const APath: string; const AAccess: DWORD;
    out AHandle: THandle; out AAttributes: DWORD;
    out AMissing: Boolean): Boolean;
  var
    Information: BY_HANDLE_FILE_INFORMATION;
    LastError: DWORD;
  begin
    Result := False;
    AMissing := False;
    AAttributes := 0;
    AHandle := CreateFileW(PWideChar(UnicodeString(APath)), AAccess,
      FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING,
      BACKUP_SEMANTICS_FLAG or OPEN_REPARSE_POINT_FLAG, 0);
    if AHandle = INVALID_HANDLE_VALUE then
    begin
      LastError := GetLastError;
      AMissing := (LastError = ERROR_FILE_NOT_FOUND) or
        (LastError = ERROR_PATH_NOT_FOUND);
      if not AMissing then
        AError := Format('cannot open %s: %s', [APath,
          SysErrorMessage(LastError)]);
      Exit;
    end;
    if not GetFileInformationByHandle(AHandle, Information) then
    begin
      AError := Format('cannot examine %s: %s', [APath,
        SysErrorMessage(GetLastError)]);
      CloseHandle(AHandle);
      AHandle := INVALID_HANDLE_VALUE;
      Exit;
    end;
    AAttributes := Information.dwFileAttributes;
    Result := True;
  end;

  { Marks the object AHandle holds for deletion, which happens when the
    handle closes: the object checked is the object deleted. POSIX
    semantics where the system has them (Windows 10 1709 and later, NTFS),
    else the classic disposition. }
  function DeleteHeld(const AHandle: THandle; const APath: string): Boolean;
  var
    Flags: DWORD;
    Dispose: ByteBool;
  begin
    Flags := FILE_DISPOSITION_FLAG_DELETE or
      FILE_DISPOSITION_FLAG_POSIX_SEMANTICS or
      FILE_DISPOSITION_FLAG_IGNORE_READONLY_ATTRIBUTE;
    Result := SetFileInformationByHandle(AHandle,
      FILE_DISPOSITION_INFO_EX_CLASS, @Flags, SizeOf(Flags));
    if Result then
      Exit;
    Dispose := True;
    Result := SetFileInformationByHandle(AHandle, FILE_DISPOSITION_INFO_CLASS,
      @Dispose, SizeOf(Dispose));
    if not Result then
      AError := Format('cannot remove %s: %s', [APath,
        SysErrorMessage(GetLastError)]);
  end;

  function RemoveEntry(const APath: string): Boolean;
  var
    EntryHandle: THandle;
    EntryAttributes: DWORD;
    Missing: Boolean;
    SearchRecord: TSearchRec;
  begin
    Result := False;
    if not OpenHeld(APath, ENTRY_ACCESS, EntryHandle, EntryAttributes,
       Missing) then
      Exit(Missing);
    try
      { A directory is entered only when it is not a reparse point; while
        it is held its children are listed by a path that cannot change. }
      if ((EntryAttributes and FILE_ATTRIBUTE_DIRECTORY) <> 0) and
         ((EntryAttributes and FILE_ATTRIBUTE_REPARSE_POINT) = 0) then
      begin
        if FindFirst(IncludeTrailingPathDelimiter(APath) + '*', faAnyFile,
           SearchRecord) = 0 then
        try
          repeat
            if (SearchRecord.Name = '.') or (SearchRecord.Name = '..') then
              Continue;
            if not RemoveEntry(IncludeTrailingPathDelimiter(APath) +
               SearchRecord.Name) then
              Exit;
          until FindNext(SearchRecord) <> 0;
        finally
          FindClose(SearchRecord);
        end;
      end;
      Result := DeleteHeld(EntryHandle, APath);
    finally
      CloseHandle(EntryHandle);
    end;
  end;

  procedure Hold(const AHandle: THandle);
  begin
    SetLength(Held, Length(Held) + 1);
    Held[High(Held)] := AHandle;
  end;

var
  Missing: Boolean;
begin
  Result := False;
  AError := '';
  Held := nil;
  Parts := SplitRelativeHostPath(ARelativePath);
  try
    if Parts.Count = 0 then
    begin
      AError := 'no path below ' + ARoot;
      Exit;
    end;
    for I := 0 to Parts.Count - 1 do
      if (Parts[I] = '.') or (Parts[I] = '..') then
      begin
        AError := 'the path climbs out of ' + ARoot;
        Exit;
      end;
    { The root and every directory on the way are held for the whole
      removal, and none may be a reparse point. }
    Path := ExcludeTrailingPathDelimiter(ARoot);
    for I := -1 to Parts.Count - 2 do
    begin
      if I >= 0 then
        Path := Path + PathDelim + Parts[I];
      if not OpenHeld(Path, HOLD_ACCESS, Handle, Attributes, Missing) then
      begin
        if Missing and (I >= 0) then
          Result := True
        else if Missing then
          AError := ARoot + ' does not exist';
        Exit;
      end;
      Hold(Handle);
      if (Attributes and FILE_ATTRIBUTE_REPARSE_POINT) <> 0 then
      begin
        AError := Path + ' is a reparse point';
        Exit;
      end;
      if (Attributes and FILE_ATTRIBUTE_DIRECTORY) = 0 then
      begin
        AError := Path + ' is not a directory';
        Exit;
      end;
    end;
    Result := RemoveEntry(Path + PathDelim + Parts[Parts.Count - 1]);
  finally
    for I := High(Held) downto 0 do
      CloseHandle(Held[I]);
    Parts.Free;
  end;
end;
{$ELSE}
begin
  AError := 'this build cannot remove a directory tree';
  Result := False;
end;
{$IFEND}

end.
