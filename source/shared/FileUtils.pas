unit FileUtils;

{$I Shared.inc}

interface

uses
  {$IFDEF UNIX}BaseUnix,{$ENDIF}
  {$IFDEF MSWINDOWS}Windows,{$ENDIF}
  Classes,
  SysUtils;

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

{ Read an entire file as strict UTF-8 source text. No BOM stripping or
  newline normalization is performed. Invalid UTF-8 raises EConvertError. }
function ReadUTF8FileText(const APath: string): string;
procedure WriteUTF8FileText(const APath, AText: string);

{ Read an entire file as raw bytes, preserving every byte exactly
  (NUL bytes, non-UTF-8 sequences, and original newlines). }
function ReadFileBytes(const APath: string): TBytes;

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
  beforehand. The Lakon/WASI lane writes its in-memory filesystem, which has
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

implementation

uses
  TextEncoding;

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
const
  DEVICE_PATH_PREFIX = '\\?\';
  DEVICE_UNC_PATH_PREFIX = '\\?\UNC\';
var
  Handle: THandle;
  Buffer: array of WideChar;
  Needed: DWORD;
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
    Needed := GetFinalPathNameByHandleW(Handle, nil, 0, 0);
    if Needed = 0 then
      Exit;
    { The probing call reports the length *including* the terminator and the
      filling one reports it without, so a buffer of that size always holds the
      answer. A second call that asks for more than it fits means the file was
      renamed between the two, and an unknown beats a truncated path. }
    SetLength(Buffer, Needed + 1);
    Needed := GetFinalPathNameByHandleW(Handle, @Buffer[0], Needed, 0);
    if (Needed = 0) or (Needed > DWORD(Length(Buffer) - 1)) then
      Exit;
    SetString(Result, PWideChar(@Buffer[0]), Integer(Needed));
  finally
    CloseHandle(Handle);
  end;
  if Copy(Result, 1, Length(DEVICE_UNC_PATH_PREFIX)) =
     DEVICE_UNC_PATH_PREFIX then
    Result := '\\' + Copy(Result, Length(DEVICE_UNC_PATH_PREFIX) + 1, MaxInt)
  else if Copy(Result, 1, Length(DEVICE_PATH_PREFIX)) = DEVICE_PATH_PREFIX then
    Result := Copy(Result, Length(DEVICE_PATH_PREFIX) + 1, MaxInt);
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
  Written: DWORD;
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
    if MoveFileExW(PWideChar(ATemporaryPath), PWideChar(APath),
         MOVEFILE_REPLACE_EXISTING or MOVEFILE_WRITE_THROUGH) then
      Result := True
    else
      AError := SysErrorMessage(GetLastError);
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

end.
