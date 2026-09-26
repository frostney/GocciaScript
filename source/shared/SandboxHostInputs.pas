unit SandboxHostInputs;

{ Host files copied into a sandbox virtual filesystem, and the host-applied
  write-back of what a run changed in them.

  A copy is an import baseline, never a mount: the guest reads and writes its
  own filesystem, and nothing it does reaches the host. Each copy is recorded
  as an origin, the sandbox path it landed at and the host path it came from,
  so that the host can decide after the run to keep what the run produced
  ([ADR 0119](../../docs/adr/0119-host-applied-sandbox-write-back.md)). An
  origin copied read-write may be written back; a read-only one never is.

  Symbolic links are refused wherever a copy meets one
  ([ADR 0071](../../docs/adr/0071-reject-symlinks-in-sandbox-seed-imports.md)): a
  link inside a copied directory would otherwise let the copy read, and the
  write-back write, outside the tree the user named. }

{$I Shared.inc}

interface

uses
  Classes,
  SysUtils,

  FileUtils,
  SandboxVirtualFileSystem;

type
  { A copy the host could not make: a missing path, a symbolic link, or a
    sandbox target that does not fit. }
  ESandboxHostInputError = class(Exception);

  { Where a sandbox path came from on the host. }
  TSandboxHostOrigin = record
    SandboxPath: string;
    HostPath: string;
    IsDirectory: Boolean;
    { Copied with --copy-rw: changed files under it may be written back. }
    ReadWrite: Boolean;
    { The directory write-back writes under — the input itself, or a file
      input's directory — canonical, and which directory it was, both as of
      the copy. A write goes through this recorded directory, never through
      the input's path looked up again, so a directory swapped for a link
      during the run cannot redirect it. }
    WriteRoot: string;
    WriteRootIdentity: THostDirectoryIdentity;
  end;
  TSandboxHostOriginArray = array of TSandboxHostOrigin;

  TSandboxWriteBackAction = (
    { Written to HostPath. }
    swaWrite,
    { The file came from, or lies in, an input copied read-only. }
    swaSkipReadOnly,
    { Nothing was copied to this sandbox path, so it has no host path. }
    swaSkipNoOrigin,
    { The host path is a symbolic link, or resolves outside its input. }
    swaSkipOutside
  );

  TSandboxWriteBackItem = record
    SandboxPath: string;
    HostPath: string;
    Action: TSandboxWriteBackAction;
    { The origin's index, and the path under its WriteRoot. }
    OriginIndex: Integer;
    RelativePath: string;
  end;
  TSandboxWriteBackPlan = array of TSandboxWriteBackItem;

  { A host file written after the run, such as the diff file, pinned before
    it: its nearest existing directory, canonical and as the directory it
    was. The write goes through that directory and refuses one moved or
    swapped for a link during the run, and a link at the file itself. }
  TSandboxHostOutputFile = record
    Path: string;
    RootPath: string;
    Root: string;
    RootIdentity: THostDirectoryIdentity;
    RelativePath: string;
    class function Pin(const APath: string): TSandboxHostOutputFile; static;
    function Write(const ABytes: TBytes; out AError: string): Boolean;
  end;

  TSandboxHostInputs = class
  private
    FFs: TSandboxVirtualFileSystem;
    FOrigins: TSandboxHostOriginArray;
    procedure RecordOrigin(const AHostPath, ASandboxPath: string;
      const AIsDirectory, AReadWrite: Boolean);
    procedure CopyFile(const AHostPath, ASandboxPath: string);
    procedure CopyDirectoryContents(const AHostDirectory,
      ASandboxDirectory: string);
    function BestOrigin(const ASandboxPath: string): Integer;
    function OriginRootUnchanged(const AOrigin: TSandboxHostOrigin): Boolean;
  public
    constructor Create(const AFs: TSandboxVirtualFileSystem);

    { Copies the host file or directory AHostPath into the sandbox and returns
      the sandbox path it landed at. A directory's contents are copied into
      ASandboxPath. A file is copied to ASandboxPath, or inside it when
      ASandboxPath is '/', ends in '/', or names an existing directory. }
    function CopyIn(const AHostPath, ASandboxPath: string;
      const AReadWrite: Boolean): string;

    { The sandbox path at which the host file AHostPath was copied, when it is
      one of the inputs or lies inside a copied directory. Paths are compared
      canonically, so a spelling through `..` or a symlinked parent matches.
      The longest match wins. }
    function SandboxPathOfHostFile(const AHostPath: string;
      out ASandboxPath: string): Boolean;

    { The origin that supplied ASandboxPath and the host path it maps to. The
      longest matching origin wins, so an input copied inside another resolves
      against the one that actually supplied the path. False when no origin
      covers it. }
    function HostPathForSandboxPath(const ASandboxPath: string;
      out AOrigin: TSandboxHostOrigin; out AHostPath: string): Boolean;

    function HasReadWriteInput: Boolean;

    { Every file of the current filesystem that differs from ABaseline (or is
      new), sorted by sandbox path, with what write-back does with it. The
      plan is complete before anything is written, so a path that cannot be
      written is reported rather than discovered halfway. Deletions are never
      part of it. }
    function PlanWriteBack(
      const ABaseline: TSandboxVirtualFileSystem): TSandboxWriteBackPlan;

    { Carries out APlan's writes, each an atomic replacement, and appends one
      report line per file and a summary to AReport. Before writing anything
      it checks that every read-write input it writes under is still the
      directory that was copied; when one is not, nothing is written. Returns
      False when a write failed or was refused. }
    function ApplyWriteBack(const APlan: TSandboxWriteBackPlan;
      const AReport: TStrings): Boolean;

    property Origins: TSandboxHostOriginArray read FOrigins;
  end;

const
  OUTPUT_TEMPORARY_SUFFIX = '.goccia-output';
  WRITE_BACK_REPORT_PREFIX = 'write-back: ';
  WRITE_BACK_TEMPORARY_SUFFIX = '.goccia-write-back';

implementation

const
  SANDBOX_SEPARATOR = '/';

function EnsureSandboxAbsolute(const APath: string): string;
var
  Path: string;
begin
  Path := NormalizeSandboxPathSeparators(APath);
  if Path = '' then
    Exit(SANDBOX_SEPARATOR);
  if Path[1] = SANDBOX_SEPARATOR then
    Result := Path
  else
    Result := SANDBOX_SEPARATOR + Path;
end;

function SandboxPathHasTrailingSeparator(const APath: string): Boolean;
var
  Path: string;
begin
  Path := NormalizeSandboxPathSeparators(APath);
  Result := (Path <> '') and (Path[Length(Path)] = SANDBOX_SEPARATOR);
end;

function SandboxJoinPath(const ABase, AName: string): string;
begin
  if ABase = SANDBOX_SEPARATOR then
    Result := SANDBOX_SEPARATOR + AName
  else
    Result := ABase + SANDBOX_SEPARATOR + AName;
end;

function SandboxParentPath(const APath: string): string;
var
  SlashIndex: Integer;
begin
  Result := SANDBOX_SEPARATOR;
  SlashIndex := Length(APath);
  while (SlashIndex > 1) and (APath[SlashIndex] <> SANDBOX_SEPARATOR) do
    Dec(SlashIndex);
  if SlashIndex > 1 then
    Result := Copy(APath, 1, SlashIndex - 1);
end;

{ True when APath is ARoot or lies under it. Both are absolute host paths
  without a trailing separator. }
function HostPathIsWithin(const APath, ARoot: string): Boolean;
var
  Root: string;
begin
  {$IF DEFINED(DARWIN) OR DEFINED(MSWINDOWS)}
  if SameText(APath, ARoot) then
    Exit(True);
  Root := IncludeTrailingPathDelimiter(ARoot);
  Result := SameText(Copy(APath, 1, Length(Root)), Root);
  {$ELSE}
  if APath = ARoot then
    Exit(True);
  Root := IncludeTrailingPathDelimiter(ARoot);
  Result := Copy(APath, 1, Length(Root)) = Root;
  {$ENDIF}
end;

{ APath canonical when the host can say, else its lexical expansion. }
function CanonicalOrExpanded(const APath: string): string;
begin
  Result := CanonicalHostPath(APath);
  if Result = '' then
    Result := ExpandHostFileName(APath);
  Result := ExcludeTrailingPathDelimiter(Result);
end;

function ReadHostBytes(const APath: string): TBytes;
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(APath, fmOpenRead or fmShareDenyWrite);
  try
    SetLength(Result, Stream.Size);
    if Stream.Size > 0 then
      Stream.ReadBuffer(Result[0], Stream.Size);
  finally
    Stream.Free;
  end;
end;

function SameBytes(const ALeft, ARight: TBytes): Boolean;
begin
  Result := (Length(ALeft) = Length(ARight)) and
    ((Length(ALeft) = 0) or
     CompareMem(@ALeft[0], @ARight[0], Length(ALeft)));
end;

{ TSandboxHostOutputFile }

class function TSandboxHostOutputFile.Pin(
  const APath: string): TSandboxHostOutputFile;
begin
  Result := Default(TSandboxHostOutputFile);
  Result.Path := ExcludeTrailingPathDelimiter(ExpandHostFileName(APath));
  Result.RootPath := ExcludeTrailingPathDelimiter(ExtractFileDir(Result.Path));
  while (Result.RootPath <> '') and not HostDirectoryExists(Result.RootPath) and
     (ExtractFileDir(Result.RootPath) <> Result.RootPath) do
    Result.RootPath := ExcludeTrailingPathDelimiter(ExtractFileDir(
      Result.RootPath));
  Result.Root := CanonicalOrExpanded(Result.RootPath);
  TryHostDirectoryIdentity(Result.Root, Result.RootIdentity);
  Result.RelativePath := Copy(Result.Path,
    Length(IncludeTrailingPathDelimiter(Result.RootPath)) + 1, MaxInt);
end;

function TSandboxHostOutputFile.Write(const ABytes: TBytes;
  out AError: string): Boolean;
begin
  if CanonicalOrExpanded(RootPath) <> Root then
  begin
    AError := RootPath + ' was replaced during the run';
    Exit(False);
  end;
  Result := ReplaceHostFileBeneath(Root, RootIdentity, RelativePath,
    OUTPUT_TEMPORARY_SUFFIX, ABytes, AError);
end;

{ TSandboxHostInputs }

constructor TSandboxHostInputs.Create(const AFs: TSandboxVirtualFileSystem);
begin
  inherited Create;
  FFs := AFs;
  FOrigins := nil;
end;

{ The directory write-back writes an origin's files under, as a path: the
  input itself, or a file input's directory. }
function OriginRootPath(const AOrigin: TSandboxHostOrigin): string;
begin
  if AOrigin.IsDirectory then
    Result := AOrigin.HostPath
  else
    Result := ExcludeTrailingPathDelimiter(ExtractFileDir(AOrigin.HostPath));
end;

procedure TSandboxHostInputs.RecordOrigin(const AHostPath,
  ASandboxPath: string; const AIsDirectory, AReadWrite: Boolean);
var
  Index: Integer;
begin
  Index := Length(FOrigins);
  SetLength(FOrigins, Index + 1);
  FOrigins[Index].SandboxPath := ASandboxPath;
  FOrigins[Index].HostPath := ExcludeTrailingPathDelimiter(AHostPath);
  FOrigins[Index].IsDirectory := AIsDirectory;
  FOrigins[Index].ReadWrite := AReadWrite;
  FOrigins[Index].WriteRoot := CanonicalOrExpanded(OriginRootPath(
    FOrigins[Index]));
  TryHostDirectoryIdentity(FOrigins[Index].WriteRoot,
    FOrigins[Index].WriteRootIdentity);
end;

{ The input's directory still resolves to the one recorded at the copy, and
  is still that directory. }
function TSandboxHostInputs.OriginRootUnchanged(
  const AOrigin: TSandboxHostOrigin): Boolean;
var
  Identity: THostDirectoryIdentity;
begin
  if CanonicalOrExpanded(OriginRootPath(AOrigin)) <> AOrigin.WriteRoot then
    Exit(False);
  if not TryHostDirectoryIdentity(AOrigin.WriteRoot, Identity) then
    Exit(False);
  Result := (not AOrigin.WriteRootIdentity.Known) or
    ((Identity.Device = AOrigin.WriteRootIdentity.Device) and
     (Identity.Inode = AOrigin.WriteRootIdentity.Inode));
end;

procedure TSandboxHostInputs.CopyFile(const AHostPath, ASandboxPath: string);
begin
  FFs.MakeDirectory(SandboxParentPath(ASandboxPath), True);
  FFs.WriteAllBytes(ASandboxPath, ReadHostBytes(AHostPath));
end;

procedure TSandboxHostInputs.CopyDirectoryContents(const AHostDirectory,
  ASandboxDirectory: string);
var
  SearchRec: TSearchRec;
  HostChild: string;
begin
  FFs.MakeDirectory(ASandboxDirectory, True);
  if FindFirst(IncludeTrailingPathDelimiter(AHostDirectory) + '*',
     faAnyFile, SearchRec) <> 0 then
    Exit;
  try
    repeat
      if (SearchRec.Name = '.') or (SearchRec.Name = '..') then
        Continue;
      HostChild := IncludeTrailingPathDelimiter(AHostDirectory) +
        SearchRec.Name;
      if HostPathIsSymlink(HostChild) then
        raise ESandboxHostInputError.Create(
          'Copy path is a symlink (not supported): ' + HostChild);
      if (SearchRec.Attr and faDirectory) <> 0 then
        CopyDirectoryContents(HostChild,
          SandboxJoinPath(ASandboxDirectory, SearchRec.Name))
      else
        CopyFile(HostChild, SandboxJoinPath(ASandboxDirectory,
          SearchRec.Name));
    until FindNext(SearchRec) <> 0;
  finally
    FindClose(SearchRec);
  end;
end;

function TSandboxHostInputs.CopyIn(const AHostPath, ASandboxPath: string;
  const AReadWrite: Boolean): string;
var
  HostPath: string;
begin
  { Strip any trailing separator before the symlink check: POSIX lstat()
    follows a final symlink when the path ends in '/', so without this a
    symlinked leaf would be rejected as `linkdir` but dereferenced as
    `linkdir/`. }
  HostPath := ExcludeTrailingPathDelimiter(ExpandHostFileName(AHostPath));
  if HostPathIsSymlink(HostPath) then
    raise ESandboxHostInputError.Create(
      'Copy path is a symlink (not supported): ' + HostPath);
  if not HostFileExists(HostPath) and not HostDirectoryExists(HostPath) then
    raise ESandboxHostInputError.Create('Copy path does not exist: ' +
      HostPath);

  Result := FFs.Normalize(EnsureSandboxAbsolute(ASandboxPath));
  if HostDirectoryExists(HostPath) then
  begin
    if FFs.IsFile(Result) then
      raise ESandboxHostInputError.CreateFmt(
        'Cannot copy directory %s to %s: the sandbox already has a file ' +
        'there', [HostPath, Result]);
    CopyDirectoryContents(HostPath, Result);
    RecordOrigin(HostPath, Result, True, AReadWrite);
    Exit;
  end;

  if (Result = SANDBOX_SEPARATOR) or
     SandboxPathHasTrailingSeparator(ASandboxPath) or
     FFs.IsDirectory(Result) then
    Result := FFs.Normalize(SandboxJoinPath(Result,
      ExtractFileName(HostPath)));
  CopyFile(HostPath, Result);
  RecordOrigin(HostPath, Result, False, AReadWrite);
end;

function TSandboxHostInputs.SandboxPathOfHostFile(const AHostPath: string;
  out ASandboxPath: string): Boolean;
var
  FilePath, OriginPath, Relative: string;
  I, Best, BestLength: Integer;
begin
  Result := False;
  ASandboxPath := '';
  FilePath := CanonicalOrExpanded(AHostPath);
  Best := -1;
  BestLength := -1;
  for I := 0 to High(FOrigins) do
  begin
    OriginPath := CanonicalOrExpanded(FOrigins[I].HostPath);
    if FOrigins[I].IsDirectory then
    begin
      if (FilePath = OriginPath) or not HostPathIsWithin(FilePath,
         OriginPath) then
        Continue;
    end
    else if not HostPathIsWithin(FilePath, OriginPath) or
      (Length(FilePath) <> Length(OriginPath)) then
      Continue;
    if Length(OriginPath) > BestLength then
    begin
      Best := I;
      BestLength := Length(OriginPath);
    end;
  end;
  if Best < 0 then
    Exit;

  if not FOrigins[Best].IsDirectory then
  begin
    ASandboxPath := FOrigins[Best].SandboxPath;
    Exit(True);
  end;
  Relative := Copy(FilePath, BestLength + 2, MaxInt);
  Relative := StringReplace(Relative, PathDelim, SANDBOX_SEPARATOR,
    [rfReplaceAll]);
  ASandboxPath := FFs.Normalize(SandboxJoinPath(FOrigins[Best].SandboxPath,
    Relative));
  Result := True;
end;

function TSandboxHostInputs.BestOrigin(const ASandboxPath: string): Integer;
var
  I: Integer;
  Origin: TSandboxHostOrigin;
begin
  Result := -1;
  for I := 0 to High(FOrigins) do
  begin
    Origin := FOrigins[I];
    if not Origin.IsDirectory then
    begin
      if Origin.SandboxPath = ASandboxPath then
        if (Result < 0) or (Length(Origin.SandboxPath) >=
           Length(FOrigins[Result].SandboxPath)) then
          Result := I;
      Continue;
    end;

    if (ASandboxPath = Origin.SandboxPath) or
       ((Copy(ASandboxPath, 1, Length(Origin.SandboxPath)) =
         Origin.SandboxPath) and
        ((Origin.SandboxPath = SANDBOX_SEPARATOR) or
         (ASandboxPath[Length(Origin.SandboxPath) + 1] =
          SANDBOX_SEPARATOR))) then
      if (Result < 0) or (Length(Origin.SandboxPath) >
         Length(FOrigins[Result].SandboxPath)) then
        Result := I;
  end;
end;

function TSandboxHostInputs.HostPathForSandboxPath(
  const ASandboxPath: string; out AOrigin: TSandboxHostOrigin;
  out AHostPath: string): Boolean;
var
  Index: Integer;
  Relative: string;
begin
  Result := False;
  AHostPath := '';
  AOrigin := Default(TSandboxHostOrigin);
  Index := BestOrigin(ASandboxPath);
  if Index < 0 then
    Exit;
  AOrigin := FOrigins[Index];
  if not AOrigin.IsDirectory then
  begin
    AHostPath := AOrigin.HostPath;
    Exit(True);
  end;

  Relative := Copy(ASandboxPath, Length(AOrigin.SandboxPath) + 1, MaxInt);
  while (Relative <> '') and (Relative[1] = SANDBOX_SEPARATOR) do
    Delete(Relative, 1, 1);
  if Relative = '' then
    Exit;

  AHostPath := IncludeTrailingPathDelimiter(AOrigin.HostPath) +
    StringReplace(Relative, SANDBOX_SEPARATOR, PathDelim, [rfReplaceAll]);
  { The sandbox normalizes its own paths, so `Relative` cannot climb out on
    its own. Checking the result anyway costs nothing and means the guarantee
    does not depend on a normalizer in another unit. }
  Result := HostPathIsWithin(ExpandHostFileName(AHostPath), AOrigin.HostPath);
  if not Result then
    AHostPath := '';
end;

function TSandboxHostInputs.HasReadWriteInput: Boolean;
var
  I: Integer;
begin
  for I := 0 to High(FOrigins) do
    if FOrigins[I].ReadWrite then
      Exit(True);
  Result := False;
end;

{ True when writing AHostPath stays inside the recorded root once symbolic
  links are resolved: the target itself is not a link, and its nearest
  existing ancestor resolves inside the root. The write repeats the check
  component by component (ReplaceHostFileBeneath); this one only lets the
  plan report such a file instead of failing on it. }
function WriteStaysInside(const AHostPath, AWriteRoot: string): Boolean;
var
  Ancestor, CanonicalAncestor: string;
begin
  if HostPathIsSymlink(ExcludeTrailingPathDelimiter(AHostPath)) then
    Exit(False);
  Ancestor := ExcludeTrailingPathDelimiter(ExtractFileDir(AHostPath));
  while (Ancestor <> '') and not HostDirectoryExists(Ancestor) do
  begin
    if ExtractFileDir(Ancestor) = Ancestor then
      Break;
    Ancestor := ExcludeTrailingPathDelimiter(ExtractFileDir(Ancestor));
  end;
  CanonicalAncestor := CanonicalHostPath(Ancestor);
  if CanonicalAncestor = '' then
    { The host cannot canonicalize (Lakon/WASI, no links); the lexical check
      in HostPathForSandboxPath already holds. }
    Exit(True);
  Result := HostPathIsWithin(ExcludeTrailingPathDelimiter(CanonicalAncestor),
    AWriteRoot) or HostPathIsWithin(AWriteRoot,
    ExcludeTrailingPathDelimiter(CanonicalAncestor));
end;

function TSandboxHostInputs.PlanWriteBack(
  const ABaseline: TSandboxVirtualFileSystem): TSandboxWriteBackPlan;
var
  Paths: TStringList;
  Origin: TSandboxHostOrigin;
  HostPath: string;
  I, Count: Integer;

  procedure Collect(const ADirectory: string);
  var
    Index: Integer;
    Listing: TSandboxFsStatArray;
  begin
    Listing := FFs.SnapshotList(ADirectory);
    for Index := 0 to High(Listing) do
      if Listing[Index].Kind = nkFile then
        Paths.Add(Listing[Index].Path)
      else
        Collect(Listing[Index].Path);
  end;

begin
  Result := nil;
  Paths := TStringList.Create;
  try
    Collect(SANDBOX_SEPARATOR);
    Paths.Sort;
    SetLength(Result, Paths.Count);
    Count := 0;
    for I := 0 to Paths.Count - 1 do
    begin
      if Assigned(ABaseline) and ABaseline.IsFile(Paths[I]) and
         SameBytes(ABaseline.SnapshotReadAllBytes(Paths[I]),
           FFs.SnapshotReadAllBytes(Paths[I])) then
        Continue;

      Result[Count].SandboxPath := Paths[I];
      Result[Count].HostPath := '';
      Result[Count].OriginIndex := -1;
      Result[Count].RelativePath := '';
      if not HostPathForSandboxPath(Paths[I], Origin, HostPath) then
        Result[Count].Action := swaSkipNoOrigin
      else
      begin
        Result[Count].HostPath := HostPath;
        Result[Count].OriginIndex := BestOrigin(Paths[I]);
        Result[Count].RelativePath := Copy(HostPath,
          Length(IncludeTrailingPathDelimiter(OriginRootPath(Origin))) + 1,
          MaxInt);
        if not Origin.ReadWrite then
          Result[Count].Action := swaSkipReadOnly
        else if not WriteStaysInside(HostPath, Origin.WriteRoot) then
          Result[Count].Action := swaSkipOutside
        else
          Result[Count].Action := swaWrite;
      end;
      Inc(Count);
    end;
    SetLength(Result, Count);
  finally
    Paths.Free;
  end;
end;

function TSandboxHostInputs.ApplyWriteBack(const APlan: TSandboxWriteBackPlan;
  const AReport: TStrings): Boolean;
var
  I, Written, Skipped: Integer;
  ErrorMessage: string;
  Ok, Replaced: Boolean;
  Origin: TSandboxHostOrigin;
begin
  Result := True;
  Written := 0;
  Skipped := 0;
  for I := 0 to High(APlan) do
    case APlan[I].Action of
      swaSkipNoOrigin:
      begin
        AReport.Add(WRITE_BACK_REPORT_PREFIX + APlan[I].SandboxPath +
          ' was not copied from the host, skipped');
        Inc(Skipped);
      end;
      swaSkipReadOnly:
      begin
        AReport.Add(WRITE_BACK_REPORT_PREFIX + APlan[I].SandboxPath +
          ' was copied read-only, skipped (use --copy-rw to write it back)');
        Inc(Skipped);
      end;
      swaSkipOutside:
      begin
        AReport.Add(WRITE_BACK_REPORT_PREFIX + APlan[I].HostPath +
          ' is a symlink or resolves outside its input, skipped');
        Inc(Skipped);
      end;
    end;

  { Every input written under must still be the directory that was copied.
    One that is not has been moved or swapped for a link while the run was
    going on, and a half-applied write-back is worse than none, so nothing
    is written. }
  Replaced := False;
  for I := 0 to High(APlan) do
    if (APlan[I].Action in [swaWrite, swaSkipOutside]) and
       (APlan[I].OriginIndex >= 0) and
       FOrigins[APlan[I].OriginIndex].ReadWrite and
       not OriginRootUnchanged(FOrigins[APlan[I].OriginIndex]) then
    begin
      Origin := FOrigins[APlan[I].OriginIndex];
      if AReport.IndexOf(WRITE_BACK_REPORT_PREFIX + OriginRootPath(Origin) +
         ' was replaced during the run; nothing written') < 0 then
        AReport.Add(WRITE_BACK_REPORT_PREFIX + OriginRootPath(Origin) +
          ' was replaced during the run; nothing written');
      Replaced := True;
    end;
  if Replaced then
  begin
    for I := 0 to High(APlan) do
      if APlan[I].Action = swaWrite then
        Inc(Skipped);
    AReport.Add(Format('%s%d file(s) written, %d skipped',
      [WRITE_BACK_REPORT_PREFIX, 0, Skipped]));
    Exit(False);
  end;

  for I := 0 to High(APlan) do
  begin
    if APlan[I].Action <> swaWrite then
      Continue;
    Origin := FOrigins[APlan[I].OriginIndex];
    try
      Ok := ReplaceHostFileBeneath(Origin.WriteRoot, Origin.WriteRootIdentity,
        APlan[I].RelativePath, WRITE_BACK_TEMPORARY_SUFFIX,
        FFs.SnapshotReadAllBytes(APlan[I].SandboxPath), ErrorMessage);
    except
      on E: Exception do
      begin
        ErrorMessage := E.Message;
        Ok := False;
      end;
    end;
    if Ok then
    begin
      AReport.Add(WRITE_BACK_REPORT_PREFIX + APlan[I].HostPath);
      Inc(Written);
    end
    else
    begin
      AReport.Add(WRITE_BACK_REPORT_PREFIX + APlan[I].HostPath + ': ' +
        ErrorMessage);
      Inc(Skipped);
      Result := False;
    end;
  end;

  AReport.Add(Format('%s%d file(s) written, %d skipped',
    [WRITE_BACK_REPORT_PREFIX, Written, Skipped]));
end;

end.
