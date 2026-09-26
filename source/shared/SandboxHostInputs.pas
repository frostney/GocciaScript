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
  end;
  TSandboxWriteBackPlan = array of TSandboxWriteBackItem;

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
      report line per file and a summary to AReport. Returns False when a
      write failed. }
    function ApplyWriteBack(const APlan: TSandboxWriteBackPlan;
      const AReport: TStrings): Boolean;

    property Origins: TSandboxHostOriginArray read FOrigins;
  end;

const
  WRITE_BACK_REPORT_PREFIX = 'write-back: ';

implementation

uses
  FileUtils;

const
  SANDBOX_SEPARATOR = '/';
  WRITE_BACK_TEMPORARY_SUFFIX = '.goccia-write-back';

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

{ TSandboxHostInputs }

constructor TSandboxHostInputs.Create(const AFs: TSandboxVirtualFileSystem);
begin
  inherited Create;
  FFs := AFs;
  FOrigins := nil;
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

{ True when writing AHostPath stays inside AOriginRoot once symbolic links
  are resolved: the target itself is not a link, and its nearest existing
  ancestor resolves inside the root. A link planted after the copy, at the
  file or at a directory on its way, would otherwise redirect the write. }
function WriteStaysInside(const AHostPath, AOriginRoot: string): Boolean;
var
  Ancestor, CanonicalRoot, CanonicalAncestor: string;
begin
  if HostPathIsSymlink(ExcludeTrailingPathDelimiter(AHostPath)) then
    Exit(False);
  CanonicalRoot := CanonicalOrExpanded(AOriginRoot);
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
    CanonicalRoot) or HostPathIsWithin(CanonicalRoot,
    ExcludeTrailingPathDelimiter(CanonicalAncestor));
end;

function TSandboxHostInputs.PlanWriteBack(
  const ABaseline: TSandboxVirtualFileSystem): TSandboxWriteBackPlan;
var
  Paths: TStringList;
  Origin: TSandboxHostOrigin;
  HostPath, OriginRoot: string;
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
      if not HostPathForSandboxPath(Paths[I], Origin, HostPath) then
        Result[Count].Action := swaSkipNoOrigin
      else
      begin
        Result[Count].HostPath := HostPath;
        if Origin.IsDirectory then
          OriginRoot := Origin.HostPath
        else
          OriginRoot := ExtractFileDir(Origin.HostPath);
        if not Origin.ReadWrite then
          Result[Count].Action := swaSkipReadOnly
        else if not WriteStaysInside(HostPath, OriginRoot) then
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
  Ok: Boolean;
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

  for I := 0 to High(APlan) do
  begin
    if APlan[I].Action <> swaWrite then
      Continue;
    try
      ForceDirectories(ExtractFilePath(APlan[I].HostPath));
      Ok := ReplaceHostFile(APlan[I].HostPath,
        APlan[I].HostPath + WRITE_BACK_TEMPORARY_SUFFIX,
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
