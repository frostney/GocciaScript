{ SandboxVirtualFileSystem - embeddable in-memory virtual filesystem.

  A self-contained POSIX-flavoured namespace that never touches the
  host disk. Designed for embedding in native FreePascal applications
  that want to run isolated commands against a private filesystem:

    - case-sensitive, '/'-separated paths; '..' clamps at root
      (chroot-jail semantics - there is no way to address the host)
    - files are TBytes with capacity doubling; directories are
      sorted child maps (deterministic, byte-ordered listings)
    - streaming handles (TSandboxFsFile) with seek / truncate / append
    - a byte quota so a sandboxed workload cannot balloon host memory
    - Fork - deep-clone the whole tree for run-and-discard execution
      (copy-on-write is the optimisation path once the semantics are
      proven; a deep copy keeps the baseline semantics simple)

  Deliberately unsupported: symlinks, permissions,
  hard links, thread safety, POSIX unlink-while-open (deleting an
  open file raises ESandboxFsBusy instead). }

unit SandboxVirtualFileSystem;

{$I Shared.inc}

interface

uses
  Classes,
  SysUtils;

type
  TStringArray = array of string;

  TSandboxFsNodeKind = (nkFile, nkDirectory);

  { Optional, non-owned clock used by the VFS.  Embedders can inject a
    deterministic clock; nil selects the host wall clock. }
  TSandboxFsClock = class
  public
    function Now: TDateTime; virtual; abstract;
  end;

  TSandboxFsStat = record
    Path: string;
    Name: string;
    Kind: TSandboxFsNodeKind;
    Size: Int64;
    AccessedAt: TDateTime;
    ModifiedAt: TDateTime;
    ChangedAt: TDateTime;
    BirthTime: TDateTime;
  end;

  TSandboxFsStatArray = array of TSandboxFsStat;

  TSandboxFsOpenMode = (omRead, omWrite, omReadWrite, omAppend);

  ESandboxFsError = class(Exception);
  ESandboxFsInvalidPath = class(ESandboxFsError);
  ESandboxFsNotFound = class(ESandboxFsError);
  ESandboxFsExists = class(ESandboxFsError);
  ESandboxFsNotADirectory = class(ESandboxFsError);
  ESandboxFsIsADirectory = class(ESandboxFsError);
  ESandboxFsNotEmpty = class(ESandboxFsError);
  ESandboxFsBusy = class(ESandboxFsError);
  ESandboxFsQuotaExceeded = class(ESandboxFsError);

  TSandboxFsNode = class
  private
    FName: string;
    FKind: TSandboxFsNodeKind;
    FParent: TSandboxFsNode;
    FAccessedAt: TDateTime;
    FModifiedAt: TDateTime;
    FChangedAt: TDateTime;
    FBirthTime: TDateTime;
    FData: TBytes;
    FSize: Int64;
    FOpenCount: Integer;
    FChildren: TStringList;
    function FullPath: string;
    function FindChild(const AName: string): TSandboxFsNode;
    procedure AttachChild(const AChild: TSandboxFsNode;
      const ATime: TDateTime);
    procedure DetachChild(const AChild: TSandboxFsNode;
      const ATime: TDateTime);
  public
    constructor Create(const AName: string; const AKind: TSandboxFsNodeKind;
      const ATime: TDateTime);
    destructor Destroy; override;
  end;

  TSandboxFsFile = class;

  TSandboxVirtualFileSystem = class
  private
    FRoot: TSandboxFsNode;
    FQuotaBytes: Int64;
    FUsedBytes: Int64;
    FNodeCount: Integer;
    FClock: TSandboxFsClock;
    function CurrentTime: TDateTime;
    function LookupNode(const ACanonicalPath: string): TSandboxFsNode;
    function RequireNode(const APath: string): TSandboxFsNode;
    function RequireDirectory(const APath: string): TSandboxFsNode;
    function RequireFile(const APath: string): TSandboxFsNode;
    function ResolveParent(const ACanonicalPath: string;
      out ALeafName: string): TSandboxFsNode;
    function StatOf(const ANode: TSandboxFsNode): TSandboxFsStat;
    function CloneNodePreservingMetadata(
      const ASource: TSandboxFsNode): TSandboxFsNode;
    function CloneNodeForCopy(const ASource: TSandboxFsNode;
      const ATime: TDateTime): TSandboxFsNode;
    function SubtreeBytes(const ANode: TSandboxFsNode): Int64;
    function SubtreeNodes(const ANode: TSandboxFsNode): Integer;
    function SubtreeHasOpenHandles(const ANode: TSandboxFsNode): Boolean;
    function IsDescendantOf(const ANode, AAncestor: TSandboxFsNode): Boolean;
    procedure EnsureGrowth(const ADelta: Int64);
    procedure GrowFile(const ANode: TSandboxFsNode; const ANewSize: Int64);
    procedure ValidateLeafName(const AName: string);
    function CreateFileNode(const ACanonicalPath: string): TSandboxFsNode;
  public
    constructor Create(const AQuotaBytes: Int64 = 0;
      const AClock: TSandboxFsClock = nil);
    destructor Destroy; override;

    { Canonicalize APath against ABase ('/'-rooted). Collapses '.',
      resolves '..' with clamping at root, rejects NUL bytes. }
    function Normalize(const APath: string;
      const ABase: string = '/'): string;

    function Exists(const APath: string): Boolean;
    function IsFile(const APath: string): Boolean;
    function IsDirectory(const APath: string): Boolean;
    function Stat(const APath: string): TSandboxFsStat;
    function List(const APath: string): TSandboxFsStatArray;

    procedure MakeDirectory(const APath: string;
      const ARecursive: Boolean = False);
    procedure DeletePath(const APath: string;
      const ARecursive: Boolean = False);
    procedure MovePath(const ASource, ADestination: string);
    procedure CopyPath(const ASource, ADestination: string;
      const ARecursive: Boolean = False);
    procedure Touch(const APath: string);

    procedure WriteAllBytes(const APath: string; const AData: TBytes);
    procedure WriteAllText(const APath: string; const AText: string);
    procedure AppendAllText(const APath: string; const AText: string);
    function ReadAllBytes(const APath: string): TBytes;
    function ReadAllText(const APath: string): string;

    { Non-observing reads for baselines, diffing, and diagnostics. }
    function SnapshotList(const APath: string): TSandboxFsStatArray;
    function SnapshotReadAllBytes(const APath: string): TBytes;
    function SnapshotReadAllText(const APath: string): string;

    function Open(const APath: string;
      const AMode: TSandboxFsOpenMode): TSandboxFsFile;

    { Deep-clone the entire tree into an independent filesystem.
      Open handles are not carried across; quota settings are. }
    function Fork: TSandboxVirtualFileSystem;

    property QuotaBytes: Int64 read FQuotaBytes write FQuotaBytes;
    property UsedBytes: Int64 read FUsedBytes;
    property NodeCount: Integer read FNodeCount;
  end;

  TSandboxFsFile = class
  private
    FFs: TSandboxVirtualFileSystem;
    FNode: TSandboxFsNode;
    FMode: TSandboxFsOpenMode;
    FPosition: Int64;
    FClosed: Boolean;
    function GetSize: Int64;
    function GetPath: string;
  public
    constructor Create(const AFs: TSandboxVirtualFileSystem; const ANode: TSandboxFsNode;
      const AMode: TSandboxFsOpenMode);
    destructor Destroy; override;

    function Read(var ABuffer; const ACount: Integer): Integer;
    function Write(const ABuffer; const ACount: Integer): Integer;
    function Seek(const AOffset: Int64;
      const AOrigin: TSeekOrigin): Int64;
    procedure Truncate(const ASize: Int64);
    procedure Close;

    function ReadText(const ACount: Integer): string;
    procedure WriteText(const AText: string);

    property Position: Int64 read FPosition;
    property Size: Int64 read GetSize;
    property Path: string read GetPath;
    property Mode: TSandboxFsOpenMode read FMode;
  end;

function SandboxFsKindName(const AKind: TSandboxFsNodeKind): string;
function SandboxHumanBytes(const ABytes: Int64): string;
function SandboxDateTimeToUnixMilliseconds(const AValue: TDateTime): Double;
function NormalizeSandboxPathSeparators(const APath: string): string;

implementation

uses
  TimingUtils;

const
  PathSeparator = '/';
  AlternatePathSeparator = '\';
  MillisecondsPerDay = 24 * 60 * 60 * 1000;

function NormalizeSandboxPathSeparators(const APath: string): string;
begin
  Result := StringReplace(APath, AlternatePathSeparator, PathSeparator,
    [rfReplaceAll]);
end;

function SandboxFsKindName(const AKind: TSandboxFsNodeKind): string;
begin
  if AKind = nkDirectory then
    Result := 'directory'
  else
    Result := 'file';
end;

function SandboxHumanBytes(const ABytes: Int64): string;
const
  Units: array[0..4] of string = ('B', 'KiB', 'MiB', 'GiB', 'TiB');
var
  Value: Double;
  UnitIndex: Integer;
begin
  Value := ABytes;
  UnitIndex := 0;
  while (Value >= 1024) and (UnitIndex < High(Units)) do
  begin
    Value := Value / 1024;
    Inc(UnitIndex);
  end;
  if UnitIndex = 0 then
    Result := Format('%d %s', [ABytes, Units[UnitIndex]])
  else
    Result := Format('%.1f %s', [Value, Units[UnitIndex]]);
end;

function SandboxDateTimeToUnixMilliseconds(const AValue: TDateTime): Double;
begin
  Result := (AValue - EncodeDate(1970, 1, 1)) * MillisecondsPerDay;
end;

function SplitPath(const ACanonicalPath: string): TStringArray;
var
  Count: Integer;
  Start: Integer;
  Index: Integer;
begin
  Result := nil;
  Count := 0;
  Start := 2;
  for Index := 2 to Length(ACanonicalPath) + 1 do
    if (Index > Length(ACanonicalPath)) or
      (ACanonicalPath[Index] = PathSeparator) then
    begin
      if Index > Start then
      begin
        SetLength(Result, Count + 1);
        Result[Count] := Copy(ACanonicalPath, Start, Index - Start);
        Inc(Count);
      end;
      Start := Index + 1;
    end;
end;

{ TSandboxFsNode }

constructor TSandboxFsNode.Create(const AName: string;
  const AKind: TSandboxFsNodeKind; const ATime: TDateTime);
begin
  inherited Create;
  FName := AName;
  FKind := AKind;
  FAccessedAt := ATime;
  FModifiedAt := ATime;
  FChangedAt := ATime;
  FBirthTime := ATime;
  if AKind = nkDirectory then
  begin
    FChildren := TStringList.Create;
    FChildren.Sorted := True;
    FChildren.CaseSensitive := True;
    FChildren.UseLocale := False;
    FChildren.Duplicates := dupError;
  end;
end;

destructor TSandboxFsNode.Destroy;
var
  Index: Integer;
begin
  if Assigned(FChildren) then
  begin
    for Index := 0 to FChildren.Count - 1 do
      FChildren.Objects[Index].Free;
    FChildren.Free;
  end;
  inherited Destroy;
end;

function TSandboxFsNode.FullPath: string;
begin
  if FParent = nil then
    Result := PathSeparator
  else if FParent.FParent = nil then
    Result := PathSeparator + FName
  else
    Result := FParent.FullPath + PathSeparator + FName;
end;

function TSandboxFsNode.FindChild(const AName: string): TSandboxFsNode;
var
  Index: Integer;
begin
  Result := nil;
  if Assigned(FChildren) and FChildren.Find(AName, Index) then
    Result := TSandboxFsNode(FChildren.Objects[Index]);
end;

procedure TSandboxFsNode.AttachChild(const AChild: TSandboxFsNode;
  const ATime: TDateTime);
begin
  FChildren.AddObject(AChild.FName, AChild);
  AChild.FParent := Self;
  FModifiedAt := ATime;
  FChangedAt := ATime;
end;

procedure TSandboxFsNode.DetachChild(const AChild: TSandboxFsNode;
  const ATime: TDateTime);
var
  Index: Integer;
begin
  if FChildren.Find(AChild.FName, Index) then
    FChildren.Delete(Index);
  AChild.FParent := nil;
  FModifiedAt := ATime;
  FChangedAt := ATime;
end;

{ TSandboxVirtualFileSystem }

constructor TSandboxVirtualFileSystem.Create(const AQuotaBytes: Int64;
  const AClock: TSandboxFsClock);
begin
  inherited Create;
  FClock := AClock;
  FRoot := TSandboxFsNode.Create('', nkDirectory, CurrentTime);
  FQuotaBytes := AQuotaBytes;
  FUsedBytes := 0;
  FNodeCount := 0;
end;

function TSandboxVirtualFileSystem.CurrentTime: TDateTime;
var
  EpochNanoseconds: Int64;
  EpochSeconds: Int64;
  WholeDays: Int64;
  SecondsOfDay: Int64;
begin
  if Assigned(FClock) then
    Result := FClock.Now
  else
  begin
    EpochNanoseconds := GetEpochNanoseconds;
    EpochSeconds := EpochNanoseconds div 1000000000;
    WholeDays := EpochSeconds div (24 * 60 * 60);
    SecondsOfDay := EpochSeconds mod (24 * 60 * 60);
    Result := EncodeDate(1970, 1, 1) + WholeDays +
      (Double(SecondsOfDay) / Double(24 * 60 * 60)) +
      (Double(EpochNanoseconds mod 1000000000) /
        (Double(MillisecondsPerDay) * 1000000));
  end;
end;

destructor TSandboxVirtualFileSystem.Destroy;
begin
  FRoot.Free;
  inherited Destroy;
end;

function TSandboxVirtualFileSystem.Normalize(const APath: string;
  const ABase: string): string;
var
  Path: string;
  Base: string;
  Combined: string;
  Segments: TStringList;
  Segment: string;
  Start: Integer;
  Index: Integer;

  procedure PushSegment(const AValue: string);
  begin
    if (AValue = '') or (AValue = '.') then
      Exit;
    if AValue = '..' then
    begin
      if Segments.Count > 0 then
        Segments.Delete(Segments.Count - 1);
      { at root, '..' clamps - the jail has no upstairs }
      Exit;
    end;
    Segments.Add(AValue);
  end;

begin
  Path := NormalizeSandboxPathSeparators(APath);
  Base := NormalizeSandboxPathSeparators(ABase);
  if (Pos(#0, Path) > 0) or (Pos(#0, Base) > 0) then
    raise ESandboxFsInvalidPath.Create('path contains a NUL byte');
  if (Length(Path) > 0) and (Path[1] = PathSeparator) then
    Combined := Path
  else if Path = '' then
    Combined := Base
  else
    Combined := Base + PathSeparator + Path;

  Segments := TStringList.Create;
  try
    Start := 1;
    for Index := 1 to Length(Combined) + 1 do
      if (Index > Length(Combined)) or
        (Combined[Index] = PathSeparator) then
      begin
        Segment := Copy(Combined, Start, Index - Start);
        PushSegment(Segment);
        Start := Index + 1;
      end;

    Result := '';
    for Index := 0 to Segments.Count - 1 do
      Result := Result + PathSeparator + Segments[Index];
    if Result = '' then
      Result := PathSeparator;
  finally
    Segments.Free;
  end;
end;

function TSandboxVirtualFileSystem.LookupNode(
  const ACanonicalPath: string): TSandboxFsNode;
var
  Segments: TStringArray;
  Index: Integer;
begin
  Result := FRoot;
  Segments := SplitPath(ACanonicalPath);
  for Index := 0 to High(Segments) do
  begin
    if Result.FKind <> nkDirectory then
      Exit(nil);
    Result := Result.FindChild(Segments[Index]);
    if Result = nil then
      Exit;
  end;
end;

function TSandboxVirtualFileSystem.RequireNode(const APath: string): TSandboxFsNode;
begin
  Result := LookupNode(Normalize(APath));
  if Result = nil then
    raise ESandboxFsNotFound.CreateFmt('%s: no such file or directory',
      [Normalize(APath)]);
end;

function TSandboxVirtualFileSystem.RequireDirectory(const APath: string): TSandboxFsNode;
begin
  Result := RequireNode(APath);
  if Result.FKind <> nkDirectory then
    raise ESandboxFsNotADirectory.CreateFmt('%s: not a directory',
      [Normalize(APath)]);
end;

function TSandboxVirtualFileSystem.RequireFile(const APath: string): TSandboxFsNode;
begin
  Result := RequireNode(APath);
  if Result.FKind <> nkFile then
    raise ESandboxFsIsADirectory.CreateFmt('%s: is a directory',
      [Normalize(APath)]);
end;

function TSandboxVirtualFileSystem.ResolveParent(const ACanonicalPath: string;
  out ALeafName: string): TSandboxFsNode;
var
  Segments: TStringArray;
  Index: Integer;
begin
  Segments := SplitPath(ACanonicalPath);
  if Length(Segments) = 0 then
    raise ESandboxFsInvalidPath.Create('/: operation not permitted on root');
  ALeafName := Segments[High(Segments)];
  Result := FRoot;
  for Index := 0 to High(Segments) - 1 do
  begin
    if Result.FKind <> nkDirectory then
      raise ESandboxFsNotADirectory.CreateFmt('%s: not a directory',
        [Result.FullPath]);
    Result := Result.FindChild(Segments[Index]);
    if Result = nil then
      raise ESandboxFsNotFound.CreateFmt('%s: no such file or directory',
        [ACanonicalPath]);
  end;
  if Result.FKind <> nkDirectory then
    raise ESandboxFsNotADirectory.CreateFmt('%s: not a directory',
      [Result.FullPath]);
end;

procedure TSandboxVirtualFileSystem.ValidateLeafName(const AName: string);
begin
  if (AName = '') or (AName = '.') or (AName = '..') or
    (Pos(PathSeparator, AName) > 0) or (Pos(#0, AName) > 0) then
    raise ESandboxFsInvalidPath.CreateFmt('%s: invalid name', [AName]);
end;

procedure TSandboxVirtualFileSystem.EnsureGrowth(const ADelta: Int64);
begin
  if (FQuotaBytes > 0) and (ADelta > 0) and
    (FUsedBytes + ADelta > FQuotaBytes) then
    raise ESandboxFsQuotaExceeded.CreateFmt(
      'quota exceeded: %s used + %s requested > %s limit',
      [SandboxHumanBytes(FUsedBytes), SandboxHumanBytes(ADelta),
      SandboxHumanBytes(FQuotaBytes)]);
end;

procedure TSandboxVirtualFileSystem.GrowFile(const ANode: TSandboxFsNode;
  const ANewSize: Int64);
var
  Capacity: Int64;
begin
  EnsureGrowth(ANewSize - ANode.FSize);
  if ANewSize > Length(ANode.FData) then
  begin
    Capacity := Length(ANode.FData);
    if Capacity < 64 then
      Capacity := 64;
    while Capacity < ANewSize do
      Capacity := Capacity * 2;
    SetLength(ANode.FData, Capacity);
  end;
  if ANewSize > ANode.FSize then
    FillChar(ANode.FData[ANode.FSize], ANewSize - ANode.FSize, 0);
  FUsedBytes := FUsedBytes + (ANewSize - ANode.FSize);
  ANode.FSize := ANewSize;
end;

function TSandboxVirtualFileSystem.CreateFileNode(
  const ACanonicalPath: string): TSandboxFsNode;
var
  Parent: TSandboxFsNode;
  LeafName: string;
  Time: TDateTime;
begin
  Parent := ResolveParent(ACanonicalPath, LeafName);
  ValidateLeafName(LeafName);
  Time := CurrentTime;
  Result := TSandboxFsNode.Create(LeafName, nkFile, Time);
  Parent.AttachChild(Result, Time);
  Inc(FNodeCount);
end;

function TSandboxVirtualFileSystem.StatOf(const ANode: TSandboxFsNode): TSandboxFsStat;
begin
  Result.Path := ANode.FullPath;
  Result.Name := ANode.FName;
  Result.Kind := ANode.FKind;
  if ANode.FKind = nkFile then
    Result.Size := ANode.FSize
  else
    Result.Size := ANode.FChildren.Count;
  Result.AccessedAt := ANode.FAccessedAt;
  Result.ModifiedAt := ANode.FModifiedAt;
  Result.ChangedAt := ANode.FChangedAt;
  Result.BirthTime := ANode.FBirthTime;
end;

function TSandboxVirtualFileSystem.Exists(const APath: string): Boolean;
begin
  Result := LookupNode(Normalize(APath)) <> nil;
end;

function TSandboxVirtualFileSystem.IsFile(const APath: string): Boolean;
var
  Node: TSandboxFsNode;
begin
  Node := LookupNode(Normalize(APath));
  Result := Assigned(Node) and (Node.FKind = nkFile);
end;

function TSandboxVirtualFileSystem.IsDirectory(const APath: string): Boolean;
var
  Node: TSandboxFsNode;
begin
  Node := LookupNode(Normalize(APath));
  Result := Assigned(Node) and (Node.FKind = nkDirectory);
end;

function TSandboxVirtualFileSystem.Stat(const APath: string): TSandboxFsStat;
begin
  Result := StatOf(RequireNode(APath));
end;

function TSandboxVirtualFileSystem.List(const APath: string): TSandboxFsStatArray;
var
  Node: TSandboxFsNode;
  Index: Integer;
begin
  Result := nil;
  Node := RequireDirectory(APath);
  Node.FAccessedAt := CurrentTime;
  SetLength(Result, Node.FChildren.Count);
  for Index := 0 to Node.FChildren.Count - 1 do
    Result[Index] := StatOf(TSandboxFsNode(Node.FChildren.Objects[Index]));
end;

procedure TSandboxVirtualFileSystem.MakeDirectory(const APath: string;
  const ARecursive: Boolean);
var
  Canonical: string;
  Segments: TStringArray;
  Current: TSandboxFsNode;
  Next: TSandboxFsNode;
  Index: Integer;
  Time: TDateTime;
begin
  Canonical := Normalize(APath);
  if Canonical = PathSeparator then
  begin
    if ARecursive then
      Exit;
    raise ESandboxFsExists.Create('/: already exists');
  end;

  Segments := SplitPath(Canonical);
  Current := FRoot;
  for Index := 0 to High(Segments) do
  begin
    if Current.FKind <> nkDirectory then
      raise ESandboxFsNotADirectory.CreateFmt('%s: not a directory',
        [Current.FullPath]);
    Next := Current.FindChild(Segments[Index]);
    if Next = nil then
    begin
      if (Index < High(Segments)) and not ARecursive then
        raise ESandboxFsNotFound.CreateFmt(
          '%s: no such file or directory', [Canonical]);
      ValidateLeafName(Segments[Index]);
      Time := CurrentTime;
      Next := TSandboxFsNode.Create(Segments[Index], nkDirectory, Time);
      Current.AttachChild(Next, Time);
      Inc(FNodeCount);
    end
    else if (Index = High(Segments)) and not ARecursive then
      raise ESandboxFsExists.CreateFmt('%s: already exists', [Canonical]);
    Current := Next;
  end;
  if Current.FKind <> nkDirectory then
    raise ESandboxFsNotADirectory.CreateFmt('%s: not a directory',
      [Canonical]);
end;

function TSandboxVirtualFileSystem.SubtreeBytes(const ANode: TSandboxFsNode): Int64;
var
  Index: Integer;
begin
  if ANode.FKind = nkFile then
    Exit(ANode.FSize);
  Result := 0;
  for Index := 0 to ANode.FChildren.Count - 1 do
    Result := Result +
      SubtreeBytes(TSandboxFsNode(ANode.FChildren.Objects[Index]));
end;

function TSandboxVirtualFileSystem.SubtreeNodes(const ANode: TSandboxFsNode): Integer;
var
  Index: Integer;
begin
  Result := 1;
  if ANode.FKind = nkDirectory then
    for Index := 0 to ANode.FChildren.Count - 1 do
      Result := Result +
        SubtreeNodes(TSandboxFsNode(ANode.FChildren.Objects[Index]));
end;

function TSandboxVirtualFileSystem.SubtreeHasOpenHandles(
  const ANode: TSandboxFsNode): Boolean;
var
  Index: Integer;
begin
  if ANode.FOpenCount > 0 then
    Exit(True);
  Result := False;
  if ANode.FKind = nkDirectory then
    for Index := 0 to ANode.FChildren.Count - 1 do
      if SubtreeHasOpenHandles(
        TSandboxFsNode(ANode.FChildren.Objects[Index])) then
        Exit(True);
end;

function TSandboxVirtualFileSystem.IsDescendantOf(const ANode,
  AAncestor: TSandboxFsNode): Boolean;
var
  Current: TSandboxFsNode;
begin
  Current := ANode;
  while Assigned(Current) do
  begin
    if Current = AAncestor then
      Exit(True);
    Current := Current.FParent;
  end;
  Result := False;
end;

procedure TSandboxVirtualFileSystem.DeletePath(const APath: string;
  const ARecursive: Boolean);
var
  Node: TSandboxFsNode;
  Time: TDateTime;
begin
  Node := RequireNode(APath);
  if Node = FRoot then
    raise ESandboxFsInvalidPath.Create('/: cannot delete the root');
  if (Node.FKind = nkDirectory) and (Node.FChildren.Count > 0) and
    not ARecursive then
    raise ESandboxFsNotEmpty.CreateFmt('%s: directory not empty',
      [Node.FullPath]);
  if SubtreeHasOpenHandles(Node) then
    raise ESandboxFsBusy.CreateFmt('%s: resource busy (open handle)',
      [Node.FullPath]);
  FUsedBytes := FUsedBytes - SubtreeBytes(Node);
  FNodeCount := FNodeCount - SubtreeNodes(Node);
  Time := CurrentTime;
  Node.FParent.DetachChild(Node, Time);
  Node.Free;
end;

procedure TSandboxVirtualFileSystem.MovePath(const ASource, ADestination: string);
var
  SourceNode: TSandboxFsNode;
  DestNode: TSandboxFsNode;
  DestParent: TSandboxFsNode;
  DestCanonical: string;
  LeafName: string;
  Time: TDateTime;
begin
  SourceNode := RequireNode(ASource);
  if SourceNode = FRoot then
    raise ESandboxFsInvalidPath.Create('/: cannot move the root');
  if SubtreeHasOpenHandles(SourceNode) then
    raise ESandboxFsBusy.CreateFmt('%s: resource busy (open handle)',
      [SourceNode.FullPath]);

  DestCanonical := Normalize(ADestination);
  DestNode := LookupNode(DestCanonical);

  { moving into an existing directory keeps the source's name }
  if Assigned(DestNode) and (DestNode.FKind = nkDirectory) then
  begin
    DestParent := DestNode;
    LeafName := SourceNode.FName;
    DestNode := DestParent.FindChild(LeafName);
  end
  else
  begin
    DestParent := ResolveParent(DestCanonical, LeafName);
    ValidateLeafName(LeafName);
  end;

  if IsDescendantOf(DestParent, SourceNode) then
    raise ESandboxFsInvalidPath.CreateFmt(
      '%s: cannot move a directory into itself',
      [SourceNode.FullPath]);

  if Assigned(DestNode) then
  begin
    if DestNode = SourceNode then
      Exit;
    if (DestNode.FKind = nkFile) and (SourceNode.FKind = nkFile) then
      DeletePath(DestNode.FullPath)
    else
      raise ESandboxFsExists.CreateFmt('%s: already exists',
        [DestNode.FullPath]);
  end;

  Time := CurrentTime;
  SourceNode.FParent.DetachChild(SourceNode, Time);
  SourceNode.FName := LeafName;
  SourceNode.FChangedAt := Time;
  DestParent.AttachChild(SourceNode, Time);
end;

function TSandboxVirtualFileSystem.CloneNodePreservingMetadata(
  const ASource: TSandboxFsNode): TSandboxFsNode;
var
  Index: Integer;
  Child: TSandboxFsNode;
begin
  Result := TSandboxFsNode.Create(ASource.FName, ASource.FKind,
    ASource.FBirthTime);
  if ASource.FKind = nkFile then
  begin
    Result.FSize := ASource.FSize;
    SetLength(Result.FData, ASource.FSize);
    if ASource.FSize > 0 then
      System.Move(ASource.FData[0], Result.FData[0], ASource.FSize);
  end
  else
    for Index := 0 to ASource.FChildren.Count - 1 do
    begin
      Child := CloneNodePreservingMetadata(
        TSandboxFsNode(ASource.FChildren.Objects[Index]));
      Result.AttachChild(Child, ASource.FModifiedAt);
    end;
  Result.FAccessedAt := ASource.FAccessedAt;
  Result.FModifiedAt := ASource.FModifiedAt;
  Result.FChangedAt := ASource.FChangedAt;
  Result.FBirthTime := ASource.FBirthTime;
end;

function TSandboxVirtualFileSystem.CloneNodeForCopy(
  const ASource: TSandboxFsNode; const ATime: TDateTime): TSandboxFsNode;
var
  Index: Integer;
  Child: TSandboxFsNode;
begin
  ASource.FAccessedAt := ATime;
  Result := TSandboxFsNode.Create(ASource.FName, ASource.FKind, ATime);
  if ASource.FKind = nkFile then
  begin
    Result.FSize := ASource.FSize;
    SetLength(Result.FData, ASource.FSize);
    if ASource.FSize > 0 then
      System.Move(ASource.FData[0], Result.FData[0], ASource.FSize);
  end
  else
    for Index := 0 to ASource.FChildren.Count - 1 do
    begin
      Child := CloneNodeForCopy(
        TSandboxFsNode(ASource.FChildren.Objects[Index]), ATime);
      Result.AttachChild(Child, ATime);
    end;
end;

procedure TSandboxVirtualFileSystem.CopyPath(const ASource, ADestination: string;
  const ARecursive: Boolean);
var
  SourceNode: TSandboxFsNode;
  DestNode: TSandboxFsNode;
  DestParent: TSandboxFsNode;
  DestCanonical: string;
  LeafName: string;
  Clone: TSandboxFsNode;
  Time: TDateTime;
begin
  SourceNode := RequireNode(ASource);
  if (SourceNode.FKind = nkDirectory) and not ARecursive then
    raise ESandboxFsIsADirectory.CreateFmt(
      '%s: is a directory (use recursive copy)',
      [SourceNode.FullPath]);

  DestCanonical := Normalize(ADestination);
  DestNode := LookupNode(DestCanonical);

  if Assigned(DestNode) and (DestNode.FKind = nkDirectory) then
  begin
    DestParent := DestNode;
    LeafName := SourceNode.FName;
    DestNode := DestParent.FindChild(LeafName);
  end
  else
  begin
    DestParent := ResolveParent(DestCanonical, LeafName);
    ValidateLeafName(LeafName);
  end;

  if IsDescendantOf(DestParent, SourceNode) then
    raise ESandboxFsInvalidPath.CreateFmt(
      '%s: cannot copy a directory into itself',
      [SourceNode.FullPath]);

  if Assigned(DestNode) then
  begin
    if DestNode = SourceNode then
      Exit;
    if (DestNode.FKind = nkFile) and (SourceNode.FKind = nkFile) then
      DeletePath(DestNode.FullPath)
    else
      raise ESandboxFsExists.CreateFmt('%s: already exists',
        [DestNode.FullPath]);
  end;

  EnsureGrowth(SubtreeBytes(SourceNode));
  Time := CurrentTime;
  Clone := CloneNodeForCopy(SourceNode, Time);
  Clone.FName := LeafName;
  DestParent.AttachChild(Clone, Time);
  FUsedBytes := FUsedBytes + SubtreeBytes(Clone);
  FNodeCount := FNodeCount + SubtreeNodes(Clone);
end;

procedure TSandboxVirtualFileSystem.Touch(const APath: string);
var
  Canonical: string;
  Node: TSandboxFsNode;
  Time: TDateTime;
begin
  Canonical := Normalize(APath);
  Node := LookupNode(Canonical);
  if Assigned(Node) then
  begin
    Time := CurrentTime;
    Node.FAccessedAt := Time;
    Node.FModifiedAt := Time;
    Node.FChangedAt := Time;
  end
  else
    CreateFileNode(Canonical);
end;

procedure TSandboxVirtualFileSystem.WriteAllBytes(const APath: string;
  const AData: TBytes);
var
  Handle: TSandboxFsFile;
begin
  Handle := Open(APath, omWrite);
  try
    if Length(AData) > 0 then
      Handle.Write(AData[0], Length(AData));
  finally
    Handle.Free;
  end;
end;

procedure TSandboxVirtualFileSystem.WriteAllText(const APath: string;
  const AText: string);
var
  Handle: TSandboxFsFile;
begin
  Handle := Open(APath, omWrite);
  try
    Handle.WriteText(AText);
  finally
    Handle.Free;
  end;
end;

procedure TSandboxVirtualFileSystem.AppendAllText(const APath: string;
  const AText: string);
var
  Handle: TSandboxFsFile;
begin
  Handle := Open(APath, omAppend);
  try
    Handle.WriteText(AText);
  finally
    Handle.Free;
  end;
end;

function TSandboxVirtualFileSystem.ReadAllBytes(const APath: string): TBytes;
var
  Node: TSandboxFsNode;
begin
  Result := nil;
  Node := RequireFile(APath);
  Node.FAccessedAt := CurrentTime;
  SetLength(Result, Node.FSize);
  if Node.FSize > 0 then
    System.Move(Node.FData[0], Result[0], Node.FSize);
end;

function TSandboxVirtualFileSystem.ReadAllText(const APath: string): string;
var
  Node: TSandboxFsNode;
begin
  Result := '';
  Node := RequireFile(APath);
  Node.FAccessedAt := CurrentTime;
  SetLength(Result, Node.FSize);
  if Node.FSize > 0 then
    System.Move(Node.FData[0], Result[1], Node.FSize);
end;

function TSandboxVirtualFileSystem.SnapshotList(
  const APath: string): TSandboxFsStatArray;
var
  Node: TSandboxFsNode;
  Index: Integer;
begin
  Result := nil;
  Node := RequireDirectory(APath);
  SetLength(Result, Node.FChildren.Count);
  for Index := 0 to Node.FChildren.Count - 1 do
    Result[Index] := StatOf(TSandboxFsNode(Node.FChildren.Objects[Index]));
end;

function TSandboxVirtualFileSystem.SnapshotReadAllBytes(
  const APath: string): TBytes;
var
  Node: TSandboxFsNode;
begin
  Result := nil;
  Node := RequireFile(APath);
  SetLength(Result, Node.FSize);
  if Node.FSize > 0 then
    System.Move(Node.FData[0], Result[0], Node.FSize);
end;

function TSandboxVirtualFileSystem.SnapshotReadAllText(
  const APath: string): string;
var
  Node: TSandboxFsNode;
begin
  Result := '';
  Node := RequireFile(APath);
  SetLength(Result, Node.FSize);
  if Node.FSize > 0 then
    System.Move(Node.FData[0], Result[1], Node.FSize);
end;

function TSandboxVirtualFileSystem.Open(const APath: string;
  const AMode: TSandboxFsOpenMode): TSandboxFsFile;
var
  Canonical: string;
  Node: TSandboxFsNode;
begin
  Canonical := Normalize(APath);
  Node := LookupNode(Canonical);

  if Assigned(Node) and (Node.FKind = nkDirectory) then
    raise ESandboxFsIsADirectory.CreateFmt('%s: is a directory',
      [Canonical]);

  if Node = nil then
  begin
    if AMode = omRead then
      raise ESandboxFsNotFound.CreateFmt('%s: no such file or directory',
        [Canonical]);
    Node := CreateFileNode(Canonical);
  end;

  Result := TSandboxFsFile.Create(Self, Node, AMode);
end;

function TSandboxVirtualFileSystem.Fork: TSandboxVirtualFileSystem;
begin
  Result := TSandboxVirtualFileSystem.Create(FQuotaBytes, FClock);
  Result.FRoot.Free;
  Result.FRoot := CloneNodePreservingMetadata(FRoot);
  Result.FUsedBytes := FUsedBytes;
  Result.FNodeCount := FNodeCount;
end;

{ TSandboxFsFile }

constructor TSandboxFsFile.Create(const AFs: TSandboxVirtualFileSystem;
  const ANode: TSandboxFsNode; const AMode: TSandboxFsOpenMode);
begin
  inherited Create;
  FFs := AFs;
  FNode := ANode;
  FMode := AMode;
  Inc(FNode.FOpenCount);
  case AMode of
    omWrite:
    begin
      FFs.FUsedBytes := FFs.FUsedBytes - FNode.FSize;
      FNode.FSize := 0;
      FNode.FModifiedAt := FFs.CurrentTime;
      FNode.FChangedAt := FNode.FModifiedAt;
      FPosition := 0;
    end;
    omAppend:
      FPosition := FNode.FSize;
  else
    FPosition := 0;
  end;
end;

destructor TSandboxFsFile.Destroy;
begin
  Close;
  inherited Destroy;
end;

procedure TSandboxFsFile.Close;
begin
  if not FClosed then
  begin
    FClosed := True;
    Dec(FNode.FOpenCount);
  end;
end;

function TSandboxFsFile.GetSize: Int64;
begin
  Result := FNode.FSize;
end;

function TSandboxFsFile.GetPath: string;
begin
  Result := FNode.FullPath;
end;

function TSandboxFsFile.Read(var ABuffer; const ACount: Integer): Integer;
begin
  if FClosed then
    raise ESandboxFsError.Create('read on a closed handle');
  Result := ACount;
  if FPosition + Result > FNode.FSize then
    Result := FNode.FSize - FPosition;
  if Result <= 0 then
    Exit(0);
  System.Move(FNode.FData[FPosition], ABuffer, Result);
  FPosition := FPosition + Result;
  FNode.FAccessedAt := FFs.CurrentTime;
end;

function TSandboxFsFile.Write(const ABuffer;
  const ACount: Integer): Integer;
begin
  if FClosed then
    raise ESandboxFsError.Create('write on a closed handle');
  if FMode = omRead then
    raise ESandboxFsError.CreateFmt('%s: handle is read-only',
      [FNode.FullPath]);
  if ACount <= 0 then
    Exit(0);
  if FMode = omAppend then
    FPosition := FNode.FSize;
  if FPosition + ACount > FNode.FSize then
    FFs.GrowFile(FNode, FPosition + ACount);
  System.Move(ABuffer, FNode.FData[FPosition], ACount);
  FPosition := FPosition + ACount;
  FNode.FModifiedAt := FFs.CurrentTime;
  FNode.FChangedAt := FNode.FModifiedAt;
  Result := ACount;
end;

function TSandboxFsFile.Seek(const AOffset: Int64;
  const AOrigin: TSeekOrigin): Int64;
var
  Target: Int64;
begin
  case AOrigin of
    soCurrent: Target := FPosition + AOffset;
    soEnd: Target := FNode.FSize + AOffset;
  else
    Target := AOffset;
  end;
  if Target < 0 then
    raise ESandboxFsError.Create('seek before start of file');
  FPosition := Target;
  Result := FPosition;
end;

procedure TSandboxFsFile.Truncate(const ASize: Int64);
begin
  if FMode = omRead then
    raise ESandboxFsError.CreateFmt('%s: handle is read-only',
      [FNode.FullPath]);
  if ASize < 0 then
    raise ESandboxFsError.Create('truncate to a negative size');
  if ASize > FNode.FSize then
    FFs.GrowFile(FNode, ASize)
  else
  begin
    FFs.FUsedBytes := FFs.FUsedBytes - (FNode.FSize - ASize);
    FNode.FSize := ASize;
  end;
  if FPosition > FNode.FSize then
    FPosition := FNode.FSize;
  FNode.FModifiedAt := FFs.CurrentTime;
  FNode.FChangedAt := FNode.FModifiedAt;
end;

function TSandboxFsFile.ReadText(const ACount: Integer): string;
var
  Actual: Integer;
begin
  Result := '';
  SetLength(Result, ACount);
  if ACount <= 0 then
    Exit('');
  Actual := Read(Result[1], ACount);
  SetLength(Result, Actual);
end;

procedure TSandboxFsFile.WriteText(const AText: string);
begin
  if Length(AText) > 0 then
    Write(AText[1], Length(AText));
end;

end.
