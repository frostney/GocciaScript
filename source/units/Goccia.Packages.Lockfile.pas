unit Goccia.Packages.Lockfile;

{ `goccia.lock.json`: the pins of provider packages (ADR 0122).

  The lockfile sits beside the import map that declares the packages. It
  pins each package key `github:<owner>/<repo>@<ref>` to a commit and every
  file of the package to a SHA-256:

    {
      "version": 1,
      "packages": {
        "github:frostney/GocciaScript-Raylib@v0.10.0": {
          "ref": "tag",
          "commit": "<40 hex>",
          "artifacts": {
            "bindings/raylib.ts": { "sha256": "<64 hex>" }
          }
        }
      }
    }

  The reader is strict: an unknown key, a duplicate key, a value of the wrong
  type, an unsafe path, or two paths that differ only in case is an error,
  never ignored. A run only ever reads the lockfile; install mode writes it
  deterministically: keys in byte order, two-space indentation, LF line
  endings, and a trailing newline, so rewriting an unchanged lock changes no
  byte and a diff shows only real changes. }

{$I Goccia.inc}

interface

uses
  Generics.Collections,
  SysUtils,

  Goccia.Packages.Address;

const
  LOCKFILE_NAME = 'goccia.lock.json';
  LOCKFILE_VERSION = 1;
  LOCK_REF_TAG = 'tag';
  LOCK_REF_COMMIT = 'commit';

type
  EGocciaLockfileError = class(Exception);

  TGocciaLockRefKind = (lrkTag, lrkCommit);

  TGocciaLockedArtifact = record
    Path: string;
    SHA256: string;
  end;

  TGocciaLockedArtifacts = array of TGocciaLockedArtifact;

  TGocciaLockedPackage = class
  private
    FAddress: TGocciaProviderAddress;
    FArtifacts: TGocciaLockedArtifacts;
    FCommit: string;
    FKey: string;
    FRefKind: TGocciaLockRefKind;
  public
    constructor Create(const AKey: string;
      const AAddress: TGocciaProviderAddress);
    procedure AddArtifact(const APath, ASHA256: string);
    { Sorts the artifacts by path in byte order. }
    procedure SortArtifacts;
    function FindArtifact(const APath: string; out ASHA256: string): Boolean;
    function ArtifactCount: Integer;
    function Artifact(const AIndex: Integer): TGocciaLockedArtifact;

    property Address: TGocciaProviderAddress read FAddress;
    property Commit: string read FCommit write FCommit;
    property Key: string read FKey;
    property RefKind: TGocciaLockRefKind read FRefKind write FRefKind;
  end;

  TGocciaLockedPackageList = TObjectList<TGocciaLockedPackage>;

  TGocciaLockfile = class
  private
    FPackages: TGocciaLockedPackageList;
  public
    constructor Create;
    destructor Destroy; override;
    { The package pinned for AKey. Owner and repository compare
      case-insensitively, the ref exactly. }
    function FindPackage(const AKey: string): TGocciaLockedPackage;
    property Packages: TGocciaLockedPackageList read FPackages;
  end;

{ Parses lockfile text. ALocation names the file in error messages. Raises
  EGocciaLockfileError. }
function ParseLockfile(const AText, ALocation: string): TGocciaLockfile;

{ Reads APath. A missing file raises EGocciaLockfileError too. }
function LoadLockfile(const APath: string): TGocciaLockfile;

{ The lockfile's deterministic text. }
function SerializeLockfile(const ALockfile: TGocciaLockfile): string;

{ Writes ALockfile to APath through a temporary renamed into place, after
  checking that the text reads back as a valid lockfile. }
procedure SaveLockfile(const APath: string; const ALockfile: TGocciaLockfile);

function LockRefKindName(const AKind: TGocciaLockRefKind): string;

implementation

uses
  Classes,

  FileUtils,
  JSONParser,
  StringBuffer,
  TextEncoding,

  Goccia.JSON.Utils;

type
  TLockValueKind = (lvkObject, lvkArray, lvkString, lvkInteger, lvkOther);

  TLockfileReader = class(TAbstractJSONParser)
  private
    FLocation: string;
    FLockfile: TGocciaLockfile;
    FDepth: Integer;
    FKeys: array[1..5] of string;
    FSeenKeys: array[1..5] of TDictionary<string, Boolean>;
    FSawVersion: Boolean;
    FSawPackages: Boolean;
    FPackage: TGocciaLockedPackage;
    FSawRef: Boolean;
    FSawCommit: Boolean;
    FSawArtifacts: Boolean;
    FArtifactPath: string;
    FArtifactHash: string;
    procedure Fail(const AProblem: string);
    procedure Value(const AKind: TLockValueKind; const AText: string;
      const AInteger: Int64);
    procedure BeginPackage(const AKey: string);
    procedure EndPackage;
  protected
    procedure OnNull; override;
    procedure OnBoolean(const AValue: Boolean); override;
    procedure OnString(const AValue: string); override;
    procedure OnInteger(const AValue: Int64); override;
    procedure OnFloat(const AValue: Double); override;
    procedure OnBeginObject; override;
    procedure OnObjectKey(const AKey: string); override;
    procedure OnEndObject; override;
    procedure OnBeginArray; override;
    procedure OnEndArray; override;
  public
    constructor CreateReader(const ALocation: string);
    destructor Destroy; override;
    function Read(const AText: string): TGocciaLockfile;
  end;

const
  VERSION_KEY = 'version';
  PACKAGES_KEY = 'packages';
  REF_KEY = 'ref';
  COMMIT_KEY = 'commit';
  ARTIFACTS_KEY = 'artifacts';
  SHA256_KEY = 'sha256';
  LOCK_TEXT_CAPACITY = 4096;

function LockRefKindName(const AKind: TGocciaLockRefKind): string;
begin
  if AKind = lrkTag then
    Result := LOCK_REF_TAG
  else
    Result := LOCK_REF_COMMIT;
end;

{ TGocciaLockedPackage }

constructor TGocciaLockedPackage.Create(const AKey: string;
  const AAddress: TGocciaProviderAddress);
begin
  inherited Create;
  FKey := AKey;
  FAddress := AAddress;
  FArtifacts := nil;
end;

procedure TGocciaLockedPackage.AddArtifact(const APath, ASHA256: string);
begin
  SetLength(FArtifacts, Length(FArtifacts) + 1);
  FArtifacts[High(FArtifacts)].Path := APath;
  FArtifacts[High(FArtifacts)].SHA256 := ASHA256;
end;

procedure TGocciaLockedPackage.SortArtifacts;
var
  I, J: Integer;
  Swap: TGocciaLockedArtifact;
begin
  { Insertion sort in byte order: stable, and lockfiles are small. }
  for I := 1 to High(FArtifacts) do
  begin
    Swap := FArtifacts[I];
    J := I - 1;
    while (J >= 0) and (CompareStr(FArtifacts[J].Path, Swap.Path) > 0) do
    begin
      FArtifacts[J + 1] := FArtifacts[J];
      Dec(J);
    end;
    FArtifacts[J + 1] := Swap;
  end;
end;

function TGocciaLockedPackage.FindArtifact(const APath: string;
  out ASHA256: string): Boolean;
var
  LowIndex, HighIndex, Middle, Comparison: Integer;
begin
  ASHA256 := '';
  LowIndex := 0;
  HighIndex := Length(FArtifacts) - 1;
  while LowIndex <= HighIndex do
  begin
    Middle := (LowIndex + HighIndex) div 2;
    Comparison := CompareStr(FArtifacts[Middle].Path, APath);
    if Comparison = 0 then
    begin
      ASHA256 := FArtifacts[Middle].SHA256;
      Exit(True);
    end;
    if Comparison < 0 then
      LowIndex := Middle + 1
    else
      HighIndex := Middle - 1;
  end;
  Result := False;
end;

function TGocciaLockedPackage.ArtifactCount: Integer;
begin
  Result := Length(FArtifacts);
end;

function TGocciaLockedPackage.Artifact(
  const AIndex: Integer): TGocciaLockedArtifact;
begin
  Result := FArtifacts[AIndex];
end;

{ TGocciaLockfile }

constructor TGocciaLockfile.Create;
begin
  inherited Create;
  FPackages := TGocciaLockedPackageList.Create(True);
end;

destructor TGocciaLockfile.Destroy;
begin
  FPackages.Free;
  inherited;
end;

function TGocciaLockfile.FindPackage(
  const AKey: string): TGocciaLockedPackage;
var
  Address: TGocciaProviderAddress;
  Error: string;
  Package: TGocciaLockedPackage;
begin
  Result := nil;
  if not TryParsePackageKey(AKey, Address, Error) then
    Exit;
  for Package in FPackages do
    if Package.Address.NormalizedPackageKey = Address.NormalizedPackageKey then
      Exit(Package);
end;

{ TLockfileReader }

constructor TLockfileReader.CreateReader(const ALocation: string);
var
  I: Integer;
begin
  inherited Create(JSONParserStrictCapabilities);
  FLocation := ALocation;
  for I := Low(FSeenKeys) to High(FSeenKeys) do
    FSeenKeys[I] := TDictionary<string, Boolean>.Create;
end;

destructor TLockfileReader.Destroy;
var
  I: Integer;
begin
  for I := Low(FSeenKeys) to High(FSeenKeys) do
    FSeenKeys[I].Free;
  FLockfile.Free;
  inherited;
end;

procedure TLockfileReader.Fail(const AProblem: string);
begin
  raise EGocciaLockfileError.CreateFmt('%s is not a valid lockfile: %s',
    [FLocation, AProblem]);
end;

procedure TLockfileReader.BeginPackage(const AKey: string);
var
  Address: TGocciaProviderAddress;
  Error: string;
begin
  if not TryParsePackageKey(AKey, Address, Error) then
    Fail(Format('package "%s": %s', [AKey, Error]));
  if Assigned(FLockfile.FindPackage(AKey)) then
    Fail(Format('package "%s" is pinned twice (owner and repository names ' +
      'differ only in case)', [AKey]));
  FPackage := TGocciaLockedPackage.Create(AKey, Address);
  FLockfile.Packages.Add(FPackage);
  FSawRef := False;
  FSawCommit := False;
  FSawArtifacts := False;
end;

procedure TLockfileReader.EndPackage;
var
  I, SlashIndex: Integer;
  Directories: TDictionary<string, Boolean>;
  Folded: TDictionary<string, string>;
  Directory, Path, Other: string;
begin
  if not FSawRef then
    Fail(Format('package "%s" has no "%s"', [FPackage.Key, REF_KEY]));
  if not FSawCommit then
    Fail(Format('package "%s" has no "%s"', [FPackage.Key, COMMIT_KEY]));
  if not FSawArtifacts or (FPackage.ArtifactCount = 0) then
    Fail(Format('package "%s" pins no artifacts', [FPackage.Key]));
  if (FPackage.RefKind = lrkCommit) and
     (FPackage.Address.Ref <> FPackage.Commit) then
    Fail(Format('package "%s" is a commit pin, so its ref must be its commit',
      [FPackage.Key]));
  if (FPackage.RefKind = lrkTag) and IsCommitHash(FPackage.Address.Ref) then
    Fail(Format('package "%s" names a commit, so its "ref" must be "commit"',
      [FPackage.Key]));

  FPackage.SortArtifacts;
  { Paths that differ only in case would overwrite each other on a
    case-insensitive filesystem, and a file cannot also be a directory. }
  Folded := TDictionary<string, string>.Create;
  Directories := TDictionary<string, Boolean>.Create;
  try
    for I := 0 to FPackage.ArtifactCount - 1 do
    begin
      Path := FPackage.Artifact(I).Path;
      if Folded.TryGetValue(LowerCase(Path), Other) then
        Fail(Format('package "%s" pins "%s" and "%s", which differ only in ' +
          'case', [FPackage.Key, Other, Path]));
      Folded.Add(LowerCase(Path), Path);
      Directory := LowerCase(Path);
      SlashIndex := LastDelimiter('/', Directory);
      while SlashIndex > 0 do
      begin
        Directory := Copy(Directory, 1, SlashIndex - 1);
        Directories.AddOrSetValue(Directory, True);
        SlashIndex := LastDelimiter('/', Directory);
      end;
    end;
    for I := 0 to FPackage.ArtifactCount - 1 do
      if Directories.ContainsKey(LowerCase(FPackage.Artifact(I).Path)) then
        Fail(Format('package "%s" pins "%s" as a file and as a directory',
          [FPackage.Key, FPackage.Artifact(I).Path]));
  finally
    Directories.Free;
    Folded.Free;
  end;
  FPackage := nil;
end;

procedure TLockfileReader.Value(const AKind: TLockValueKind;
  const AText: string; const AInteger: Int64);
begin
  case FDepth of
    0:
      if AKind <> lvkObject then
        Fail('the top level is not an object');
    1:
      if FKeys[1] = VERSION_KEY then
      begin
        if AKind <> lvkInteger then
          Fail('"version" is not an integer');
        if AInteger > LOCKFILE_VERSION then
          raise EGocciaLockfileError.CreateFmt(
            '%s was written by a newer GocciaScript (version %d); upgrade ' +
            'GocciaScript', [FLocation, AInteger]);
        if AInteger <> LOCKFILE_VERSION then
          Fail(Format('"version" must be %d', [LOCKFILE_VERSION]));
        FSawVersion := True;
      end
      else
      begin
        if AKind <> lvkObject then
          Fail('"packages" is not an object');
        FSawPackages := True;
      end;
    2:
      if AKind <> lvkObject then
        Fail(Format('package "%s" is not an object', [FKeys[2]]));
    3:
      if FKeys[3] = REF_KEY then
      begin
        if AKind <> lvkString then
          Fail(Format('"ref" of "%s" is not a string', [FKeys[2]]));
        if AText = LOCK_REF_TAG then
          FPackage.RefKind := lrkTag
        else if AText = LOCK_REF_COMMIT then
          FPackage.RefKind := lrkCommit
        else
          Fail(Format('"ref" of "%s" must be "tag" or "commit"', [FKeys[2]]));
        FSawRef := True;
      end
      else if FKeys[3] = COMMIT_KEY then
      begin
        if (AKind <> lvkString) or not IsCommitHash(AText) then
          Fail(Format('"commit" of "%s" must be 40 lowercase hexadecimal ' +
            'digits', [FKeys[2]]));
        FPackage.Commit := AText;
        FSawCommit := True;
      end
      else
      begin
        if AKind <> lvkObject then
          Fail(Format('"artifacts" of "%s" is not an object', [FKeys[2]]));
        FSawArtifacts := True;
      end;
    4:
      if AKind <> lvkObject then
        Fail(Format('artifact "%s" of "%s" is not an object',
          [FKeys[4], FKeys[2]]));
    5:
      begin
        if (AKind <> lvkString) or not IsSHA256Hash(AText) then
          Fail(Format('"sha256" of "%s" in "%s" must be 64 lowercase ' +
            'hexadecimal digits', [FKeys[4], FKeys[2]]));
        FArtifactHash := AText;
      end;
  end;
end;

procedure TLockfileReader.OnNull;
begin
  Value(lvkOther, '', 0);
end;

procedure TLockfileReader.OnBoolean(const AValue: Boolean);
begin
  Value(lvkOther, '', 0);
end;

procedure TLockfileReader.OnString(const AValue: string);
begin
  Value(lvkString, AValue, 0);
end;

procedure TLockfileReader.OnInteger(const AValue: Int64);
begin
  Value(lvkInteger, '', AValue);
end;

procedure TLockfileReader.OnFloat(const AValue: Double);
begin
  Value(lvkOther, '', 0);
end;

procedure TLockfileReader.OnBeginArray;
begin
  Value(lvkArray, '', 0);
end;

procedure TLockfileReader.OnEndArray;
begin
end;

procedure TLockfileReader.OnBeginObject;
begin
  Value(lvkObject, '', 0);
  Inc(FDepth);
  if FDepth > High(FKeys) then
    Fail('it nests deeper than the lockfile schema');
  FSeenKeys[FDepth].Clear;
  FKeys[FDepth] := '';
  case FDepth of
    3:
      BeginPackage(FKeys[2]);
    5:
      begin
        FArtifactPath := FKeys[4];
        FArtifactHash := '';
      end;
  end;
end;

procedure TLockfileReader.OnObjectKey(const AKey: string);
begin
  if FSeenKeys[FDepth].ContainsKey(AKey) then
    Fail(Format('the key "%s" appears twice', [AKey]));
  FSeenKeys[FDepth].Add(AKey, True);
  FKeys[FDepth] := AKey;
  case FDepth of
    1:
      if (AKey <> VERSION_KEY) and (AKey <> PACKAGES_KEY) then
        Fail(Format('unknown key "%s"', [AKey]));
    3:
      if (AKey <> REF_KEY) and (AKey <> COMMIT_KEY) and
         (AKey <> ARTIFACTS_KEY) then
        Fail(Format('unknown key "%s" in package "%s"', [AKey, FKeys[2]]));
    4:
      if not IsSafeArtifactPath(AKey) then
        Fail(Format('artifact path "%s" of "%s" is not a safe relative path',
          [AKey, FKeys[2]]));
    5:
      if AKey <> SHA256_KEY then
        Fail(Format('unknown key "%s" in artifact "%s" of "%s"',
          [AKey, FKeys[4], FKeys[2]]));
  end;
end;

procedure TLockfileReader.OnEndObject;
begin
  case FDepth of
    1:
      begin
        if not FSawVersion then
          Fail('no "version"');
        if not FSawPackages then
          Fail('no "packages"');
      end;
    3:
      EndPackage;
    5:
      begin
        if FArtifactHash = '' then
          Fail(Format('artifact "%s" of "%s" has no "sha256"',
            [FArtifactPath, FKeys[2]]));
        FPackage.AddArtifact(FArtifactPath, FArtifactHash);
      end;
  end;
  Dec(FDepth);
end;

function TLockfileReader.Read(const AText: string): TGocciaLockfile;
begin
  FLockfile := TGocciaLockfile.Create;
  FDepth := 0;
  try
    DoParse(AText);
  except
    on E: EGocciaLockfileError do
      raise;
    on E: Exception do
      raise EGocciaLockfileError.CreateFmt('%s is not valid JSON: %s',
        [FLocation, E.Message]);
  end;
  Result := FLockfile;
  FLockfile := nil;
end;

function SerializeLockfile(const ALockfile: TGocciaLockfile): string;
var
  Buffer: TStringBuffer;
  Keys: TStringList;
  I, J: Integer;
  Package: TGocciaLockedPackage;
begin
  Keys := TStringList.Create;
  try
    Keys.UseLocale := False;
    Keys.CaseSensitive := True;
    for Package in ALockfile.Packages do
      Keys.AddObject(Package.Key, Package);
    Keys.Sort;
    Buffer := TStringBuffer.Create(LOCK_TEXT_CAPACITY);
    Buffer.Append('{' + #10 + '  "packages": {');
    for I := 0 to Keys.Count - 1 do
    begin
      Package := TGocciaLockedPackage(Keys.Objects[I]);
      Package.SortArtifacts;
      if I > 0 then
        Buffer.Append(',');
      Buffer.Append(#10 + '    ' + QuoteJSONString(Package.Key) + ': {' +
        #10 + '      "artifacts": {');
      for J := 0 to Package.ArtifactCount - 1 do
      begin
        if J > 0 then
          Buffer.Append(',');
        Buffer.Append(#10 + '        ' +
          QuoteJSONString(Package.Artifact(J).Path) + ': {' + #10 +
          '          "sha256": ' + QuoteJSONString(Package.Artifact(J).SHA256) +
          #10 + '        }');
      end;
      if Package.ArtifactCount > 0 then
        Buffer.Append(#10 + '      ');
      Buffer.Append('},' + #10 + '      "commit": ' +
        QuoteJSONString(Package.Commit) + ',' + #10 + '      "ref": ' +
        QuoteJSONString(LockRefKindName(Package.RefKind)) + #10 + '    }');
    end;
    if Keys.Count > 0 then
      Buffer.Append(#10 + '  ');
    Buffer.Append('},' + #10 + '  "version": ' + IntToStr(LOCKFILE_VERSION) +
      #10 + '}' + #10);
    Result := Buffer.ToString;
  finally
    Keys.Free;
  end;
end;

procedure SaveLockfile(const APath: string; const ALockfile: TGocciaLockfile);
var
  Bytes: TBytes;
  ErrorMessage, Text: string;
  ErrorOffset: Integer;
begin
  Text := SerializeLockfile(ALockfile);
  { Never write what the reader would refuse. }
  ParseLockfile(Text, APath).Free;
  if not TryEncodeUTF8(Text, Bytes, ErrorOffset) then
    raise EGocciaLockfileError.CreateFmt('%s cannot be encoded as UTF-8',
      [APath]);
  if not ReplaceHostFile(APath, APath + '.goccia-write-' +
     IntToStr(GetProcessID), Bytes, ErrorMessage) then
    raise EGocciaLockfileError.CreateFmt('cannot write %s: %s',
      [APath, ErrorMessage]);
end;

function ParseLockfile(const AText, ALocation: string): TGocciaLockfile;
var
  Reader: TLockfileReader;
begin
  Reader := TLockfileReader.CreateReader(ALocation);
  try
    Result := Reader.Read(AText);
  finally
    Reader.Free;
  end;
end;

function LoadLockfile(const APath: string): TGocciaLockfile;
var
  Text: string;
begin
  if not HostFileExists(APath) then
    raise EGocciaLockfileError.CreateFmt('lockfile not found: %s', [APath]);
  try
    Text := ReadUTF8FileText(APath);
  except
    on E: Exception do
      raise EGocciaLockfileError.CreateFmt('cannot read %s: %s',
        [APath, E.Message]);
  end;
  Result := ParseLockfile(Text, APath);
end;

end.
