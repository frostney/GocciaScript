program Goccia.Packages.Lockfile.Test;

{$I Goccia.inc}

uses
  SysUtils,

  TestingPascalLibrary,

  Goccia.Packages.Lockfile,
  Goccia.TestSetup;

const
  COMMIT = '0123456789abcdef0123456789abcdef01234567';
  HASH_A = 'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa';
  HASH_B = 'bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb';

type
  TLockfileTests = class(TTestSuite)
  private
    function Lock(const APackages: string): string;
    function Package(const AKey, ARef, ACommit, AArtifacts: string): string;
    function Rejects(const AText: string): Boolean;
    function RejectionMessage(const AText: string): string;
    procedure TestReadsATagPin;
    procedure TestReadsACommitPin;
    procedure TestRejectsUnknownAndMissingKeys;
    procedure TestRejectsWrongTypes;
    procedure TestRejectsDuplicateKeys;
    procedure TestRejectsUnsafeArtifactPaths;
    procedure TestRejectsCaseAndDirectoryCollisions;
    procedure TestRefMustMatchKind;
    procedure TestRejectsNewerAndOlderVersions;
    procedure TestRejectsInvalidJSON;
  public
    procedure SetupTests; override;
  end;

procedure TLockfileTests.SetupTests;
begin
  Test('Reads a tag pin', TestReadsATagPin);
  Test('Reads a commit pin', TestReadsACommitPin);
  Test('Rejects unknown and missing keys', TestRejectsUnknownAndMissingKeys);
  Test('Rejects values of the wrong type', TestRejectsWrongTypes);
  Test('Rejects duplicate keys', TestRejectsDuplicateKeys);
  Test('Rejects unsafe artifact paths', TestRejectsUnsafeArtifactPaths);
  Test('Rejects case and file/directory collisions',
    TestRejectsCaseAndDirectoryCollisions);
  Test('The ref kind must match the key', TestRefMustMatchKind);
  Test('Rejects other lockfile versions', TestRejectsNewerAndOlderVersions);
  Test('Rejects invalid JSON', TestRejectsInvalidJSON);
end;

function TLockfileTests.Lock(const APackages: string): string;
begin
  Result := '{"version": 1, "packages": {' + APackages + '}}';
end;

function TLockfileTests.Package(const AKey, ARef, ACommit,
  AArtifacts: string): string;
begin
  Result := '"' + AKey + '": {"ref": "' + ARef + '", "commit": "' + ACommit +
    '", "artifacts": {' + AArtifacts + '}}';
end;

function TLockfileTests.RejectionMessage(const AText: string): string;
var
  Lockfile: TGocciaLockfile;
begin
  Result := '';
  try
    Lockfile := ParseLockfile(AText, 'goccia.lock.json');
    Lockfile.Free;
  except
    on E: EGocciaLockfileError do
      Result := E.Message;
  end;
end;

function TLockfileTests.Rejects(const AText: string): Boolean;
begin
  Result := RejectionMessage(AText) <> '';
end;

procedure TLockfileTests.TestReadsATagPin;
var
  Lockfile: TGocciaLockfile;
  Locked: TGocciaLockedPackage;
  Hash: string;
begin
  Lockfile := ParseLockfile(Lock(Package('github:o/r@v1', 'tag', COMMIT,
    '"z.ts": {"sha256": "' + HASH_B + '"}, "a/b.ts": {"sha256": "' +
    HASH_A + '"}')), 'goccia.lock.json');
  try
    Expect<Integer>(Lockfile.Packages.Count).ToBe(1);
    Locked := Lockfile.FindPackage('github:o/r@v1');
    Expect<Boolean>(Assigned(Locked)).ToBe(True);
    Expect<Boolean>(Locked.RefKind = lrkTag).ToBe(True);
    Expect<string>(Locked.Commit).ToBe(COMMIT);
    Expect<string>(Locked.Address.Owner).ToBe('o');
    Expect<Integer>(Locked.ArtifactCount).ToBe(2);
    { Artifacts are kept in byte order. }
    Expect<string>(Locked.Artifact(0).Path).ToBe('a/b.ts');
    Expect<Boolean>(Locked.FindArtifact('z.ts', Hash)).ToBe(True);
    Expect<string>(Hash).ToBe(HASH_B);
    Expect<Boolean>(Locked.FindArtifact('Z.ts', Hash)).ToBe(False);
    Expect<Boolean>(Assigned(Lockfile.FindPackage('github:o/r@v2')))
      .ToBe(False);
  finally
    Lockfile.Free;
  end;
end;

procedure TLockfileTests.TestReadsACommitPin;
var
  Lockfile: TGocciaLockfile;
begin
  Lockfile := ParseLockfile(Lock(Package('github:o/r@' + COMMIT, 'commit',
    COMMIT, '"x.ts": {"sha256": "' + HASH_A + '"}')), 'goccia.lock.json');
  try
    Expect<Boolean>(Lockfile.Packages[0].RefKind = lrkCommit).ToBe(True);
  finally
    Lockfile.Free;
  end;
end;

procedure TLockfileTests.TestRejectsUnknownAndMissingKeys;
var
  Artifact: string;
begin
  Artifact := '"x.ts": {"sha256": "' + HASH_A + '"}';
  Expect<Boolean>(Rejects('{"version": 1}')).ToBe(True);
  Expect<Boolean>(Rejects('{"packages": {}}')).ToBe(True);
  Expect<Boolean>(Rejects('{"version": 1, "packages": {}, "x": 1}'))
    .ToBe(True);
  { #1054's fields are gone. }
  Expect<Boolean>(Rejects(Lock('"github:o/r@v1": {"resolvedRef": "' + COMMIT +
    '", "ref": "tag", "commit": "' + COMMIT + '", "artifacts": {' + Artifact +
    '}}'))).ToBe(True);
  Expect<Boolean>(Rejects(Lock('"github:o/r@v1": {"entry": "x.ts", ' +
    '"ref": "tag", "commit": "' + COMMIT + '", "artifacts": {' + Artifact +
    '}}'))).ToBe(True);
  Expect<Boolean>(Rejects(Lock(Package('github:o/r@v1', 'tag', COMMIT,
    '"x.ts": {"sha256": "' + HASH_A + '", "platform": "linux-x86_64"}'))))
    .ToBe(True);
  Expect<Boolean>(Rejects(Lock('"github:o/r@v1": {"ref": "tag", ' +
    '"artifacts": {' + Artifact + '}}'))).ToBe(True);
  Expect<Boolean>(Rejects(Lock('"github:o/r@v1": {"commit": "' + COMMIT +
    '", "artifacts": {' + Artifact + '}}'))).ToBe(True);
  Expect<Boolean>(Rejects(Lock('"github:o/r@v1": {"ref": "tag", ' +
    '"commit": "' + COMMIT + '"}'))).ToBe(True);
  Expect<Boolean>(Rejects(Lock(Package('github:o/r@v1', 'tag', COMMIT, ''))))
    .ToBe(True);
  Expect<Boolean>(Rejects(Lock(Package('github:o/r@v1', 'tag', COMMIT,
    '"x.ts": {}')))).ToBe(True);
  { A branch is not a ref kind. }
  Expect<Boolean>(Rejects(Lock(Package('github:o/r@main', 'branch', COMMIT,
    Artifact)))).ToBe(True);
end;

procedure TLockfileTests.TestRejectsWrongTypes;
begin
  Expect<Boolean>(Rejects('[]')).ToBe(True);
  Expect<Boolean>(Rejects('{"version": "1", "packages": {}}')).ToBe(True);
  Expect<Boolean>(Rejects('{"version": 1, "packages": []}')).ToBe(True);
  Expect<Boolean>(Rejects(Lock('"github:o/r@v1": "x"'))).ToBe(True);
  Expect<Boolean>(Rejects(Lock(Package('github:o/r@v1', 'tag',
    'ABCDEF0123456789abcdef0123456789abcdef01', '"x.ts": {"sha256": "' +
    HASH_A + '"}')))).ToBe(True);
  Expect<Boolean>(Rejects(Lock(Package('github:o/r@v1', 'tag', COMMIT,
    '"x.ts": {"sha256": "' + UpperCase(HASH_A) + '"}')))).ToBe(True);
  Expect<Boolean>(Rejects(Lock(Package('github:o/r@v1', 'tag', COMMIT,
    '"x.ts": {"sha256": ["' + HASH_A + '"]}')))).ToBe(True);
  Expect<Boolean>(Rejects(Lock(Package('not-a-package', 'tag', COMMIT,
    '"x.ts": {"sha256": "' + HASH_A + '"}')))).ToBe(True);
  { A key names a package, never a path inside it. }
  Expect<Boolean>(Rejects(Lock(Package('github:o/r@v1/x.ts', 'tag', COMMIT,
    '"x.ts": {"sha256": "' + HASH_A + '"}')))).ToBe(True);
end;

procedure TLockfileTests.TestRejectsDuplicateKeys;
begin
  Expect<Boolean>(Rejects('{"version": 1, "version": 1, "packages": {}}'))
    .ToBe(True);
  Expect<Boolean>(Rejects(Lock(Package('github:o/r@v1', 'tag', COMMIT,
    '"x.ts": {"sha256": "' + HASH_A + '"}, "x.ts": {"sha256": "' + HASH_B +
    '"}')))).ToBe(True);
  Expect<Boolean>(Rejects(Lock(Package('github:o/r@v1', 'tag', COMMIT,
    '"x.ts": {"sha256": "' + HASH_A + '"}') + ', ' + Package('github:o/r@v1',
    'tag', COMMIT, '"x.ts": {"sha256": "' + HASH_A + '"}')))).ToBe(True);
end;

procedure TLockfileTests.TestRejectsUnsafeArtifactPaths;
begin
  Expect<Boolean>(Rejects(Lock(Package('github:o/r@v1', 'tag', COMMIT,
    '"../x.ts": {"sha256": "' + HASH_A + '"}')))).ToBe(True);
  Expect<Boolean>(Rejects(Lock(Package('github:o/r@v1', 'tag', COMMIT,
    '"/etc/x": {"sha256": "' + HASH_A + '"}')))).ToBe(True);
  Expect<Boolean>(Rejects(Lock(Package('github:o/r@v1', 'tag', COMMIT,
    '"goccia.json": {"sha256": "' + HASH_A + '"}')))).ToBe(True);
end;

procedure TLockfileTests.TestRejectsCaseAndDirectoryCollisions;
begin
  Expect<Boolean>(Rejects(Lock(Package('github:o/r@v1', 'tag', COMMIT,
    '"A.ts": {"sha256": "' + HASH_A + '"}, "a.ts": {"sha256": "' + HASH_B +
    '"}')))).ToBe(True);
  Expect<Boolean>(Rejects(Lock(Package('github:o/r@v1', 'tag', COMMIT,
    '"lib": {"sha256": "' + HASH_A + '"}, "lib/x.ts": {"sha256": "' + HASH_B +
    '"}')))).ToBe(True);
  { `lib.ts` sorts between `lib` and `lib/x.ts`. }
  Expect<Boolean>(Rejects(Lock(Package('github:o/r@v1', 'tag', COMMIT,
    '"lib": {"sha256": "' + HASH_A + '"}, "lib.ts": {"sha256": "' + HASH_A +
    '"}, "lib/x.ts": {"sha256": "' + HASH_B + '"}')))).ToBe(True);
  Expect<Boolean>(Rejects(Lock(Package('github:o/r@v1', 'tag', COMMIT,
    '"Lib": {"sha256": "' + HASH_A + '"}, "lib/x.ts": {"sha256": "' + HASH_B +
    '"}')))).ToBe(True);
  Expect<Boolean>(Rejects(Lock(Package('github:o/r@v1', 'tag', COMMIT,
    '"lib.ts": {"sha256": "' + HASH_A + '"}, "lib/x.ts": {"sha256": "' +
    HASH_B + '"}')))).ToBe(False);
end;

procedure TLockfileTests.TestRefMustMatchKind;
var
  Artifact: string;
begin
  Artifact := '"x.ts": {"sha256": "' + HASH_A + '"}';
  Expect<Boolean>(Rejects(Lock(Package('github:o/r@v1', 'commit', COMMIT,
    Artifact)))).ToBe(True);
  Expect<Boolean>(Rejects(Lock(Package('github:o/r@' + COMMIT, 'tag', COMMIT,
    Artifact)))).ToBe(True);
  Expect<Boolean>(Rejects(Lock(Package(
    'github:o/r@1111111111111111111111111111111111111111', 'commit', COMMIT,
    Artifact)))).ToBe(True);
end;

procedure TLockfileTests.TestRejectsNewerAndOlderVersions;
begin
  Expect<Boolean>(Pos('newer GocciaScript',
    RejectionMessage('{"version": 2, "packages": {}}')) > 0).ToBe(True);
  Expect<Boolean>(Rejects('{"version": 0, "packages": {}}')).ToBe(True);
end;

procedure TLockfileTests.TestRejectsInvalidJSON;
begin
  Expect<Boolean>(Pos('not valid JSON', RejectionMessage('{"version": 1,'))
    > 0).ToBe(True);
  { Comments and trailing commas are JSON5, not JSON. }
  Expect<Boolean>(Rejects('{"version": 1, "packages": {},}')).ToBe(True);
end;

begin
  TestRunnerProgram.AddSuite(TLockfileTests.Create('Provider lockfile'));
  RunGocciaTests;

  ExitCode := TestResultToExitCode;
end.
