program GitRefs.Test;

{$I Shared.inc}

uses
  SysUtils,

  GitRefs,
  TestingPascalLibrary;

const
  MAIN_COMMIT = '1111111111111111111111111111111111111111';
  LIGHT_COMMIT = '2222222222222222222222222222222222222222';
  TAG_OBJECT = '3333333333333333333333333333333333333333';
  TAG_COMMIT = '4444444444444444444444444444444444444444';
  FORK_COMMIT = '5555555555555555555555555555555555555555';

type
  TGitRefsTests = class(TTestSuite)
  private
    FStatus: Integer;
    FBody: TBytes;
    FFail: Boolean;
    FRequestedURL: string;
    function Transport(const AURL: string; out AStatusCode: Integer;
      out ABody: TBytes; out AError: string): Boolean;
    procedure TestParsesTagsHeadsAndPeeledTags;
    procedure TestIgnoresPullAndOtherRefs;
    procedure TestEmptyRepository;
    procedure TestRejectsMalformedAdvertisements;
    procedure TestFetchUsesTheTransport;
  public
    procedure SetupTests; override;
  end;

function Pkt(const ALine: string): string;
begin
  Result := LowerCase(IntToHex(Length(ALine) + 4, 4)) + ALine;
end;

function ToBytes(const AText: string): TBytes;
var
  I: Integer;
begin
  SetLength(Result, Length(AText));
  for I := 1 to Length(AText) do
    Result[I - 1] := Ord(AText[I]);
end;

function Advertisement: TBytes;
begin
  Result := ToBytes(
    Pkt('# service=git-upload-pack'#10) + '0000' +
    Pkt(MAIN_COMMIT + ' HEAD'#0'multi_ack side-band-64k symref=HEAD:refs/heads/main'#10) +
    Pkt(MAIN_COMMIT + ' refs/heads/main'#10) +
    Pkt(FORK_COMMIT + ' refs/pull/7/head'#10) +
    Pkt(LIGHT_COMMIT + ' refs/tags/v1.0.0'#10) +
    Pkt(TAG_OBJECT + ' refs/tags/v2.0.0'#10) +
    Pkt(TAG_COMMIT + ' refs/tags/v2.0.0^{}'#10) +
    '0000');
end;

procedure TGitRefsTests.SetupTests;
begin
  Test('Parses tags, heads, and peeled tags',
    TestParsesTagsHeadsAndPeeledTags);
  Test('Pull-request and other refs are not tips',
    TestIgnoresPullAndOtherRefs);
  Test('An empty repository advertises nothing', TestEmptyRepository);
  Test('Malformed advertisements are refused',
    TestRejectsMalformedAdvertisements);
  Test('Fetching goes through the transport', TestFetchUsesTheTransport);
end;

function TGitRefsTests.Transport(const AURL: string;
  out AStatusCode: Integer; out ABody: TBytes; out AError: string): Boolean;
begin
  FRequestedURL := AURL;
  AStatusCode := FStatus;
  ABody := FBody;
  AError := 'connection refused';
  Result := not FFail;
end;

procedure TGitRefsTests.TestParsesTagsHeadsAndPeeledTags;
var
  Refs: TGitRefAdvertisement;
  Commit: string;
begin
  Refs := TGitRefAdvertisement.CreateFromBytes(Advertisement);
  try
    Expect<Integer>(Refs.Count).ToBe(3);
    Expect<Boolean>(Refs.FindHead('main', Commit)).ToBe(True);
    Expect<string>(Commit).ToBe(MAIN_COMMIT);
    Expect<Boolean>(Refs.FindTag('v1.0.0', Commit)).ToBe(True);
    Expect<string>(Commit).ToBe(LIGHT_COMMIT);
    { An annotated tag resolves to the commit it names, not the tag
      object. }
    Expect<Boolean>(Refs.FindTag('v2.0.0', Commit)).ToBe(True);
    Expect<string>(Commit).ToBe(TAG_COMMIT);
    Expect<Boolean>(Refs.FindTag('main', Commit)).ToBe(False);
    Expect<Boolean>(Refs.FindTag('v3', Commit)).ToBe(False);
    Expect<Boolean>(Refs.IsAdvertisedTip(TAG_COMMIT)).ToBe(True);
    Expect<Boolean>(Refs.IsAdvertisedTip(MAIN_COMMIT)).ToBe(True);
    Expect<Boolean>(Refs.IsAdvertisedTip(TAG_OBJECT)).ToBe(False);
  finally
    Refs.Free;
  end;
end;

procedure TGitRefsTests.TestIgnoresPullAndOtherRefs;
var
  Refs: TGitRefAdvertisement;
begin
  Refs := TGitRefAdvertisement.CreateFromBytes(Advertisement);
  try
    { A fork's commit reachable only through refs/pull is exactly the
      imposter a commit pin must not accept. }
    Expect<Boolean>(Refs.IsAdvertisedTip(FORK_COMMIT)).ToBe(False);
  finally
    Refs.Free;
  end;
end;

procedure TGitRefsTests.TestEmptyRepository;
var
  Refs: TGitRefAdvertisement;
begin
  Refs := TGitRefAdvertisement.CreateFromBytes(ToBytes(
    Pkt('# service=git-upload-pack'#10) + '0000' +
    Pkt('0000000000000000000000000000000000000000 capabilities^{}'#0'x'#10) +
    '0000'));
  try
    Expect<Integer>(Refs.Count).ToBe(0);
  finally
    Refs.Free;
  end;
end;

procedure TGitRefsTests.TestRejectsMalformedAdvertisements;

  function Rejects(const AText: string): Boolean;
  begin
    try
      TGitRefAdvertisement.CreateFromBytes(ToBytes(AText)).Free;
      Result := False;
    except
      on EGitRefsError do
        Result := True;
    end;
  end;

begin
  Expect<Boolean>(Rejects('00')).ToBe(True);
  Expect<Boolean>(Rejects('zzzz')).ToBe(True);
  Expect<Boolean>(Rejects('0003')).ToBe(True);
  Expect<Boolean>(Rejects('00ffshort')).ToBe(True);
  Expect<Boolean>(Rejects(Pkt('nospace'#10))).ToBe(True);
  Expect<Boolean>(Rejects(Pkt('ABCDEF refs/heads/main'#10))).ToBe(True);
  Expect<Boolean>(Rejects(Pkt(MAIN_COMMIT + ' refs/tags/x^{}'#10))).ToBe(True);
  Expect<Boolean>(Rejects(Pkt(MAIN_COMMIT + ' refs/heads/a'#10) +
    Pkt(MAIN_COMMIT + ' refs/heads/a'#10))).ToBe(True);
  Expect<Boolean>(Rejects(Pkt(MAIN_COMMIT + ' refs/heads/'#$C3#$A9#10)))
    .ToBe(True);
end;

procedure TGitRefsTests.TestFetchUsesTheTransport;
var
  Refs: TGitRefAdvertisement;
  Refused: Boolean;
begin
  FStatus := 200;
  FBody := Advertisement;
  FFail := False;
  Refs := FetchRefAdvertisement(GitHubInfoRefsURL('o', 'r'), Transport);
  try
    Expect<string>(FRequestedURL).ToBe(
      'https://github.com/o/r.git/info/refs?service=git-upload-pack');
    Expect<Integer>(Refs.Count).ToBe(3);
  finally
    Refs.Free;
  end;

  FStatus := 404;
  Refused := False;
  try
    FetchRefAdvertisement(GitHubInfoRefsURL('o', 'r'), Transport).Free;
  except
    on EGitRefsError do
      Refused := True;
  end;
  Expect<Boolean>(Refused).ToBe(True);

  FFail := True;
  Refused := False;
  try
    FetchRefAdvertisement(GitHubInfoRefsURL('o', 'r'), Transport).Free;
  except
    on EGitRefsError do
      Refused := True;
  end;
  Expect<Boolean>(Refused).ToBe(True);
end;

begin
  TestRunnerProgram.AddSuite(TGitRefsTests.Create('GitRefs'));
  TestRunnerProgram.Run;

  ExitCode := TestResultToExitCode;
end.
