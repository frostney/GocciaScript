unit GitRefs;

{ Git ref advertisements over smart HTTP, without git.

  One GET of `https://github.com/<owner>/<repo>.git/info/refs?service=
  git-upload-pack` answers with the repository's refs as pkt-lines
  (https://git-scm.com/docs/http-protocol, https://git-scm.com/docs/
  protocol-common#_pkt_line_format). This unit parses that answer and keeps
  only branch tips (`refs/heads/*`) and tags (`refs/tags/*`, peeled to the
  commit an annotated tag names). Every other ref — `refs/pull/*` above all,
  which advertises commits pushed to forks — is dropped, so "the commit is an
  advertised tip" means a tag or a branch of this repository points at it.

  The transport is a parameter, so the unit depends on nothing else here and
  tests feed it recorded advertisements. }

{$I Shared.inc}

interface

uses
  SysUtils;

type
  EGitRefsError = class(Exception);

  TGitRefKind = (grkTag, grkHead);

  TGitRef = record
    { The full name, `refs/tags/v1.0.0` or `refs/heads/main`. }
    Name: string;
    Kind: TGitRefKind;
    { The object the ref names: a commit, or an annotated tag object. }
    ObjectId: string;
    { The commit: the peeled object for an annotated tag, else ObjectId. }
    Commit: string;
  end;

  TGitRefArray = array of TGitRef;

  { One GET of AURL. True with the HTTP status and body; False with AError
    when no response arrived. }
  TGitRefsTransport = function(const AURL: string; out AStatusCode: Integer;
    out ABody: TBytes; out AError: string): Boolean of object;

  TGitRefAdvertisement = class
  private
    FRefs: TGitRefArray;
    function IndexOf(const AName: string): Integer;
  public
    { Parses a smart-HTTP v0/v1 advertisement. Raises EGitRefsError for a
      malformed one. }
    constructor CreateFromBytes(const ABody: TBytes);
    { The commit tag ATag (short name) points at, peeled. }
    function FindTag(const ATag: string; out ACommit: string): Boolean;
    { The commit branch AHead (short name) points at. }
    function FindHead(const AHead: string; out ACommit: string): Boolean;
    { Whether ACommit is the commit of an advertised tag or branch. }
    function IsAdvertisedTip(const ACommit: string): Boolean;
    function Count: Integer;
    function Ref(const AIndex: Integer): TGitRef;
  end;

const
  GIT_TAG_PREFIX = 'refs/tags/';
  GIT_HEAD_PREFIX = 'refs/heads/';
  GIT_OBJECT_ID_LENGTH = 40;

{ `https://github.com/<owner>/<repo>.git/info/refs?service=git-upload-pack`. }
function GitHubInfoRefsURL(const AOwner, ARepository: string): string;

{ Fetches and parses the advertisement at AURL. Raises EGitRefsError when
  the transport fails, the status is not 200, or the body is malformed. }
function FetchRefAdvertisement(const AURL: string;
  const ATransport: TGitRefsTransport): TGitRefAdvertisement;

function IsGitObjectId(const AValue: string): Boolean;

implementation

const
  PKT_LENGTH_DIGITS = 4;
  PKT_FLUSH = 0;
  PEELED_SUFFIX = '^{}';
  SERVICE_LINE_PREFIX = '# service=';
  VERSION_LINE_PREFIX = 'version ';
  EMPTY_REPOSITORY_REF = 'capabilities^{}';
  HTTP_STATUS_OK = 200;
  GITHUB_BASE_URL = 'https://github.com/';
  INFO_REFS_SUFFIX = '.git/info/refs?service=git-upload-pack';

function GitHubInfoRefsURL(const AOwner, ARepository: string): string;
begin
  Result := GITHUB_BASE_URL + AOwner + '/' + ARepository + INFO_REFS_SUFFIX;
end;

function IsGitObjectId(const AValue: string): Boolean;
var
  I: Integer;
begin
  if Length(AValue) <> GIT_OBJECT_ID_LENGTH then
    Exit(False);
  for I := 1 to Length(AValue) do
    if not (((AValue[I] >= '0') and (AValue[I] <= '9')) or
      ((AValue[I] >= 'a') and (AValue[I] <= 'f'))) then
      Exit(False);
  Result := True;
end;

function HexDigit(const AByte: Byte): Integer;
begin
  case Chr(AByte) of
    '0'..'9':
      Result := AByte - Ord('0');
    'a'..'f':
      Result := AByte - Ord('a') + 10;
    'A'..'F':
      Result := AByte - Ord('A') + 10;
  else
    Result := -1;
  end;
end;

function BytesToText(const ABytes: TBytes; const AStart,
  ACount: Integer): string;
var
  I: Integer;
begin
  SetLength(Result, ACount);
  for I := 1 to ACount do
  begin
    { Ref lines are ASCII; anything else is refused rather than decoded. }
    if ABytes[AStart + I - 1] > $7F then
      raise EGitRefsError.Create('ref advertisement is not ASCII');
    Result[I] := Chr(ABytes[AStart + I - 1]);
  end;
end;

{ TGitRefAdvertisement }

constructor TGitRefAdvertisement.CreateFromBytes(const ABody: TBytes);
var
  Position, PacketLength, Digit, I, SpaceIndex, NulIndex, Index: Integer;
  Line, ObjectId, Name, BaseName: string;
  Peeled: array of TGitRef;
  Ref: TGitRef;
begin
  inherited Create;
  FRefs := nil;
  Peeled := nil;
  Position := 0;
  while Position < Length(ABody) do
  begin
    if Position + PKT_LENGTH_DIGITS > Length(ABody) then
      raise EGitRefsError.Create('truncated pkt-line length');
    PacketLength := 0;
    for I := 0 to PKT_LENGTH_DIGITS - 1 do
    begin
      Digit := HexDigit(ABody[Position + I]);
      if Digit < 0 then
        raise EGitRefsError.Create('malformed pkt-line length');
      PacketLength := PacketLength * 16 + Digit;
    end;
    if PacketLength = PKT_FLUSH then
    begin
      Inc(Position, PKT_LENGTH_DIGITS);
      Continue;
    end;
    if (PacketLength < PKT_LENGTH_DIGITS) or
       (Position + PacketLength > Length(ABody)) then
      raise EGitRefsError.Create('malformed pkt-line length');
    Line := BytesToText(ABody, Position + PKT_LENGTH_DIGITS,
      PacketLength - PKT_LENGTH_DIGITS);
    Inc(Position, PacketLength);

    if (Line <> '') and (Line[Length(Line)] = #10) then
      SetLength(Line, Length(Line) - 1);
    if (Copy(Line, 1, Length(SERVICE_LINE_PREFIX)) = SERVICE_LINE_PREFIX) or
       (Copy(Line, 1, Length(VERSION_LINE_PREFIX)) = VERSION_LINE_PREFIX) then
      Continue;
    NulIndex := Pos(#0, Line);
    if NulIndex > 0 then
      Line := Copy(Line, 1, NulIndex - 1);
    SpaceIndex := Pos(' ', Line);
    if SpaceIndex = 0 then
      raise EGitRefsError.Create('malformed ref line');
    ObjectId := Copy(Line, 1, SpaceIndex - 1);
    Name := Copy(Line, SpaceIndex + 1, MaxInt);
    if not IsGitObjectId(ObjectId) then
      raise EGitRefsError.Create('malformed object id in ref line');
    if Name = EMPTY_REPOSITORY_REF then
      Continue;

    Ref := Default(TGitRef);
    if Copy(Name, Length(Name) - Length(PEELED_SUFFIX) + 1,
       Length(PEELED_SUFFIX)) = PEELED_SUFFIX then
    begin
      BaseName := Copy(Name, 1, Length(Name) - Length(PEELED_SUFFIX));
      if Copy(BaseName, 1, Length(GIT_TAG_PREFIX)) <> GIT_TAG_PREFIX then
        Continue;
      Ref.Name := BaseName;
      Ref.Commit := ObjectId;
      SetLength(Peeled, Length(Peeled) + 1);
      Peeled[High(Peeled)] := Ref;
      Continue;
    end;

    if Copy(Name, 1, Length(GIT_TAG_PREFIX)) = GIT_TAG_PREFIX then
      Ref.Kind := grkTag
    else if Copy(Name, 1, Length(GIT_HEAD_PREFIX)) = GIT_HEAD_PREFIX then
      Ref.Kind := grkHead
    else
      Continue;
    Ref.Name := Name;
    Ref.ObjectId := ObjectId;
    Ref.Commit := ObjectId;
    if IndexOf(Name) >= 0 then
      raise EGitRefsError.CreateFmt('ref %s is advertised twice', [Name]);
    SetLength(FRefs, Length(FRefs) + 1);
    FRefs[High(FRefs)] := Ref;
  end;

  { A peeled line names the commit of the annotated tag above it. }
  for I := 0 to High(Peeled) do
  begin
    Index := IndexOf(Peeled[I].Name);
    if Index < 0 then
      raise EGitRefsError.CreateFmt('peeled ref %s has no tag',
        [Peeled[I].Name]);
    FRefs[Index].Commit := Peeled[I].Commit;
  end;
end;

function TGitRefAdvertisement.IndexOf(const AName: string): Integer;
var
  I: Integer;
begin
  for I := 0 to High(FRefs) do
    if FRefs[I].Name = AName then
      Exit(I);
  Result := -1;
end;

function TGitRefAdvertisement.FindTag(const ATag: string;
  out ACommit: string): Boolean;
var
  Index: Integer;
begin
  ACommit := '';
  Index := IndexOf(GIT_TAG_PREFIX + ATag);
  Result := Index >= 0;
  if Result then
    ACommit := FRefs[Index].Commit;
end;

function TGitRefAdvertisement.FindHead(const AHead: string;
  out ACommit: string): Boolean;
var
  Index: Integer;
begin
  ACommit := '';
  Index := IndexOf(GIT_HEAD_PREFIX + AHead);
  Result := Index >= 0;
  if Result then
    ACommit := FRefs[Index].Commit;
end;

function TGitRefAdvertisement.IsAdvertisedTip(const ACommit: string): Boolean;
var
  I: Integer;
begin
  for I := 0 to High(FRefs) do
    if FRefs[I].Commit = ACommit then
      Exit(True);
  Result := False;
end;

function TGitRefAdvertisement.Count: Integer;
begin
  Result := Length(FRefs);
end;

function TGitRefAdvertisement.Ref(const AIndex: Integer): TGitRef;
begin
  Result := FRefs[AIndex];
end;

function FetchRefAdvertisement(const AURL: string;
  const ATransport: TGitRefsTransport): TGitRefAdvertisement;
var
  Body: TBytes;
  Error: string;
  StatusCode: Integer;
begin
  if not ATransport(AURL, StatusCode, Body, Error) then
    raise EGitRefsError.CreateFmt('fetching %s failed: %s', [AURL, Error]);
  if StatusCode <> HTTP_STATUS_OK then
    raise EGitRefsError.CreateFmt('fetching %s failed with HTTP %d',
      [AURL, StatusCode]);
  Result := TGitRefAdvertisement.CreateFromBytes(Body);
end;

end.
