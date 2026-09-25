unit HTTPTypes;

// The wire-facing TYPES of the HTTP client, split out of HTTPClient
// so abstraction layers (Goccia.FetchManager's abstract base, the
// fetch builtin's request assembly) can name THTTPHeaders without
// pulling the socket implementation into their unit closure —
// HTTPClient itself does not compile on runtimes without BSD
// sockets (the Lakon WASM lane). HTTPClient re-exports these under
// their original names, so existing native consumers are untouched.

{$I Shared.inc}

interface

uses
  SysUtils;

type
  THTTPHeader = record
    Name: string;
    Value: string;
  end;

  THTTPHeaders = array of THTTPHeader;

  THTTPResponse = record
    StatusCode: Integer;
    StatusText: string;
    Headers: THTTPHeaders;
    Body: TBytes;
    FinalURL: string;
    Redirected: Boolean;
  end;

  EHTTPError = class(Exception);

  { Per-request network policy.

    Separate from the allowlist because the allowlist answers "which names may
    this script reach" while these answer "what may those names resolve to,
    and how much may they return" — questions the allowlist cannot express.
    Use DefaultHTTPPolicy to get the historical behavior.

    Lives here rather than in HTTPClient for the same reason THTTPHeaders
    does: the fetch manager's abstract base carries a policy in its interface
    and must not pull the socket implementation into its unit closure. }
  THTTPRequestPolicy = record
    { Reject targets that resolve into RFC1918, loopback, link-local, CGNAT,
      or IPv6 ULA/loopback space. Applied to the *resolved address*, on the
      initial request and on every redirect hop. }
    DenyPrivateRanges: Boolean;
    { Hard ceiling on the response body, in bytes. Zero selects
      DEFAULT_MAX_RESPONSE_BODY_BYTES. }
    MaxResponseBytes: Integer;
  end;

  THTTPParsedURL = record
    Scheme: string;
    Host: string;
    Port: Integer;
    Path: string;
  end;

const
  { Ceiling on a response body when the caller states no preference. Matches
    the limit that was hard-coded in HTTPClient before it became configurable,
    so an embedder that never sets a policy sees no behavior change. }
  DEFAULT_MAX_RESPONSE_BODY_BYTES = 8 * 1024 * 1024;

function DefaultHTTPPolicy: THTTPRequestPolicy;

{ Parses an absolute http(s) URL into scheme/host/port/path. This is pure
  string work with no socket dependency, so it lives here rather than in
  HTTPClient: socket-free layers (the fetch builtin's host validation, the
  Lakon WASM lane) can canonicalize a URL without pulling the socket closure.
  HTTPClient re-exports these under their original names. }
function ParseHTTPURL(const AURL: string;
  const ARequireSupportedScheme: Boolean = True;
  const AAllowUserInfo: Boolean = False): THTTPParsedURL;
function HTTPURLHost(const AURL: string): string;
function HTTPURLAuditHost(const AURL: string): string;

implementation

function DefaultHTTPPolicy: THTTPRequestPolicy;
begin
  Result.DenyPrivateRanges := False;
  Result.MaxResponseBytes := DEFAULT_MAX_RESPONSE_BODY_BYTES;
end;

{ A bracketed authority must hold an IPv6 literal, not an arbitrary name:
  without this check ParseHTTPURL would strip the brackets off
  `[allowed.example]` and hand back `allowed.example` as a valid host, so a
  malformed URL would slip past the invalid-URL reject+audit path in
  TGocciaGlobalFetch.ValidateHost. This is deliberately a plausibility gate,
  not a full RFC 4291 parser: the address portion must contain a ':' and use
  only IPv6-legal characters (hex digits, ':', '.', for embedded IPv4), with an
  optional non-empty '%zone' suffix (RFC 6874), which is enough to keep a
  hostname from masquerading as an IPv6 literal. }
function IsPlausibleIPv6Literal(const AContent: string): Boolean;
var
  I, PercentPos: Integer;
  AddressPart: string;
  HasColon: Boolean;
begin
  Result := False;
  if AContent = '' then
    Exit;
  PercentPos := Pos('%', AContent);
  if PercentPos > 0 then
  begin
    // A zone identifier follows '%'; require it to be non-empty and validate
    // only the address portion that precedes it.
    if PercentPos = Length(AContent) then
      Exit;
    AddressPart := Copy(AContent, 1, PercentPos - 1);
  end
  else
    AddressPart := AContent;
  if AddressPart = '' then
    Exit;
  HasColon := False;
  for I := 1 to Length(AddressPart) do
    case AddressPart[I] of
      '0'..'9', 'a'..'f', 'A'..'F', '.':
        ;
      ':':
        HasColon := True;
    else
      Exit;
    end;
  Result := HasColon;
end;

{ A URI scheme must match RFC 3986 §3.1 syntax —
  scheme = ALPHA *( ALPHA / DIGIT / "+" / "-" / "." ) — i.e. non-empty, first
  character a letter, remaining characters letters/digits/'+'/'-'/'.'. Without
  this gate ParseHTTPURL would accept an empty scheme (`://allowed.example`) or
  one with illegal characters (`ht*tp://allowed.example`) whenever
  ARequireSupportedScheme is False, returning a bare host that skips the fetch
  invalid-URL reject+audit path in TGocciaGlobalFetch.ValidateHost. The scheme
  is validated on the raw (case-preserved) text; letter-case does not affect
  validity. }
function IsValidURIScheme(const AScheme: string): Boolean;
var
  I: Integer;
begin
  Result := False;
  if AScheme = '' then
    Exit;
  case AScheme[1] of
    'a'..'z', 'A'..'Z':
      ;
  else
    Exit;
  end;
  for I := 2 to Length(AScheme) do
    case AScheme[I] of
      'a'..'z', 'A'..'Z', '0'..'9', '+', '-', '.':
        ;
    else
      Exit;
    end;
  Result := True;
end;

function ParseHTTPURL(const AURL: string;
  const ARequireSupportedScheme: Boolean = True;
  const AAllowUserInfo: Boolean = False): THTTPParsedURL;
var
  S, Rest, HostContent, RawScheme: string;
  I, AuthorityEnd, ColonCount, ParsedPort: Integer;
  PortText: string;
begin
  Result.Scheme := '';
  Result.Host := '';
  Result.Port := 0;
  Result.Path := '/';

  S := AURL;
  for I := 1 to Length(S) do
    if (Ord(S[I]) < 32) or (Ord(S[I]) = 127) then
      raise EHTTPError.Create('Invalid URL: control character');

  // Scheme
  I := Pos('://', S);
  if I > 0 then
  begin
    RawScheme := Copy(S, 1, I - 1);
    if not IsValidURIScheme(RawScheme) then
      raise EHTTPError.Create('Invalid URL: malformed scheme');
    Result.Scheme := LowerCase(RawScheme);
    Rest := Copy(S, I + 3, Length(S));
  end
  else
    raise EHTTPError.Create('Invalid URL: missing scheme');

  if ARequireSupportedScheme and
     (Result.Scheme <> 'http') and (Result.Scheme <> 'https') then
    raise EHTTPError.Create('Unsupported scheme: ' + Result.Scheme);

  AuthorityEnd := Length(Rest) + 1;
  for I := 1 to Length(Rest) do
    if Rest[I] in ['/', '?', '#'] then
    begin
      AuthorityEnd := I;
      Break;
    end;
  if AuthorityEnd <= Length(Rest) then
  begin
    Result.Path := Copy(Rest, AuthorityEnd, MaxInt);
    Rest := Copy(Rest, 1, AuthorityEnd - 1);
    I := Pos('#', Result.Path);
    if I > 0 then
      Delete(Result.Path, I, MaxInt);
    if (Result.Path <> '') and (Result.Path[1] = '?') then
      Result.Path := '/' + Result.Path;
  end;

  I := LastDelimiter('@', Rest);
  if I > 0 then
  begin
    if not AAllowUserInfo then
      raise EHTTPError.Create('Invalid URL: userinfo is not allowed');
    Rest := Copy(Rest, I + 1, MaxInt);
  end;

  // Parse host:port
  if (Length(Rest) > 0) and (Rest[1] = '[') then
  begin
    // IPv6 — strip brackets for DNS resolution
    I := Pos(']', Rest);
    if I > 1 then
    begin
      HostContent := Copy(Rest, 2, I - 2);
      if not IsPlausibleIPv6Literal(HostContent) then
        raise EHTTPError.Create('Invalid URL: malformed IPv6 authority');
      Result.Host := LowerCase(HostContent);
      Rest := Copy(Rest, I + 1, Length(Rest));
      if (Length(Rest) > 0) and (Rest[1] = ':') then
      begin
        PortText := Copy(Rest, 2, MaxInt);
        ParsedPort := StrToIntDef(PortText, -1);
        if (ParsedPort < 1) or (ParsedPort > 65535) then
          raise EHTTPError.Create('Invalid URL: invalid port');
        Result.Port := ParsedPort;
      end
      else if Rest <> '' then
        raise EHTTPError.Create('Invalid URL: malformed IPv6 authority');
    end
    else
      raise EHTTPError.Create('Invalid URL: malformed IPv6 authority');
  end
  else
  begin
    ColonCount := 0;
    for I := 1 to Length(Rest) do
      if Rest[I] = ':' then
        Inc(ColonCount);
    if ColonCount > 1 then
      raise EHTTPError.Create('Invalid URL: IPv6 address must use brackets');
    I := Pos(':', Rest);
    if I > 0 then
    begin
      Result.Host := LowerCase(Copy(Rest, 1, I - 1));
      PortText := Copy(Rest, I + 1, MaxInt);
      ParsedPort := StrToIntDef(PortText, -1);
      if (ParsedPort < 1) or (ParsedPort > 65535) then
        raise EHTTPError.Create('Invalid URL: invalid port');
      Result.Port := ParsedPort;
    end
    else
      Result.Host := LowerCase(Rest);
  end;

  if Result.Host = '' then
    raise EHTTPError.Create('Invalid URL: empty host');

  // Default ports
  if Result.Port = 0 then
  begin
    if Result.Scheme = 'https' then
      Result.Port := 443
    else
      Result.Port := 80;
  end;

  if Result.Path = '' then
    Result.Path := '/';
end;

function HTTPURLHost(const AURL: string): string;
begin
  // Host validation is synchronous, but fetch network failures (including an
  // unsupported scheme) are delivered through the returned promise. Parse the
  // authority canonically here while leaving scheme enforcement to DoRequest.
  Result := ParseHTTPURL(AURL, False).Host;
end;

function HTTPURLAuditHost(const AURL: string): string;
begin
  // This parser is only for capability-audit subjects. It canonicalizes the
  // destination after userinfo without relaxing request validation.
  Result := ParseHTTPURL(AURL, False, True).Host;
end;

end.
