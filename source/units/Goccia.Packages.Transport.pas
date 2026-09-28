unit Goccia.Packages.Transport;

{ The network seam of provider packages (ADR 0122).

  A provider fetches from hosts its implementation fixes, never from a host
  a script, an import map, or a lockfile names. Every request is a GET with
  no body and no credentials, pinned to one host on every hop (a redirect to
  any other host, or to another port, is refused before it is sent), with
  private, loopback, and link-local destinations refused, a body ceiling,
  and a deadline.

  Tests substitute a fixture transport. Nothing in a shipped binary can
  redirect a provider host: there is no option or environment variable for
  it. }

{$I Goccia.inc}

interface

uses
  Classes,
  SysUtils,

  HTTPTypes;

const
  { Bounds one request, connect through body. }
  PROVIDER_REQUEST_TIMEOUT_MILLISECONDS = 60000;
  { Ceiling on one package file. }
  PROVIDER_MAX_ARTIFACT_BYTES = 64 * 1024 * 1024;
  PROVIDER_HTTPS_PORT = 443;

type
  EGocciaProviderTransportError = class(Exception);

  TGocciaProviderResponse = record
    StatusCode: Integer;
    Body: TBytes;
  end;

  TGocciaProviderTransport = class
  public
    { One GET of AURL, which must be an https URL on AHost. Raises
      EGocciaProviderTransportError when the request cannot be made or is
      refused; any HTTP status is returned. }
    function Get(const AURL, AHost: string;
      const AMaxBytes: Integer): TGocciaProviderResponse; virtual; abstract;
  end;

  TGocciaHTTPProviderTransport = class(TGocciaProviderTransport)
  private
    FPinnedHost: string;
    function CheckHost(const AHost: string; const APort: Integer;
      const AResolvedAddress: string; out AReason: string): Boolean;
  protected
    { The request itself. Tests override it to observe the pinning without
      network access. }
    function Send(const AURL: string; const AAllowedHosts: TStrings;
      const ATimeoutMilliseconds: Integer;
      const APolicy: THTTPRequestPolicy): THTTPResponse; virtual;
  public
    function Get(const AURL, AHost: string;
      const AMaxBytes: Integer): TGocciaProviderResponse; override;
  end;

implementation

uses
  HTTPClient;

const
  HTTPS_SCHEME = 'https';

function TGocciaHTTPProviderTransport.CheckHost(const AHost: string;
  const APort: Integer; const AResolvedAddress: string;
  out AReason: string): Boolean;
begin
  Result := SameText(AHost, FPinnedHost) and
    (APort = PROVIDER_HTTPS_PORT);
  if not Result then
    AReason := 'a provider request may reach only ' + FPinnedHost + ':' +
      IntToStr(PROVIDER_HTTPS_PORT);
end;

function TGocciaHTTPProviderTransport.Send(const AURL: string;
  const AAllowedHosts: TStrings; const ATimeoutMilliseconds: Integer;
  const APolicy: THTTPRequestPolicy): THTTPResponse;
var
  Headers: THTTPHeaders;
begin
  SetLength(Headers, 0);
  Result := HTTPGet(AURL, Headers, AAllowedHosts, ATimeoutMilliseconds,
    APolicy);
end;

function TGocciaHTTPProviderTransport.Get(const AURL, AHost: string;
  const AMaxBytes: Integer): TGocciaProviderResponse;
var
  AllowedHosts: TStringList;
  Parsed: THTTPParsedURL;
  Policy: THTTPRequestPolicy;
  Response: THTTPResponse;
begin
  try
    Parsed := ParseHTTPURL(AURL);
  except
    on E: Exception do
      raise EGocciaProviderTransportError.CreateFmt(
        'not a provider URL: %s', [AURL]);
  end;
  if (LowerCase(Parsed.Scheme) <> HTTPS_SCHEME) or
     not SameText(Parsed.Host, AHost) or
     (Parsed.Port <> PROVIDER_HTTPS_PORT) then
    raise EGocciaProviderTransportError.CreateFmt(
      'a provider request may reach only https://%s/: %s', [AHost, AURL]);

  FPinnedHost := LowerCase(AHost);
  Policy := DefaultHTTPPolicy;
  Policy.DenyPrivateRanges := True;
  Policy.MaxResponseBytes := AMaxBytes;
  Policy.HostCheck := CheckHost;
  AllowedHosts := TStringList.Create;
  try
    AllowedHosts.Add(FPinnedHost);
    try
      Response := Send(AURL, AllowedHosts,
        PROVIDER_REQUEST_TIMEOUT_MILLISECONDS, Policy);
    except
      on E: EGocciaProviderTransportError do
        raise;
      on E: Exception do
        raise EGocciaProviderTransportError.Create(E.Message);
    end;
  finally
    AllowedHosts.Free;
  end;
  Result.StatusCode := Response.StatusCode;
  Result.Body := Response.Body;
end;

end.
