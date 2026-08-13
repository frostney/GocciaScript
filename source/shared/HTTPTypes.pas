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

const
  { Ceiling on a response body when the caller states no preference. Matches
    the limit that was hard-coded in HTTPClient before it became configurable,
    so an embedder that never sets a policy sees no behavior change. }
  DEFAULT_MAX_RESPONSE_BODY_BYTES = 8 * 1024 * 1024;

function DefaultHTTPPolicy: THTTPRequestPolicy;

implementation

function DefaultHTTPPolicy: THTTPRequestPolicy;
begin
  Result.DenyPrivateRanges := False;
  Result.MaxResponseBytes := DEFAULT_MAX_RESPONSE_BODY_BYTES;
end;

end.
