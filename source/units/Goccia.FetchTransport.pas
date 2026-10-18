unit Goccia.FetchTransport;

// The embedder's fetch transport hook (ADR 0146): types only, so the engine
// can carry a transport without pulling the fetch manager or the socket
// client into its unit closure.

{$I Goccia.inc}

interface

uses
  SysUtils,

  HTTPTypes,

  Goccia.Capabilities;

type
  { One request a script made with fetch(), as the transport receives it.
    The engine has already applied everything fetch() decides before any
    network work: the URL parsed, the destination host allowed by the net
    capability (and audited), the method checked. A request the built-in
    client would refuse to send (an unsupported scheme, a control character
    in the request target or a header) fails with that client's TypeError
    and never reaches the transport. }
  TGocciaFetchTransportRequest = record
    { "GET" or "HEAD", upper case. }
    Method: string;
    { The URL as fetch() received it (a URL object's href), unnormalised,
      exactly what the built-in network client would request. }
    URL: string;
    { The request's header list in the order fetch() collected it. Names from
      a plain object are lower-cased; the built-in client adds its own
      connection headers (Host, Connection, ...) to these on the wire. }
    Headers: THTTPHeaders;
    { The request body. Always empty today: fetch() supports only GET and
      HEAD, which carry no body. }
    Body: TBytes;
    { Milliseconds left until the run's deadline or the request's
      AbortSignal.timeout(), whichever is sooner; zero when neither applies.
      The engine enforces both itself; this is what a transport that does
      real I/O should bound it by. }
    TimeoutMilliseconds: Integer;
    { The response-body ceiling in bytes, already resolved (never zero). A
      response whose body is larger rejects with TypeError. }
    MaxResponseBytes: Integer;
    { The engine's capability set. The engine checked the URL's host by name;
      a transport that performs real I/O must check every redirect hop and
      the address each name resolves to against this set, as
      FetchOverNetwork does. }
    Capabilities: TGocciaCapabilities;
  end;

  { The answer side of one request. Every method may be called from any
    thread; the first answer wins and later ones are ignored. The engine
    settles the fetch() promise on its own thread at its next completion
    pump, never inside the call. }
  IGocciaFetchResponder = interface
    { Answers with a response: the final one, after any redirects the
      transport followed. AResponse.FinalURL empty means the request URL,
      otherwise it must be an absolute http(s) URL; either way Response.url
      excludes its fragment. Header values lose their leading and trailing
      tabs and spaces. The body of a HEAD response or of a 204, 205 or 304
      response is ignored. A status outside 200..599, status text that is
      not a reason phrase (tab, space, visible or non-ASCII characters), a
      header name that is not a token, a header value with NUL, CR or LF, an
      invalid final URL, or a body over the ceiling rejects the fetch with
      TypeError instead. The engine copies what it keeps. Returns False
      when the request was already answered or cancelled. }
    function Respond(const AResponse: THTTPResponse): Boolean;
    { Answers with a network error: the fetch rejects with a TypeError whose
      message is AMessage. A non-empty ADeniedScope means the net capability
      refused a destination (host, plus a non-default port); the fetch then
      rejects with PermissionDenied for that scope, as a refused redirect hop
      does. Returns False when the request was already answered or
      cancelled. }
    function Fail(const AMessage: string;
      const ADeniedScope: string = ''): Boolean;
    { True once the script will not see an answer: the request's signal
      aborted it, its engine discarded it (the run failed or ended), or
      Fetch raised. That holds even for an answer already taken but not yet
      settled. A transport may stop its work then; an answer it still gives
      is refused. A request that settled normally is not cancelled. }
    function IsCancelled: Boolean;
  end;

  { An embedder's transport for fetch(). Installed per engine through
    TGocciaEngine.FetchTransport; with none installed, fetch() performs
    requests over the network as before. }
  IGocciaFetchTransport = interface
    { Called on the engine's thread, once per request, while the script's
      fetch() call runs. Answer through AResponder now or later, from any
      thread; the run waits while the request is unanswered. The wait ends
      at the run's deadline or the request's signal; it executes no
      instructions, so an instruction limit alone does not end it. Must not
      run script or call
      into the engine. An exception raised here cancels the request and
      rejects the fetch with TypeError (`fetch failed: <message>`), even if
      the transport answered first. }
    procedure Fetch(const ARequest: TGocciaFetchTransportRequest;
      const AResponder: IGocciaFetchResponder);
  end;

implementation

end.
