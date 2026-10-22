unit Goccia.FetchManager;

{$I Goccia.inc}

interface

// Interface dependencies are TYPES ONLY (HTTPTypes carries
// THTTPHeaders); the socket-backed client stays behind the LAKON
// gate in the implementation.

uses
  Classes,

  CriticalSections,
  HTTPTypes,

  Goccia.Capabilities,
  Goccia.CapabilityAudit,
  Goccia.FetchTransport,
  Goccia.Realm,
  Goccia.Values.AbortValue,
  Goccia.Values.PromiseValue;

type
  { The dispatching engine's network policy, carried by every request rather
    than held by the thread's manager: engines on one thread may hold
    different sets, and nested engines run inside their parent's call
    (ADR 0122). Built from host configuration, never from script space. }
  TGocciaFetchPolicy = record
    { The engine's capability set; its net rules are re-checked on every hop,
      redirects included, against the host name and the resolved address. }
    Capabilities: TGocciaCapabilities;
    { Response-body ceiling in bytes; zero selects the default. }
    MaxResponseBytes: Integer;
    { Receives the per-hop net.fetch decisions made while the request ran,
      attributed to AuditSource (the fetch() call). They are delivered on the
      runtime thread when the completion arrives, even after an abort, until
      the engine discards its requests. }
    AuditEmitter: TGocciaCapabilityAuditSourcedEmitter;
    AuditSource: TGocciaCapabilityAuditSource;
    { The engine's fetch transport (ADR 0146), or nil for the built-in
      network client. A request handed to a transport makes no hop decisions
      of its own: its audit trail is the dispatch and the name check fetch()
      made before the hand-over. }
    Transport: IGocciaFetchTransport;
  end;

  { One manager serves every engine on a thread: engines nest there (a sandbox
    runScript child runs inside its parent's call), and they share the worker
    cap. The manager therefore holds no per-engine state of its own. Each
    request carries its engine's network policy, and is tagged with that
    engine's realm, which scopes waits and discards to the engine that started
    the request and is the realm its settlement values are created in. }
  TGocciaFetchManager = class
  public
    class function Instance: TGocciaFetchManager;
    { Registers one user of this thread's manager, creating it on first use.
      The manager lives until its last user calls ReleaseInstance, so a nested
      engine that ends cannot free the manager an outer engine is still using —
      possibly from inside one of the manager's own methods further up the
      stack. Pair every AcquireInstance with exactly one ReleaseInstance. }
    class procedure AcquireInstance;
    class procedure ReleaseInstance;

    { APolicy is the dispatching engine's network policy (capability set,
      response ceiling, audit emitter). It is passed per request rather than
      stored here because engines on one thread may hold different policies;
      the caller reads it from host configuration, never from script space.
      ARealm is the dispatching engine's realm. ARedirectMode is the
      script's `redirect` option. }
    procedure StartFetch(const AURL, AMethod: string;
      const AHeaders: THTTPHeaders; const APolicy: TGocciaFetchPolicy;
      const ARealm: TGocciaRealm; const APromise: TGocciaPromiseValue;
      const ASignal: TGocciaAbortSignalValue = nil;
      const ARedirectMode: THTTPRedirectMode = hrmFollow); virtual; abstract;
    function PumpCompletions: Integer; virtual; abstract;
    { True while any engine on the thread has a request in flight. }
    function HasPending: Boolean; virtual; abstract;
    { True while the engine owning ARealm has a request in flight. }
    function HasPendingFor(const ARealm: TGocciaRealm): Boolean;
      virtual; abstract;
    function WaitForPromise(const APromise: TGocciaPromiseValue): Boolean; virtual; abstract;
    { Waits until the engine owning ARealm has no request in flight. }
    procedure WaitForIdle(const ARealm: TGocciaRealm); virtual; abstract;
    { Detaches the in-flight requests of the engine owning ARealm; their late
      completions are discarded. Other engines' requests are untouched. }
    procedure DiscardPending(const ARealm: TGocciaRealm); virtual; abstract;
  end;

procedure DrainMicrotasksAndFetchCompletions;
function WaitForFetchPromise(const APromise: TGocciaPromiseValue): Boolean;
{ The realm-less forms act on the current realm's engine. }
procedure WaitForFetchIdle; overload;
procedure WaitForFetchIdle(const ARealm: TGocciaRealm); overload;
{ True while the engine owning ARealm has a request in flight. }
function HasPendingFetches(const ARealm: TGocciaRealm): Boolean;
procedure DiscardFetchCompletions; overload;
procedure DiscardFetchCompletions(const ARealm: TGocciaRealm); overload;

{ Performs ARequest over the network exactly as fetch() does with no
  transport installed — the net capability checked on every redirect hop and
  against every resolved address, the request's redirect mode, the response
  ceiling, the timeout — and
  answers AResponder with the result. Blocking: a transport that forwards to
  the network (to record what it answers, say) calls it from a thread of its
  own. The hop decisions it makes are not audited. Its answer passes the
  responder's rules, which a conforming server's response meets unchanged.
  Where no network client exists (the WASM lane) it answers with a network
  error. }
procedure FetchOverNetwork(const ARequest: TGocciaFetchTransportRequest;
  const AResponder: IGocciaFetchResponder);

implementation

// The DEFAULT WORKER BACKEND (TThread + HTTPClient + polling Sleep)
// is gated on platforms with real threads and sockets; the Lakon
// WASM lane compiles only the abstract manager and the drain/wait
// helpers — AcquireInstance leaves Instance nil there, so fetch() is
// unavailable until a WASI backend exists (the host-integration
// slice). Everything the shared helpers need stays outside the gate.

uses
  {$IFNDEF LAKON}
  Generics.Collections,
  SyncObjs,

  HTTPClient,

  Goccia.Error.Suggestions,
  Goccia.URL.Parser,
  Goccia.Values.ErrorHelper,
  Goccia.Values.HeadersValue,
  Goccia.Values.ResponseValue,
  {$ENDIF}
  SysUtils,

  Goccia.Builtins.Atomics,
  Goccia.GarbageCollector,
  Goccia.InstructionLimit,
  Goccia.MicrotaskQueue,
  Goccia.Timeout,
  Goccia.Timers;

const
  FETCH_POLL_INTERVAL_MS = 1;
  FETCH_NETWORK_UNAVAILABLE_ERROR = 'no fetch backend is available';

{$IFNDEF LAKON}
const
  FETCH_WORKER_STACK_SIZE = 8 * 1024 * 1024;
  MAX_FETCH_WORKERS = 16;
  FETCH_WORKER_LIMIT_ERROR = 'fetch worker limit exceeded';
  { The decision stays allow — the request was allowed and dispatched — and
    the `abandoned:` tag tells this event apart from a destination check. }
  FETCH_ABANDONED_AUDIT_REASON = 'abandoned: the request was aborted; hop ' +
    'decisions its worker reports after the engine ends are not audited';
  FETCH_TRANSPORT_INVALID_STATUS_ERROR =
    'fetch transport answered an invalid status: %d';
  FETCH_TRANSPORT_INVALID_HEADER_ERROR =
    'fetch transport answered an invalid header';
  FETCH_TRANSPORT_INVALID_STATUS_TEXT_ERROR =
    'fetch transport answered an invalid status text';
  FETCH_TRANSPORT_INVALID_URL_ERROR =
    'fetch transport answered an invalid URL';
  { The built-in client's wording, so a transport's oversized answer reads
    as the network's would. }
  FETCH_RESPONSE_BODY_LIMIT_ERROR = 'HTTP response body exceeds %d byte limit';
  { WHATWG Fetch: a response a fetch can settle with has a status in the
    range 200 to 599 (the Response constructor's range; 1xx responses are
    interim and never final). }
  FETCH_MIN_RESPONSE_STATUS = 200;
  FETCH_MAX_RESPONSE_STATUS = 599;
  HTTP_STATUS_NO_CONTENT = 204;
  HTTP_STATUS_RESET_CONTENT = 205;
  HTTP_STATUS_NOT_MODIFIED = 304;
  FETCH_METHOD_HEAD = 'HEAD';
  LAST_BYTE_CHARACTER = #$00FF;

  EXCHANGE_OPEN = 0;
  EXCHANGE_ANSWERED = 1;
  EXCHANGE_CANCELLED = 2;

type
  { One net decision a worker made for a hop, replayed to the audit sink on
    the runtime thread when the request settles. }
  TGocciaFetchHopDecision = record
    Allowed: Boolean;
    Host: string;
    Reason: string;
  end;

  TGocciaFetchCompletion = class
  public
    RequestID: Integer;
    Success: Boolean;
    Response: THTTPResponse;
    ErrorMessage: string;
    { Non-empty when the capability refused a hop: the guest-visible net scope
      for the PermissionDenied the request rejects with. }
    DeniedScope: string;
    HopDecisions: array of TGocciaFetchHopDecision;
    constructor Create(const ARequestID: Integer);
  end;

  TGocciaFetchLimiter = class
  private
    FLock: TGocciaCriticalSection;
    FActiveWorkers: Integer;
    FRefCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddRef;
    procedure Release;
    function TryAcquireWorker: Boolean;
    procedure ReleaseWorker;
  end;

  TGocciaFetchState = class
  private
    FLock: TGocciaCriticalSection;
    FCompletions: TList<TGocciaFetchCompletion>;
    FAcceptCompletions: Boolean;
    FRefCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddRef;
    procedure Release;
    function PostCompletion(
      const ACompletion: TGocciaFetchCompletion): Boolean;
    function PopCompletion(out ACompletion: TGocciaFetchCompletion): Boolean;
    procedure ClearCompletions;
    procedure Abandon;
  end;

  { The manager's side of a request handed to a transport. The pending entry
    holds it until the request settles, aborts or is discarded, which
    cancels it; the transport holds it as its responder until it answers. }
  IGocciaFetchExchange = interface(IGocciaFetchResponder)
    { Runtime thread only. An abort or a discard: no answer is wanted any
      more, and IsCancelled says so. }
    procedure Cancel;
    { Runtime thread only. The request settled: later answers are refused,
      but it was not cancelled. }
    procedure Close;
  end;

  TGocciaPendingFetch = record
    RequestID: Integer;
    AuditEmitter: TGocciaCapabilityAuditSourcedEmitter;
    AuditSource: TGocciaCapabilityAuditSource;
    // The request's host, the subject of the abandonment event an abort
    // records, as it is of every net.fetch event (never the URL's path or
    // query).
    AuditSubject: string;
    // Realm of the engine that started the request; see TGocciaFetchManager.
    Realm: TGocciaRealm;
    Promise: TGocciaPromiseValue;
    Signal: TGocciaAbortSignalValue;
    // WHATWG DOM §3.2 abort algorithm registration for this request, removed
    // again whenever the pending entry leaves FPending.
    AbortAlgorithmHandle: Integer;
    // A request handed to the engine's transport; nil for a worker's. The
    // entry holds the request's worker slot, released with the exchange
    // when the entry leaves FPending.
    Exchange: IGocciaFetchExchange;
  end;

  { The destination check one network request applies on every hop, and the
    decisions it made, for the audit trail. }
  TGocciaFetchHopChecker = class
  private
    FCapabilities: TGocciaCapabilities;
    FNameChecks: Integer;
    FHopDecisions: array of TGocciaFetchHopDecision;
    procedure RecordDecision(const AAllowed: Boolean;
      const AHost, AReason: string);
  public
    constructor Create(const ACapabilities: TGocciaCapabilities);
    function CheckDestination(const AHost: string; const APort: Integer;
      const AResolvedAddress: string; out AReason: string): Boolean;
  end;

  TGocciaFetchWorker = class(TThread)
  private
    FState: TGocciaFetchState;
    FLimiter: TGocciaFetchLimiter;
    FRequestID: Integer;
    FURL: string;
    FMethod: string;
    FHeaders: THTTPHeaders;
    FRedirectMode: THTTPRedirectMode;

    FTimeoutMilliseconds: Integer;
    { The worker's own copy of the dispatching engine's policy. }
    FPolicy: TGocciaFetchPolicy;
  protected
    procedure Execute; override;
  public
    constructor Create(const AState: TGocciaFetchState;
      const ALimiter: TGocciaFetchLimiter; const ARequestID: Integer;
      const AURL, AMethod: string; const AHeaders: THTTPHeaders;
      const ARedirectMode: THTTPRedirectMode;
      const ATimeoutMilliseconds: Integer;
      const APolicy: TGocciaFetchPolicy);
    destructor Destroy; override;
  end;

  TGocciaFetchExchange = class(TInterfacedObject, IGocciaFetchExchange)
  private
    { Kept alive by this reference until the exchange goes, so an answer
      given after the engine ended still has a queue to be dropped by. }
    FState: TGocciaFetchState;
    FRequestID: Integer;
    FURL: string;
    FIsHead: Boolean;
    FMaxResponseBytes: Integer;
    { EXCHANGE_OPEN until the first answer or the cancellation claims it. }
    FSettled: Longint;
    { Non-zero once an abort or a discard made any answer unwanted, even one
      already given but not yet settled. }
    FCancelled: Longint;
    function Claim: Boolean;
    procedure Post(const ACompletion: TGocciaFetchCompletion);
  public
    constructor Create(const AState: TGocciaFetchState;
      const ARequestID: Integer; const AURL: string; const AIsHead: Boolean;
      const AMaxResponseBytes: Integer);
    destructor Destroy; override;
    function Respond(const AResponse: THTTPResponse): Boolean;
    function Fail(const AMessage: string;
      const ADeniedScope: string = ''): Boolean;
    function IsCancelled: Boolean;
    procedure Cancel;
    procedure Close;
  end;

  TGocciaFetchManagerImpl = class(TGocciaFetchManager)
  private
    FState: TGocciaFetchState;
    FLimiter: TGocciaFetchLimiter;
    FPending: TList<TGocciaPendingFetch>;
    { Requests settled early (aborted) whose completion has not arrived yet:
      kept only so the worker's hop decisions still reach the audit sink. }
    FAbandoned: TList<TGocciaPendingFetch>;
    FNextRequestID: Integer;
    procedure ReplayHopDecisions(const APending: TGocciaPendingFetch;
      const ACompletion: TGocciaFetchCompletion);
    procedure KeepForAudit(const APending: TGocciaPendingFetch);
    function PopCompletion(out ACompletion: TGocciaFetchCompletion): Boolean;
    function FindPendingIndex(const ARequestID: Integer): Integer;
    function RejectAbortedFetches: Integer;
    procedure AbortPendingFetch(const AToken: Int64);
    { ASettled: the entry leaves because its completion settled it, not
      because an abort or a discard gave up on it. }
    procedure ReleasePendingRoots(const APending: TGocciaPendingFetch;
      const ASettled: Boolean = False);
    procedure SettleCompletion(const ACompletion: TGocciaFetchCompletion);
  public
    constructor Create;
    destructor Destroy; override;

    procedure StartFetch(const AURL, AMethod: string;
      const AHeaders: THTTPHeaders; const APolicy: TGocciaFetchPolicy;
      const ARealm: TGocciaRealm; const APromise: TGocciaPromiseValue;
      const ASignal: TGocciaAbortSignalValue = nil;
      const ARedirectMode: THTTPRedirectMode = hrmFollow); override;
    function PumpCompletions: Integer; override;
    function HasPending: Boolean; override;
    function HasPendingFor(const ARealm: TGocciaRealm): Boolean; override;
    function WaitForPromise(const APromise: TGocciaPromiseValue): Boolean; override;
    procedure WaitForIdle(const ARealm: TGocciaRealm); override;
    procedure DiscardPending(const ARealm: TGocciaRealm); override;
  end;

{$ENDIF}

threadvar
  FetchManagerThreadInstance: TGocciaFetchManager;
  FetchManagerThreadUsers: Integer;

{$IFNDEF LAKON}

{ TGocciaFetchCompletion }

constructor TGocciaFetchCompletion.Create(const ARequestID: Integer);
begin
  inherited Create;
  RequestID := ARequestID;
  Success := False;
  ErrorMessage := '';
end;

{ TGocciaFetchLimiter }

constructor TGocciaFetchLimiter.Create;
begin
  inherited Create;
  CriticalSectionInit(FLock);
  FActiveWorkers := 0;
  FRefCount := 1;
end;

destructor TGocciaFetchLimiter.Destroy;
begin
  CriticalSectionDone(FLock);
  inherited;
end;

procedure TGocciaFetchLimiter.AddRef;
begin
  AtomicIncrementInt32(FRefCount);
end;

procedure TGocciaFetchLimiter.Release;
begin
  if AtomicDecrementInt32(FRefCount) = 0 then
    Free;
end;

function TGocciaFetchLimiter.TryAcquireWorker: Boolean;
begin
  CriticalSectionEnter(FLock);
  try
    Result := FActiveWorkers < MAX_FETCH_WORKERS;
    if Result then
      Inc(FActiveWorkers);
  finally
    CriticalSectionLeave(FLock);
  end;
end;

procedure TGocciaFetchLimiter.ReleaseWorker;
begin
  CriticalSectionEnter(FLock);
  try
    if FActiveWorkers > 0 then
      Dec(FActiveWorkers);
  finally
    CriticalSectionLeave(FLock);
  end;
end;

{ TGocciaFetchState }

constructor TGocciaFetchState.Create;
begin
  inherited Create;
  CriticalSectionInit(FLock);
  FCompletions := TList<TGocciaFetchCompletion>.Create;
  FAcceptCompletions := True;
  FRefCount := 1;
end;

destructor TGocciaFetchState.Destroy;
begin
  ClearCompletions;
  FCompletions.Free;
  CriticalSectionDone(FLock);
  inherited;
end;

procedure TGocciaFetchState.AddRef;
begin
  AtomicIncrementInt32(FRefCount);
end;

procedure TGocciaFetchState.Release;
begin
  if AtomicDecrementInt32(FRefCount) = 0 then
    Free;
end;

function TGocciaFetchState.PostCompletion(
  const ACompletion: TGocciaFetchCompletion): Boolean;
begin
  CriticalSectionEnter(FLock);
  try
    Result := FAcceptCompletions;
    if Result then
      FCompletions.Add(ACompletion);
  finally
    CriticalSectionLeave(FLock);
  end;
end;

function TGocciaFetchState.PopCompletion(
  out ACompletion: TGocciaFetchCompletion): Boolean;
begin
  ACompletion := nil;
  CriticalSectionEnter(FLock);
  try
    Result := FCompletions.Count > 0;
    if Result then
    begin
      ACompletion := FCompletions[0];
      FCompletions.Delete(0);
    end;
  finally
    CriticalSectionLeave(FLock);
  end;
end;

procedure TGocciaFetchState.ClearCompletions;
var
  I: Integer;
begin
  CriticalSectionEnter(FLock);
  try
    for I := 0 to FCompletions.Count - 1 do
      FCompletions[I].Free;
    FCompletions.Clear;
  finally
    CriticalSectionLeave(FLock);
  end;
end;

procedure TGocciaFetchState.Abandon;
begin
  CriticalSectionEnter(FLock);
  try
    FAcceptCompletions := False;
  finally
    CriticalSectionLeave(FLock);
  end;
  ClearCompletions;
end;

{ TGocciaFetchWorker }

constructor TGocciaFetchWorker.Create(const AState: TGocciaFetchState;
  const ALimiter: TGocciaFetchLimiter; const ARequestID: Integer;
  const AURL, AMethod: string; const AHeaders: THTTPHeaders;
  const ARedirectMode: THTTPRedirectMode;
  const ATimeoutMilliseconds: Integer; const APolicy: TGocciaFetchPolicy);
begin
  inherited Create(True, FETCH_WORKER_STACK_SIZE);
  FreeOnTerminate := True;
  FRequestID := ARequestID;
  FURL := AURL;
  FMethod := AMethod;
  FHeaders := AHeaders;
  FRedirectMode := ARedirectMode;
  FTimeoutMilliseconds := ATimeoutMilliseconds;
  FPolicy := APolicy;
  FState := AState;
  FState.AddRef;
  FLimiter := ALimiter;
  FLimiter.AddRef;
end;

destructor TGocciaFetchWorker.Destroy;
begin
  if Assigned(FLimiter) then
  begin
    FLimiter.ReleaseWorker;
    FLimiter.Release;
  end;
  if Assigned(FState) then
    FState.Release;
  inherited;
end;

{ TGocciaFetchHopChecker }

constructor TGocciaFetchHopChecker.Create(
  const ACapabilities: TGocciaCapabilities);
begin
  inherited Create;
  FCapabilities := ACapabilities;
end;

procedure TGocciaFetchHopChecker.RecordDecision(const AAllowed: Boolean;
  const AHost, AReason: string);
begin
  SetLength(FHopDecisions, Length(FHopDecisions) + 1);
  FHopDecisions[High(FHopDecisions)].Allowed := AAllowed;
  FHopDecisions[High(FHopDecisions)].Host := AHost;
  FHopDecisions[High(FHopDecisions)].Reason := AReason;
end;

{ THTTPHostCheck for this request: the engine's net rules, applied to every
  hop before the name is resolved and again to the address it resolved to.
  The first hop's name was already checked and audited synchronously by
  fetch() itself, so only later hops record a name decision. }
function TGocciaFetchHopChecker.CheckDestination(const AHost: string;
  const APort: Integer; const AResolvedAddress: string;
  out AReason: string): Boolean;
begin
  if AResolvedAddress = '' then
  begin
    Inc(FNameChecks);
    Result := FCapabilities.AllowsNetHost(AHost, APort);
    if Result then
      AReason := 'redirect target is allowed by the net capability'
    else
      AReason := 'redirect target is not allowed by the net capability';
    if FNameChecks > 1 then
      RecordDecision(Result, AHost, AReason);
    Exit;
  end;

  Result := FCapabilities.AllowsNetAddress(AHost, APort,
    AResolvedAddress);
  if Result then
    AReason := 'resolved to ' + AResolvedAddress +
      ', which the net capability allows'
  else
    AReason := 'resolved to ' + AResolvedAddress +
      ', a private or denied address the net capability does not allow';
  RecordDecision(Result, AHost, AReason);
end;

{ The built-in network request behind fetch(): fills ACompletion with the
  response or the error, and with the hop decisions it made. }
procedure PerformNetworkRequest(const AURL, AMethod: string;
  const AHeaders: THTTPHeaders; const ARedirectMode: THTTPRedirectMode;
  const ATimeoutMilliseconds: Integer;
  const ACapabilities: TGocciaCapabilities; const AMaxResponseBytes: Integer;
  const ACompletion: TGocciaFetchCompletion);
var
  Checker: TGocciaFetchHopChecker;
  RequestPolicy: THTTPRequestPolicy;
begin
  Checker := TGocciaFetchHopChecker.Create(ACapabilities);
  try
    RequestPolicy := DefaultHTTPPolicy;
    RequestPolicy.MaxResponseBytes := AMaxResponseBytes;
    RequestPolicy.HostCheck := Checker.CheckDestination;
    RequestPolicy.RedirectMode := ARedirectMode;
    try
      if AMethod = FETCH_METHOD_HEAD then
        ACompletion.Response := HTTPHead(AURL, AHeaders, nil,
          ATimeoutMilliseconds, RequestPolicy)
      else
        ACompletion.Response := HTTPGet(AURL, AHeaders, nil,
          ATimeoutMilliseconds, RequestPolicy);
      ACompletion.Success := True;
    except
      on E: EHTTPDestinationDenied do
      begin
        ACompletion.ErrorMessage := E.Message;
        ACompletion.DeniedScope := E.Scope;
      end;
      on E: EHTTPError do
        ACompletion.ErrorMessage := E.Message;
      on E: Exception do
        ACompletion.ErrorMessage := 'fetch failed: ' + E.Message;
    end;
    ACompletion.HopDecisions := Copy(Checker.FHopDecisions, 0,
      Length(Checker.FHopDecisions));
  finally
    Checker.Free;
  end;
end;

procedure TGocciaFetchWorker.Execute;
var
  Completion: TGocciaFetchCompletion;
begin
  Completion := TGocciaFetchCompletion.Create(FRequestID);
  try
    PerformNetworkRequest(FURL, FMethod, FHeaders, FRedirectMode,
      FTimeoutMilliseconds, FPolicy.Capabilities, FPolicy.MaxResponseBytes,
      Completion);
    if FState.PostCompletion(Completion) then
      Completion := nil;
  finally
    Completion.Free;
  end;
end;

{ TGocciaFetchExchange }

constructor TGocciaFetchExchange.Create(const AState: TGocciaFetchState;
  const ARequestID: Integer; const AURL: string; const AIsHead: Boolean;
  const AMaxResponseBytes: Integer);
begin
  inherited Create;
  FState := AState;
  FState.AddRef;
  FRequestID := ARequestID;
  FURL := AURL;
  FIsHead := AIsHead;
  FMaxResponseBytes := AMaxResponseBytes;
  FSettled := EXCHANGE_OPEN;
  FCancelled := 0;
end;

destructor TGocciaFetchExchange.Destroy;
begin
  if Assigned(FState) then
    FState.Release;
  inherited;
end;

function TGocciaFetchExchange.Claim: Boolean;
begin
  Result := InterlockedCompareExchange(FSettled, EXCHANGE_ANSWERED,
    EXCHANGE_OPEN) = EXCHANGE_OPEN;
end;

procedure TGocciaFetchExchange.Post(const ACompletion: TGocciaFetchCompletion);
begin
  // An answer taken just before an abort or a discard is not queued once the
  // cancellation is visible; the queue also refuses it once the engine
  // discarded its requests. Either way nothing would settle it.
  if IsCancelled or not FState.PostCompletion(ACompletion) then
    ACompletion.Free;
end;

{ RFC 9110 §5.6.2: token = 1*tchar. }
function IsHTTPToken(const AText: string): Boolean;
var
  I: Integer;
begin
  if AText = '' then
    Exit(False);
  for I := 1 to Length(AText) do
    case AText[I] of
      'a'..'z', 'A'..'Z', '0'..'9', '!', '#', '$', '%', '&', '''', '*', '+',
      '-', '.', '^', '_', '`', '|', '~':
        ;
    else
      Exit(False);
    end;
  Result := True;
end;

{ WHATWG Fetch: a header value is a byte sequence with no NUL, CR or LF.
  Text reaches the engine isomorphically decoded, so a character past U+00FF
  stands for no byte. }
function HasForbiddenValueCharacter(const AText: string): Boolean;
var
  I: Integer;
begin
  for I := 1 to Length(AText) do
    if (AText[I] in [#0, #10, #13]) or (AText[I] > LAST_BYTE_CHARACTER) then
      Exit(True);
  Result := False;
end;

{ WHATWG Fetch, initialize a response: status text matches RFC 9110's
  reason-phrase, *( HTAB / SP / VCHAR / obs-text ), as bytes: characters
  U+0080 to U+00FF stand for obs-text, and none past them is a byte. }
function IsReasonPhrase(const AText: string): Boolean;
const
  TAB_CHARACTER = #9;
  FIRST_VISIBLE_OR_SPACE = ' ';
  DELETE_CHARACTER = #127;
var
  I: Integer;
begin
  for I := 1 to Length(AText) do
    if ((AText[I] < FIRST_VISIBLE_OR_SPACE) and
        (AText[I] <> TAB_CHARACTER)) or (AText[I] = DELETE_CHARACTER) or
       (AText[I] > LAST_BYTE_CHARACTER) then
      Exit(False);
  Result := True;
end;

{ WHATWG Fetch: to normalize a header value, strip its leading and trailing
  HTTP whitespace (tab and space here: CR and LF were refused above). }
function NormalizeHeaderValue(const AValue: string): string;
var
  First, Last: Integer;
begin
  First := 1;
  Last := Length(AValue);
  while (First <= Last) and (AValue[First] in [#9, ' ']) do
    Inc(First);
  while (Last >= First) and (AValue[Last] in [#9, ' ']) do
    Dec(Last);
  Result := Copy(AValue, First, Last - First + 1);
end;

{ WHATWG Fetch: a null body status is 101, 103, 204, 205 or 304; only the
  last three can reach here. }
function IsNullBodyStatus(const AStatus: Integer): Boolean;
begin
  Result := (AStatus = HTTP_STATUS_NO_CONTENT) or
    (AStatus = HTTP_STATUS_RESET_CONTENT) or
    (AStatus = HTTP_STATUS_NOT_MODIFIED);
end;

{ A URL the built-in client could have reported as a response's final URL:
  an absolute http(s) URL its own parser accepts, and the URL Standard's
  parser too, since Response.url is the latter's serialization. }
function IsClientResponseURL(const AURL: string): Boolean;
begin
  try
    ParseHTTPURL(AURL);
    Result := ParseURL(AURL).IsValid;
  except
    on EHTTPError do
      Result := False;
  end;
end;

{ Response.url (WHATWG Fetch, https://fetch.spec.whatwg.org/#dom-response-url):
  the response's URL serialized by the URL Standard (§4.5) with the fragment
  excluded, so the scheme and host are lowercase, dot segments are resolved
  and no fragment is left. The built-in client and a transport both report the
  URL they were given or followed to, and this is the one place either becomes
  what a script reads. A URL the URL Standard's parser refuses but the
  built-in client's own parser accepted (a transport's is refused first) keeps
  its text with only the fragment cut off. Host parsing is the engine's
  (Goccia.URL.Parser), so the result is what new URL(...).href gives. }
function ResponseURLForScript(const AURL: string): string;
var
  Parsed: TGocciaURLRecord;
  FragmentStart: Integer;
begin
  Parsed := ParseURL(AURL);
  if Parsed.IsValid then
    Exit(SerializeURL(Parsed, True));
  FragmentStart := Pos('#', AURL);
  if FragmentStart > 0 then
    Result := Copy(AURL, 1, FragmentStart - 1)
  else
    Result := AURL;
end;

{ Why the engine refuses a transport's response, or '' when it takes it. }
function TransportResponseError(const AResponse: THTTPResponse;
  const AIsHead: Boolean; const AMaxResponseBytes: Integer): string;
var
  I: Integer;
begin
  Result := '';
  if (AResponse.StatusCode < FETCH_MIN_RESPONSE_STATUS) or
     (AResponse.StatusCode > FETCH_MAX_RESPONSE_STATUS) then
    Exit(Format(FETCH_TRANSPORT_INVALID_STATUS_ERROR,
      [AResponse.StatusCode]));
  if not IsReasonPhrase(AResponse.StatusText) then
    Exit(FETCH_TRANSPORT_INVALID_STATUS_TEXT_ERROR);
  for I := 0 to High(AResponse.Headers) do
    if not IsHTTPToken(AResponse.Headers[I].Name) or
       HasForbiddenValueCharacter(AResponse.Headers[I].Value) then
      Exit(FETCH_TRANSPORT_INVALID_HEADER_ERROR);
  if (AResponse.FinalURL <> '') and
     not IsClientResponseURL(AResponse.FinalURL) then
    Exit(FETCH_TRANSPORT_INVALID_URL_ERROR);
  // A body the response will not keep is not held to the ceiling.
  if (not AIsHead) and (not IsNullBodyStatus(AResponse.StatusCode)) and
     (Length(AResponse.Body) > AMaxResponseBytes) then
    Exit(Format(FETCH_RESPONSE_BODY_LIMIT_ERROR, [AMaxResponseBytes]));
end;

function TGocciaFetchExchange.Respond(const AResponse: THTTPResponse): Boolean;
var
  Completion: TGocciaFetchCompletion;
  I: Integer;
begin
  Result := Claim;
  if not Result then
    Exit;
  Completion := TGocciaFetchCompletion.Create(FRequestID);
  try
    Completion.ErrorMessage := TransportResponseError(AResponse, FIsHead,
      FMaxResponseBytes);
    if Completion.ErrorMessage = '' then
    begin
      Completion.Success := True;
      Completion.Response := AResponse;
      // Private copies: dynamic arrays are shared, not copied on write, and
      // the transport may go on using its own on another thread.
      Completion.Response.Headers := Copy(AResponse.Headers);
      for I := 0 to High(Completion.Response.Headers) do
        Completion.Response.Headers[I].Value :=
          NormalizeHeaderValue(Completion.Response.Headers[I].Value);
      // WHATWG Fetch, HTTP-network fetch: a HEAD request's response and a
      // null body status's have a null body, whatever the transport gave.
      if FIsHead or IsNullBodyStatus(AResponse.StatusCode) then
        Completion.Response.Body := nil
      else
        Completion.Response.Body := Copy(AResponse.Body);
      { Response.url is the final URL the built-in client reports, so a run
        reads the same with and without a transport; the response value
        serializes it (ResponseURLForScript). }
      if Completion.Response.FinalURL = '' then
        Completion.Response.FinalURL := FURL;
    end;
  except
    Completion.Free;
    raise;
  end;
  Post(Completion);
end;

function TGocciaFetchExchange.Fail(const AMessage: string;
  const ADeniedScope: string): Boolean;
var
  Completion: TGocciaFetchCompletion;
begin
  Result := Claim;
  if not Result then
    Exit;
  Completion := TGocciaFetchCompletion.Create(FRequestID);
  Completion.ErrorMessage := AMessage;
  Completion.DeniedScope := ADeniedScope;
  Post(Completion);
end;

function TGocciaFetchExchange.IsCancelled: Boolean;
begin
  Result := InterlockedCompareExchange(FCancelled, 0, 0) <> 0;
end;

procedure TGocciaFetchExchange.Cancel;
begin
  // Claim first, so a responder that reads IsCancelled as true can never
  // still have an answer taken.
  InterlockedCompareExchange(FSettled, EXCHANGE_CANCELLED, EXCHANGE_OPEN);
  InterlockedExchange(FCancelled, 1);
end;

procedure TGocciaFetchExchange.Close;
begin
  InterlockedCompareExchange(FSettled, EXCHANGE_CANCELLED, EXCHANGE_OPEN);
end;

{$ENDIF}

{ TGocciaFetchManager }

class function TGocciaFetchManager.Instance: TGocciaFetchManager;
begin
  Result := FetchManagerThreadInstance;
end;

class procedure TGocciaFetchManager.AcquireInstance;
begin
  {$IFNDEF LAKON}
  if not Assigned(FetchManagerThreadInstance) then
    FetchManagerThreadInstance := TGocciaFetchManagerImpl.Create;
  Inc(FetchManagerThreadUsers);
  {$ENDIF}
end;

class procedure TGocciaFetchManager.ReleaseInstance;
begin
  if FetchManagerThreadUsers <= 0 then
    Exit;
  Dec(FetchManagerThreadUsers);
  if FetchManagerThreadUsers = 0 then
    FreeAndNil(FetchManagerThreadInstance);
end;

{$IFNDEF LAKON}

{ TGocciaFetchManagerImpl }

constructor TGocciaFetchManagerImpl.Create;
begin
  inherited Create;
  FState := TGocciaFetchState.Create;
  FLimiter := TGocciaFetchLimiter.Create;
  FPending := TList<TGocciaPendingFetch>.Create;
  FAbandoned := TList<TGocciaPendingFetch>.Create;
  FNextRequestID := 1;
end;

destructor TGocciaFetchManagerImpl.Destroy;
var
  I: Integer;
begin
  for I := 0 to FPending.Count - 1 do
    ReleasePendingRoots(FPending[I]);
  FPending.Clear;
  FState.Abandon;
  FState.Release;
  FLimiter.Release;
  FPending.Free;
  FAbandoned.Free;
  inherited;
end;

{ Why the built-in client would refuse to send this request — an
  unsupported scheme, a malformed URL, a control character in the request
  target or a header — in its words, or '' when it would send it. }
function TransportRequestError(const AURL: string;
  const AHeaders: THTTPHeaders): string;
var
  Parsed: THTTPParsedURL;
begin
  Result := '';
  try
    Parsed := ParseHTTPURL(AURL);
    ValidateHTTPRequestText(Parsed.Path, 'request target', False);
    ValidateHTTPRequestHeaders(AHeaders);
  except
    on E: EHTTPError do
      Result := E.Message;
  end;
end;

procedure TGocciaFetchManagerImpl.StartFetch(const AURL, AMethod: string;
  const AHeaders: THTTPHeaders; const APolicy: TGocciaFetchPolicy;
  const ARealm: TGocciaRealm; const APromise: TGocciaPromiseValue;
  const ASignal: TGocciaAbortSignalValue;
  const ARedirectMode: THTTPRedirectMode);
var
  Pending: TGocciaPendingFetch;
  Worker: TGocciaFetchWorker;
  Request: TGocciaFetchTransportRequest;
  Added, LimitAcquired, Rooted: Boolean;
  RequestTimeoutMilliseconds, SignalTimeoutMilliseconds, PendingIndex,
    MaxResponseBytes: Integer;
  RequestError, RequestURL: string;
begin
  { WHATWG Fetch: the request's URL is the parsed one, dot segments removed.
    The built-in client and a transport are handed it, and the client
    resolves each redirect against it (NormalizeHTTPRequestURL). }
  RequestURL := NormalizeHTTPRequestURL(AURL);
  Worker := nil;
  Added := False;
  LimitAcquired := False;
  Rooted := False;

  if Assigned(ASignal) and ASignal.IsAborted then
  begin
    // WHATWG DOM §3.2 signal abort: abort algorithms (the fetch rejection)
    // run before the "abort" event is fired at the signal.
    APromise.Reject(ASignal.Reason);
    ASignal.FlushAbortEvent;
    Exit;
  end;

  if not FLimiter.TryAcquireWorker then
  begin
    APromise.Reject(CreateErrorObject('TypeError', FETCH_WORKER_LIMIT_ERROR));
    Exit;
  end;
  LimitAcquired := True;

  Pending.RequestID := FNextRequestID;
  Inc(FNextRequestID);
  Pending.Realm := ARealm;
  Pending.AuditEmitter := APolicy.AuditEmitter;
  Pending.AuditSource := APolicy.AuditSource;
  try
    Pending.AuditSubject := HTTPURLAuditHost(AURL);
  except
    on EHTTPError do
      Pending.AuditSubject := '';
  end;
  Pending.AbortAlgorithmHandle := 0;
  Pending.Promise := APromise;
  Pending.Signal := ASignal;
  Pending.Exchange := nil;

  try
    RequestTimeoutMilliseconds := RemainingExecutionTimeoutMilliseconds;
    if Assigned(ASignal) then
    begin
      SignalTimeoutMilliseconds := ASignal.RemainingTimeoutMilliseconds;
      if SignalTimeoutMilliseconds = 0 then
      begin
        ASignal.RefreshTimeout;
        APromise.Reject(ASignal.Reason);
        ASignal.FlushAbortEvent;
        FLimiter.ReleaseWorker;
        Exit;
      end;
      if (SignalTimeoutMilliseconds > 0) and
         ((RequestTimeoutMilliseconds = 0) or
          (SignalTimeoutMilliseconds < RequestTimeoutMilliseconds)) then
        RequestTimeoutMilliseconds := SignalTimeoutMilliseconds;
    end;

    // WHATWG DOM §3.2 / Fetch: register this request's abort algorithm so a
    // later abort rejects it before the "abort" event is fired, instead of
    // waiting for the next pump. Registered before the worker exists so the
    // already-aborted answer is known while this request is still cheap to
    // abandon.
    if Assigned(ASignal) then
    begin
      Pending.AbortAlgorithmHandle :=
        ASignal.AddAbortAlgorithm(AbortPendingFetch, Pending.RequestID);
      // "If signal is aborted, then return" — a zero handle means the signal
      // aborted between the checks above and this registration, which a
      // timeout signal can do purely by the deadline elapsing in between. No
      // algorithm would ever reject this request, so phase 2 of
      // RejectAbortedFetches would fire the abort event and the fetch would go
      // on to settle normally afterwards. Take the same path the pre-flight
      // already-aborted branch takes instead.
      if Pending.AbortAlgorithmHandle = 0 then
      begin
        APromise.Reject(ASignal.Reason);
        ASignal.FlushAbortEvent;
        FLimiter.ReleaseWorker;
        Exit;
      end;
    end;

    if Assigned(APolicy.Transport) then
    begin
      MaxResponseBytes := APolicy.MaxResponseBytes;
      if MaxResponseBytes <= 0 then
        MaxResponseBytes := DEFAULT_MAX_RESPONSE_BODY_BYTES;
      Request.Method := AMethod;
      Request.URL := RequestURL;
      Request.Headers := Copy(AHeaders);
      Request.Body := nil;
      Request.Redirect := ARedirectMode;
      Request.TimeoutMilliseconds := RequestTimeoutMilliseconds;
      Request.MaxResponseBytes := MaxResponseBytes;
      Request.Capabilities := APolicy.Capabilities;
      Pending.Exchange := TGocciaFetchExchange.Create(FState,
        Pending.RequestID, RequestURL, AMethod = FETCH_METHOD_HEAD,
        MaxResponseBytes);
    end
    else
    begin
      Worker := TGocciaFetchWorker.Create(FState, FLimiter,
        Pending.RequestID, RequestURL, AMethod, AHeaders, ARedirectMode,
        RequestTimeoutMilliseconds, APolicy);
      LimitAcquired := False;
    end;

    if (TGarbageCollector.Instance <> nil) then
    begin
      TGarbageCollector.Instance.AddTempRoot(APromise);
      if Assigned(ASignal) then
        TGarbageCollector.Instance.AddTempRoot(ASignal);
      Rooted := True;
    end;

    FPending.Add(Pending);
    Added := True;
    if Assigned(Pending.Exchange) then
    begin
      { The request is pending before the transport sees it, so an answer
        given inside the call finds it at the next pump. From here the entry
        holds the worker slot. A request the built-in client would refuse to
        send never reaches the transport; it fails the same way, through the
        promise. }
      LimitAcquired := False;
      RequestError := TransportRequestError(RequestURL, AHeaders);
      if RequestError <> '' then
        Pending.Exchange.Fail(RequestError)
      else
        APolicy.Transport.Fetch(Request, Pending.Exchange);
    end
    else
    begin
      Worker.Start;
      Worker := nil;
    end;
  except
    if Added then
    begin
      // The transport cannot reach FPending, but look the entry up anyway.
      PendingIndex := FindPendingIndex(Pending.RequestID);
      if PendingIndex >= 0 then
        FPending.Delete(PendingIndex);
    end;
    if Assigned(ASignal) and (Pending.AbortAlgorithmHandle <> 0) then
      ASignal.RemoveAbortAlgorithm(Pending.AbortAlgorithmHandle);
    if Rooted and (TGarbageCollector.Instance <> nil) then
    begin
      TGarbageCollector.Instance.RemoveTempRoot(APromise);
      if Assigned(ASignal) then
        TGarbageCollector.Instance.RemoveTempRoot(ASignal);
    end;
    Worker.Free;
    if Assigned(Pending.Exchange) then
    begin
      // A raising transport fails its request even when it answered first;
      // fetch() rejects it. The entry is gone, so release its slot here.
      Pending.Exchange.Cancel;
      if Added then
        LimitAcquired := True;
    end;
    if LimitAcquired then
      FLimiter.ReleaseWorker;
    raise;
  end;
end;

procedure TGocciaFetchManagerImpl.ReleasePendingRoots(
  const APending: TGocciaPendingFetch; const ASettled: Boolean);
begin
  // The pending entry is leaving FPending, so its abort algorithm must go with
  // it. This is a no-op when the algorithm list was already emptied by the
  // signal abort that brought us here.
  if Assigned(APending.Signal) and (APending.AbortAlgorithmHandle <> 0) then
    APending.Signal.RemoveAbortAlgorithm(APending.AbortAlgorithmHandle);

  // A transport's request gives up its worker slot here, on this thread, so
  // the cap counts exactly the requests a script still awaits. Its answer, if
  // it comes after this, is ignored.
  if Assigned(APending.Exchange) then
  begin
    if ASettled then
      APending.Exchange.Close
    else
      APending.Exchange.Cancel;
    FLimiter.ReleaseWorker;
  end;

  if TGarbageCollector.Instance = nil then
    Exit;
  TGarbageCollector.Instance.RemoveTempRoot(APending.Promise);
  if Assigned(APending.Signal) then
    TGarbageCollector.Instance.RemoveTempRoot(APending.Signal);
end;

// WHATWG DOM §3.2 abort algorithm for one in-flight fetch: reject the request's
// promise and drop its pending entry. Runs during signal abort, before the
// "abort" event, so a listener already sees the fetch settled.
procedure TGocciaFetchManagerImpl.AbortPendingFetch(const AToken: Int64);
var
  PendingIndex: Integer;
  Pending: TGocciaPendingFetch;
begin
  PendingIndex := FindPendingIndex(Integer(AToken));
  if PendingIndex < 0 then
    Exit;

  Pending := FPending[PendingIndex];
  FPending.Delete(PendingIndex);
  // A transport's request records no hop decisions, so it owes no audit.
  if not Assigned(Pending.Exchange) then
    KeepForAudit(Pending);
  try
    Pending.Promise.Reject(Pending.Signal.Reason);
  finally
    ReleasePendingRoots(Pending);
  end;
end;

{ An aborted request's worker still checks, and records, its address and
  redirect decisions; they are replayed if its completion arrives while the
  engine runs. The engine does not wait for aborted requests, so when it
  ends first they are never delivered. The abort itself therefore records
  the abandonment, so the log shows where a request's hop decisions may
  stop. }
procedure TGocciaFetchManagerImpl.KeepForAudit(
  const APending: TGocciaPendingFetch);
begin
  if not Assigned(APending.AuditEmitter) then
    Exit;
  APending.AuditEmitter(gckNetFetch, gcdAllow, APending.AuditSubject,
    FETCH_ABANDONED_AUDIT_REASON, APending.AuditSource);
  FAbandoned.Add(APending);
end;

procedure TGocciaFetchManagerImpl.ReplayHopDecisions(
  const APending: TGocciaPendingFetch;
  const ACompletion: TGocciaFetchCompletion);
var
  I: Integer;
begin
  if not Assigned(APending.AuditEmitter) then
    Exit;
  for I := 0 to High(ACompletion.HopDecisions) do
    if ACompletion.HopDecisions[I].Allowed then
      APending.AuditEmitter(gckNetFetch, gcdAllow,
        ACompletion.HopDecisions[I].Host,
        ACompletion.HopDecisions[I].Reason, APending.AuditSource)
    else
      APending.AuditEmitter(gckNetFetch, gcdDeny,
        ACompletion.HopDecisions[I].Host,
        ACompletion.HopDecisions[I].Reason, APending.AuditSource);
end;

// Settles fetches whose signal has aborted. Controller-driven aborts already
// rejected themselves through their abort algorithm, so what reaches here is
// the lazily observed case: a timeout signal that expired since the last pump.
function TGocciaFetchManagerImpl.RejectAbortedFetches: Integer;
var
  I, CountBefore: Integer;
  Signal: TGocciaAbortSignalValue;
  AbortedSignals: TGocciaAbortSignalList;
  SignalRealms: TList<TGocciaRealm>;
  SignalRoot: TGocciaTempRoot;
  PreviousRealm: TGocciaRealm;
begin
  CountBefore := FPending.Count;
  // As in SettleCompletion: the abort reason and the "abort" event belong to
  // the realm of the engine that started the request, not to whichever
  // engine's drain observed the expired timeout.
  PreviousRealm := CurrentRealm;
  AbortedSignals := TGocciaAbortSignalList.Create(False);
  SignalRealms := TList<TGocciaRealm>.Create;
  try
    // Phase 1 flips expired timeouts only. RefreshTimeout deliberately does not
    // run abort algorithms, because an algorithm mutates FPending and this
    // walks it.
    for I := 0 to FPending.Count - 1 do
    begin
      Signal := FPending[I].Signal;
      if not Assigned(Signal) then
        Continue;
      if Assigned(FPending[I].Realm) then
        SetCurrentRealm(FPending[I].Realm)
      else
        SetCurrentRealm(PreviousRealm);
      Signal.RefreshTimeout;
      if not Signal.IsAborted then
        Continue;
      if AbortedSignals.IndexOf(Signal) < 0 then
      begin
        AbortedSignals.Add(Signal);
        SignalRealms.Add(CurrentRealm);
      end;
    end;

    // Phase 2 runs with no walk in progress, so each signal may now run its
    // abort algorithms (rejecting and removing its own pending entries) and
    // then fire its one-shot "abort" event. No script runs between the two, so
    // a listener still cannot be registered after the signal aborted but
    // before its event fires.
    for I := 0 to AbortedSignals.Count - 1 do
    begin
      Signal := AbortedSignals[I];
      SetCurrentRealm(SignalRealms[I]);
      Signal.RunPendingAbortAlgorithms;
      // Those algorithms dropped the pending entries that were rooting this
      // signal, so root it for the dispatch itself. AddTempRootIfNeeded is a
      // no-op when another pending fetch still holds a root on it, which is
      // why the raw AddTempRoot/RemoveTempRoot pair must not be used here.
      InitializeTempRoot(SignalRoot);
      AddTempRootIfNeeded(SignalRoot, Signal);
      try
        Signal.FlushAbortEvent;
      finally
        RemoveTempRootIfNeeded(SignalRoot);
      end;
    end;
  finally
    SetCurrentRealm(PreviousRealm);
    SignalRealms.Free;
    AbortedSignals.Free;
  end;
  Result := CountBefore - FPending.Count;
end;

function TGocciaFetchManagerImpl.PopCompletion(
  out ACompletion: TGocciaFetchCompletion): Boolean;
begin
  Result := FState.PopCompletion(ACompletion);
end;

function TGocciaFetchManagerImpl.FindPendingIndex(
  const ARequestID: Integer): Integer;
var
  I: Integer;
begin
  for I := 0 to FPending.Count - 1 do
    if FPending[I].RequestID = ARequestID then
      Exit(I);
  Result := -1;
end;

procedure TGocciaFetchManagerImpl.SettleCompletion(
  const ACompletion: TGocciaFetchCompletion);
var
  Pending: TGocciaPendingFetch;
  PendingIndex, I: Integer;
  RespHeaders: TGocciaHeadersValue;
  RespValue: TGocciaResponseValue;
  PreviousRealm: TGocciaRealm;
begin
  PendingIndex := FindPendingIndex(ACompletion.RequestID);
  if PendingIndex < 0 then
  begin
    { Settled early by an abort: only the audit trail is still owed. }
    for I := FAbandoned.Count - 1 downto 0 do
      if FAbandoned[I].RequestID = ACompletion.RequestID then
      begin
        Pending := FAbandoned[I];
        FAbandoned.Delete(I);
        ReplayHopDecisions(Pending, ACompletion);
        Break;
      end;
    Exit;
  end;

  Pending := FPending[PendingIndex];
  FPending.Delete(PendingIndex);

  // Build the settlement values in the realm of the engine that started the
  // request. A completion can be pumped while a nested engine's realm is
  // current, and values made there would keep that realm's prototypes alive
  // past the nested engine's end.
  PreviousRealm := CurrentRealm;
  try
    if Assigned(Pending.Realm) then
      SetCurrentRealm(Pending.Realm);
    try
      ReplayHopDecisions(Pending, ACompletion);

      if ACompletion.Success then
      begin
        RespHeaders := TGocciaHeadersValue.Create;
        RespHeaders.Immutable := True;
        for I := 0 to High(ACompletion.Response.Headers) do
          RespHeaders.AddHeader(ACompletion.Response.Headers[I].Name,
            ACompletion.Response.Headers[I].Value);

        RespValue := TGocciaResponseValue.Create;
        RespValue.InitFromHTTP(
          ACompletion.Response.StatusCode,
          ACompletion.Response.StatusText,
          ResponseURLForScript(ACompletion.Response.FinalURL),
          RespHeaders,
          ACompletion.Response.Body,
          ACompletion.Response.Redirected);

        Pending.Promise.Resolve(RespValue);
      end
      else if ACompletion.DeniedScope <> '' then
        Pending.Promise.Reject(CreatePermissionDeniedError(
          CapabilityName(gcNet), ACompletion.DeniedScope))
      else
        Pending.Promise.Reject(CreateErrorObject('TypeError',
          ACompletion.ErrorMessage));
    finally
      SetCurrentRealm(PreviousRealm);
    end;

    if (TGocciaMicrotaskQueue.Instance <> nil) then
      TGocciaMicrotaskQueue.Instance.DrainQueue;
  finally
    ReleasePendingRoots(Pending, True);
  end;
end;

function TGocciaFetchManagerImpl.PumpCompletions: Integer;
var
  Completion: TGocciaFetchCompletion;
begin
  { A request's socket gives up when the run's deadline does, so its failure
    reaches this queue at the same instant the run should end. The deadline
    wins: an expired run is ended by its TimeoutError, not handed a rejected
    promise it could catch and run past. Checked again before every
    settlement, because settling one completion runs its reactions, and
    script that long can carry the run across the deadline. }
  CheckExecutionTimeoutNow;
  Result := RejectAbortedFetches;
  while PopCompletion(Completion) do
  begin
    try
      CheckExecutionTimeoutNow;
      SettleCompletion(Completion);
      Inc(Result);
    finally
      Completion.Free;
    end;
  end;
  if (Result > 0) and (TGocciaMicrotaskQueue.Instance <> nil) then
    TGocciaMicrotaskQueue.Instance.DrainQueue;
end;

function TGocciaFetchManagerImpl.HasPending: Boolean;
begin
  Result := FPending.Count > 0;
end;

function TGocciaFetchManagerImpl.WaitForPromise(
  const APromise: TGocciaPromiseValue): Boolean;
begin
  Result := Assigned(APromise) and (APromise.State <> gpsPending);
  while Assigned(APromise) and (APromise.State = gpsPending) do
  begin
    DrainMicrotasksAndFetchCompletions;
    if APromise.State <> gpsPending then
      Exit(True);
    if not HasPending then
      Exit(False);

    while HasPending and (PumpCompletions = 0) do
    begin
      CheckExecutionTimeoutNow;
      Sleep(FETCH_POLL_INTERVAL_MS);
    end;
  end;
end;

function TGocciaFetchManagerImpl.HasPendingFor(
  const ARealm: TGocciaRealm): Boolean;
var
  I: Integer;
begin
  for I := 0 to FPending.Count - 1 do
    if FPending[I].Realm = ARealm then
      Exit(True);
  Result := False;
end;

procedure TGocciaFetchManagerImpl.WaitForIdle(const ARealm: TGocciaRealm);
begin
  repeat
    DrainMicrotasksAndFetchCompletions;
    if not HasPendingFor(ARealm) then
      Break;
    while HasPendingFor(ARealm) and (PumpCompletions = 0) do
    begin
      CheckExecutionTimeoutNow;
      CheckInstructionLimit;
      { An Atomics.waitAsync timeout that falls due while a request is in
        flight settles now, not once the response arrives: the drain above
        runs its reactions. }
      if PumpAtomicsWaitAsyncCompletions > 0 then
        Break;
      Sleep(FETCH_POLL_INTERVAL_MS);
    end;
  until False;
  DrainMicrotasksAndFetchCompletions;
end;

procedure TGocciaFetchManagerImpl.DiscardPending(const ARealm: TGocciaRealm);
var
  I: Integer;
  HadPending: Boolean;
  OldState: TGocciaFetchState;
  Pending: TGocciaPendingFetch;
begin
  HadPending := False;
  for I := FPending.Count - 1 downto 0 do
  begin
    Pending := FPending[I];
    if Pending.Realm <> ARealm then
      Continue;
    FPending.Delete(I);
    ReleasePendingRoots(Pending);
    HadPending := True;
  end;
  { The engine is discarding its requests, so its audit emitter must not
    outlive this call. }
  for I := FAbandoned.Count - 1 downto 0 do
    if FAbandoned[I].Realm = ARealm then
      FAbandoned.Delete(I);

  { Another engine still has requests in flight, so the completion queue is
    shared with live work. The detached requests' late completions no longer
    match a pending entry and are dropped when popped. }
  if (FPending.Count > 0) or (FAbandoned.Count > 0) then
    Exit;

  if not HadPending then
  begin
    FState.ClearCompletions;
    Exit;
  end;

  OldState := FState;
  FState := TGocciaFetchState.Create;
  OldState.Abandon;
  OldState.Release;
end;
{$ENDIF}

procedure DrainMicrotasksAndFetchCompletions;
var
  DidWork: Boolean;
  GC: TGarbageCollector;
  Queue: TGocciaMicrotaskQueue;
  Manager: TGocciaFetchManager;
begin
  GC := TGarbageCollector.Instance;
  try
    if Assigned(GC) then
      GC.ClearKeptObjects;

    repeat
      DidWork := False;

      Queue := TGocciaMicrotaskQueue.Instance;
      if Assigned(Queue) and Queue.HasPending then
      begin
        Queue.DrainQueue;
        DidWork := True;
      end;

      Manager := TGocciaFetchManager.Instance;
      if Assigned(Manager) and Manager.HasPending and
         (Manager.PumpCompletions > 0) then
        DidWork := True;

      if PumpAtomicsWaitAsyncCompletions > 0 then
        DidWork := True;
    until not DidWork;
  finally
    if Assigned(GC) then
      GC.ClearKeptObjects;
  end;
end;

function WaitForFetchPromise(const APromise: TGocciaPromiseValue): Boolean;
var
  Manager: TGocciaFetchManager;
  HasPendingFetch: Boolean;
  TimersRun: Integer;
begin
  if not Assigned(APromise) then
    Exit(False);

  TimersRun := 0;
  while APromise.State = gpsPending do
  begin
    DrainMicrotasksAndFetchCompletions;
    if APromise.State <> gpsPending then
      Exit(True);

    CheckExecutionTimeoutNow;
    CheckInstructionLimit;

    Manager := TGocciaFetchManager.Instance;
    HasPendingFetch := Assigned(Manager) and Manager.HasPending;

    { Work that is really outstanding outranks virtual time.

      Despite the name this is the host's general "drive this promise to
      settlement" wait — the test runner uses it for every async test's
      returned promise — so the virtual timer queue belongs here alongside
      fetch and Atomics.waitAsync. But it must not be reached FIRST. A real-mode
      timer costs no real time, so running one while a fetch was still in
      flight made `Promise.race([fetch(url), timeoutAfter(ms)])` resolve to the
      timeout every single time, whatever ms was, and a live interval could
      spend the entire budget before the response ever arrived. Polling the
      real work first means the race is decided by whether the fetch completes
      at all, which is the outcome a suite writing that race expects. }
    if HasPendingFetch then
    begin
      Sleep(FETCH_POLL_INTERVAL_MS);
      Continue;
    end;

    { A pending async Atomics waiter is not, unlike a fetch, necessarily
      resolved by outside work: a real-mode timer calling Atomics.notify can be
      what wakes it, so timers must still run while one is pending. Service a due
      timer first; only when none is runnable does this poll for a cross-thread
      notify. Blocking timers here left an indefinite Atomics.waitAsync pending
      forever, and CheckInstructionLimit never advanced the counter to break it. }
    if HasPendingAtomicsWaitAsyncCompletions then
    begin
      if HasRunnableRealTimers then
      begin
        if TimersRun >= TIMER_LOOP_LIMIT then
          RaiseRealTimerLoopLimit;
        if RunOneRealTimer then
        begin
          Inc(TimersRun);
          Continue;
        end;
      end;
      Sleep(FETCH_POLL_INTERVAL_MS);
      Continue;
    end;

    { Nothing real is outstanding, so the clock may jump to the next timer. A
      real-mode timer is a continuation no amount of microtask draining will
      produce. Under fake timers this does nothing, because the suite, not the
      engine, decides when those run — and it does nothing for a queue owned by
      another realm either. }
    if TimersRun >= TIMER_LOOP_LIMIT then
    begin
      { Spending the whole budget with timers still runnable is a diagnosis of
        its own, and one the caller cannot make: reported as an unsettled
        promise it read as a missing `await` rather than as a timer that keeps
        rescheduling itself. }
      if HasRunnableRealTimers then
        RaiseRealTimerLoopLimit;
      Exit(False);
    end;

    if not RunOneRealTimer then
      Exit(False);
    Inc(TimersRun);
  end;

  Result := True;
end;

procedure WaitForFetchIdle;
begin
  WaitForFetchIdle(CurrentRealm);
end;

procedure WaitForFetchIdle(const ARealm: TGocciaRealm);
var
  Manager: TGocciaFetchManager;
begin
  Manager := TGocciaFetchManager.Instance;
  if Assigned(Manager) and Manager.HasPendingFor(ARealm) then
    Manager.WaitForIdle(ARealm)
  else
    DrainMicrotasksAndFetchCompletions;
end;

function HasPendingFetches(const ARealm: TGocciaRealm): Boolean;
var
  Manager: TGocciaFetchManager;
begin
  Manager := TGocciaFetchManager.Instance;
  Result := Assigned(Manager) and Manager.HasPendingFor(ARealm);
end;

procedure DiscardFetchCompletions;
begin
  DiscardFetchCompletions(CurrentRealm);
end;

procedure DiscardFetchCompletions(const ARealm: TGocciaRealm);
var
  Manager: TGocciaFetchManager;
begin
  Manager := TGocciaFetchManager.Instance;
  if Assigned(Manager) then
    Manager.DiscardPending(ARealm);
end;

procedure FetchOverNetwork(const ARequest: TGocciaFetchTransportRequest;
  const AResponder: IGocciaFetchResponder);
{$IFNDEF LAKON}
var
  Completion: TGocciaFetchCompletion;
begin
  Completion := TGocciaFetchCompletion.Create(0);
  try
    PerformNetworkRequest(ARequest.URL, ARequest.Method, ARequest.Headers,
      ARequest.Redirect, ARequest.TimeoutMilliseconds, ARequest.Capabilities,
      ARequest.MaxResponseBytes, Completion);
    if Completion.Success then
      AResponder.Respond(Completion.Response)
    else
      AResponder.Fail(Completion.ErrorMessage, Completion.DeniedScope);
  finally
    Completion.Free;
  end;
end;
{$ELSE}
begin
  AResponder.Fail(FETCH_NETWORK_UNAVAILABLE_ERROR);
end;
{$ENDIF}

end.
