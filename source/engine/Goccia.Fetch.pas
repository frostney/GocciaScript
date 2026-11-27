unit Goccia.Fetch;
{ fetch() (docs/built-ins-fetch.md): the host side of prelude/fetch.js's fetch. fetchCheck judges a URL's
  host by the engine's net capability before anything else, synchronously; fetchStart hands the request to a worker
  thread, which performs it with HTTPClient (the shared network client), checking the net capability on every hop,
  before the name is looked up and again against the address it resolved to; the event loop (Goccia.HostAPI) settles
  each finished request through the prelude's settle function.

  A run's requests belong to its engine: FetchDone detaches the ones still running, whose answers are dropped. At
  most 16 requests of a thread are in flight at once, each on its own worker; the ones past that wait their turn, in
  the order they were made, with no thread of their own, and start as workers finish. A waiting request is the
  engine's as a running one is: its storage counts against the memory limit, its signal's deadline runs from the
  call, the run waits for it, and an abort or an aborted run drops it before it starts. A running request that is
  aborted, dropped or detached has its worker's network cancellation cancelled: the worker stops waiting on its
  socket within one poll (NETWORK_CANCELLATION_POLL_MILLISECONDS), closes it and finishes, and its slot frees. }
{$mode delphiunicode}{$H+}
{$I Goccia.Checks.inc}
{$MINFPCONSTPREC 64}
{$POINTERMATH ON}

interface

uses
  Goccia.Capabilities, Goccia.Core;

{ the engine's net capability set and response-body ceiling (0: the default), before its host is installed }
procedure SetFetchPolicy(const Caps: TGocciaCapabilities; MaxResponseBytes: Integer);
{ fetchCheck, fetchStart, fetchCancel and fetchRegister on the host's natives object N }
procedure DefineFetchNatives(N: TValue);
{ whether a request of this engine is in flight }
function FetchPending: Boolean;
{ the id the next request gets: FetchCancelFrom(FetchMark) later cancels every request started since }
function FetchMark: Integer;
{ the requests started since Mark stop being waited for, as their signals' aborts stop them (fetchCancel): a test
  that timed out leaves no request for the next one to wait on }
procedure FetchCancelFrom(Mark: Integer);
{ ends the aborted run's requests and drops their JS completions without settling them }
procedure DiscardFetches;
{ settles the requests that finished; with Block, first waits for one to finish, for a request's signal deadline
  (whose signals are then observed), for the run's interrupt, or until the tick Until_ (0: none). False when script
  failed or the run ended }
function FetchPump(Block: Boolean; Until_: QWord = 0): Boolean;
{ the engine is torn down: its requests in flight are detached, their answers dropped }
procedure FetchDone;

implementation

uses
  SysUtils, Classes, Math, HTTPTypes, HTTPClient, NetworkAddress, NetworkCancellation, Goccia.Builtins, Goccia.Error.Messages,
  Goccia.Error.Suggestions, Goccia.VM, Goccia.Typed, Goccia.ThreadCleanupRegistry;

const
  MAX_FETCH_WORKERS = 16;
  FETCH_WORKER_STACK_SIZE = 8 * 1024 * 1024;
  { how long past the run's or its signal's deadline a request's socket may wait: that deadline ends it first }
  FETCH_DEADLINE_SLACK_MS = 2000;
  { the slice the event loop waits in, so that the run's interrupt is seen }
  WAIT_SLICE_MS = 50;
  { how soon the pump observes a waiting request's signal again when observing it did not abort it yet: the host's
    tick and the signal's clock can differ by a tick }
  SIGNAL_RECHECK_MS = 10;

type
  THop = record
    Allowed: Boolean;
    Host, Reason: string;
  end;

  TFetchOwner = class;

  { one request, performed on its own thread }
  TFetchWorker = class(TThread)
  public
    Owner: TFetchOwner;
    Id: Integer;
    URL, Method: string;
    Headers: THTTPHeaders;
    Redirect: THTTPRedirectMode;
    TimeoutMs, MaxBytes: Integer;
    Caps: TGocciaCapabilities;
    { set under Owner.Lock: the script no longer waits for the answer }
    Cancelled: Boolean;
    { cancelled with Cancelled: the request ends wherever it waits on the network. The worker's own reference }
    Cancellation: TNetworkCancellation;
    Ok: Boolean;
    Response: THTTPResponse;
    ErrorMessage, DeniedScope: string;
    Hops: array of THop;
    NameChecks: Integer;
    function CheckDestination(const AHost: string; const APort: Integer; const AResolvedAddress: string;
      out AReason: string): Boolean;
    procedure Execute; override;
    destructor Destroy; override;
    { under Owner.Lock: the script no longer waits, and the request stops }
    procedure Cancel;
  end;

  { a runtime thread's side of its requests: the workers that finished, under Lock; shared with the workers, freed
    by the last reference }
  TFetchOwner = class
  public
    Lock: TRTLCriticalSection;
    Wake: PRTLEvent;
    Done: TList;
    Detached: Boolean;
    Refs: LongInt;
    constructor Create;
    destructor Destroy; override;
    procedure Release;
  end;

  { a request of this thread: running on its worker, or waiting for one (Worker nil), in the order of the calls }
  TPendingFetch = record
    Id: Integer;
    Worker: TFetchWorker;
    { the request, kept for its worker until it starts }
    URL, Method: string;
    Headers: THTTPHeaders;
    Redirect: THTTPRedirectMode;
    { the bytes of the entry charged against the memory limit (Charge), until it is removed }
    Bytes: Int64;
    { GetTickCount64 at which the pump next has the signals observed for the request (0: none): its signal's deadline,
      then for a request still waiting a recheck shortly after }
    Deadline: QWord;
    { GetTickCount64 at the request's signal deadline (0: none): the socket's timeout when it starts, and for a
      waiting request the time past which it never starts }
    SignalDeadline: QWord;
    { the fetch() call, which the hop decisions are attributed to }
    Site: UnicodeString;
    Line, Column: Integer;
    { the script stopped waiting (fetchCancel): a running entry keeps its slot until its worker finishes, so at
      most MAX_FETCH_WORKERS workers run, but the run no longer waits for it (FetchPending), and its request stops
      within one poll, so the worker finishes then. A waiting entry is removed instead }
    Cancelled: Boolean;
  end;

threadvar
  Caps: TGocciaCapabilities;
  MaxResponseBytes: Integer;
  Owner: TFetchOwner;
  Pendings: array of TPendingFetch;
  NextId: Integer;
  SettleFn, PendingCompletions: TValue;
  RootedGen: Integer;
  { the entries' storage, charged against the memory limit }
  Charge: THeapExternalCharge;

{ ---------------- the owner ---------------- }

constructor TFetchOwner.Create;
begin
  inherited Create;
  InitCriticalSection(Lock);
  Wake := RTLEventCreate;
  Done := TList.Create;
  Refs := 1;
end;

destructor TFetchOwner.Destroy;
begin
  Done.Free;
  RTLEventDestroy(Wake);
  DoneCriticalSection(Lock);
  inherited Destroy;
end;

procedure TFetchOwner.Release;
begin
  if InterlockedDecrement(Refs) = 0 then Free;
end;

{ ---------------- the worker ---------------- }

{ the engine's net rules on every hop: before the name is looked up, and again for the address it resolved to; the
  first hop's name was judged by fetchCheck, so only later hops record a name decision }
function TFetchWorker.CheckDestination(const AHost: string; const APort: Integer; const AResolvedAddress: string;
  out AReason: string): Boolean;
var N: Integer;
begin
  if AResolvedAddress = '' then
  begin
    Inc(NameChecks);
    Result := Caps.AllowsNetHost(AHost, APort);
    if Result then AReason := 'redirect target is allowed by the net capability'
    else AReason := 'redirect target is not allowed by the net capability';
    if NameChecks = 1 then Exit;
  end
  else
  begin
    Result := Caps.AllowsNetAddress(AHost, APort, AResolvedAddress);
    if Result then AReason := 'resolved to ' + AResolvedAddress + ', which the net capability allows'
    else AReason := 'resolved to ' + AResolvedAddress +
      ', a private or denied address the net capability does not allow';
  end;
  N := Length(Hops);
  SetLength(Hops, N + 1);
  Hops[N].Allowed := Result;
  Hops[N].Host := AHost;
  Hops[N].Reason := AReason;
end;

procedure TFetchWorker.Execute;
var Policy: THTTPRequestPolicy; O: TFetchOwner;
begin
  Policy := DefaultHTTPPolicy;
  Policy.MaxResponseBytes := MaxBytes;
  Policy.HostCheck := CheckDestination;
  Policy.RedirectMode := Redirect;
  Policy.Cancellation := Cancellation;
  try
    if Method = 'HEAD' then Response := HTTPHead(URL, Headers, nil, TimeoutMs, Policy)
    else Response := HTTPGet(URL, Headers, nil, TimeoutMs, Policy);
    Ok := True;
  except
    on E: EHTTPDestinationDenied do
    begin
      ErrorMessage := E.Message;
      DeniedScope := E.Scope;
    end;
    on E: EHTTPError do ErrorMessage := E.Message;
    on E: Exception do ErrorMessage := 'fetch failed: ' + E.Message;
  end;
  O := Owner;
  EnterCriticalSection(O.Lock);
  try
    { the engine gone: no one frees the worker but itself. A cancelled request's worker still goes to Done, where
      the engine frees it and its slot (its entry names it until then) }
    if Cancelled then Response := Default(THTTPResponse);
    if O.Detached then FreeOnTerminate := True
    else O.Done.Add(Self);
  finally
    LeaveCriticalSection(O.Lock);
  end;
  RTLEventSetEvent(O.Wake);
  O.Release;
end;

destructor TFetchWorker.Destroy;
begin
  Cancellation.Release;
  inherited Destroy;
end;

procedure TFetchWorker.Cancel;
begin
  Cancelled := True;
  Cancellation.Cancel;
end;

{ ---------------- the policy and the checks ---------------- }

procedure SetFetchPolicy(const Caps: TGocciaCapabilities; MaxResponseBytes: Integer);
begin
  Goccia.Fetch.Caps := Caps;
  if MaxResponseBytes > 0 then Goccia.Fetch.MaxResponseBytes := MaxResponseBytes
  else Goccia.Fetch.MaxResponseBytes := DEFAULT_MAX_RESPONSE_BODY_BYTES;
end;

function ArgOf(Args: PValue; Argc, I: Integer): TValue; inline;
begin
  if I <= Argc then Result := Args[I] else Result := VUndefined;
end;

{ the net scope a script sees in a denial: the host, and the port when the URL names a non-default one }
function NetDenialScope(const Parsed: THTTPParsedURL): string;
begin
  if Pos(':', Parsed.Host) > 0 then Result := '[' + Parsed.Host + ']' else Result := Parsed.Host;
  if not (((Parsed.Scheme = 'http') and (Parsed.Port = 80)) or ((Parsed.Scheme = 'https') and (Parsed.Port = 443))) then
    Result := Result + ':' + IntToStr(Parsed.Port);
end;

{ the host-side hint for a refused destination: the deny that matched, a private address nothing names, or a
  missing host grant }
function NetDenialSuggestion(const Host: string; Port: Integer): string;
var Scope: string; Address: TNetworkAddress;
begin
  case Caps.NetHostVerdict(Host, Port) of
    nhvDenied:
      if Caps.NetDenyScope(Host, Port, Scope) and (Scope <> '') then
        Result := Format(SSuggestFetchDeniedScope, [Scope, Scope])
      else
        Result := SSuggestFetchDeniedAll;
    nhvPrivateNotNamed:
      Result := SSuggestFetchPrivateDestination;
  else
    if TryParseIPAddress(Host, Address) and IsPrivateIPAddress(Address) then
      Result := SSuggestFetchPrivateDestination
    else
      Result := SSuggestFetchAllowedHosts;
  end;
end;

{ fetchCheck(url): the URL parsed and its host judged by the net capability, before any lookup, so a refused
  request has no observable side effect }
function Fetch_Check(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var URL, Host: string; Parsed: THTTPParsedURL; S: TValue;
begin
  if not ToStringV(ArgOf(Args, Argc, 1), S) then Exit(False);
  URL := StrToUnicode(StrOf(S));
  try
    Parsed := ParseHTTPURL(URL, False);
  except
    on E: EHTTPError do
    begin
      try
        Host := HTTPURLAuditHost(URL);
      except
        on EHTTPError do Host := '<invalid URL>';
      end;
      if not AuditDecision(akNetFetch, False, Host, 'fetch URL is invalid') then Exit(False);
      Exit(ThrowTypeError('Invalid fetch URL: ' + E.Message));
    end;
  end;
  if not Caps.AllowsNetHost(Parsed.Host, Parsed.Port) then
  begin
    if not AuditDecision(akNetFetch, False, Parsed.Host, Caps.ExplainNetHostDenial(Parsed.Host, Parsed.Port)) then
      Exit(False);
    if NewPermissionDenied('net', NetDenialScope(Parsed), NetDenialSuggestion(Parsed.Host, Parsed.Port), S) then
      Pending := S;
    Exit(False);
  end;
  if not AuditDecision(akNetFetch, True, Parsed.Host, 'the net capability allows this host') then Exit(False);
  Res := VUndefined;
  Result := True;
end;

function PendingIndex(Id: Integer): Integer; forward;
procedure RemovePending(I: Integer); forward;

{ the finished workers of cancelled requests freed, with their entries }
procedure ReapCancelled;
var I, K: Integer; W: TFetchWorker; Keep, Free_: TList;
begin
  if Owner = nil then Exit;
  Free_ := TList.Create;
  try
    EnterCriticalSection(Owner.Lock);
    try
      Keep := TList.Create;
      try
        for I := 0 to Owner.Done.Count - 1 do
          if TFetchWorker(Owner.Done[I]).Cancelled then Free_.Add(Owner.Done[I]) else Keep.Add(Owner.Done[I]);
        Owner.Done.Assign(Keep);
      finally
        Keep.Free;
      end;
    finally
      LeaveCriticalSection(Owner.Lock);
    end;
    for I := 0 to Free_.Count - 1 do
    begin
      W := TFetchWorker(Free_[I]);
      W.WaitFor;
      K := PendingIndex(W.Id);
      if K >= 0 then RemovePending(K);
      W.Free;
    end;
  finally
    Free_.Free;
  end;
end;

{ the number of entries with a worker, finished or not: the slots taken }
function RunningCount: Integer;
var I: Integer;
begin
  Result := 0;
  for I := 0 to High(Pendings) do
    if Pendings[I].Worker <> nil then Inc(Result);
end;

{ the waiting request P handed to a worker. Its socket's timeout runs past the run's deadline, or past the signal's.
  Each deadline is the event loop's to act on: the run's ends the run (the loop sees its interrupt), and the signal's
  aborts the signal, which rejects the request with the signal's reason (Fetch, "abort fetch"). A socket timeout at
  the same moment must not settle the request first: the pump takes a finished worker before it looks at deadlines,
  so on a loaded runner the script then catches a TypeError instead. The slack keeps each deadline first. Both are
  read when the request starts: the time it waited for a slot is already off them }
procedure StartWorker(var P: TPendingFetch);
var W: TFetchWorker; Timeout, SignalMs: Int64; Now_: QWord;
begin
  Timeout := LimitRemainingMs;
  if Timeout > 0 then Timeout := Timeout + FETCH_DEADLINE_SLACK_MS;
  if P.SignalDeadline <> 0 then
  begin
    Now_ := GetTickCount64;
    if P.SignalDeadline > Now_ then SignalMs := P.SignalDeadline - Now_ else SignalMs := 0;
    if (Timeout = 0) or (SignalMs + FETCH_DEADLINE_SLACK_MS < Timeout) then
      Timeout := SignalMs + FETCH_DEADLINE_SLACK_MS;
  end;
  W := TFetchWorker.Create(True, FETCH_WORKER_STACK_SIZE);
  W.FreeOnTerminate := False;
  W.Owner := Owner;
  InterlockedIncrement(Owner.Refs);
  W.Id := P.Id;
  W.URL := P.URL;
  W.Method := P.Method;
  W.Headers := P.Headers;
  W.Redirect := P.Redirect;
  W.TimeoutMs := Integer(Min(Timeout, High(Integer)));
  W.MaxBytes := MaxResponseBytes;
  W.Caps := Caps;
  W.Cancellation := TNetworkCancellation.Create;
  P.Worker := W;
  { the worker has its own copies now }
  P.Headers := nil;
  W.Start;
end;

{ the waiting requests started, oldest first, while a slot is free; the slots of cancelled requests whose workers
  finished free first. None starts once the run is interrupted (its abort discards them), and none whose signal's
  deadline passed: observing its signal aborts it, which drops it unstarted, and an abort listener that calls fetch()
  before the pump observes the rest must not start them }
procedure StartWaiting;
var I, Running: Integer; Now_: QWord;
begin
  if Interrupt <> 0 then Exit;
  Running := RunningCount;
  if Running = Length(Pendings) then Exit;
  if Running >= MAX_FETCH_WORKERS then
  begin
    ReapCancelled;
    Running := RunningCount;
  end;
  Now_ := GetTickCount64;
  for I := 0 to High(Pendings) do
  begin
    if Running >= MAX_FETCH_WORKERS then Exit;
    if (Pendings[I].Worker = nil) and ((Pendings[I].SignalDeadline = 0) or (Pendings[I].SignalDeadline > Now_)) then
    begin
      StartWorker(Pendings[I]);
      Inc(Running);
    end;
  end;
end;

{ fetchStart(url, method, headers, redirect, signalTimeoutMs): the request's id. It starts on a worker when one of
  the thread's MAX_FETCH_WORKERS slots is free, and otherwise waits its turn behind the requests made before it.
  headers is an array of [name, value] pairs; redirect 0 follow, 1 error, 2 manual; signalTimeoutMs the time left on
  the request's AbortSignal.timeout (-1 none). A RangeError when the request's storage would pass the memory limit }
function Fetch_Start(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var URL, Method: string; S, H, P, E: TValue; I, N, Id: Integer; Len, Bytes, SignalMs: Int64;
  Src: UnicodeString; Line, Col: Integer; Headers: THTTPHeaders; Redirect: THTTPRedirectMode;
begin
  if not ToStringV(ArgOf(Args, Argc, 1), S) then Exit(False);
  URL := StrToUnicode(StrOf(S));
  if not ToStringV(ArgOf(Args, Argc, 2), S) then Exit(False);
  Method := StrToUnicode(StrOf(S));
  H := ArgOf(Args, Argc, 3);
  SetLength(Headers, 0);
  if IsObject(H) and (ObjOf(H)^.OKind = okArray) then
  begin
    Len := ArrayLength(ObjOf(H));
    N := Len;
    SetLength(Headers, N);
    for I := 0 to N - 1 do
    begin
      if not GetElemV(Args[3], MkInt(I), P) then Exit(False);
      if not GetElemV(P, MkInt(0), E) or not ToStringV(E, S) then Exit(False);
      Headers[I].Name := StrToUnicode(StrOf(S));
      if not GetElemV(P, MkInt(1), E) or not ToStringV(E, S) then Exit(False);
      Headers[I].Value := StrToUnicode(StrOf(S));
    end;
  end;
  case Trunc(NumOf(ArgOf(Args, Argc, 4))) of
    1: Redirect := hrmError;
    2: Redirect := hrmManual;
  else Redirect := hrmFollow;
  end;
  { the signal's time left, rounded up to whole milliseconds and held to a range a tick count adds safely; a
    negative value (no signal, or one without a timeout) means none }
  if IsNumber(ArgOf(Args, Argc, 5)) and (NumOf(Args[5]) >= 0) then
    SignalMs := Ceil64(Min(NumOf(Args[5]), 1.0 * High(Integer)))
  else
    SignalMs := -1;
  if not AuditDecision(akNetDispatch, True, URL, 'fetch dispatch is allowed') then Exit(False);
  { the entry keeps the normalized URL, which percent-encoding can make several times longer }
  URL := NormalizeHTTPRequestURL(URL);
  Bytes := SizeOf(TPendingFetch) + (Int64(Length(URL)) + Length(Method)) * SizeOf(Char);
  for I := 0 to High(Headers) do
    Inc(Bytes, (Int64(Length(Headers[I].Name)) + Length(Headers[I].Value)) * SizeOf(Char) + SizeOf(THTTPHeader));
  if CallSiteNow(Src, Line, Col) then Inc(Bytes, Int64(Length(Src)) * SizeOf(WideChar))
  else
  begin
    Src := '';
    Line := 0;
    Col := 0;
  end;
  { collects when the entry does not fit yet; nothing below reads Args }
  if not Charge.TryGrow(Bytes) then Exit(ThrowError('RangeError', SErrorMemoryLimitExceeded));
  if Owner = nil then Owner := TFetchOwner.Create;
  Id := NextId;
  Inc(NextId);
  N := Length(Pendings);
  SetLength(Pendings, N + 1);
  Pendings[N].Id := Id;
  Pendings[N].Worker := nil;
  Pendings[N].URL := URL;
  Pendings[N].Method := Method;
  Pendings[N].Headers := Headers;
  Pendings[N].Redirect := Redirect;
  Pendings[N].Bytes := Bytes;
  { the signal's deadline runs from the call, whether the request starts now or waits its turn. A tick past the
    signal's own: when the pump sees it pass, the signal has timed out too, so observing it aborts the request }
  if SignalMs >= 0 then Pendings[N].Deadline := GetTickCount64 + QWord(SignalMs) + 1 else Pendings[N].Deadline := 0;
  Pendings[N].SignalDeadline := Pendings[N].Deadline;
  Pendings[N].Site := Src;
  Pendings[N].Line := Line;
  Pendings[N].Column := Col;
  Pendings[N].Cancelled := False;
  StartWaiting;
  Res := MkInt(Id);
  Result := True;
end;

function PendingIndex(Id: Integer): Integer;
var I: Integer;
begin
  for I := 0 to High(Pendings) do
    if Pendings[I].Id = Id then Exit(I);
  Result := -1;
end;

procedure RemovePending(I: Integer);
var J: Integer;
begin
  Charge.Release(Pendings[I].Bytes);
  for J := I to High(Pendings) - 1 do Pendings[J] := Pendings[J + 1];
  SetLength(Pendings, Length(Pendings) - 1);
end;

{ the script stopped waiting for entry I: a waiting request is dropped before it starts, and a running one's answer
  will be }
procedure CancelRequest(I: Integer);
var Request: ^TPendingFetch;
begin
  if Pendings[I].Worker = nil then
  begin
    RemovePending(I);
    Exit;
  end;
  Request := @Pendings[I];
  EnterCriticalSection(Owner.Lock);
  try
    Request^.Worker.Cancel;
    { a worker that already posted its reply will not see the cancellation itself }
    if Owner.Done.IndexOf(Request^.Worker) >= 0 then Request^.Worker.Response := Default(THTTPResponse);
  finally
    LeaveCriticalSection(Owner.Lock);
  end;
  { the slot stays taken until the worker finishes, which its cancelled request makes it do at once }
  Request^.Cancelled := True;
  Request^.Deadline := 0;
end;

{ fetchCancel(id): the script stopped waiting (its signal aborted); the worker's answer is dropped }
function Fetch_Cancel(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var I: Integer;
begin
  Res := VUndefined;
  Result := True;
  I := PendingIndex(Trunc(NumOf(ArgOf(Args, Argc, 1))));
  if I < 0 then Exit;
  CancelRequest(I);
end;

function FetchMark: Integer;
begin
  Result := NextId;
end;

procedure FetchCancelFrom(Mark: Integer);
var I: Integer;
begin
  { from the end: a waiting entry is removed }
  for I := High(Pendings) downto 0 do
    if (Pendings[I].Id >= Mark) and not Pendings[I].Cancelled then
      CancelRequest(I);
end;

procedure DiscardFetches;
begin
  FetchCancelFrom(0);
  if SameEngine(RootedGen) and IsObject(PendingCompletions) then DiscardMapEntries(PendingCompletions);
end;

{ fetchRegister(settle): the prelude's settle function, called as settle(id, kind, ...): kind 0 a response
  (status, statusText, [[name, value]...], body ArrayBuffer or null, url, redirected); 1 a rejection (the
  error); 2 a signal deadline passed (the signals are observed); 3 nothing (a call the run's interrupt ends) }
function Fetch_Register(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
begin
  SettleFn := ArgOf(Args, Argc, 1);
  PendingCompletions := ArgOf(Args, Argc, 2);
  Res := VUndefined;
  Result := True;
end;

procedure DefineFetchNatives(N: TValue);
begin
  if FirstInEngine(RootedGen) then
  begin
    AddRoot(@SettleFn);
    AddRoot(@PendingCompletions);
  end;
  SettleFn := VUndefined;
  PendingCompletions := VUndefined;
  if MaxResponseBytes <= 0 then MaxResponseBytes := DEFAULT_MAX_RESPONSE_BODY_BYTES;
  PushRoot(N);
  DefineNative(RootAt(TempTop - 1), 'fetchCheck', Fetch_Check, 1);
  DefineNative(RootAt(TempTop - 1), 'fetchStart', Fetch_Start, 5);
  DefineNative(RootAt(TempTop - 1), 'fetchCancel', Fetch_Cancel, 1);
  DefineNative(RootAt(TempTop - 1), 'fetchRegister', Fetch_Register, 2);
  PopRoots(1);
end;

{ ---------------- settling ---------------- }

function FetchPending: Boolean;
var I: Integer;
begin
  for I := 0 to High(Pendings) do
    if not Pendings[I].Cancelled then Exit(True);
  Result := False;
end;

{ calls the settle function with A, passed from root slots, which a collection inside the call updates (A's values
  are rooted by the caller until then) }
function Settle(const A: array of TValue): Boolean;
var I, Base: Integer; R: TValue;
begin
  if not IsCallable(SettleFn) then Exit(True);
  Base := TempTop;
  for I := 0 to High(A) do PushRoot(A[I]);
  Result := CallValue(SettleFn, VUndefined, @TempRoots[Base], Length(A), R);
  PopRoots(TempTop - Base);
end;

{ the realm's ArrayBuffer.prototype }
function ArrayBufferProtoOf: TValue;
var C: TValue;
begin
  if GetV(GlobalObj, AtomOf('ArrayBuffer'), C) and GetV(C, AtomPrototype, C) then Result := C
  else Result := ObjectProto;
  Pending := VUndefined;
end;

{ a finished request's answer for the script, with the hop decisions it made audited at its fetch() call }
function SettleWorker(W: TFetchWorker; const P: TPendingFetch): Boolean;
var I, Base: Integer; B: PObj; HasBody: Boolean; Len: Int64; E, Pair: TValue; URL: string;
begin
  for I := 0 to High(W.Hops) do
  begin
    SetAuditSite(P.Site, P.Line, P.Column);
    try
      if not AuditDecision(akNetFetch, W.Hops[I].Allowed, W.Hops[I].Host, W.Hops[I].Reason) then Exit(False);
    finally
      ClearAuditSite;
    end;
  end;
  Base := TempTop;
  try
    if not W.Ok then
    begin
      if W.DeniedScope <> '' then
      begin
        if not NewPermissionDenied('net', W.DeniedScope, '', E) then Exit(False);
      end
      else
        E := MakeError(TypeErrorProto, W.ErrorMessage);
      PushRoot(E);
      Exit(Settle([MkInt(P.Id), MkInt(1), RootAt(TempTop - 1)]));
    end;
    { a HEAD response, and a 204, 205 or 304, has no body; the body's bytes count against the memory limit }
    HasBody := (W.Method <> 'HEAD') and not (W.Response.StatusCode in [204, 205, 304]);
    Len := Length(W.Response.Body);
    if HasBody and not HeapReserve(Len) then
    begin
      PushRoot(MakeError(RangeErrorProto,
        'Allocation failed: GC heap size exceeds the configured memory limit'));
      Exit(Settle([MkInt(P.Id), MkInt(1), RootAt(TempTop - 1)]));
    end;
    PushRoot(MkPtr(NewArray(0, Length(W.Response.Headers))));
    for I := 0 to High(W.Response.Headers) do
    begin
      PushRoot(MkPtr(NewArray(0, 2)));
      DefineElemRaw(RootAt(TempTop - 1), 0, StrFromUnicode(W.Response.Headers[I].Name));
      DefineElemRaw(RootAt(TempTop - 1), 1, StrFromUnicode(W.Response.Headers[I].Value));
      Pair := RootAt(TempTop - 1);
      DefineElemRaw(RootAt(TempTop - 2), I, Pair);
      PopRoots(1);
    end;
    if HasBody then
    begin
      B := NewArrayBuffer(Len, ArrayBufferProtoOf);
      if Len > 0 then Move(W.Response.Body[0], BufferData(B)^, Len);
      PushRoot(MkPtr(B));
    end
    else
      PushRoot(VNull);
    URL := W.Response.FinalURL;
    if URL = '' then URL := W.URL;
    { every value rooted before the next allocation: a collection inside StrFromUnicode moves what it reaches, so
      the arguments are read from the root slots only after the last of them is made }
    PushRoot(StrFromUnicode(W.Response.StatusText));
    PushRoot(StrFromUnicode(URL));
    Result := Settle([MkInt(P.Id), MkInt(0), MkInt(W.Response.StatusCode), RootAt(TempTop - 2),
      RootAt(TempTop - 4), RootAt(TempTop - 3), RootAt(TempTop - 1), MkBool(W.Response.Redirected)]);
  finally
    PopRoots(TempTop - Base);
  end;
end;

function FetchPump(Block: Boolean; Until_: QWord): Boolean;
var Taken: TList; I, K, Waits: Integer; W: TFetchWorker; P: TPendingFetch; Now_, Earliest: QWord; Slice: Cardinal;
begin
  Result := True;
  if Owner = nil then Exit;
  { a slot a failed pump left free }
  StartWaiting;
  if Block then
    while True do
    begin
      EnterCriticalSection(Owner.Lock);
      Waits := Owner.Done.Count;
      LeaveCriticalSection(Owner.Lock);
      if Waits > 0 then Break;
      if not FetchPending then Exit;
      { the run's deadline passed: a call into script ends the run }
      if Interrupt <> 0 then Exit(Settle([MkInt(-1), MkInt(3)]));
      Now_ := GetTickCount64;
      Earliest := 0;
      for I := 0 to High(Pendings) do
        if (Pendings[I].Deadline <> 0) and ((Earliest = 0) or (Pendings[I].Deadline < Earliest)) then
          Earliest := Pendings[I].Deadline;
      { a signal's deadline passed: its signal aborts when observed, which rejects and cancels its request. A waiting
        request is observed again shortly, until its signal aborts: it never starts }
      if (Earliest <> 0) and (Earliest <= Now_) then
      begin
        for I := 0 to High(Pendings) do
          if (Pendings[I].Deadline <> 0) and (Pendings[I].Deadline <= Now_) then
            if Pendings[I].Worker = nil then Pendings[I].Deadline := Now_ + SIGNAL_RECHECK_MS
            else Pendings[I].Deadline := 0;
        Exit(Settle([MkInt(-1), MkInt(2)]));
      end;
      { the caller's own deadline (an async wait's timeout): it waits no longer }
      if (Until_ <> 0) and (Until_ <= Now_) then Exit(True);
      Slice := WAIT_SLICE_MS;
      if (Earliest <> 0) and (Earliest - Now_ < Slice) then Slice := Earliest - Now_;
      if (Until_ <> 0) and (Until_ - Now_ < Slice) then Slice := Until_ - Now_;
      RTLEventWaitFor(Owner.Wake, Slice);
      RTLEventResetEvent(Owner.Wake);
    end;
  EnterCriticalSection(Owner.Lock);
  Taken := TList.Create;
  try
    for I := 0 to Owner.Done.Count - 1 do Taken.Add(Owner.Done[I]);
    Owner.Done.Clear;
  finally
    LeaveCriticalSection(Owner.Lock);
  end;
  try
    for I := 0 to Taken.Count - 1 do
    begin
      W := TFetchWorker(Taken[I]);
      Taken[I] := nil;
      W.WaitFor;
      K := PendingIndex(W.Id);
      if W.Cancelled or (K < 0) then
      begin
        { a cancelled request's worker finished: its slot frees now }
        if K >= 0 then RemovePending(K);
        W.Free;
        Continue;
      end;
      P := Pendings[K];
      RemovePending(K);
      try
        if not SettleWorker(W, P) then Result := False;
      finally
        W.Free;
      end;
      if not Result then Break;
    end;
  finally
    { what a failed settle left goes back for the next pump: those requests are still the script's, and their
      entries still name their workers }
    EnterCriticalSection(Owner.Lock);
    try
      for I := Taken.Count - 1 downto 0 do
        if Taken[I] <> nil then Owner.Done.Insert(0, Taken[I]);
    finally
      LeaveCriticalSection(Owner.Lock);
    end;
    Taken.Free;
  end;
  { the slots that freed go to the requests waiting for one, unless the run failed: its abort discards them }
  if Result then StartWaiting;
end;

procedure FetchDone;
var I: Integer; W: TFetchWorker;
begin
  SettleFn := VUndefined;
  PendingCompletions := VUndefined;
  if Owner = nil then Exit;
  EnterCriticalSection(Owner.Lock);
  try
    Owner.Detached := True;
    for I := 0 to High(Pendings) do
      if Pendings[I].Worker <> nil then Pendings[I].Worker.Cancel;
    for I := 0 to Owner.Done.Count - 1 do
    begin
      W := TFetchWorker(Owner.Done[I]);
      W.WaitFor;
      W.Free;
    end;
    Owner.Done.Clear;
  finally
    LeaveCriticalSection(Owner.Lock);
  end;
  Pendings := nil;
  Charge.Clear;
  Owner.Release;
  Owner := nil;
end;

{ the managed threadvars of this unit, released when the thread ends (Goccia.ThreadCleanupRegistry): FPC does not
  finalize a threadvar, and every engine thread leaves its strings, arrays and objects behind otherwise }
procedure GocciaFetchThreadvarCleanup;
begin
  Caps := Default(TGocciaCapabilities);
  Pendings := nil;
end;

initialization
  RegisterThreadvarCleanup(@GocciaFetchThreadvarCleanup);
end.
