program Goccia.FetchTransport.Test;

{ The embedder's fetch transport (ADR 0146): a request reaches the engine's
  own transport instead of the network, answered synchronously or later from
  another thread, with errors, aborts, deadlines and the worker cap applied
  as for the built-in client; no transport leaves fetch() unchanged; engines
  and threads keep their own transports; and a test embedder records a run
  over a loopback server and replays it without one. }

{$I Goccia.inc}

uses
  {$IFDEF UNIX}
  cthreads,
  BaseUnix,
  Sockets,
  {$ENDIF}
  Classes,
  SysUtils,

  HTTPTypes,
  TestingPascalLibrary,

  Goccia.Capabilities,
  Goccia.Engine,
  Goccia.FetchManager,
  Goccia.FetchTransport,
  Goccia.GarbageCollector,
  Goccia.Runtime,
  Goccia.RuntimeExtensions.Fetch,
  Goccia.TestSetup,
  Goccia.Threading,
  Goccia.Timeout,
  Goccia.Values.ObjectValue,
  Goccia.Values.PromiseValue;

const
  API_HOST = 'api.example';
  API_URL = 'http://' + API_HOST + '/items?id=1';
  LOOPBACK_HOST = '127.0.0.1';
  // Nothing listens on port 1.
  CLOSED_LOOPBACK_URL = 'http://' + LOOPBACK_HOST + ':1/';
  { Allowed by name, refused by the built-in client once it resolves to the
    loopback address when private ranges are denied: no network needed. }
  LOCALHOST_NAME = 'localhost';
  CLOSED_LOCALHOST_URL = 'http://' + LOCALHOST_NAME + ':1/';
  LOCALHOST_DENIAL = 'PermissionDenied: net: ' + LOCALHOST_NAME + ':1';
  ANSWER_DELAY_MS = 100;
  RUN_DEADLINE_MS = 200;
  WORKER_CAP = 16;
  THREAD_COUNT = 4;
  REQUESTS_PER_THREAD = 3;
  SETTLE_DEADLINE_MS = 10000;
  REPORT_OUTCOME = '.then((response) => response.text().then((body) => {' +
    ' globalThis.outcome = response.status + "|" + body; }),' +
    ' (error) => { globalThis.outcome = error.name + ": " + error.message; });';

type
  TTransportMode = (tmAnswer, tmHold, tmAnswerLater, tmFail, tmDeny,
    tmRaise, tmAnswerThenRaise);

  { Answers on a thread of its own after ANSWER_DELAY_MS. }
  TDelayedAnswer = class(TThread)
  private
    FResponder: IGocciaFetchResponder;
    FResponse: THTTPResponse;
    FAccepted: Boolean;
  protected
    procedure Execute; override;
  public
    constructor Create(const AResponder: IGocciaFetchResponder;
      const AResponse: THTTPResponse);
    property Accepted: Boolean read FAccepted;
  end;

  TFakeTransport = class(TInterfacedObject, IGocciaFetchTransport)
  private
    FMode: TTransportMode;
    FResponse: THTTPResponse;
    FRequests: array of TGocciaFetchTransportRequest;
    FResponder: IGocciaFetchResponder;
    FResponders: array of IGocciaFetchResponder;
    FAnswerTaken: Boolean;
    FLater: TList;
  public
    constructor Create(const AMode: TTransportMode);
    destructor Destroy; override;
    procedure Fetch(const ARequest: TGocciaFetchTransportRequest;
      const AResponder: IGocciaFetchResponder);
    function RequestCount: Integer;
    function Request(const AIndex: Integer): TGocciaFetchTransportRequest;
    { Joins the threads tmAnswerLater requests started; True when every
      answer they gave was taken. }
    function JoinLaterAnswers: Boolean;
    { The responder of request AIndex. }
    function ResponderAt(const AIndex: Integer): IGocciaFetchResponder;
    property Mode: TTransportMode read FMode write FMode;
    property Response: THTTPResponse read FResponse write FResponse;
    property Responder: IGocciaFetchResponder read FResponder;
    { What the last answer given inside Fetch returned. }
    property AnswerTaken: Boolean read FAnswerTaken;
  end;

  TGocciaPromiseArray = array of TGocciaPromiseValue;

  TFetchTransportTests = class(TTestSuite)
  private
    procedure TestWithoutTransportUsesNetwork;
    procedure TestSynchronousAnswer;
    procedure TestHeadAnswerHasNoBody;
    procedure TestNullBodyStatusAndFragment;
    procedure TestLaterAnswerKeepsRunWaiting;
    procedure TestFailedAnswerRejectsWithTypeError;
    procedure TestDeniedAnswerRejectsWithPermissionDenied;
    procedure TestRaisingTransportRejects;
    procedure TestInvalidStatusRejects;
    procedure TestInvalidHeaderRejects;
    procedure TestOversizedBodyRejects;
    procedure TestAbortCancelsRequest;
    procedure TestTimeoutSignalCancelsRequest;
    procedure TestRunDeadlineEndsWait;
    procedure TestPendingUntilAnswered;
    procedure TestRequestsShareWorkerCap;
    procedure TestAbortFreesOneSlot;
    procedure TestUnsendableRequestNeverReachesTransport;
    procedure TestOversizedNullBodyIsDropped;
    procedure TestStatusTextIsAReasonPhrase;
    procedure TestFinalURLMustBeAURL;
    procedure TestAbortAfterAnswerCancels;
    procedure TestRacingAnswersOneWins;
    procedure TestEnginesKeepTheirOwnTransport;
    procedure TestThreadsKeepTheirOwnTransport;
    procedure TestRecorderSkipsRefusedAnswers;
    procedure TestRecordThenReplay;
  public
    procedure SetupTests; override;
  end;

function TextBytes(const AText: string): TBytes;
begin
  Result := TEncoding.UTF8.GetBytes(AText);
end;

function TextResponse(const AStatus: Integer;
  const ABody: string): THTTPResponse;
begin
  Result := Default(THTTPResponse);
  Result.StatusCode := AStatus;
  Result.StatusText := 'OK';
  Result.Body := TextBytes(ABody);
end;

{ TDelayedAnswer }

constructor TDelayedAnswer.Create(const AResponder: IGocciaFetchResponder;
  const AResponse: THTTPResponse);
begin
  FResponder := AResponder;
  FResponse := AResponse;
  inherited Create(False);
end;

procedure TDelayedAnswer.Execute;
begin
  Sleep(ANSWER_DELAY_MS);
  FAccepted := FResponder.Respond(FResponse);
  FResponder := nil;
end;

{ TFakeTransport }

constructor TFakeTransport.Create(const AMode: TTransportMode);
begin
  inherited Create;
  FMode := AMode;
  FResponse := TextResponse(200, 'answer');
  FLater := TList.Create;
end;

destructor TFakeTransport.Destroy;
begin
  JoinLaterAnswers;
  FLater.Free;
  inherited;
end;

procedure TFakeTransport.Fetch(const ARequest: TGocciaFetchTransportRequest;
  const AResponder: IGocciaFetchResponder);
begin
  SetLength(FRequests, Length(FRequests) + 1);
  FRequests[High(FRequests)] := ARequest;
  FResponder := AResponder;
  SetLength(FResponders, Length(FResponders) + 1);
  FResponders[High(FResponders)] := AResponder;
  case FMode of
    tmAnswer:
      FAnswerTaken := AResponder.Respond(FResponse);
    tmAnswerLater:
      FLater.Add(TDelayedAnswer.Create(AResponder, FResponse));
    tmFail:
      AResponder.Fail('connection reset by peer');
    tmDeny:
      AResponder.Fail('fetch destination not allowed', 'other.example');
    tmRaise:
      raise Exception.Create('transport down');
    tmAnswerThenRaise:
      begin
        FAnswerTaken := AResponder.Respond(FResponse);
        raise Exception.Create('transport down');
      end;
  end;
end;

function TFakeTransport.ResponderAt(
  const AIndex: Integer): IGocciaFetchResponder;
begin
  Result := FResponders[AIndex];
end;

function TFakeTransport.RequestCount: Integer;
begin
  Result := Length(FRequests);
end;

function TFakeTransport.Request(
  const AIndex: Integer): TGocciaFetchTransportRequest;
begin
  Result := FRequests[AIndex];
end;

function TFakeTransport.JoinLaterAnswers: Boolean;
var
  I: Integer;
  Later: TDelayedAnswer;
begin
  Result := FLater.Count > 0;
  for I := 0 to FLater.Count - 1 do
  begin
    Later := TDelayedAnswer(FLater[I]);
    Later.WaitFor;
    Result := Result and Later.Accepted;
    Later.Free;
  end;
  FLater.Clear;
end;

{ Engine helpers }

function CreateFetchEngine(const ASource: TStringList;
  const ACapabilities: TGocciaCapabilities): TGocciaEngine;
begin
  Result := TGocciaEngine.Create('<fetch-transport>.js', ASource,
    ACapabilities);
  try
    AttachRuntime(Result).Install(TGocciaFetchRuntimeExtension.Create);
  except
    Result.Free;
    raise;
  end;
end;

function ApiCapabilities: TGocciaCapabilities;
begin
  Result := TGocciaCapabilities.None.Allow(gcNet, API_HOST);
end;

function LocalhostCapabilities: TGocciaCapabilities;
begin
  Result := TGocciaCapabilities.None.Allow(gcNet, LOCALHOST_NAME)
    .Deny(gcNet, NET_PRIVATE_SCOPE);
end;

function GlobalText(const AEngine: TGocciaEngine; const AName: string): string;
begin
  Result := TGocciaObjectValue(AEngine.Realm.GlobalObject).GetProperty(AName)
    .ToStringLiteral.Value;
end;

{ Runs AScript on a fresh engine with ATransport (nil for none) and returns
  globalThis.outcome. }
function RunWithTransport(const AScript: string;
  const ATransport: IGocciaFetchTransport;
  const ACapabilities: TGocciaCapabilities;
  const AMaxResponseBytes: Integer = 0): string;
var
  Source: TStringList;
  Engine: TGocciaEngine;
begin
  Source := TStringList.Create;
  try
    Source.Text := 'globalThis.outcome = "pending";' + sLineBreak + AScript;
    Engine := CreateFetchEngine(Source, ACapabilities);
    try
      Engine.FetchTransport := ATransport;
      Engine.FetchMaxResponseBytes := AMaxResponseBytes;
      Engine.Execute;
      Result := GlobalText(Engine, 'outcome');
    finally
      Engine.Free;
    end;
  finally
    Source.Free;
  end;
end;

function FetchScript(const AURL: string): string;
begin
  Result := 'fetch("' + AURL + '")' + REPORT_OUTCOME;
end;

{ TFetchTransportTests }

procedure TFetchTransportTests.SetupTests;
begin
  Test('without a transport, fetch uses the built-in network client',
    TestWithoutTransportUsesNetwork);
  Test('a transport receives the request and answers it synchronously',
    TestSynchronousAnswer);
  Test('a HEAD answer has no body', TestHeadAnswerHasNoBody);
  Test('a 204 answer has no body and Response.url excludes the fragment',
    TestNullBodyStatusAndFragment);
  Test('an answer from another thread keeps the run waiting for it',
    TestLaterAnswerKeepsRunWaiting);
  Test('a network error answer rejects with TypeError',
    TestFailedAnswerRejectsWithTypeError);
  Test('a refused destination answer rejects with PermissionDenied',
    TestDeniedAnswerRejectsWithPermissionDenied);
  Test('a transport that raises rejects the fetch with TypeError',
    TestRaisingTransportRejects);
  Test('a status outside 200..599 rejects with TypeError',
    TestInvalidStatusRejects);
  Test('a header name that is not a token rejects with TypeError',
    TestInvalidHeaderRejects);
  Test('a body over the response ceiling rejects with TypeError',
    TestOversizedBodyRejects);
  Test('an abort cancels the request and a later answer is ignored',
    TestAbortCancelsRequest);
  Test('AbortSignal.timeout ends the wait for an answer',
    TestTimeoutSignalCancelsRequest);
  Test('the run deadline ends the wait for an answer',
    TestRunDeadlineEndsWait);
  Test('an unanswered request is pending work until its answer is pumped',
    TestPendingUntilAnswered);
  Test('transport requests share the per-thread cap and free each slot once',
    TestRequestsShareWorkerCap);
  Test('an abort frees exactly the aborted request''s slot',
    TestAbortFreesOneSlot);
  Test('a request the built-in client would refuse never reaches the ' +
    'transport', TestUnsendableRequestNeverReachesTransport);
  Test('an oversized body of a null body status is dropped, not refused',
    TestOversizedNullBodyIsDropped);
  Test('status text must be a reason phrase', TestStatusTextIsAReasonPhrase);
  Test('a final URL must be an absolute http(s) URL',
    TestFinalURLMustBeAURL);
  Test('an abort after a taken but unsettled answer cancels the request',
    TestAbortAfterAnswerCancels);
  Test('of two racing answers exactly one is taken',
    TestRacingAnswersOneWins);
  Test('engines on one thread each use only their own transport',
    TestEnginesKeepTheirOwnTransport);
  Test('engines on different threads each use only their own transport',
    TestThreadsKeepTheirOwnTransport);
  Test('the test embedder records only answers the engine took',
    TestRecorderSkipsRefusedAnswers);
  Test('a run recorded through the network replays without it',
    TestRecordThenReplay);
end;

procedure TFetchTransportTests.TestWithoutTransportUsesNetwork;
var
  Source: TStringList;
  Engine: TGocciaEngine;
begin
  Source := TStringList.Create;
  try
    Source.Text := 'globalThis.outcome = "pending";' + sLineBreak +
      FetchScript(CLOSED_LOCALHOST_URL);
    Engine := CreateFetchEngine(Source, LocalhostCapabilities);
    try
      Expect<Boolean>(Engine.FetchTransport = nil).ToBe(True);
      Engine.Execute;
      Expect<string>(GlobalText(Engine, 'outcome')).ToBe(LOCALHOST_DENIAL);
    finally
      Engine.Free;
    end;
  finally
    Source.Free;
  end;
end;

procedure TFetchTransportTests.TestSynchronousAnswer;
var
  Fake: TFakeTransport;
  Transport: IGocciaFetchTransport;
  Response: THTTPResponse;
  Request: TGocciaFetchTransportRequest;
  Source: TStringList;
  Engine: TGocciaEngine;
begin
  Fake := TFakeTransport.Create(tmAnswer);
  Transport := Fake;
  Response := TextResponse(201, 'hello');
  Response.StatusText := 'Created';
  SetLength(Response.Headers, 2);
  Response.Headers[0].Name := 'Content-Type';
  Response.Headers[0].Value := 'text/plain';
  Response.Headers[1].Name := 'X-Answer';
  Response.Headers[1].Value := '  padded'#9;
  Fake.Response := Response;
  Source := TStringList.Create;
  try
    Source.Text :=
      'globalThis.outcome = "pending";' + sLineBreak +
      'fetch("' + API_URL + '", { headers: { "X-Trace": "abc" } })' +
      '.then((response) => response.text().then((body) => {' +
      ' globalThis.outcome = [response.status, response.statusText,' +
      ' response.ok, response.url, response.redirected,' +
      ' response.headers.get("content-type"),' +
      ' response.headers.get("x-answer"), body].join("|"); }),' +
      ' (error) => { globalThis.outcome = "rejected: " + error.message; });' +
      sLineBreak +
      // fetch() returned before the answer settled anything.
      'globalThis.duringCall = globalThis.outcome;';
    Engine := CreateFetchEngine(Source, ApiCapabilities);
    try
      Engine.FetchTransport := Transport;
      Engine.Execute;
      Expect<string>(GlobalText(Engine, 'duringCall')).ToBe('pending');
      Expect<string>(GlobalText(Engine, 'outcome')).ToBe(
        '201|Created|true|' + API_URL + '|false|text/plain|padded|hello');
    finally
      Engine.Free;
    end;
  finally
    Source.Free;
  end;

  Expect<Integer>(Fake.RequestCount).ToBe(1);
  Request := Fake.Request(0);
  Expect<string>(Request.Method).ToBe('GET');
  Expect<string>(Request.URL).ToBe(API_URL);
  Expect<Integer>(Length(Request.Headers)).ToBe(1);
  Expect<string>(Request.Headers[0].Name).ToBe('x-trace');
  Expect<string>(Request.Headers[0].Value).ToBe('abc');
  Expect<Integer>(Length(Request.Body)).ToBe(0);
  Expect<Integer>(Request.MaxResponseBytes)
    .ToBe(DEFAULT_MAX_RESPONSE_BODY_BYTES);
  Expect<Boolean>(Request.Capabilities.AllowsNetHost(API_HOST, 80))
    .ToBe(True);
  // The answer was taken and settled: a second one is refused, and the
  // request was not cancelled.
  Expect<Boolean>(Fake.AnswerTaken).ToBe(True);
  Expect<Boolean>(Fake.Responder.Respond(TextResponse(200, 'again')))
    .ToBe(False);
  Expect<Boolean>(Fake.Responder.IsCancelled).ToBe(False);
end;

procedure TFetchTransportTests.TestHeadAnswerHasNoBody;
var
  Fake: TFakeTransport;
  Transport: IGocciaFetchTransport;
begin
  Fake := TFakeTransport.Create(tmAnswer);
  Transport := Fake;
  Expect<string>(RunWithTransport('fetch("' + API_URL +
    '", { method: "head" })' + REPORT_OUTCOME, Transport, ApiCapabilities))
    .ToBe('200|');
  Expect<string>(Fake.Request(0).Method).ToBe('HEAD');
end;

procedure TFetchTransportTests.TestNullBodyStatusAndFragment;
var
  Fake: TFakeTransport;
  Transport: IGocciaFetchTransport;
begin
  Fake := TFakeTransport.Create(tmAnswer);
  Transport := Fake;
  Fake.Response := TextResponse(204, 'ignored');
  Expect<string>(RunWithTransport('fetch("' + API_URL + '#part")' +
    '.then((response) => response.text().then((body) => {' +
    ' globalThis.outcome = response.status + "|" + body + "|" +' +
    ' response.url; }));', Transport, ApiCapabilities))
    .ToBe('204||' + API_URL);
  Expect<string>(Fake.Request(0).URL).ToBe(API_URL + '#part');
end;

procedure TFetchTransportTests.TestLaterAnswerKeepsRunWaiting;
var
  Fake: TFakeTransport;
  Transport: IGocciaFetchTransport;
begin
  Fake := TFakeTransport.Create(tmAnswerLater);
  Transport := Fake;
  Fake.Response := TextResponse(200, 'late');
  Expect<string>(RunWithTransport(FetchScript(API_URL), Transport,
    ApiCapabilities)).ToBe('200|late');
  Expect<Boolean>(Fake.JoinLaterAnswers).ToBe(True);
end;

procedure TFetchTransportTests.TestFailedAnswerRejectsWithTypeError;
var
  Transport: IGocciaFetchTransport;
begin
  Transport := TFakeTransport.Create(tmFail);
  Expect<string>(RunWithTransport(FetchScript(API_URL), Transport,
    ApiCapabilities)).ToBe('TypeError: connection reset by peer');
end;

procedure TFetchTransportTests.TestDeniedAnswerRejectsWithPermissionDenied;
var
  Transport: IGocciaFetchTransport;
begin
  Transport := TFakeTransport.Create(tmDeny);
  Expect<string>(RunWithTransport(FetchScript(API_URL), Transport,
    ApiCapabilities)).ToBe('PermissionDenied: net: other.example');
end;

procedure TFetchTransportTests.TestRaisingTransportRejects;
var
  Fake: TFakeTransport;
  Transport: IGocciaFetchTransport;
begin
  Fake := TFakeTransport.Create(tmRaise);
  Transport := Fake;
  Expect<string>(RunWithTransport(FetchScript(API_URL), Transport,
    ApiCapabilities)).ToBe('TypeError: fetch failed: transport down');
  Expect<Boolean>(Fake.Responder.IsCancelled).ToBe(True);
end;

procedure TFetchTransportTests.TestInvalidStatusRejects;
var
  Fake: TFakeTransport;
  Transport: IGocciaFetchTransport;
begin
  Fake := TFakeTransport.Create(tmAnswer);
  Transport := Fake;
  Fake.Response := TextResponse(99, '');
  Expect<string>(RunWithTransport(FetchScript(API_URL), Transport,
    ApiCapabilities))
    .ToBe('TypeError: fetch transport answered an invalid status: 99');
end;

procedure TFetchTransportTests.TestInvalidHeaderRejects;
var
  Fake: TFakeTransport;
  Transport: IGocciaFetchTransport;
  Response: THTTPResponse;
begin
  Fake := TFakeTransport.Create(tmAnswer);
  Transport := Fake;
  Response := TextResponse(200, 'body');
  SetLength(Response.Headers, 1);
  Response.Headers[0].Name := 'bad name';
  Response.Headers[0].Value := 'x';
  Fake.Response := Response;
  Expect<string>(RunWithTransport(FetchScript(API_URL), Transport,
    ApiCapabilities))
    .ToBe('TypeError: fetch transport answered an invalid header');
  // A header value is bytes: U+00FF passes, U+0100 does not.
  Response.Headers[0].Name := 'x-latin';
  Response.Headers[0].Value := 'a'#$0100;
  Fake.Response := Response;
  Expect<string>(RunWithTransport(FetchScript(API_URL), Transport,
    ApiCapabilities))
    .ToBe('TypeError: fetch transport answered an invalid header');
  Response.Headers[0].Value := 'a'#$00FF;
  Fake.Response := Response;
  Expect<string>(RunWithTransport('fetch("' + API_URL + '").then(' +
    '(response) => { globalThis.outcome = response.headers.get(' +
    '"x-latin"); });', Transport, ApiCapabilities)).ToBe('a'#$00FF);
end;

procedure TFetchTransportTests.TestOversizedBodyRejects;
const
  CEILING = 4;
var
  Fake: TFakeTransport;
  Transport: IGocciaFetchTransport;
begin
  Fake := TFakeTransport.Create(tmAnswer);
  Transport := Fake;
  Fake.Response := TextResponse(200, 'hello');
  Expect<string>(RunWithTransport(FetchScript(API_URL), Transport,
    ApiCapabilities, CEILING))
    .ToBe('TypeError: HTTP response body exceeds 4 byte limit');
  Expect<Integer>(Fake.Request(0).MaxResponseBytes).ToBe(CEILING);
end;

procedure TFetchTransportTests.TestAbortCancelsRequest;
var
  Fake: TFakeTransport;
  Transport: IGocciaFetchTransport;
begin
  Fake := TFakeTransport.Create(tmHold);
  Transport := Fake;
  Expect<string>(RunWithTransport(
    'const controller = new AbortController();' + sLineBreak +
    'fetch("' + API_URL + '", { signal: controller.signal })' +
    REPORT_OUTCOME + sLineBreak +
    'controller.abort();', Transport, ApiCapabilities))
    .ToBe('AbortError: This operation was aborted');
  Expect<Boolean>(Fake.Responder.IsCancelled).ToBe(True);
  Expect<Boolean>(Fake.Responder.Respond(TextResponse(200, 'late')))
    .ToBe(False);
end;

procedure TFetchTransportTests.TestTimeoutSignalCancelsRequest;
var
  Fake: TFakeTransport;
  Transport: IGocciaFetchTransport;
begin
  Fake := TFakeTransport.Create(tmHold);
  Transport := Fake;
  Expect<string>(RunWithTransport('fetch("' + API_URL +
    '", { signal: AbortSignal.timeout(50) })' + REPORT_OUTCOME, Transport,
    ApiCapabilities))
    .ToBe('TimeoutError: The operation was aborted due to timeout');
  Expect<Boolean>(Fake.Request(0).TimeoutMilliseconds > 0).ToBe(True);
  Expect<Boolean>(Fake.Request(0).TimeoutMilliseconds <= 50).ToBe(True);
  Expect<Boolean>(Fake.Responder.IsCancelled).ToBe(True);
end;

procedure TFetchTransportTests.TestRunDeadlineEndsWait;
var
  Fake: TFakeTransport;
  Transport: IGocciaFetchTransport;
  TimedOut: Boolean;
begin
  Fake := TFakeTransport.Create(tmHold);
  Transport := Fake;
  TimedOut := False;
  StartExecutionTimeout(RUN_DEADLINE_MS);
  try
    try
      RunWithTransport(FetchScript(API_URL), Transport, ApiCapabilities);
    except
      on E: TGocciaTimeoutError do
        TimedOut := True;
    end;
  finally
    ClearExecutionTimeout;
  end;
  Expect<Boolean>(TimedOut).ToBe(True);
  Expect<Boolean>(Fake.Request(0).TimeoutMilliseconds > 0).ToBe(True);
  Expect<Boolean>(Fake.Request(0).TimeoutMilliseconds <= RUN_DEADLINE_MS)
    .ToBe(True);
  // The engine is gone; an answer now is refused, not crashed on.
  Expect<Boolean>(Fake.Responder.IsCancelled).ToBe(True);
  Expect<Boolean>(Fake.Responder.Respond(TextResponse(200, 'late')))
    .ToBe(False);
end;

function StartTransportFetch(const AEngine: TGocciaEngine;
  const ATransport: IGocciaFetchTransport): TGocciaPromiseValue;
var
  Policy: TGocciaFetchPolicy;
  Headers: THTTPHeaders;
begin
  Policy := Default(TGocciaFetchPolicy);
  Policy.Capabilities := AEngine.Capabilities;
  Policy.Transport := ATransport;
  SetLength(Headers, 0);
  Result := TGocciaPromiseValue.Create;
  TGarbageCollector.Instance.AddTempRoot(Result);
  TGocciaFetchManager.Instance.StartFetch(API_URL, 'GET', Headers, Policy,
    AEngine.Realm, Result);
end;

procedure TFetchTransportTests.TestPendingUntilAnswered;
var
  Fake: TFakeTransport;
  Transport: IGocciaFetchTransport;
  Source: TStringList;
  Engine: TGocciaEngine;
  Promise: TGocciaPromiseValue;
begin
  Fake := TFakeTransport.Create(tmHold);
  Transport := Fake;
  Source := TStringList.Create;
  Engine := CreateFetchEngine(Source, ApiCapabilities);
  Promise := nil;
  try
    Promise := StartTransportFetch(Engine, Transport);
    Expect<Boolean>(GetRuntime(Engine).HasPendingWork).ToBe(True);
    // Nothing answers, so pumping settles nothing and the work stays.
    Expect<Integer>(TGocciaFetchManager.Instance.PumpCompletions).ToBe(0);
    Expect<Boolean>(GetRuntime(Engine).HasPendingWork).ToBe(True);

    Expect<Boolean>(Fake.Responder.Respond(TextResponse(200, 'ok')))
      .ToBe(True);
    // Answered but not yet pumped: still the engine's pending work.
    Expect<Boolean>(Promise.State = gpsPending).ToBe(True);
    Expect<Boolean>(GetRuntime(Engine).HasPendingWork).ToBe(True);
    Expect<Integer>(TGocciaFetchManager.Instance.PumpCompletions).ToBe(1);
    Expect<Boolean>(Promise.State = gpsFulfilled).ToBe(True);
    Expect<Boolean>(GetRuntime(Engine).HasPendingWork).ToBe(False);
  finally
    if Assigned(Promise) then
      TGarbageCollector.Instance.RemoveTempRoot(Promise);
    Engine.Free;
    Source.Free;
  end;
end;

{ Starts one more request under the transport and says whether it was taken
  (pending) or refused by the cap (rejected). Roots the promise in APromises
  for the caller to unroot. }
function StartCounted(const AEngine: TGocciaEngine;
  const ATransport: IGocciaFetchTransport;
  var APromises: TGocciaPromiseArray): Boolean;
var
  Promise: TGocciaPromiseValue;
begin
  Promise := StartTransportFetch(AEngine, ATransport);
  SetLength(APromises, Length(APromises) + 1);
  APromises[High(APromises)] := Promise;
  Result := Promise.State = gpsPending;
  if not Result then
    Expect<string>(TGocciaObjectValue(Promise.PromiseResult)
      .GetProperty('message').ToStringLiteral.Value)
      .ToBe('fetch worker limit exceeded');
end;

{ With the cap full, frees slots through each exit path and checks that
  exactly as many more requests fit. }
procedure TFetchTransportTests.TestRequestsShareWorkerCap;
var
  Fake: TFakeTransport;
  Transport: IGocciaFetchTransport;
  Source: TStringList;
  Engine: TGocciaEngine;
  Promises: TGocciaPromiseArray;
  I: Integer;
  Raised: Boolean;
  Policy: TGocciaFetchPolicy;
  Headers: THTTPHeaders;
  Raising: TGocciaPromiseValue;
  OtherSource: TStringList;
  Other: TGocciaEngine;
begin
  Fake := TFakeTransport.Create(tmHold);
  Transport := Fake;
  Source := TStringList.Create;
  OtherSource := TStringList.Create;
  Engine := CreateFetchEngine(Source, ApiCapabilities);
  Other := CreateFetchEngine(OtherSource, ApiCapabilities);
  SetLength(Promises, 0);
  try
    for I := 1 to WORKER_CAP do
      Expect<Boolean>(StartCounted(Engine, Transport, Promises)).ToBe(True);
    Expect<Boolean>(StartCounted(Engine, Transport, Promises)).ToBe(False);

    // Settlement frees exactly one slot.
    Expect<Boolean>(Fake.ResponderAt(0).Respond(TextResponse(200, 'ok')))
      .ToBe(True);
    Expect<Integer>(TGocciaFetchManager.Instance.PumpCompletions).ToBe(1);
    Expect<Boolean>(Promises[0].State = gpsFulfilled).ToBe(True);
    Expect<Boolean>(StartCounted(Engine, Transport, Promises)).ToBe(True);
    Expect<Boolean>(StartCounted(Engine, Transport, Promises)).ToBe(False);

    // A transport that answers and then raises frees its slot once.
    Expect<Boolean>(Fake.ResponderAt(1).Fail('gone')).ToBe(True);
    Expect<Integer>(TGocciaFetchManager.Instance.PumpCompletions).ToBe(1);
    Fake.Mode := tmAnswerThenRaise;
    Raised := False;
    Policy := Default(TGocciaFetchPolicy);
    Policy.Capabilities := Engine.Capabilities;
    Policy.Transport := Transport;
    SetLength(Headers, 0);
    Raising := TGocciaPromiseValue.Create;
    TGarbageCollector.Instance.AddTempRoot(Raising);
    try
      try
        TGocciaFetchManager.Instance.StartFetch(API_URL, 'GET', Headers,
          Policy, Engine.Realm, Raising);
      except
        on E: Exception do
          Raised := E.Message = 'transport down';
      end;
      Expect<Boolean>(Raised).ToBe(True);
      Expect<Boolean>(Fake.AnswerTaken).ToBe(True);
      Expect<Boolean>(Fake.Responder.IsCancelled).ToBe(True);
      // Its answer is dropped at the pump; fetch() rejects it instead.
      TGocciaFetchManager.Instance.PumpCompletions;
      Expect<Boolean>(Raising.State = gpsPending).ToBe(True);
    finally
      TGarbageCollector.Instance.RemoveTempRoot(Raising);
    end;
    Fake.Mode := tmHold;
    Expect<Boolean>(StartCounted(Engine, Transport, Promises)).ToBe(True);
    Expect<Boolean>(StartCounted(Engine, Transport, Promises)).ToBe(False);

    // A discard (also what a failed run or its deadline does) frees the
    // slots its engine held, and no more: another engine on the thread keeps
    // the rest of the cap filled.
    GetRuntime(Engine).DiscardPending;
    for I := 1 to WORKER_CAP - 1 do
      Expect<Boolean>(StartCounted(Other, Transport, Promises)).ToBe(True);
    Expect<Boolean>(StartCounted(Engine, Transport, Promises)).ToBe(True);
    Expect<Boolean>(StartCounted(Engine, Transport, Promises)).ToBe(False);
    GetRuntime(Engine).DiscardPending;
    Expect<Boolean>(StartCounted(Other, Transport, Promises)).ToBe(True);
    Expect<Boolean>(StartCounted(Other, Transport, Promises)).ToBe(False);
    GetRuntime(Other).DiscardPending;
  finally
    for I := 0 to High(Promises) do
      TGarbageCollector.Instance.RemoveTempRoot(Promises[I]);
    Other.Free;
    Engine.Free;
    OtherSource.Free;
    Source.Free;
  end;
end;

{ An abort frees exactly the aborted request's slot. }
procedure TFetchTransportTests.TestAbortFreesOneSlot;
var
  Transport: IGocciaFetchTransport;
begin
  Transport := TFakeTransport.Create(tmHold);
  Expect<string>(RunWithTransport(
    'const controllers = Array.from({ length: ' + IntToStr(WORKER_CAP) +
    ' }, () => new AbortController());' + sLineBreak +
    'const settle = (signal) => fetch("' + API_URL + '", { signal })' +
    '.then(() => "fulfilled", (error) => error.message);' + sLineBreak +
    'controllers.forEach((controller) => settle(controller.signal));' +
    sLineBreak +
    'controllers[0].abort();' + sLineBreak +
    'const spare = new AbortController();' + sLineBreak +
    'const fits = settle(spare.signal);' + sLineBreak +
    'const refused = settle(undefined);' + sLineBreak +
    'controllers.forEach((controller) => controller.abort());' + sLineBreak +
    'spare.abort();' + sLineBreak +
    'Promise.all([fits, refused]).then((results) => {' +
    ' globalThis.outcome = results.join("|"); });', Transport,
    ApiCapabilities)).ToBe('This operation was aborted|' +
    'fetch worker limit exceeded');
end;

procedure TFetchTransportTests.TestUnsendableRequestNeverReachesTransport;
var
  Fake: TFakeTransport;
  Transport: IGocciaFetchTransport;
begin
  Fake := TFakeTransport.Create(tmAnswer);
  Transport := Fake;
  // ftp: passes the net check by host name; the client refuses the scheme.
  Expect<string>(RunWithTransport(FetchScript('ftp://' + API_HOST + '/file'),
    Transport, ApiCapabilities)).ToBe('TypeError: Unsupported scheme: ftp');
  Expect<string>(RunWithTransport('fetch("' + API_URL +
    '", { headers: { "x-bad": "a\u0001b" } })' + REPORT_OUTCOME, Transport,
    ApiCapabilities)).ToBe('TypeError: Invalid HTTP header value');
  Expect<Integer>(Fake.RequestCount).ToBe(0);
end;

procedure TFetchTransportTests.TestOversizedNullBodyIsDropped;
const
  CEILING = 4;
var
  Fake: TFakeTransport;
  Transport: IGocciaFetchTransport;
begin
  Fake := TFakeTransport.Create(tmAnswer);
  Transport := Fake;
  Fake.Response := TextResponse(204, 'hello');
  Expect<string>(RunWithTransport(FetchScript(API_URL), Transport,
    ApiCapabilities, CEILING)).ToBe('204|');
end;

procedure TFetchTransportTests.TestStatusTextIsAReasonPhrase;
var
  Fake: TFakeTransport;
  Transport: IGocciaFetchTransport;
  Response: THTTPResponse;
begin
  Fake := TFakeTransport.Create(tmAnswer);
  Transport := Fake;
  Response := TextResponse(200, '');
  Response.StatusText := 'O'#1'K';
  Fake.Response := Response;
  Expect<string>(RunWithTransport(FetchScript(API_URL), Transport,
    ApiCapabilities))
    .ToBe('TypeError: fetch transport answered an invalid status text');
  Response.StatusText := 'OK'#127;
  Fake.Response := Response;
  Expect<string>(RunWithTransport(FetchScript(API_URL), Transport,
    ApiCapabilities))
    .ToBe('TypeError: fetch transport answered an invalid status text');
  // No character past U+00FF stands for a byte.
  Response.StatusText := 'OK '#$0100;
  Fake.Response := Response;
  Expect<string>(RunWithTransport(FetchScript(API_URL), Transport,
    ApiCapabilities))
    .ToBe('TypeError: fetch transport answered an invalid status text');
  // Tab, space and U+0080..U+00FF (obs-text) are allowed.
  Response.StatusText := 'All'#9'good '#$00E9#$00FF;
  Fake.Response := Response;
  Expect<string>(RunWithTransport('fetch("' + API_URL + '").then(' +
    '(response) => { globalThis.outcome = response.statusText; });',
    Transport, ApiCapabilities)).ToBe('All'#9'good '#$00E9#$00FF);
end;

procedure TFetchTransportTests.TestFinalURLMustBeAURL;
var
  Fake: TFakeTransport;
  Transport: IGocciaFetchTransport;
  Response: THTTPResponse;
begin
  Fake := TFakeTransport.Create(tmAnswer);
  Transport := Fake;
  Response := TextResponse(200, 'body');
  Response.FinalURL := 'not a URL#fragment';
  Fake.Response := Response;
  Expect<string>(RunWithTransport(FetchScript(API_URL), Transport,
    ApiCapabilities))
    .ToBe('TypeError: fetch transport answered an invalid URL');
  // WHATWG URL: a space is a forbidden host code point.
  Response.FinalURL := 'http://bad host/';
  Fake.Response := Response;
  Expect<string>(RunWithTransport(FetchScript(API_URL), Transport,
    ApiCapabilities))
    .ToBe('TypeError: fetch transport answered an invalid URL');
  // Serialized as a URL: scheme and host lower-cased, the default port and
  // dot segments gone, the fragment excluded.
  Response.FinalURL := 'HTTP://API.Example:80/a/../moved#top';
  Response.Redirected := True;
  Fake.Response := Response;
  Expect<string>(RunWithTransport('fetch("' + API_URL + '").then(' +
    '(response) => { globalThis.outcome = response.url + "|" +' +
    ' response.redirected; });', Transport, ApiCapabilities))
    .ToBe('http://' + API_HOST + '/moved|true');
end;

procedure TFetchTransportTests.TestAbortAfterAnswerCancels;
var
  Fake: TFakeTransport;
  Transport: IGocciaFetchTransport;
begin
  Fake := TFakeTransport.Create(tmAnswer);
  Transport := Fake;
  // The answer is taken inside fetch(); the abort comes before any pump.
  Expect<string>(RunWithTransport(
    'const controller = new AbortController();' + sLineBreak +
    'fetch("' + API_URL + '", { signal: controller.signal })' +
    REPORT_OUTCOME + sLineBreak +
    'controller.abort();', Transport, ApiCapabilities))
    .ToBe('AbortError: This operation was aborted');
  Expect<Boolean>(Fake.AnswerTaken).ToBe(True);
  Expect<Boolean>(Fake.Responder.IsCancelled).ToBe(True);
end;

type
  { One of two answers raced against each other on one responder. }
  TRacingAnswer = class(TThread)
  private
    FResponder: IGocciaFetchResponder;
    FGate: PLongint;
    FRespond: Boolean;
    FTaken: Boolean;
  protected
    procedure Execute; override;
  public
    constructor Create(const AResponder: IGocciaFetchResponder;
      const AGate: PLongint; const ARespond: Boolean);
    property Taken: Boolean read FTaken;
  end;

constructor TRacingAnswer.Create(const AResponder: IGocciaFetchResponder;
  const AGate: PLongint; const ARespond: Boolean);
begin
  FResponder := AResponder;
  FGate := AGate;
  FRespond := ARespond;
  inherited Create(False);
end;

procedure TRacingAnswer.Execute;
begin
  while InterlockedCompareExchange(FGate^, 0, 0) = 0 do
    ThreadSwitch;
  if FRespond then
    FTaken := FResponder.Respond(TextResponse(200, 'responded'))
  else
    FTaken := FResponder.Fail('failed');
  FResponder := nil;
end;

procedure TFetchTransportTests.TestRacingAnswersOneWins;
const
  ROUNDS = 50;
var
  Fake: TFakeTransport;
  Transport: IGocciaFetchTransport;
  Source: TStringList;
  Engine: TGocciaEngine;
  Promise: TGocciaPromiseValue;
  Gate: Longint;
  Responding, Failing: TRacingAnswer;
  Round, Taken: Integer;
begin
  Fake := TFakeTransport.Create(tmHold);
  Transport := Fake;
  Source := TStringList.Create;
  Engine := CreateFetchEngine(Source, ApiCapabilities);
  try
    for Round := 1 to ROUNDS do
    begin
      Promise := StartTransportFetch(Engine, Transport);
      try
        Gate := 0;
        Responding := TRacingAnswer.Create(Fake.Responder, @Gate, True);
        Failing := TRacingAnswer.Create(Fake.Responder, @Gate, False);
        InterlockedExchange(Gate, 1);
        // TThread.WaitFor alone sleeps in coarse steps.
        while not (Responding.Finished and Failing.Finished) do
          ThreadSwitch;
        Responding.WaitFor;
        Failing.WaitFor;
        Taken := Ord(Responding.Taken) + Ord(Failing.Taken);
        Expect<Integer>(Taken).ToBe(1);
        Expect<Integer>(TGocciaFetchManager.Instance.PumpCompletions)
          .ToBe(1);
        if Responding.Taken then
          Expect<Boolean>(Promise.State = gpsFulfilled).ToBe(True)
        else
          Expect<Boolean>(Promise.State = gpsRejected).ToBe(True);
        Expect<Boolean>(Fake.Responder.IsCancelled).ToBe(False);
        Responding.Free;
        Failing.Free;
      finally
        TGarbageCollector.Instance.RemoveTempRoot(Promise);
      end;
    end;
  finally
    Engine.Free;
    Source.Free;
  end;
end;

procedure TFetchTransportTests.TestEnginesKeepTheirOwnTransport;
var
  FakeA, FakeB: TFakeTransport;
  TransportA, TransportB: IGocciaFetchTransport;
  SourceA, SourceB, SourceC: TStringList;
  EngineA, EngineB, EngineC: TGocciaEngine;
begin
  FakeA := TFakeTransport.Create(tmAnswer);
  TransportA := FakeA;
  FakeA.Response := TextResponse(200, 'from A');
  FakeB := TFakeTransport.Create(tmAnswer);
  TransportB := FakeB;
  FakeB.Response := TextResponse(200, 'from B');
  SourceA := TStringList.Create;
  SourceB := TStringList.Create;
  SourceC := TStringList.Create;
  EngineA := nil;
  EngineB := nil;
  EngineC := nil;
  try
    SourceA.Text := FetchScript(API_URL);
    SourceB.Text := FetchScript(API_URL);
    SourceC.Text := FetchScript(CLOSED_LOCALHOST_URL);
    // All three share this thread's fetch manager.
    EngineA := CreateFetchEngine(SourceA, ApiCapabilities);
    EngineB := CreateFetchEngine(SourceB, ApiCapabilities);
    EngineC := CreateFetchEngine(SourceC, LocalhostCapabilities);
    EngineA.FetchTransport := TransportA;
    EngineB.FetchTransport := TransportB;

    EngineA.Execute;
    EngineC.Execute;
    EngineB.Execute;
    Expect<string>(GlobalText(EngineA, 'outcome')).ToBe('200|from A');
    Expect<string>(GlobalText(EngineB, 'outcome')).ToBe('200|from B');
    Expect<string>(GlobalText(EngineC, 'outcome')).ToBe(LOCALHOST_DENIAL);
    Expect<Integer>(FakeA.RequestCount).ToBe(1);
    Expect<Integer>(FakeB.RequestCount).ToBe(1);

    // Replaced between runs: the next request goes to the new transport.
    EngineA.FetchTransport := TransportB;
    EngineA.Execute;
    Expect<string>(GlobalText(EngineA, 'outcome')).ToBe('200|from B');
    Expect<Integer>(FakeA.RequestCount).ToBe(1);
    Expect<Integer>(FakeB.RequestCount).ToBe(2);
  finally
    EngineC.Free;
    EngineB.Free;
    EngineA.Free;
    SourceC.Free;
    SourceB.Free;
    SourceA.Free;
  end;
end;

type
  { Runs one engine with a transport of its own on a thread of its own. }
  TTransportThread = class(TThread)
  private
    FLabel: string;
    FOutcome: string;
    FRequests: Integer;
    FError: string;
  protected
    procedure Execute; override;
  public
    constructor Create(const ALabel: string);
    property Outcome: string read FOutcome;
    property Requests: Integer read FRequests;
    property Error: string read FError;
  end;

constructor TTransportThread.Create(const ALabel: string);
begin
  FLabel := ALabel;
  inherited Create(False);
end;

procedure TTransportThread.Execute;
var
  Fake: TFakeTransport;
  Transport: IGocciaFetchTransport;
  Source: TStringList;
  Engine: TGocciaEngine;
begin
  InitThreadRuntime;
  try
    try
      Fake := TFakeTransport.Create(tmAnswerLater);
      Transport := Fake;
      Fake.Response := TextResponse(200, FLabel);
      Source := TStringList.Create;
      try
        Source.Text := 'globalThis.outcome = "pending";' + sLineBreak +
          'Promise.all(Array.from({ length: ' +
          IntToStr(REQUESTS_PER_THREAD) + ' }, () => fetch("' + API_URL +
          '").then((r) => r.text())))' +
          '.then((bodies) => { globalThis.outcome = bodies.join(","); },' +
          ' (error) => { globalThis.outcome = error.message; });';
        Engine := CreateFetchEngine(Source, ApiCapabilities);
        try
          Engine.FetchTransport := Transport;
          Engine.Execute;
          FOutcome := GlobalText(Engine, 'outcome');
        finally
          Engine.Free;
        end;
      finally
        Source.Free;
      end;
      FRequests := Fake.RequestCount;
      Transport := nil;
    except
      on E: Exception do
        FError := E.ClassName + ': ' + E.Message;
    end;
  finally
    ShutdownThreadRuntime;
  end;
end;

procedure TFetchTransportTests.TestThreadsKeepTheirOwnTransport;
var
  Threads: array[0..THREAD_COUNT - 1] of TTransportThread;
  Expected: string;
  I, J: Integer;
  Transport: IGocciaFetchTransport;
begin
  // Warm the shared prototypes on this thread before the workers start.
  Transport := TFakeTransport.Create(tmAnswer);
  Expect<string>(RunWithTransport(FetchScript(API_URL), Transport,
    ApiCapabilities)).ToBe('200|answer');

  for I := 0 to THREAD_COUNT - 1 do
    Threads[I] := TTransportThread.Create('thread-' + IntToStr(I));
  for I := 0 to THREAD_COUNT - 1 do
  begin
    Threads[I].WaitFor;
    Expected := '';
    for J := 1 to REQUESTS_PER_THREAD do
    begin
      if J > 1 then
        Expected := Expected + ',';
      Expected := Expected + 'thread-' + IntToStr(I);
    end;
    Expect<string>(Threads[I].Error).ToBe('');
    Expect<string>(Threads[I].Outcome).ToBe(Expected);
    Expect<Integer>(Threads[I].Requests).ToBe(REQUESTS_PER_THREAD);
    Threads[I].Free;
  end;
end;

{ A test embedder's record and replay. The recording transport forwards each
  request to the network through FetchOverNetwork on a thread of its own and
  keeps every answer the engine took on a tape, under the request's fingerprint, in the
  order it was given; the replay transport answers each request from the
  tape, in that order, without the network. }
type
  TTapeEntry = record
    Fingerprint: string;
    Succeeded: Boolean;
    Response: THTTPResponse;
    ErrorMessage: string;
    DeniedScope: string;
  end;

  TTape = class
  private
    FLock: TRTLCriticalSection;
    FEntries: array of TTapeEntry;
    FConsumed: array of Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(const AEntry: TTapeEntry);
    { The first unconsumed entry for AFingerprint, consumed. }
    function Take(const AFingerprint: string; out AEntry: TTapeEntry): Boolean;
    function Count: Integer;
    function Unconsumed: Integer;
  end;

  TRecordingResponder = class(TInterfacedObject, IGocciaFetchResponder)
  private
    FInner: IGocciaFetchResponder;
    FTape: TTape;
    FFingerprint: string;
  public
    constructor Create(const AInner: IGocciaFetchResponder;
      const ATape: TTape; const AFingerprint: string);
    function Respond(const AResponse: THTTPResponse): Boolean;
    function Fail(const AMessage: string;
      const ADeniedScope: string = ''): Boolean;
    function IsCancelled: Boolean;
  end;

  TForwardThread = class(TThread)
  private
    FRequest: TGocciaFetchTransportRequest;
    FResponder: IGocciaFetchResponder;
  protected
    procedure Execute; override;
  public
    constructor Create(const ARequest: TGocciaFetchTransportRequest;
      const AResponder: IGocciaFetchResponder);
  end;

  TRecordingTransport = class(TInterfacedObject, IGocciaFetchTransport)
  private
    FTape: TTape;
    FThreads: TList;
  public
    constructor Create(const ATape: TTape);
    destructor Destroy; override;
    procedure Fetch(const ARequest: TGocciaFetchTransportRequest;
      const AResponder: IGocciaFetchResponder);
  end;

  TReplayTransport = class(TInterfacedObject, IGocciaFetchTransport)
  private
    FTape: TTape;
  public
    constructor Create(const ATape: TTape);
    procedure Fetch(const ARequest: TGocciaFetchTransportRequest;
      const AResponder: IGocciaFetchResponder);
  end;

{ Method, URL and the header list, names lower-cased, sorted. }
function Fingerprint(const ARequest: TGocciaFetchTransportRequest): string;
var
  Lines: TStringList;
  I: Integer;
begin
  Lines := TStringList.Create;
  try
    for I := 0 to High(ARequest.Headers) do
      Lines.Add(LowerCase(ARequest.Headers[I].Name) + ':' +
        ARequest.Headers[I].Value);
    Lines.Sort;
    Result := ARequest.Method + ' ' + ARequest.URL + #10 + Lines.Text;
  finally
    Lines.Free;
  end;
end;

constructor TTape.Create;
begin
  inherited Create;
  InitCriticalSection(FLock);
end;

destructor TTape.Destroy;
begin
  DoneCriticalSection(FLock);
  inherited;
end;

procedure TTape.Add(const AEntry: TTapeEntry);
begin
  EnterCriticalSection(FLock);
  try
    SetLength(FEntries, Length(FEntries) + 1);
    FEntries[High(FEntries)] := AEntry;
    SetLength(FConsumed, Length(FEntries));
    FConsumed[High(FConsumed)] := False;
  finally
    LeaveCriticalSection(FLock);
  end;
end;

function TTape.Take(const AFingerprint: string;
  out AEntry: TTapeEntry): Boolean;
var
  I: Integer;
begin
  EnterCriticalSection(FLock);
  try
    for I := 0 to High(FEntries) do
      if (not FConsumed[I]) and (FEntries[I].Fingerprint = AFingerprint) then
      begin
        FConsumed[I] := True;
        AEntry := FEntries[I];
        Exit(True);
      end;
    Result := False;
  finally
    LeaveCriticalSection(FLock);
  end;
end;

function TTape.Count: Integer;
begin
  EnterCriticalSection(FLock);
  try
    Result := Length(FEntries);
  finally
    LeaveCriticalSection(FLock);
  end;
end;

function TTape.Unconsumed: Integer;
var
  I: Integer;
begin
  EnterCriticalSection(FLock);
  try
    Result := 0;
    for I := 0 to High(FConsumed) do
      if not FConsumed[I] then
        Inc(Result);
  finally
    LeaveCriticalSection(FLock);
  end;
end;

constructor TRecordingResponder.Create(const AInner: IGocciaFetchResponder;
  const ATape: TTape; const AFingerprint: string);
begin
  inherited Create;
  FInner := AInner;
  FTape := ATape;
  FFingerprint := AFingerprint;
end;

function TRecordingResponder.Respond(const AResponse: THTTPResponse): Boolean;
var
  Entry: TTapeEntry;
begin
  // Only an answer the engine took is part of the run.
  Result := FInner.Respond(AResponse);
  if not Result then
    Exit;
  Entry := Default(TTapeEntry);
  Entry.Fingerprint := FFingerprint;
  Entry.Succeeded := True;
  // A private copy: the tape outlives the caller's arrays.
  Entry.Response := AResponse;
  Entry.Response.Headers := Copy(AResponse.Headers);
  Entry.Response.Body := Copy(AResponse.Body);
  FTape.Add(Entry);
end;

function TRecordingResponder.Fail(const AMessage: string;
  const ADeniedScope: string): Boolean;
var
  Entry: TTapeEntry;
begin
  Result := FInner.Fail(AMessage, ADeniedScope);
  if not Result then
    Exit;
  Entry := Default(TTapeEntry);
  Entry.Fingerprint := FFingerprint;
  Entry.ErrorMessage := AMessage;
  Entry.DeniedScope := ADeniedScope;
  FTape.Add(Entry);
end;

function TRecordingResponder.IsCancelled: Boolean;
begin
  Result := FInner.IsCancelled;
end;

constructor TForwardThread.Create(const ARequest: TGocciaFetchTransportRequest;
  const AResponder: IGocciaFetchResponder);
begin
  FRequest := ARequest;
  FResponder := AResponder;
  inherited Create(False);
end;

procedure TForwardThread.Execute;
begin
  FetchOverNetwork(FRequest, FResponder);
  FResponder := nil;
end;

constructor TRecordingTransport.Create(const ATape: TTape);
begin
  inherited Create;
  FTape := ATape;
  FThreads := TList.Create;
end;

destructor TRecordingTransport.Destroy;
var
  I: Integer;
begin
  for I := 0 to FThreads.Count - 1 do
  begin
    TForwardThread(FThreads[I]).WaitFor;
    TForwardThread(FThreads[I]).Free;
  end;
  FThreads.Free;
  inherited;
end;

procedure TRecordingTransport.Fetch(
  const ARequest: TGocciaFetchTransportRequest;
  const AResponder: IGocciaFetchResponder);
begin
  FThreads.Add(TForwardThread.Create(ARequest,
    TRecordingResponder.Create(AResponder, FTape, Fingerprint(ARequest))));
end;

constructor TReplayTransport.Create(const ATape: TTape);
begin
  inherited Create;
  FTape := ATape;
end;

procedure TReplayTransport.Fetch(const ARequest: TGocciaFetchTransportRequest;
  const AResponder: IGocciaFetchResponder);
var
  Entry: TTapeEntry;
begin
  if not FTape.Take(Fingerprint(ARequest), Entry) then
    AResponder.Fail('no recorded response for ' + ARequest.URL)
  else if Entry.Succeeded then
    AResponder.Respond(Entry.Response)
  else
    AResponder.Fail(Entry.ErrorMessage, Entry.DeniedScope);
end;

{$IFDEF UNIX}
type
  { An HTTP origin on the loopback interface: answers AConnections
    connections in turn, the n-th with the body "response-<n>". }
  TLoopbackOrigin = class(TThread)
  private
    FListener: TSocket;
    FConnections: Integer;
    FPort: Word;
  protected
    procedure Execute; override;
  public
    constructor Create(const AConnections: Integer);
    destructor Destroy; override;
    property Port: Word read FPort;
  end;

constructor TLoopbackOrigin.Create(const AConnections: Integer);
var
  Address: TInetSockAddr;
  AddressLength: TSockLen;
begin
  FConnections := AConnections;
  FListener := fpSocket(AF_INET, SOCK_STREAM, 0);
  FillChar(Address, SizeOf(Address), 0);
  Address.sin_family := AF_INET;
  Address.sin_port := 0;
  Address.sin_addr := StrToNetAddr(LOOPBACK_HOST);
  fpBind(FListener, @Address, SizeOf(Address));
  fpListen(FListener, AConnections);
  AddressLength := SizeOf(Address);
  fpGetSockName(FListener, @Address, @AddressLength);
  FPort := NToHs(Address.sin_port);
  inherited Create(False);
end;

destructor TLoopbackOrigin.Destroy;
begin
  CloseSocket(FListener);
  inherited;
end;

procedure TLoopbackOrigin.Execute;
var
  Client: TSocket;
  Buffer: array[0..4095] of Byte;
  Body, Reply: AnsiString;
  I: Integer;
begin
  for I := 1 to FConnections do
  begin
    Client := fpAccept(FListener, nil, nil);
    if Client < 0 then
      Exit;
    fpRecv(Client, @Buffer[0], SizeOf(Buffer), 0);
    Body := 'response-' + AnsiString(IntToStr(I));
    Reply := 'HTTP/1.1 200 OK'#13#10'Content-Type: text/plain'#13#10 +
      'X-Sequence: ' + AnsiString(IntToStr(I)) + #13#10 +
      'Content-Length: ' + AnsiString(IntToStr(Length(Body))) + #13#10 +
      'Connection: close'#13#10#13#10 + Body;
    fpSend(Client, PAnsiChar(Reply), Length(Reply), 0);
    CloseSocket(Client);
  end;
end;
{$ENDIF}

procedure TFetchTransportTests.TestRecorderSkipsRefusedAnswers;
var
  Fake: TFakeTransport;
  Transport: IGocciaFetchTransport;
  Source: TStringList;
  Engine: TGocciaEngine;
  Promise: TGocciaPromiseValue;
  Tape: TTape;
  Recorder: IGocciaFetchResponder;
begin
  Fake := TFakeTransport.Create(tmHold);
  Transport := Fake;
  Source := TStringList.Create;
  Engine := CreateFetchEngine(Source, ApiCapabilities);
  Tape := TTape.Create;
  try
    Promise := StartTransportFetch(Engine, Transport);
    try
      Recorder := TRecordingResponder.Create(Fake.Responder, Tape, 'key');
      // The engine gave up on the request before any answer came.
      GetRuntime(Engine).DiscardPending;
      Expect<Boolean>(Recorder.IsCancelled).ToBe(True);
      Expect<Boolean>(Recorder.Respond(TextResponse(200, 'late'))).ToBe(False);
      Expect<Boolean>(Recorder.Fail('late')).ToBe(False);
      Expect<Integer>(Tape.Count).ToBe(0);
      Recorder := nil;
    finally
      TGarbageCollector.Instance.RemoveTempRoot(Promise);
    end;
  finally
    Tape.Free;
    Engine.Free;
    Source.Free;
  end;
end;

procedure TFetchTransportTests.TestRecordThenReplay;
{$IFDEF UNIX}
const
  ORIGIN_REQUESTS = 3;
var
  Origin: TLoopbackOrigin;
  Tape: TTape;
  Script, Base, Recorded, Replayed: string;
  Capabilities: TGocciaCapabilities;
  Transport: IGocciaFetchTransport;
begin
  Tape := TTape.Create;
  try
    Origin := TLoopbackOrigin.Create(ORIGIN_REQUESTS);
    try
      Base := 'http://' + LOOPBACK_HOST + ':' + IntToStr(Origin.Port);
      Script :=
        'const read = (url, init) => fetch(url, init).then(' +
        '(r) => r.text().then((body) => r.status + " " +' +
        ' r.headers.get("x-sequence") + " " + body),' +
        ' (e) => e.name + ": " + e.message);' + sLineBreak +
        '(async () => {' +
        ' const first = await read("' + Base + '/counter");' +
        ' const second = await read("' + Base + '/counter");' +
        ' const other = await read("' + Base + '/other",' +
        ' { headers: { Accept: "text/plain" } });' +
        ' const refused = await read("' + CLOSED_LOOPBACK_URL + '");' +
        ' globalThis.outcome = [first, second, other, refused].join("\n");' +
        ' })();';
      Capabilities := TGocciaCapabilities.None.Allow(gcNet, LOOPBACK_HOST)
        .Allow(gcNet, NET_PRIVATE_SCOPE);

      Transport := TRecordingTransport.Create(Tape);
      Recorded := RunWithTransport(Script, Transport, Capabilities);
      // Joins the forwarding threads.
      Transport := nil;
      Origin.WaitFor;
    finally
      Origin.Free;
    end;

    Expect<Integer>(Tape.Count).ToBe(ORIGIN_REQUESTS + 1);
    Expect<Boolean>(Pos('200 1 response-1'#10'200 2 response-2'#10 +
      '200 3 response-3'#10'TypeError: ', Recorded) = 1).ToBe(True);

    // The origin is gone: only the tape can answer now.
    Transport := TReplayTransport.Create(Tape);
    Replayed := RunWithTransport(Script, Transport, Capabilities);
    Expect<string>(Replayed).ToBe(Recorded);
    Expect<Integer>(Tape.Unconsumed).ToBe(0);
    Transport := nil;
  finally
    Tape.Free;
  end;
end;
{$ELSE}
begin
  // The loopback origin uses BSD sockets; the transport itself is covered by
  // the tests above on every platform.
end;
{$ENDIF}

begin
  TestRunnerProgram.AddSuite(TFetchTransportTests.Create('Fetch transport'));
  RunGocciaTests;
  ExitCode := TestResultToExitCode;
end.
