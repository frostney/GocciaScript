unit Goccia.Builtins.GlobalFetch;

// Fetch API — global fetch() function and Headers/Response registration
// https://fetch.spec.whatwg.org/

{$I Goccia.inc}

interface

uses
  Goccia.Arguments.Collection,
  Goccia.Builtins.Base,
  Goccia.Capabilities,
  Goccia.CapabilityAudit,
  Goccia.Error.ThrowErrorCallback,
  Goccia.Realm,
  Goccia.Scope,
  Goccia.Values.Primitives;

type
  { Returns the dispatching engine's current response-body ceiling, read at
    request time because it is an engine setting a host may change after the
    runtime is attached. }
  TGocciaFetchMaxResponseBytesProvider = function: Integer of object;

  { One engine's fetch global. Everything that decides what this engine's
    requests may reach — its capability set and response ceiling — lives here,
    per engine, and travels with each request it starts. }
  TGocciaGlobalFetch = class(TGocciaBuiltin)
  private
    FCapabilities: TGocciaCapabilities;
    FCapabilityAuditEmitter: TGocciaCapabilityAuditEmitter;
    FSourcedAuditEmitter: TGocciaCapabilityAuditSourcedEmitter;
    FMaxResponseBytesProvider: TGocciaFetchMaxResponseBytesProvider;
    FRealm: TGocciaRealm;
    FAcquiredFetchManager: Boolean;
    function FetchCallback(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    procedure ValidateHost(const AURLStr: string);
  public
    { ACapabilities is the owning engine's set, fixed for its lifetime; every
      request is checked against its net rules (ADR 0122). ARealm is the
      owning engine's realm: this engine's requests are tagged with it, and it
      is where their results are created. }
    constructor Create(const AName: string; const AScope: TGocciaScope;
      const AThrowError: TGocciaThrowErrorCallback;
      const ACapabilities: TGocciaCapabilities;
      const ACapabilityAuditEmitter: TGocciaCapabilityAuditEmitter;
      const ASourcedAuditEmitter: TGocciaCapabilityAuditSourcedEmitter;
      const AMaxResponseBytesProvider: TGocciaFetchMaxResponseBytesProvider;
      const ARealm: TGocciaRealm);
    destructor Destroy; override;

    property Capabilities: TGocciaCapabilities read FCapabilities;
    property Realm: TGocciaRealm read FRealm;
  end;

implementation

uses
  SysUtils,

  HTTPTypes,
  NetworkAddress,

  Goccia.Constants.ConstructorNames,
  Goccia.Constants.PropertyNames,
  Goccia.EngineFault,
  Goccia.Error.Messages,
  Goccia.Error.Suggestions,
  Goccia.Execution.CallSite,
  Goccia.FetchManager,
  Goccia.InstructionLimit,
  Goccia.MemoryLimit,
  Goccia.Timeout,
  Goccia.Values.AbortValue,
  Goccia.Values.ErrorHelper,
  Goccia.Values.HeadersValue,
  Goccia.Values.NativeFunction,
  Goccia.Values.ObjectValue,
  Goccia.Values.PromiseValue,
  Goccia.Values.URLValue;

const
  INVALID_FETCH_AUDIT_SUBJECT = '<invalid URL>';
  FETCH_BACKEND_UNAVAILABLE_ERROR = 'no fetch backend is available';

{ TGocciaGlobalFetch }

constructor TGocciaGlobalFetch.Create(const AName: string;
  const AScope: TGocciaScope;
  const AThrowError: TGocciaThrowErrorCallback;
  const ACapabilities: TGocciaCapabilities;
  const ACapabilityAuditEmitter: TGocciaCapabilityAuditEmitter;
  const ASourcedAuditEmitter: TGocciaCapabilityAuditSourcedEmitter;
  const AMaxResponseBytesProvider: TGocciaFetchMaxResponseBytesProvider;
  const ARealm: TGocciaRealm);
begin
  inherited Create(AName, AScope, AThrowError);

  FCapabilities := ACapabilities;
  FCapabilityAuditEmitter := ACapabilityAuditEmitter;
  FSourcedAuditEmitter := ASourcedAuditEmitter;
  FMaxResponseBytesProvider := AMaxResponseBytesProvider;
  FRealm := ARealm;
  TGocciaFetchManager.AcquireInstance;
  FAcquiredFetchManager := True;

  // Register fetch as a global function
  AScope.DefineLexicalBinding('fetch',
    TGocciaNativeFunctionValue.Create(FetchCallback, 'fetch', 1), dtConst, True);
end;

destructor TGocciaGlobalFetch.Destroy;
begin
  if FAcquiredFetchManager then
  begin
    // Detach this engine's in-flight requests before its realm goes away;
    // other engines' requests on the thread are left running.
    DiscardFetchCompletions(FRealm);
    TGocciaFetchManager.ReleaseInstance;
  end;
  inherited Destroy;
end;

{ The net scope a guest sees in a denial: the host, plus the port when the URL
  names a non-default one. }
function NetDenialScope(const AParsed: THTTPParsedURL): string;
begin
  if Pos(':', AParsed.Host) > 0 then
    Result := '[' + AParsed.Host + ']'
  else
    Result := AParsed.Host;
  if not (((AParsed.Scheme = 'http') and (AParsed.Port = 80)) or
          ((AParsed.Scheme = 'https') and (AParsed.Port = 443))) then
    Result := Result + ':' + IntToStr(AParsed.Port);
end;

{ The host-side hint for a refused destination: a private address literal is
  refused by the private-range rule rather than by a missing host grant. }
function NetDenialSuggestion(const AHost: string): string;
var
  Address: TNetworkAddress;
begin
  if TryParseIPAddress(AHost, Address) and IsPrivateIPAddress(Address) then
    Result := SSuggestFetchPrivateDestination
  else
    Result := SSuggestFetchAllowedHosts;
end;

procedure TGocciaGlobalFetch.ValidateHost(const AURLStr: string);
var
  Host: string;
  Parsed: THTTPParsedURL;
begin
  try
    Parsed := ParseHTTPURL(AURLStr, False);
    Host := Parsed.Host;
  except
    on E: EHTTPError do
    begin
      try
        Host := HTTPURLAuditHost(AURLStr);
      except
        on EAudit: EHTTPError do
          Host := INVALID_FETCH_AUDIT_SUBJECT;
      end;
      if Assigned(FCapabilityAuditEmitter) then
        FCapabilityAuditEmitter(gckNetFetch, gcdDeny, Host,
          'fetch URL is invalid');
      ThrowTypeError('Invalid fetch URL: ' + E.Message);
    end;
  end;

  { The name is checked here, before any lookup, so a refused request has no
    observable side effect. Where the name resolves to is checked again by the
    request itself, on every redirect hop. }
  if not FCapabilities.AllowsNetHost(Parsed.Host, Parsed.Port) then
  begin
    if Assigned(FCapabilityAuditEmitter) then
      FCapabilityAuditEmitter(gckNetFetch, gcdDeny, Host,
        Format('the net capability does not allow port %d of this host',
          [Parsed.Port]));
    ThrowPermissionDenied(CapabilityName(gcNet), NetDenialScope(Parsed),
      NetDenialSuggestion(Parsed.Host));
  end;

  if Assigned(FCapabilityAuditEmitter) then
    FCapabilityAuditEmitter(gckNetFetch, gcdAllow, Host,
      'the net capability allows this host');
end;

function TGocciaGlobalFetch.FetchCallback(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  URLArg, OptionsArg, MethodVal, HeadersVal, SignalVal: TGocciaValue;
  URLStr, Method: string;
  RequestHeaders: THTTPHeaders;
  Promise: TGocciaPromiseValue;
  Signal: TGocciaAbortSignalValue;
  Obj: TGocciaObjectValue;
  PropNames: TArray<string>;
  I: Integer;
  Policy: TGocciaFetchPolicy;
  Manager: TGocciaFetchManager;
  CallSite: TGocciaCallSite;
begin
  // Extract URL
  if AArgs.Length = 0 then
    ThrowTypeError(SErrorFetchRequiresURL, SSuggestFetchUsage);

  URLArg := AArgs.GetElement(0);
  if URLArg is TGocciaURLValue then
    URLStr := TGocciaURLValue(URLArg).ComputeHref
  else
    URLStr := URLArg.ToStringLiteral.Value;

  // Validate allowed hosts
  ValidateHost(URLStr);

  // Extract options
  Method := 'GET';
  Signal := nil;
  SetLength(RequestHeaders, 0);

  if AArgs.Length >= 2 then
  begin
    OptionsArg := AArgs.GetElement(1);
    if (OptionsArg is TGocciaObjectValue) and
       not (OptionsArg is TGocciaUndefinedLiteralValue) and
       not (OptionsArg is TGocciaNullLiteralValue) then
    begin
      Obj := TGocciaObjectValue(OptionsArg);

      // Read method
      MethodVal := Obj.GetProperty(PROP_METHOD);
      if Assigned(MethodVal) and not (MethodVal is TGocciaUndefinedLiteralValue) then
        Method := UpperCase(MethodVal.ToStringLiteral.Value);

      // Read headers
      HeadersVal := Obj.GetProperty(PROP_HEADERS);
      if Assigned(HeadersVal) and not (HeadersVal is TGocciaUndefinedLiteralValue) then
      begin
        if HeadersVal is TGocciaHeadersValue then
        begin
          SetLength(RequestHeaders, TGocciaHeadersValue(HeadersVal).Entries.Count);
          for I := 0 to TGocciaHeadersValue(HeadersVal).Entries.Count - 1 do
          begin
            RequestHeaders[I].Name := TGocciaHeadersValue(HeadersVal).Entries[I].Name;
            RequestHeaders[I].Value := TGocciaHeadersValue(HeadersVal).Entries[I].Value;
          end;
        end
        else if HeadersVal is TGocciaObjectValue then
        begin
          PropNames := TGocciaObjectValue(HeadersVal).GetAllPropertyNames;
          SetLength(RequestHeaders, Length(PropNames));
          for I := 0 to High(PropNames) do
          begin
            RequestHeaders[I].Name := LowerCase(PropNames[I]);
            RequestHeaders[I].Value :=
              TGocciaObjectValue(HeadersVal).GetProperty(PropNames[I]).ToStringLiteral.Value;
          end;
        end;
      end;

      // Read cancellation signal
      SignalVal := Obj.GetProperty(PROP_SIGNAL);
      if Assigned(SignalVal) and
         not (SignalVal is TGocciaUndefinedLiteralValue) and
         not (SignalVal is TGocciaNullLiteralValue) then
      begin
        if not (SignalVal is TGocciaAbortSignalValue) then
          ThrowTypeError('fetch signal must be an AbortSignal');
        Signal := TGocciaAbortSignalValue(SignalVal);
      end;
    end;
  end;

  // Validate method — only GET and HEAD allowed
  if (Method <> 'GET') and (Method <> 'HEAD') then
    ThrowTypeError(Format(SErrorFetchUnsupportedMethod, [Method]),
      SSuggestFetchUsage);

  // Perform the request
  Promise := TGocciaPromiseValue.Create;
  if Assigned(FCapabilityAuditEmitter) then
    FCapabilityAuditEmitter(gckNetDispatch, gcdAllow, URLStr,
      'fetch dispatch is allowed');
  Policy.Capabilities := FCapabilities;
  Policy.AuditEmitter := FSourcedAuditEmitter;
  { The worker's decisions are delivered later, from whatever code is
    running then; attribute them to this fetch() call. }
  Policy.AuditSource := Default(TGocciaCapabilityAuditSource);
  if CurrentGocciaCallSite(CallSite) then
  begin
    Policy.AuditSource.FilePath := CallSite.FilePath;
    Policy.AuditSource.Line := CallSite.Line;
    Policy.AuditSource.Column := CallSite.Column;
  end;
  if Assigned(FMaxResponseBytesProvider) then
    Policy.MaxResponseBytes := FMaxResponseBytesProvider()
  else
    Policy.MaxResponseBytes := 0;
  try
    Manager := TGocciaFetchManager.Instance;
    if not Assigned(Manager) then
      raise Exception.Create(FETCH_BACKEND_UNAVAILABLE_ERROR);
    Manager.StartFetch(URLStr, Method, RequestHeaders, Policy, FRealm,
      Promise, Signal);
  except
    on E: TGocciaTimeoutError do
      raise;
    on E: TGocciaInstructionLimitError do
      raise;
    on E: TGocciaMemoryLimitError do
      raise;
    on E: Exception do
    begin
      if IsEngineIntegrityFault(E) then
        raise;
      Promise.Reject(CreateErrorObject('TypeError', 'fetch failed: ' + E.Message));
    end;
  end;

  Result := Promise;
end;

end.
