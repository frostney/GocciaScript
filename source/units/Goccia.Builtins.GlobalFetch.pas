unit Goccia.Builtins.GlobalFetch;

// Fetch API — global fetch() function and Headers/Response registration
// https://fetch.spec.whatwg.org/

{$I Goccia.inc}

interface

uses
  Classes,

  Goccia.Arguments.Collection,
  Goccia.Builtins.Base,
  Goccia.CapabilityAudit,
  Goccia.Error.ThrowErrorCallback,
  Goccia.Scope,
  Goccia.Values.Primitives;

type
  TGocciaGlobalFetch = class(TGocciaBuiltin)
  private
    FAllowedHosts: TStringList;
    FCapabilityAuditEmitter: TGocciaCapabilityAuditEmitter;
    function FetchCallback(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    procedure ValidateHost(const AURLStr: string);
  public
    constructor Create(const AName: string; const AScope: TGocciaScope;
      const AThrowError: TGocciaThrowErrorCallback;
      const ACapabilityAuditEmitter: TGocciaCapabilityAuditEmitter);
    destructor Destroy; override;

    procedure SetAllowedHosts(const AHosts: TStrings);

    property AllowedHosts: TStringList read FAllowedHosts;
  end;

implementation

uses
  SysUtils,

  HTTPClient,
  HTTPTypes,

  Goccia.Constants.ConstructorNames,
  Goccia.Constants.PropertyNames,
  Goccia.Error.Messages,
  Goccia.Error.Suggestions,
  Goccia.FetchManager,
  Goccia.InstructionLimit,
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

{ TGocciaGlobalFetch }

constructor TGocciaGlobalFetch.Create(const AName: string;
  const AScope: TGocciaScope;
  const AThrowError: TGocciaThrowErrorCallback;
  const ACapabilityAuditEmitter: TGocciaCapabilityAuditEmitter);
begin
  inherited Create(AName, AScope, AThrowError);

  FCapabilityAuditEmitter := ACapabilityAuditEmitter;
  FAllowedHosts := TStringList.Create;
  FAllowedHosts.CaseSensitive := False;

  // Register fetch as a global function
  AScope.DefineLexicalBinding('fetch',
    TGocciaNativeFunctionValue.Create(FetchCallback, 'fetch', 1), dtConst, True);
end;

destructor TGocciaGlobalFetch.Destroy;
begin
  FAllowedHosts.Free;
  inherited Destroy;
end;

procedure TGocciaGlobalFetch.SetAllowedHosts(const AHosts: TStrings);
var
  I: Integer;
begin
  FAllowedHosts.Clear;
  for I := 0 to AHosts.Count - 1 do
    FAllowedHosts.Add(LowerCase(AHosts[I]));
end;

procedure TGocciaGlobalFetch.ValidateHost(const AURLStr: string);
var
  Host: string;
begin
  try
    Host := HTTPURLHost(AURLStr);
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
        FCapabilityAuditEmitter(gckFetchHost, gcdDeny, Host,
          'fetch URL is invalid');
      ThrowTypeError('Invalid fetch URL: ' + E.Message);
    end;
  end;
  if FAllowedHosts.Count = 0 then
  begin
    if Assigned(FCapabilityAuditEmitter) then
      FCapabilityAuditEmitter(gckFetchHost, gcdDeny, Host,
        'no fetch hosts are allowed');
    ThrowTypeError(SErrorFetchNoAllowedHosts, SSuggestFetchAllowedHosts);
  end;

  if FAllowedHosts.IndexOf(Host) < 0 then
  begin
    if Assigned(FCapabilityAuditEmitter) then
      FCapabilityAuditEmitter(gckFetchHost, gcdDeny, Host,
        'host is not in the allowed hosts list');
    ThrowTypeError(Format(SErrorFetchHostNotAllowed, [Host]),
      SSuggestFetchAllowedHosts);
  end;

  if Assigned(FCapabilityAuditEmitter) then
    FCapabilityAuditEmitter(gckFetchHost, gcdAllow, Host,
      'host is in the allowed hosts list');
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
  if (TGocciaFetchManager.Instance = nil) then
    TGocciaFetchManager.Initialize;
  if Assigned(FCapabilityAuditEmitter) then
    FCapabilityAuditEmitter(gckFetchDispatch, gcdAllow, URLStr,
      'fetch dispatch is allowed');
  try
    TGocciaFetchManager.Instance.StartFetch(URLStr, Method, RequestHeaders,
      FAllowedHosts, Promise, Signal);
  except
    on E: TGocciaTimeoutError do
      raise;
    on E: TGocciaInstructionLimitError do
      raise;
    on E: Exception do
      Promise.Reject(CreateErrorObject('TypeError', 'fetch failed: ' + E.Message));
  end;

  Result := Promise;
end;

end.
