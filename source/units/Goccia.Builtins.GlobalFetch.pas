unit Goccia.Builtins.GlobalFetch;

// Fetch API — global fetch() function and Headers/Response registration
// https://fetch.spec.whatwg.org/

{$I Goccia.inc}

interface

uses
  Classes,

  Goccia.Arguments.Collection,
  Goccia.Builtins.Base,
  Goccia.Error.ThrowErrorCallback,
  Goccia.Scope,
  Goccia.Values.Primitives;

type
  TGocciaGlobalFetch = class(TGocciaBuiltin)
  private
    FAllowedHosts: TStringList;
    function FetchCallback(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    procedure ValidateHost(const AURLStr: string);
  public
    constructor Create(const AName: string; const AScope: TGocciaScope;
      const AThrowError: TGocciaThrowErrorCallback);
    destructor Destroy; override;

    procedure SetAllowedHosts(const AHosts: TStrings);

    property AllowedHosts: TStringList read FAllowedHosts;
  end;

implementation

uses
  SysUtils,

  HTTPClient,

  Goccia.Constants.ConstructorNames,
  Goccia.Constants.PropertyNames,
  Goccia.Error.Messages,
  Goccia.Error.Suggestions,
  Goccia.Values.ErrorHelper,
  Goccia.Values.HeadersValue,
  Goccia.Values.NativeFunction,
  Goccia.Values.ObjectValue,
  Goccia.Values.PromiseValue,
  Goccia.Values.ResponseValue,
  Goccia.Values.URLValue;

{ Host extraction }

function ExtractHostFromURL(const AURL: string): string;
var
  SchemeEnd, HostStart, HostEnd, AtPos, ColonPos: Integer;
begin
  Result := '';
  SchemeEnd := Pos('://', AURL);
  if SchemeEnd = 0 then
    Exit;

  HostStart := SchemeEnd + 3;

  // Find the end of the authority component
  AtPos := HostStart;
  HostEnd := HostStart;
  while (HostEnd <= Length(AURL)) and
        not (AURL[HostEnd] in ['/', '?', '#']) do
  begin
    if AURL[HostEnd] = '@' then
      AtPos := HostEnd + 1;
    Inc(HostEnd);
  end;
  HostStart := AtPos;

  Result := LowerCase(Copy(AURL, HostStart, HostEnd - HostStart));

  // Strip port (but preserve IPv6 bracket notation)
  if (Length(Result) > 0) and (Result[1] <> '[') then
  begin
    ColonPos := Pos(':', Result);
    if ColonPos > 0 then
      Result := Copy(Result, 1, ColonPos - 1);
  end
  else if (Length(Result) > 0) and (Result[1] = '[') then
  begin
    ColonPos := Pos(']:', Result);
    if ColonPos > 0 then
      Result := Copy(Result, 1, ColonPos);
  end;
end;

{ TGocciaGlobalFetch }

constructor TGocciaGlobalFetch.Create(const AName: string;
  const AScope: TGocciaScope;
  const AThrowError: TGocciaThrowErrorCallback);
begin
  inherited Create(AName, AScope, AThrowError);

  FAllowedHosts := TStringList.Create;
  FAllowedHosts.CaseSensitive := False;

  // Register fetch as a global function
  AScope.DefineLexicalBinding('fetch',
    TGocciaNativeFunctionValue.Create(FetchCallback, 'fetch', 1), dtConst);
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
  if FAllowedHosts.Count = 0 then
    ThrowTypeError(SErrorFetchNoAllowedHosts, SSuggestFetchAllowedHosts);

  Host := ExtractHostFromURL(AURLStr);
  if FAllowedHosts.IndexOf(Host) < 0 then
    ThrowTypeError(Format(SErrorFetchHostNotAllowed, [Host]),
      SSuggestFetchAllowedHosts);
end;

function TGocciaGlobalFetch.FetchCallback(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  URLArg, OptionsArg, MethodVal, HeadersVal: TGocciaValue;
  URLStr, Method: string;
  RequestHeaders: THTTPHeaders;
  HTTPResp: THTTPResponse;
  RespHeaders: TGocciaHeadersValue;
  RespValue: TGocciaResponseValue;
  Promise: TGocciaPromiseValue;
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
    end;
  end;

  // Validate method — only GET and HEAD allowed
  if (Method <> 'GET') and (Method <> 'HEAD') then
    ThrowTypeError(Format(SErrorFetchUnsupportedMethod, [Method]),
      SSuggestFetchUsage);

  // Perform the request
  Promise := TGocciaPromiseValue.Create;
  try
    if Method = 'HEAD' then
      HTTPResp := HTTPHead(URLStr, RequestHeaders)
    else
      HTTPResp := HTTPGet(URLStr, RequestHeaders);

    // Build Headers object from response
    RespHeaders := TGocciaHeadersValue.Create;
    RespHeaders.Immutable := True;
    for I := 0 to High(HTTPResp.Headers) do
      RespHeaders.AddHeader(HTTPResp.Headers[I].Name, HTTPResp.Headers[I].Value);

    // Build Response object
    RespValue := TGocciaResponseValue.Create;
    RespValue.InitFromHTTP(
      HTTPResp.StatusCode,
      HTTPResp.StatusText,
      HTTPResp.FinalURL,
      RespHeaders,
      HTTPResp.Body,
      HTTPResp.Redirected);

    Promise.Resolve(RespValue);
  except
    on E: EHTTPError do
      Promise.Reject(CreateErrorObject('TypeError', E.Message));
    on E: Exception do
      Promise.Reject(CreateErrorObject('TypeError', 'fetch failed: ' + E.Message));
  end;

  Result := Promise;
end;

end.
