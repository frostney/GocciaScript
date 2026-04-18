unit Goccia.Builtins.GlobalFetch;

// Fetch API — global fetch() function and Headers/Response registration
// https://fetch.spec.whatwg.org/

{$I Goccia.inc}

interface

uses
  Goccia.Arguments.Collection,
  Goccia.Builtins.Base,
  Goccia.Error.ThrowErrorCallback,
  Goccia.Scope,
  Goccia.Values.Primitives;

type
  TGocciaGlobalFetch = class(TGocciaBuiltin)
  private
    function FetchCallback(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
  public
    constructor Create(const AName: string; const AScope: TGocciaScope;
      const AThrowError: TGocciaThrowErrorCallback);
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

{ TGocciaGlobalFetch }

constructor TGocciaGlobalFetch.Create(const AName: string;
  const AScope: TGocciaScope;
  const AThrowError: TGocciaThrowErrorCallback);
begin
  inherited Create(AName, AScope, AThrowError);

  // Register fetch as a global function
  AScope.DefineLexicalBinding('fetch',
    TGocciaNativeFunctionValue.Create(FetchCallback, 'fetch', 1), dtConst);
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
