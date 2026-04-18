unit Goccia.Values.ResponseValue;

// Fetch API Response
// https://fetch.spec.whatwg.org/#response-class

{$I Goccia.inc}

interface

uses
  SysUtils,

  Goccia.Arguments.Collection,
  Goccia.ObjectModel,
  Goccia.SharedPrototype,
  Goccia.Values.ClassValue,
  Goccia.Values.HeadersValue,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  TGocciaResponseValue = class(TGocciaInstanceValue)
  private
    FStatus: Integer;
    FStatusText: string;
    FUrl: string;
    FHeaders: TGocciaHeadersValue;
    FBody: TBytes;
    FBodyUsed: Boolean;
    FRedirected: Boolean;

    // Prototype getters
    function ResponseStatusGetter(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function ResponseStatusTextGetter(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function ResponseOkGetter(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function ResponseUrlGetter(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function ResponseHeadersGetter(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function ResponseTypeGetter(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function ResponseRedirectedGetter(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function ResponseBodyUsedGetter(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;

    // Prototype methods
    function ResponseText(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function ResponseJSON(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function ResponseArrayBuffer(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;

    procedure InitializePrototype;
  public
    constructor Create(const AClass: TGocciaClassValue = nil);

    procedure InitFromHTTP(const AStatus: Integer; const AStatusText, AUrl: string;
      const AHeaders: TGocciaHeadersValue; const ABody: TBytes;
      const ARedirected: Boolean);

    function ToStringTag: string; override;
    procedure MarkReferences; override;

    procedure InitializeNativeFromArguments(
      const AArguments: TGocciaArgumentsCollection); override;

    class procedure ExposePrototype(const AConstructor: TGocciaValue);

    property Status: Integer read FStatus;
    property StatusText: string read FStatusText;
  end;

implementation

uses
  Goccia.Constants.ConstructorNames,
  Goccia.Constants.PropertyNames,
  Goccia.Error.Messages,
  Goccia.Error.Suggestions,
  Goccia.JSON,
  Goccia.Values.ArrayBufferValue,
  Goccia.Values.ErrorHelper,
  Goccia.Values.NativeFunction,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.PromiseValue;

threadvar
  FShared: TGocciaSharedPrototype;
  FPrototypeMembers: TArray<TGocciaMemberDefinition>;

{ TGocciaResponseValue }

constructor TGocciaResponseValue.Create(const AClass: TGocciaClassValue);
begin
  inherited Create(AClass);
  FStatus := 200;
  FStatusText := '';
  FUrl := '';
  FHeaders := TGocciaHeadersValue.Create;
  SetLength(FBody, 0);
  FBodyUsed := False;
  FRedirected := False;
  InitializePrototype;
  if not Assigned(AClass) and Assigned(FShared) then
    FPrototype := FShared.Prototype;
end;

procedure TGocciaResponseValue.InitFromHTTP(const AStatus: Integer;
  const AStatusText, AUrl: string; const AHeaders: TGocciaHeadersValue;
  const ABody: TBytes; const ARedirected: Boolean);
begin
  FStatus := AStatus;
  FStatusText := AStatusText;
  FUrl := AUrl;
  FHeaders := AHeaders;
  FBody := ABody;
  FRedirected := ARedirected;
  FBodyUsed := False;
end;

procedure TGocciaResponseValue.InitializePrototype;
var
  Members: TGocciaMemberCollection;
begin
  if Assigned(FShared) then Exit;

  FShared := TGocciaSharedPrototype.Create(Self);
  if Length(FPrototypeMembers) = 0 then
  begin
    Members := TGocciaMemberCollection.Create;
    try
      // Read-only accessors
      Members.AddAccessor(PROP_STATUS,
        ResponseStatusGetter, nil, [pfConfigurable, pfEnumerable]);
      Members.AddAccessor(PROP_STATUS_TEXT,
        ResponseStatusTextGetter, nil, [pfConfigurable, pfEnumerable]);
      Members.AddAccessor(PROP_OK,
        ResponseOkGetter, nil, [pfConfigurable, pfEnumerable]);
      Members.AddAccessor(PROP_URL,
        ResponseUrlGetter, nil, [pfConfigurable, pfEnumerable]);
      Members.AddAccessor(PROP_HEADERS,
        ResponseHeadersGetter, nil, [pfConfigurable, pfEnumerable]);
      Members.AddAccessor(PROP_TYPE,
        ResponseTypeGetter, nil, [pfConfigurable, pfEnumerable]);
      Members.AddAccessor(PROP_REDIRECTED,
        ResponseRedirectedGetter, nil, [pfConfigurable, pfEnumerable]);
      Members.AddAccessor(PROP_BODY_USED,
        ResponseBodyUsedGetter, nil, [pfConfigurable, pfEnumerable]);

      // Body consumption methods
      Members.AddNamedMethod(PROP_TEXT, ResponseText, 0,
        gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddNamedMethod(PROP_JSON, ResponseJSON, 0,
        gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddNamedMethod(PROP_ARRAY_BUFFER_METHOD, ResponseArrayBuffer, 0,
        gmkPrototypeMethod, [gmfNoFunctionPrototype]);

      FPrototypeMembers := Members.ToDefinitions;
    finally
      Members.Free;
    end;
  end;
  RegisterMemberDefinitions(FShared.Prototype, FPrototypeMembers);
end;

class procedure TGocciaResponseValue.ExposePrototype(const AConstructor: TGocciaValue);
begin
  if not Assigned(FShared) then
    TGocciaResponseValue.Create;
  ExposeSharedPrototypeOnConstructor(FShared, AConstructor);
end;

// ---------------------------------------------------------------------------
// Getters
// ---------------------------------------------------------------------------

function TGocciaResponseValue.ResponseStatusGetter(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  if not (AThisValue is TGocciaResponseValue) then
    ThrowTypeError(SErrorResponseNotResponse, SSuggestResponseThisType);
  Result := TGocciaNumberLiteralValue.Create(
    TGocciaResponseValue(AThisValue).FStatus);
end;

function TGocciaResponseValue.ResponseStatusTextGetter(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  if not (AThisValue is TGocciaResponseValue) then
    ThrowTypeError(SErrorResponseNotResponse, SSuggestResponseThisType);
  Result := TGocciaStringLiteralValue.Create(
    TGocciaResponseValue(AThisValue).FStatusText);
end;

function TGocciaResponseValue.ResponseOkGetter(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  S: Integer;
begin
  if not (AThisValue is TGocciaResponseValue) then
    ThrowTypeError(SErrorResponseNotResponse, SSuggestResponseThisType);
  S := TGocciaResponseValue(AThisValue).FStatus;
  if (S >= 200) and (S <= 299) then
    Result := TGocciaBooleanLiteralValue.TrueValue
  else
    Result := TGocciaBooleanLiteralValue.FalseValue;
end;

function TGocciaResponseValue.ResponseUrlGetter(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  if not (AThisValue is TGocciaResponseValue) then
    ThrowTypeError(SErrorResponseNotResponse, SSuggestResponseThisType);
  Result := TGocciaStringLiteralValue.Create(
    TGocciaResponseValue(AThisValue).FUrl);
end;

function TGocciaResponseValue.ResponseHeadersGetter(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  if not (AThisValue is TGocciaResponseValue) then
    ThrowTypeError(SErrorResponseNotResponse, SSuggestResponseThisType);
  Result := TGocciaResponseValue(AThisValue).FHeaders;
  if not Assigned(Result) then
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaResponseValue.ResponseTypeGetter(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  if not (AThisValue is TGocciaResponseValue) then
    ThrowTypeError(SErrorResponseNotResponse, SSuggestResponseThisType);
  Result := TGocciaStringLiteralValue.Create('basic');
end;

function TGocciaResponseValue.ResponseRedirectedGetter(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  if not (AThisValue is TGocciaResponseValue) then
    ThrowTypeError(SErrorResponseNotResponse, SSuggestResponseThisType);
  if TGocciaResponseValue(AThisValue).FRedirected then
    Result := TGocciaBooleanLiteralValue.TrueValue
  else
    Result := TGocciaBooleanLiteralValue.FalseValue;
end;

function TGocciaResponseValue.ResponseBodyUsedGetter(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  if not (AThisValue is TGocciaResponseValue) then
    ThrowTypeError(SErrorResponseNotResponse, SSuggestResponseThisType);
  if TGocciaResponseValue(AThisValue).FBodyUsed then
    Result := TGocciaBooleanLiteralValue.TrueValue
  else
    Result := TGocciaBooleanLiteralValue.FalseValue;
end;

// ---------------------------------------------------------------------------
// Body consumption methods
// ---------------------------------------------------------------------------

function TGocciaResponseValue.ResponseText(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  R: TGocciaResponseValue;
  U8: UTF8String;
  P: TGocciaPromiseValue;
begin
  if not (AThisValue is TGocciaResponseValue) then
    ThrowTypeError(SErrorResponseNotResponse, SSuggestResponseThisType);
  R := TGocciaResponseValue(AThisValue);
  P := TGocciaPromiseValue.Create;
  if R.FBodyUsed then
  begin
    P.Reject(CreateErrorObject('TypeError', SErrorResponseBodyAlreadyUsed));
    Result := P;
    Exit;
  end;
  R.FBodyUsed := True;

  // Decode body as UTF-8
  SetLength(U8, Length(R.FBody));
  if Length(R.FBody) > 0 then
    Move(R.FBody[0], U8[1], Length(R.FBody));

  P.Resolve(TGocciaStringLiteralValue.Create(string(U8)));
  Result := P;
end;

function TGocciaResponseValue.ResponseJSON(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  R: TGocciaResponseValue;
  U8: UTF8String;
  Parser: TGocciaJSONParser;
  Parsed: TGocciaValue;
  P: TGocciaPromiseValue;
begin
  if not (AThisValue is TGocciaResponseValue) then
    ThrowTypeError(SErrorResponseNotResponse, SSuggestResponseThisType);
  R := TGocciaResponseValue(AThisValue);
  P := TGocciaPromiseValue.Create;
  if R.FBodyUsed then
  begin
    P.Reject(CreateErrorObject('TypeError', SErrorResponseBodyAlreadyUsed));
    Result := P;
    Exit;
  end;
  R.FBodyUsed := True;

  SetLength(U8, Length(R.FBody));
  if Length(R.FBody) > 0 then
    Move(R.FBody[0], U8[1], Length(R.FBody));

  Parser := TGocciaJSONParser.Create;
  try
    try
      Parsed := Parser.Parse(U8);
      P.Resolve(Parsed);
    except
      on E: Exception do
        P.Reject(CreateErrorObject('SyntaxError', E.Message));
    end;
  finally
    Parser.Free;
  end;
  Result := P;
end;

function TGocciaResponseValue.ResponseArrayBuffer(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  R: TGocciaResponseValue;
  AB: TGocciaArrayBufferValue;
  P: TGocciaPromiseValue;
begin
  if not (AThisValue is TGocciaResponseValue) then
    ThrowTypeError(SErrorResponseNotResponse, SSuggestResponseThisType);
  R := TGocciaResponseValue(AThisValue);
  P := TGocciaPromiseValue.Create;
  if R.FBodyUsed then
  begin
    P.Reject(CreateErrorObject('TypeError', SErrorResponseBodyAlreadyUsed));
    Result := P;
    Exit;
  end;
  R.FBodyUsed := True;

  AB := TGocciaArrayBufferValue.Create(Length(R.FBody));
  if Length(R.FBody) > 0 then
    Move(R.FBody[0], AB.Data[0], Length(R.FBody));

  P.Resolve(AB);
  Result := P;
end;

// ---------------------------------------------------------------------------
// Constructor / boilerplate
// ---------------------------------------------------------------------------

procedure TGocciaResponseValue.InitializeNativeFromArguments(
  const AArguments: TGocciaArgumentsCollection);
var
  BodyArg, InitArg, PropVal: TGocciaValue;
  InitObj, HeadersObj: TGocciaObjectValue;
  U8: UTF8String;
  PropNames: TArray<string>;
  I, StatusVal: Integer;
begin
  if AArguments.Length = 0 then Exit;

  // Argument 0: body (string or null/undefined)
  BodyArg := AArguments.GetElement(0);
  if not ((BodyArg is TGocciaUndefinedLiteralValue) or
          (BodyArg is TGocciaNullLiteralValue)) then
  begin
    U8 := UTF8String(BodyArg.ToStringLiteral.Value);
    SetLength(FBody, Length(U8));
    if Length(U8) > 0 then
      Move(U8[1], FBody[0], Length(U8));
  end;

  // Argument 1: init dictionary { status, statusText, headers }
  if AArguments.Length < 2 then Exit;
  InitArg := AArguments.GetElement(1);
  if not (InitArg is TGocciaObjectValue) then Exit;
  InitObj := TGocciaObjectValue(InitArg);

  // status — must be 200-599 per WHATWG Fetch spec
  PropVal := InitObj.GetProperty(PROP_STATUS);
  if Assigned(PropVal) and not (PropVal is TGocciaUndefinedLiteralValue) then
  begin
    StatusVal := Trunc(PropVal.ToNumberLiteral.Value);
    if (StatusVal < 200) or (StatusVal > 599) then
      ThrowRangeError('Response status must be in the range 200-599');
    FStatus := StatusVal;
  end;

  // statusText
  PropVal := InitObj.GetProperty(PROP_STATUS_TEXT);
  if Assigned(PropVal) and not (PropVal is TGocciaUndefinedLiteralValue) then
    FStatusText := PropVal.ToStringLiteral.Value;

  // headers
  PropVal := InitObj.GetProperty(PROP_HEADERS);
  if Assigned(PropVal) and not (PropVal is TGocciaUndefinedLiteralValue) then
  begin
    if PropVal is TGocciaHeadersValue then
      FHeaders := TGocciaHeadersValue(PropVal)
    else if PropVal is TGocciaObjectValue then
    begin
      HeadersObj := TGocciaObjectValue(PropVal);
      PropNames := HeadersObj.GetAllPropertyNames;
      for I := 0 to High(PropNames) do
        FHeaders.AddHeader(PropNames[I],
          HeadersObj.GetProperty(PropNames[I]).ToStringLiteral.Value);
    end;
  end;
end;

function TGocciaResponseValue.ToStringTag: string;
begin
  Result := CONSTRUCTOR_RESPONSE;
end;

procedure TGocciaResponseValue.MarkReferences;
begin
  if GCMarked then Exit;
  inherited;
  if Assigned(FHeaders) then
    FHeaders.MarkReferences;
end;

end.
