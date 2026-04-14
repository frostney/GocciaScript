unit Goccia.Values.URLValue;

// WHATWG URL Standard §4 URL class
// https://url.spec.whatwg.org/#url-class

{$I Goccia.inc}

interface

uses
  Goccia.Arguments.Collection,
  Goccia.ObjectModel,
  Goccia.SharedPrototype,
  Goccia.Values.ClassValue,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives,
  Goccia.Values.URLSearchParamsValue;

type
  TGocciaURLValue = class(TGocciaInstanceValue)
  private
    // WHATWG URL §4.1 URL record components (stored parsed)
    FScheme: string;       // e.g. 'https' — no ':'
    FUsername: string;     // percent-encoded
    FPassword: string;     // percent-encoded
    FHost: string;         // serialized hostname
    FHasNullHost: Boolean; // true = no authority component (null host)
    FPort: Integer;        // -1 = null port
    FPathname: string;     // serialized path including leading '/'
    FHasNullQuery: Boolean;
    FQuery: string;        // without leading '?'
    FHasNullFragment: Boolean;
    FFragment: string;     // without leading '#'
    FHasOpaquePath: Boolean;

    // Cached URLSearchParams — lazily created
    FSearchParams: TGocciaURLSearchParamsValue;
    // True = URL is invalid (parse failure)
    FIsValid: Boolean;

    procedure InitializePrototype;
    procedure SyncSearchParamsFromQuery;
  private
    // Getters
    function URLHrefGetter(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function URLOriginGetter(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function URLProtocolGetter(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function URLUsernameGetter(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function URLPasswordGetter(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function URLHostGetter(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function URLHostnameGetter(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function URLPortGetter(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function URLPathnameGetter(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function URLSearchGetter(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function URLSearchParamsGetter(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function URLHashGetter(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    // Setters
    function URLHrefSetter(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function URLProtocolSetter(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function URLUsernameSetter(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function URLPasswordSetter(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function URLHostSetter(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function URLHostnameSetter(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function URLPortSetter(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function URLPathnameSetter(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function URLSearchSetter(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function URLHashSetter(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    // Methods
    function URLToString(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function URLToJSON(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
  public
    constructor Create(const AClass: TGocciaClassValue = nil);
    destructor Destroy; override;

    // Initialize from a parsed URL record
    procedure InitFromRecord(const AScheme, AUsername, APassword, AHost: string;
      const AHasNullHost: Boolean; const APort: Integer;
      const APathname: string; const AHasNullQuery: Boolean;
      const AQuery: string; const AHasNullFragment: Boolean;
      const AFragment: string; const AHasOpaquePath: Boolean);

    // Called by URLSearchParams when params change
    procedure SetSearchFromParams(const AParams: TGocciaURLSearchParamsValue);

    // Compute the href string
    function ComputeHref: string;

    function GetProperty(const AName: string): TGocciaValue; override;
    function GetPropertyWithContext(const AName: string;
      const AThisContext: TGocciaValue): TGocciaValue; override;
    function ToStringLiteral: TGocciaStringLiteralValue; override;
    function ToStringTag: string; override;

    procedure InitializeNativeFromArguments(
      const AArguments: TGocciaArgumentsCollection); override;
    procedure MarkReferences; override;

    class procedure ExposePrototype(const AConstructor: TGocciaValue);

    property IsValid: Boolean read FIsValid;
  end;

implementation

uses
  SysUtils,

  Goccia.Constants.ConstructorNames,
  Goccia.Constants.PropertyNames,
  Goccia.GarbageCollector,
  Goccia.URL.Parser,
  Goccia.Values.ArrayValue,
  Goccia.Values.ErrorHelper,
  Goccia.Values.NativeFunction,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.SymbolValue;

threadvar
  FShared: TGocciaSharedPrototype;
  FPrototypeMembers: TArray<TGocciaMemberDefinition>;

{ TGocciaURLValue }

constructor TGocciaURLValue.Create(const AClass: TGocciaClassValue);
begin
  inherited Create(AClass);
  FPort := URL_NULL_PORT;
  FHasNullHost := True;
  FHasNullQuery := True;
  FHasNullFragment := True;
  FIsValid := False;
  FSearchParams := nil;
  InitializePrototype;
  if not Assigned(AClass) and Assigned(FShared) then
    FPrototype := FShared.Prototype;
end;

destructor TGocciaURLValue.Destroy;
begin
  inherited;
end;

procedure TGocciaURLValue.InitializePrototype;
var
  Members: TGocciaMemberCollection;
begin
  if Assigned(FShared) then Exit;

  FShared := TGocciaSharedPrototype.Create(Self);
  if Length(FPrototypeMembers) = 0 then
  begin
    Members := TGocciaMemberCollection.Create;
    try
      // Accessor properties (getter + setter pairs)
      Members.AddAccessor(PROP_HREF,
        URLHrefGetter, URLHrefSetter,
        [pfConfigurable, pfEnumerable]);
      Members.AddAccessor(PROP_ORIGIN,
        URLOriginGetter, nil,
        [pfConfigurable, pfEnumerable]);
      Members.AddAccessor(PROP_PROTOCOL,
        URLProtocolGetter, URLProtocolSetter,
        [pfConfigurable, pfEnumerable]);
      Members.AddAccessor(PROP_USERNAME,
        URLUsernameGetter, URLUsernameSetter,
        [pfConfigurable, pfEnumerable]);
      Members.AddAccessor(PROP_PASSWORD,
        URLPasswordGetter, URLPasswordSetter,
        [pfConfigurable, pfEnumerable]);
      Members.AddAccessor(PROP_HOST,
        URLHostGetter, URLHostSetter,
        [pfConfigurable, pfEnumerable]);
      Members.AddAccessor(PROP_HOSTNAME,
        URLHostnameGetter, URLHostnameSetter,
        [pfConfigurable, pfEnumerable]);
      Members.AddAccessor(PROP_PORT,
        URLPortGetter, URLPortSetter,
        [pfConfigurable, pfEnumerable]);
      Members.AddAccessor(PROP_PATHNAME,
        URLPathnameGetter, URLPathnameSetter,
        [pfConfigurable, pfEnumerable]);
      Members.AddAccessor(PROP_SEARCH,
        URLSearchGetter, URLSearchSetter,
        [pfConfigurable, pfEnumerable]);
      Members.AddAccessor(PROP_SEARCH_PARAMS,
        URLSearchParamsGetter, nil,
        [pfConfigurable, pfEnumerable]);
      Members.AddAccessor(PROP_HASH,
        URLHashGetter, URLHashSetter,
        [pfConfigurable, pfEnumerable]);
      // Methods
      Members.AddNamedMethod('toString', URLToString, 0,
        gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddNamedMethod('toJSON', URLToJSON, 0,
        gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      FPrototypeMembers := Members.ToDefinitions;
    finally
      Members.Free;
    end;
  end;
  RegisterMemberDefinitions(FShared.Prototype, FPrototypeMembers);
end;

class procedure TGocciaURLValue.ExposePrototype(
  const AConstructor: TGocciaValue);
begin
  if not Assigned(FShared) then
    TGocciaURLValue.Create;
  ExposeSharedPrototypeOnConstructor(FShared, AConstructor);
end;

// ---------------------------------------------------------------------------
// Initialization helpers
// ---------------------------------------------------------------------------

procedure TGocciaURLValue.InitFromRecord(const AScheme, AUsername, APassword,
  AHost: string; const AHasNullHost: Boolean; const APort: Integer;
  const APathname: string; const AHasNullQuery: Boolean;
  const AQuery: string; const AHasNullFragment: Boolean;
  const AFragment: string; const AHasOpaquePath: Boolean);
begin
  FScheme := AScheme;
  FUsername := AUsername;
  FPassword := APassword;
  FHost := AHost;
  FHasNullHost := AHasNullHost;
  FPort := APort;
  FPathname := APathname;
  FHasNullQuery := AHasNullQuery;
  FQuery := AQuery;
  FHasNullFragment := AHasNullFragment;
  FFragment := AFragment;
  FHasOpaquePath := AHasOpaquePath;
  FIsValid := True;

  // Reset cached search params
  FSearchParams := nil;
end;

// Serialize the URL to its href string directly from stored fields.
// This avoids needing to split FPathname back into segments.
function TGocciaURLValue.ComputeHref: string;
var
  HasCredentials: Boolean;
begin
  // scheme:
  Result := FScheme + ':';

  // //authority
  if not FHasNullHost then
  begin
    Result := Result + '//';
    HasCredentials := (FUsername <> '') or (FPassword <> '');
    if HasCredentials then
    begin
      Result := Result + FUsername;
      if FPassword <> '' then
        Result := Result + ':' + FPassword;
      Result := Result + '@';
    end;
    Result := Result + FHost;
    if FPort <> URL_NULL_PORT then
      Result := Result + ':' + IntToStr(FPort);
  end
  else if FScheme = 'file' then
    Result := Result + '//';

  // path
  Result := Result + FPathname;

  // ?query
  if not FHasNullQuery then
    Result := Result + '?' + FQuery;

  // #fragment
  if not FHasNullFragment then
    Result := Result + '#' + FFragment;
end;

procedure TGocciaURLValue.SyncSearchParamsFromQuery;
begin
  if Assigned(FSearchParams) then
  begin
    // Temporarily remove URL ref to avoid re-entrancy
    FSearchParams.URLRef := nil;
    if FHasNullQuery then
      FSearchParams.ParseFromString('')
    else
      FSearchParams.ParseFromString(FQuery);
    FSearchParams.URLRef := Self;
  end;
end;

// Called by URLSearchParams when its contents change
procedure TGocciaURLValue.SetSearchFromParams(
  const AParams: TGocciaURLSearchParamsValue);
var
  S: string;
begin
  S := AParams.Serialize;
  if S = '' then
  begin
    FHasNullQuery := True;
    FQuery := '';
  end
  else
  begin
    FHasNullQuery := False;
    FQuery := S;
  end;
end;

// ---------------------------------------------------------------------------
// Getters
// ---------------------------------------------------------------------------

function TGocciaURLValue.URLHrefGetter(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  if not (AThisValue is TGocciaURLValue) then
    ThrowTypeError('URL href getter: not a URL');
  Result := TGocciaStringLiteralValue.Create(
    TGocciaURLValue(AThisValue).ComputeHref);
end;

function TGocciaURLValue.URLOriginGetter(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  U: TGocciaURLValue;
  URLRec: TGocciaURLRecord;
begin
  if not (AThisValue is TGocciaURLValue) then
    ThrowTypeError('URL origin getter: not a URL');
  U := TGocciaURLValue(AThisValue);
  FillChar(URLRec, SizeOf(URLRec), 0);
  URLRec.Scheme := U.FScheme;
  URLRec.Host := U.FHost;
  URLRec.HasNullHost := U.FHasNullHost;
  URLRec.Port := U.FPort;
  Result := TGocciaStringLiteralValue.Create(SerializeOrigin(URLRec));
end;

function TGocciaURLValue.URLProtocolGetter(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  if not (AThisValue is TGocciaURLValue) then
    ThrowTypeError('URL protocol getter: not a URL');
  Result := TGocciaStringLiteralValue.Create(
    TGocciaURLValue(AThisValue).FScheme + ':');
end;

function TGocciaURLValue.URLUsernameGetter(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  if not (AThisValue is TGocciaURLValue) then
    ThrowTypeError('URL username getter: not a URL');
  Result := TGocciaStringLiteralValue.Create(
    TGocciaURLValue(AThisValue).FUsername);
end;

function TGocciaURLValue.URLPasswordGetter(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  if not (AThisValue is TGocciaURLValue) then
    ThrowTypeError('URL password getter: not a URL');
  Result := TGocciaStringLiteralValue.Create(
    TGocciaURLValue(AThisValue).FPassword);
end;

function TGocciaURLValue.URLHostGetter(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  U: TGocciaURLValue;
  H: string;
begin
  if not (AThisValue is TGocciaURLValue) then
    ThrowTypeError('URL host getter: not a URL');
  U := TGocciaURLValue(AThisValue);
  H := U.FHost;
  if U.FPort <> URL_NULL_PORT then
    H := H + ':' + IntToStr(U.FPort);
  Result := TGocciaStringLiteralValue.Create(H);
end;

function TGocciaURLValue.URLHostnameGetter(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  if not (AThisValue is TGocciaURLValue) then
    ThrowTypeError('URL hostname getter: not a URL');
  Result := TGocciaStringLiteralValue.Create(
    TGocciaURLValue(AThisValue).FHost);
end;

function TGocciaURLValue.URLPortGetter(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  U: TGocciaURLValue;
begin
  if not (AThisValue is TGocciaURLValue) then
    ThrowTypeError('URL port getter: not a URL');
  U := TGocciaURLValue(AThisValue);
  if U.FPort = URL_NULL_PORT then
    Result := TGocciaStringLiteralValue.Create('')
  else
    Result := TGocciaStringLiteralValue.Create(IntToStr(U.FPort));
end;

function TGocciaURLValue.URLPathnameGetter(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  if not (AThisValue is TGocciaURLValue) then
    ThrowTypeError('URL pathname getter: not a URL');
  Result := TGocciaStringLiteralValue.Create(
    TGocciaURLValue(AThisValue).FPathname);
end;

function TGocciaURLValue.URLSearchGetter(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  U: TGocciaURLValue;
begin
  if not (AThisValue is TGocciaURLValue) then
    ThrowTypeError('URL search getter: not a URL');
  U := TGocciaURLValue(AThisValue);
  if U.FHasNullQuery or (U.FQuery = '') then
    Result := TGocciaStringLiteralValue.Create('')
  else
    Result := TGocciaStringLiteralValue.Create('?' + U.FQuery);
end;

function TGocciaURLValue.URLSearchParamsGetter(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  U: TGocciaURLValue;
begin
  if not (AThisValue is TGocciaURLValue) then
    ThrowTypeError('URL searchParams getter: not a URL');
  U := TGocciaURLValue(AThisValue);

  // Lazy initialization of the search params object
  if not Assigned(U.FSearchParams) then
  begin
    U.FSearchParams := TGocciaURLSearchParamsValue.Create;
    U.FSearchParams.URLRef := U;
    if not U.FHasNullQuery then
      U.FSearchParams.ParseFromString(U.FQuery);
  end;

  Result := U.FSearchParams;
end;

function TGocciaURLValue.URLHashGetter(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  U: TGocciaURLValue;
begin
  if not (AThisValue is TGocciaURLValue) then
    ThrowTypeError('URL hash getter: not a URL');
  U := TGocciaURLValue(AThisValue);
  if U.FHasNullFragment or (U.FFragment = '') then
    Result := TGocciaStringLiteralValue.Create('')
  else
    Result := TGocciaStringLiteralValue.Create('#' + U.FFragment);
end;

// ---------------------------------------------------------------------------
// Setters
// ---------------------------------------------------------------------------

// WHATWG URL §7.1 href setter: full re-parse
function TGocciaURLValue.URLHrefSetter(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  U: TGocciaURLValue;
  NewHref: string;
  Parsed: TGocciaURLRecord;
begin
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  if not (AThisValue is TGocciaURLValue) then
    ThrowTypeError('URL href setter: not a URL');
  U := TGocciaURLValue(AThisValue);
  if AArgs.Length = 0 then Exit;
  NewHref := AArgs.GetElement(0).ToStringLiteral.Value;
  Parsed := ParseURL(NewHref);
  if not Parsed.IsValid then
    ThrowTypeError('Invalid URL: ' + NewHref);
  U.FScheme := Parsed.Scheme;
  U.FUsername := Parsed.Username;
  U.FPassword := Parsed.Password;
  U.FHost := Parsed.Host;
  U.FHasNullHost := Parsed.HasNullHost;
  U.FPort := Parsed.Port;
  U.FPathname := SerializePath(Parsed);
  U.FHasNullQuery := Parsed.HasNullQuery;
  U.FQuery := Parsed.Query;
  U.FHasNullFragment := Parsed.HasNullFragment;
  U.FFragment := Parsed.Fragment;
  U.FHasOpaquePath := Parsed.HasOpaquePath;
  U.SyncSearchParamsFromQuery;
end;

// WHATWG URL §7.1 protocol setter
function TGocciaURLValue.URLProtocolSetter(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  U: TGocciaURLValue;
  Input, S: string;
  I: Integer;
begin
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  if not (AThisValue is TGocciaURLValue) then
    ThrowTypeError('URL protocol setter: not a URL');
  U := TGocciaURLValue(AThisValue);
  if AArgs.Length = 0 then Exit;
  Input := AArgs.GetElement(0).ToStringLiteral.Value;
  // Strip trailing ':' and anything after
  S := '';
  for I := 1 to Length(Input) do
  begin
    if Input[I] = ':' then Break;
    S := S + Input[I];
  end;
  // lowercase
  S := LowerCase(S);
  // Validate: must be valid scheme chars
  if S = '' then Exit;
  if not (S[1] in ['a'..'z']) then Exit;
  for I := 1 to Length(S) do
    if not (S[I] in ['a'..'z', '0'..'9', '+', '-', '.']) then Exit;
  // Cannot change a special scheme to non-special or vice-versa
  if IsSpecialScheme(U.FScheme) <> IsSpecialScheme(S) then Exit;
  // Cannot change to/from 'file'
  if (U.FScheme = 'file') <> (S = 'file') then Exit;
  U.FScheme := S;
  // Clear port if it matches new scheme's default
  if U.FPort = DefaultPortForScheme(U.FScheme) then
    U.FPort := URL_NULL_PORT;
end;

// WHATWG URL §7.1 username setter
function TGocciaURLValue.URLUsernameSetter(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  U: TGocciaURLValue;
begin
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  if not (AThisValue is TGocciaURLValue) then
    ThrowTypeError('URL username setter: not a URL');
  U := TGocciaURLValue(AThisValue);
  // Cannot set credentials on a URL with no host or file: scheme
  if U.FHasNullHost or (U.FScheme = 'file') then Exit;
  if AArgs.Length = 0 then Exit;
  U.FUsername := PercentEncodeForUserinfo(AArgs.GetElement(0).ToStringLiteral.Value);
end;

// WHATWG URL §7.1 password setter
function TGocciaURLValue.URLPasswordSetter(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  U: TGocciaURLValue;
begin
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  if not (AThisValue is TGocciaURLValue) then
    ThrowTypeError('URL password setter: not a URL');
  U := TGocciaURLValue(AThisValue);
  if U.FHasNullHost or (U.FScheme = 'file') then Exit;
  if AArgs.Length = 0 then Exit;
  U.FPassword := PercentEncodeForUserinfo(AArgs.GetElement(0).ToStringLiteral.Value);
end;

// WHATWG URL §7.1 host setter
function TGocciaURLValue.URLHostSetter(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  U: TGocciaURLValue;
  Input: string;
  Parsed: TGocciaURLRecord;
  Temp: string;
begin
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  if not (AThisValue is TGocciaURLValue) then
    ThrowTypeError('URL host setter: not a URL');
  U := TGocciaURLValue(AThisValue);
  if U.FHasOpaquePath then Exit;
  if AArgs.Length = 0 then Exit;
  Input := AArgs.GetElement(0).ToStringLiteral.Value;
  // Re-parse using a temporary URL prefix to get host + port
  Temp := U.FScheme + '://' + Input + '/';
  Parsed := ParseURL(Temp);
  if Parsed.IsValid and not Parsed.HasNullHost then
  begin
    U.FHost := Parsed.Host;
    U.FHasNullHost := False;
    U.FPort := Parsed.Port;
  end;
end;

// WHATWG URL §7.1 hostname setter
function TGocciaURLValue.URLHostnameSetter(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  U: TGocciaURLValue;
  Input: string;
  Parsed: TGocciaURLRecord;
  Temp: string;
begin
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  if not (AThisValue is TGocciaURLValue) then
    ThrowTypeError('URL hostname setter: not a URL');
  U := TGocciaURLValue(AThisValue);
  if U.FHasOpaquePath then Exit;
  if AArgs.Length = 0 then Exit;
  Input := AArgs.GetElement(0).ToStringLiteral.Value;
  // Parse just the hostname (no port)
  Temp := U.FScheme + '://' + Input + '/';
  Parsed := ParseURL(Temp);
  if Parsed.IsValid and not Parsed.HasNullHost then
  begin
    U.FHost := Parsed.Host;
    U.FHasNullHost := False;
    // Do not update port for hostname setter
  end;
end;

// WHATWG URL §7.1 port setter
function TGocciaURLValue.URLPortSetter(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  U: TGocciaURLValue;
  Input: string;
  PortNum: Integer;
begin
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  if not (AThisValue is TGocciaURLValue) then
    ThrowTypeError('URL port setter: not a URL');
  U := TGocciaURLValue(AThisValue);
  if U.FHasNullHost or (U.FScheme = 'file') then Exit;
  if AArgs.Length = 0 then Exit;
  Input := Trim(AArgs.GetElement(0).ToStringLiteral.Value);
  if Input = '' then
  begin
    U.FPort := URL_NULL_PORT;
    Exit;
  end;
  PortNum := StrToIntDef(Input, -2);
  if (PortNum < 0) or (PortNum > 65535) then Exit;
  if PortNum = DefaultPortForScheme(U.FScheme) then
    U.FPort := URL_NULL_PORT
  else
    U.FPort := PortNum;
end;

// WHATWG URL §7.1 pathname setter
function TGocciaURLValue.URLPathnameSetter(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  U: TGocciaURLValue;
  Input: string;
  Parsed: TGocciaURLRecord;
  Temp: string;
begin
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  if not (AThisValue is TGocciaURLValue) then
    ThrowTypeError('URL pathname setter: not a URL');
  U := TGocciaURLValue(AThisValue);
  if U.FHasOpaquePath then Exit;
  if AArgs.Length = 0 then Exit;
  Input := AArgs.GetElement(0).ToStringLiteral.Value;
  // WHATWG URL §7.1: pathname must begin with '/' for URLs with authority
  if not U.FHasOpaquePath and not U.FHasNullHost and
     (Length(Input) > 0) and (Input[1] <> '/') then
    Input := '/' + Input
  else if U.FHasNullHost and (Length(Input) > 0) and (Input[1] <> '/') then
    Input := '/' + Input;
  // Re-parse path via a temporary URL to apply normalization
  if U.FHasNullHost then
    Temp := U.FScheme + ':' + Input
  else
    Temp := U.FScheme + '://' + U.FHost + Input;
  Parsed := ParseURL(Temp);
  if Parsed.IsValid then
    U.FPathname := SerializePath(Parsed);
end;

// WHATWG URL §7.1 search setter
function TGocciaURLValue.URLSearchSetter(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  U: TGocciaURLValue;
  Input: string;
begin
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  if not (AThisValue is TGocciaURLValue) then
    ThrowTypeError('URL search setter: not a URL');
  U := TGocciaURLValue(AThisValue);
  if AArgs.Length = 0 then Exit;
  Input := AArgs.GetElement(0).ToStringLiteral.Value;
  if Input = '' then
  begin
    U.FHasNullQuery := True;
    U.FQuery := '';
  end
  else
  begin
    // Strip leading '?'
    if (Length(Input) > 0) and (Input[1] = '?') then
      Delete(Input, 1, 1);
    U.FHasNullQuery := False;
    U.FQuery := PercentEncodeForQuery(Input, IsSpecialScheme(U.FScheme));
  end;
  U.SyncSearchParamsFromQuery;
end;

// WHATWG URL §7.1 hash setter
function TGocciaURLValue.URLHashSetter(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  U: TGocciaURLValue;
  Input: string;
begin
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  if not (AThisValue is TGocciaURLValue) then
    ThrowTypeError('URL hash setter: not a URL');
  U := TGocciaURLValue(AThisValue);
  if AArgs.Length = 0 then Exit;
  Input := AArgs.GetElement(0).ToStringLiteral.Value;
  if Input = '' then
  begin
    U.FHasNullFragment := True;
    U.FFragment := '';
  end
  else
  begin
    if (Length(Input) > 0) and (Input[1] = '#') then
      Delete(Input, 1, 1);
    U.FHasNullFragment := False;
    U.FFragment := PercentEncodeForFragment(Input);
  end;
end;

// ---------------------------------------------------------------------------
// Methods
// ---------------------------------------------------------------------------

// WHATWG URL §7.1 toString() → same as href
function TGocciaURLValue.URLToString(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  if not (AThisValue is TGocciaURLValue) then
    ThrowTypeError('URL.prototype.toString: not a URL');
  Result := TGocciaStringLiteralValue.Create(
    TGocciaURLValue(AThisValue).ComputeHref);
end;

// WHATWG URL §7.1 toJSON() → same as href
function TGocciaURLValue.URLToJSON(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  if not (AThisValue is TGocciaURLValue) then
    ThrowTypeError('URL.prototype.toJSON: not a URL');
  Result := TGocciaStringLiteralValue.Create(
    TGocciaURLValue(AThisValue).ComputeHref);
end;

// ---------------------------------------------------------------------------
// Property access, GC, and constructor
// ---------------------------------------------------------------------------

function TGocciaURLValue.GetProperty(const AName: string): TGocciaValue;
begin
  Result := GetPropertyWithContext(AName, Self);
end;

function TGocciaURLValue.GetPropertyWithContext(const AName: string;
  const AThisContext: TGocciaValue): TGocciaValue;
begin
  Result := inherited GetPropertyWithContext(AName, AThisContext);
end;

function TGocciaURLValue.ToStringLiteral: TGocciaStringLiteralValue;
begin
  Result := TGocciaStringLiteralValue.Create(ComputeHref);
end;

function TGocciaURLValue.ToStringTag: string;
begin
  Result := CONSTRUCTOR_URL;
end;

procedure TGocciaURLValue.InitializeNativeFromArguments(
  const AArguments: TGocciaArgumentsCollection);
var
  URLStr, BaseStr: string;
  Parsed, BaseParsed: TGocciaURLRecord;
begin
  if AArguments.Length = 0 then
    ThrowTypeError('URL constructor: 1 argument required');

  URLStr := AArguments.GetElement(0).ToStringLiteral.Value;

  if AArguments.Length >= 2 then
  begin
    // Parse with base
    BaseStr := AArguments.GetElement(1).ToStringLiteral.Value;
    BaseParsed := ParseURL(BaseStr);
    if not BaseParsed.IsValid then
      ThrowTypeError('Invalid base URL: ' + BaseStr);
    Parsed := ParseURLWithBase(URLStr, BaseParsed);
  end
  else
    Parsed := ParseURL(URLStr);

  if not Parsed.IsValid then
    ThrowTypeError('Invalid URL: ' + URLStr);

  FScheme := Parsed.Scheme;
  FUsername := Parsed.Username;
  FPassword := Parsed.Password;
  FHost := Parsed.Host;
  FHasNullHost := Parsed.HasNullHost;
  FPort := Parsed.Port;
  FPathname := SerializePath(Parsed);
  FHasNullQuery := Parsed.HasNullQuery;
  FQuery := Parsed.Query;
  FHasNullFragment := Parsed.HasNullFragment;
  FFragment := Parsed.Fragment;
  FHasOpaquePath := Parsed.HasOpaquePath;
  FIsValid := True;
end;

procedure TGocciaURLValue.MarkReferences;
begin
  if GCMarked then Exit;
  inherited;
  if Assigned(FSearchParams) then
    FSearchParams.MarkReferences;
end;

end.
