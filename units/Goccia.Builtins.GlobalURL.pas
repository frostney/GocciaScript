unit Goccia.Builtins.GlobalURL;

// WHATWG URL Standard §4 URL / §6 URLSearchParams — engine registration
// https://url.spec.whatwg.org/

{$I Goccia.inc}

interface

uses
  Goccia.Arguments.Collection,
  Goccia.Builtins.Base,
  Goccia.Error.ThrowErrorCallback,
  Goccia.ObjectModel,
  Goccia.Scope,
  Goccia.Values.Primitives;

type
  TGocciaGlobalURL = class(TGocciaBuiltin)
  private
    class var FStaticMembers: array of TGocciaMemberDefinition;
  published
    // WHATWG URL §4.7.3 URL.canParse(url[, base])
    function CanParse(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;

    // WHATWG URL §4.7.2 URL.parse(url[, base])
    function Parse(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
  public
    constructor Create(const AName: string; const AScope: TGocciaScope;
      const AThrowError: TGocciaThrowErrorCallback);
  end;

  TGocciaGlobalURLSearchParams = class(TGocciaBuiltin)
  public
    constructor Create(const AName: string; const AScope: TGocciaScope;
      const AThrowError: TGocciaThrowErrorCallback);
  end;

implementation

uses
  SysUtils,

  Goccia.Arguments.Validator,
  Goccia.Constants.PropertyNames,
  Goccia.URL.Parser,
  Goccia.Values.ErrorHelper,
  Goccia.Values.URLSearchParamsValue,
  Goccia.Values.URLValue;

{ TGocciaGlobalURL }

constructor TGocciaGlobalURL.Create(const AName: string;
  const AScope: TGocciaScope;
  const AThrowError: TGocciaThrowErrorCallback);
var
  Members: TGocciaMemberCollection;
begin
  inherited Create(AName, AScope, AThrowError);

  // Initialize the shared URL prototype lazily
  TGocciaURLValue.ExposePrototype(FBuiltinObject);

  Members := TGocciaMemberCollection.Create;
  try
    Members.AddNamedMethod(PROP_CAN_PARSE, CanParse, 1, gmkStaticMethod);
    Members.AddNamedMethod(PROP_PARSE, Parse, 1, gmkStaticMethod);
    FStaticMembers := Members.ToDefinitions;
  finally
    Members.Free;
  end;
  RegisterMemberDefinitions(FBuiltinObject, FStaticMembers);
end;

// WHATWG URL §4.7.3 URL.canParse(url[, base]) -> boolean
function TGocciaGlobalURL.CanParse(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  URLStr, BaseStr: string;
  Parsed, BaseParsed: TGocciaURLRecord;
begin
  TGocciaArgumentValidator.RequireAtLeast(AArgs, 1, 'URL.canParse', ThrowError);
  URLStr := AArgs.GetElement(0).ToStringLiteral.Value;

  if AArgs.Length >= 2 then
  begin
    BaseStr := AArgs.GetElement(1).ToStringLiteral.Value;
    BaseParsed := ParseURL(BaseStr);
    if not BaseParsed.IsValid then
    begin
      Result := TGocciaBooleanLiteralValue.FalseValue;
      Exit;
    end;
    Parsed := ParseURLWithBase(URLStr, BaseParsed);
  end
  else
    Parsed := ParseURL(URLStr);

  if Parsed.IsValid then
    Result := TGocciaBooleanLiteralValue.TrueValue
  else
    Result := TGocciaBooleanLiteralValue.FalseValue;
end;

// WHATWG URL §4.7.2 URL.parse(url[, base]) -> URL or null
function TGocciaGlobalURL.Parse(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  URLStr, BaseStr: string;
  Parsed, BaseParsed: TGocciaURLRecord;
  URLObj: TGocciaURLValue;
begin
  TGocciaArgumentValidator.RequireAtLeast(AArgs, 1, 'URL.parse', ThrowError);
  URLStr := AArgs.GetElement(0).ToStringLiteral.Value;

  if AArgs.Length >= 2 then
  begin
    BaseStr := AArgs.GetElement(1).ToStringLiteral.Value;
    BaseParsed := ParseURL(BaseStr);
    if not BaseParsed.IsValid then
    begin
      Result := TGocciaNullLiteralValue.NullValue;
      Exit;
    end;
    Parsed := ParseURLWithBase(URLStr, BaseParsed);
  end
  else
    Parsed := ParseURL(URLStr);

  if not Parsed.IsValid then
  begin
    Result := TGocciaNullLiteralValue.NullValue;
    Exit;
  end;

  URLObj := TGocciaURLValue.Create;
  URLObj.InitFromRecord(
    Parsed.Scheme, Parsed.Username, Parsed.Password,
    Parsed.Host, Parsed.HasNullHost, Parsed.Port,
    SerializePath(Parsed),
    Parsed.HasNullQuery, Parsed.Query,
    Parsed.HasNullFragment, Parsed.Fragment,
    Parsed.HasOpaquePath);
  Result := URLObj;
end;

{ TGocciaGlobalURLSearchParams }

constructor TGocciaGlobalURLSearchParams.Create(const AName: string;
  const AScope: TGocciaScope;
  const AThrowError: TGocciaThrowErrorCallback);
begin
  inherited Create(AName, AScope, AThrowError);
  // Initialize the shared URLSearchParams prototype lazily
  TGocciaURLSearchParamsValue.ExposePrototype(FBuiltinObject);
end;

end.
