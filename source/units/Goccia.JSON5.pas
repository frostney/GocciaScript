unit Goccia.JSON5;

{$I Goccia.inc}

interface

uses
  Goccia.JSON,
  Goccia.Values.Primitives;

type
  EGocciaJSON5ParseError = class(EGocciaJSONParseError);

  TGocciaJSON5Parser = class(TGocciaJSONParser)
  public
    constructor Create;
    function Parse(const AText: UTF8String): TGocciaValue; override;
  end;

  TGocciaJSON5Stringifier = class
  private
    FStringifier: TGocciaJSONStringifier;
  public
    constructor Create;
    destructor Destroy; override;
    function Stringify(const AValue: TGocciaValue; const AGap: string = '';
      const APreferredQuoteChar: Char = #0): string;
  end;

implementation

uses
  JSONParser;

constructor TGocciaJSON5Parser.Create;
begin
  inherited Create(JSONParserJSON5Capabilities);
end;

function TGocciaJSON5Parser.Parse(const AText: UTF8String): TGocciaValue;
begin
  try
    Result := inherited Parse(AText);
  except
    on E: EGocciaJSONParseError do
      raise EGocciaJSON5ParseError.Create(E.Message);
  end;
end;

constructor TGocciaJSON5Stringifier.Create;
begin
  inherited Create;
  FStringifier := TGocciaJSONStringifier.Create(jsmJSON5);
end;

destructor TGocciaJSON5Stringifier.Destroy;
begin
  FStringifier.Free;
  inherited;
end;

function TGocciaJSON5Stringifier.Stringify(const AValue: TGocciaValue;
  const AGap: string; const APreferredQuoteChar: Char): string;
begin
  Result := FStringifier.Stringify(AValue, AGap, APreferredQuoteChar);
end;

end.
