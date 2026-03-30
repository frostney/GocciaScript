unit Goccia.ScriptLoader.Globals;

{$I Goccia.inc}

interface

uses
  Classes,

  Goccia.Values.Primitives;

type
  TScriptLoaderGlobalPair = record
    Key: string;
    ValueText: string;
  end;

function ParseGlobalPair(const AArg: string): TScriptLoaderGlobalPair;
function ParseInlineGlobalValue(const AValueText: string): TGocciaValue;
function IsJSONGlobalsFile(const APath: string): Boolean;
function ReadFileText(const APath: string): string;

implementation

uses
  SysUtils,

  Goccia.FileExtensions,
  Goccia.JSON;

function ParseGlobalPair(const AArg: string): TScriptLoaderGlobalPair;
var
  SeparatorIndex: Integer;
begin
  SeparatorIndex := Pos('=', AArg);
  if SeparatorIndex <= 1 then
    raise Exception.Create('Invalid --global argument. Use --global name=value.');

  Result.Key := Copy(AArg, 1, SeparatorIndex - 1);
  Result.ValueText := Copy(AArg, SeparatorIndex + 1, MaxInt);
end;

function ParseInlineGlobalValue(const AValueText: string): TGocciaValue;
var
  Parser: TGocciaJSONParser;
begin
  Parser := TGocciaJSONParser.Create;
  try
    try
      Result := Parser.Parse(AValueText);
    except
      on EGocciaJSONParseError do
        Result := TGocciaStringLiteralValue.Create(AValueText);
    end;
  finally
    Parser.Free;
  end;
end;

function IsJSONGlobalsFile(const APath: string): Boolean;
begin
  Result := LowerCase(ExtractFileExt(APath)) = EXT_JSON;
end;

function ReadFileText(const APath: string): string;
var
  Source: TStringList;
begin
  Source := TStringList.Create;
  try
    Source.LoadFromFile(APath);
    Result := Source.Text;
  finally
    Source.Free;
  end;
end;

end.
