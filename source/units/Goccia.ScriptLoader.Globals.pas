unit Goccia.ScriptLoader.Globals;

{$I Goccia.inc}

interface

uses
  Goccia.Values.Primitives;

type
  TScriptLoaderGlobalPair = record
    Key: string;
    ValueText: string;
  end;

function ParseGlobalPair(const AArg: string): TScriptLoaderGlobalPair;
function ParseInlineGlobalValue(const AValueText: string): TGocciaValue;
function IsStructuredGlobalsFile(const APath: string): Boolean;
function IsJSON5GlobalsFile(const APath: string): Boolean;
function IsTOMLGlobalsFile(const APath: string): Boolean;
function IsYAMLGlobalsFile(const APath: string): Boolean;
function ReadFileText(const APath: string): UTF8String;

implementation

uses
  SysUtils,

  Goccia.FileExtensions,
  Goccia.JSON,
  Goccia.TextFiles;

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
      on Exception do
        Result := TGocciaStringLiteralValue.Create(AValueText);
    end;
  finally
    Parser.Free;
  end;
end;

function IsStructuredGlobalsFile(const APath: string): Boolean;
begin
  Result := IsStructuredGlobalsExtension(ExtractFileExt(APath));
end;

function IsJSON5GlobalsFile(const APath: string): Boolean;
begin
  Result := IsJSON5Extension(ExtractFileExt(APath));
end;

function IsTOMLGlobalsFile(const APath: string): Boolean;
begin
  Result := IsTOMLExtension(ExtractFileExt(APath));
end;

function IsYAMLGlobalsFile(const APath: string): Boolean;
begin
  Result := IsYAMLExtension(ExtractFileExt(APath));
end;

function ReadFileText(const APath: string): UTF8String;
begin
  Result := ReadUTF8FileText(APath);
end;

end.
