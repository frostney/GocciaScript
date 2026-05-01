unit Goccia.FileExtensions;

{$I Goccia.inc}

interface

const
  EXT_JS   = '.js';
  EXT_JSX  = '.jsx';
  EXT_TS   = '.ts';
  EXT_TSX  = '.tsx';
  EXT_MJS  = '.mjs';
  EXT_JSON = '.json';
  EXT_JSON5 = '.json5';
  EXT_JSONC = '.jsonc';
  EXT_JSONL = '.jsonl';
  EXT_TOML = '.toml';
  EXT_YAML = '.yaml';
  EXT_YML  = '.yml';
  EXT_CSV  = '.csv';
  EXT_TSV  = '.tsv';
  EXT_TXT  = '.txt';
  EXT_MD   = '.md';
  EXT_GBC  = '.gbc';
  EXT_MAP  = '.map';

  ScriptExtensions: array[0..4] of string = (
    EXT_JS, EXT_JSX, EXT_TS, EXT_TSX, EXT_MJS
  );

  JSXNativeExtensions: array[0..1] of string = (
    EXT_JSX, EXT_TSX
  );

  ModuleImportExtensions: array[0..15] of string = (
    EXT_JS, EXT_JSX, EXT_TS, EXT_TSX, EXT_MJS,
    EXT_JSON, EXT_JSON5, EXT_JSONC, EXT_JSONL, EXT_TOML, EXT_YAML, EXT_YML,
    EXT_CSV, EXT_TSV,
    EXT_TXT, EXT_MD
  );

  EngineModuleImportExtensions: array[0..5] of string = (
    EXT_JS, EXT_JSX, EXT_TS, EXT_TSX, EXT_MJS, EXT_JSON
  );

function IsScriptExtension(const AExtension: string): Boolean;
function IsCSVExtension(const AExtension: string): Boolean;
function IsJSON5Extension(const AExtension: string): Boolean;
function IsJSONLExtension(const AExtension: string): Boolean;
function IsJSXNativeExtension(const AExtension: string): Boolean;
function IsTextAssetExtension(const AExtension: string): Boolean;
function IsTOMLExtension(const AExtension: string): Boolean;
function IsTSVExtension(const AExtension: string): Boolean;
function IsYAMLExtension(const AExtension: string): Boolean;
function IsStructuredDataExtension(const AExtension: string): Boolean;
function IsStructuredGlobalsExtension(const AExtension: string): Boolean;

implementation

uses
  SysUtils;

function IsScriptExtension(const AExtension: string): Boolean;
var
  Ext: string;
  I: Integer;
begin
  Ext := LowerCase(AExtension);
  for I := Low(ScriptExtensions) to High(ScriptExtensions) do
    if Ext = ScriptExtensions[I] then
      Exit(True);
  Result := False;
end;

function IsJSXNativeExtension(const AExtension: string): Boolean;
var
  Ext: string;
  I: Integer;
begin
  Ext := LowerCase(AExtension);
  for I := Low(JSXNativeExtensions) to High(JSXNativeExtensions) do
    if Ext = JSXNativeExtensions[I] then
      Exit(True);
  Result := False;
end;

function IsCSVExtension(const AExtension: string): Boolean;
var
  Ext: string;
begin
  Ext := LowerCase(AExtension);
  Result := Ext = EXT_CSV;
end;

function IsJSONLExtension(const AExtension: string): Boolean;
var
  Ext: string;
begin
  Ext := LowerCase(AExtension);
  Result := Ext = EXT_JSONL;
end;

function IsJSON5Extension(const AExtension: string): Boolean;
var
  Ext: string;
begin
  Ext := LowerCase(AExtension);
  Result := (Ext = EXT_JSON5) or (Ext = EXT_JSONC);
end;

function IsYAMLExtension(const AExtension: string): Boolean;
var
  Ext: string;
begin
  Ext := LowerCase(AExtension);
  Result := (Ext = EXT_YAML) or (Ext = EXT_YML);
end;

function IsTOMLExtension(const AExtension: string): Boolean;
var
  Ext: string;
begin
  Ext := LowerCase(AExtension);
  Result := Ext = EXT_TOML;
end;

function IsTSVExtension(const AExtension: string): Boolean;
var
  Ext: string;
begin
  Ext := LowerCase(AExtension);
  Result := Ext = EXT_TSV;
end;

function IsTextAssetExtension(const AExtension: string): Boolean;
var
  Ext: string;
begin
  Ext := LowerCase(AExtension);
  Result := (Ext = EXT_TXT) or (Ext = EXT_MD);
end;

function IsStructuredDataExtension(const AExtension: string): Boolean;
var
  Ext: string;
begin
  Ext := LowerCase(AExtension);
  Result := (Ext = EXT_JSON) or IsJSON5Extension(Ext) or IsJSONLExtension(Ext) or
    IsTOMLExtension(Ext) or IsYAMLExtension(Ext) or IsCSVExtension(Ext) or
    IsTSVExtension(Ext);
end;

function IsStructuredGlobalsExtension(const AExtension: string): Boolean;
var
  Ext: string;
begin
  Ext := LowerCase(AExtension);
  Result := (Ext = EXT_JSON) or IsJSON5Extension(Ext) or IsTOMLExtension(Ext) or
    IsYAMLExtension(Ext);
end;

end.
