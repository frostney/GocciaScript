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
  EXT_JSONL = '.jsonl';
  EXT_TOML = '.toml';
  EXT_YAML = '.yaml';
  EXT_YML  = '.yml';
  EXT_GBC  = '.gbc';

  ScriptExtensions: array[0..4] of string = (
    EXT_JS, EXT_JSX, EXT_TS, EXT_TSX, EXT_MJS
  );

  JSXNativeExtensions: array[0..1] of string = (
    EXT_JSX, EXT_TSX
  );

function IsScriptExtension(const AExtension: string): Boolean;
function IsJSONLExtension(const AExtension: string): Boolean;
function IsJSXNativeExtension(const AExtension: string): Boolean;
function IsTOMLExtension(const AExtension: string): Boolean;
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

function IsJSONLExtension(const AExtension: string): Boolean;
var
  Ext: string;
begin
  Ext := LowerCase(AExtension);
  Result := Ext = EXT_JSONL;
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

function IsStructuredDataExtension(const AExtension: string): Boolean;
var
  Ext: string;
begin
  Ext := LowerCase(AExtension);
  Result := (Ext = EXT_JSON) or IsJSONLExtension(Ext) or
    IsTOMLExtension(Ext) or IsYAMLExtension(Ext);
end;

function IsStructuredGlobalsExtension(const AExtension: string): Boolean;
var
  Ext: string;
begin
  Ext := LowerCase(AExtension);
  Result := (Ext = EXT_JSON) or IsTOMLExtension(Ext) or IsYAMLExtension(Ext);
end;

end.
