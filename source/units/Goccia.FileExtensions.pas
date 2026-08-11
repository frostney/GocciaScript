unit Goccia.FileExtensions;

{$I Goccia.inc}

interface

const
  EXT_JS   = '.js';
  EXT_JSX  = '.jsx';
  EXT_TS   = '.ts';
  EXT_TSX  = '.tsx';
  EXT_MJS  = '.mjs';
  EXT_MTS  = '.mts';
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

  ScriptExtensions: array[0..5] of string = (
    EXT_JS, EXT_JSX, EXT_TS, EXT_TSX, EXT_MJS, EXT_MTS
  );

  JSXNativeExtensions: array[0..1] of string = (
    EXT_JSX, EXT_TSX
  );

  // Extensions where a leading '<' is always type syntax and never JSX, so the
  // JSX preprocessor must not run. TypeScript draws the same line: '.ts' and
  // '.mts' keep angle-bracket type assertions and generic arrow functions
  // (`<T>(x: T) => T`), and only '.tsx' hands '<' to JSX and requires the
  // `<T,>` disambiguation.
  JSXExcludedExtensions: array[0..1] of string = (
    EXT_TS, EXT_MTS
  );

  ModuleImportExtensions: array[0..15] of string = (
    EXT_JS, EXT_JSX, EXT_TS, EXT_TSX, EXT_MJS,
    EXT_JSON, EXT_JSON5, EXT_JSONC, EXT_JSONL, EXT_TOML, EXT_YAML, EXT_YML,
    EXT_CSV, EXT_TSV,
    EXT_TXT, EXT_MD
  );

  EngineModuleImportExtensions: array[0..7] of string = (
    EXT_JS, EXT_JSX, EXT_TS, EXT_TSX, EXT_MJS, EXT_JSON, EXT_TXT, EXT_MD
  );

type
  TFileExtensionArray = array of string;

function IsScriptExtension(const AExtension: string): Boolean;
function IsCSVExtension(const AExtension: string): Boolean;
function IsJSON5Extension(const AExtension: string): Boolean;
function IsJSONLExtension(const AExtension: string): Boolean;
function IsJSXNativeExtension(const AExtension: string): Boolean;
function IsJSXExcludedExtension(const AExtension: string): Boolean;
function IsModuleSourceExtension(const AExtension: string): Boolean;
function IsModuleSourceFileName(const AFileName: string): Boolean;
function IsTextAssetExtension(const AExtension: string): Boolean;
function IsTOMLExtension(const AExtension: string): Boolean;
function IsTSVExtension(const AExtension: string): Boolean;
function IsTypeScriptExtension(const AExtension: string): Boolean;
function IsTypeScriptFileName(const AFileName: string): Boolean;
function IsYAMLExtension(const AExtension: string): Boolean;
function IsStructuredDataExtension(const AExtension: string): Boolean;
function IsStructuredGlobalsExtension(const AExtension: string): Boolean;
function TypeScriptSourceCandidates(const APath: string): TFileExtensionArray;

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

function IsJSXExcludedExtension(const AExtension: string): Boolean;
var
  Ext: string;
  I: Integer;
begin
  Ext := LowerCase(AExtension);
  for I := Low(JSXExcludedExtensions) to High(JSXExcludedExtensions) do
    if Ext = JSXExcludedExtensions[I] then
      Exit(True);
  Result := False;
end;

function IsModuleSourceExtension(const AExtension: string): Boolean;
var
  Ext: string;
begin
  Ext := LowerCase(AExtension);
  Result := (Ext = EXT_MJS) or (Ext = EXT_MTS);
end;

function IsModuleSourceFileName(const AFileName: string): Boolean;
begin
  Result := IsModuleSourceExtension(ExtractFileExt(AFileName));
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

// Extensions whose source is TypeScript rather than JavaScript. GocciaScript
// erases type syntax in every source file (types as comments), so this is not
// the gate for annotations in general — it is the gate for the few TypeScript
// constructs that collide with valid JavaScript, and so may only be read as
// types where the file says the source language is TypeScript. Call-site type
// arguments are the case in point: `a < b > (c)` is a comparison chain in
// JavaScript and a generic call in TypeScript.
function IsTypeScriptExtension(const AExtension: string): Boolean;
var
  Ext: string;
begin
  Ext := LowerCase(AExtension);
  Result := (Ext = EXT_TS) or (Ext = EXT_TSX) or (Ext = EXT_MTS);
end;

function IsTypeScriptFileName(const AFileName: string): Boolean;
begin
  Result := IsTypeScriptExtension(ExtractFileExt(AFileName));
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

// TypeScript's ESM convention: the source writes the *emitted* specifier
// (`./dep.js`) while the file on disk is the TypeScript input (`dep.ts`). The
// mapping below is what tsx, ts-node, vite and bun apply when a JavaScript
// specifier misses on disk. Callers must try the literal path first — an
// on-disk `.js` file always wins over its `.ts` neighbour, so this is a
// fallback and never a shadow.
//
// `.jsx` maps to `.ts` before `.tsx`, matching bun (the differential oracle for
// module resolution) rather than tsc, which only offers `.tsx` for a `.jsx`
// specifier. The extra candidate is strictly permissive: it can only resolve a
// specifier that would otherwise have failed.
function TypeScriptSourceCandidates(const APath: string): TFileExtensionArray;
var
  Extension, Stem: string;
begin
  SetLength(Result, 0);

  Extension := LowerCase(ExtractFileExt(APath));
  if (Extension <> EXT_JS) and (Extension <> EXT_JSX) and
    (Extension <> EXT_MJS) then
    Exit;

  Stem := Copy(APath, 1, Length(APath) - Length(Extension));

  if Extension = EXT_MJS then
  begin
    SetLength(Result, 1);
    Result[0] := Stem + EXT_MTS;
    Exit;
  end;

  SetLength(Result, 2);
  Result[0] := Stem + EXT_TS;
  Result[1] := Stem + EXT_TSX;
end;

end.
