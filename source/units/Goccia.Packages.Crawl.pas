unit Goccia.Packages.Crawl;

{ The file set of a provider package, found without a package manifest
  (ADR 0122).

  Install mode starts from the files the import map's entries name and
  follows what each module says it needs, literally:

  - `import … from "./x"`, `import "./x"`, `export … from "./x"`, and
    `import("./x")` name modules, which are fetched and followed in turn;
  - the same with an import attribute (`with`, type json) names a data
    file, fetched but not parsed;
  - `new URL("./x", import.meta.url)` and `import.meta.resolve("./x")` name
    assets such as native libraries, fetched but not parsed.

  Inside a package only relative specifiers are allowed: a bare, absolute,
  URL, or `github:` specifier is refused (no transitive provider packages),
  and so is one that leaves the repository or names a `goccia.*` file.
  Computed specifiers are not followed; a package that needs a file only a
  computed specifier reaches must name it literally somewhere. Modules are
  tokenized by the engine's own parser, so a specifier inside a string,
  comment, template, or regular expression is never mistaken for one. }

{$I Goccia.inc}

interface

uses
  Classes,
  Generics.Collections,
  SysUtils;

const
  { Ceilings on what one package's crawl may fetch. }
  MAX_PACKAGE_FILES = 2000;
  MAX_PACKAGE_BYTES = 256 * 1024 * 1024;

type
  EGocciaCrawlError = class(Exception);

  TGocciaReferenceKind = (grkModule, grkData, grkAsset);

  TGocciaLiteralReference = record
    Specifier: string;
    Kind: TGocciaReferenceKind;
  end;

  TGocciaLiteralReferences = array of TGocciaLiteralReference;

  { Fetches the package file APath: True with its bytes, False when it does
    not exist. Raises for any other failure. }
  TGocciaPackageFileFetcher = function(const APath: string;
    out ABytes: TBytes): Boolean of object;

  TGocciaCrawledFile = class
  public
    Path: string;
    Bytes: TBytes;
  end;

  TGocciaCrawledFileList = TObjectList<TGocciaCrawledFile>;

  TGocciaPackageCrawl = class
  private
    FFetcher: TGocciaPackageFileFetcher;
    FFiles: TGocciaCrawledFileList;
    FKey: string;
    FPending: TList<TGocciaLiteralReference>;
    FPendingFrom: TStringList;
    FTotalBytes: Int64;
    function IndexOfFile(const APath: string): Integer;
    function AddFile(const APath: string; const ABytes: TBytes): Boolean;
    procedure Follow(const AReference: TGocciaLiteralReference;
      const AFromFile: string);
    procedure FollowReferences(const APath: string; const ABytes: TBytes);
  public
    constructor Create(const APackageKey: string;
      const AFetcher: TGocciaPackageFileFetcher);
    destructor Destroy; override;
    { An entry point: a module path (probed in the resolver's candidate
      order), a directory ending in `/`, or '' for the repository root. }
    procedure AddModuleEntry(const APath: string);
    procedure Run;
    { The files reached, sorted by path in byte order. }
    property Files: TGocciaCrawledFileList read FFiles;
  end;

{ The literal references in module source ASource (AFileName picks the
  JSX preprocessor). Raises EGocciaCrawlError when it does not parse. }
function ExtractLiteralReferences(const ASource,
  AFileName: string): TGocciaLiteralReferences;

{ Joins the relative specifier ASpecifier to the package file AFromFile.
  False when the result leaves the repository. }
function TryJoinPackagePath(const AFromFile, ASpecifier: string;
  out APath: string): Boolean;

function IsRelativeSpecifier(const ASpecifier: string): Boolean;

implementation

uses
  TextEncoding,
  TextSemantics,

  Goccia.Error,
  Goccia.FileExtensions,
  Goccia.Packages.Address,
  Goccia.SourcePipeline,
  Goccia.Token;

const
  IDENTIFIER_URL = 'URL';
  IDENTIFIER_META = 'meta';
  IDENTIFIER_RESOLVE = 'resolve';
  IDENTIFIER_TYPE = 'type';
  IDENTIFIER_ASSERT = 'assert';
  IDENTIFIER_WITH = 'with';
  IDENTIFIER_URL_PROPERTY = 'url';

function IsRelativeSpecifier(const ASpecifier: string): Boolean;
begin
  Result := (Copy(ASpecifier, 1, 2) = './') or (Copy(ASpecifier, 1, 3) = '../');
end;

function TryJoinPackagePath(const AFromFile, ASpecifier: string;
  out APath: string): Boolean;
var
  Parts: TStringList;
  Directory, Segment: string;
  I, SlashIndex: Integer;
begin
  APath := '';
  Result := False;
  SlashIndex := LastDelimiter('/', AFromFile);
  Directory := Copy(AFromFile, 1, SlashIndex);
  Parts := TStringList.Create;
  try
    Parts.Delimiter := '/';
    Parts.StrictDelimiter := True;
    Parts.DelimitedText := Directory + ASpecifier;
    I := 0;
    while I < Parts.Count do
    begin
      Segment := Parts[I];
      if (Segment = '') or (Segment = '.') then
      begin
        { A trailing empty segment keeps the directory's `/`. }
        if (Segment = '') and (I = Parts.Count - 1) and (I > 0) then
        begin
          Parts[I - 1] := Parts[I - 1] + '/';
        end;
        Parts.Delete(I);
        Continue;
      end;
      if Segment = '..' then
      begin
        if I = 0 then
          Exit;
        Parts.Delete(I);
        Parts.Delete(I - 1);
        Dec(I);
        Continue;
      end;
      Inc(I);
    end;
    APath := '';
    for I := 0 to Parts.Count - 1 do
    begin
      if I > 0 then
        APath := APath + '/';
      APath := APath + Parts[I];
    end;
  finally
    Parts.Free;
  end;
  Result := True;
end;

{ ── literal references ───────────────────────────────────────── }

type
  TTokenCursor = record
    Tokens: TGocciaSourceTokenArray;
    function IsType(const AIndex: Integer;
      const AType: TGocciaTokenType): Boolean;
    function IsIdentifier(const AIndex: Integer;
      const AName: string): Boolean;
  end;

function TTokenCursor.IsType(const AIndex: Integer;
  const AType: TGocciaTokenType): Boolean;
begin
  Result := (AIndex >= 0) and (AIndex <= High(Tokens)) and
    (Tokens[AIndex].TokenType = AType);
end;

function TTokenCursor.IsIdentifier(const AIndex: Integer;
  const AName: string): Boolean;
begin
  Result := IsType(AIndex, gttIdentifier) and (Tokens[AIndex].Lexeme = AName);
end;

procedure AddReference(var AReferences: TGocciaLiteralReferences;
  const ASpecifier: string; const AKind: TGocciaReferenceKind);
begin
  SetLength(AReferences, Length(AReferences) + 1);
  AReferences[High(AReferences)].Specifier := ASpecifier;
  AReferences[High(AReferences)].Kind := AKind;
end;

(* `with { type: "json" }` (or `assert`) at AIndex: True when an attribute
  list follows the specifier, so the import is of data. *)
function HasImportAttributes(const ACursor: TTokenCursor;
  const AIndex: Integer): Boolean;
begin
  Result := (ACursor.IsType(AIndex, gttWith) or
    ACursor.IsIdentifier(AIndex, IDENTIFIER_ASSERT)) and
    ACursor.IsType(AIndex + 1, gttLeftBrace) and
    (ACursor.IsIdentifier(AIndex + 2, IDENTIFIER_TYPE) or
     (ACursor.IsType(AIndex + 2, gttString) and
      (ACursor.Tokens[AIndex + 2].Lexeme = IDENTIFIER_TYPE)));
end;

{ The index of the `from` that ends the clause starting at AIndex, at brace
  depth zero, or -1 when the statement ends first. }
function FindFrom(const ACursor: TTokenCursor; const AIndex: Integer): Integer;
var
  Depth, I: Integer;
begin
  Depth := 0;
  I := AIndex;
  while I <= High(ACursor.Tokens) do
  begin
    case ACursor.Tokens[I].TokenType of
      gttLeftBrace:
        Inc(Depth);
      gttRightBrace:
        begin
          Dec(Depth);
          if Depth < 0 then
            Exit(-1);
        end;
      gttFrom:
        if Depth = 0 then
          Exit(I);
      gttSemicolon, gttImport, gttExport, gttEOF, gttString:
        if Depth = 0 then
          Exit(-1);
    end;
    Inc(I);
  end;
  Result := -1;
end;

function ExtractLiteralReferences(const ASource,
  AFileName: string): TGocciaLiteralReferences;
var
  Cursor: TTokenCursor;
  I, FromIndex: Integer;
  Lines: TStringList;
  Options: TGocciaSourcePipelineOptions;
  Parsed: TGocciaSourcePipelineResult;

  procedure AddStatic(const ASpecifierIndex: Integer);
  begin
    if HasImportAttributes(Cursor, ASpecifierIndex + 1) then
      AddReference(Result, Cursor.Tokens[ASpecifierIndex].Lexeme, grkData)
    else
      AddReference(Result, Cursor.Tokens[ASpecifierIndex].Lexeme, grkModule);
  end;

begin
  Result := nil;
  Options := TGocciaSourcePipeline.DefaultOptions;
  Options.Compatibility := [Low(TGocciaCompatibility)..
    High(TGocciaCompatibility)];
  Options.LabelStatementsEnabled := True;
  Options.ForInLoopsEnabled := True;
  Options.ExperimentalJSModuleSourceEnabled := True;
  Options.WarningUnsupportedFeatures := True;
  Options.SourceType := stModule;
  Options.CollectTokens := True;
  if IsJSXNativeExtension(ExtractFileExt(AFileName)) then
    Options.Preprocessors := [ppJSX];
  Lines := CreateFileTextLines(ASource);
  try
    try
      Parsed := TGocciaSourcePipeline.Parse(Lines, AFileName, Options);
    except
      on E: Exception do
        raise EGocciaCrawlError.CreateFmt('%s does not parse: %s',
          [AFileName, E.Message]);
    end;
    try
      Cursor.Tokens := Parsed.Tokens;
    finally
      Parsed.Free;
    end;
  finally
    Lines.Free;
  end;

  I := 0;
  while I <= High(Cursor.Tokens) do
  begin
    case Cursor.Tokens[I].TokenType of
      gttImport:
        if Cursor.IsType(I - 1, gttDot) then
          { A property named import. }
        else if Cursor.IsType(I + 1, gttLeftParen) then
        begin
          (* import("./x") and import("./x", { with: … }) *)
          if Cursor.IsType(I + 2, gttString) and
             (Cursor.IsType(I + 3, gttRightParen) or
              Cursor.IsType(I + 3, gttComma)) then
          begin
            if Cursor.IsType(I + 3, gttComma) and
               Cursor.IsType(I + 4, gttLeftBrace) and
               (Cursor.IsType(I + 5, gttWith) or
                Cursor.IsIdentifier(I + 5, IDENTIFIER_WITH)) and
               Cursor.IsType(I + 6, gttColon) and
               Cursor.IsType(I + 7, gttLeftBrace) then
              AddReference(Result, Cursor.Tokens[I + 2].Lexeme, grkData)
            else
              AddReference(Result, Cursor.Tokens[I + 2].Lexeme, grkModule);
          end;
        end
        else if Cursor.IsType(I + 1, gttDot) then
        begin
          { import.meta.resolve("./x") }
          if Cursor.IsIdentifier(I + 2, IDENTIFIER_META) and
             Cursor.IsType(I + 3, gttDot) and
             Cursor.IsIdentifier(I + 4, IDENTIFIER_RESOLVE) and
             Cursor.IsType(I + 5, gttLeftParen) and
             Cursor.IsType(I + 6, gttString) and
             Cursor.IsType(I + 7, gttRightParen) then
            AddReference(Result, Cursor.Tokens[I + 6].Lexeme, grkAsset);
        end
        else if Cursor.IsType(I + 1, gttString) then
          AddStatic(I + 1)
        else if not Cursor.IsIdentifier(I + 1, IDENTIFIER_TYPE) or
          Cursor.IsType(I + 2, gttFrom) or Cursor.IsType(I + 2, gttComma) then
        begin
          { `import type … from` loads nothing; `import type from "x"`
            imports a binding named type. }
          FromIndex := FindFrom(Cursor, I + 1);
          if (FromIndex > 0) and Cursor.IsType(FromIndex + 1, gttString) then
            AddStatic(FromIndex + 1);
        end;
      gttExport:
        if (Cursor.IsType(I + 1, gttStar) or Cursor.IsType(I + 1, gttLeftBrace)) then
        begin
          FromIndex := FindFrom(Cursor, I + 1);
          if (FromIndex > 0) and Cursor.IsType(FromIndex + 1, gttString) then
            AddStatic(FromIndex + 1);
        end;
      gttNew:
        { new URL("./x", import.meta.url) }
        if Cursor.IsIdentifier(I + 1, IDENTIFIER_URL) and
           Cursor.IsType(I + 2, gttLeftParen) and
           Cursor.IsType(I + 3, gttString) and Cursor.IsType(I + 4, gttComma) and
           Cursor.IsType(I + 5, gttImport) and Cursor.IsType(I + 6, gttDot) and
           Cursor.IsIdentifier(I + 7, IDENTIFIER_META) and
           Cursor.IsType(I + 8, gttDot) and
           Cursor.IsIdentifier(I + 9, IDENTIFIER_URL_PROPERTY) and
           (Cursor.IsType(I + 10, gttRightParen) or
            (Cursor.IsType(I + 10, gttComma) and
             Cursor.IsType(I + 11, gttRightParen))) then
          AddReference(Result, Cursor.Tokens[I + 3].Lexeme, grkAsset);
    end;
    Inc(I);
  end;
end;

{ ── the crawl ────────────────────────────────────────────────── }

constructor TGocciaPackageCrawl.Create(const APackageKey: string;
  const AFetcher: TGocciaPackageFileFetcher);
begin
  inherited Create;
  FKey := APackageKey;
  FFetcher := AFetcher;
  FFiles := TGocciaCrawledFileList.Create(True);
  FPending := TList<TGocciaLiteralReference>.Create;
  FPendingFrom := TStringList.Create;
end;

destructor TGocciaPackageCrawl.Destroy;
begin
  FPendingFrom.Free;
  FPending.Free;
  FFiles.Free;
  inherited;
end;

function TGocciaPackageCrawl.IndexOfFile(const APath: string): Integer;
var
  I: Integer;
begin
  for I := 0 to FFiles.Count - 1 do
    if FFiles[I].Path = APath then
      Exit(I);
  Result := -1;
end;

function TGocciaPackageCrawl.AddFile(const APath: string;
  const ABytes: TBytes): Boolean;
var
  Crawled: TGocciaCrawledFile;
begin
  Result := IndexOfFile(APath) < 0;
  if not Result then
    Exit;
  if FFiles.Count >= MAX_PACKAGE_FILES then
    raise EGocciaCrawlError.CreateFmt('%s reaches more than %d files',
      [FKey, MAX_PACKAGE_FILES]);
  Inc(FTotalBytes, Length(ABytes));
  if FTotalBytes > MAX_PACKAGE_BYTES then
    raise EGocciaCrawlError.CreateFmt('%s is larger than %d MiB',
      [FKey, MAX_PACKAGE_BYTES div (1024 * 1024)]);
  Crawled := TGocciaCrawledFile.Create;
  Crawled.Path := APath;
  Crawled.Bytes := ABytes;
  FFiles.Add(Crawled);
end;

procedure TGocciaPackageCrawl.AddModuleEntry(const APath: string);
var
  Reference: TGocciaLiteralReference;
begin
  Reference.Specifier := APath;
  Reference.Kind := grkModule;
  FPending.Add(Reference);
  { An entry is a path in the package, not relative to a file. }
  FPendingFrom.Add('');
end;

procedure TGocciaPackageCrawl.FollowReferences(const APath: string;
  const ABytes: TBytes);
var
  ErrorOffset, I: Integer;
  References: TGocciaLiteralReferences;
  Source: string;
begin
  if not IsScriptExtension(ExtractFileExt(APath)) then
    Exit;
  if not TryDecodeUTF8(ABytes, Source, ErrorOffset) then
    raise EGocciaCrawlError.CreateFmt('%s/%s is not UTF-8 (byte %d)',
      [FKey, APath, ErrorOffset]);
  References := ExtractLiteralReferences(Source, APath);
  for I := 0 to High(References) do
  begin
    if not IsRelativeSpecifier(References[I].Specifier) then
    begin
      { Assets name files only when relative; anything else (a remote URL,
        say) is not a package file. Imports must be relative. }
      if References[I].Kind = grkAsset then
        Continue;
      raise EGocciaCrawlError.CreateFmt(
        '%s/%s imports "%s": a package may import only its own files, by ' +
        'relative specifier (no bare, absolute, URL, or github: specifiers)',
        [FKey, APath, References[I].Specifier]);
    end;
    FPending.Add(References[I]);
    FPendingFrom.Add(APath);
  end;
end;

procedure TGocciaPackageCrawl.Follow(
  const AReference: TGocciaLiteralReference; const AFromFile: string);
var
  Bytes: TBytes;
  Candidates: TGocciaPackagePathArray;
  Path: string;
  I: Integer;
begin
  if AFromFile = '' then
    Path := AReference.Specifier
  else if not TryJoinPackagePath(AFromFile, AReference.Specifier, Path) then
    raise EGocciaCrawlError.CreateFmt(
      '%s/%s names "%s", which leaves the repository',
      [FKey, AFromFile, AReference.Specifier]);

  if AReference.Kind = grkModule then
    Candidates := PackageModuleCandidates(Path, EngineModuleImportExtensions)
  else
  begin
    SetLength(Candidates, 1);
    Candidates[0] := Path;
  end;

  for I := 0 to High(Candidates) do
  begin
    if not IsSafeArtifactPath(Candidates[I]) then
      raise EGocciaCrawlError.CreateFmt(
        '%s: "%s" is not a file a package may contain (a goccia.* file, ' +
        'a .goccia directory, or an unsafe name)', [FKey, Candidates[I]]);
    if IndexOfFile(Candidates[I]) >= 0 then
      Exit;
    if FFetcher(Candidates[I], Bytes) then
    begin
      AddFile(Candidates[I], Bytes);
      FollowReferences(Candidates[I], Bytes);
      Exit;
    end;
  end;
  if AFromFile = '' then
    raise EGocciaCrawlError.CreateFmt('%s has no file for "%s"',
      [FKey, AReference.Specifier])
  else
    raise EGocciaCrawlError.CreateFmt('%s/%s names "%s", which does not ' +
      'exist', [FKey, AFromFile, AReference.Specifier]);
end;

procedure TGocciaPackageCrawl.Run;
var
  Reference: TGocciaLiteralReference;
  From: string;
  I, J: Integer;
  Swap: TGocciaCrawledFile;
begin
  while FPending.Count > 0 do
  begin
    Reference := FPending[0];
    From := FPendingFrom[0];
    FPending.Delete(0);
    FPendingFrom.Delete(0);
    Follow(Reference, From);
  end;
  { Byte order, so the lockfile is the same whatever order files were
    reached in. }
  for I := 1 to FFiles.Count - 1 do
  begin
    J := I;
    while (J > 0) and (CompareStr(FFiles[J - 1].Path, FFiles[J].Path) > 0) do
    begin
      Swap := FFiles.Extract(FFiles[J]);
      FFiles.Insert(J - 1, Swap);
      Dec(J);
    end;
  end;
end;

end.
