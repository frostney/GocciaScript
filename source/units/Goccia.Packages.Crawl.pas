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
  MAX_CRAWL_REQUESTS = 10000;

type
  EGocciaCrawlError = class(Exception);

  TGocciaReferenceKind = (crkModule, crkData, crkAsset);

  { The crawl needs a file its known file set does not settle: the caller
    must fetch it (see TGocciaPackageCrawl.KnownFiles). }
  EGocciaCrawlNeedsFetch = class(EGocciaCrawlError);

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

  { A file waiting to be fetched: a package path, how it was named, and the
    file that named it ('' for an entry point). }
  TGocciaPendingReference = record
    Path: string;
    Specifier: string;
    Kind: TGocciaReferenceKind;
    FromFile: string;
  end;

  TGocciaPendingReferenceList = TList<TGocciaPendingReference>;

  TGocciaPackageCrawl = class
  private
    FAbsent: TDictionary<string, Boolean>;
    FFetcher: TGocciaPackageFileFetcher;
    FFileIndex: TDictionary<string, Integer>;
    FFiles: TGocciaCrawledFileList;
    FKey: string;
    FKnownFiles: TDictionary<string, Boolean>;
    FNext: Integer;
    { Files already parsed as modules; a file first reached as data or an
      asset is parsed when a module reference reaches it later. }
    FParsed: TDictionary<string, Boolean>;
    FPending: TGocciaPendingReferenceList;
    FQueued: TDictionary<string, Boolean>;
    FRequests: Integer;
    FTotalBytes: Int64;
    procedure Enqueue(const APath, ASpecifier: string;
      const AKind: TGocciaReferenceKind; const AFromFile: string);
    procedure AddFile(const APath: string; const ABytes: TBytes);
    function Fetch(const APath: string; out ABytes: TBytes): Boolean;
    procedure Follow(const AReference: TGocciaPendingReference);
    procedure FollowReferences(const APath: string; const ABytes: TBytes);
  public
    constructor Create(const APackageKey: string;
      const AFetcher: TGocciaPackageFileFetcher);
    destructor Destroy; override;
    { An entry point: a module path (probed in the resolver's candidate
      order), a directory ending in `/`, or '' for the repository root. }
    procedure AddModuleEntry(const APath: string);
    { Settles candidates from a known file set instead of probing: the first
      candidate in the set is the file, as run-time resolution picks among
      pinned files, and the others are absent without a request. A reference
      no known file answers raises EGocciaCrawlNeedsFetch. }
    procedure SetKnownFiles(const APaths: array of string);
    procedure Run;
    { The files reached, sorted by path in byte order. }
    property Files: TGocciaCrawledFileList read FFiles;
    { Fetcher calls so far, including those answered as absent. }
    property Requests: Integer read FRequests;
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

  Goccia.Constants.ConstructorNames,
  Goccia.Constants.PropertyNames,
  Goccia.Error,
  Goccia.FileExtensions,
  Goccia.Keywords.Contextual,
  Goccia.Keywords.Reserved,
  Goccia.Packages.Address,
  Goccia.SourcePipeline,
  Goccia.Token;

const
  { The legacy import-assertion keyword; not a reserved word anywhere else. }
  IDENTIFIER_ASSERT = 'assert';
  PENDING_KEY_SEPARATOR = #0;

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
    (ACursor.IsIdentifier(AIndex + 2, KEYWORD_TYPE) or
     (ACursor.IsType(AIndex + 2, gttString) and
      (ACursor.Tokens[AIndex + 2].Lexeme = KEYWORD_TYPE)));
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
      AddReference(Result, Cursor.Tokens[ASpecifierIndex].Lexeme, crkData)
    else
      AddReference(Result, Cursor.Tokens[ASpecifierIndex].Lexeme, crkModule);
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
                Cursor.IsIdentifier(I + 5, KEYWORD_WITH)) and
               Cursor.IsType(I + 6, gttColon) and
               Cursor.IsType(I + 7, gttLeftBrace) then
              AddReference(Result, Cursor.Tokens[I + 2].Lexeme, crkData)
            else
              AddReference(Result, Cursor.Tokens[I + 2].Lexeme, crkModule);
          end;
        end
        else if Cursor.IsType(I + 1, gttDot) then
        begin
          { import.meta.resolve("./x") }
          if Cursor.IsIdentifier(I + 2, KEYWORD_META) and
             Cursor.IsType(I + 3, gttDot) and
             Cursor.IsIdentifier(I + 4, PROP_RESOLVE) and
             Cursor.IsType(I + 5, gttLeftParen) and
             Cursor.IsType(I + 6, gttString) and
             Cursor.IsType(I + 7, gttRightParen) then
            AddReference(Result, Cursor.Tokens[I + 6].Lexeme, crkAsset);
        end
        else if Cursor.IsType(I + 1, gttString) then
          AddStatic(I + 1)
        else if not Cursor.IsIdentifier(I + 1, KEYWORD_TYPE) or
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
        if Cursor.IsIdentifier(I + 1, CONSTRUCTOR_URL) and
           Cursor.IsType(I + 2, gttLeftParen) and
           Cursor.IsType(I + 3, gttString) and Cursor.IsType(I + 4, gttComma) and
           Cursor.IsType(I + 5, gttImport) and Cursor.IsType(I + 6, gttDot) and
           Cursor.IsIdentifier(I + 7, KEYWORD_META) and
           Cursor.IsType(I + 8, gttDot) and
           Cursor.IsIdentifier(I + 9, PROP_URL) and
           (Cursor.IsType(I + 10, gttRightParen) or
            (Cursor.IsType(I + 10, gttComma) and
             Cursor.IsType(I + 11, gttRightParen))) then
          AddReference(Result, Cursor.Tokens[I + 3].Lexeme, crkAsset);
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
  FFileIndex := TDictionary<string, Integer>.Create;
  FPending := TGocciaPendingReferenceList.Create;
  FQueued := TDictionary<string, Boolean>.Create;
  FAbsent := TDictionary<string, Boolean>.Create;
  FParsed := TDictionary<string, Boolean>.Create;
end;

destructor TGocciaPackageCrawl.Destroy;
begin
  FKnownFiles.Free;
  FParsed.Free;
  FAbsent.Free;
  FQueued.Free;
  FPending.Free;
  FFileIndex.Free;
  FFiles.Free;
  inherited;
end;

procedure TGocciaPackageCrawl.SetKnownFiles(const APaths: array of string);
var
  I: Integer;
begin
  FreeAndNil(FKnownFiles);
  FKnownFiles := TDictionary<string, Boolean>.Create;
  for I := Low(APaths) to High(APaths) do
    FKnownFiles.AddOrSetValue(APaths[I], True);
end;

procedure TGocciaPackageCrawl.AddFile(const APath: string;
  const ABytes: TBytes);
var
  Crawled: TGocciaCrawledFile;
begin
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
  FFileIndex.Add(APath, FFiles.Add(Crawled));
end;

{ Each path and kind is queued once: repeating an import in many modules
  costs nothing more. }
procedure TGocciaPackageCrawl.Enqueue(const APath, ASpecifier: string;
  const AKind: TGocciaReferenceKind; const AFromFile: string);
var
  Key: string;
  Reference: TGocciaPendingReference;
begin
  Key := IntToStr(Ord(AKind)) + PENDING_KEY_SEPARATOR + APath;
  if FQueued.ContainsKey(Key) then
    Exit;
  FQueued.Add(Key, True);
  Reference.Path := APath;
  Reference.Specifier := ASpecifier;
  Reference.Kind := AKind;
  Reference.FromFile := AFromFile;
  FPending.Add(Reference);
end;

procedure TGocciaPackageCrawl.AddModuleEntry(const APath: string);
begin
  Enqueue(APath, APath, crkModule, '');
end;

{ One candidate: a fetch, unless the candidate is already known absent, or
  the known file set settles it. }
function TGocciaPackageCrawl.Fetch(const APath: string;
  out ABytes: TBytes): Boolean;
begin
  ABytes := nil;
  if FAbsent.ContainsKey(APath) then
    Exit(False);
  if Assigned(FKnownFiles) and not FKnownFiles.ContainsKey(APath) then
  begin
    FAbsent.Add(APath, True);
    Exit(False);
  end;
  Inc(FRequests);
  if FRequests > MAX_CRAWL_REQUESTS then
    raise EGocciaCrawlError.CreateFmt('%s needs more than %d requests to ' +
      'find its files; name them with extensions', [FKey,
      MAX_CRAWL_REQUESTS]);
  Result := FFetcher(APath, ABytes);
  if not Result then
    FAbsent.Add(APath, True);
end;

{ Only modules are parsed: a data file or an asset is fetched and pinned,
  whatever its extension, and names nothing. }
procedure TGocciaPackageCrawl.FollowReferences(const APath: string;
  const ABytes: TBytes);
var
  ErrorOffset, I: Integer;
  References: TGocciaLiteralReferences;
  Path, Source: string;
begin
  if FParsed.ContainsKey(APath) then
    Exit;
  FParsed.Add(APath, True);
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
      if References[I].Kind = crkAsset then
        Continue;
      raise EGocciaCrawlError.CreateFmt(
        '%s/%s imports "%s": a package may import only its own files, by ' +
        'relative specifier (no bare, absolute, URL, or github: specifiers)',
        [FKey, APath, References[I].Specifier]);
    end;
    if not TryJoinPackagePath(APath, References[I].Specifier, Path) then
      raise EGocciaCrawlError.CreateFmt(
        '%s/%s names "%s", which leaves the repository',
        [FKey, APath, References[I].Specifier]);
    Enqueue(Path, References[I].Specifier, References[I].Kind, APath);
  end;
end;

procedure TGocciaPackageCrawl.Follow(
  const AReference: TGocciaPendingReference);
var
  Bytes: TBytes;
  Candidates: TGocciaPackagePathArray;
  I, Index: Integer;
begin
  if AReference.Kind = crkModule then
    Candidates := PackageModuleCandidates(AReference.Path,
      EngineModuleImportExtensions)
  else
  begin
    SetLength(Candidates, 1);
    Candidates[0] := AReference.Path;
  end;

  for I := 0 to High(Candidates) do
  begin
    if not IsSafeArtifactPath(Candidates[I]) then
      raise EGocciaCrawlError.CreateFmt(
        '%s: "%s" is not a file a package may contain (a goccia.* file, ' +
        'a .goccia directory, or an unsafe name)', [FKey, Candidates[I]]);
    if FFileIndex.TryGetValue(Candidates[I], Index) then
    begin
      { Deduplicated by path, but a module reference still parses a file
        first reached only as data or an asset. }
      if AReference.Kind = crkModule then
        FollowReferences(Candidates[I], FFiles[Index].Bytes);
      Exit;
    end;
    if Fetch(Candidates[I], Bytes) then
    begin
      AddFile(Candidates[I], Bytes);
      if AReference.Kind = crkModule then
        FollowReferences(Candidates[I], Bytes);
      Exit;
    end;
  end;
  if Assigned(FKnownFiles) then
    raise EGocciaCrawlNeedsFetch.CreateFmt('%s: "%s" is not a known file',
      [FKey, AReference.Path]);
  if AReference.FromFile = '' then
    raise EGocciaCrawlError.CreateFmt('%s has no file for "%s"',
      [FKey, AReference.Specifier])
  else
    raise EGocciaCrawlError.CreateFmt('%s/%s names "%s", which does not ' +
      'exist', [FKey, AReference.FromFile, AReference.Specifier]);
end;

procedure TGocciaPackageCrawl.Run;
var
  Swap: TGocciaCrawledFile;
  I, J: Integer;
begin
  while FNext < FPending.Count do
  begin
    Inc(FNext);
    Follow(FPending[FNext - 1]);
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
  FFileIndex.Clear;
  for I := 0 to FFiles.Count - 1 do
    FFileIndex.Add(FFiles[I].Path, I);
end;

end.
