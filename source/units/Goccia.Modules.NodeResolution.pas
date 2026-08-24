unit Goccia.Modules.NodeResolution;

{$I Goccia.inc}

{ Bare-specifier resolution against `node_modules`.

  This unit is the whole algorithm: splitting a bare specifier into package
  name and subpath, walking ancestor `node_modules` directories, reading the
  package manifest, and picking a target out of the manifest's "exports" map or
  its legacy fields. It performs no extension probing — the caller owns that,
  because the extension candidate list is a resolver policy (`.ts` sources,
  directory `index` files) rather than a package-manifest concern.

  It is deliberately a *subset* of Node's ESM resolver. See
  docs/module-resolution.md for the supported surface, the two deliberate
  deviations (the "module" field, which Node ignores, and the CommonJS refusal),
  and what is not implemented. }

interface

uses
  SysUtils,

  JSONParser;

const
  NODE_MODULES_DIRECTORY_NAME = 'node_modules';
  PACKAGE_MANIFEST_FILE_NAME = 'package.json';
  { The key an `import "pkg"` (no subpath) looks up in an "exports" map. }
  PACKAGE_MAIN_EXPORT_KEY = '.';
  { The only export conditions this resolver understands. Node's default ESM
    condition set is ["node", "import"]; "node" is deliberately absent because
    GocciaScript is not a Node host (see VISION.md). }
  EXPORT_CONDITION_IMPORT = 'import';
  EXPORT_CONDITION_DEFAULT = 'default';
  PACKAGE_TYPE_MODULE = 'module';
  SCOPE_PREFIX_CHARACTER = '@';
  SPECIFIER_SEGMENT_SEPARATOR = '/';
  EXPORT_WILDCARD_CHARACTER = '*';

type
  { A captured "exports" value. Only the shapes the resolver can act on are
    distinguished; numbers and booleans collapse into `enOther` because a
    manifest that puts one in an export target is malformed either way. }
  TGocciaExportsNodeKind = (enString, enObject, enArray, enNull, enOther);

  TGocciaExportsNode = record
    Kind: TGocciaExportsNodeKind;
    Text: string;
    Keys: array of string;
    Children: array of Integer;
  end;

  TGocciaExportsNodes = array of TGocciaExportsNode;

  { How a subpath lookup against a manifest ended. The three outcomes are
    distinct because they lead to different caller behavior: only
    `eoNoExportsField` may fall back to "module"/"main", and `eoNotExported`
    is a deliberate refusal by the package author rather than a missing file. }
  TGocciaExportsOutcome = (
    eoNoExportsField,
    eoResolved,
    eoNotExported,
    { The map matched, but the specifier or the target it selected is one Node
      rejects outright: a `.`/`..`/`node_modules` segment, or a target that
      does not start with "./". Kept apart from eoNotExported because this is a
      malformed or hostile request rather than an author's deliberate refusal. }
    eoInvalidTarget
  );

  { The fields of a `package.json` this resolver reads. Everything else in the
    manifest is skipped during parsing rather than stored. }
  TGocciaPackageManifest = record
    Name: string;
    { The "type" field verbatim: 'module', 'commonjs', or '' when absent. }
    ModuleType: string;
    Main: string;
    { The "module" field — a bundler convention Node itself ignores. }
    ModuleField: string;
    HasExports: Boolean;
    ExportsNodes: TGocciaExportsNodes;
    ExportsRoot: Integer;

    function IsModuleType: Boolean;
    function ResolveExportsSubpath(const ASubpath: string;
      out ATarget: string): TGocciaExportsOutcome;
  end;

{ Splits a bare specifier into its package name and its subpath.

  Returns False for anything that is not a bare package specifier: an empty
  string, a relative or absolute path, a `#`-prefixed private import, a
  protocol-qualified specifier, or a malformed scope. `ASubpath` comes back as
  '.' for a bare package name and './rest' otherwise, matching the keys an
  "exports" map uses. }
function SplitBareSpecifier(const ASpecifier: string;
  out APackageName, ASubpath: string): Boolean;

{ Walks up from AStartDirectory looking for `<dir>/node_modules/<package>`.

  ACeilingDirectory bounds the walk: when it is non-empty, only directories at
  or below it are probed, so a host can pin resolution to one project tree.
  An empty ceiling walks to the filesystem root, which is Node's behavior. }
function FindPackageDirectory(const AStartDirectory, ACeilingDirectory,
  APackageName: string; out APackageDirectory: string): Boolean;

{ Parses the subset of `package.json` this resolver needs. Raises
  EJSONParseError on malformed JSON. }
function ParsePackageManifest(const AText: string): TGocciaPackageManifest;

{ Reads and parses `<APackageDirectory>/package.json`. False when the file is
  absent; parse failures still raise. }
function LoadPackageManifest(const APackageDirectory: string;
  out AManifest: TGocciaPackageManifest): Boolean;

{ Selects the package-relative target for a subpath.

  "exports" decides when present. Without it, the main entry falls back to
  "module" then "main" then `index`, and a subpath is taken literally. Returns
  False only when "exports" exists and refuses the subpath. }
function ResolvePackageSubpath(const AManifest: TGocciaPackageManifest;
  const ASubpath: string; out ATarget: string): Boolean;

{ True when any segment of APath is `.`, `..`, or `node_modules`.

  This is Node's `invalidSegmentRegEx`, `(^|/)(\.\.?|node_modules)(/|$)`, with
  a backslash accepted as a separator too so a Windows-spelled specifier cannot
  slip a segment past the check. Both an exports target and the star value
  substituted into one are validated with it; a package that reaches outside
  itself is malformed, and a specifier that tries to is hostile. }
function HasInvalidPathSegment(const APath: string): Boolean;

{ Whether an exports-map string target is one Node would accept: it must start
  with "./" and carry no invalid segment after that prefix. Node raises
  ERR_INVALID_PACKAGE_TARGET otherwise. }
function IsValidExportsTarget(const ATarget: string): Boolean;

{ True when APath lives strictly beneath ADirectory.

  The comparison is against ADirectory plus a trailing separator, so ADirectory
  itself does NOT pass: this answers "is this file inside the package", and
  every caller passes a resolved file path. A caller that needs the directory
  to count as inside itself must say so explicitly rather than assume it.

  The final containment gate: segment validation rejects the specifiers and
  targets that are invalid on their face, and this catches whatever a
  combination of them still managed to normalize into. Both sides are expanded
  first, so it compares real host paths rather than spellings. }
function IsPathInsideDirectory(const APath, ADirectory: string): Boolean;

{ Heuristic CommonJS detection over module source text.

  True when the source carries CommonJS markers (`require(...)`,
  `module.exports`, `exports.x`) and no ES module markers. The asymmetry is
  deliberate: a file with both is being read as an ES module by every other
  toolchain, and a file with neither is inert and loads fine either way.

  The scan matches raw text and does not tokenize, so an `import` or `export`
  keyword inside a comment or a string literal counts as an ES module marker.
  That direction is the safe one: the file is loaded rather than refused, and
  fails on its own terms at the first `require`. Removing the false negative
  would cost a parse of every resolved package entry. }
function LooksLikeCommonJSSource(const ASource: string): Boolean;

{ Whether a resolved file inside a package must be refused as CommonJS.

  `.mjs`/`.mts` are always ES modules and `.cjs` is always CommonJS, both
  regardless of content. The manifest's "type" field is then consulted
  asymmetrically: `"type": "module"` is trusted and ends the check, while
  `"type": "commonjs"` (or its absence) is not, and the source text decides.
  The asymmetry is what makes the "module"-field fallback usable — those ES
  module builds routinely sit in packages that declare no type or declare
  commonjs for their `main` — and trusting a positive ESM declaration keeps the
  heuristic away from files whose author already answered the question. }
function IsCommonJSModuleFile(const AManifest: TGocciaPackageManifest;
  const AResolvedPath: string): Boolean;

{ The package-relative spelling of a resolved path, used in diagnostics.

  Deliberately relative: ADR 0108 keeps expanded host paths out of every
  message script code can read, and a path inside the package discloses the
  package's own layout rather than the host's. }
function PackageRelativePath(const APackageDirectory,
  AResolvedPath: string): string;

implementation

uses
  StrUtils,

  FileUtils,

  Goccia.FileExtensions,
  Goccia.TextFiles;

const
  RELATIVE_TARGET_PREFIX = './';
  DEFAULT_MAIN_FILE_BASE_NAME = 'index';
  MANIFEST_KEY_NAME = 'name';
  MANIFEST_KEY_TYPE = 'type';
  MANIFEST_KEY_MAIN = 'main';
  MANIFEST_KEY_MODULE = 'module';
  MANIFEST_KEY_EXPORTS = 'exports';
  TOP_LEVEL_DEPTH = 1;
  NO_NODE_INDEX = -1;
  COMMONJS_REQUIRE_NAME = 'require';
  COMMONJS_MODULE_EXPORTS = 'module.exports';
  COMMONJS_EXPORTS_NAME = 'exports';
  ESM_IMPORT_KEYWORD = 'import';
  ESM_EXPORT_KEYWORD = 'export';

type
  { SAX handler that keeps the four scalar fields the resolver reads and
    captures the whole "exports" value as a node tree. Everything else in the
    manifest is discarded as it streams past. }
  TGocciaPackageManifestParser = class(TAbstractJSONParser)
  private
    FDepth: Integer;
    FTopLevelKey: string;
    FCapturing: Boolean;
    FManifest: TGocciaPackageManifest;
    FNodeCount: Integer;
    FStack: array of Integer;
    FPendingKeys: array of string;

    function AppendNode(const AKind: TGocciaExportsNodeKind;
      const AText: string): Integer;
    procedure AttachNode(const AIndex: Integer);
    procedure PushContainer(const AIndex: Integer);
    procedure PopContainer;
    procedure CaptureScalar(const AKind: TGocciaExportsNodeKind;
      const AText: string);
    procedure RecordTopLevelString(const AValue: string);
  protected
    procedure OnNull; override;
    procedure OnBoolean(const AValue: Boolean); override;
    procedure OnString(const AValue: string); override;
    procedure OnInteger(const AValue: Int64); override;
    procedure OnFloat(const AValue: Double); override;
    procedure OnBeginObject; override;
    procedure OnObjectKey(const AKey: string); override;
    procedure OnEndObject; override;
    procedure OnBeginArray; override;
    procedure OnEndArray; override;
  public
    function Parse(const AText: string): TGocciaPackageManifest;
  end;

function IsIdentifierPartCharacter(const ACharacter: Char): Boolean;
begin
  Result := (ACharacter in ['A'..'Z', 'a'..'z', '0'..'9', '_', '$']);
end;

{ ── Manifest parsing ───────────────────────────────────────── }

function TGocciaPackageManifestParser.AppendNode(
  const AKind: TGocciaExportsNodeKind; const AText: string): Integer;
begin
  if FNodeCount >= Length(FManifest.ExportsNodes) then
    SetLength(FManifest.ExportsNodes, FNodeCount * 2 + 8);
  Result := FNodeCount;
  FManifest.ExportsNodes[Result].Kind := AKind;
  FManifest.ExportsNodes[Result].Text := AText;
  SetLength(FManifest.ExportsNodes[Result].Keys, 0);
  SetLength(FManifest.ExportsNodes[Result].Children, 0);
  Inc(FNodeCount);
end;

procedure TGocciaPackageManifestParser.AttachNode(const AIndex: Integer);
var
  ChildCount, ParentIndex, TopIndex: Integer;
begin
  TopIndex := High(FStack);
  if TopIndex < 0 then
  begin
    FManifest.ExportsRoot := AIndex;
    FManifest.HasExports := True;
    Exit;
  end;

  ParentIndex := FStack[TopIndex];
  ChildCount := Length(FManifest.ExportsNodes[ParentIndex].Children);
  SetLength(FManifest.ExportsNodes[ParentIndex].Children, ChildCount + 1);
  FManifest.ExportsNodes[ParentIndex].Children[ChildCount] := AIndex;
  SetLength(FManifest.ExportsNodes[ParentIndex].Keys, ChildCount + 1);
  FManifest.ExportsNodes[ParentIndex].Keys[ChildCount] := FPendingKeys[TopIndex];
  FPendingKeys[TopIndex] := '';
end;

procedure TGocciaPackageManifestParser.PushContainer(const AIndex: Integer);
begin
  SetLength(FStack, Length(FStack) + 1);
  FStack[High(FStack)] := AIndex;
  SetLength(FPendingKeys, Length(FStack));
  FPendingKeys[High(FPendingKeys)] := '';
end;

procedure TGocciaPackageManifestParser.PopContainer;
begin
  if Length(FStack) = 0 then
    Exit;
  SetLength(FStack, Length(FStack) - 1);
  SetLength(FPendingKeys, Length(FStack));
  if Length(FStack) = 0 then
    FCapturing := False;
end;

procedure TGocciaPackageManifestParser.CaptureScalar(
  const AKind: TGocciaExportsNodeKind; const AText: string);
begin
  AttachNode(AppendNode(AKind, AText));
  if Length(FStack) = 0 then
    FCapturing := False;
end;

procedure TGocciaPackageManifestParser.RecordTopLevelString(
  const AValue: string);
begin
  if FTopLevelKey = MANIFEST_KEY_NAME then
    FManifest.Name := AValue
  else if FTopLevelKey = MANIFEST_KEY_TYPE then
    FManifest.ModuleType := AValue
  else if FTopLevelKey = MANIFEST_KEY_MAIN then
    FManifest.Main := AValue
  else if FTopLevelKey = MANIFEST_KEY_MODULE then
    FManifest.ModuleField := AValue;
end;

procedure TGocciaPackageManifestParser.OnNull;
begin
  if FCapturing then
    CaptureScalar(enNull, '');
end;

procedure TGocciaPackageManifestParser.OnBoolean(const AValue: Boolean);
begin
  if FCapturing then
    CaptureScalar(enOther, '');
end;

procedure TGocciaPackageManifestParser.OnString(const AValue: string);
begin
  if FCapturing then
    CaptureScalar(enString, AValue)
  else if FDepth = TOP_LEVEL_DEPTH then
    RecordTopLevelString(AValue);
end;

procedure TGocciaPackageManifestParser.OnInteger(const AValue: Int64);
begin
  if FCapturing then
    CaptureScalar(enOther, '');
end;

procedure TGocciaPackageManifestParser.OnFloat(const AValue: Double);
begin
  if FCapturing then
    CaptureScalar(enOther, '');
end;

procedure TGocciaPackageManifestParser.OnBeginObject;
var
  NodeIndex: Integer;
begin
  Inc(FDepth);
  if not FCapturing then
    Exit;
  NodeIndex := AppendNode(enObject, '');
  AttachNode(NodeIndex);
  PushContainer(NodeIndex);
end;

procedure TGocciaPackageManifestParser.OnObjectKey(const AKey: string);
begin
  if FCapturing then
  begin
    if Length(FPendingKeys) > 0 then
      FPendingKeys[High(FPendingKeys)] := AKey;
    Exit;
  end;

  if FDepth <> TOP_LEVEL_DEPTH then
    Exit;

  FTopLevelKey := AKey;
  if AKey = MANIFEST_KEY_EXPORTS then
  begin
    { A later duplicate "exports" wins, matching how a JSON object with a
      repeated key collapses in every JavaScript parser. }
    FCapturing := True;
    FNodeCount := 0;
    FManifest.ExportsRoot := NO_NODE_INDEX;
    FManifest.HasExports := False;
    SetLength(FStack, 0);
    SetLength(FPendingKeys, 0);
  end;
end;

procedure TGocciaPackageManifestParser.OnEndObject;
begin
  if FCapturing then
    PopContainer;
  Dec(FDepth);
end;

procedure TGocciaPackageManifestParser.OnBeginArray;
var
  NodeIndex: Integer;
begin
  Inc(FDepth);
  if not FCapturing then
    Exit;
  NodeIndex := AppendNode(enArray, '');
  AttachNode(NodeIndex);
  PushContainer(NodeIndex);
end;

procedure TGocciaPackageManifestParser.OnEndArray;
begin
  if FCapturing then
    PopContainer;
  Dec(FDepth);
end;

function TGocciaPackageManifestParser.Parse(
  const AText: string): TGocciaPackageManifest;
begin
  FDepth := 0;
  FTopLevelKey := '';
  FCapturing := False;
  FNodeCount := 0;
  SetLength(FStack, 0);
  SetLength(FPendingKeys, 0);

  FManifest.Name := '';
  FManifest.ModuleType := '';
  FManifest.Main := '';
  FManifest.ModuleField := '';
  FManifest.HasExports := False;
  FManifest.ExportsRoot := NO_NODE_INDEX;
  SetLength(FManifest.ExportsNodes, 0);

  DoParse(AText);

  SetLength(FManifest.ExportsNodes, FNodeCount);
  Result := FManifest;
end;

function ParsePackageManifest(const AText: string): TGocciaPackageManifest;
var
  Parser: TGocciaPackageManifestParser;
begin
  Parser := TGocciaPackageManifestParser.Create;
  try
    Result := Parser.Parse(AText);
  finally
    Parser.Free;
  end;
end;

function LoadPackageManifest(const APackageDirectory: string;
  out AManifest: TGocciaPackageManifest): Boolean;
var
  ManifestPath: string;
begin
  ManifestPath := IncludeTrailingPathDelimiter(APackageDirectory) +
    PACKAGE_MANIFEST_FILE_NAME;
  Result := HostFileExists(ManifestPath);
  if not Result then
    Exit;
  AManifest := ParsePackageManifest(ReadUTF8FileText(ManifestPath));
end;

{ ── Exports map ────────────────────────────────────────────── }

function TGocciaPackageManifest.IsModuleType: Boolean;
begin
  Result := ModuleType = PACKAGE_TYPE_MODULE;
end;

function IsSubpathKey(const AKey: string): Boolean;
begin
  Result := (AKey <> '') and (AKey[1] = '.');
end;

function IsSubpathExportsMap(const ANode: TGocciaExportsNode): Boolean;
var
  I: Integer;
begin
  for I := 0 to High(ANode.Keys) do
    if IsSubpathKey(ANode.Keys[I]) then
      Exit(True);
  Result := False;
end;

function SubstituteWildcard(const ATarget, AStarValue: string): string;
begin
  Result := StringReplace(ATarget, EXPORT_WILDCARD_CHARACTER, AStarValue,
    [rfReplaceAll]);
end;

function HasInvalidPathSegment(const APath: string): Boolean;
var
  Segment: string;
  SegmentStart, I, PathLength: Integer;

  function SegmentIsInvalid(const ASegment: string): Boolean;
  begin
    Result := (ASegment = '.') or (ASegment = '..') or
      (LowerCase(ASegment) = NODE_MODULES_DIRECTORY_NAME);
  end;

begin
  PathLength := Length(APath);
  SegmentStart := 1;
  for I := 1 to PathLength do
    if APath[I] in [SPECIFIER_SEGMENT_SEPARATOR, '\'] then
    begin
      Segment := Copy(APath, SegmentStart, I - SegmentStart);
      if SegmentIsInvalid(Segment) then
        Exit(True);
      SegmentStart := I + 1;
    end;

  Result := SegmentIsInvalid(Copy(APath, SegmentStart,
    PathLength - SegmentStart + 1));
end;

function IsValidExportsTarget(const ATarget: string): Boolean;
begin
  if Copy(ATarget, 1, Length(RELATIVE_TARGET_PREFIX)) <>
     RELATIVE_TARGET_PREFIX then
    Exit(False);
  Result := not HasInvalidPathSegment(
    Copy(ATarget, Length(RELATIVE_TARGET_PREFIX) + 1, MaxInt));
end;

function IsPathInsideDirectory(const APath, ADirectory: string): Boolean;
var
  Directory, Path: string;
begin
  if (APath = '') or (ADirectory = '') then
    Exit(False);
  Path := ExpandHostFileName(APath);
  Directory := IncludeTrailingPathDelimiter(ExpandHostFileName(ADirectory));
  Result := Copy(Path, 1, Length(Directory)) = Directory;
end;

{ Walks a condition value — a string, a nested condition object, or an array of
  fallbacks — and returns the first target the supported conditions select. }
function ResolveConditionalTarget(const ANodes: TGocciaExportsNodes;
  const AIndex: Integer; const AStarValue: string;
  out ATarget: string): Boolean;
var
  I: Integer;
  Key: string;
begin
  ATarget := '';
  if (AIndex < 0) or (AIndex > High(ANodes)) then
    Exit(False);

  case ANodes[AIndex].Kind of
    enString:
      begin
        { Validate the authored target before substitution, so a target that
          reaches outside the package is rejected on its own terms rather than
          being judged by whatever the star value happened to make of it. }
        if not IsValidExportsTarget(ANodes[AIndex].Text) then
          Exit(False);
        ATarget := SubstituteWildcard(ANodes[AIndex].Text, AStarValue);
        Result := ATarget <> '';
      end;
    enArray:
      begin
        for I := 0 to High(ANodes[AIndex].Children) do
          if ResolveConditionalTarget(ANodes, ANodes[AIndex].Children[I],
            AStarValue, ATarget) then
            Exit(True);
        Result := False;
      end;
    enObject:
      begin
        for I := 0 to High(ANodes[AIndex].Children) do
        begin
          Key := ANodes[AIndex].Keys[I];
          if (Key <> EXPORT_CONDITION_IMPORT) and
             (Key <> EXPORT_CONDITION_DEFAULT) then
            Continue;
          if ResolveConditionalTarget(ANodes, ANodes[AIndex].Children[I],
            AStarValue, ATarget) then
            Exit(True);
        end;
        Result := False;
      end;
  else
    { enNull is an author's explicit block; enOther is malformed. }
    Result := False;
  end;
end;

{ Node's PACKAGE_EXPORTS_RESOLVE ranks pattern keys by the length of the text
  before the '*', then by the length of the text after it. }
function IsMoreSpecificPattern(const ACandidateBase, ACandidateTrailer,
  ABestBase, ABestTrailer: string): Boolean;
begin
  if Length(ACandidateBase) <> Length(ABestBase) then
    Exit(Length(ACandidateBase) > Length(ABestBase));
  Result := Length(ACandidateTrailer) > Length(ABestTrailer);
end;

function TryMatchPatternKey(const AKey, ASubpath: string;
  out ABase, ATrailer, AStarValue: string): Boolean;
var
  StarIndex: Integer;
begin
  ABase := '';
  ATrailer := '';
  AStarValue := '';

  StarIndex := Pos(EXPORT_WILDCARD_CHARACTER, AKey);
  if StarIndex = 0 then
    Exit(False);

  ABase := Copy(AKey, 1, StarIndex - 1);
  ATrailer := Copy(AKey, StarIndex + 1, MaxInt);
  { A second '*' is not a pattern Node recognises, so the key never matches. }
  if Pos(EXPORT_WILDCARD_CHARACTER, ATrailer) > 0 then
    Exit(False);

  if Copy(ASubpath, 1, Length(ABase)) <> ABase then
    Exit(False);
  if Length(ASubpath) < Length(ABase) + Length(ATrailer) then
    Exit(False);
  if (ATrailer <> '') and
     (Copy(ASubpath, Length(ASubpath) - Length(ATrailer) + 1, MaxInt) <>
      ATrailer) then
    Exit(False);

  AStarValue := Copy(ASubpath, Length(ABase) + 1,
    Length(ASubpath) - Length(ABase) - Length(ATrailer));
  Result := True;
end;

function TGocciaPackageManifest.ResolveExportsSubpath(const ASubpath: string;
  out ATarget: string): TGocciaExportsOutcome;
var
  Base, BestBase, BestTrailer, BestStarValue, StarValue, Trailer: string;
  BestIndex, I: Integer;
  RootNode: TGocciaExportsNode;
begin
  ATarget := '';
  if (not HasExports) or (ExportsRoot < 0) then
    Exit(eoNoExportsField);

  RootNode := ExportsNodes[ExportsRoot];

  { A plain string "exports", and an "exports" that is a bare condition object
    with no subpath keys, are both main-entry-only sugar. }
  if (RootNode.Kind <> enObject) or (not IsSubpathExportsMap(RootNode)) then
  begin
    if ASubpath <> PACKAGE_MAIN_EXPORT_KEY then
      Exit(eoNotExported);
    if ResolveConditionalTarget(ExportsNodes, ExportsRoot, '', ATarget) then
      Exit(eoResolved);
    Exit(eoNotExported);
  end;

  for I := 0 to High(RootNode.Keys) do
    if RootNode.Keys[I] = ASubpath then
    begin
      if ResolveConditionalTarget(ExportsNodes, RootNode.Children[I], '',
        ATarget) then
        Exit(eoResolved);
      Exit(eoNotExported);
    end;

  BestIndex := NO_NODE_INDEX;
  BestBase := '';
  BestTrailer := '';
  BestStarValue := '';
  for I := 0 to High(RootNode.Keys) do
  begin
    if not TryMatchPatternKey(RootNode.Keys[I], ASubpath, Base, Trailer,
      StarValue) then
      Continue;
    if (BestIndex >= 0) and
       (not IsMoreSpecificPattern(Base, Trailer, BestBase, BestTrailer)) then
      Continue;
    BestIndex := RootNode.Children[I];
    BestBase := Base;
    BestTrailer := Trailer;
    BestStarValue := StarValue;
  end;

  if BestIndex < 0 then
    Exit(eoNotExported);
  { The star value comes from the importing specifier, so it is the one part of
    a pattern resolution an attacker controls. Node raises
    ERR_INVALID_MODULE_SPECIFIER for it rather than falling through to another
    pattern, and so does this: the request is refused, not re-matched. }
  if HasInvalidPathSegment(BestStarValue) then
    Exit(eoInvalidTarget);
  if ResolveConditionalTarget(ExportsNodes, BestIndex, BestStarValue,
    ATarget) then
    Exit(eoResolved);
  Result := eoNotExported;
end;

function ResolvePackageSubpath(const AManifest: TGocciaPackageManifest;
  const ASubpath: string; out ATarget: string): Boolean;
begin
  case AManifest.ResolveExportsSubpath(ASubpath, ATarget) of
    eoResolved: Exit(True);
    eoNotExported, eoInvalidTarget: Exit(False);
  end;

  if ASubpath <> PACKAGE_MAIN_EXPORT_KEY then
  begin
    { The legacy path takes the subpath literally, which is exactly how a
      specifier like `pkg/../../evil.js` walked out of the package before this
      check existed. `..` in an import subpath is never legitimate. }
    if HasInvalidPathSegment(Copy(ASubpath,
      Length(RELATIVE_TARGET_PREFIX) + 1, MaxInt)) then
      Exit(False);
    ATarget := ASubpath;
    Exit(True);
  end;

  { Deviation from Node, which ignores "module" entirely: GocciaScript loads
    only ES modules, and a package without an exports map that ships both
    fields keeps its ES module build behind "module". }
  if AManifest.ModuleField <> '' then
    ATarget := AManifest.ModuleField
  else if AManifest.Main <> '' then
    ATarget := AManifest.Main
  else
    ATarget := RELATIVE_TARGET_PREFIX + DEFAULT_MAIN_FILE_BASE_NAME;
  Result := True;
end;

{ ── Specifier splitting and the ancestor walk ──────────────── }

function SplitBareSpecifier(const ASpecifier: string;
  out APackageName, ASubpath: string): Boolean;
var
  SeparatorIndex, SegmentsNeeded: Integer;
begin
  APackageName := '';
  ASubpath := '';
  if ASpecifier = '' then
    Exit(False);
  if ASpecifier[1] in ['.', '/', '\', '#'] then
    Exit(False);
  { A protocol-qualified specifier (`node:fs`, `https://…`) is never a package
    directory name. }
  if Pos(':', ASpecifier) > 0 then
    Exit(False);

  if ASpecifier[1] = SCOPE_PREFIX_CHARACTER then
    SegmentsNeeded := 2
  else
    SegmentsNeeded := 1;

  SeparatorIndex := 0;
  repeat
    SeparatorIndex := PosEx(SPECIFIER_SEGMENT_SEPARATOR, ASpecifier,
      SeparatorIndex + 1);
    Dec(SegmentsNeeded);
  until (SegmentsNeeded = 0) or (SeparatorIndex = 0);

  if SegmentsNeeded > 0 then
    { A scoped specifier without its second segment (`@scope`) names no
      package. }
    Exit(False);

  if SeparatorIndex = 0 then
  begin
    APackageName := ASpecifier;
    ASubpath := PACKAGE_MAIN_EXPORT_KEY;
  end
  else
  begin
    APackageName := Copy(ASpecifier, 1, SeparatorIndex - 1);
    ASubpath := RELATIVE_TARGET_PREFIX +
      Copy(ASpecifier, SeparatorIndex + 1, MaxInt);
  end;

  Result := (APackageName <> '') and
    (ASubpath <> RELATIVE_TARGET_PREFIX);
end;

function ToHostRelativePath(const APath: string): string;
begin
  Result := StringReplace(APath, SPECIFIER_SEGMENT_SEPARATOR, PathDelim,
    [rfReplaceAll]);
end;

function IsWithinDirectory(const ADirectory, ACeiling: string): Boolean;
var
  Ceiling: string;
begin
  if ACeiling = '' then
    Exit(True);
  Ceiling := IncludeTrailingPathDelimiter(ACeiling);
  Result := Copy(IncludeTrailingPathDelimiter(ADirectory), 1,
    Length(Ceiling)) = Ceiling;
end;

function FindPackageDirectory(const AStartDirectory, ACeilingDirectory,
  APackageName: string; out APackageDirectory: string): Boolean;
var
  Candidate, Ceiling, CurrentDirectory, ParentDirectory, RelativeName: string;
begin
  APackageDirectory := '';
  if (AStartDirectory = '') or (APackageName = '') then
    Exit(False);

  RelativeName := ToHostRelativePath(APackageName);
  CurrentDirectory := ExcludeTrailingPathDelimiter(
    ExpandHostFileName(AStartDirectory));
  if ACeilingDirectory <> '' then
    Ceiling := ExcludeTrailingPathDelimiter(
      ExpandHostFileName(ACeilingDirectory))
  else
    Ceiling := '';

  while CurrentDirectory <> '' do
  begin
    if IsWithinDirectory(CurrentDirectory, Ceiling) and
       (ExtractFileName(CurrentDirectory) <> NODE_MODULES_DIRECTORY_NAME) then
    begin
      Candidate := IncludeTrailingPathDelimiter(CurrentDirectory) +
        NODE_MODULES_DIRECTORY_NAME + PathDelim + RelativeName;
      if HostDirectoryExists(Candidate) then
      begin
        APackageDirectory := Candidate;
        Exit(True);
      end;
    end;

    if (Ceiling <> '') and (CurrentDirectory = Ceiling) then
      Break;

    ParentDirectory := ExtractFileDir(CurrentDirectory);
    if (ParentDirectory = '') or (ParentDirectory = CurrentDirectory) then
      Break;
    CurrentDirectory := ParentDirectory;
  end;

  Result := False;
end;

{ ── CommonJS refusal ───────────────────────────────────────── }

function ContainsKeywordBefore(const ASource, AKeyword: string;
  const AFollowers: TSysCharSet): Boolean;
var
  Index, KeywordLength, SourceLength: Integer;
begin
  KeywordLength := Length(AKeyword);
  SourceLength := Length(ASource);
  Index := PosEx(AKeyword, ASource, 1);
  while Index > 0 do
  begin
    if ((Index = 1) or (not IsIdentifierPartCharacter(ASource[Index - 1]))) and
       (Index + KeywordLength <= SourceLength) and
       (ASource[Index + KeywordLength] in AFollowers) then
      Exit(True);
    Index := PosEx(AKeyword, ASource, Index + 1);
  end;
  Result := False;
end;

function ContainsCallTo(const ASource, AName: string): Boolean;
var
  Index, NameLength, Scan, SourceLength: Integer;
begin
  NameLength := Length(AName);
  SourceLength := Length(ASource);
  Index := PosEx(AName, ASource, 1);
  while Index > 0 do
  begin
    if (Index = 1) or (not IsIdentifierPartCharacter(ASource[Index - 1])) then
    begin
      Scan := Index + NameLength;
      while (Scan <= SourceLength) and (ASource[Scan] in [' ', #9, #13, #10]) do
        Inc(Scan);
      if (Scan <= SourceLength) and (ASource[Scan] = '(') then
        Exit(True);
    end;
    Index := PosEx(AName, ASource, Index + 1);
  end;
  Result := False;
end;

function ContainsMemberAccess(const ASource, AObjectName: string): Boolean;
begin
  Result := ContainsKeywordBefore(ASource, AObjectName, ['.', '[']);
end;

function LooksLikeESModuleSource(const ASource: string): Boolean;
begin
  { `import(` is dynamic import, which CommonJS files use too, so a bare '('
    after the keyword is not evidence either way. }
  Result := ContainsKeywordBefore(ASource, ESM_IMPORT_KEYWORD,
      [' ', #9, #13, #10, '{', '*', '"', '''']) or
    ContainsKeywordBefore(ASource, ESM_EXPORT_KEYWORD,
      [' ', #9, #13, #10, '{', '*']);
end;

function LooksLikeCommonJSSource(const ASource: string): Boolean;
begin
  Result := (ContainsCallTo(ASource, COMMONJS_REQUIRE_NAME) or
    (Pos(COMMONJS_MODULE_EXPORTS, ASource) > 0) or
    ContainsMemberAccess(ASource, COMMONJS_EXPORTS_NAME)) and
    (not LooksLikeESModuleSource(ASource));
end;

function IsCommonJSModuleFile(const AManifest: TGocciaPackageManifest;
  const AResolvedPath: string): Boolean;
var
  Extension: string;
begin
  Extension := LowerCase(ExtractFileExt(AResolvedPath));
  if (Extension = EXT_MJS) or (Extension = EXT_MTS) then
    Exit(False);
  if Extension = EXT_CJS then
    Exit(True);
  { An explicit `"type": "module"` is the author's declaration and outranks any
    heuristic; only a package that declares nothing (or declares commonjs) is
    decided by reading the file. }
  if AManifest.IsModuleType then
    Exit(False);
  { Structured-data and text assets are not JavaScript at all. }
  if not IsScriptExtension(Extension) then
    Exit(False);
  if not HostFileExists(AResolvedPath) then
    Exit(False);

  try
    Result := LooksLikeCommonJSSource(ReadUTF8FileText(AResolvedPath));
  except
    { An unreadable or non-UTF-8 file is not evidence of CommonJS. Let the
      loader fail on its own terms rather than turning an encoding problem into
      a module-format accusation. }
    on Exception do
      Result := False;
  end;
end;

function PackageRelativePath(const APackageDirectory,
  AResolvedPath: string): string;
var
  Directory: string;
begin
  Directory := IncludeTrailingPathDelimiter(
    ExpandHostFileName(APackageDirectory));
  if Copy(AResolvedPath, 1, Length(Directory)) = Directory then
    Result := Copy(AResolvedPath, Length(Directory) + 1, MaxInt)
  else
    Result := ExtractFileName(AResolvedPath);
  Result := StringReplace(Result, PathDelim, SPECIFIER_SEGMENT_SEPARATOR,
    [rfReplaceAll]);
end;

end.
