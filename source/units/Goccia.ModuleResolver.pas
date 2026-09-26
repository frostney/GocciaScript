unit Goccia.ModuleResolver;

{$I Goccia.inc}

interface

uses
  SysUtils,

  OrderedStringMap;

const
  { Script-visible resolution failures name the specifier exactly as the import
    statement wrote it. The expanded host candidate never enters the message —
    it travels in EModuleNotFound.ResolvedCandidatePath instead (ADR 0108). }
  MODULE_NOT_FOUND_MESSAGE_FORMAT = 'Module not found: "%s"';

  { Raised for a bare specifier when node_modules resolution is not enabled.
    Names no host path, so ADR 0108 leaves it as-is. }
  BARE_SPECIFIER_MESSAGE_FORMAT =
    'Cannot resolve bare module specifier "%s". Imports must start with "./" or "../"';

  { The path in this message is the package-relative one. It describes the
    package's own layout, never the host directory the package was found in,
    so it stays inside the ADR 0108 boundary; the expanded path still travels
    in ResolvedCandidatePath for host reporters. }
  COMMONJS_MODULE_MESSAGE_FORMAT =
    'Package "%s" resolved to a CommonJS file (%s); GocciaScript loads only ES modules';

type
  TModuleResolverExtensionArray = array of string;

  { Asked once per bare specifier before any node_modules directory is
    probed. Answers whether the walk may run for an importer in
    AImportingDirectory and, when it may, the highest directory it may reach
    (ACeiling, empty = unbounded). The engine implements it from its capability
    set; a resolver with no callback stays sealed. May raise to refuse. }
  TModuleResolverNodeModulesGrant = function(const ASpecifier,
    AImportingDirectory: string; out ACeiling: string): Boolean of object;

  { Called with each host path the resolver is about to test for existence,
    in probe order, before it tests it. May raise to stop resolution; a host
    uses it to refuse a candidate it may not read without learning whether
    that file exists. }
  TModuleResolverProbeGuard = procedure(const ACandidatePath: string) of object;

  TModuleResolver = class
  private
    FAliases: TStringStringMap;
    FBaseDirectory: string;
    FExtensions: TModuleResolverExtensionArray;
    FNodeModulesGrant: TModuleResolverNodeModulesGrant;
    FProbeGuard: TModuleResolverProbeGuard;
    FLastPackageDirectory: string;
  protected
    function ProbeHostFile(const APath: string): Boolean;
    function ApplyAliases(const AModulePath, AImportingFilePath: string): string;
    function TryResolveWithExtensions(const ABasePath: string; out AResolvedPath: string): Boolean;
    { Resolves a bare specifier against node_modules. The base implementation
      is the host-filesystem one and returns False unless NodeModulesGrant
      grants the walk; resolvers over a different filesystem override it. }
    function TryResolveBareSpecifier(const AModulePath, AImportingFilePath: string;
      out AResolvedPath: string): Boolean; virtual;
  public
    constructor Create(const ABaseDirectory: string = '');
    destructor Destroy; override;

    procedure AddAlias(const APattern, AReplacement: string);
    function ApplyAlias(const AModulePath,
      AImportingFilePath: string): string;
    function GetExtensions: TModuleResolverExtensionArray;
    function HasAlias(const AModulePath: string): Boolean;
    procedure SetExtensions(const AExtensions: array of string);
    function Resolve(const AModulePath, AImportingFilePath: string): string; virtual;

    property Aliases: TStringStringMap read FAliases;
    property BaseDirectory: string read FBaseDirectory write FBaseDirectory;
    property NodeModulesGrant: TModuleResolverNodeModulesGrant
      read FNodeModulesGrant write FNodeModulesGrant;
    property ProbeGuard: TModuleResolverProbeGuard
      read FProbeGuard write FProbeGuard;
    { The package directory the last Resolve found a bare specifier in, as
      the node_modules walk spelled it; empty when that resolution did not go
      through node_modules. }
    property LastPackageDirectory: string read FLastPackageDirectory;
  end;

  { Raised when a specifier cannot be resolved. Message is safe to hand to
    untrusted script code; ResolvedCandidatePath is host-only diagnostic
    detail and is empty unless the constructor supplied one. }
  EModuleNotFound = class(Exception)
  private
    FResolvedCandidatePath: string;
  public
    constructor CreateNotFound(const ASpecifier, AResolvedCandidatePath: string);
    constructor CreateWithCandidate(const AMessage,
      AResolvedCandidatePath: string);

    property ResolvedCandidatePath: string read FResolvedCandidatePath;
  end;

  { A node_modules specifier that resolved to a file GocciaScript will not
    load. Distinct from a missing module so a host can tell "the package is not
    installed" from "the package is CommonJS", and so the failure never reaches
    the parser as a SyntaxError. }
  EModuleIsCommonJS = class(EModuleNotFound)
  public
    constructor CreateCommonJS(const APackageName, APackageRelativePath,
      AResolvedCandidatePath: string);
  end;

implementation

uses
  FileUtils,

  Goccia.FileExtensions,
  Goccia.Modules.NodeResolution;

const
  ALIAS_SEGMENT_DELIMITER = '/';
  CURRENT_DIRECTORY_PREFIX = './';
  PARENT_DIRECTORY_PREFIX = '../';

function HasTrailingPathDelimiter(const APath: string): Boolean;
begin
  Result := (APath <> '') and (APath[Length(APath)] = PathDelim);
end;

function EnsureTrailingPathDelimiterIfNeeded(const APath: string;
  const ANeedsTrailingDelimiter: Boolean): string;
begin
  Result := APath;
  if ANeedsTrailingDelimiter and not HasTrailingPathDelimiter(Result) then
    Result := Result + PathDelim;
end;

function IsAbsolutePath(const APath: string): Boolean;
begin
  if Length(APath) = 0 then
    Exit(False);
  if APath[1] = '/' then
    Exit(True);
  if (Length(APath) >= 2) and (APath[2] = ':') then
    Exit(True);
  Result := Copy(APath, 1, 2) = '\\';
end;

function IsRelativeModuleSpecifier(const AModulePath: string): Boolean;
begin
  Result := (Copy(AModulePath, 1, Length(CURRENT_DIRECTORY_PREFIX)) =
      CURRENT_DIRECTORY_PREFIX) or
    (Copy(AModulePath, 1, Length(PARENT_DIRECTORY_PREFIX)) =
      PARENT_DIRECTORY_PREFIX);
end;

function IsURLLikeModuleSpecifier(const AModulePath: string): Boolean;
begin
  Result := IsAbsolutePath(AModulePath) or IsRelativeModuleSpecifier(AModulePath);
end;

function IsPrefixAlias(const AAlias: string): Boolean;
begin
  Result := (AAlias <> '') and (AAlias[Length(AAlias)] = ALIAS_SEGMENT_DELIMITER);
end;

function NormalizeSpecifierForMatching(const AModulePath,
  AImportingFilePath: string): string;
var
  BaseDirectory: string;
begin
  if not IsURLLikeModuleSpecifier(AModulePath) then
    Exit(AModulePath);

  if IsAbsolutePath(AModulePath) then
    Result := ExpandHostFileName(AModulePath)
  else
  begin
    BaseDirectory := ExtractFilePath(AImportingFilePath);
    if BaseDirectory = '' then
      BaseDirectory := GetCurrentDir + PathDelim;
    Result := ExpandHostFileName(BaseDirectory + AModulePath);
  end;

  Result := EnsureTrailingPathDelimiterIfNeeded(Result, IsPrefixAlias(AModulePath));
end;

function AliasMatchesModulePath(const AAlias, AModulePath,
  AImportingFilePath: string): Boolean;
var
  MatchPath: string;
begin
  if AAlias = '' then
    Exit(False);

  if IsURLLikeModuleSpecifier(AAlias) then
    MatchPath := NormalizeSpecifierForMatching(AModulePath, AImportingFilePath)
  else
    MatchPath := AModulePath;

  if IsPrefixAlias(AAlias) then
    Result := Copy(MatchPath, 1, Length(AAlias)) = AAlias
  else
    Result := MatchPath = AAlias;
end;

constructor EModuleNotFound.CreateNotFound(const ASpecifier,
  AResolvedCandidatePath: string);
begin
  inherited CreateFmt(MODULE_NOT_FOUND_MESSAGE_FORMAT, [ASpecifier]);
  FResolvedCandidatePath := AResolvedCandidatePath;
end;

constructor EModuleNotFound.CreateWithCandidate(const AMessage,
  AResolvedCandidatePath: string);
begin
  inherited Create(AMessage);
  FResolvedCandidatePath := AResolvedCandidatePath;
end;

constructor EModuleIsCommonJS.CreateCommonJS(const APackageName,
  APackageRelativePath, AResolvedCandidatePath: string);
begin
  inherited CreateWithCandidate(Format(COMMONJS_MODULE_MESSAGE_FORMAT,
    [APackageName, APackageRelativePath]), AResolvedCandidatePath);
end;

constructor TModuleResolver.Create(const ABaseDirectory: string);
begin
  FAliases := TStringStringMap.Create;
  SetLength(FExtensions, 0);
  if ABaseDirectory <> '' then
    FBaseDirectory := IncludeTrailingPathDelimiter(ExpandHostFileName(ABaseDirectory))
  else
    FBaseDirectory := IncludeTrailingPathDelimiter(GetCurrentDir);
end;

destructor TModuleResolver.Destroy;
begin
  FAliases.Free;
  inherited;
end;

procedure TModuleResolver.AddAlias(const APattern, AReplacement: string);
begin
  FAliases.AddOrSetValue(APattern, AReplacement);
end;

function TModuleResolver.ApplyAlias(const AModulePath,
  AImportingFilePath: string): string;
begin
  Result := ApplyAliases(AModulePath, AImportingFilePath);
end;

function TModuleResolver.GetExtensions: TModuleResolverExtensionArray;
var
  I: Integer;
begin
  SetLength(Result, Length(FExtensions));
  for I := 0 to High(FExtensions) do
    Result[I] := FExtensions[I];
end;

function TModuleResolver.HasAlias(const AModulePath: string): Boolean;
var
  Pair: TStringStringMap.TKeyValuePair;
begin
  for Pair in FAliases do
    if AliasMatchesModulePath(Pair.Key, AModulePath, '') then
      Exit(True);
  Result := False;
end;

procedure TModuleResolver.SetExtensions(const AExtensions: array of string);
var
  I: Integer;
begin
  SetLength(FExtensions, Length(AExtensions));
  for I := 0 to High(AExtensions) do
    FExtensions[I] := AExtensions[I];
end;

function TModuleResolver.ApplyAliases(const AModulePath,
  AImportingFilePath: string): string;
var
  Pair: TStringStringMap.TKeyValuePair;
  BestKey, BestValue, MatchPath, Replacement: string;
  Found: Boolean;
begin
  Result := AModulePath;
  Found := False;

  for Pair in FAliases do
  begin
    if AliasMatchesModulePath(Pair.Key, AModulePath, AImportingFilePath) then
    begin
      if (not Found) or (Length(Pair.Key) > Length(BestKey)) then
      begin
        BestKey := Pair.Key;
        BestValue := Pair.Value;
        Found := True;
      end;
    end;
  end;

  if Found then
  begin
    if IsURLLikeModuleSpecifier(BestKey) then
      MatchPath := NormalizeSpecifierForMatching(AModulePath, AImportingFilePath)
    else
      MatchPath := AModulePath;

    if IsPrefixAlias(BestKey) then
      Replacement := BestValue + Copy(MatchPath, Length(BestKey) + 1, MaxInt)
    else
      Replacement := BestValue;

    if not IsAbsolutePath(Replacement) then
      Result := FBaseDirectory + Replacement
    else
      Result := Replacement;
  end;
end;

function TModuleResolver.ProbeHostFile(const APath: string): Boolean;
begin
  if Assigned(FProbeGuard) then
    FProbeGuard(APath);
  Result := HostFileExists(APath);
end;

function TModuleResolver.TryResolveWithExtensions(const ABasePath: string; out AResolvedPath: string): Boolean;
var
  I: Integer;
  TypeScriptCandidates: TFileExtensionArray;
begin
  if ProbeHostFile(ABasePath) then
  begin
    AResolvedPath := ABasePath;
    Exit(True);
  end;

  TypeScriptCandidates := TypeScriptSourceCandidates(ABasePath);
  for I := 0 to High(TypeScriptCandidates) do
  begin
    if ProbeHostFile(TypeScriptCandidates[I]) then
    begin
      AResolvedPath := TypeScriptCandidates[I];
      Exit(True);
    end;
  end;

  for I := 0 to High(FExtensions) do
  begin
    if ProbeHostFile(ABasePath + FExtensions[I]) then
    begin
      AResolvedPath := ABasePath + FExtensions[I];
      Exit(True);
    end;
  end;

  for I := 0 to High(FExtensions) do
  begin
    if ProbeHostFile(ABasePath + PathDelim + 'index' + FExtensions[I]) then
    begin
      AResolvedPath := ABasePath + PathDelim + 'index' + FExtensions[I];
      Exit(True);
    end;
  end;

  Result := False;
end;

function TModuleResolver.TryResolveBareSpecifier(const AModulePath,
  AImportingFilePath: string; out AResolvedPath: string): Boolean;
var
  Ceiling: string;
  Manifest: TGocciaPackageManifest;
  ManifestPath, PackageDirectory, PackageName, StartDirectory: string;
  Subpath, Target, TargetCandidate: string;
begin
  AResolvedPath := '';
  if not Assigned(FNodeModulesGrant) then
    Exit(False);
  if not SplitBareSpecifier(AModulePath, PackageName, Subpath) then
    Exit(False);

  StartDirectory := ExtractFilePath(AImportingFilePath);
  if StartDirectory = '' then
    StartDirectory := FBaseDirectory;

  if not FNodeModulesGrant(AModulePath, StartDirectory, Ceiling) then
    Exit(False);

  if not FindPackageDirectory(StartDirectory, Ceiling, PackageName,
    PackageDirectory) then
    raise EModuleNotFound.CreateNotFound(AModulePath,
      IncludeTrailingPathDelimiter(ExpandHostFileName(StartDirectory)) +
        NODE_MODULES_DIRECTORY_NAME + PathDelim + PackageName);

  ManifestPath := IncludeTrailingPathDelimiter(PackageDirectory) +
    PACKAGE_MANIFEST_FILE_NAME;
  try
    if not LoadPackageManifest(PackageDirectory, Manifest) then
      raise EModuleNotFound.CreateNotFound(AModulePath, ManifestPath);
  except
    { A malformed manifest is a resolution failure like any other. Letting the
      JSON parser's own exception escape would bypass the loader's rewrapping
      and reach script as an uncatchable host error. }
    on EModuleNotFound do
      raise;
    on Exception do
      raise EModuleNotFound.CreateNotFound(AModulePath, ManifestPath);
  end;

  if not ResolvePackageSubpath(Manifest, Subpath, Target) then
    raise EModuleNotFound.CreateNotFound(AModulePath, PackageDirectory);

  TargetCandidate := ExpandHostFileName(
    IncludeTrailingPathDelimiter(PackageDirectory) +
    StringReplace(Target, SPECIFIER_SEGMENT_SEPARATOR, PathDelim,
      [rfReplaceAll]));
  { Containment is checked before the extension probe as well as after it, so a
    target that escaped the package is refused without the resolver stat-ing
    paths outside it. The segment validation in Goccia.Modules.NodeResolution
    rejects the specifiers and targets that are invalid on their face; this
    catches whatever any combination of them still normalized into, and is what
    makes the --allow-node-modules ceiling a real boundary (ADR 0111). }
  if not IsPathInsideDirectory(TargetCandidate, PackageDirectory) then
    raise EModuleNotFound.CreateNotFound(AModulePath, TargetCandidate);

  if not TryResolveWithExtensions(TargetCandidate, AResolvedPath) then
    raise EModuleNotFound.CreateNotFound(AModulePath, TargetCandidate);

  { The post-probe gate is the physical one. The candidate above is a name that
    may not exist yet, so only its spelling can be judged; by here a real file
    has been found, and a real file is what a symlinked child of the package
    would have escaped through. }
  if not IsPathPhysicallyInsideDirectory(AResolvedPath, PackageDirectory) then
    raise EModuleNotFound.CreateNotFound(AModulePath, AResolvedPath);

  if IsCommonJSModuleFile(Manifest, AResolvedPath) then
    raise EModuleIsCommonJS.CreateCommonJS(PackageName,
      PackageRelativePath(PackageDirectory, AResolvedPath), AResolvedPath);

  FLastPackageDirectory := PackageDirectory;
  Result := True;
end;

function TModuleResolver.Resolve(const AModulePath, AImportingFilePath: string): string;
var
  AliasApplied, BaseDirectory, CandidatePath: string;
begin
  FLastPackageDirectory := '';
  AliasApplied := ApplyAliases(AModulePath, AImportingFilePath);

  if AliasApplied <> AModulePath then
  begin
    CandidatePath := ExpandHostFileName(AliasApplied);
    if TryResolveWithExtensions(CandidatePath, Result) then
      Exit;
    raise EModuleNotFound.CreateNotFound(AModulePath, CandidatePath);
  end;

  if IsAbsolutePath(AModulePath) then
  begin
    CandidatePath := ExpandHostFileName(AModulePath);
    if TryResolveWithExtensions(CandidatePath, Result) then
      Exit;
    raise EModuleNotFound.CreateNotFound(AModulePath, CandidatePath);
  end;

  if (Copy(AModulePath, 1, 2) = './') or (Copy(AModulePath, 1, 3) = '../') then
  begin
    BaseDirectory := ExtractFilePath(AImportingFilePath);
    if BaseDirectory = '' then
      BaseDirectory := GetCurrentDir + PathDelim;

    CandidatePath := ExpandHostFileName(BaseDirectory + AModulePath);
    if TryResolveWithExtensions(CandidatePath, Result) then
      Exit;

    raise EModuleNotFound.CreateNotFound(AModulePath, CandidatePath);
  end;

  if TryResolveBareSpecifier(AModulePath, AImportingFilePath, Result) then
    Exit;

  raise EModuleNotFound.CreateFmt(BARE_SPECIFIER_MESSAGE_FORMAT,
    [AModulePath]);
end;

end.
