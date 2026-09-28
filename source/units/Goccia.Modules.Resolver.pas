unit Goccia.Modules.Resolver;

{$I Goccia.inc}

interface

uses
  SysUtils,

  OrderedStringMap,

  Goccia.Error,
  Goccia.ModuleResolver,
  Goccia.Packages.Address,
  Goccia.Packages.Store,
  Goccia.Packages.Transport;

type
  { Asked before a provider package is materialized for ASpecifier, with the
    package its import-map entry names. Raises (PermissionDenied) to refuse;
    returning allows. The engine implements it from its capability set. }
  TGocciaProviderGrant = procedure(const ASpecifier: string;
    const AAddress: TGocciaProviderAddress) of object;

  { A provider package could not be resolved: not pinned, not cached under
    --cached-only, a fetch or hash failure, or an import a package may not
    make. The message names packages and package-relative files only. }
  EGocciaProviderResolutionError = class(EModuleNotFound);

  TGocciaModuleResolver = class(TModuleResolver)
  private
    FProviderAudit: TGocciaProviderAuditHandler;
    FProviderCachedOnly: Boolean;
    FProviderGrant: TGocciaProviderGrant;
    FProviderImportMaps: TStringStringMap;
    FProviderPackages: TGocciaProviderPackageStore;
    FProviderTransport: TGocciaProviderTransport;
    function ProviderPackageOf(const APath: string):
      TGocciaMaterializedPackage;
    procedure SetProviderAudit(const AValue: TGocciaProviderAuditHandler);
    procedure SetProviderCachedOnly(const AValue: Boolean);
  protected
    function IsAbsoluteImportMapPath(const APath: string): Boolean; virtual;
    function IsRelativeImportMapPath(const APath: string): Boolean; virtual;
    function NormalizeImportMapBaseDirectory(
      const AImportMapDirectory: string): string; virtual;
    function NormalizeImportMapPath(const APath, ABaseDirectory: string): string;
      virtual;
    function CandidateExists(const APath: string): Boolean; override;
    function IsExternalAliasTarget(const ATarget: string): Boolean; override;
    function ResolveExternalAliasTarget(const AModulePath, ATarget,
      AImportingFilePath: string): string; override;
    procedure CheckPathCandidate(const AModulePath, AImportingFilePath,
      ACandidatePath: string); override;
    { Asks ProviderGrant about AAddress for AModulePath; refuses when no
      grant is wired. }
    procedure RequireProviderGrant(const AModulePath: string;
      const AAddress: TGocciaProviderAddress);
  public
    constructor Create(const ABaseDirectory: string = '');
    destructor Destroy; override;
    class function DiscoverProjectConfig(const AStartDirectory: string): string; static;
    procedure LoadImportMap(const APath: string);
    function Resolve(const AModulePath,
      AImportingFilePath: string): string; override;

    { Verify on load: raises EGocciaProviderVerificationError when APath lies
      inside a materialized provider package and ABytes are not the bytes
      its lockfile pins. }
    procedure VerifyProviderContent(const APath: string; const ABytes: TBytes);
    { Whether APath lies inside a materialized provider package. }
    function IsProviderPackagePath(const APath: string): Boolean;

    property ProviderGrant: TGocciaProviderGrant
      read FProviderGrant write FProviderGrant;
    property ProviderAudit: TGocciaProviderAuditHandler
      read FProviderAudit write SetProviderAudit;
    { --cached-only: a package file missing from the cache is an error, never
      a fetch. }
    property ProviderCachedOnly: Boolean
      read FProviderCachedOnly write SetProviderCachedOnly;
    { The transport provider files are fetched with; nil selects HTTP. Not
      owned. Tests substitute a fixture. }
    property ProviderTransport: TGocciaProviderTransport
      read FProviderTransport write FProviderTransport;
    { The packages materialized so far; nil until the first. }
    property ProviderPackages: TGocciaProviderPackageStore
      read FProviderPackages;
  end;

  EGocciaModuleNotFound = EModuleNotFound;

  { The module loader turns EGocciaModuleNotFound into this runtime error so a
    failed import is catchable from script. Message stays specifier-only —
    ResolvedCandidatePath carries the expanded host address for host-side
    diagnostics and is never copied into a script-visible error (ADR 0108). }
  TGocciaModuleResolutionError = class(TGocciaRuntimeError)
  private
    FResolvedCandidatePath: string;
  public
    constructor CreateResolutionFailure(const AMessage,
      AResolvedCandidatePath, AFileName: string);

    property ResolvedCandidatePath: string read FResolvedCandidatePath;
  end;

{ Renders any engine error for host output: the usual detailed message plus,
  for a module resolution failure, the expanded candidate path the resolver
  tried. Host reporters use this instead of GetDetailedMessage so the candidate
  path kept out of AError.Message still reaches the host. }
function FormatHostErrorDiagnostic(const AError: TGocciaError;
  const AUseColor: Boolean): string;

implementation

uses
  FileUtils,

  Goccia.FileExtensions,
  Goccia.GarbageCollector,
  Goccia.JSON,
  Goccia.TextFiles,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

const
  PROJECT_CONFIG_FILE_NAME = 'goccia.json';
  IMPORTS_PROPERTY_NAME = 'imports';
  CURRENT_DIRECTORY_PREFIX = './';
  PARENT_DIRECTORY_PREFIX = '../';
  RESOLVED_CANDIDATE_DIAGNOSTIC_FORMAT = '  Resolved to: %s';

constructor TGocciaModuleResolutionError.CreateResolutionFailure(
  const AMessage, AResolvedCandidatePath, AFileName: string);
begin
  inherited Create(AMessage, 0, 0, AFileName, nil);
  FResolvedCandidatePath := AResolvedCandidatePath;
end;

function FormatHostErrorDiagnostic(const AError: TGocciaError;
  const AUseColor: Boolean): string;
begin
  Result := AError.GetDetailedMessage(AUseColor);
  if (AError is TGocciaModuleResolutionError) and
     (TGocciaModuleResolutionError(AError).ResolvedCandidatePath <> '') then
    Result := Result + Format(RESOLVED_CANDIDATE_DIAGNOSTIC_FORMAT,
      [TGocciaModuleResolutionError(AError).ResolvedCandidatePath]) + sLineBreak;
end;

function TGocciaModuleResolver.IsAbsoluteImportMapPath(
  const APath: string): Boolean;
begin
  if Length(APath) = 0 then
    Exit(False);
  if APath[1] = PathDelim then
    Exit(True);
  if (Length(APath) >= 2) and (APath[2] = ':') then
    Exit(True);
  Result := Copy(APath, 1, 2) = '\\';
end;

function TGocciaModuleResolver.IsRelativeImportMapPath(
  const APath: string): Boolean;
begin
  Result := (Copy(APath, 1, Length(CURRENT_DIRECTORY_PREFIX)) =
      CURRENT_DIRECTORY_PREFIX) or
    (Copy(APath, 1, Length(PARENT_DIRECTORY_PREFIX)) =
      PARENT_DIRECTORY_PREFIX);
end;

function HasImportMapTrailingSlash(const APath: string): Boolean;
begin
  Result := (APath <> '') and (APath[Length(APath)] = '/');
end;

function TGocciaModuleResolver.NormalizeImportMapBaseDirectory(
  const AImportMapDirectory: string): string;
begin
  Result := AImportMapDirectory;
end;

function TGocciaModuleResolver.NormalizeImportMapPath(const APath,
  ABaseDirectory: string): string;
begin
  if IsAbsoluteImportMapPath(APath) then
    Result := ExpandHostFileName(APath)
  else if IsRelativeImportMapPath(APath) then
    Result := ExpandHostFileName(ABaseDirectory + APath)
  else
    Result := APath;

  if HasImportMapTrailingSlash(APath) then
    Result := IncludeTrailingPathDelimiter(Result);
end;

function ReadImportMapText(const APath: string): string;
begin
  Result := ReadUTF8FileText(APath);
end;

constructor TGocciaModuleResolver.Create(const ABaseDirectory: string);
begin
  inherited Create(ABaseDirectory);
  SetExtensions(EngineModuleImportExtensions);
  FProviderImportMaps := TStringStringMap.Create;
end;

destructor TGocciaModuleResolver.Destroy;
begin
  FProviderPackages.Free;
  FProviderImportMaps.Free;
  inherited;
end;

procedure TGocciaModuleResolver.SetProviderAudit(
  const AValue: TGocciaProviderAuditHandler);
begin
  FProviderAudit := AValue;
  if Assigned(FProviderPackages) then
    FProviderPackages.OnAudit := AValue;
end;

procedure TGocciaModuleResolver.SetProviderCachedOnly(const AValue: Boolean);
begin
  FProviderCachedOnly := AValue;
  if Assigned(FProviderPackages) then
    FProviderPackages.CachedOnly := AValue;
end;

function TGocciaModuleResolver.ProviderPackageOf(
  const APath: string): TGocciaMaterializedPackage;
var
  RelativePath: string;
begin
  if not Assigned(FProviderPackages) or (APath = '') then
    Exit(nil);
  Result := FProviderPackages.FindPackage(APath, RelativePath);
end;

function TGocciaModuleResolver.IsProviderPackagePath(
  const APath: string): Boolean;
begin
  Result := Assigned(ProviderPackageOf(APath));
end;

procedure TGocciaModuleResolver.VerifyProviderContent(const APath: string;
  const ABytes: TBytes);
begin
  if Assigned(FProviderPackages) then
    FProviderPackages.VerifyContent(APath, ABytes);
end;

function TGocciaModuleResolver.IsExternalAliasTarget(
  const ATarget: string): Boolean;
begin
  Result := IsProviderAddress(ATarget);
end;

procedure TGocciaModuleResolver.RequireProviderGrant(
  const AModulePath: string; const AAddress: TGocciaProviderAddress);
begin
  if not Assigned(FProviderGrant) then
    raise EGocciaProviderResolutionError.CreateWithCandidate(Format(
      'Provider package %s needs the import capability',
      [AAddress.PackageKey]), '');
  FProviderGrant(AModulePath, AAddress);
end;

{ A package file inside a materialized package is one its lockfile pins, and
  nothing else in the cache directory exists for resolution. }
function TGocciaModuleResolver.CandidateExists(const APath: string): Boolean;
var
  Package: TGocciaMaterializedPackage;
  Pin, RelativePath: string;
begin
  if Assigned(FProviderPackages) then
  begin
    Package := FProviderPackages.FindPackage(APath, RelativePath);
    if Assigned(Package) then
      Exit(Package.TryGetPin(RelativePath, Pin) and HostFileExists(APath));
  end;
  Result := inherited CandidateExists(APath);
end;

{ A provider package imports only its own files (ADR 0122): no transitive
  packages, no project files, no host paths. }
procedure TGocciaModuleResolver.CheckPathCandidate(const AModulePath,
  AImportingFilePath, ACandidatePath: string);
var
  Package: TGocciaMaterializedPackage;
begin
  Package := ProviderPackageOf(AImportingFilePath);
  if Assigned(Package) and (ProviderPackageOf(ACandidatePath) <> Package) then
    raise EGocciaProviderResolutionError.CreateWithCandidate(Format(
      'Provider package %s cannot import "%s": a package imports only its ' +
      'own files', [Package.Key, AModulePath]), ACandidatePath);
end;

function TGocciaModuleResolver.Resolve(const AModulePath,
  AImportingFilePath: string): string;
var
  CandidatePath: string;
  Package: TGocciaMaterializedPackage;
begin
  { A package names its own files by relative specifier. An absolute path
    is let through to CheckPathCandidate, which keeps it inside the package:
    the loader reloads a changed module by its own resolved path. }
  Package := ProviderPackageOf(AImportingFilePath);
  if not Assigned(Package) then
    Exit(inherited Resolve(AModulePath, AImportingFilePath));
  if (Copy(AModulePath, 1, Length(CURRENT_DIRECTORY_PREFIX)) <>
      CURRENT_DIRECTORY_PREFIX) and
     (Copy(AModulePath, 1, Length(PARENT_DIRECTORY_PREFIX)) <>
      PARENT_DIRECTORY_PREFIX) and not IsAbsoluteHostPath(AModulePath) then
    raise EGocciaProviderResolutionError.CreateWithCandidate(Format(
      'Provider package %s cannot import "%s": a package imports only its ' +
      'own files, by relative specifier', [Package.Key, AModulePath]), '');
  { No import-map entry or alias applies to a package's imports: a path
    key would otherwise carry a relative specifier out of the package, to a
    project file or another provider package, past the check below. }
  SetLastPackageDirectory('');
  if IsAbsoluteHostPath(AModulePath) then
    CandidatePath := ExpandHostFileName(AModulePath)
  else
    CandidatePath := ExpandHostFileName(ExtractFilePath(AImportingFilePath) +
      AModulePath);
  CheckPathCandidate(AModulePath, AImportingFilePath, CandidatePath);
  if not TryResolveWithExtensions(CandidatePath, Result) then
    raise EModuleNotFound.CreateNotFound(AModulePath, CandidatePath);
end;

{ The import-map entry mapped AModulePath to ATarget, a provider address
  with any prefix tail appended. The package is granted, materialized, and
  then searched for the file in the resolver's candidate order: the path
  itself, its TypeScript sources, each extension, each index file. Only
  pinned files are candidates. }
function TGocciaModuleResolver.ResolveExternalAliasTarget(const AModulePath,
  ATarget, AImportingFilePath: string): string;
var
  Address: TGocciaProviderAddress;
  Candidates: array of string;
  Error, ImportMapPath, Pin, Stem: string;
  Extensions: TModuleResolverExtensionArray;
  I: Integer;
  Package: TGocciaMaterializedPackage;
  TypeScriptCandidates: TFileExtensionArray;

  procedure AddCandidate(const ACandidate: string);
  begin
    SetLength(Candidates, Length(Candidates) + 1);
    Candidates[High(Candidates)] := ACandidate;
  end;

begin
  if not TryParseProviderAddress(ATarget, Address, Error) then
    raise EGocciaProviderResolutionError.CreateWithCandidate(Format(
      'Cannot resolve "%s" in its provider package: %s',
      [AModulePath, Error]), ATarget);
  if not FProviderImportMaps.TryGetValue(Address.NormalizedPackageKey,
     ImportMapPath) then
    raise EGocciaProviderResolutionError.CreateWithCandidate(Format(
      'Provider package %s is not declared by an import map',
      [Address.PackageKey]), ATarget);

  RequireProviderGrant(AModulePath, Address);

  if not Assigned(FProviderPackages) then
  begin
    FProviderPackages := TGocciaProviderPackageStore.Create(
      FProviderTransport, False);
    FProviderPackages.CachedOnly := FProviderCachedOnly;
    FProviderPackages.OnAudit := FProviderAudit;
  end;
  try
    Package := FProviderPackages.Materialize(Address, ImportMapPath);
  except
    on E: EGocciaProviderPackageError do
      raise EGocciaProviderResolutionError.CreateWithCandidate(E.Message,
        E.Detail);
  end;

  Candidates := nil;
  Extensions := GetExtensions;
  if Address.IsPrefix then
    Stem := Address.Path
  else
  begin
    AddCandidate(Address.Path);
    TypeScriptCandidates := TypeScriptSourceCandidates(Address.Path);
    for I := 0 to High(TypeScriptCandidates) do
      AddCandidate(TypeScriptCandidates[I]);
    for I := 0 to High(Extensions) do
      AddCandidate(Address.Path + Extensions[I]);
    Stem := Address.Path + '/';
  end;
  for I := 0 to High(Extensions) do
    AddCandidate(Stem + 'index' + Extensions[I]);

  SetProbePackageDirectory(Package.Root);
  try
    for I := 0 to High(Candidates) do
    begin
      Result := Package.Root + PathDelim + ToHostRelativePath(Candidates[I]);
      if Assigned(ProbeGuard) then
        ProbeGuard(Result);
      if Package.TryGetPin(Candidates[I], Pin) then
      begin
        SetLastPackageDirectory(Package.Root);
        Exit;
      end;
    end;
  finally
    SetProbePackageDirectory('');
  end;
  raise EModuleNotFound.CreateNotFound(AModulePath, Package.Root + PathDelim +
    ToHostRelativePath(Address.Path));
end;

class function TGocciaModuleResolver.DiscoverProjectConfig(
  const AStartDirectory: string): string;
var
  CandidatePath, CurrentDirectory, ParentDirectory: string;
begin
  if AStartDirectory <> '' then
    CurrentDirectory := ExpandHostFileName(AStartDirectory)
  else
    CurrentDirectory := GetCurrentDir;

  if not HostDirectoryExists(CurrentDirectory) then
    CurrentDirectory := ExtractFilePath(CurrentDirectory);

  CurrentDirectory := ExcludeTrailingPathDelimiter(CurrentDirectory);
  if CurrentDirectory = '' then
    CurrentDirectory := PathDelim;

  while True do
  begin
    CandidatePath := IncludeTrailingPathDelimiter(CurrentDirectory) +
      PROJECT_CONFIG_FILE_NAME;
    if HostFileExists(CandidatePath) then
      Exit(CandidatePath);

    ParentDirectory := ExtractFileDir(CurrentDirectory);
    if (ParentDirectory = '') or (ParentDirectory = CurrentDirectory) then
      Break;

    CurrentDirectory := ParentDirectory;
  end;

  Result := '';
end;

procedure TGocciaModuleResolver.LoadImportMap(const APath: string);
var
  Address: TGocciaProviderAddress;
  AddressError, DeclaringImportMap: string;
  ImportMapBaseDirectory, ImportMapDirectory, ImportMapPath, Key: string;
  NormalizedKey, NormalizedValue: string;
  Parser: TGocciaJSONParser;
  ParsedValue, ImportsValue, Value: TGocciaValue;
  ImportsObject, ImportMapObject: TGocciaObjectValue;
begin
  ImportMapPath := ExpandHostFileName(APath);
  if not HostFileExists(ImportMapPath) then
    raise Exception.Create('Import map not found: ' + ImportMapPath);

  Parser := TGocciaJSONParser.Create;
  try
    ParsedValue := Parser.Parse(ReadImportMapText(ImportMapPath));
  finally
    Parser.Free;
  end;

  if not (ParsedValue is TGocciaObjectValue) then
    raise Exception.Create('Import map must be a top-level JSON object.');

  if (TGarbageCollector.Instance <> nil) then
    TGarbageCollector.Instance.AddTempRoot(ParsedValue);
  try
    ImportMapObject := TGocciaObjectValue(ParsedValue);
    ImportsValue := ImportMapObject.GetProperty(IMPORTS_PROPERTY_NAME);
    if (not Assigned(ImportsValue)) or
       (ImportsValue is TGocciaUndefinedLiteralValue) then
      Exit;
    if not (ImportsValue is TGocciaObjectValue) then
      raise Exception.Create('Import map "imports" field must be a JSON object.');

    ImportsObject := TGocciaObjectValue(ImportsValue);
    ImportMapDirectory := IncludeTrailingPathDelimiter(
      ExtractFilePath(ImportMapPath));
    ImportMapBaseDirectory := NormalizeImportMapBaseDirectory(
      ImportMapDirectory);

    for Key in ImportsObject.GetOwnPropertyKeys do
    begin
      Value := ImportsObject.GetProperty(Key);
      if not (Value is TGocciaStringLiteralValue) then
        raise Exception.CreateFmt(
          'Import map entry "%s" must map to a string address.', [Key]);

      if HasImportMapTrailingSlash(Key) and
         not HasImportMapTrailingSlash(TGocciaStringLiteralValue(Value).Value) then
        raise Exception.CreateFmt(
          'Import map entry "%s" ends with "/" so its address must also end with "/".',
          [Key]);

      NormalizedKey := NormalizeImportMapPath(Key, ImportMapBaseDirectory);
      if IsProviderAddress(TGocciaStringLiteralValue(Value).Value) then
      begin
        { A provider entry is recorded, not resolved: its package is granted
          and materialized when an import first resolves through it, so a
          run that never imports it needs no grant. }
        if not TryParseProviderAddress(TGocciaStringLiteralValue(Value).Value,
           Address, AddressError) then
          raise Exception.CreateFmt(
            'Import map entry "%s" has an invalid provider address: %s',
            [Key, AddressError]);
        if FProviderImportMaps.TryGetValue(Address.NormalizedPackageKey,
           DeclaringImportMap) and (DeclaringImportMap <> ImportMapPath) then
          raise Exception.CreateFmt(
            'Import map entry "%s" names %s, which %s already declares',
            [Key, Address.PackageKey, DeclaringImportMap]);
        FProviderImportMaps.AddOrSetValue(Address.NormalizedPackageKey,
          ImportMapPath);
        AddAlias(NormalizedKey, TGocciaStringLiteralValue(Value).Value);
        Continue;
      end;

      if not (IsAbsoluteImportMapPath(TGocciaStringLiteralValue(Value).Value) or
              IsRelativeImportMapPath(TGocciaStringLiteralValue(Value).Value)) then
        raise Exception.CreateFmt(
          'Import map entry "%s" must use an absolute or relative file path ' +
          'address, or a provider address (github:<owner>/<repo>@<ref>).',
          [Key]);

      NormalizedValue := NormalizeImportMapPath(
        TGocciaStringLiteralValue(Value).Value, ImportMapBaseDirectory);
      AddAlias(NormalizedKey, NormalizedValue);
    end;
  finally
    if (TGarbageCollector.Instance <> nil) then
      TGarbageCollector.Instance.RemoveTempRoot(ParsedValue);
  end;
end;

end.
