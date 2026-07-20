unit Goccia.Sandbox.Modules;

{$I Goccia.inc}

interface

uses
  Classes,
  SysUtils,

  SandboxVirtualFileSystem,

  Goccia.ModuleResolver,
  Goccia.Modules.ContentProvider,
  Goccia.Modules.Resolver;

type
  TGocciaSandboxModuleContentProvider = class(TGocciaModuleContentProvider)
  private
    FFs: TSandboxVirtualFileSystem;
  public
    constructor Create(const AFs: TSandboxVirtualFileSystem);

    function Exists(const APath: string): Boolean; override;
    function LoadContent(const APath: string): TGocciaModuleContent; override;
    function LoadContentBytes(const APath: string): TBytes; override;
    function TryGetLastModified(const APath: string;
      out ALastModified: TDateTime): Boolean; override;
  end;

  TGocciaSandboxModuleResolver = class(TGocciaModuleResolver)
  private
    FFs: TSandboxVirtualFileSystem;
    function TryResolveWithSandboxExtensions(const ABasePath: string;
      out AResolvedPath: string): Boolean;
    function NormalizeImportBase(const AImportingFilePath: string): string;
  protected
    function IsAbsoluteImportMapPath(const APath: string): Boolean; override;
    function IsRelativeImportMapPath(const APath: string): Boolean; override;
    function NormalizeImportMapBaseDirectory(
      const AImportMapDirectory: string): string; override;
    function NormalizeImportMapPath(const APath, ABaseDirectory: string): string;
      override;
  public
    constructor Create(const AFs: TSandboxVirtualFileSystem;
      const ABaseDirectory: string = '/');

    function Resolve(const AModulePath, AImportingFilePath: string): string; override;
  end;

implementation

const
  CURRENT_DIRECTORY_PREFIX = './';
  PARENT_DIRECTORY_PREFIX = '../';

function IsAbsoluteSandboxPath(const APath: string): Boolean;
var
  Path: string;
begin
  Path := NormalizeSandboxPathSeparators(APath);
  Result := (Path <> '') and (Path[1] = '/');
end;

function IsRelativeModuleSpecifier(const AModulePath: string): Boolean;
var
  ModulePath: string;
begin
  ModulePath := NormalizeSandboxPathSeparators(AModulePath);
  Result := (Copy(ModulePath, 1, Length(CURRENT_DIRECTORY_PREFIX)) =
      CURRENT_DIRECTORY_PREFIX) or
    (Copy(ModulePath, 1, Length(PARENT_DIRECTORY_PREFIX)) =
      PARENT_DIRECTORY_PREFIX);
end;

function JoinSandboxPath(const ABase, APath: string): string;
var
  Base: string;
  Path: string;
begin
  Base := NormalizeSandboxPathSeparators(ABase);
  Path := NormalizeSandboxPathSeparators(APath);
  if Base = '/' then
    Result := '/' + Path
  else
    Result := Base + '/' + Path;
end;

function HasSandboxTrailingSeparator(const APath: string): Boolean;
var
  Path: string;
begin
  Path := NormalizeSandboxPathSeparators(APath);
  Result := (Path <> '') and (Path[Length(Path)] = '/');
end;

function EnsureSandboxTrailingSeparatorIfNeeded(const APath: string;
  const ANeedsTrailingSeparator: Boolean): string;
begin
  Result := APath;
  if ANeedsTrailingSeparator and not HasSandboxTrailingSeparator(Result) then
    Result := Result + '/';
end;

{ TGocciaSandboxModuleContentProvider }

constructor TGocciaSandboxModuleContentProvider.Create(
  const AFs: TSandboxVirtualFileSystem);
begin
  inherited Create;
  FFs := AFs;
end;

function TGocciaSandboxModuleContentProvider.Exists(
  const APath: string): Boolean;
begin
  Result := Assigned(FFs) and FFs.IsFile(APath);
end;

function TGocciaSandboxModuleContentProvider.LoadContent(
  const APath: string): TGocciaModuleContent;
var
  Stat: TSandboxFsStat;
begin
  if not Assigned(FFs) then
    raise EStreamError.Create('No sandbox filesystem configured.');

  Stat := FFs.Stat(APath);
  Result := TGocciaModuleContent.Create(string(FFs.ReadAllText(APath)),
    Stat.ModifiedAt);
end;

function TGocciaSandboxModuleContentProvider.LoadContentBytes(
  const APath: string): TBytes;
begin
  if not Assigned(FFs) then
    raise EStreamError.Create('No sandbox filesystem configured.');

  // Read raw bytes directly from the virtual filesystem so Import Bytes
  // preserves the exact file contents instead of round-tripping through text.
  Result := FFs.ReadAllBytes(APath);
end;

function TGocciaSandboxModuleContentProvider.TryGetLastModified(
  const APath: string; out ALastModified: TDateTime): Boolean;
var
  Stat: TSandboxFsStat;
begin
  ALastModified := 0;
  Result := Assigned(FFs) and FFs.Exists(APath);
  if not Result then
    Exit;
  Stat := FFs.Stat(APath);
  ALastModified := Stat.ModifiedAt;
end;

{ TGocciaSandboxModuleResolver }

constructor TGocciaSandboxModuleResolver.Create(
  const AFs: TSandboxVirtualFileSystem; const ABaseDirectory: string);
begin
  inherited Create(ABaseDirectory);
  FFs := AFs;
  BaseDirectory := ABaseDirectory;
end;

function TGocciaSandboxModuleResolver.IsAbsoluteImportMapPath(
  const APath: string): Boolean;
begin
  Result := IsAbsoluteSandboxPath(APath);
end;

function TGocciaSandboxModuleResolver.IsRelativeImportMapPath(
  const APath: string): Boolean;
begin
  Result := IsRelativeModuleSpecifier(APath);
end;

function TGocciaSandboxModuleResolver.NormalizeImportMapBaseDirectory(
  const AImportMapDirectory: string): string;
begin
  Result := FFs.Normalize(BaseDirectory);
end;

function TGocciaSandboxModuleResolver.NormalizeImportMapPath(const APath,
  ABaseDirectory: string): string;
var
  Path: string;
begin
  Path := NormalizeSandboxPathSeparators(APath);
  if IsAbsoluteSandboxPath(Path) then
    Result := FFs.Normalize(Path)
  else if IsRelativeModuleSpecifier(Path) then
    Result := FFs.Normalize(Path, NormalizeSandboxPathSeparators(ABaseDirectory))
  else
    Result := Path;

  Result := EnsureSandboxTrailingSeparatorIfNeeded(Result,
    HasSandboxTrailingSeparator(Path));
end;

function TGocciaSandboxModuleResolver.NormalizeImportBase(
  const AImportingFilePath: string): string;
var
  SlashIndex: Integer;
begin
  Result := BaseDirectory;
  if AImportingFilePath = '' then
    Exit;

  Result := FFs.Normalize(AImportingFilePath);
  SlashIndex := Length(Result);
  while (SlashIndex > 1) and (Result[SlashIndex] <> '/') do
    Dec(SlashIndex);
  if SlashIndex <= 1 then
    Result := '/'
  else
    Result := Copy(Result, 1, SlashIndex - 1);
end;

function TGocciaSandboxModuleResolver.TryResolveWithSandboxExtensions(
  const ABasePath: string; out AResolvedPath: string): Boolean;
var
  Extensions: TModuleResolverExtensionArray;
  I: Integer;
  Candidate: string;
begin
  if FFs.IsFile(ABasePath) then
  begin
    AResolvedPath := ABasePath;
    Exit(True);
  end;

  Extensions := GetExtensions;
  for I := 0 to High(Extensions) do
  begin
    Candidate := ABasePath + Extensions[I];
    if FFs.IsFile(Candidate) then
    begin
      AResolvedPath := Candidate;
      Exit(True);
    end;
  end;

  for I := 0 to High(Extensions) do
  begin
    Candidate := ABasePath;
    if Candidate <> '/' then
      Candidate := Candidate + '/';
    Candidate := Candidate + 'index' + Extensions[I];
    if FFs.IsFile(Candidate) then
    begin
      AResolvedPath := Candidate;
      Exit(True);
    end;
  end;

  Result := False;
end;

function TGocciaSandboxModuleResolver.Resolve(const AModulePath,
  AImportingFilePath: string): string;
var
  AliasApplied: string;
  BaseDirectoryPath: string;
  Candidate: string;
begin
  if not Assigned(FFs) then
    raise EModuleNotFound.Create('No sandbox filesystem configured.');

  AliasApplied := ApplyAliases(AModulePath, AImportingFilePath);
  if AliasApplied <> AModulePath then
  begin
    Candidate := StringReplace(AliasApplied, '\', '/', [rfReplaceAll]);
    if not IsAbsoluteSandboxPath(Candidate) then
      Candidate := JoinSandboxPath(BaseDirectory, Candidate);
    Candidate := FFs.Normalize(Candidate);
    if TryResolveWithSandboxExtensions(Candidate, Result) then
      Exit;
    raise EModuleNotFound.CreateFmt(
      'Module not found: "%s" (alias resolved to "%s")',
      [AModulePath, Candidate]);
  end;

  if IsAbsoluteSandboxPath(AModulePath) then
  begin
    Candidate := FFs.Normalize(NormalizeSandboxPathSeparators(AModulePath));
    if TryResolveWithSandboxExtensions(Candidate, Result) then
      Exit;
    raise EModuleNotFound.CreateFmt('Module not found: "%s"', [AModulePath]);
  end;

  if IsRelativeModuleSpecifier(AModulePath) then
  begin
    BaseDirectoryPath := NormalizeImportBase(AImportingFilePath);
    Candidate := FFs.Normalize(NormalizeSandboxPathSeparators(AModulePath),
      BaseDirectoryPath);
    if TryResolveWithSandboxExtensions(Candidate, Result) then
      Exit;
    raise EModuleNotFound.CreateFmt(
      'Module not found: "%s" (resolved to "%s")',
      [AModulePath, Candidate]);
  end;

  raise EModuleNotFound.CreateFmt(
    'Cannot resolve bare module specifier "%s". Imports must start with "./" or "../"',
    [AModulePath]);
end;

end.
