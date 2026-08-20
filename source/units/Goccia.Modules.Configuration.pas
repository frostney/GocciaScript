unit Goccia.Modules.Configuration;

{$I Goccia.inc}

interface

uses
  Classes,

  Goccia.Modules.Resolver;

const
  { Config-file spellings of the node_modules capability. `true`/`false` are
    what a JSON boolean flattens to; any other value is read as the ceiling
    directory for the ancestor walk. }
  NODE_MODULES_SETTING_ENABLED = 'true';
  NODE_MODULES_SETTING_DISABLED = 'false';

procedure ConfigureModuleResolver(const AResolver: TGocciaModuleResolver;
  const AEntryFileName, AExplicitImportMapPath: string;
  const AInlineAliases: TStrings;
  const AInlineAliasBaseDirectory: string = '');

{ Applies the --allow-node-modules capability to a resolver.

  APresent is whether the option was given at all — the flag alone arrives as
  a present option with an empty value, so presence and value carry different
  information. The default profile never calls this, which is what keeps bare
  specifiers sealed unless a host opts in. }
procedure ConfigureNodeModulesResolution(
  const AResolver: TGocciaModuleResolver; const APresent: Boolean;
  const ASetting: string);

implementation

uses
  SysUtils,

  FileUtils;

type
  TModuleAliasPair = record
    Key: string;
    ValueText: string;
  end;

function ParseAliasPair(const AArg: string): TModuleAliasPair;
var
  SeparatorIndex: Integer;
begin
  SeparatorIndex := Pos('=', AArg);
  if SeparatorIndex <= 1 then
    raise Exception.Create('Invalid --alias argument. Use --alias key=value.');

  Result.Key := Copy(AArg, 1, SeparatorIndex - 1);
  Result.ValueText := Copy(AArg, SeparatorIndex + 1, MaxInt);
end;

function ResolveEntryBaseDirectory(const AFileName: string): string;
var
  ExpandedFileName: string;
begin
  if AFileName = '' then
    Exit(GetCurrentDir);

  ExpandedFileName := ExpandHostFileName(AFileName);
  if HostDirectoryExists(ExpandedFileName) then
    Exit(ExpandedFileName);

  if HostFileExists(ExpandedFileName) or (ExtractFilePath(AFileName) <> '') then
    Exit(ExtractFilePath(ExpandedFileName));

  Result := GetCurrentDir;
end;

procedure ConfigureModuleResolver(const AResolver: TGocciaModuleResolver;
  const AEntryFileName, AExplicitImportMapPath: string;
  const AInlineAliases: TStrings;
  const AInlineAliasBaseDirectory: string);
var
  AliasPair: TModuleAliasPair;
  I: Integer;
  ImportMapPath: string;
begin
  if not Assigned(AResolver) then
    Exit;

  if AExplicitImportMapPath <> '' then
    ImportMapPath := ExpandHostFileName(AExplicitImportMapPath)
  else
    ImportMapPath := TGocciaModuleResolver.DiscoverProjectConfig(
      ResolveEntryBaseDirectory(AEntryFileName));

  if ImportMapPath <> '' then
    AResolver.LoadImportMap(ImportMapPath);

  if not Assigned(AInlineAliases) then
    Exit;

  if (AInlineAliases.Count > 0) and
     (AInlineAliasBaseDirectory <> '') then
    AResolver.BaseDirectory := IncludeTrailingPathDelimiter(
      ExpandHostFileName(AInlineAliasBaseDirectory));

  for I := 0 to AInlineAliases.Count - 1 do
  begin
    AliasPair := ParseAliasPair(AInlineAliases[I]);
    AResolver.AddAlias(AliasPair.Key, AliasPair.ValueText);
  end;
end;

procedure ConfigureNodeModulesResolution(
  const AResolver: TGocciaModuleResolver; const APresent: Boolean;
  const ASetting: string);
begin
  if (not Assigned(AResolver)) or (not APresent) then
    Exit;
  if ASetting = NODE_MODULES_SETTING_DISABLED then
    Exit;
  if (ASetting = '') or (ASetting = NODE_MODULES_SETTING_ENABLED) then
    AResolver.AllowNodeModules
  else
    AResolver.AllowNodeModules(ASetting);
end;

end.
