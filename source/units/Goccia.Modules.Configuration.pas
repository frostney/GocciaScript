unit Goccia.Modules.Configuration;

{$I Goccia.inc}

interface

uses
  Classes,

  Goccia.Modules.Resolver;

procedure ConfigureModuleResolver(const AResolver: TGocciaModuleResolver;
  const AEntryFileName, AExplicitImportMapPath: string;
  const AInlineAliases: TStrings);

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

  ExpandedFileName := ExpandUTF8FileName(AFileName);
  if UTF8DirectoryExists(ExpandedFileName) then
    Exit(ExpandedFileName);

  if UTF8FileExists(ExpandedFileName) or (ExtractFilePath(AFileName) <> '') then
    Exit(ExtractFilePath(ExpandedFileName));

  Result := GetCurrentDir;
end;

procedure ConfigureModuleResolver(const AResolver: TGocciaModuleResolver;
  const AEntryFileName, AExplicitImportMapPath: string;
  const AInlineAliases: TStrings);
var
  AliasPair: TModuleAliasPair;
  I: Integer;
  ImportMapPath: string;
begin
  if not Assigned(AResolver) then
    Exit;

  if AExplicitImportMapPath <> '' then
    ImportMapPath := ExpandUTF8FileName(AExplicitImportMapPath)
  else
    ImportMapPath := TGocciaModuleResolver.DiscoverProjectConfig(
      ResolveEntryBaseDirectory(AEntryFileName));

  if ImportMapPath <> '' then
    AResolver.LoadImportMap(ImportMapPath);

  if not Assigned(AInlineAliases) then
    Exit;

  for I := 0 to AInlineAliases.Count - 1 do
  begin
    AliasPair := ParseAliasPair(AInlineAliases[I]);
    AResolver.AddAlias(AliasPair.Key, AliasPair.ValueText);
  end;
end;

end.
