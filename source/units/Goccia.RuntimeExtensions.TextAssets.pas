unit Goccia.RuntimeExtensions.TextAssets;

{$I Goccia.inc}

interface

uses
  Classes,

  Goccia.Modules,
  Goccia.Runtime;

type
  TGocciaTextAssetsRuntimeExtension = class(TGocciaRuntimeExtension)
  public
    procedure AddModuleExtensions(const AExtensions: TStrings); override;
    function TryLoadModule(const AResolvedPath: string;
      out AModule: TGocciaModule): Boolean; override;
  end;

implementation

uses
  SysUtils,

  TextSemantics,

  Goccia.Constants.PropertyNames,
  Goccia.FileExtensions,
  Goccia.Keywords.Reserved,
  Goccia.Modules.ContentProvider,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

const
  TEXT_ASSET_KIND = 'text';

procedure TGocciaTextAssetsRuntimeExtension.AddModuleExtensions(
  const AExtensions: TStrings);
begin
  AExtensions.Add(EXT_TXT);
  AExtensions.Add(EXT_MD);
end;

function TGocciaTextAssetsRuntimeExtension.TryLoadModule(
  const AResolvedPath: string; out AModule: TGocciaModule): Boolean;
var
  Content: TGocciaModuleContent;
  LoadSucceeded: Boolean;
  Metadata: TGocciaObjectValue;
  NormalizedText: UTF8String;
  TextValue: TGocciaValue;
begin
  AModule := nil;
  Result := IsTextAssetExtension(ExtractFileExt(AResolvedPath));
  if not Result then
    Exit;

  Content := Runtime.Engine.ModuleLoader.ContentProvider.LoadContent(
    AResolvedPath);
  try
    NormalizedText := NormalizeUTF8NewlinesToLF(Content.Text);
    Metadata := TGocciaObjectValue.Create(
      TGocciaObjectValue.SharedObjectPrototype, 5);
    Metadata.SetProperty(PROP_KIND,
      TGocciaStringLiteralValue.Create(TEXT_ASSET_KIND));
    Metadata.SetProperty(PROP_PATH,
      TGocciaStringLiteralValue.Create(AResolvedPath));
    Metadata.SetProperty(PROP_FILE_NAME,
      TGocciaStringLiteralValue.Create(ExtractFileName(AResolvedPath)));
    Metadata.SetProperty(PROP_EXTENSION,
      TGocciaStringLiteralValue.Create(ExtractFileExt(AResolvedPath)));
    Metadata.SetProperty(PROP_BYTE_LENGTH,
      TGocciaNumberLiteralValue.Create(Length(Content.Text)));
    Metadata.Freeze;
    TextValue := TGocciaStringLiteralValue.FromUTF8(NormalizedText);

    AModule := TGocciaModule.Create(AResolvedPath);
    AModule.LastModified := Content.LastModified;
    LoadSucceeded := False;
    try
      AModule.AddExportValue(PROP_METADATA, Metadata);
      AModule.AddExportValue(PROP_CONTENT, TextValue);
      AModule.AddExportValue(KEYWORD_DEFAULT, TextValue);
      LoadSucceeded := True;
    finally
      if not LoadSucceeded then
      begin
        AModule.Free;
        AModule := nil;
      end;
    end;
  finally
    Content.Free;
  end;
end;

end.
