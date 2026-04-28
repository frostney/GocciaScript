unit Goccia.Modules.ContentProvider;

{$I Goccia.inc}

interface

uses
  Classes,
  SysUtils;

type
  TGocciaModuleContent = class
  private
    FLastModified: TDateTime;
    FSourceLines: TStringList;
    FText: UTF8String;
    function GetText: UTF8String;
  public
    constructor Create(const AText: UTF8String; const ALastModified: TDateTime);
    destructor Destroy; override;

    property LastModified: TDateTime read FLastModified;
    property SourceLines: TStringList read FSourceLines;
    property Text: UTF8String read GetText;
  end;

  TGocciaModuleContentProvider = class
  public
    function Exists(const APath: string): Boolean; virtual; abstract;
    function LoadContent(const APath: string): TGocciaModuleContent; virtual; abstract;
    function TryGetLastModified(const APath: string;
      out ALastModified: TDateTime): Boolean; virtual; abstract;
  end;

  TGocciaFileSystemModuleContentProvider = class(TGocciaModuleContentProvider)
  public
    function Exists(const APath: string): Boolean; override;
    function LoadContent(const APath: string): TGocciaModuleContent; override;
    function TryGetLastModified(const APath: string;
      out ALastModified: TDateTime): Boolean; override;
  end;

implementation

uses
  TextSemantics,

  Goccia.TextFiles;

function TryGetFileLastModified(const APath: string;
  out ALastModified: TDateTime): Boolean;
var
  FileAgeValue: LongInt;
begin
  FileAgeValue := FileAge(APath);
  Result := FileAgeValue <> -1;
  if Result then
    ALastModified := FileDateToDateTime(FileAgeValue)
  else
    ALastModified := 0;
end;

{ TGocciaModuleContent }

constructor TGocciaModuleContent.Create(const AText: UTF8String;
  const ALastModified: TDateTime);
begin
  inherited Create;
  FLastModified := ALastModified;
  FText := AText;
  FSourceLines := CreateUTF8FileTextLines(FText);
end;

destructor TGocciaModuleContent.Destroy;
begin
  FSourceLines.Free;
  inherited;
end;

function TGocciaModuleContent.GetText: UTF8String;
begin
  Result := FText;
end;

{ TGocciaFileSystemModuleContentProvider }

function TGocciaFileSystemModuleContentProvider.Exists(
  const APath: string): Boolean;
begin
  Result := FileExists(APath);
end;

function TGocciaFileSystemModuleContentProvider.LoadContent(
  const APath: string): TGocciaModuleContent;
var
  LastModified: TDateTime;
  SourceText: UTF8String;
begin
  SourceText := ReadUTF8FileText(APath);
  if not TryGetFileLastModified(APath, LastModified) then
    LastModified := 0;
  Result := TGocciaModuleContent.Create(SourceText, LastModified);
end;

function TGocciaFileSystemModuleContentProvider.TryGetLastModified(
  const APath: string; out ALastModified: TDateTime): Boolean;
begin
  Result := TryGetFileLastModified(APath, ALastModified);
end;

end.
