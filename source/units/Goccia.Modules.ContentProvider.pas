unit Goccia.Modules.ContentProvider;

{$I Goccia.inc}

interface

uses
  Classes,
  SysUtils;

type
  TGocciaModuleContent = class
  private
    FByteLength: Integer;
    FLastModified: TDateTime;
    FSourceLines: TStringList;
    FText: string;
    function GetByteLength: Integer;
    function GetText: string;
  public
    constructor Create(const AText: string; const ALastModified: TDateTime);
    destructor Destroy; override;

    property ByteLength: Integer read GetByteLength;
    property LastModified: TDateTime read FLastModified;
    property SourceLines: TStringList read FSourceLines;
    property Text: string read GetText;
  end;

  TGocciaModuleContentProvider = class
  public
    function Exists(const APath: string): Boolean; virtual; abstract;
    function LoadContent(const APath: string): TGocciaModuleContent; virtual; abstract;
    function TryGetLastModified(const APath: string;
      out ALastModified: TDateTime): Boolean; virtual; abstract;

    { Raw bytes of the resolved module, preserving every byte exactly for
      Import Bytes (NUL bytes, non-UTF-8 data, original newlines). The base
      implementation reuses LoadContent; providers backed by exact byte storage
      should override it to avoid the UTF-8/source-line round trip. }
    function LoadContentBytes(const APath: string): TBytes; virtual;
  end;

  TGocciaUnavailableModuleContentProvider = class(TGocciaModuleContentProvider)
  public
    function Exists(const APath: string): Boolean; override;
    function LoadContent(const APath: string): TGocciaModuleContent; override;
    function TryGetLastModified(const APath: string;
      out ALastModified: TDateTime): Boolean; override;
  end;

  TGocciaFileSystemModuleContentProvider = class(TGocciaModuleContentProvider)
  public
    function Exists(const APath: string): Boolean; override;
    function LoadContent(const APath: string): TGocciaModuleContent; override;
    function LoadContentBytes(const APath: string): TBytes; override;
    function TryGetLastModified(const APath: string;
      out ALastModified: TDateTime): Boolean; override;
  end;

implementation

uses
  TextEncoding,
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

constructor TGocciaModuleContent.Create(const AText: string;
  const ALastModified: TDateTime);
begin
  inherited Create;
  FByteLength := Length(EncodeUTF8WithReplacement(AText));
  FLastModified := ALastModified;
  FText := AText;
  FSourceLines := CreateFileTextLines(FText);
end;

destructor TGocciaModuleContent.Destroy;
begin
  FSourceLines.Free;
  inherited;
end;

function TGocciaModuleContent.GetText: string;
begin
  Result := FText;
end;

function TGocciaModuleContent.GetByteLength: Integer;
begin
  Result := FByteLength;
end;

{ TGocciaModuleContentProvider }

function TGocciaModuleContentProvider.LoadContentBytes(
  const APath: string): TBytes;
var
  Content: TGocciaModuleContent;
begin
  Content := LoadContent(APath);
  try
    Result := EncodeUTF8WithReplacement(Content.Text);
  finally
    Content.Free;
  end;
end;

{ TGocciaUnavailableModuleContentProvider }

function TGocciaUnavailableModuleContentProvider.Exists(
  const APath: string): Boolean;
begin
  Result := False;
end;

function TGocciaUnavailableModuleContentProvider.LoadContent(
  const APath: string): TGocciaModuleContent;
begin
  raise EStreamError.Create(
    'No module content provider configured for: ' + APath);
end;

function TGocciaUnavailableModuleContentProvider.TryGetLastModified(
  const APath: string; out ALastModified: TDateTime): Boolean;
begin
  ALastModified := 0;
  Result := False;
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
  SourceText: string;
begin
  SourceText := ReadUTF8FileText(APath);
  if not TryGetFileLastModified(APath, LastModified) then
    LastModified := 0;
  Result := TGocciaModuleContent.Create(SourceText, LastModified);
end;

function TGocciaFileSystemModuleContentProvider.LoadContentBytes(
  const APath: string): TBytes;
begin
  Result := ReadFileBytes(APath);
end;

function TGocciaFileSystemModuleContentProvider.TryGetLastModified(
  const APath: string; out ALastModified: TDateTime): Boolean;
begin
  Result := TryGetFileLastModified(APath, ALastModified);
end;

end.
