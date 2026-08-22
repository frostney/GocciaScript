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
    FCanonicalIdentity: string;
    FIdentityRequired: Boolean;
    FLastModified: TDateTime;
    FSourceLines: TStringList;
    FText: string;
    function GetByteLength: Integer;
    function GetText: string;
  public
    constructor Create(const AText: string; const ALastModified: TDateTime;
      const ACanonicalIdentity: string = '';
      const AIdentityRequired: Boolean = False);
    destructor Destroy; override;

    property ByteLength: Integer read GetByteLength;
    { Stable file-object identity captured from the same open handle used to
      read Text. Empty with IdentityRequired=True means identity acquisition
      failed and diagnostics must not retain the source under a path key. }
    property CanonicalIdentity: string read FCanonicalIdentity;
    property IdentityRequired: Boolean read FIdentityRequired;
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
{$IFDEF UNIX}
  BaseUnix,
{$ENDIF}
{$IFDEF WINDOWS}
  Windows,
{$ENDIF}

  TextEncoding,
  TextSemantics,

  Goccia.Modules.Errors,
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
  const ALastModified: TDateTime; const ACanonicalIdentity: string;
  const AIdentityRequired: Boolean);
begin
  inherited Create;
  FByteLength := Length(EncodeUTF8WithReplacement(AText));
  FCanonicalIdentity := ACanonicalIdentity;
  FIdentityRequired := AIdentityRequired;
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
  { An embedder that runs untrusted source without installing a provider must
    not get an RTL exception through its engine boundary, so the refusal is
    reported to source as a plain Error carrying a stable code. LoadContentBytes
    inherits this: the base implementation calls LoadContent first.

    APath is deliberately dropped: it is the resolved address, which the default
    resolver expands against the host filesystem, and this refusal is reported
    to potentially untrusted source. See Goccia.Modules.Errors. }
  Result := nil;
  ThrowModuleLoadingUnsupported;
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
  Bytes: TBytes;
  CanonicalIdentity: string;
  ErrorOffset: Integer;
{$IFDEF UNIX}
  Information: Stat;
{$ENDIF}
{$IFDEF WINDOWS}
  Information: BY_HANDLE_FILE_INFORMATION;
{$ENDIF}
  LastModified: TDateTime;
  SourceText: string;
  Stream: TFileStream;
begin
  CanonicalIdentity := '';
  Stream := TFileStream.Create(APath, fmOpenRead or fmShareDenyWrite);
  try
{$IFDEF UNIX}
    if fpFStat(Stream.Handle, Information) = 0 then
      CanonicalIdentity := '#id:' + IntToStr(Int64(Information.st_dev)) + ':' +
        IntToStr(Int64(Information.st_ino));
{$ENDIF}
{$IFDEF WINDOWS}
    if GetFileInformationByHandle(Windows.THandle(Stream.Handle),
       Information) then
      CanonicalIdentity := '#id:' +
        IntToStr(Int64(Information.dwVolumeSerialNumber)) + ':' +
        IntToStr(Int64(Information.nFileIndexHigh)) + ':' +
        IntToStr(Int64(Information.nFileIndexLow));
{$ENDIF}
    SetLength(Bytes, Stream.Size);
    if Length(Bytes) > 0 then
      Stream.ReadBuffer(Bytes[0], Length(Bytes));
  finally
    Stream.Free;
  end;
  if not TryDecodeUTF8(Bytes, SourceText, ErrorOffset) then
    { Report only the byte offset: this message reaches guest code through
      TGocciaRuntimeError, and the resolved host path must not be disclosed
      (ADR 0108). }
    raise EConvertError.CreateFmt('Invalid UTF-8 at byte %d', [ErrorOffset]);
  if not TryGetFileLastModified(APath, LastModified) then
    LastModified := 0;
  Result := TGocciaModuleContent.Create(SourceText, LastModified,
    CanonicalIdentity, True);
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
