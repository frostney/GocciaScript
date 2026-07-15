unit Goccia.Modules.Virtual;

{$I Goccia.inc}

interface

uses
  Classes,
  SysUtils,

  OrderedStringMap,

  Goccia.Modules.ContentProvider;

type
  TGocciaVirtualModuleContentType = (
    vmctJavaScript,
    vmctJSON,
    vmctText,
    vmctBytes
  );

  TGocciaVirtualModuleDefinition = class
  private
    FAddress: string;
    FBytes: TBytes;
    FContentType: TGocciaVirtualModuleContentType;
    FLoaded: Boolean;
    FProvenance: string;
    FText: UTF8String;
  public
    constructor Create(const AAddress: string;
      const AContentType: TGocciaVirtualModuleContentType;
      const AText: UTF8String; const ABytes: TBytes;
      const AProvenance: string);
    function Clone: TGocciaVirtualModuleDefinition;

    property Address: string read FAddress;
    property Bytes: TBytes read FBytes;
    property ContentType: TGocciaVirtualModuleContentType read FContentType;
    property Loaded: Boolean read FLoaded write FLoaded;
    property Provenance: string read FProvenance;
    property Text: UTF8String read FText;
  end;

  TGocciaVirtualModuleRegistry = class
  private
    FDefinitions: TOrderedStringMap<TGocciaVirtualModuleDefinition>;
    function CanonicalizeAddress(const AAddress,
      ABaseAddress: string): string;
    procedure FreeDefinitions;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Add(const AAddress, ABaseAddress: string;
      const AContentType: TGocciaVirtualModuleContentType;
      const AText: UTF8String; const ABytes: TBytes;
      const AProvenance: string);
    procedure AddText(const AAddress, ABaseAddress: string;
      const AContentType: TGocciaVirtualModuleContentType;
      const AContent: UTF8String; const AProvenance: string = '');
    function CanonicalAddress(const AAddress,
      ABaseAddress: string): string;
    procedure CopyFrom(const ASource: TGocciaVirtualModuleRegistry);
    function Contains(const AAddress: string): Boolean;
    function GetContentType(const AAddress: string;
      out AContentType: TGocciaVirtualModuleContentType): Boolean;
    function LoadContent(const AAddress: string): TGocciaModuleContent;
    function LoadContentBytes(const AAddress: string): TBytes;
    function Resolve(const AModulePath,
      AImportingAddress: string; out AResolvedAddress: string): Boolean;
    function TryGetDefinition(const AAddress: string;
      out ADefinition: TGocciaVirtualModuleDefinition): Boolean;
  end;

function ParseVirtualModuleContentType(const AType: string;
  out AContentType: TGocciaVirtualModuleContentType): Boolean;
function IsRelativeSpecifier(const AValue: string): Boolean;
function VirtualModuleContentTypeName(
  const AContentType: TGocciaVirtualModuleContentType): string;

implementation

uses
  base64;

function IsRelativeSpecifier(const AValue: string): Boolean;
begin
  Result := (Copy(AValue, 1, 2) = './') or
    (Copy(AValue, 1, 3) = '../');
end;

function HasScheme(const AValue: string): Boolean;
var
  ColonPosition, SlashPosition: SizeInt;
begin
  ColonPosition := Pos(':', AValue);
  SlashPosition := Pos('/', AValue);
  if (ColonPosition = 2) and (Length(AValue) >= 3) and
     (AValue[3] in ['/', '\']) then
    Exit(False);
  Result := (ColonPosition > 1) and
    ((SlashPosition = 0) or (ColonPosition < SlashPosition));
end;

function IsAbsolutePath(const AValue: string): Boolean;
begin
  Result := (AValue <> '') and ((AValue[1] = PathDelim) or
    ((Length(AValue) >= 2) and (AValue[2] = ':')));
end;

function FindAddressSuffixStart(const AValue: string): SizeInt;
var
  I: Integer;
begin
  for I := 1 to Length(AValue) do
    if AValue[I] in ['?', '#'] then
      Exit(I);
  Result := 0;
end;

function NormalizeVirtualAddress(const AValue: string): string;
var
  I, LeadingSlashCount, ProtectedSegmentCount, SegmentStart: Integer;
  IsLastSegment: Boolean;
  ResultParts: TStringList;
  PathPart, Prefix, Remainder, Segment, Suffix: string;
  SchemeEnd, SuffixStart: SizeInt;
begin
  Prefix := '';
  Remainder := AValue;
  SchemeEnd := Pos(':', AValue);
  if HasScheme(AValue) then
  begin
    Prefix := Copy(AValue, 1, SchemeEnd);
    Remainder := Copy(AValue, SchemeEnd + 1, MaxInt);
  end;

  SuffixStart := FindAddressSuffixStart(Remainder);
  if SuffixStart > 0 then
  begin
    Suffix := Copy(Remainder, SuffixStart, MaxInt);
    PathPart := Copy(Remainder, 1, SuffixStart - 1);
  end
  else
  begin
    Suffix := '';
    PathPart := Remainder;
  end;

  LeadingSlashCount := 0;
  while (LeadingSlashCount < Length(PathPart)) and
        (PathPart[LeadingSlashCount + 1] = '/') do
    Inc(LeadingSlashCount);
  Delete(PathPart, 1, LeadingSlashCount);
  if LeadingSlashCount >= 2 then
    ProtectedSegmentCount := 1
  else
    ProtectedSegmentCount := 0;

  ResultParts := TStringList.Create;
  try
    SegmentStart := 1;
    for I := 1 to Length(PathPart) + 1 do
    begin
      if (I <= Length(PathPart)) and (PathPart[I] <> '/') then
        Continue;
      Segment := Copy(PathPart, SegmentStart, I - SegmentStart);
      SegmentStart := I + 1;
      IsLastSegment := I > Length(PathPart);
      if Segment = '.' then
      begin
        if IsLastSegment then
          ResultParts.Add('');
        Continue;
      end;
      if Segment = '..' then
      begin
        if ResultParts.Count > ProtectedSegmentCount then
          ResultParts.Delete(ResultParts.Count - 1);
        if IsLastSegment then
          ResultParts.Add('');
        Continue;
      end;
      ResultParts.Add(Segment);
    end;
    Result := Prefix + StringOfChar('/', LeadingSlashCount);
    for I := 0 to ResultParts.Count - 1 do
    begin
      if I > 0 then
        Result := Result + '/';
      Result := Result + ResultParts[I];
    end;
    Result := Result + Suffix;
  finally
    ResultParts.Free;
  end;
end;

function ResolveRelativeVirtualAddress(const AModulePath,
  AImportingAddress: string): string;
var
  BaseAddress: string;
  DelimiterPosition, SuffixStart: SizeInt;
begin
  BaseAddress := AImportingAddress;
  if BaseAddress = '' then
    Exit(AModulePath);
  SuffixStart := FindAddressSuffixStart(BaseAddress);
  if SuffixStart > 0 then
    BaseAddress := Copy(BaseAddress, 1, SuffixStart - 1);
  DelimiterPosition := LastDelimiter('/', BaseAddress);
  if (DelimiterPosition = 0) and HasScheme(BaseAddress) then
    DelimiterPosition := Pos(':', BaseAddress);
  BaseAddress := Copy(BaseAddress, 1, DelimiterPosition);
  Result := NormalizeVirtualAddress(BaseAddress + AModulePath);
end;

function ParseVirtualModuleContentType(const AType: string;
  out AContentType: TGocciaVirtualModuleContentType): Boolean;
begin
  Result := True;
  if (Trim(AType) = '') or SameText(Trim(AType), 'javascript') or
     SameText(Trim(AType), 'typescript') then
    AContentType := vmctJavaScript
  else if SameText(Trim(AType), 'json') then
    AContentType := vmctJSON
  else if SameText(Trim(AType), 'text') then
    AContentType := vmctText
  else if SameText(Trim(AType), 'bytes') then
    AContentType := vmctBytes
  else
    Result := False;
end;

function VirtualModuleContentTypeName(
  const AContentType: TGocciaVirtualModuleContentType): string;
begin
  case AContentType of
    vmctJavaScript: Result := 'javascript';
    vmctJSON: Result := 'json';
    vmctText: Result := 'text';
    vmctBytes: Result := 'bytes';
  end;
end;

constructor TGocciaVirtualModuleDefinition.Create(const AAddress: string;
  const AContentType: TGocciaVirtualModuleContentType;
  const AText: UTF8String; const ABytes: TBytes;
  const AProvenance: string);
begin
  inherited Create;
  FAddress := AAddress;
  FContentType := AContentType;
  FText := AText;
  FBytes := Copy(ABytes, 0, Length(ABytes));
  FProvenance := AProvenance;
end;

function TGocciaVirtualModuleDefinition.Clone:
  TGocciaVirtualModuleDefinition;
begin
  Result := TGocciaVirtualModuleDefinition.Create(FAddress, FContentType,
    FText, FBytes, FProvenance);
end;

constructor TGocciaVirtualModuleRegistry.Create;
begin
  inherited Create;
  FDefinitions := TOrderedStringMap<TGocciaVirtualModuleDefinition>.Create;
end;

destructor TGocciaVirtualModuleRegistry.Destroy;
begin
  FreeDefinitions;
  FDefinitions.Free;
  inherited;
end;

procedure TGocciaVirtualModuleRegistry.FreeDefinitions;
var
  Pair: TOrderedStringMap<TGocciaVirtualModuleDefinition>.TKeyValuePair;
begin
  for Pair in FDefinitions do
    Pair.Value.Free;
  FDefinitions.Clear;
end;

function TGocciaVirtualModuleRegistry.CanonicalizeAddress(
  const AAddress, ABaseAddress: string): string;
var
  BaseDirectory: string;
begin
  if IsRelativeSpecifier(AAddress) then
  begin
    if HasScheme(ABaseAddress) then
      Exit(ResolveRelativeVirtualAddress(AAddress, ABaseAddress));
    BaseDirectory := ABaseAddress;
    if (BaseDirectory = '') or not DirectoryExists(BaseDirectory) then
      BaseDirectory := ExtractFilePath(BaseDirectory);
    if BaseDirectory = '' then
      BaseDirectory := GetCurrentDir;
    Exit(ExpandFileName(IncludeTrailingPathDelimiter(BaseDirectory) +
      AAddress));
  end;
  if IsAbsolutePath(AAddress) then
    Exit(ExpandFileName(AAddress));
  Result := NormalizeVirtualAddress(AAddress);
end;

function TGocciaVirtualModuleRegistry.CanonicalAddress(
  const AAddress, ABaseAddress: string): string;
begin
  Result := CanonicalizeAddress(AAddress, ABaseAddress);
end;

procedure TGocciaVirtualModuleRegistry.Add(const AAddress,
  ABaseAddress: string; const AContentType: TGocciaVirtualModuleContentType;
  const AText: UTF8String; const ABytes: TBytes;
  const AProvenance: string);
var
  CanonicalAddress: string;
  Existing, Definition: TGocciaVirtualModuleDefinition;
begin
  if Trim(AAddress) = '' then
    raise EArgumentException.Create('Virtual module address cannot be empty.');
  CanonicalAddress := CanonicalizeAddress(AAddress, ABaseAddress);
  if FDefinitions.TryGetValue(CanonicalAddress, Existing) then
  begin
    if Existing.Loaded then
      raise EInvalidOperation.CreateFmt(
        'Virtual module "%s" cannot be replaced after it has been loaded.',
        [CanonicalAddress]);
    Existing.Free;
    FDefinitions.Remove(CanonicalAddress);
  end;
  Definition := TGocciaVirtualModuleDefinition.Create(CanonicalAddress,
    AContentType, AText, ABytes, AProvenance);
  FDefinitions.Add(CanonicalAddress, Definition);
end;

procedure TGocciaVirtualModuleRegistry.AddText(const AAddress,
  ABaseAddress: string; const AContentType: TGocciaVirtualModuleContentType;
  const AContent: UTF8String; const AProvenance: string);
var
  Bytes: TBytes;
  Decoded: string;
begin
  SetLength(Bytes, 0);
  if AContentType = vmctBytes then
  begin
    try
      Decoded := DecodeStringBase64(AContent, True);
      Bytes := BytesOf(Decoded);
    except
      on E: Exception do
        raise EConvertError.CreateFmt(
          'Virtual module "%s" contains invalid base64: %s',
          [AAddress, E.Message]);
    end;
  end;
  Add(AAddress, ABaseAddress, AContentType, AContent, Bytes, AProvenance);
end;

procedure TGocciaVirtualModuleRegistry.CopyFrom(
  const ASource: TGocciaVirtualModuleRegistry);
var
  Pair: TOrderedStringMap<TGocciaVirtualModuleDefinition>.TKeyValuePair;
begin
  FreeDefinitions;
  if not Assigned(ASource) then
    Exit;
  for Pair in ASource.FDefinitions do
    FDefinitions.Add(Pair.Key, Pair.Value.Clone);
end;

function TGocciaVirtualModuleRegistry.Contains(
  const AAddress: string): Boolean;
begin
  Result := FDefinitions.ContainsKey(AAddress);
end;

function TGocciaVirtualModuleRegistry.GetContentType(const AAddress: string;
  out AContentType: TGocciaVirtualModuleContentType): Boolean;
var
  Definition: TGocciaVirtualModuleDefinition;
begin
  Result := FDefinitions.TryGetValue(AAddress, Definition);
  if Result then
    AContentType := Definition.ContentType;
end;

function TGocciaVirtualModuleRegistry.LoadContent(
  const AAddress: string): TGocciaModuleContent;
var
  Definition: TGocciaVirtualModuleDefinition;
begin
  if not FDefinitions.TryGetValue(AAddress, Definition) then
    raise EStreamError.Create('Virtual module not found: ' + AAddress);
  Definition.Loaded := True;
  Result := TGocciaModuleContent.Create(Definition.Text, 0);
end;

function TGocciaVirtualModuleRegistry.LoadContentBytes(
  const AAddress: string): TBytes;
var
  Definition: TGocciaVirtualModuleDefinition;
begin
  if not FDefinitions.TryGetValue(AAddress, Definition) then
    raise EStreamError.Create('Virtual module not found: ' + AAddress);
  Definition.Loaded := True;
  if Definition.ContentType = vmctBytes then
    Result := Copy(Definition.Bytes, 0, Length(Definition.Bytes))
  else
    Result := BytesOf(Definition.Text);
end;

function TGocciaVirtualModuleRegistry.Resolve(const AModulePath,
  AImportingAddress: string; out AResolvedAddress: string): Boolean;
var
  Candidate: string;
begin
  Candidate := AModulePath;
  if IsRelativeSpecifier(Candidate) then
  begin
    if HasScheme(AImportingAddress) or
       FDefinitions.ContainsKey(AImportingAddress) then
      Candidate := ResolveRelativeVirtualAddress(Candidate, AImportingAddress)
    else
      Candidate := ExpandFileName(ExtractFilePath(AImportingAddress) +
        Candidate);
  end
  else if IsAbsolutePath(Candidate) then
    Candidate := ExpandFileName(Candidate)
  else
    Candidate := NormalizeVirtualAddress(Candidate);
  AResolvedAddress := Candidate;
  Result := FDefinitions.ContainsKey(Candidate);
end;

function TGocciaVirtualModuleRegistry.TryGetDefinition(
  const AAddress: string;
  out ADefinition: TGocciaVirtualModuleDefinition): Boolean;
begin
  Result := FDefinitions.TryGetValue(AAddress, ADefinition);
end;

end.
