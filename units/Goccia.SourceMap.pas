unit Goccia.SourceMap;

{$I Goccia.inc}

// TC39 Source Map Specification — Source Map v3 builder, serializer, and translator
// https://tc39.es/source-map-spec/

interface

uses
  Classes,
  SysUtils;

type
  TGocciaSourceMapSegment = record
    GeneratedLine: Integer;     // 1-based (converted to 0-based on encode)
    GeneratedColumn: Integer;   // 0-based
    SourceIndex: Integer;       // index into Sources (-1 = no source)
    SourceLine: Integer;        // 0-based
    SourceColumn: Integer;      // 0-based
    NameIndex: Integer;         // index into Names (-1 = no name)
  end;

  TGocciaSourceMap = class
  private
    const
      SOURCE_MAP_VERSION = 3;
      INITIAL_SEGMENT_CAPACITY = 64;
      NO_INDEX = -1;
  private
    FVersion: Integer;
    FFile: string;
    FSourceRoot: string;
    FSources: TStringList;
    FSourcesContent: TStringList;
    FNames: TStringList;
    FSegments: array of TGocciaSourceMapSegment;
    FSegmentCount: Integer;
    FSegmentCapacity: Integer;

    procedure GrowSegments;
    function FindLineStart(const AGeneratedLine: Integer): Integer;
    function EncodeMappings: string;
    function EscapeJSONString(const AValue: string): string;
  public
    constructor Create(const AFile: string = '');
    destructor Destroy; override;

    function AddSource(const ASourcePath: string): Integer;
    procedure SetSourcePath(const ASourceIndex: Integer; const ASourcePath: string);
    procedure SetSourceContent(const ASourceIndex: Integer; const AContent: string);
    function AddName(const AName: string): Integer;

    procedure AddMapping(
      const AGeneratedLine, AGeneratedColumn: Integer;
      const ASourceIndex, ASourceLine, ASourceColumn: Integer;
      const ANameIndex: Integer = -1
    );

    function Translate(const ALine, AColumn: Integer;
      out ASourceLine, ASourceColumn: Integer): Boolean;

    function ToJSON: string;
    procedure SaveToFile(const APath: string);
    function ToInlineComment: string;

    property Version: Integer read FVersion;
    property FileName: string read FFile write FFile;
    property SourceRoot: string read FSourceRoot write FSourceRoot;
    property SegmentCount: Integer read FSegmentCount;
  end;

implementation

uses
  StringBuffer,

  Goccia.SourceMap.VLQ;

{ TGocciaSourceMap }

constructor TGocciaSourceMap.Create(const AFile: string);
begin
  inherited Create;
  FVersion := SOURCE_MAP_VERSION;
  FFile := AFile;
  FSourceRoot := '';
  FSources := TStringList.Create;
  FSourcesContent := TStringList.Create;
  FNames := TStringList.Create;
  FSegmentCount := 0;
  FSegmentCapacity := INITIAL_SEGMENT_CAPACITY;
  SetLength(FSegments, FSegmentCapacity);
end;

destructor TGocciaSourceMap.Destroy;
begin
  FNames.Free;
  FSourcesContent.Free;
  FSources.Free;
  inherited;
end;

procedure TGocciaSourceMap.GrowSegments;
begin
  FSegmentCapacity := FSegmentCapacity * 2;
  SetLength(FSegments, FSegmentCapacity);
end;

function TGocciaSourceMap.AddSource(const ASourcePath: string): Integer;
var
  I: Integer;
begin
  // Deduplicate: return existing index if already present
  for I := 0 to FSources.Count - 1 do
    if FSources[I] = ASourcePath then
      Exit(I);
  Result := FSources.Add(ASourcePath);
  // Ensure sourcesContent list stays in sync (empty string = no content)
  while FSourcesContent.Count <= Result do
    FSourcesContent.Add('');
end;

procedure TGocciaSourceMap.SetSourcePath(const ASourceIndex: Integer;
  const ASourcePath: string);
begin
  if (ASourceIndex < 0) or (ASourceIndex >= FSources.Count) then
    raise ERangeError.CreateFmt('Source index %d out of range', [ASourceIndex]);
  FSources[ASourceIndex] := ASourcePath;
end;

procedure TGocciaSourceMap.SetSourceContent(const ASourceIndex: Integer;
  const AContent: string);
begin
  if (ASourceIndex < 0) or (ASourceIndex >= FSources.Count) then
    raise ERangeError.CreateFmt('Source index %d out of range', [ASourceIndex]);
  while FSourcesContent.Count <= ASourceIndex do
    FSourcesContent.Add('');
  FSourcesContent[ASourceIndex] := AContent;
end;

function TGocciaSourceMap.AddName(const AName: string): Integer;
var
  I: Integer;
begin
  // Deduplicate
  for I := 0 to FNames.Count - 1 do
    if FNames[I] = AName then
      Exit(I);
  Result := FNames.Add(AName);
end;

procedure TGocciaSourceMap.AddMapping(
  const AGeneratedLine, AGeneratedColumn: Integer;
  const ASourceIndex, ASourceLine, ASourceColumn: Integer;
  const ANameIndex: Integer);
begin
  if FSegmentCount >= FSegmentCapacity then
    GrowSegments;
  FSegments[FSegmentCount].GeneratedLine := AGeneratedLine;
  FSegments[FSegmentCount].GeneratedColumn := AGeneratedColumn;
  FSegments[FSegmentCount].SourceIndex := ASourceIndex;
  FSegments[FSegmentCount].SourceLine := ASourceLine;
  FSegments[FSegmentCount].SourceColumn := ASourceColumn;
  FSegments[FSegmentCount].NameIndex := ANameIndex;
  Inc(FSegmentCount);
end;

// Binary search for the first segment on AGeneratedLine.
// Returns the index of the first match, or -1 if no segments exist on that line.
function TGocciaSourceMap.FindLineStart(const AGeneratedLine: Integer): Integer;
var
  Lo, Hi, Mid: Integer;
begin
  Lo := 0;
  Hi := FSegmentCount - 1;
  Result := -1;
  while Lo <= Hi do
  begin
    Mid := (Lo + Hi) div 2;
    if FSegments[Mid].GeneratedLine < AGeneratedLine then
      Lo := Mid + 1
    else if FSegments[Mid].GeneratedLine > AGeneratedLine then
      Hi := Mid - 1
    else
    begin
      Result := Mid;
      Hi := Mid - 1; // continue searching leftward for the first occurrence
    end;
  end;
end;

// Translate a generated position back to the original source position.
// Uses the same algorithm as the previous TGocciaJSXSourceMap.Translate.
function TGocciaSourceMap.Translate(const ALine, AColumn: Integer;
  out ASourceLine, ASourceColumn: Integer): Boolean;
var
  I, BestIndex, LineStart: Integer;
begin
  Result := False;
  if FSegmentCount = 0 then
    Exit;

  LineStart := FindLineStart(ALine);

  if LineStart < 0 then
  begin
    // No exact line match — find the closest preceding mapping
    BestIndex := -1;
    for I := FSegmentCount - 1 downto 0 do
    begin
      if FSegments[I].GeneratedLine <= ALine then
      begin
        BestIndex := I;
        Break;
      end;
    end;
    if BestIndex < 0 then
      Exit;
    // SourceLine/SourceColumn are 0-based in segments, but callers use
    // 1-based line numbers (matching the JSX transformer convention)
    ASourceLine := FSegments[BestIndex].SourceLine + (ALine - FSegments[BestIndex].GeneratedLine);
    ASourceColumn := AColumn;
    Result := True;
    Exit;
  end;

  // Find the best matching column within the line
  BestIndex := LineStart;
  I := LineStart;
  while (I < FSegmentCount) and (FSegments[I].GeneratedLine = ALine) do
  begin
    if FSegments[I].GeneratedColumn <= AColumn then
      BestIndex := I
    else
      Break;
    Inc(I);
  end;

  ASourceLine := FSegments[BestIndex].SourceLine;
  ASourceColumn := FSegments[BestIndex].SourceColumn + (AColumn - FSegments[BestIndex].GeneratedColumn);
  Result := True;
end;

function TGocciaSourceMap.EscapeJSONString(const AValue: string): string;
var
  Buffer: TStringBuffer;
  I: Integer;
  Ch: Char;
begin
  Buffer := TStringBuffer.Create(Length(AValue));
  for I := 1 to Length(AValue) do
  begin
    Ch := AValue[I];
    case Ch of
      '"':  Buffer.Append('\"');
      '\':  Buffer.Append('\\');
      '/':  Buffer.Append('\/');
      #8:   Buffer.Append('\b');
      #9:   Buffer.Append('\t');
      #10:  Buffer.Append('\n');
      #12:  Buffer.Append('\f');
      #13:  Buffer.Append('\r');
    else
      if Ord(Ch) < 32 then
      begin
        Buffer.Append('\u');
        Buffer.Append(IntToHex(Ord(Ch), 4));
      end
      else
        Buffer.AppendChar(Ch);
    end;
  end;
  Result := Buffer.ToString;
end;

// TC39 Source Map Spec — Mappings encoding
// Each segment encodes up to 5 VLQ fields, delta-encoded relative to the
// previous segment. Segments within the same generated line are separated by
// commas; lines are separated by semicolons.
function TGocciaSourceMap.EncodeMappings: string;
var
  Buffer: TStringBuffer;
  I: Integer;
  PrevGeneratedColumn: Integer;
  PrevSourceIndex: Integer;
  PrevSourceLine: Integer;
  PrevSourceColumn: Integer;
  PrevNameIndex: Integer;
  CurrentLine: Integer;
  FirstInLine: Boolean;
  Seg: TGocciaSourceMapSegment;
begin
  Buffer := TStringBuffer.Create(FSegmentCount * 8); // reasonable estimate
  PrevGeneratedColumn := 0;
  PrevSourceIndex := 0;
  PrevSourceLine := 0;
  PrevSourceColumn := 0;
  PrevNameIndex := 0;
  CurrentLine := 1;
  FirstInLine := True;

  for I := 0 to FSegmentCount - 1 do
  begin
    Seg := FSegments[I];

    // Emit semicolons for empty generated lines
    while CurrentLine < Seg.GeneratedLine do
    begin
      Buffer.AppendChar(';');
      Inc(CurrentLine);
      PrevGeneratedColumn := 0;
      FirstInLine := True;
    end;

    if not FirstInLine then
      Buffer.AppendChar(',');
    FirstInLine := False;

    // Field 1: generated column (delta)
    Buffer.Append(EncodeVLQ(Seg.GeneratedColumn - PrevGeneratedColumn));
    PrevGeneratedColumn := Seg.GeneratedColumn;

    if Seg.SourceIndex <> NO_INDEX then
    begin
      // Field 2: source index (delta)
      Buffer.Append(EncodeVLQ(Seg.SourceIndex - PrevSourceIndex));
      PrevSourceIndex := Seg.SourceIndex;

      // Field 3: source line (delta)
      Buffer.Append(EncodeVLQ(Seg.SourceLine - PrevSourceLine));
      PrevSourceLine := Seg.SourceLine;

      // Field 4: source column (delta)
      Buffer.Append(EncodeVLQ(Seg.SourceColumn - PrevSourceColumn));
      PrevSourceColumn := Seg.SourceColumn;

      if Seg.NameIndex <> NO_INDEX then
      begin
        // Field 5: name index (delta)
        Buffer.Append(EncodeVLQ(Seg.NameIndex - PrevNameIndex));
        PrevNameIndex := Seg.NameIndex;
      end;
    end;
  end;

  Result := Buffer.ToString;
end;

// TC39 Source Map Spec — JSON serialization (version 3)
function TGocciaSourceMap.ToJSON: string;
var
  Buffer: TStringBuffer;
  I: Integer;
  HasContent: Boolean;
begin
  Buffer := TStringBuffer.Create(256);

  Buffer.Append('{"version":');
  Buffer.Append(IntToStr(FVersion));

  if FFile <> '' then
  begin
    Buffer.Append(',"file":"');
    Buffer.Append(EscapeJSONString(FFile));
    Buffer.AppendChar('"');
  end;

  if FSourceRoot <> '' then
  begin
    Buffer.Append(',"sourceRoot":"');
    Buffer.Append(EscapeJSONString(FSourceRoot));
    Buffer.AppendChar('"');
  end;

  // Sources array
  Buffer.Append(',"sources":[');
  for I := 0 to FSources.Count - 1 do
  begin
    if I > 0 then
      Buffer.AppendChar(',');
    Buffer.AppendChar('"');
    Buffer.Append(EscapeJSONString(FSources[I]));
    Buffer.AppendChar('"');
  end;
  Buffer.AppendChar(']');

  // sourcesContent array — only include if at least one source has content
  HasContent := False;
  for I := 0 to FSourcesContent.Count - 1 do
    if FSourcesContent[I] <> '' then
    begin
      HasContent := True;
      Break;
    end;

  if HasContent then
  begin
    Buffer.Append(',"sourcesContent":[');
    for I := 0 to FSourcesContent.Count - 1 do
    begin
      if I > 0 then
        Buffer.AppendChar(',');
      if FSourcesContent[I] <> '' then
      begin
        Buffer.AppendChar('"');
        Buffer.Append(EscapeJSONString(FSourcesContent[I]));
        Buffer.AppendChar('"');
      end
      else
        Buffer.Append('null');
    end;
    Buffer.AppendChar(']');
  end;

  // Names array
  Buffer.Append(',"names":[');
  for I := 0 to FNames.Count - 1 do
  begin
    if I > 0 then
      Buffer.AppendChar(',');
    Buffer.AppendChar('"');
    Buffer.Append(EscapeJSONString(FNames[I]));
    Buffer.AppendChar('"');
  end;
  Buffer.AppendChar(']');

  // Mappings string
  Buffer.Append(',"mappings":"');
  Buffer.Append(EncodeMappings);
  Buffer.AppendChar('"');

  Buffer.AppendChar('}');
  Result := Buffer.ToString;
end;

procedure TGocciaSourceMap.SaveToFile(const APath: string);
var
  Output: TStringList;
begin
  Output := TStringList.Create;
  try
    Output.Text := ToJSON;
    Output.SaveToFile(APath);
  finally
    Output.Free;
  end;
end;

// Inline source map as a data URI comment
function TGocciaSourceMap.ToInlineComment: string;
var
  JSON: string;
  Encoded: string;
  I: Integer;
  B: Byte;
  Remaining, TripleCount, J: Integer;
  A0, A1, A2: Byte;
const
  BASE64_TABLE: string = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';
begin
  JSON := ToJSON;

  // Base64 encode the JSON
  Remaining := Length(JSON) mod 3;
  TripleCount := Length(JSON) div 3;
  Encoded := '';
  J := 1;
  for I := 1 to TripleCount do
  begin
    A0 := Ord(JSON[J]);
    A1 := Ord(JSON[J + 1]);
    A2 := Ord(JSON[J + 2]);
    Encoded := Encoded +
      BASE64_TABLE[(A0 shr 2) + 1] +
      BASE64_TABLE[((A0 and 3) shl 4 or (A1 shr 4)) + 1] +
      BASE64_TABLE[((A1 and 15) shl 2 or (A2 shr 6)) + 1] +
      BASE64_TABLE[(A2 and 63) + 1];
    Inc(J, 3);
  end;

  if Remaining = 1 then
  begin
    A0 := Ord(JSON[J]);
    Encoded := Encoded +
      BASE64_TABLE[(A0 shr 2) + 1] +
      BASE64_TABLE[((A0 and 3) shl 4) + 1] +
      '==';
  end
  else if Remaining = 2 then
  begin
    A0 := Ord(JSON[J]);
    A1 := Ord(JSON[J + 1]);
    Encoded := Encoded +
      BASE64_TABLE[(A0 shr 2) + 1] +
      BASE64_TABLE[((A0 and 3) shl 4 or (A1 shr 4)) + 1] +
      BASE64_TABLE[((A1 and 15) shl 2) + 1] +
      '=';
  end;

  Result := '//# sourceMappingURL=data:application/json;charset=utf-8;base64,' + Encoded;
end;

end.
