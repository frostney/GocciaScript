unit Goccia.SourceMap.Consumer;

{$I Goccia.inc}

// TC39 Source Map Specification — Source Map v3 consumer (reverse lookup)
// https://tc39.es/source-map-spec/

interface

uses
  Classes,
  SysUtils;

type
  TGocciaSourceMapConsumer = class
  private
    type
      TDecodedSegment = record
        GeneratedLine: Integer;   // 1-based
        GeneratedColumn: Integer; // 0-based
        SourceIndex: Integer;     // index into FSources (-1 = none)
        SourceLine: Integer;      // 0-based
        SourceColumn: Integer;    // 0-based
        NameIndex: Integer;       // index into FNames (-1 = none)
      end;
  private
    FSources: TStringList;
    FNames: TStringList;
    FSourceRoot: string;
    FSegments: array of TDecodedSegment;
    FSegmentCount: Integer;
    FSegmentCapacity: Integer;

    procedure GrowSegments;
    procedure ParseJSON(const AJSON: string);
    procedure DecodeMappings(const AMappings: string);
    function FindSegment(const AGeneratedLine, AGeneratedColumn: Integer): Integer;
    function ResolveSourcePath(const ASourcePath: string): string;
    function GetSourceCount: Integer;
    function GetNameCount: Integer;
  public
    constructor CreateFromJSON(const AJSON: string);
    constructor CreateFromFile(const APath: string);
    destructor Destroy; override;

    function Translate(const AGeneratedLine, AGeneratedColumn: Integer;
      out ASourceFile: string;
      out ASourceLine, ASourceColumn: Integer): Boolean;
    function TranslateWithName(const AGeneratedLine, AGeneratedColumn: Integer;
      out ASourceFile: string;
      out ASourceLine, ASourceColumn: Integer;
      out AName: string): Boolean;

    property SourceCount: Integer read GetSourceCount;
    property NameCount: Integer read GetNameCount;
    property SegmentCount: Integer read FSegmentCount;
  end;

implementation

uses
  Goccia.JSON,
  Goccia.SourceMap.VLQ,
  Goccia.Values.ArrayValue,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

const
  INITIAL_CAPACITY = 64;
  NO_INDEX = -1;

{ TGocciaSourceMapConsumer }

constructor TGocciaSourceMapConsumer.CreateFromJSON(const AJSON: string);
begin
  inherited Create;
  FSources := TStringList.Create;
  FNames := TStringList.Create;
  FSegmentCount := 0;
  FSegmentCapacity := INITIAL_CAPACITY;
  SetLength(FSegments, FSegmentCapacity);
  ParseJSON(AJSON);
end;

constructor TGocciaSourceMapConsumer.CreateFromFile(const APath: string);
var
  Lines: TStringList;
begin
  Lines := TStringList.Create;
  try
    Lines.LoadFromFile(APath);
    CreateFromJSON(Lines.Text);
  finally
    Lines.Free;
  end;
end;

destructor TGocciaSourceMapConsumer.Destroy;
begin
  FNames.Free;
  FSources.Free;
  inherited;
end;

function TGocciaSourceMapConsumer.GetSourceCount: Integer;
begin
  Result := FSources.Count;
end;

function TGocciaSourceMapConsumer.GetNameCount: Integer;
begin
  Result := FNames.Count;
end;

// TC39 Source Map Spec — resolve source path with sourceRoot prefix.
// Absolute paths (starting with / or containing ://) are returned as-is.
function TGocciaSourceMapConsumer.ResolveSourcePath(const ASourcePath: string): string;
begin
  if FSourceRoot = '' then
    Exit(ASourcePath);
  if (Length(ASourcePath) > 0) and (ASourcePath[1] = '/') then
    Exit(ASourcePath);
  if Pos('://', ASourcePath) > 0 then
    Exit(ASourcePath);
  Result := FSourceRoot + ASourcePath;
end;

procedure TGocciaSourceMapConsumer.GrowSegments;
begin
  FSegmentCapacity := FSegmentCapacity * 2;
  SetLength(FSegments, FSegmentCapacity);
end;

// Parse the source map JSON using the project's existing TGocciaJSONParser.
procedure TGocciaSourceMapConsumer.ParseJSON(const AJSON: string);
var
  Parser: TGocciaJSONParser;
  Root, PropValue, Elem: TGocciaValue;
  Obj: TGocciaObjectValue;
  Arr: TGocciaArrayValue;
  I: Integer;
begin
  Parser := TGocciaJSONParser.Create;
  try
    Root := Parser.Parse(UTF8String(AJSON));
  finally
    Parser.Free;
  end;

  if not (Root is TGocciaObjectValue) then
    raise Exception.Create('Source map must be a JSON object');
  Obj := TGocciaObjectValue(Root);

  // Extract sourceRoot
  PropValue := Obj.GetProperty('sourceRoot');
  if Assigned(PropValue) and (PropValue is TGocciaStringLiteralValue) then
  begin
    FSourceRoot := TGocciaStringLiteralValue(PropValue).Value;
    // Ensure trailing separator for non-empty roots
    if (FSourceRoot <> '') and (FSourceRoot[Length(FSourceRoot)] <> '/') then
      FSourceRoot := FSourceRoot + '/';
  end;

  // Extract sources array
  PropValue := Obj.GetProperty('sources');
  if Assigned(PropValue) and (PropValue is TGocciaArrayValue) then
  begin
    Arr := TGocciaArrayValue(PropValue);
    for I := 0 to Arr.Elements.Count - 1 do
    begin
      Elem := Arr.Elements[I];
      if Elem is TGocciaStringLiteralValue then
        FSources.Add(TGocciaStringLiteralValue(Elem).Value)
      else
        FSources.Add('');
    end;
  end;

  // Extract names array
  PropValue := Obj.GetProperty('names');
  if Assigned(PropValue) and (PropValue is TGocciaArrayValue) then
  begin
    Arr := TGocciaArrayValue(PropValue);
    for I := 0 to Arr.Elements.Count - 1 do
    begin
      Elem := Arr.Elements[I];
      if Elem is TGocciaStringLiteralValue then
        FNames.Add(TGocciaStringLiteralValue(Elem).Value)
      else
        FNames.Add('');
    end;
  end;

  // Decode mappings string
  PropValue := Obj.GetProperty('mappings');
  if Assigned(PropValue) and (PropValue is TGocciaStringLiteralValue) then
  begin
    if TGocciaStringLiteralValue(PropValue).Value <> '' then
      DecodeMappings(TGocciaStringLiteralValue(PropValue).Value);
  end;
end;

// TC39 Source Map Spec — Decode the VLQ-encoded mappings string
procedure TGocciaSourceMapConsumer.DecodeMappings(const AMappings: string);
var
  Pos: Integer;
  CurrentLine: Integer;
  PrevGeneratedColumn: Integer;
  PrevSourceIndex: Integer;
  PrevSourceLine: Integer;
  PrevSourceColumn: Integer;
  PrevNameIndex: Integer;
  Seg: TDecodedSegment;
begin
  Pos := 1;
  CurrentLine := 1;
  PrevGeneratedColumn := 0;
  PrevSourceIndex := 0;
  PrevSourceLine := 0;
  PrevSourceColumn := 0;
  PrevNameIndex := 0;

  while Pos <= Length(AMappings) do
  begin
    if AMappings[Pos] = ';' then
    begin
      Inc(CurrentLine);
      PrevGeneratedColumn := 0;
      Inc(Pos);
      Continue;
    end;

    if AMappings[Pos] = ',' then
    begin
      Inc(Pos);
      Continue;
    end;

    // Decode a segment
    Seg.GeneratedLine := CurrentLine;
    Seg.SourceIndex := NO_INDEX;
    Seg.SourceLine := 0;
    Seg.SourceColumn := 0;
    Seg.NameIndex := NO_INDEX;

    // Field 1: generated column (delta)
    Seg.GeneratedColumn := PrevGeneratedColumn + DecodeVLQ(AMappings, Pos);
    PrevGeneratedColumn := Seg.GeneratedColumn;

    // Check if there are more fields before a separator
    if (Pos <= Length(AMappings)) and not (AMappings[Pos] in [',', ';']) then
    begin
      // Field 2: source index (delta)
      Seg.SourceIndex := PrevSourceIndex + DecodeVLQ(AMappings, Pos);
      PrevSourceIndex := Seg.SourceIndex;

      // Field 3: source line (delta)
      Seg.SourceLine := PrevSourceLine + DecodeVLQ(AMappings, Pos);
      PrevSourceLine := Seg.SourceLine;

      // Field 4: source column (delta)
      Seg.SourceColumn := PrevSourceColumn + DecodeVLQ(AMappings, Pos);
      PrevSourceColumn := Seg.SourceColumn;

      // Field 5 (optional): name index (delta)
      if (Pos <= Length(AMappings)) and not (AMappings[Pos] in [',', ';']) then
      begin
        Seg.NameIndex := PrevNameIndex + DecodeVLQ(AMappings, Pos);
        PrevNameIndex := Seg.NameIndex;
      end;
    end;

    // Store the decoded segment
    if FSegmentCount >= FSegmentCapacity then
      GrowSegments;
    FSegments[FSegmentCount] := Seg;
    Inc(FSegmentCount);
  end;
end;

// Binary search for the best matching segment at the given generated position.
// Returns the index of the closest segment on or before the given position,
// or -1 if no match is found.
function TGocciaSourceMapConsumer.FindSegment(
  const AGeneratedLine, AGeneratedColumn: Integer): Integer;
var
  Lo, Hi, Mid, I, BestIndex: Integer;
  LineStart: Integer;
begin
  Result := -1;
  if FSegmentCount = 0 then
    Exit;

  // Binary search for the first segment on AGeneratedLine
  Lo := 0;
  Hi := FSegmentCount - 1;
  LineStart := -1;
  while Lo <= Hi do
  begin
    Mid := (Lo + Hi) div 2;
    if FSegments[Mid].GeneratedLine < AGeneratedLine then
      Lo := Mid + 1
    else if FSegments[Mid].GeneratedLine > AGeneratedLine then
      Hi := Mid - 1
    else
    begin
      LineStart := Mid;
      Hi := Mid - 1;
    end;
  end;

  if LineStart < 0 then
  begin
    // No exact line match — find the closest preceding line
    BestIndex := -1;
    for I := FSegmentCount - 1 downto 0 do
      if FSegments[I].GeneratedLine <= AGeneratedLine then
      begin
        BestIndex := I;
        Break;
      end;
    Exit(BestIndex);
  end;

  // Find the best matching column within the line — only select segments
  // whose generated column is at or before the requested column.
  BestIndex := -1;
  I := LineStart;
  while (I < FSegmentCount) and (FSegments[I].GeneratedLine = AGeneratedLine) do
  begin
    if FSegments[I].GeneratedColumn <= AGeneratedColumn then
      BestIndex := I
    else
      Break;
    Inc(I);
  end;

  // If no segment on this line starts at or before the column, fall back
  // to the last segment on the preceding line.
  if BestIndex >= 0 then
    Exit(BestIndex);
  if LineStart > 0 then
    Exit(LineStart - 1);
  Result := -1;
end;

function TGocciaSourceMapConsumer.Translate(
  const AGeneratedLine, AGeneratedColumn: Integer;
  out ASourceFile: string;
  out ASourceLine, ASourceColumn: Integer): Boolean;
var
  Idx: Integer;
begin
  Result := False;
  ASourceFile := '';
  ASourceLine := 0;
  ASourceColumn := 0;

  Idx := FindSegment(AGeneratedLine, AGeneratedColumn);
  if Idx < 0 then
    Exit;
  if FSegments[Idx].SourceIndex = NO_INDEX then
    Exit;
  if (FSegments[Idx].SourceIndex < 0) or
     (FSegments[Idx].SourceIndex >= FSources.Count) then
    Exit;

  ASourceFile := ResolveSourcePath(FSources[FSegments[Idx].SourceIndex]);
  ASourceLine := FSegments[Idx].SourceLine;
  ASourceColumn := FSegments[Idx].SourceColumn;
  Result := True;
end;

function TGocciaSourceMapConsumer.TranslateWithName(
  const AGeneratedLine, AGeneratedColumn: Integer;
  out ASourceFile: string;
  out ASourceLine, ASourceColumn: Integer;
  out AName: string): Boolean;
var
  Idx: Integer;
begin
  AName := '';
  Result := Translate(AGeneratedLine, AGeneratedColumn, ASourceFile,
    ASourceLine, ASourceColumn);
  if not Result then
    Exit;

  Idx := FindSegment(AGeneratedLine, AGeneratedColumn);
  if (Idx >= 0) and (FSegments[Idx].NameIndex <> NO_INDEX) and
     (FSegments[Idx].NameIndex >= 0) and (FSegments[Idx].NameIndex < FNames.Count) then
    AName := FNames[FSegments[Idx].NameIndex];
end;

end.
