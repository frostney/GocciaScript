unit StringJoin;

{$I Shared.inc}

// Joins a list of strings with a separator between each two, as
// Array.prototype.join does, in time linear in the result, where appending to
// a string reallocates it on every append.
//
// Short strings and separators are copied into chunks of at most
// MAXIMUM_CHUNK_LENGTH characters, which grow by doubling from
// MINIMUM_CHUNK_LENGTH, so a join keeps nothing per element. A long string is
// kept as it is, not copied. Join then writes the result once, at its exact
// length, and releases each chunk and string as it copies it. A join whose
// only content is one long string returns that string itself, as the appends
// did.

interface

type
  // Called between blocks of the final copy, so that a host's deadline or
  // abort can end a join whose result is long.
  TStringJoinPoll = procedure;

  TStringJoinSegment = record
    Text: UnicodeString;
    // The characters of Text in use: all of a long string, the filled part of
    // a chunk.
    Length: SizeInt;
  end;

  TStringJoinBuffer = class
  private
    FSeparator: UnicodeString;
    FPoll: TStringJoinPoll;
    FSegments: array of TStringJoinSegment;
    FSegmentCount: SizeInt;
    FChunk: UnicodeString;
    FChunkLength: SizeInt;
    FChunkCapacity: SizeInt;
    FTotalLength: SizeInt;
    FElementCount: SizeInt;
    procedure AddSegment(const AText: UnicodeString; const ALength: SizeInt);
    procedure CloseChunk;
    procedure WriteToChunk(const AText: UnicodeString; const ALength: SizeInt);
    procedure WriteText(const AText: UnicodeString);
    procedure Reset;
  public
    constructor Create(const ASeparator: UnicodeString;
      const APoll: TStringJoinPoll = nil);
    // Adds the next element's string; an empty one still takes its place
    // between two separators.
    procedure Add(const AText: UnicodeString);
    // The joined string. The buffer is empty afterwards.
    function Join: UnicodeString;
    property ElementCount: SizeInt read FElementCount;
  end;

const
  // A string at least this long is kept rather than copied into a chunk.
  LONG_TEXT_LENGTH = 256;
  MINIMUM_CHUNK_LENGTH = 256;
  MAXIMUM_CHUNK_LENGTH = 32768;
  // Characters the final copy writes between two polls.
  POLL_INTERVAL_LENGTH = 1024 * 1024;

implementation

const
  // Characters, so that the byte size of the result fits a SizeInt too.
  MAXIMUM_LENGTH = High(SizeInt) div (2 * SizeOf(WideChar));
  INITIAL_SEGMENT_CAPACITY = 8;

constructor TStringJoinBuffer.Create(const ASeparator: UnicodeString;
  const APoll: TStringJoinPoll);
begin
  inherited Create;
  FSeparator := ASeparator;
  FPoll := APoll;
end;

procedure TStringJoinBuffer.AddSegment(const AText: UnicodeString;
  const ALength: SizeInt);
begin
  if FSegmentCount = System.Length(FSegments) then
  begin
    if FSegmentCount = 0 then
      SetLength(FSegments, INITIAL_SEGMENT_CAPACITY)
    else
      SetLength(FSegments, FSegmentCount * 2);
  end;
  FSegments[FSegmentCount].Text := AText;
  FSegments[FSegmentCount].Length := ALength;
  Inc(FSegmentCount);
end;

procedure TStringJoinBuffer.CloseChunk;
begin
  if FChunkLength > 0 then
    AddSegment(FChunk, FChunkLength);
  // The segment holds the chunk now; the next chunk is a new block.
  FChunk := '';
  FChunkLength := 0;
  FChunkCapacity := 0;
end;

procedure TStringJoinBuffer.WriteToChunk(const AText: UnicodeString;
  const ALength: SizeInt);
var
  NewCapacity: SizeInt;
begin
  if FChunkLength + ALength > FChunkCapacity then
  begin
    if FChunkCapacity >= MAXIMUM_CHUNK_LENGTH then
      CloseChunk;
    if FChunkCapacity = 0 then
      NewCapacity := MINIMUM_CHUNK_LENGTH
    else
      NewCapacity := FChunkCapacity * 2;
    if NewCapacity > MAXIMUM_CHUNK_LENGTH then
      NewCapacity := MAXIMUM_CHUNK_LENGTH;
    if NewCapacity < FChunkLength + ALength then
      NewCapacity := FChunkLength + ALength;
    // FChunk is never shared until CloseChunk hands it to a segment, so this
    // grows it in its own block.
    SetLength(FChunk, NewCapacity);
    FChunkCapacity := NewCapacity;
  end;
  Move(PWideChar(AText)^, (PWideChar(FChunk) + FChunkLength)^,
    ALength * SizeOf(WideChar));
  Inc(FChunkLength, ALength);
end;

procedure TStringJoinBuffer.WriteText(const AText: UnicodeString);
var
  TextLength: SizeInt;
begin
  TextLength := System.Length(AText);
  if TextLength = 0 then
    Exit;
  if TextLength > MAXIMUM_LENGTH - FTotalLength then
    System.Error(reOutOfMemory);
  if TextLength >= LONG_TEXT_LENGTH then
  begin
    CloseChunk;
    AddSegment(AText, TextLength);
  end
  else
    WriteToChunk(AText, TextLength);
  Inc(FTotalLength, TextLength);
end;

procedure TStringJoinBuffer.Add(const AText: UnicodeString);
begin
  if FElementCount > 0 then
    WriteText(FSeparator);
  WriteText(AText);
  Inc(FElementCount);
end;

procedure TStringJoinBuffer.Reset;
begin
  FChunk := '';
  FChunkLength := 0;
  FChunkCapacity := 0;
  FSegmentCount := 0;
  FTotalLength := 0;
  FElementCount := 0;
end;

function TStringJoinBuffer.Join: UnicodeString;
var
  I, Copied, BlockLength, SinceLastPoll: SizeInt;
  Source, Target: PWideChar;
begin
  CloseChunk;
  if FSegmentCount = 0 then
  begin
    Reset;
    Exit('');
  end;
  if (FSegmentCount = 1) and
     (FSegments[0].Length = System.Length(FSegments[0].Text)) then
  begin
    Result := FSegments[0].Text;
    FSegments[0].Text := '';
    Reset;
    Exit;
  end;

  Result := '';
  SetLength(Result, FTotalLength);
  Target := PWideChar(Result);
  SinceLastPoll := 0;
  for I := 0 to FSegmentCount - 1 do
  begin
    Source := PWideChar(FSegments[I].Text);
    Copied := 0;
    while Copied < FSegments[I].Length do
    begin
      BlockLength := FSegments[I].Length - Copied;
      if BlockLength > POLL_INTERVAL_LENGTH - SinceLastPoll then
        BlockLength := POLL_INTERVAL_LENGTH - SinceLastPoll;
      Move(Source^, Target^, BlockLength * SizeOf(WideChar));
      Inc(Source, BlockLength);
      Inc(Target, BlockLength);
      Inc(Copied, BlockLength);
      Inc(SinceLastPoll, BlockLength);
      if SinceLastPoll >= POLL_INTERVAL_LENGTH then
      begin
        SinceLastPoll := 0;
        if Assigned(FPoll) then
          FPoll;
      end;
    end;
    // Released while it is still in the cache; the result holds a copy.
    FSegments[I].Text := '';
  end;
  Reset;
end;

end.
