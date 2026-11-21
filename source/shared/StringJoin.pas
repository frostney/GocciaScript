unit StringJoin;

{$I Shared.inc}
{$POINTERMATH ON}

// Joins a list of strings with a separator between each two, as
// Array.prototype.join does, in two passes: Add keeps a reference to each
// non-empty string, and Join sums their lengths, allocates the result once at
// its exact size and copies the strings and separators into it. The work is
// linear in the result, where appending to a string reallocates it on every
// append. The memory is the result and the strings it is made of, plus one
// reference per non-empty element; separators and empty elements take none,
// and a join whose only content is one string returns that string itself.
//
// The buffer holds no managed field, so a local one costs nothing to set up
// or tear down: it keeps its references by hand and releases them in Join or
// Release. Call Release in a finally block around the Adds and the Join, so
// that an exception between them does not leak the references. The separator
// is not held: the string passed to Init must outlive the buffer. The first
// INLINE_PART_COUNT references are held in the record, so a small join
// allocates nothing but its result. Init records where the buffer lives, and
// a copy refuses to be used, because it would share the original's storage.
//
// A poll, when given, is called between blocks of the final copy, so that a
// host's deadline or abort can end a join whose result is long. A gate, when
// given, is asked about the result's bytes before the result is allocated,
// and about a result too long for any string before the join gives up.

interface

uses
  SysUtils;

const
  // The longest result, in characters, so that its byte size fits a SizeInt
  // with room for the string's header on every target.
  MAXIMUM_JOIN_LENGTH = High(SizeInt) div (2 * SizeOf(WideChar));
  INLINE_PART_COUNT = 8;
  // The most elements a caller joins by appending instead: below about this
  // many short strings, the two passes cost more than the appends' copies.
  SMALL_JOIN_LENGTH = 16;
  // Characters the final copy writes between two polls.
  POLL_INTERVAL_LENGTH = 1024 * 1024;

type
  // Raised by a copy of a TStringJoinBuffer asked to add or join.
  EStringJoinCopied = class(Exception);

  TStringJoinPoll = procedure;
  // Raises unless a result of AResultBytes may be allocated. AResultBytes can
  // exceed MAXIMUM_JOIN_LENGTH characters, which no string can hold.
  TStringJoinGate = procedure(const AResultBytes: Int64);

  // A run of empty elements, before the part at PartIndex.
  TStringJoinGap = record
    PartIndex: SizeInt;
    EmptyCount: SizeInt;
  end;
  PStringJoinGap = ^TStringJoinGap;

  TStringJoinBuffer = record
  private
    FSelf: Pointer;
    FSeparator: PWideChar;
    FSeparatorLength: SizeInt;
    // References held by hand: each is a UnicodeString's data pointer, with
    // its reference count raised by one.
    FInlineParts: array[0..INLINE_PART_COUNT - 1] of Pointer;
    FParts: PPointer;
    FPartCapacity: SizeInt;
    FPoll: TStringJoinPoll;
    FGate: TStringJoinGate;
    FPartCount: SizeInt;
    FGaps: PStringJoinGap;
    FGapCapacity: SizeInt;
    FGapCount: SizeInt;
    FPendingEmptyCount: SizeInt;
    FElementCount: SizeInt;
    FPartLength: SizeInt;
    procedure CheckInPlace; {$IFDEF FPC}inline;{$ENDIF}
    function PartSlot(const AIndex: SizeInt): PPointer; {$IFDEF FPC}inline;{$ENDIF}
    procedure AddGap;
    procedure GrowParts;
    procedure RefuseLength(const ALength: Int64);
    procedure ReleaseParts;
  public
    procedure Init(const ASeparator: UnicodeString;
      const APoll: TStringJoinPoll = nil; const AGate: TStringJoinGate = nil);
    // Adds the next element's string; an empty one still takes its place
    // between two separators.
    procedure Add(const AText: UnicodeString);
    // The joined string. The buffer is empty afterwards and can join again;
    // when the poll or the gate raises, Release empties it.
    function Join: UnicodeString;
    // Releases every reference the buffer holds and its storage. The buffer
    // is used again only after another Init.
    procedure Release;
    property ElementCount: SizeInt read FElementCount;
  end;

implementation

const
  // Below this many characters a plain loop copies faster than Move, whose
  // set-up costs more than the copy.
  SHORT_COPY_LENGTH = 16;

procedure HoldText(var ASlot: Pointer; const AText: UnicodeString); {$IFDEF FPC}inline;{$ENDIF}
begin
  ASlot := nil;
  UnicodeString(ASlot) := AText;
end;

procedure ReleaseText(var ASlot: Pointer); {$IFDEF FPC}inline;{$ENDIF}
begin
  UnicodeString(ASlot) := '';
end;

function HeldLength(const ASlot: Pointer): SizeInt; {$IFDEF FPC}inline;{$ENDIF}
begin
  Result := System.Length(UnicodeString(ASlot));
end;

procedure CopyCharacters(const ASource: PWideChar; const ATarget: PWideChar;
  const ALength: SizeInt); {$IFDEF FPC}inline;{$ENDIF}
var
  I: SizeInt;
begin
  if ALength < SHORT_COPY_LENGTH then
    for I := 0 to ALength - 1 do
      ATarget[I] := ASource[I]
  else
    Move(ASource^, ATarget^, ALength * SizeOf(WideChar));
end;

procedure TStringJoinBuffer.Init(const ASeparator: UnicodeString;
  const APoll: TStringJoinPoll; const AGate: TStringJoinGate);
begin
  FSelf := @Self;
  FSeparator := PWideChar(ASeparator);
  FSeparatorLength := System.Length(ASeparator);
  FPoll := APoll;
  FGate := AGate;
  FParts := nil;
  FPartCapacity := 0;
  FPartCount := 0;
  FGaps := nil;
  FGapCapacity := 0;
  FGapCount := 0;
  FPendingEmptyCount := 0;
  FElementCount := 0;
  FPartLength := 0;
end;

procedure TStringJoinBuffer.CheckInPlace;
begin
  if FSelf <> @Self then
    raise EStringJoinCopied.Create(
      'A TStringJoinBuffer is used only where Init set it up');
end;

function TStringJoinBuffer.PartSlot(const AIndex: SizeInt): PPointer;
begin
  if AIndex < INLINE_PART_COUNT then
    Result := @FInlineParts[AIndex]
  else
    Result := FParts + (AIndex - INLINE_PART_COUNT);
end;

procedure TStringJoinBuffer.AddGap;
begin
  if FGapCount = FGapCapacity then
  begin
    if FGapCapacity = 0 then
      FGapCapacity := INLINE_PART_COUNT
    else
      FGapCapacity := FGapCapacity * 2;
    ReAllocMem(FGaps, FGapCapacity * SizeOf(TStringJoinGap));
  end;
  FGaps[FGapCount].PartIndex := FPartCount;
  FGaps[FGapCount].EmptyCount := FPendingEmptyCount;
  Inc(FGapCount);
  FPendingEmptyCount := 0;
end;

procedure TStringJoinBuffer.GrowParts;
begin
  if FPartCapacity = 0 then
    FPartCapacity := INLINE_PART_COUNT * 2
  else
    FPartCapacity := FPartCapacity * 2;
  ReAllocMem(FParts, FPartCapacity * SizeOf(Pointer));
end;

procedure TStringJoinBuffer.RefuseLength(const ALength: Int64);
begin
  if Assigned(FGate) then
    FGate(ALength * SizeOf(WideChar));
  System.Error(reOutOfMemory);
end;

procedure TStringJoinBuffer.Add(const AText: UnicodeString);
var
  TextLength: SizeInt;
begin
  CheckInPlace;
  TextLength := System.Length(AText);
  if TextLength = 0 then
  begin
    Inc(FPendingEmptyCount);
    Inc(FElementCount);
    Exit;
  end;
  if TextLength > MAXIMUM_JOIN_LENGTH - FPartLength then
    RefuseLength(Int64(FPartLength) + TextLength);
  if FPendingEmptyCount > 0 then
    AddGap;
  if FPartCount >= INLINE_PART_COUNT + FPartCapacity then
    GrowParts;
  HoldText(PartSlot(FPartCount)^, AText);
  Inc(FPartCount);
  Inc(FPartLength, TextLength);
  Inc(FElementCount);
end;

procedure TStringJoinBuffer.ReleaseParts;
var
  I: SizeInt;
begin
  for I := 0 to FPartCount - 1 do
    ReleaseText(PartSlot(I)^);
  FPartCount := 0;
  FGapCount := 0;
  FPendingEmptyCount := 0;
  FElementCount := 0;
  FPartLength := 0;
end;

procedure TStringJoinBuffer.Release;
begin
  if FSelf <> @Self then
    Exit;
  ReleaseParts;
  if FParts <> nil then
  begin
    FreeMem(FParts);
    FParts := nil;
    FPartCapacity := 0;
  end;
  if FGaps <> nil then
  begin
    FreeMem(FGaps);
    FGaps := nil;
    FGapCapacity := 0;
  end;
  FSelf := nil;
end;

function TStringJoinBuffer.Join: UnicodeString;
var
  SeparatorLength, SeparatorCount, SeparatorsWritten: SizeInt;
  ElementIndex, GapIndex, PartIndex, Remaining, BlockLength: SizeInt;
  SinceLastPoll: SizeInt;
  TotalLength: Int64;
  Part: PPointer;
  Separator, Source, Target: PWideChar;
begin
  CheckInPlace;
  SeparatorLength := FSeparatorLength;
  if (FElementCount > 1) and (SeparatorLength > 0) then
    SeparatorCount := FElementCount - 1
  else
    SeparatorCount := 0;
  if SeparatorCount = 0 then
  begin
    if FPartCount = 0 then
    begin
      ReleaseParts;
      Exit('');
    end;
    if FPartCount = 1 then
    begin
      Result := UnicodeString(FInlineParts[0]);
      ReleaseParts;
      Exit;
    end;
  end;

  TotalLength := Int64(SeparatorCount) * SeparatorLength + FPartLength;
  if TotalLength > MAXIMUM_JOIN_LENGTH then
    RefuseLength(TotalLength);
  if Assigned(FGate) then
    FGate(TotalLength * SizeOf(WideChar));
  Result := '';
  SetLength(Result, SizeInt(TotalLength));
  Target := PWideChar(Result);
  Separator := FSeparator;
  SinceLastPoll := 0;
  SeparatorsWritten := 0;
  ElementIndex := 0;
  GapIndex := 0;
  for PartIndex := 0 to FPartCount - 1 do
  begin
    while (GapIndex < FGapCount) and
          (FGaps[GapIndex].PartIndex = PartIndex) do
    begin
      Inc(ElementIndex, FGaps[GapIndex].EmptyCount);
      Inc(GapIndex);
    end;
    if SeparatorCount > 0 then
      while SeparatorsWritten < ElementIndex do
      begin
        CopyCharacters(Separator, Target, SeparatorLength);
        Inc(Target, SeparatorLength);
        Inc(SinceLastPoll, SeparatorLength);
        Inc(SeparatorsWritten);
        if (SinceLastPoll >= POLL_INTERVAL_LENGTH) and Assigned(FPoll) then
        begin
          SinceLastPoll := 0;
          FPoll;
        end;
      end;
    Part := PartSlot(PartIndex);
    Source := PWideChar(Part^);
    Remaining := HeldLength(Part^);
    while Remaining > 0 do
    begin
      BlockLength := Remaining;
      if BlockLength > POLL_INTERVAL_LENGTH then
        BlockLength := POLL_INTERVAL_LENGTH;
      CopyCharacters(Source, Target, BlockLength);
      Inc(Source, BlockLength);
      Inc(Target, BlockLength);
      Inc(SinceLastPoll, BlockLength);
      Dec(Remaining, BlockLength);
      if (SinceLastPoll >= POLL_INTERVAL_LENGTH) and Assigned(FPoll) then
      begin
        SinceLastPoll := 0;
        FPoll;
      end;
    end;
    // Released while it is still in the cache; the result holds a copy.
    ReleaseText(Part^);
    Inc(ElementIndex);
  end;
  while SeparatorsWritten < SeparatorCount do
  begin
    CopyCharacters(Separator, Target, SeparatorLength);
    Inc(Target, SeparatorLength);
    Inc(SinceLastPoll, SeparatorLength);
    Inc(SeparatorsWritten);
    if (SinceLastPoll >= POLL_INTERVAL_LENGTH) and Assigned(FPoll) then
    begin
      SinceLastPoll := 0;
      FPoll;
    end;
  end;
  ReleaseParts;
end;

end.
