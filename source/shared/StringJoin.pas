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
// A poll, when given, is called every POLL_INTERVAL_LENGTH characters of the
// final copy, strings and separators alike, so that a host's deadline or
// abort can end a join whose result is long. A charge, when given, reserves
// what the buffer holds that nothing else accounts for, its tables and the
// strings added as held only here, for as long as it holds them, so that
// every other allocation meanwhile sees them too. A gate, when given, is
// asked about the result before it is allocated, and about a result too long
// for any string before the join gives up.

interface

uses
  SysUtils;

const
  // The longest result, in characters, so that its byte size fits a SizeInt
  // with room for the string's header on every target.
  {$IFDEF FPC}
  MAXIMUM_JOIN_LENGTH = High(SizeInt) div (2 * SizeOf(WideChar));
  {$ELSE}
  // Delphi's UnicodeString length is an Integer on every target.
  MAXIMUM_JOIN_LENGTH = High(Integer) div 2;
  {$ENDIF}
  // What a gate is asked about for a result no string can hold.
  TOO_LONG_RESULT_BYTES = High(Int64);
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
  // Raises unless ABytes may be allocated. ABytes is TOO_LONG_RESULT_BYTES
  // for a result longer than MAXIMUM_JOIN_LENGTH characters, which no string
  // can hold: the gate raises its own error for it, or the join raises
  // EOutOfMemory.
  TStringJoinGate = procedure(const ABytes: Int64);
  // Reserves ABytes against a budget when positive, raising when they do not
  // fit, and releases -ABytes when negative, which never raises.
  TStringJoinCharge = procedure(const ABytes: Int64);

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
    FCharge: TStringJoinCharge;
    // The bytes charged for the tables, and for the strings held only here.
    FTableBytes: Int64;
    FHeldTextBytes: Int64;
    procedure Charge(const ABytes: Int64); {$IFDEF FPC}inline;{$ENDIF}
    procedure CheckInPlace; {$IFDEF FPC}inline;{$ENDIF}
    function PartSlot(const AIndex: SizeInt): PPointer; {$IFDEF FPC}inline;{$ENDIF}
    procedure AddGap;
    procedure GrowParts;
    procedure WritePolled(ASource: PWideChar; ALength: SizeInt;
      var ATarget: PWideChar; var ASinceLastPoll: SizeInt);
    procedure RefuseLength(const ALength: Int64);
    procedure ReleaseParts;
  public
    procedure Init(const ASeparator: UnicodeString;
      const APoll: TStringJoinPoll = nil; const AGate: TStringJoinGate = nil;
      const ACharge: TStringJoinCharge = nil);
    // Adds the next element's string; an empty one still takes its place
    // between two separators. AHeldOnlyHere says that the value that owns the
    // string, and accounts for its bytes, may be freed while the buffer holds
    // it, as a string converted from a number is: the buffer counts its bytes
    // itself.
    procedure Add(const AText: UnicodeString;
      const AHeldOnlyHere: Boolean = False);
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
  const APoll: TStringJoinPoll; const AGate: TStringJoinGate;
  const ACharge: TStringJoinCharge);
begin
  FSelf := @Self;
  FSeparator := PWideChar(ASeparator);
  FSeparatorLength := System.Length(ASeparator);
  FPoll := APoll;
  FGate := AGate;
  FCharge := ACharge;
  FParts := nil;
  FPartCapacity := 0;
  FPartCount := 0;
  FGaps := nil;
  FGapCapacity := 0;
  FGapCount := 0;
  FPendingEmptyCount := 0;
  FElementCount := 0;
  FPartLength := 0;
  FTableBytes := 0;
  FHeldTextBytes := 0;
end;

procedure TStringJoinBuffer.Charge(const ABytes: Int64);
begin
  if Assigned(FCharge) and (ABytes <> 0) then
    FCharge(ABytes);
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
var
  NewCapacity: SizeInt;
begin
  if FGapCount = FGapCapacity then
  begin
    if FGapCapacity = 0 then
      NewCapacity := INLINE_PART_COUNT
    else
      NewCapacity := FGapCapacity * 2;
    // The old block may still be held while the new one is filled: both are
    // charged until the move is done.
    Charge(Int64(NewCapacity) * SizeOf(TStringJoinGap));
    Inc(FTableBytes, Int64(NewCapacity) * SizeOf(TStringJoinGap));
    ReAllocMem(FGaps, NewCapacity * SizeOf(TStringJoinGap));
    Charge(-Int64(FGapCapacity) * SizeOf(TStringJoinGap));
    Dec(FTableBytes, Int64(FGapCapacity) * SizeOf(TStringJoinGap));
    FGapCapacity := NewCapacity;
  end;
  FGaps[FGapCount].PartIndex := FPartCount;
  FGaps[FGapCount].EmptyCount := FPendingEmptyCount;
  Inc(FGapCount);
  FPendingEmptyCount := 0;
end;

procedure TStringJoinBuffer.GrowParts;
var
  NewCapacity: SizeInt;
begin
  if FPartCapacity = 0 then
    NewCapacity := INLINE_PART_COUNT * 2
  else
    NewCapacity := FPartCapacity * 2;
  // The old block may still be held while the new one is filled: both are
  // charged until the move is done.
  Charge(Int64(NewCapacity) * SizeOf(Pointer));
  Inc(FTableBytes, Int64(NewCapacity) * SizeOf(Pointer));
  ReAllocMem(FParts, NewCapacity * SizeOf(Pointer));
  Charge(-Int64(FPartCapacity) * SizeOf(Pointer));
  Dec(FTableBytes, Int64(FPartCapacity) * SizeOf(Pointer));
  FPartCapacity := NewCapacity;
end;

// Copies ALength characters to ATarget in blocks that end where the poll is
// due, and polls there.
procedure TStringJoinBuffer.WritePolled(ASource: PWideChar; ALength: SizeInt;
  var ATarget: PWideChar; var ASinceLastPoll: SizeInt);
var
  BlockLength: SizeInt;
begin
  while ALength > 0 do
  begin
    BlockLength := POLL_INTERVAL_LENGTH - ASinceLastPoll;
    if BlockLength > ALength then
      BlockLength := ALength;
    CopyCharacters(ASource, ATarget, BlockLength);
    Inc(ASource, BlockLength);
    Inc(ATarget, BlockLength);
    Inc(ASinceLastPoll, BlockLength);
    Dec(ALength, BlockLength);
    if ASinceLastPoll >= POLL_INTERVAL_LENGTH then
    begin
      ASinceLastPoll := 0;
      if Assigned(FPoll) then
        FPoll;
    end;
  end;
end;

procedure TStringJoinBuffer.RefuseLength(const ALength: Int64);
begin
  if Assigned(FGate) then
    FGate(TOO_LONG_RESULT_BYTES);
  System.Error(reOutOfMemory);
end;

procedure TStringJoinBuffer.Add(const AText: UnicodeString;
  const AHeldOnlyHere: Boolean);
var
  TextLength: SizeInt;
begin
  CheckInPlace;
  TextLength := System.Length(AText);
  if TextLength = 0 then
  begin
    // Without a separator, an empty element leaves no trace in the result.
    if FSeparatorLength > 0 then
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
  // A string held only here is charged while the buffer holds it, unless
  // something else holds its text too: the caller's own reference and that of
  // the value it came from account for two, and a third, from a value still
  // reachable or this buffer's own earlier copy, means the text is counted
  // already, so a string joined many times is charged once at most.
  if AHeldOnlyHere and (StringRefCount(AText) <= 2) then
  begin
    Charge(Int64(TextLength) * SizeOf(WideChar));
    Inc(FHeldTextBytes, Int64(TextLength) * SizeOf(WideChar));
  end;
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
  Charge(-FHeldTextBytes);
  FHeldTextBytes := 0;
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
  Charge(-FTableBytes);
  FTableBytes := 0;
  FSelf := nil;
end;

function TStringJoinBuffer.Join: UnicodeString;
var
  SeparatorLength, SeparatorCount, SeparatorsWritten: SizeInt;
  ElementIndex, GapIndex, PartIndex, Remaining: SizeInt;
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

  // Compared before multiplying, so that the product cannot overflow.
  if (SeparatorCount > 0) and
     (SeparatorCount > (MAXIMUM_JOIN_LENGTH - FPartLength) div SeparatorLength)
  then
    RefuseLength(MAXIMUM_JOIN_LENGTH);
  TotalLength := Int64(SeparatorCount) * SeparatorLength + FPartLength;
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
        if (SeparatorLength < SHORT_COPY_LENGTH) and
           (SinceLastPoll + SeparatorLength < POLL_INTERVAL_LENGTH) then
        begin
          CopyCharacters(Separator, Target, SeparatorLength);
          Inc(Target, SeparatorLength);
          Inc(SinceLastPoll, SeparatorLength);
        end
        else
          WritePolled(Separator, SeparatorLength, Target, SinceLastPoll);
        Inc(SeparatorsWritten);
      end;
    Part := PartSlot(PartIndex);
    Source := PWideChar(Part^);
    Remaining := HeldLength(Part^);
    if (Remaining < SHORT_COPY_LENGTH) and
       (SinceLastPoll + Remaining < POLL_INTERVAL_LENGTH) then
    begin
      CopyCharacters(Source, Target, Remaining);
      Inc(Target, Remaining);
      Inc(SinceLastPoll, Remaining);
    end
    else
      WritePolled(Source, Remaining, Target, SinceLastPoll);
    // Released while it is still in the cache; the result holds a copy.
    ReleaseText(Part^);
    Inc(ElementIndex);
  end;
  while SeparatorsWritten < SeparatorCount do
  begin
    if (SeparatorLength < SHORT_COPY_LENGTH) and
       (SinceLastPoll + SeparatorLength < POLL_INTERVAL_LENGTH) then
    begin
      CopyCharacters(Separator, Target, SeparatorLength);
      Inc(Target, SeparatorLength);
      Inc(SinceLastPoll, SeparatorLength);
    end
    else
      WritePolled(Separator, SeparatorLength, Target, SinceLastPoll);
    Inc(SeparatorsWritten);
  end;
  ReleaseParts;
end;

end.
