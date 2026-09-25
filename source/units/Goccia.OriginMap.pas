unit Goccia.OriginMap;

{$I Goccia.inc}

{ Offset-level correspondence between a preprocessor's output and its input.

  A source map answers "which line did this come from", which is what a stack
  trace needs. A tool that edits the file needs a different answer: "which
  bytes of the file are these bytes". That is an offset question, and a v3
  source map cannot answer it — its segments are line/column pairs emitted
  wherever the generator felt like emitting one, with no claim that the text
  between two of them corresponds at all.

  This map claims exactly that, and only that. It is a list of *runs*, each
  asserting

    Generated[GeneratedStart .. GeneratedStart + Length) =
    Original [OriginalStart  .. OriginalStart  + Length)

  character for character. Everything the generator synthesized — a
  `createElement(`, a `", "`, an escaped string literal — lies in a gap
  between two runs and has no original text of its own.

  A run of zero length is an *anchor*: it asserts a position correspondence
  without asserting any text, and is how a construct that was rewritten whole
  still reports exact edges. See
  [ADR 0118](../../docs/adr/0118-original-file-source-ranges.md).

  Collecting runs and looking them up are separate types. A generator that
  buffers part of its output — the JSX transformer accumulates a whole
  attribute list before it knows whether to emit an object literal or an
  `Object.assign` — needs to collect correspondences in more than one place at
  once, and a value it can hold in a local is a great deal less bookkeeping
  than an object it has to free on every exit. Lookup needs the finished list
  and the original's length, so that is the object. }

interface

type
  TGocciaOriginRun = record
    GeneratedStart: Integer;
    OriginalStart: Integer;
    Length: Integer;
  end;

  { An ordered, still-growing list of correspondences. Runs are appended in
    generated order and are non-decreasing in both coordinate spaces. }
  TGocciaOriginRuns = record
  private
    const
      INITIAL_RUN_CAPACITY = 64;
  private
    FRuns: array of TGocciaOriginRun;
    FCount: Integer;

    function GetRun(const AIndex: Integer): TGocciaOriginRun;
    procedure Add(const AGeneratedStart, AOriginalStart, ALength: Integer);
  public
    { Records Generated[AGeneratedStart .. +ALength) as a verbatim copy of
      Original[AOriginalStart .. +ALength). Contiguous copies merge, so a
      character-at-a-time generator produces one run per copied region rather
      than one run per character. }
    procedure NoteCopy(const AGeneratedStart, AOriginalStart,
      ALength: Integer);

    { Records a position correspondence carrying no text. Used at the two
      edges of a construct that was rewritten whole. }
    procedure NoteAnchor(const AGeneratedOffset, AOriginalOffset: Integer);

    { Splices another list in, shifted into this one's coordinates. How a
      nested generator's result, or a buffered fragment, is folded into the
      output it ends up in. }
    procedure AppendShifted(const AOther: TGocciaOriginRuns;
      const AGeneratedShift, AOriginalShift: Integer);

    { Detaches the storage rather than emptying it. The array is reference
      counted and not copied on write, so a list that was assigned elsewhere
      and then cleared in place would rewrite what the other copy holds. }
    procedure Reset;

    property Count: Integer read FCount;
    property Items[const AIndex: Integer]: TGocciaOriginRun read GetRun;
  end;

  { Which way an offset that lands in a gap is rounded. A gap is text with no
    original of its own, so the honest answer is the whole construct that
    produced it: a range start rounds down to where the construct began, a
    range end rounds up to where it ended. A range is therefore never narrower
    than the text it covers, which is the direction an edit survives. }
  TGocciaOriginBias = (obStart, obEnd);

  TGocciaOriginMap = class
  private
    FRuns: TGocciaOriginRuns;
    FOriginalLength: Integer;

    function FindRun(const AGeneratedOffset: Integer): Integer;
  public
    constructor Create(const ARuns: TGocciaOriginRuns;
      const AOriginalLength: Integer);

    { The original offset for a generated one. Returns True when the answer is
      exact — the offset fell on a run — and False when it fell in a gap and
      was rounded outward per ABias. }
    function Map(const AGeneratedOffset: Integer;
      const ABias: TGocciaOriginBias; out AOriginalOffset: Integer): Boolean;

    property Runs: TGocciaOriginRuns read FRuns;
    property OriginalLength: Integer read FOriginalLength;
  end;

implementation

{ TGocciaOriginRuns }

function TGocciaOriginRuns.GetRun(const AIndex: Integer): TGocciaOriginRun;
begin
  Result := FRuns[AIndex];
end;

procedure TGocciaOriginRuns.Add(const AGeneratedStart, AOriginalStart,
  ALength: Integer);
begin
  if FCount = System.Length(FRuns) then
    if FCount = 0 then
      SetLength(FRuns, INITIAL_RUN_CAPACITY)
    else
      SetLength(FRuns, FCount * 2);
  FRuns[FCount].GeneratedStart := AGeneratedStart;
  FRuns[FCount].OriginalStart := AOriginalStart;
  FRuns[FCount].Length := ALength;
  Inc(FCount);
end;

procedure TGocciaOriginRuns.NoteCopy(const AGeneratedStart, AOriginalStart,
  ALength: Integer);
begin
  if ALength <= 0 then
    Exit;

  { Merge with the run before when both coordinates continue it. A generator
    that copies one character at a time would otherwise produce one run per
    character, and the binary search would have nothing left to search. }
  if (FCount > 0) and (FRuns[FCount - 1].Length > 0) and
     (FRuns[FCount - 1].GeneratedStart + FRuns[FCount - 1].Length =
      AGeneratedStart) and
     (FRuns[FCount - 1].OriginalStart + FRuns[FCount - 1].Length =
      AOriginalStart) then
  begin
    Inc(FRuns[FCount - 1].Length, ALength);
    Exit;
  end;

  Add(AGeneratedStart, AOriginalStart, ALength);
end;

procedure TGocciaOriginRuns.NoteAnchor(const AGeneratedOffset,
  AOriginalOffset: Integer);
begin
  { An anchor that repeats what the run before it already says is noise. One
    that contradicts it is a second, later answer for the same generated
    offset, and being later is what makes it the one a range start finds:
    anchors sit at construct edges, which is the answer an edit wants. }
  if (FCount > 0) and
     (FRuns[FCount - 1].GeneratedStart + FRuns[FCount - 1].Length =
      AGeneratedOffset) and
     (FRuns[FCount - 1].OriginalStart + FRuns[FCount - 1].Length =
      AOriginalOffset) then
    Exit;

  Add(AGeneratedOffset, AOriginalOffset, 0);
end;

procedure TGocciaOriginRuns.AppendShifted(const AOther: TGocciaOriginRuns;
  const AGeneratedShift, AOriginalShift: Integer);
var
  I: Integer;
begin
  for I := 0 to AOther.FCount - 1 do
    if AOther.FRuns[I].Length > 0 then
      NoteCopy(AOther.FRuns[I].GeneratedStart + AGeneratedShift,
        AOther.FRuns[I].OriginalStart + AOriginalShift,
        AOther.FRuns[I].Length)
    else
      NoteAnchor(AOther.FRuns[I].GeneratedStart + AGeneratedShift,
        AOther.FRuns[I].OriginalStart + AOriginalShift);
end;

procedure TGocciaOriginRuns.Reset;
begin
  FRuns := nil;
  FCount := 0;
end;

{ TGocciaOriginMap }

constructor TGocciaOriginMap.Create(const ARuns: TGocciaOriginRuns;
  const AOriginalLength: Integer);
begin
  inherited Create;
  FRuns := ARuns;
  FOriginalLength := AOriginalLength;
end;

// Index of the last run beginning at or before AGeneratedOffset, or -1.
function TGocciaOriginMap.FindRun(const AGeneratedOffset: Integer): Integer;
var
  Lo, Hi, Mid: Integer;
begin
  Result := -1;
  Lo := 0;
  Hi := FRuns.Count - 1;
  while Lo <= Hi do
  begin
    Mid := (Lo + Hi) div 2;
    if FRuns.Items[Mid].GeneratedStart <= AGeneratedOffset then
    begin
      Result := Mid;
      Lo := Mid + 1;
    end
    else
      Hi := Mid - 1;
  end;
end;

function TGocciaOriginMap.Map(const AGeneratedOffset: Integer;
  const ABias: TGocciaOriginBias; out AOriginalOffset: Integer): Boolean;
var
  Index: Integer;
begin
  AOriginalOffset := 0;
  Result := False;
  if FRuns.Count = 0 then
    Exit;

  Index := FindRun(AGeneratedOffset);

  { Before the first run nothing has been copied yet, so the only honest
    answer is the start of the file. }
  if Index < 0 then
    Exit;

  { A boundary two runs share — one ending exactly where the next begins — has
    two exact answers, and which is wanted depends on whether a range starts
    or ends here. A start belongs to what follows, an end to what precedes.
    FindRun already returns the last run beginning at or before the offset,
    which is what a start wants; an end walks back over the runs that merely
    touch this boundary. }
  if ABias = obEnd then
    while (Index > 0) and
          (FRuns.Items[Index].GeneratedStart = AGeneratedOffset) and
          (FRuns.Items[Index - 1].GeneratedStart +
           FRuns.Items[Index - 1].Length = AGeneratedOffset) do
      Dec(Index);

  if AGeneratedOffset <=
     FRuns.Items[Index].GeneratedStart + FRuns.Items[Index].Length then
  begin
    AOriginalOffset := FRuns.Items[Index].OriginalStart +
      (AGeneratedOffset - FRuns.Items[Index].GeneratedStart);
    Result := True;
    Exit;
  end;

  { In a gap, so round outward to the construct that produced it: a start to
    where the copying before it stopped, an end to where the copying after it
    resumes. }
  if ABias = obStart then
    AOriginalOffset := FRuns.Items[Index].OriginalStart +
      FRuns.Items[Index].Length
  else if Index + 1 < FRuns.Count then
    AOriginalOffset := FRuns.Items[Index + 1].OriginalStart
  else
    AOriginalOffset := FOriginalLength;
end;

end.
