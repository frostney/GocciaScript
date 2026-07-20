unit Goccia.RegExp.VM;

{$I Goccia.inc}

interface

uses
  SysUtils,

  Goccia.RegExp.&Program,
  Goccia.RegExp.Compiler;

type
  ERegExpRuntimeError = class(Exception);

  TRegExpVMResult = record
    Matched: Boolean;
    CaptureSlots: array of Integer;
  end;

function ExecuteRegExpVM(const AProgram: TRegExpProgram;
  const AInput: string; const AStartIndex: Integer;
  const ARequireStart: Boolean; out AResult: TRegExpVMResult): Boolean;
function RegExpInputCodeUnitLength(const AInput: string): Integer;

{ Release the per-thread input-decode memo. Registered with
  Goccia.ThreadCleanupRegistry from this unit's initialization, so the drain
  releases it on worker exit (ShutdownThreadRuntime) and on the main thread (the
  registry's finalization), because FPC does not auto-finalize managed
  threadvars at thread exit. }
procedure ClearRegExpInputMemo;

implementation

uses
  TextSemantics,

  Goccia.RegExp.UnicodeData,
  Goccia.ThreadCleanupRegistry,
  Goccia.Timeout;

const
  MIN_STEP_LIMIT = 10000000;
  STEPS_PER_INPUT_BYTE = 100;
  DEFAULT_BACKTRACK_CAP = 10000000;
  MEMO_INITIAL_CAPACITY = 64;
  MEMO_MAX_CAPACITY = 65536;
  MEMO_MAX_PROBES = 16;
  SIMPLE_GREEDY_LOOP_MIN_REMAINING = 32;
  HIGH_SURROGATE_START = $D800;
  HIGH_SURROGATE_END = $DBFF;
  LOW_SURROGATE_START = $DC00;
  LOW_SURROGATE_END = $DFFF;

type
  TRegExpInputUnits = array of Cardinal;

  TRegExpInput = record
    Units: TRegExpInputUnits;
    Length: Integer;
  end;

  TBacktrackEntry = record
    PC: Integer;
    InputPos: Integer;
    RepeatDepth: Integer;
    RepeatStack: array of Integer;
    Slots: array of Integer;
  end;

  TBacktrackStack = array of TBacktrackEntry;

  TMemoEntry = record
    Occupied: Boolean;
    PC: Integer;
    InputPos: Integer;
  end;

  TMemoEntries = array of TMemoEntry;

  TMemoTable = record
    Entries: TMemoEntries;
    Count: Integer;
  end;

procedure MemoInit(var AMemo: TMemoTable);
begin
  AMemo.Count := 0;
end;

procedure MemoEnsureAllocated(var AMemo: TMemoTable); {$IFDEF FPC}inline;{$ENDIF}
begin
  if Length(AMemo.Entries) = 0 then
    SetLength(AMemo.Entries, MEMO_INITIAL_CAPACITY);
end;

function MemoHash(APC, APos, ACapacity: Integer): Integer; {$IFDEF FPC}inline;{$ENDIF}
var
  H: Cardinal;
begin
  H := Cardinal(APC);
  H := (H shl 5) xor (H shr 3) xor Cardinal(APos);
  H := H xor (H shr 7) xor (H shr 15);
  Result := Integer(H and Cardinal(ACapacity - 1));
end;

function MemoContains(var AMemo: TMemoTable; APC, APos: Integer): Boolean;
var
  Capacity, Idx, I: Integer;
begin
  Capacity := Length(AMemo.Entries);
  if Capacity = 0 then
    Exit(False);
  Idx := MemoHash(APC, APos, Capacity);
  for I := 0 to MEMO_MAX_PROBES - 1 do
  begin
    if not AMemo.Entries[Idx].Occupied then
      Exit(False);
    if (AMemo.Entries[Idx].PC = APC) and (AMemo.Entries[Idx].InputPos = APos) then
      Exit(True);
    Idx := (Idx + 1) and (Capacity - 1);
  end;
  Result := False;
end;

procedure MemoGrow(var AMemo: TMemoTable);
var
  Capacity, Idx, I, NewCapacity, Probe: Integer;
  OldEntries: TMemoEntries;
begin
  Capacity := Length(AMemo.Entries);
  if Capacity >= MEMO_MAX_CAPACITY then
    Exit;

  NewCapacity := Capacity * 2;
  if NewCapacity > MEMO_MAX_CAPACITY then
    NewCapacity := MEMO_MAX_CAPACITY;
  OldEntries := AMemo.Entries;
  SetLength(AMemo.Entries, NewCapacity);
  FillChar(AMemo.Entries[0], NewCapacity * SizeOf(TMemoEntry), 0);
  AMemo.Count := 0;

  for I := 0 to High(OldEntries) do
    if OldEntries[I].Occupied then
    begin
      Idx := MemoHash(OldEntries[I].PC, OldEntries[I].InputPos, NewCapacity);
      for Probe := 0 to MEMO_MAX_PROBES - 1 do
      begin
        if not AMemo.Entries[Idx].Occupied then
        begin
          AMemo.Entries[Idx] := OldEntries[I];
          Inc(AMemo.Count);
          Break;
        end;
        Idx := (Idx + 1) and (NewCapacity - 1);
      end;
    end;
end;

function MemoTryAdd(var AMemo: TMemoTable; APC, APos: Integer): Boolean;
var
  Capacity, Idx, I: Integer;
begin
  Capacity := Length(AMemo.Entries);
  Idx := MemoHash(APC, APos, Capacity);
  for I := 0 to MEMO_MAX_PROBES - 1 do
  begin
    if not AMemo.Entries[Idx].Occupied then
    begin
      AMemo.Entries[Idx].Occupied := True;
      AMemo.Entries[Idx].PC := APC;
      AMemo.Entries[Idx].InputPos := APos;
      Inc(AMemo.Count);
      Exit(True);
    end;
    if (AMemo.Entries[Idx].PC = APC) and (AMemo.Entries[Idx].InputPos = APos) then
      Exit(True);
    Idx := (Idx + 1) and (Capacity - 1);
  end;
  Result := False;
end;

procedure MemoAdd(var AMemo: TMemoTable; APC, APos: Integer);
var
  Capacity: Integer;
begin
  MemoEnsureAllocated(AMemo);
  while True do
  begin
    Capacity := Length(AMemo.Entries);
    if (Capacity = MEMO_MAX_CAPACITY) and
       (AMemo.Count * 4 >= Capacity * 3) then
      Exit;
    if (AMemo.Count * 4 >= Capacity * 3) and
       (Capacity < MEMO_MAX_CAPACITY) then
      MemoGrow(AMemo);
    if MemoTryAdd(AMemo, APC, APos) then
      Exit;
    if Capacity >= MEMO_MAX_CAPACITY then
      Exit;
    MemoGrow(AMemo);
  end;
end;

function CharClassContains(const AClass: TRegExpCharClass;
  ACodePoint: Cardinal): Boolean; {$IFDEF FPC}inline;{$ENDIF}
var
  Hi: Integer;
  Index: Integer;
  RangeCount: Integer;
  Lo: Integer;
  Mid: Integer;
begin
  RangeCount := Length(AClass.Ranges);
  if RangeCount <= 8 then
  begin
    for Index := 0 to RangeCount - 1 do
      if (ACodePoint >= AClass.Ranges[Index].Lo) and
         (ACodePoint <= AClass.Ranges[Index].Hi) then
        Exit(True);
    Exit(False);
  end;

  if Length(AClass.PageFirstRange) = REGEXP_CHAR_CLASS_PAGE_COUNT then
  begin
    Index := AClass.PageFirstRange[ACodePoint shr REGEXP_CHAR_CLASS_PAGE_BITS];
    while (Index <= High(AClass.Ranges)) and
          (AClass.Ranges[Index].Lo <= ACodePoint) do
    begin
      if ACodePoint <= AClass.Ranges[Index].Hi then
        Exit(True);
      Inc(Index);
    end;
    Exit(False);
  end;

  Lo := 0;
  Hi := High(AClass.Ranges);
  while Lo <= Hi do
  begin
    Mid := Lo + ((Hi - Lo) div 2);
    if ACodePoint < AClass.Ranges[Mid].Lo then
      Hi := Mid - 1
    else if ACodePoint > AClass.Ranges[Mid].Hi then
      Lo := Mid + 1
    else
      Exit(True);
  end;
  Result := False;
end;

function IsBasicWordChar(ACodePoint: Cardinal): Boolean; {$IFDEF FPC}inline;{$ENDIF}
begin
  Result := ((ACodePoint >= Ord('a')) and (ACodePoint <= Ord('z'))) or
            ((ACodePoint >= Ord('A')) and (ACodePoint <= Ord('Z'))) or
            ((ACodePoint >= Ord('0')) and (ACodePoint <= Ord('9'))) or
            (ACodePoint = Ord('_'));
end;

function IsWordChar(ACodePoint: Cardinal; AUnicodeAware,
  AIgnoreCase: Boolean): Boolean; {$IFDEF FPC}inline;{$ENDIF}
begin
  if IsBasicWordChar(ACodePoint) then
    Exit(True);
  if not AUnicodeAware or not AIgnoreCase then
    Exit(False);
  Result := IsBasicWordChar(RegExpCanonicalizeCodePoint(ACodePoint,
    AUnicodeAware, AIgnoreCase));
end;

function IsLineTerminator(ACodePoint: Cardinal): Boolean; {$IFDEF FPC}inline;{$ENDIF}
begin
  Result := (ACodePoint = $0A) or (ACodePoint = $0D) or
            (ACodePoint = $2028) or (ACodePoint = $2029);
end;

function IsHighSurrogate(ACodeUnit: Cardinal): Boolean; {$IFDEF FPC}inline;{$ENDIF}
begin
  Result := (ACodeUnit >= HIGH_SURROGATE_START) and
    (ACodeUnit <= HIGH_SURROGATE_END);
end;

function IsLowSurrogate(ACodeUnit: Cardinal): Boolean; {$IFDEF FPC}inline;{$ENDIF}
begin
  Result := (ACodeUnit >= LOW_SURROGATE_START) and
    (ACodeUnit <= LOW_SURROGATE_END);
end;

function SurrogatePairToCodePoint(const AHighSurrogate,
  ALowSurrogate: Cardinal): Cardinal; {$IFDEF FPC}inline;{$ENDIF}
begin
  Result := $10000 + ((AHighSurrogate - HIGH_SURROGATE_START) shl 10) +
    (ALowSurrogate - LOW_SURROGATE_START);
end;

procedure BuildRegExpInput(const AText: string; out AInput: TRegExpInput);
var
  ByteLength: Integer;
  CodePoint: Cardinal;
  Count: Integer;
  Index: Integer;
  Supplementary: Cardinal;
begin
  SetLength(AInput.Units, Length(AText));
  Count := 0;
  Index := 1;
  while Index <= Length(AText) do
  begin
    if Ord(AText[Index]) < $80 then
    begin
      AInput.Units[Count] := Ord(AText[Index]);
      Inc(Count);
      Inc(Index);
    end
    else if TryReadCodePointAtAllowSurrogates(AText, Index, CodePoint,
      ByteLength) then
    begin
      if CodePoint <= $FFFF then
      begin
        AInput.Units[Count] := CodePoint;
        Inc(Count);
      end
      else
      begin
        Supplementary := CodePoint - $10000;
        AInput.Units[Count] := HIGH_SURROGATE_START + (Supplementary shr 10);
        Inc(Count);
        if Count >= Length(AInput.Units) then
          SetLength(AInput.Units, Count + 1);
        AInput.Units[Count] := LOW_SURROGATE_START + (Supplementary and $3FF);
        Inc(Count);
      end;
      Inc(Index, ByteLength);
    end
    else
    begin
      AInput.Units[Count] := Ord(AText[Index]);
      Inc(Count);
      Inc(Index);
    end;

    if Count >= Length(AInput.Units) then
      SetLength(AInput.Units, Count + 16);
  end;
  SetLength(AInput.Units, Count);
  AInput.Length := Count;
end;

function ReadInputCodePoint(const AInput: TRegExpInput; APos: Integer;
  const AUnicode: Boolean; out ACodePoint: Cardinal;
  out AWidth: Integer): Boolean; {$IFDEF FPC}inline;{$ENDIF}
var
  CodeUnit: Cardinal;
begin
  if (APos < 0) or (APos >= AInput.Length) then
  begin
    ACodePoint := 0;
    AWidth := 0;
    Exit(False);
  end;

  CodeUnit := AInput.Units[APos];
  if AUnicode and IsHighSurrogate(CodeUnit) and
     (APos + 1 < AInput.Length) and IsLowSurrogate(AInput.Units[APos + 1]) then
  begin
    ACodePoint := SurrogatePairToCodePoint(CodeUnit, AInput.Units[APos + 1]);
    AWidth := 2;
  end
  else
  begin
    ACodePoint := CodeUnit;
    AWidth := 1;
  end;
  Result := True;
end;

function GetCodePointBefore(const AInput: TRegExpInput; APos: Integer;
  const AUnicode: Boolean; out ACodePoint: Cardinal): Boolean;
var
  CodeUnit: Cardinal;
begin
  Result := False;
  ACodePoint := 0;
  if APos <= 0 then
    Exit;

  CodeUnit := AInput.Units[APos - 1];
  if AUnicode and IsLowSurrogate(CodeUnit) and (APos >= 2) and
     IsHighSurrogate(AInput.Units[APos - 2]) then
    ACodePoint := SurrogatePairToCodePoint(AInput.Units[APos - 2], CodeUnit)
  else
    ACodePoint := CodeUnit;
  Result := True;
end;

function ReadInputCodePointBefore(const AInput: TRegExpInput; APos: Integer;
  const AUnicode: Boolean; out ACodePoint: Cardinal;
  out AWidth: Integer): Boolean;
var
  CodeUnit: Cardinal;
begin
  Result := False;
  ACodePoint := 0;
  AWidth := 0;
  if APos <= 0 then
    Exit;

  CodeUnit := AInput.Units[APos - 1];
  if AUnicode and IsLowSurrogate(CodeUnit) and (APos >= 2) and
     IsHighSurrogate(AInput.Units[APos - 2]) then
  begin
    ACodePoint := SurrogatePairToCodePoint(AInput.Units[APos - 2], CodeUnit);
    AWidth := 2;
  end
  else
  begin
    ACodePoint := CodeUnit;
    AWidth := 1;
  end;
  Result := True;
end;

function AdvanceInputIndex(const AInput: TRegExpInput; const AIndex: Integer;
  const AUnicode: Boolean): Integer;
var
  CodePoint: Cardinal;
  Width: Integer;
begin
  if AIndex >= AInput.Length then
    Exit(AIndex + 1);
  if not AUnicode then
    Exit(AIndex + 1);
  if ReadInputCodePoint(AInput, AIndex, True, CodePoint, Width) then
    Result := AIndex + Width
  else
    Result := AIndex + 1;
end;

function NormalizeInputIndex(const AInput: TRegExpInput; const AIndex: Integer;
  const AUnicode: Boolean): Integer;
begin
  Result := AIndex;
  if AUnicode and (AIndex > 0) and (AIndex < AInput.Length) and
     IsLowSurrogate(AInput.Units[AIndex]) and
     IsHighSurrogate(AInput.Units[AIndex - 1]) then
    Dec(Result);
end;

function StartCheckMatches(const ACheck: TRegExpStartCheck;
  const AInput: TRegExpInput; const AIndex: Integer; const AUnicode: Boolean): Boolean;
var
  CodePoint: Cardinal;
  Width: Integer;
begin
  if not ReadInputCodePoint(AInput, AIndex, AUnicode, CodePoint, Width) then
    Exit(False);
  if CodePoint > $FF then
    Exit(ACheck.HasNonLatin1);
  Result := (ACheck.Latin1Bits[CodePoint shr 5] and
    (UInt32(1) shl (CodePoint and 31))) <> 0;
end;

function StartCheckMatchesLatin1Unit(const ACheck: TRegExpStartCheck;
  ACodeUnit: Cardinal): Boolean; {$IFDEF FPC}inline;{$ENDIF}
begin
  Result := (ACodeUnit <= $FF) and
    ((ACheck.Latin1Bits[ACodeUnit shr 5] and
    (UInt32(1) shl (ACodeUnit and 31))) <> 0);
end;

function StartCheckIsASCIIOnly(const ACheck: TRegExpStartCheck): Boolean;
var
  I: Integer;
begin
  if not ACheck.Enabled or ACheck.HasNonLatin1 then
    Exit(False);
  for I := 4 to High(ACheck.Latin1Bits) do
    if ACheck.Latin1Bits[I] <> 0 then
      Exit(False);
  Result := True;
end;

function RawInputHasASCIIStartCandidate(const ACheck: TRegExpStartCheck;
  const AText: string): Boolean;
var
  CodeUnit: Cardinal;
  I: Integer;
begin
  for I := 1 to Length(AText) do
  begin
    CodeUnit := Ord(AText[I]);
    if (CodeUnit <= $7F) and StartCheckMatchesLatin1Unit(ACheck, CodeUnit) then
      Exit(True);
  end;
  Result := False;
end;

function FindNextStartCandidate(const ACheck: TRegExpStartCheck;
  const AInput: TRegExpInput; AStartPos: Integer; const AUnicode: Boolean): Integer;
var
  Skipped: Integer;
begin
  Result := AStartPos;
  Skipped := 0;
  if not ACheck.HasNonLatin1 then
  begin
    while Result < AInput.Length do
    begin
      if StartCheckMatchesLatin1Unit(ACheck, AInput.Units[Result]) then
        Exit;
      if AUnicode and IsHighSurrogate(AInput.Units[Result]) and
         (Result + 1 < AInput.Length) and IsLowSurrogate(AInput.Units[Result + 1]) then
        Inc(Result, 2)
      else
        Inc(Result);
      Inc(Skipped);
      if (Skipped and 4095) = 0 then
        CheckExecutionTimeout;
    end;
    Exit(AInput.Length + 1);
  end;

  while Result < AInput.Length do
  begin
    if StartCheckMatches(ACheck, AInput, Result, AUnicode) then
      Exit;
    Result := AdvanceInputIndex(AInput, Result, AUnicode);
    Inc(Skipped);
    if (Skipped and 4095) = 0 then
      CheckExecutionTimeout;
  end;
  Result := AInput.Length + 1;
end;

function RunVM(const AProgram: TRegExpProgram; const AInput: TRegExpInput;
  AStartPos: Integer; var ASlots: array of Integer;
  ASlotCount: Integer; var AStack: TBacktrackStack; AStartPC: Integer = 0;
  AEndPos: PInteger = nil; ABackward: Boolean = False): Boolean;
var
  PC, InputPos: Integer;
  Instr: UInt32;
  Op: TRegExpOpCode;
  Bx: Integer;
  CodePoint: Cardinal;
  ByteLen: Integer;
  StackTop: Integer;
  StepCount: Integer;
  StepLimit: Integer;
  Memo: TMemoTable;
  SlotCount: Integer;
  MatchCP: Cardinal;
  BeforeCP: Cardinal;
  BeforeIsWord, AfterIsWord: Boolean;
  WordIgnoreCase: Boolean;
  Negated: Boolean;
  BackrefGroup: Integer;
  BackrefICase: Boolean;
  BackrefUnicode: Boolean;
  LookEnd: Integer;
  LookStack: TBacktrackStack;
  LookSlots: array of Integer;
  LookMatched: Boolean;
  RefStart, RefEnd, RefPos: Integer;
  ComparePos: Integer;
  RefCP, InputCP: Cardinal;
  RefByteLen, InputByteLen: Integer;
  RepeatStack: array of Integer;
  RepeatDepth: Integer;

  procedure PushBacktrack(APC, AInputPos: Integer);
  begin
    if StackTop >= DEFAULT_BACKTRACK_CAP then
      raise ERegExpRuntimeError.Create('Maximum regular expression backtrack stack size exceeded');
    Inc(StackTop);
    if Length(AStack) = 0 then
      SetLength(AStack, 256)
    else if StackTop >= Length(AStack) then
      SetLength(AStack, StackTop * 2 + 16);
    AStack[StackTop].PC := APC;
    AStack[StackTop].InputPos := AInputPos;
    AStack[StackTop].RepeatDepth := RepeatDepth;
    if RepeatDepth = 0 then
      SetLength(AStack[StackTop].RepeatStack, 0)
    else
    begin
      if Length(AStack[StackTop].RepeatStack) <> RepeatDepth then
        SetLength(AStack[StackTop].RepeatStack, RepeatDepth);
      Move(RepeatStack[0], AStack[StackTop].RepeatStack[0],
        RepeatDepth * SizeOf(Integer));
    end;
    if Length(AStack[StackTop].Slots) <> SlotCount then
      SetLength(AStack[StackTop].Slots, SlotCount);
    if SlotCount > 0 then
      Move(ASlots[0], AStack[StackTop].Slots[0], SlotCount * SizeOf(Integer));
  end;

  function MatchStringSequence(const ASequence: TRegExpStringSequence;
    AStartPos: Integer; out AEndPos: Integer): Boolean;
  var
    CandidatePos: Integer;
    I: Integer;
  begin
    CandidatePos := AStartPos;
    for I := 0 to High(ASequence.CodePoints) do
    begin
      if not ReadInputCodePoint(AInput, CandidatePos, AProgram.FullUnicode,
         CodePoint, ByteLen) or (CodePoint <> ASequence.CodePoints[I]) then
        Exit(False);
      Inc(CandidatePos, ByteLen);
    end;
    AEndPos := CandidatePos;
    Result := True;
  end;

  function TryMatchStringSet(ASetIndex, ANextPC: Integer): Boolean;
  var
    FirstEndPos: Integer;
    FirstIndex: Integer;
    I: Integer;
    RangeEndPos: Integer;
    RangeMatches: Boolean;
    SequenceEndPos: Integer;
    StringSet: TRegExpStringSet;
  begin
    Result := False;
    if (ASetIndex < 0) or (ASetIndex > High(AProgram.StringSets)) then
      Exit;

    StringSet := AProgram.StringSets[ASetIndex];
    FirstIndex := -1;
    FirstEndPos := -1;
    for I := 0 to High(StringSet.Strings) do
    begin
      if MatchStringSequence(StringSet.Strings[I], InputPos, SequenceEndPos) then
      begin
        FirstIndex := I;
        FirstEndPos := SequenceEndPos;
        Break;
      end;
    end;

    RangeMatches := False;
    RangeEndPos := -1;
    if ReadInputCodePoint(AInput, InputPos, AProgram.FullUnicode,
       CodePoint, ByteLen) and (StringSet.CharClassIndex >= 0) and
       (StringSet.CharClassIndex <= High(AProgram.CharClasses)) and
       CharClassContains(AProgram.CharClasses[StringSet.CharClassIndex],
       CodePoint) then
    begin
      RangeMatches := True;
      RangeEndPos := InputPos + ByteLen;
    end;

    if FirstIndex < 0 then
    begin
      if RangeMatches then
      begin
        InputPos := RangeEndPos;
        Exit(True);
      end;
      Exit(False);
    end;

    if RangeMatches and not MemoContains(Memo, ANextPC, RangeEndPos) then
      PushBacktrack(ANextPC, RangeEndPos);

    for I := High(StringSet.Strings) downto FirstIndex + 1 do
    begin
      if MatchStringSequence(StringSet.Strings[I], InputPos, SequenceEndPos) and
         not MemoContains(Memo, ANextPC, SequenceEndPos) then
        PushBacktrack(ANextPC, SequenceEndPos);
    end;

    InputPos := FirstEndPos;
    Result := True;
  end;

  function PopBacktrack: Boolean;
  begin
    while StackTop >= 0 do
    begin
      PC := AStack[StackTop].PC;
      InputPos := AStack[StackTop].InputPos;
      RepeatDepth := AStack[StackTop].RepeatDepth;
      if RepeatDepth > 0 then
      begin
        if Length(RepeatStack) < RepeatDepth then
          SetLength(RepeatStack, RepeatDepth);
        Move(AStack[StackTop].RepeatStack[0], RepeatStack[0],
          RepeatDepth * SizeOf(Integer));
      end;
      if SlotCount > 0 then
        Move(AStack[StackTop].Slots[0], ASlots[0], SlotCount * SizeOf(Integer));
      Dec(StackTop);
      if not MemoContains(Memo, PC, InputPos) then
        Exit(True);
    end;
    Result := False;
  end;

  function BacktrackOrFail: Boolean;
  begin
    if StackTop >= 0 then
      MemoAdd(Memo, PC, InputPos);
    Result := PopBacktrack;
  end;

  function TailIsSimpleAccept(APC: Integer): Boolean;
  var
    TailBx: Integer;
    TailInstr: UInt32;
    TailOp: TRegExpOpCode;
  begin
    if (APC < 0) or (APC >= Length(AProgram.Code)) then
      Exit(False);

    TailInstr := AProgram.Code[APC];
    TailOp := TRegExpOpCode(TailInstr and $FF);
    TailBx := Integer(TailInstr shr 8);
    if TailOp = RX_ASSERT_END then
    begin
      if TailBx <> 0 then
        Exit(False);
      Inc(APC);
    end;

    while APC < Length(AProgram.Code) do
    begin
      TailOp := TRegExpOpCode(AProgram.Code[APC] and $FF);
      case TailOp of
        RX_SAVE:
          Inc(APC);
        RX_MATCH:
          Exit(True);
      else
        Exit(False);
      end;
    end;

    Result := False;
  end;

  function TrySimpleGreedyLoop(ASplitPC, AExitPC: Integer): Boolean;
  var
    BodyBx: Integer;
    BodyInstr: UInt32;
    BodyOp: TRegExpOpCode;
    JumpInstr: UInt32;
    Matched: Boolean;
    PollCount: Integer;
  begin
    Result := False;
    if ABackward or (ASplitPC + 2 >= Length(AProgram.Code)) then
      Exit;

    BodyInstr := AProgram.Code[ASplitPC + 1];
    BodyOp := TRegExpOpCode(BodyInstr and $FF);
    BodyBx := Integer(BodyInstr shr 8);
    case BodyOp of
      RX_CHAR, RX_CHAR_CLASS, RX_CHAR_CLASS_NEG:
        ;
    else
      Exit;
    end;

    JumpInstr := AProgram.Code[ASplitPC + 2];
    if (TRegExpOpCode(JumpInstr and $FF) <> RX_JUMP) or
       (Integer(JumpInstr shr 8) <> ASplitPC) then
      Exit;

    if not TailIsSimpleAccept(AExitPC) then
      Exit;

    PollCount := 0;
    while ReadInputCodePoint(AInput, InputPos, AProgram.FullUnicode,
      CodePoint, ByteLen) do
    begin
      case BodyOp of
        RX_CHAR:
          Matched := CodePoint = Cardinal(BodyBx);
        RX_CHAR_CLASS:
          Matched := CharClassContains(AProgram.CharClasses[BodyBx], CodePoint);
        RX_CHAR_CLASS_NEG:
          Matched := not CharClassContains(AProgram.CharClasses[BodyBx], CodePoint);
      else
        Matched := False;
      end;
      if not Matched then
        Break;

      Inc(InputPos, ByteLen);
      Inc(PollCount);
      if (PollCount and 4095) = 0 then
        CheckExecutionTimeout;
    end;

    PC := AExitPC;
    Result := True;
  end;

begin
  Result := False;
  SlotCount := ASlotCount;
  PC := AStartPC;
  InputPos := AStartPos;
  StepCount := 0;
  StepLimit := AInput.Length * STEPS_PER_INPUT_BYTE;
  if StepLimit < MIN_STEP_LIMIT then
    StepLimit := MIN_STEP_LIMIT;
  StackTop := -1;
  RepeatDepth := 0;
  MemoInit(Memo);

  while PC < Length(AProgram.Code) do
  begin
    Inc(StepCount);
    if StepCount > StepLimit then
      raise ERegExpRuntimeError.Create('Maximum regular expression backtrack stack size exceeded');
    // Catastrophic backtracking can spin here for seconds inside a single
    // exec/test call; poll the cooperative engine deadline periodically.
    // The 255 mask composes with CheckExecutionTimeout's internal 1/1024
    // throttle: the clock is read once per ~262K steps, bounding deadline
    // overshoot to low milliseconds at typical step costs.
    if (StepCount and 255) = 0 then
      CheckExecutionTimeout;

    Instr := AProgram.Code[PC];
    Op := TRegExpOpCode(Instr and $FF);
    Bx := Integer(Instr shr 8);

    case Op of
      RX_CHAR:
        begin
          if ABackward then
          begin
            if not ReadInputCodePointBefore(AInput, InputPos,
               AProgram.FullUnicode, CodePoint, ByteLen) then
            begin
              if not BacktrackOrFail then Exit;
              Continue;
            end;
          end
          else if not ReadInputCodePoint(AInput, InputPos,
             AProgram.FullUnicode, CodePoint, ByteLen) then
          begin
            if not BacktrackOrFail then Exit;
            Continue;
          end;
          MatchCP := Cardinal(Bx);
          if CodePoint <> MatchCP then
          begin
            if not BacktrackOrFail then Exit;
            Continue;
          end;
          if ABackward then
            Dec(InputPos, ByteLen)
          else
            Inc(InputPos, ByteLen);
          Inc(PC);
        end;

      RX_CHAR_CLASS:
        begin
          if ABackward then
          begin
            if not ReadInputCodePointBefore(AInput, InputPos,
               AProgram.FullUnicode, CodePoint, ByteLen) then
            begin
              if not BacktrackOrFail then Exit;
              Continue;
            end;
          end
          else if not ReadInputCodePoint(AInput, InputPos,
             AProgram.FullUnicode, CodePoint, ByteLen) then
          begin
            if not BacktrackOrFail then Exit;
            Continue;
          end;
          if not CharClassContains(AProgram.CharClasses[Bx], CodePoint) then
          begin
            if not BacktrackOrFail then Exit;
            Continue;
          end;
          if ABackward then
            Dec(InputPos, ByteLen)
          else
            Inc(InputPos, ByteLen);
          Inc(PC);
        end;

      RX_CHAR_CLASS_NEG:
        begin
          if ABackward then
          begin
            if not ReadInputCodePointBefore(AInput, InputPos,
               AProgram.FullUnicode, CodePoint, ByteLen) then
            begin
              if not BacktrackOrFail then Exit;
              Continue;
            end;
          end
          else if not ReadInputCodePoint(AInput, InputPos,
             AProgram.FullUnicode, CodePoint, ByteLen) then
          begin
            if not BacktrackOrFail then Exit;
            Continue;
          end;
          if CharClassContains(AProgram.CharClasses[Bx], CodePoint) then
          begin
            if not BacktrackOrFail then Exit;
            Continue;
          end;
          if ABackward then
            Dec(InputPos, ByteLen)
          else
            Inc(InputPos, ByteLen);
          Inc(PC);
        end;

      RX_ANY:
        begin
          if ABackward then
          begin
            if not ReadInputCodePointBefore(AInput, InputPos,
               AProgram.FullUnicode, CodePoint, ByteLen) then
            begin
              if not BacktrackOrFail then Exit;
              Continue;
            end;
          end
          else if not ReadInputCodePoint(AInput, InputPos,
             AProgram.FullUnicode, CodePoint, ByteLen) then
          begin
            if not BacktrackOrFail then Exit;
            Continue;
          end;
          if (Bx = 0) and IsLineTerminator(CodePoint) then
          begin
            if not BacktrackOrFail then Exit;
            Continue;
          end;
          if ABackward then
            Dec(InputPos, ByteLen)
          else
            Inc(InputPos, ByteLen);
          Inc(PC);
        end;

      RX_SPLIT:
        begin
          if ((AInput.Length - InputPos) >= SIMPLE_GREEDY_LOOP_MIN_REMAINING) and
             TrySimpleGreedyLoop(PC, Bx) then
            Continue;
          if not MemoContains(Memo, Bx, InputPos) then
            PushBacktrack(Bx, InputPos);
          Inc(PC);
        end;

      RX_SPLIT_LAZY:
        begin
          if not MemoContains(Memo, PC + 1, InputPos) then
            PushBacktrack(PC + 1, InputPos);
          PC := Bx;
        end;

      RX_JUMP:
        begin
          if (Bx >= 0) and (Bx < Length(AProgram.Code)) and
             (TRegExpOpCode(AProgram.Code[Bx] and $FF) = RX_SPLIT) then
          begin
          if (StackTop >= 0) and
             (AStack[StackTop].PC = Integer(AProgram.Code[Bx] shr 8)) and
             (AStack[StackTop].InputPos = InputPos) then
          begin
            PC := Integer(AProgram.Code[Bx] shr 8);
            if SlotCount > 0 then
              Move(AStack[StackTop].Slots[0], ASlots[0],
                SlotCount * SizeOf(Integer));
            RepeatDepth := AStack[StackTop].RepeatDepth;
            Dec(StackTop);
            Continue;
          end;
          end;
          PC := Bx;
        end;

      RX_SAVE:
        begin
          if Bx < SlotCount then
            ASlots[Bx] := InputPos;
          Inc(PC);
        end;

      RX_CLEAR_CAPTURES:
        begin
          RefStart := Bx shr CLEAR_CAPTURE_COUNT_BITS;
          RefEnd := RefStart + (Bx and CLEAR_CAPTURE_COUNT_MASK) - 1;
          for RefPos := RefStart to RefEnd do
          begin
            if (RefPos * 2) < SlotCount then
              ASlots[RefPos * 2] := -1;
            if (RefPos * 2 + 1) < SlotCount then
              ASlots[RefPos * 2 + 1] := -1;
          end;
          Inc(PC);
        end;

      RX_REPEAT_ENTER:
        begin
          if RepeatDepth >= Length(RepeatStack) then
            SetLength(RepeatStack, RepeatDepth * 2 + 8);
          RepeatStack[RepeatDepth] := InputPos;
          Inc(RepeatDepth);
          Inc(PC);
        end;

      RX_REPEAT_CHECK:
        begin
          if RepeatDepth <= 0 then
            raise ERegExpRuntimeError.Create('Invalid regular expression bytecode: repeat check without enter');
          Dec(RepeatDepth);
          if RepeatStack[RepeatDepth] = InputPos then
          begin
            if not BacktrackOrFail then Exit;
            Continue;
          end;
          Inc(PC);
        end;

      RX_CAPTURE_UNDEFINED_JUMP:
        begin
          if (PC + 1) >= Length(AProgram.Code) then
            raise ERegExpRuntimeError.Create('Invalid regular expression bytecode: capture guard without target');
          RefStart := -1;
          RefEnd := -1;
          if (Bx * 2) < SlotCount then
            RefStart := ASlots[Bx * 2];
          if (Bx * 2 + 1) < SlotCount then
            RefEnd := ASlots[Bx * 2 + 1];
          if (RefStart < 0) or (RefEnd < 0) or (RefStart > RefEnd) then
            PC := Integer(AProgram.Code[PC + 1] shr 8)
          else
            Inc(PC, 2);
        end;

      RX_STRING_SET:
        begin
          if ABackward or (not TryMatchStringSet(Bx, PC + 1)) then
          begin
            if not BacktrackOrFail then Exit;
            Continue;
          end;
          Inc(PC);
        end;

      RX_BACKREF:
        begin
          Negated := (Bx and BACKREF_STRICT_FLAG) <> 0;
          BackrefICase := (Bx and BACKREF_ICASE_FLAG) <> 0;
          BackrefUnicode := (Bx and BACKREF_UNICODE_FLAG) <> 0;
          BackrefGroup := Bx and BACKREF_INDEX_MASK;
          RefStart := -1;
          RefEnd := -1;
          if (BackrefGroup * 2) < SlotCount then
            RefStart := ASlots[BackrefGroup * 2];
          if (BackrefGroup * 2 + 1) < SlotCount then
            RefEnd := ASlots[BackrefGroup * 2 + 1];
          if (RefStart < 0) or (RefEnd < 0) or (RefStart > RefEnd) then
          begin
            if Negated then
            begin
              if not BacktrackOrFail then Exit;
              Continue;
            end;
            Inc(PC);
            Continue;
          end;
          RefEnd := ASlots[BackrefGroup * 2 + 1];
          ComparePos := InputPos;
          if ABackward then
          begin
            ComparePos := InputPos - (RefEnd - RefStart);
            if ComparePos < 0 then
            begin
              if not BacktrackOrFail then Exit;
              Continue;
            end;
          end;
          RefPos := RefStart;
          LookMatched := True;
          while RefPos < RefEnd do
          begin
            if not ReadInputCodePoint(AInput, RefPos, BackrefUnicode,
               RefCP, RefByteLen) then
            begin
              LookMatched := False;
              Break;
            end;
            if not ReadInputCodePoint(AInput, ComparePos, BackrefUnicode,
               InputCP, InputByteLen) then
            begin
              LookMatched := False;
              Break;
            end;
            if RefCP <> InputCP then
            begin
              if BackrefICase then
              begin
                RefCP := RegExpCanonicalizeCodePoint(RefCP, BackrefUnicode, True);
                InputCP := RegExpCanonicalizeCodePoint(InputCP, BackrefUnicode,
                  True);
              end;
              if RefCP <> InputCP then
              begin
                LookMatched := False;
                Break;
              end;
            end;
            Inc(RefPos, RefByteLen);
            Inc(ComparePos, InputByteLen);
          end;
          if not LookMatched then
          begin
            if not BacktrackOrFail then Exit;
            Continue;
          end;
          if ABackward then
            InputPos := InputPos - (RefEnd - RefStart)
          else
            InputPos := ComparePos;
          Inc(PC);
        end;

      RX_ASSERT_START:
        begin
          if Bx <> 0 then
          begin
            if InputPos > 0 then
            begin
              if not GetCodePointBefore(AInput, InputPos,
                 AProgram.FullUnicode, BeforeCP) or
                 not IsLineTerminator(BeforeCP) then
              begin
                if not BacktrackOrFail then Exit;
                Continue;
              end;
            end;
          end
          else
          begin
            if InputPos > 0 then
            begin
              if not BacktrackOrFail then Exit;
              Continue;
            end;
          end;
          Inc(PC);
        end;

      RX_ASSERT_END:
        begin
          if Bx <> 0 then
          begin
            if ReadInputCodePoint(AInput, InputPos, AProgram.FullUnicode,
               CodePoint, ByteLen) then
            begin
              if not IsLineTerminator(CodePoint) then
              begin
                if not BacktrackOrFail then Exit;
                Continue;
              end;
            end;
          end
          else
          begin
            if InputPos < AInput.Length then
            begin
              if not BacktrackOrFail then Exit;
              Continue;
            end;
          end;
          Inc(PC);
        end;

      RX_ASSERT_WORD:
        begin
          Negated := (Bx and WORD_ASSERT_NEGATED_FLAG) <> 0;
          WordIgnoreCase := (Bx and WORD_ASSERT_ICASE_FLAG) <> 0;
          BeforeIsWord := False;
          AfterIsWord := False;
          if GetCodePointBefore(AInput, InputPos, AProgram.FullUnicode,
             BeforeCP) then
            BeforeIsWord := IsWordChar(BeforeCP, AProgram.FullUnicode,
              WordIgnoreCase);
          if ReadInputCodePoint(AInput, InputPos, AProgram.FullUnicode,
             CodePoint, ByteLen) then
            AfterIsWord := IsWordChar(CodePoint, AProgram.FullUnicode,
              WordIgnoreCase);
          if Negated then
          begin
            if BeforeIsWord <> AfterIsWord then
            begin
              if not BacktrackOrFail then Exit;
              Continue;
            end;
          end
          else
          begin
            if BeforeIsWord = AfterIsWord then
            begin
              if not BacktrackOrFail then Exit;
              Continue;
            end;
          end;
          Inc(PC);
        end;

      RX_LOOKAHEAD:
        begin
          Negated := (Bx and LOOK_NEGATED_FLAG) <> 0;
          LookEnd := Bx and LOOK_TARGET_MASK;
          SetLength(LookSlots, SlotCount);
          Move(ASlots[0], LookSlots[0], SlotCount * SizeOf(Integer));
          LookMatched := RunVM(AProgram, AInput, InputPos, LookSlots,
            SlotCount, LookStack, PC + 1, nil, False);
          if Negated then
          begin
            if LookMatched then
            begin
              if not BacktrackOrFail then Exit;
              Continue;
            end;
          end
          else
          begin
            if not LookMatched then
            begin
              if not BacktrackOrFail then Exit;
              Continue;
            end;
            Move(LookSlots[0], ASlots[0], SlotCount * SizeOf(Integer));
          end;
          PC := LookEnd;
        end;

      RX_LOOKBEHIND:
        begin
          Negated := (Bx and LOOK_NEGATED_FLAG) <> 0;
          LookEnd := Bx and LOOK_TARGET_MASK;
          SetLength(LookSlots, SlotCount);
          Move(ASlots[0], LookSlots[0], SlotCount * SizeOf(Integer));
          LookMatched := RunVM(AProgram, AInput, InputPos, LookSlots,
            SlotCount, LookStack, PC + 1, nil, True);
          if Negated then
          begin
            if LookMatched then
            begin
              if not BacktrackOrFail then Exit;
              Continue;
            end;
          end
          else
          begin
            if not LookMatched then
            begin
              if not BacktrackOrFail then Exit;
              Continue;
            end;
            Move(LookSlots[0], ASlots[0], SlotCount * SizeOf(Integer));
          end;
          PC := LookEnd;
        end;

      RX_MATCH:
        begin
          if AEndPos <> nil then
            AEndPos^ := InputPos;
          Result := True;
          Exit;
        end;

      RX_FAIL:
        begin
          if not BacktrackOrFail then Exit;
          Continue;
        end;
    else
      raise ERegExpRuntimeError.CreateFmt(
        'Invalid regular expression bytecode: opcode %d at PC %d',
        [Ord(Op), PC]);
    end;
  end;
end;

// Per-thread memo of the most recently decoded subject. Global match/replace/
// split/matchAll re-enter ExecuteRegExpVM once per match against the same
// immutable subject string, so caching the decode avoids re-decoding and
// re-allocating the whole input on every match (O(matches * length) -> O(length)).
// Identity-keyed (the driver passes the same string instance each iteration),
// so the hit check is O(1). Pure optimization — clearing it is always safe.
// Single-entry: a different subject replaces the retained pair via managed
// assignment (the prior string/array is released, so the cache never grows).
// FPC does not auto-finalize managed threadvars at thread exit. ClearRegExpInputMemo
// is registered with Goccia.ThreadCleanupRegistry from this unit's initialization,
// so the registry drain releases each thread's pair on worker exit
// (ShutdownThreadRuntime) and the main thread's on process shutdown (the
// registry's finalization) — no thread retains a residual.
threadvar
  GRegExpInputMemoStr: string;
  GRegExpInputMemoUnits: TRegExpInputUnits;
  GRegExpInputMemoLength: Integer;
  GRegExpInputMemoValid: Boolean;

function TryGetRegExpInputMemo(const AInput: string;
  out ADecodedInput: TRegExpInput): Boolean; {$IFDEF FPC}inline;{$ENDIF}
begin
  Result := GRegExpInputMemoValid and
    (Pointer(AInput) = Pointer(GRegExpInputMemoStr));
  if Result then
  begin
    ADecodedInput.Units := GRegExpInputMemoUnits;
    ADecodedInput.Length := GRegExpInputMemoLength;
  end;
end;

procedure GetRegExpInput(const AInput: string;
  out ADecodedInput: TRegExpInput); {$IFDEF FPC}inline;{$ENDIF}
begin
  if TryGetRegExpInputMemo(AInput, ADecodedInput) then
    Exit;

  BuildRegExpInput(AInput, ADecodedInput);
  GRegExpInputMemoStr := AInput;
  GRegExpInputMemoUnits := ADecodedInput.Units;
  GRegExpInputMemoLength := ADecodedInput.Length;
  GRegExpInputMemoValid := True;
end;

function RegExpInputCodeUnitLength(const AInput: string): Integer;
var
  Input: TRegExpInput;
begin
  GetRegExpInput(AInput, Input);
  Result := Input.Length;
end;

function ExecuteRegExpVM(const AProgram: TRegExpProgram;
  const AInput: string; const AStartIndex: Integer;
  const ARequireStart: Boolean; out AResult: TRegExpVMResult): Boolean;
var
  Input: TRegExpInput;
  SlotCount, StartPos: Integer;
  Slots: array of Integer;
  Stack: TBacktrackStack;
begin
  Result := False;
  AResult.Matched := False;
  if not TryGetRegExpInputMemo(AInput, Input) then
  begin
    if (not ARequireStart) and StartCheckIsASCIIOnly(AProgram.StartCheck) and
       not RawInputHasASCIIStartCandidate(AProgram.StartCheck, AInput) then
      Exit;
    GetRegExpInput(AInput, Input);
  end;
  SlotCount := (AProgram.CaptureCount + 1) * 2;
  SetLength(Slots, SlotCount);
  StartPos := NormalizeInputIndex(Input, AStartIndex, AProgram.FullUnicode);
  if ARequireStart then
  begin
    FillChar(Slots[0], SlotCount * SizeOf(Integer), $FF);
    if RunVM(AProgram, Input, StartPos, Slots, SlotCount, Stack) then
    begin
      AResult.Matched := True;
      SetLength(AResult.CaptureSlots, SlotCount);
      Move(Slots[0], AResult.CaptureSlots[0], SlotCount * SizeOf(Integer));
      Result := True;
    end;
    Exit;
  end;
  if AProgram.StartCheck.Enabled then
    StartPos := FindNextStartCandidate(AProgram.StartCheck, Input, StartPos,
      AProgram.FullUnicode);
  while StartPos <= Input.Length do
  begin
    // Unanchored scans re-run the VM at every input position; on a long
    // non-matching subject this loop runs millions of times inside one
    // exec/test call, so poll the cooperative engine deadline here too.
    // Deliberately unmasked: CheckExecutionTimeout's internal 1/1024 counter
    // throttles deadlines > 16ms, and the always-check mode for <= 16ms
    // deadlines is self-limiting (the expensive window lasts at most 16ms).
    CheckExecutionTimeout;
    FillChar(Slots[0], SlotCount * SizeOf(Integer), $FF);
    if RunVM(AProgram, Input, StartPos, Slots, SlotCount, Stack) then
    begin
      AResult.Matched := True;
      SetLength(AResult.CaptureSlots, SlotCount);
      Move(Slots[0], AResult.CaptureSlots[0], SlotCount * SizeOf(Integer));
      Result := True;
      Exit;
    end;
    if StartPos >= Input.Length then
      Break;
    StartPos := AdvanceInputIndex(Input, StartPos, AProgram.FullUnicode);
    if AProgram.StartCheck.Enabled then
      StartPos := FindNextStartCandidate(AProgram.StartCheck, Input, StartPos,
        AProgram.FullUnicode);
  end;
end;

// FPC does not auto-finalize managed threadvars at thread exit; registered in
// this unit's initialization so the registry drain releases this thread's memo
// on worker exit and at main-thread shutdown, keeping its retained subject/units
// from leaking.
procedure ClearRegExpInputMemo;
begin
  GRegExpInputMemoStr := '';
  SetLength(GRegExpInputMemoUnits, 0);
  GRegExpInputMemoLength := 0;
  GRegExpInputMemoValid := False;
end;

initialization
  // FPC does not auto-finalize managed threadvars at thread exit. Register this
  // unit's regex-input memo, and also the is-ASCII memo owned by the shared
  // TextSemantics unit: TextSemantics is generic infrastructure that stays free
  // of engine dependencies, so its per-thread memo is registered here instead
  // (every engine binary links the regex VM). See ClearAsciiMemo in
  // source/shared/TextSemantics.pas. The registry drain releases both memos on
  // worker exit (ShutdownThreadRuntime) and on the main thread
  // (Goccia.ThreadCleanupRegistry's finalization).
  RegisterThreadvarCleanup(@ClearRegExpInputMemo);
  RegisterThreadvarCleanup(@ClearAsciiMemo);

end.
