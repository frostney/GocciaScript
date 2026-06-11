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

implementation

uses
  TextSemantics,

  Goccia.RegExp.UnicodeData,
  Goccia.Timeout;

const
  MIN_STEP_LIMIT = 10000000;
  STEPS_PER_INPUT_BYTE = 100;
  DEFAULT_BACKTRACK_CAP = 10000000;
  MEMO_CAPACITY = 65536;
  MEMO_LOAD_LIMIT = 49152;
  HIGH_SURROGATE_START = $D800;
  HIGH_SURROGATE_END = $DBFF;
  LOW_SURROGATE_START = $DC00;
  LOW_SURROGATE_END = $DFFF;

type
  TRegExpInput = record
    Units: array of Cardinal;
    Length: Integer;
  end;

  TBacktrackEntry = record
    PC: Integer;
    InputPos: Integer;
    Slots: array of Integer;
  end;

  TMemoEntry = record
    Occupied: Boolean;
    PC: Integer;
    InputPos: Integer;
  end;

  TMemoTable = record
    Entries: array of TMemoEntry;
    Count: Integer;
  end;

procedure MemoInit(var AMemo: TMemoTable);
begin
  AMemo.Count := 0;
end;

procedure MemoEnsureAllocated(var AMemo: TMemoTable); inline;
begin
  if Length(AMemo.Entries) = 0 then
    SetLength(AMemo.Entries, MEMO_CAPACITY);
end;

function MemoHash(APC, APos: Integer): Integer; inline;
var
  H: Cardinal;
begin
  H := Cardinal(APC);
  H := (H shl 5) xor (H shr 3) xor Cardinal(APos);
  H := H xor (H shr 7) xor (H shr 15);
  Result := Integer(H and (MEMO_CAPACITY - 1));
end;

function MemoContains(var AMemo: TMemoTable; APC, APos: Integer): Boolean;
var
  Idx, I: Integer;
begin
  if Length(AMemo.Entries) = 0 then
    Exit(False);
  Idx := MemoHash(APC, APos);
  for I := 0 to 15 do
  begin
    if not AMemo.Entries[Idx].Occupied then
      Exit(False);
    if (AMemo.Entries[Idx].PC = APC) and (AMemo.Entries[Idx].InputPos = APos) then
      Exit(True);
    Idx := (Idx + 1) and (MEMO_CAPACITY - 1);
  end;
  Result := False;
end;

procedure MemoAdd(var AMemo: TMemoTable; APC, APos: Integer);
var
  Idx, I: Integer;
begin
  MemoEnsureAllocated(AMemo);
  if AMemo.Count >= MEMO_LOAD_LIMIT then
    Exit;
  Idx := MemoHash(APC, APos);
  for I := 0 to 15 do
  begin
    if not AMemo.Entries[Idx].Occupied then
    begin
      AMemo.Entries[Idx].Occupied := True;
      AMemo.Entries[Idx].PC := APC;
      AMemo.Entries[Idx].InputPos := APos;
      Inc(AMemo.Count);
      Exit;
    end;
    if (AMemo.Entries[Idx].PC = APC) and (AMemo.Entries[Idx].InputPos = APos) then
      Exit;
    Idx := (Idx + 1) and (MEMO_CAPACITY - 1);
  end;
end;

function CharClassContainsLinear(const AClass: TRegExpCharClass;
  ACodePoint: Cardinal): Boolean; inline;
var
  I: Integer;
begin
  for I := 0 to High(AClass.Ranges) do
    if (ACodePoint >= AClass.Ranges[I].Lo) and
       (ACodePoint <= AClass.Ranges[I].Hi) then
      Exit(True);
  Result := False;
end;

function IsBasicWordChar(ACodePoint: Cardinal): Boolean; inline;
begin
  Result := ((ACodePoint >= Ord('a')) and (ACodePoint <= Ord('z'))) or
            ((ACodePoint >= Ord('A')) and (ACodePoint <= Ord('Z'))) or
            ((ACodePoint >= Ord('0')) and (ACodePoint <= Ord('9'))) or
            (ACodePoint = Ord('_'));
end;

function IsWordChar(ACodePoint: Cardinal; AUnicodeAware,
  AIgnoreCase: Boolean): Boolean; inline;
begin
  if IsBasicWordChar(ACodePoint) then
    Exit(True);
  if not AUnicodeAware or not AIgnoreCase then
    Exit(False);
  Result := IsBasicWordChar(RegExpCanonicalizeCodePoint(ACodePoint,
    AUnicodeAware, AIgnoreCase));
end;

function IsLineTerminator(ACodePoint: Cardinal): Boolean; inline;
begin
  Result := (ACodePoint = $0A) or (ACodePoint = $0D) or
            (ACodePoint = $2028) or (ACodePoint = $2029);
end;

function IsHighSurrogate(ACodeUnit: Cardinal): Boolean; inline;
begin
  Result := (ACodeUnit >= HIGH_SURROGATE_START) and
    (ACodeUnit <= HIGH_SURROGATE_END);
end;

function IsLowSurrogate(ACodeUnit: Cardinal): Boolean; inline;
begin
  Result := (ACodeUnit >= LOW_SURROGATE_START) and
    (ACodeUnit <= LOW_SURROGATE_END);
end;

function SurrogatePairToCodePoint(const AHighSurrogate,
  ALowSurrogate: Cardinal): Cardinal; inline;
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
    if TryReadUTF8CodePointAllowSurrogates(AText, Index, CodePoint,
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
  out AWidth: Integer): Boolean; inline;
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

function RunVM(const AProgram: TRegExpProgram; const AInput: TRegExpInput;
  AStartPos: Integer; var ASlots: array of Integer;
  ASlotCount: Integer; AStartPC: Integer = 0;
  AEndPos: PInteger = nil; ABackward: Boolean = False): Boolean;
var
  PC, InputPos: Integer;
  Instr: UInt32;
  Op: TRegExpOpCode;
  Bx: Integer;
  CodePoint: Cardinal;
  ByteLen: Integer;
  Stack: array of TBacktrackEntry;
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
  LookSlots: array of Integer;
  LookMatched: Boolean;
  RefStart, RefEnd, RefPos: Integer;
  ComparePos: Integer;
  RefCP, InputCP: Cardinal;
  RefByteLen, InputByteLen: Integer;

  procedure PushBacktrack(APC, AInputPos: Integer);
  begin
    if StackTop >= DEFAULT_BACKTRACK_CAP then
      raise ERegExpRuntimeError.Create('Maximum regular expression backtrack stack size exceeded');
    Inc(StackTop);
    if StackTop >= Length(Stack) then
      SetLength(Stack, StackTop * 2 + 16);
    Stack[StackTop].PC := APC;
    Stack[StackTop].InputPos := AInputPos;
    if Length(Stack[StackTop].Slots) <> SlotCount then
      SetLength(Stack[StackTop].Slots, SlotCount);
    if SlotCount > 0 then
      Move(ASlots[0], Stack[StackTop].Slots[0], SlotCount * SizeOf(Integer));
  end;

  function PopBacktrack: Boolean;
  begin
    while StackTop >= 0 do
    begin
      PC := Stack[StackTop].PC;
      InputPos := Stack[StackTop].InputPos;
      if SlotCount > 0 then
        Move(Stack[StackTop].Slots[0], ASlots[0], SlotCount * SizeOf(Integer));
      Dec(StackTop);
      if not MemoContains(Memo, PC, InputPos) then
        Exit(True);
    end;
    Result := False;
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
  SetLength(Stack, 256);
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
              MemoAdd(Memo, PC, InputPos);
              if not PopBacktrack then Exit;
              Continue;
            end;
          end
          else if not ReadInputCodePoint(AInput, InputPos,
             AProgram.FullUnicode, CodePoint, ByteLen) then
          begin
            MemoAdd(Memo, PC, InputPos);
            if not PopBacktrack then Exit;
            Continue;
          end;
          MatchCP := Cardinal(Bx);
          if CodePoint <> MatchCP then
          begin
            MemoAdd(Memo, PC, InputPos);
            if not PopBacktrack then Exit;
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
              MemoAdd(Memo, PC, InputPos);
              if not PopBacktrack then Exit;
              Continue;
            end;
          end
          else if not ReadInputCodePoint(AInput, InputPos,
             AProgram.FullUnicode, CodePoint, ByteLen) then
          begin
            MemoAdd(Memo, PC, InputPos);
            if not PopBacktrack then Exit;
            Continue;
          end;
          if not CharClassContainsLinear(AProgram.CharClasses[Bx], CodePoint) then
          begin
            MemoAdd(Memo, PC, InputPos);
            if not PopBacktrack then Exit;
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
              MemoAdd(Memo, PC, InputPos);
              if not PopBacktrack then Exit;
              Continue;
            end;
          end
          else if not ReadInputCodePoint(AInput, InputPos,
             AProgram.FullUnicode, CodePoint, ByteLen) then
          begin
            MemoAdd(Memo, PC, InputPos);
            if not PopBacktrack then Exit;
            Continue;
          end;
          if CharClassContainsLinear(AProgram.CharClasses[Bx], CodePoint) then
          begin
            MemoAdd(Memo, PC, InputPos);
            if not PopBacktrack then Exit;
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
              MemoAdd(Memo, PC, InputPos);
              if not PopBacktrack then Exit;
              Continue;
            end;
          end
          else if not ReadInputCodePoint(AInput, InputPos,
             AProgram.FullUnicode, CodePoint, ByteLen) then
          begin
            MemoAdd(Memo, PC, InputPos);
            if not PopBacktrack then Exit;
            Continue;
          end;
          if (Bx = 0) and IsLineTerminator(CodePoint) then
          begin
            MemoAdd(Memo, PC, InputPos);
            if not PopBacktrack then Exit;
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
            if (StackTop >= 0) and (Stack[StackTop].PC = Integer(AProgram.Code[Bx] shr 8)) and
               (Stack[StackTop].InputPos = InputPos) then
            begin
              PC := Integer(AProgram.Code[Bx] shr 8);
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
              MemoAdd(Memo, PC, InputPos);
              if not PopBacktrack then Exit;
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
              MemoAdd(Memo, PC, InputPos);
              if not PopBacktrack then Exit;
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
            MemoAdd(Memo, PC, InputPos);
            if not PopBacktrack then Exit;
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
                MemoAdd(Memo, PC, InputPos);
                if not PopBacktrack then Exit;
                Continue;
              end;
            end;
          end
          else
          begin
            if InputPos > 0 then
            begin
              MemoAdd(Memo, PC, InputPos);
              if not PopBacktrack then Exit;
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
                MemoAdd(Memo, PC, InputPos);
                if not PopBacktrack then Exit;
                Continue;
              end;
            end;
          end
          else
          begin
            if InputPos < AInput.Length then
            begin
              MemoAdd(Memo, PC, InputPos);
              if not PopBacktrack then Exit;
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
              MemoAdd(Memo, PC, InputPos);
              if not PopBacktrack then Exit;
              Continue;
            end;
          end
          else
          begin
            if BeforeIsWord = AfterIsWord then
            begin
              MemoAdd(Memo, PC, InputPos);
              if not PopBacktrack then Exit;
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
            SlotCount, PC + 1, nil, False);
          if Negated then
          begin
            if LookMatched then
            begin
              MemoAdd(Memo, PC, InputPos);
              if not PopBacktrack then Exit;
              Continue;
            end;
          end
          else
          begin
            if not LookMatched then
            begin
              MemoAdd(Memo, PC, InputPos);
              if not PopBacktrack then Exit;
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
            SlotCount, PC + 1, nil, True);
          if Negated then
          begin
            if LookMatched then
            begin
              MemoAdd(Memo, PC, InputPos);
              if not PopBacktrack then Exit;
              Continue;
            end;
          end
          else
          begin
            if not LookMatched then
            begin
              MemoAdd(Memo, PC, InputPos);
              if not PopBacktrack then Exit;
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
          MemoAdd(Memo, PC, InputPos);
          if not PopBacktrack then Exit;
          Continue;
        end;
    else
      raise ERegExpRuntimeError.CreateFmt(
        'Invalid regular expression bytecode: opcode %d at PC %d',
        [Ord(Op), PC]);
    end;
  end;
end;

function ExecuteRegExpVM(const AProgram: TRegExpProgram;
  const AInput: string; const AStartIndex: Integer;
  const ARequireStart: Boolean; out AResult: TRegExpVMResult): Boolean;
var
  Input: TRegExpInput;
  SlotCount, StartPos: Integer;
  Slots: array of Integer;
begin
  Result := False;
  AResult.Matched := False;
  BuildRegExpInput(AInput, Input);
  SlotCount := (AProgram.CaptureCount + 1) * 2;
  SetLength(Slots, SlotCount);
  StartPos := NormalizeInputIndex(Input, AStartIndex, AProgram.FullUnicode);
  if ARequireStart then
  begin
    FillChar(Slots[0], SlotCount * SizeOf(Integer), $FF);
    if RunVM(AProgram, Input, StartPos, Slots, SlotCount) then
    begin
      AResult.Matched := True;
      SetLength(AResult.CaptureSlots, SlotCount);
      Move(Slots[0], AResult.CaptureSlots[0], SlotCount * SizeOf(Integer));
      Result := True;
    end;
    Exit;
  end;
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
    if RunVM(AProgram, Input, StartPos, Slots, SlotCount) then
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
  end;
end;

end.
