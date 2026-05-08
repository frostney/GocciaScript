unit Goccia.RegExp.VM;

{$I Goccia.inc}

interface

uses
  SysUtils,

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
  TextSemantics;

const
  DEFAULT_STEP_LIMIT = 10000000;
  DEFAULT_BACKTRACK_CAP = 1000000;
  MAX_LOOKBEHIND_DISTANCE = 256;
  MEMO_CAPACITY = 65536;
  MEMO_LOAD_LIMIT = 49152;

type
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
  SetLength(AMemo.Entries, MEMO_CAPACITY);
  AMemo.Count := 0;
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

function CharClassContains(const AClass: TRegExpCharClass;
  ACodePoint: Cardinal): Boolean;
var
  Lo, Hi, Mid: Integer;
begin
  Lo := 0;
  Hi := High(AClass.Ranges);
  while Lo <= Hi do
  begin
    Mid := (Lo + Hi) shr 1;
    if ACodePoint < AClass.Ranges[Mid].Lo then
      Hi := Mid - 1
    else if ACodePoint > AClass.Ranges[Mid].Hi then
      Lo := Mid + 1
    else
      Exit(True);
  end;
  Result := False;
end;

function CharClassContainsLinear(const AClass: TRegExpCharClass;
  ACodePoint: Cardinal): Boolean;
var
  I: Integer;
begin
  for I := 0 to High(AClass.Ranges) do
    if (ACodePoint >= AClass.Ranges[I].Lo) and
       (ACodePoint <= AClass.Ranges[I].Hi) then
      Exit(True);
  Result := False;
end;

function IsWordChar(ACodePoint: Cardinal): Boolean; inline;
begin
  Result := ((ACodePoint >= Ord('a')) and (ACodePoint <= Ord('z'))) or
            ((ACodePoint >= Ord('A')) and (ACodePoint <= Ord('Z'))) or
            ((ACodePoint >= Ord('0')) and (ACodePoint <= Ord('9'))) or
            (ACodePoint = Ord('_'));
end;

function IsLineTerminator(ACodePoint: Cardinal): Boolean; inline;
begin
  Result := (ACodePoint = $0A) or (ACodePoint = $0D) or
            (ACodePoint = $2028) or (ACodePoint = $2029);
end;

function ReadInputCodePoint(const AInput: string; APos: Integer;
  out ACodePoint: Cardinal; out AByteLen: Integer): Boolean;
begin
  if (APos < 1) or (APos > Length(AInput)) then
  begin
    ACodePoint := 0;
    AByteLen := 0;
    Exit(False);
  end;
  Result := TryReadUTF8CodePointAllowSurrogates(AInput, APos, ACodePoint,
    AByteLen);
  if not Result then
  begin
    ACodePoint := Ord(AInput[APos]);
    AByteLen := 1;
    Result := True;
  end;
end;

function GetCodePointBefore(const AInput: string; APos: Integer;
  out ACodePoint: Cardinal): Boolean;
var
  StartPos, ByteLen: Integer;
begin
  Result := False;
  ACodePoint := 0;
  if APos <= 1 then
    Exit;
  StartPos := APos - 1;
  while (StartPos > 1) and ((Ord(AInput[StartPos]) and $C0) = $80) do
    Dec(StartPos);
  Result := TryReadUTF8CodePointAllowSurrogates(AInput, StartPos, ACodePoint,
    ByteLen);
end;

function RunVM(const AProgram: TRegExpProgram; const AInput: string;
  AStartPos: Integer; var ASlots: array of Integer;
  ASlotCount: Integer; AStartPC: Integer = 0;
  AEndPos: PInteger = nil): Boolean;
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
  Memo: TMemoTable;
  SlotCount: Integer;
  I: Integer;
  MatchCP: Cardinal;
  BeforeCP: Cardinal;
  BeforeIsWord, AfterIsWord: Boolean;
  Negated: Boolean;
  BackrefGroup: Integer;
  LookEnd: Integer;
  LookSlots: array of Integer;
  LookMatched: Boolean;
  RefStart, RefEnd, RefPos: Integer;
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
  StackTop := -1;
  SetLength(Stack, 256);
  MemoInit(Memo);

  while PC < Length(AProgram.Code) do
  begin
    Inc(StepCount);
    if StepCount > DEFAULT_STEP_LIMIT then
      raise ERegExpRuntimeError.Create('Maximum regular expression backtrack stack size exceeded');

    Instr := AProgram.Code[PC];
    Op := TRegExpOpCode(Instr and $FF);
    Bx := Integer(Instr shr 8);

    case Op of
      RX_CHAR:
        begin
          if not ReadInputCodePoint(AInput, InputPos,
             CodePoint, ByteLen) then
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
          Inc(InputPos, ByteLen);
          Inc(PC);
        end;

      RX_CHAR_CLASS:
        begin
          if not ReadInputCodePoint(AInput, InputPos,
             CodePoint, ByteLen) then
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
          Inc(InputPos, ByteLen);
          Inc(PC);
        end;

      RX_CHAR_CLASS_NEG:
        begin
          if not ReadInputCodePoint(AInput, InputPos,
             CodePoint, ByteLen) then
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
          Inc(InputPos, ByteLen);
          Inc(PC);
        end;

      RX_ANY:
        begin
          if not ReadInputCodePoint(AInput, InputPos,
             CodePoint, ByteLen) then
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
        PC := Bx;

      RX_SAVE:
        begin
          if Bx < SlotCount then
            ASlots[Bx] := InputPos;
          Inc(PC);
        end;

      RX_BACKREF:
        begin
          Negated := (Bx and $800000) <> 0;
          BackrefGroup := Bx and $7FFFFF;
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
          RefPos := RefStart;
          LookMatched := True;
          while RefPos < RefEnd do
          begin
            if not ReadInputCodePoint(AInput, RefPos,
               RefCP, RefByteLen) then
            begin
              LookMatched := False;
              Break;
            end;
            if not ReadInputCodePoint(AInput, InputPos,
               InputCP, InputByteLen) then
            begin
              LookMatched := False;
              Break;
            end;
            if RefCP <> InputCP then
            begin
              LookMatched := False;
              Break;
            end;
            Inc(RefPos, RefByteLen);
            Inc(InputPos, InputByteLen);
          end;
          if not LookMatched then
          begin
            MemoAdd(Memo, PC, InputPos);
            if not PopBacktrack then Exit;
            Continue;
          end;
          Inc(PC);
        end;

      RX_ASSERT_START:
        begin
          if Bx <> 0 then
          begin
            if InputPos > 1 then
            begin
              if not GetCodePointBefore(AInput, InputPos, BeforeCP) or
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
            if InputPos > 1 then
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
            if ReadInputCodePoint(AInput, InputPos,
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
            if InputPos <= Length(AInput) then
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
          Negated := Bx <> 0;
          BeforeIsWord := False;
          AfterIsWord := False;
          if GetCodePointBefore(AInput, InputPos, BeforeCP) then
            BeforeIsWord := IsWordChar(BeforeCP);
          if ReadInputCodePoint(AInput, InputPos,
             CodePoint, ByteLen) then
            AfterIsWord := IsWordChar(CodePoint);
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
          Negated := (Instr and $80) <> 0;
          LookEnd := Bx;
          SetLength(LookSlots, SlotCount);
          Move(ASlots[0], LookSlots[0], SlotCount * SizeOf(Integer));
          LookMatched := RunVM(AProgram, AInput, InputPos, LookSlots,
            SlotCount, PC + 1);
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
          Negated := (Instr and $80) <> 0;
          LookEnd := Bx;
          LookMatched := False;
          SetLength(LookSlots, SlotCount);
          I := InputPos - 1;
          RefStart := I - MAX_LOOKBEHIND_DISTANCE;
          if RefStart < 1 then
            RefStart := 1;
          while I >= RefStart do
          begin
            Move(ASlots[0], LookSlots[0], SlotCount * SizeOf(Integer));
            RefEnd := 0;
            if RunVM(AProgram, AInput, I, LookSlots, SlotCount, PC + 1,
               @RefEnd) then
            begin
              if RefEnd = InputPos then
              begin
                LookMatched := True;
                Break;
              end;
            end;
            Dec(I);
          end;
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
      Inc(PC);
    end;
  end;
end;

function ExecuteRegExpVM(const AProgram: TRegExpProgram;
  const AInput: string; const AStartIndex: Integer;
  const ARequireStart: Boolean; out AResult: TRegExpVMResult): Boolean;
var
  SlotCount, I, StartPos: Integer;
  Slots: array of Integer;
  ByteLen: Integer;
  CodePoint: Cardinal;
begin
  Result := False;
  AResult.Matched := False;
  SlotCount := (AProgram.CaptureCount + 1) * 2;
  SetLength(Slots, SlotCount);
  StartPos := AStartIndex + 1;
  if ARequireStart then
  begin
    for I := 0 to SlotCount - 1 do
      Slots[I] := -1;
    if RunVM(AProgram, AInput, StartPos, Slots, SlotCount) then
    begin
      AResult.Matched := True;
      SetLength(AResult.CaptureSlots, SlotCount);
      Move(Slots[0], AResult.CaptureSlots[0], SlotCount * SizeOf(Integer));
      Result := True;
    end;
    Exit;
  end;
  while StartPos <= Length(AInput) + 1 do
  begin
    for I := 0 to SlotCount - 1 do
      Slots[I] := -1;
    if RunVM(AProgram, AInput, StartPos, Slots, SlotCount) then
    begin
      AResult.Matched := True;
      SetLength(AResult.CaptureSlots, SlotCount);
      Move(Slots[0], AResult.CaptureSlots[0], SlotCount * SizeOf(Integer));
      Result := True;
      Exit;
    end;
    if StartPos > Length(AInput) then
      Break;
    if TryReadUTF8CodePointAllowSurrogates(AInput, StartPos, CodePoint, ByteLen) then
      Inc(StartPos, ByteLen)
    else
      Inc(StartPos);
  end;
end;

end.
