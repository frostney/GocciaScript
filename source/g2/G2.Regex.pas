unit G2.Regex;
{ G2's regex matcher. It runs the bytecode of Goccia's regex compiler (Goccia.RegExp.Compiler, unchanged)
  with the semantics of Goccia's VM (Goccia.RegExp.VM), restructured for G2:

  - it reads G2's UTF-16 string characters in place: no copy of the subject, no UTF-8 round trip;
  - a match attempt allocates nothing: each lookaround depth has a context whose backtrack stack is one
    flat Integer array (frames hold the capture slots and repeat stack inline) and whose memo is reset in
    O(1) by a generation number, so trying the next start position costs a few stores;
  - patterns whose every path starts with ^ are tried only where ^ can hold, and an unanchored search
    skips to positions whose code unit can start a match (a first-character set computed from the bytecode);
  - the matching loop has no nested procedures (FPC keeps a nested procedure's outer locals in memory), so
    the program counter, input position and stack top stay in registers; a failed attempt moves to the next
    start position inside the loop;
  - character classes test Latin-1 code points with a bitmap.

  The subject pointer is only used while Exec runs, which allocates nothing on the G2 heap, so a moving
  collection cannot invalidate it. }
{$mode delphiunicode}{$H+}
{$POINTERMATH ON}
{$Q-}{$R-}
{$goto on}

interface

uses
  SysUtils, Goccia.RegExp.&Program, Goccia.RegExp.Compiler;

type
  ERegexRuntime = class(Exception);

  PDepthCtx = ^TDepthCtx;
  TMemoCell = record
    Gen: UInt32;
    PC, Pos: Integer;
  end;
  TDepthCtx = record
    Stack: array of Integer;     { frames: slots, repeat stack, then RepeatDepth, RunStart, Pos, PC }
    Memo: array of TMemoCell;
    MemoCount: Integer;
    MemoGen: UInt32;
    { the bit-set memo, used when program length x (subject length + 1) is small: bit PC * Stride + Pos;
      Dirty lists the words set since the last reset, which clears only those }
    Bits: array of UInt64;
    Dirty: array of Integer;
    DirtyCount: Integer;
    Slots: array of Integer;     { a lookaround's copy of the capture slots }
    Rep: array of Integer;       { the repeat stack }
  end;

  TClassBits = record
    Bits: array[0..7] of UInt32;   { Latin-1 members }
    HasHigh: Boolean;              { members above U+00FF: test the ranges }
  end;

  TRegexMatcher = class
  private
    FProg: TRegExpProgram;
    FCodeArr: array of UInt32;       { the program after G2's peephole passes }
    FCode: PUInt32;
    FCodeLen: Integer;
    FUnicode: Boolean;
    FSlotCount: Integer;
    FSlots: array of Integer;
    FCtx: array of PDepthCtx;
    FAnchor: Integer;                { 0: none; 1: every path starts with ^; 2: with ^ under the m flag }
    FClassBits: array of TClassBits; { per character class }
    FSimpleLoop: array of Boolean;   { per PC: a RX_SPLIT whose body is one character instruction and a jump back }
    FSimpleTail: array of Boolean;   { per PC: what follows can only accept }
    FSimpleLook: array of Boolean;   { per PC: a lookaround whose body is one character instruction, then MATCH }
    FPossBody: array of Integer;     { per loop split: its body's one character instruction when the loop never
                                       needs to give a character back (auto-possessive), else -1 }
    FFirstOn, FFirstHigh: Boolean;   { a match must start with a code unit from FFirst (or above U+00FF if FFirstHigh) }
    FFirst: array[0..7] of UInt32;
    FUseBits: Boolean;               { the current subject uses the bit-set memo }
    FStride: Integer;
    FCtx0: PDepthCtx;
    FEmpty: Boolean;
    FIn: PWideChar;
    FLen: Integer;
    FStepLimit: Int64;
    FMatchIndex, FMatchEnd: Integer;
    FErr: Integer;                   { 0, or why the last Exec stopped (step limit, backtrack cap, stack, bytecode) }
    function Ctx(Depth: Integer): PDepthCtx;
    function Run(Depth: Integer; Slots: PInteger; StartPos, StartPC: Integer; EndPos: PInteger;
      Backward, Scan: Boolean): Boolean;
    function Candidate(P: Integer): Integer;
    function AnalyzeAnchor: Integer;
    procedure AnalyzeFirst;
    procedure AnalyzePossessive;
    function FirstSetFrom(PC: Integer; var Bits: TClassBits): Boolean;
    procedure Peephole;
  public
    constructor Create(const AProg: TRegExpProgram; const APattern: UnicodeString);
    destructor Destroy; override;
    { the first match at Start or later (only at Start when RequireStart); Chars/Len: the subject }
    function Exec(Chars: PWideChar; Len, Start: Integer; RequireStart: Boolean): Boolean;
    function TryGetGroup(G: Integer; out St, En: Integer): Boolean;
    function CaptureCount: Integer; inline;
    function HasNamedGroups: Boolean; inline;
    function NamedGroups: TGocciaRegExpNamedGroups; inline;
    property MatchIndex: Integer read FMatchIndex;
    property MatchEnd: Integer read FMatchEnd;
    property Anchor: Integer read FAnchor;
    { the last Exec ran into a limit instead of finishing: it returned False and ErrorMessage says why }
    function Failed: Boolean; inline;
    function ErrorMessage: UnicodeString;
  end;

implementation

uses
  Goccia.NestingLimit, Goccia.RegExp.UnicodeData;

const
  MIN_STEP_LIMIT = 10000000;
  STEPS_PER_INPUT_UNIT = 100;
  BACKTRACK_CAP = 10000000;
  MEMO_INITIAL_CAPACITY = 64;
  MEMO_MAX_CAPACITY = 65536;
  MEMO_MAX_PROBES = 16;
  SIMPLE_GREEDY_LOOP_MIN_REMAINING = 2;   { Goccia: 32; the run path gives the same result at any length }
  SStepLimitExceeded = 'Maximum regular expression step count exceeded';
  SBacktrackExceeded = 'Maximum regular expression backtrack stack size exceeded';
  EMPTY_REGEX = '(?:)';

{ ---------------- characters ---------------- }

function IsHigh(U: Cardinal): Boolean; inline; begin Result := (U >= $D800) and (U <= $DBFF); end;
function IsLow(U: Cardinal): Boolean; inline; begin Result := (U >= $DC00) and (U <= $DFFF); end;
function PairCP(H, L: Cardinal): Cardinal; inline; begin Result := $10000 + ((H - $D800) shl 10) + (L - $DC00); end;

function IsLineTerminator(C: Cardinal): Boolean; inline;
begin
  Result := (C = $0A) or (C = $0D) or (C = $2028) or (C = $2029);
end;

function IsBasicWordChar(C: Cardinal): Boolean; inline;
begin
  Result := ((C >= Ord('a')) and (C <= Ord('z'))) or ((C >= Ord('A')) and (C <= Ord('Z'))) or
    ((C >= Ord('0')) and (C <= Ord('9'))) or (C = Ord('_'));
end;

function IsWordChar(C: Cardinal; UnicodeAware, IgnoreCase: Boolean): Boolean; inline;
begin
  if IsBasicWordChar(C) then Exit(True);
  if not UnicodeAware or not IgnoreCase then Exit(False);
  Result := IsBasicWordChar(RegExpCanonicalizeCodePoint(C, UnicodeAware, IgnoreCase));
end;

{ the code point at Pos and its width in code units (a surrogate pair is one code point in Unicode mode) }
function ReadCP(P: PWideChar; Len, Pos: Integer; Uni: Boolean; out CP: Cardinal; out W: Integer): Boolean; inline;
var U: Cardinal;
begin
  if (Pos < 0) or (Pos >= Len) then begin CP := 0; W := 0; Exit(False); end;
  U := Ord(P[Pos]);
  if Uni and IsHigh(U) and (Pos + 1 < Len) and IsLow(Ord(P[Pos + 1])) then
  begin
    CP := PairCP(U, Ord(P[Pos + 1]));
    W := 2;
  end
  else
  begin
    CP := U;
    W := 1;
  end;
  Result := True;
end;

{ the code point that ends at Pos }
function ReadCPBefore(P: PWideChar; Pos: Integer; Uni: Boolean; out CP: Cardinal; out W: Integer): Boolean; inline;
var U: Cardinal;
begin
  if Pos <= 0 then begin CP := 0; W := 0; Exit(False); end;
  U := Ord(P[Pos - 1]);
  if Uni and IsLow(U) and (Pos >= 2) and IsHigh(Ord(P[Pos - 2])) then
  begin
    CP := PairCP(Ord(P[Pos - 2]), U);
    W := 2;
  end
  else
  begin
    CP := U;
    W := 1;
  end;
  Result := True;
end;

function ClassContains(const C: TRegExpCharClass; CP: Cardinal): Boolean; inline;
var Hi, I, N, Lo, Mid: Integer;
begin
  N := Length(C.Ranges);
  if N <= 8 then
  begin
    for I := 0 to N - 1 do
      if (CP >= C.Ranges[I].Lo) and (CP <= C.Ranges[I].Hi) then Exit(True);
    Exit(False);
  end;
  if Length(C.PageFirstRange) = REGEXP_CHAR_CLASS_PAGE_COUNT then
  begin
    I := C.PageFirstRange[CP shr REGEXP_CHAR_CLASS_PAGE_BITS];
    while (I <= High(C.Ranges)) and (C.Ranges[I].Lo <= CP) do
    begin
      if CP <= C.Ranges[I].Hi then Exit(True);
      Inc(I);
    end;
    Exit(False);
  end;
  Lo := 0;
  Hi := High(C.Ranges);
  while Lo <= Hi do
  begin
    Mid := Lo + (Hi - Lo) div 2;
    if CP < C.Ranges[Mid].Lo then Hi := Mid - 1
    else if CP > C.Ranges[Mid].Hi then Lo := Mid + 1
    else Exit(True);
  end;
  Result := False;
end;


{ ---------------- memo: (PC, position) pairs where a path has failed ---------------- }

function MemoHash(PC, Pos, Cap: Integer): Integer; inline;
var H: Cardinal;
begin
  H := Cardinal(PC);
  H := (H shl 5) xor (H shr 3) xor Cardinal(Pos);
  H := H xor (H shr 7) xor (H shr 15);
  Result := Integer(H and Cardinal(Cap - 1));
end;

function MemoContains(C: PDepthCtx; PC, Pos: Integer): Boolean; inline;
var Cap, Idx, I: Integer;
begin
  Cap := Length(C^.Memo);
  if (Cap = 0) or (C^.MemoCount = 0) then Exit(False);
  Idx := MemoHash(PC, Pos, Cap);
  for I := 0 to MEMO_MAX_PROBES - 1 do
  begin
    if C^.Memo[Idx].Gen <> C^.MemoGen then Exit(False);
    if (C^.Memo[Idx].PC = PC) and (C^.Memo[Idx].Pos = Pos) then Exit(True);
    Idx := (Idx + 1) and (Cap - 1);
  end;
  Result := False;
end;

function MemoTryAdd(C: PDepthCtx; PC, Pos: Integer): Boolean;
var Cap, Idx, I: Integer;
begin
  Cap := Length(C^.Memo);
  Idx := MemoHash(PC, Pos, Cap);
  for I := 0 to MEMO_MAX_PROBES - 1 do
  begin
    if C^.Memo[Idx].Gen <> C^.MemoGen then
    begin
      C^.Memo[Idx].Gen := C^.MemoGen;
      C^.Memo[Idx].PC := PC;
      C^.Memo[Idx].Pos := Pos;
      Inc(C^.MemoCount);
      Exit(True);
    end;
    if (C^.Memo[Idx].PC = PC) and (C^.Memo[Idx].Pos = Pos) then Exit(True);
    Idx := (Idx + 1) and (Cap - 1);
  end;
  Result := False;
end;

procedure MemoGrow(C: PDepthCtx);
var Old: array of TMemoCell; I, Cap, NewCap: Integer; Gen: UInt32;
begin
  Cap := Length(C^.Memo);
  if Cap >= MEMO_MAX_CAPACITY then Exit;
  NewCap := Cap * 2;
  if NewCap > MEMO_MAX_CAPACITY then NewCap := MEMO_MAX_CAPACITY;
  Old := C^.Memo;
  Gen := C^.MemoGen;
  C^.Memo := nil;
  SetLength(C^.Memo, NewCap);
  FillChar(C^.Memo[0], NewCap * SizeOf(TMemoCell), 0);
  C^.MemoCount := 0;
  for I := 0 to High(Old) do
    if Old[I].Gen = Gen then MemoTryAdd(C, Old[I].PC, Old[I].Pos);
end;

procedure MemoAdd(C: PDepthCtx; PC, Pos: Integer);
var Cap: Integer;
begin
  if Length(C^.Memo) = 0 then
  begin
    SetLength(C^.Memo, MEMO_INITIAL_CAPACITY);
    FillChar(C^.Memo[0], MEMO_INITIAL_CAPACITY * SizeOf(TMemoCell), 0);
  end;
  while True do
  begin
    Cap := Length(C^.Memo);
    if (Cap = MEMO_MAX_CAPACITY) and (C^.MemoCount * 4 >= Cap * 3) then Exit;
    if (C^.MemoCount * 4 >= Cap * 3) and (Cap < MEMO_MAX_CAPACITY) then MemoGrow(C);
    if MemoTryAdd(C, PC, Pos) then Exit;
    if Cap >= MEMO_MAX_CAPACITY then Exit;
    MemoGrow(C);
  end;
end;

{ the bit-set memo }
function BitsContains(C: PDepthCtx; Bit: Integer): Boolean; inline;
begin
  Result := (C^.DirtyCount > 0) and (((C^.Bits[Bit shr 6] shr (Bit and 63)) and 1) <> 0);
end;

procedure BitsAdd(C: PDepthCtx; Bit: Integer); inline;
var W: Integer;
begin
  W := Bit shr 6;
  if C^.Bits[W] = 0 then
  begin
    if C^.DirtyCount >= Length(C^.Dirty) then SetLength(C^.Dirty, C^.DirtyCount * 2 + 64);
    C^.Dirty[C^.DirtyCount] := W;
    Inc(C^.DirtyCount);
  end;
  C^.Bits[W] := C^.Bits[W] or (UInt64(1) shl (Bit and 63));
end;

procedure BitsReset(C: PDepthCtx); inline;
var I: Integer;
begin
  for I := 0 to C^.DirtyCount - 1 do C^.Bits[C^.Dirty[I]] := 0;
  C^.DirtyCount := 0;
end;

{ a new attempt: all earlier entries become stale at once }
procedure MemoReset(C: PDepthCtx); inline;
begin
  Inc(C^.MemoGen);
  if C^.MemoGen = 0 then
  begin
    if Length(C^.Memo) > 0 then FillChar(C^.Memo[0], Length(C^.Memo) * SizeOf(TMemoCell), 0);
    C^.MemoGen := 1;
  end;
  C^.MemoCount := 0;
end;

{ ---------------- matcher ---------------- }

function InClass(const CB: TClassBits; const C: TRegExpCharClass; CP: Cardinal): Boolean; inline;
begin
  if CP <= $FF then Result := (CB.Bits[CP shr 5] and (UInt32(1) shl (CP and 31))) <> 0
  else if not CB.HasHigh then Result := False
  else Result := ClassContains(C, CP);
end;

function MatchSeq(P: PWideChar; Len, Pos: Integer; Uni: Boolean; const ASeq: TRegExpStringSequence; out AEnd: Integer): Boolean;
var I, W: Integer; CP: Cardinal;
begin
  for I := 0 to High(ASeq.CodePoints) do
  begin
    if not ReadCP(P, Len, Pos, Uni, CP, W) or (CP <> ASeq.CodePoints[I]) then Exit(False);
    Inc(Pos, W);
  end;
  AEnd := Pos;
  Result := True;
end;

function WidthBefore(P: PWideChar; APos, ALow: Integer; Uni: Boolean): Integer; inline;
begin
  if Uni and (APos - 2 >= ALow) and IsLow(Ord(P[APos - 1])) and IsHigh(Ord(P[APos - 2])) then Result := 2
  else Result := 1;
end;

function SeenAt(C: PDepthCtx; UseBits: Boolean; Stride, PC, Pos: Integer): Boolean; inline;
begin
  if UseBits then Result := BitsContains(C, PC * Stride + Pos)
  else Result := MemoContains(C, PC, Pos);
end;

procedure GrowStack(C: PDepthCtx; Need: Integer);
begin
  SetLength(C^.Stack, Need * 2 + 256);
end;

procedure GrowRep(C: PDepthCtx; Need: Integer);
begin
  SetLength(C^.Rep, Need * 2 + 8);
end;

constructor TRegexMatcher.Create(const AProg: TRegExpProgram; const APattern: UnicodeString);
var I, J: Integer; Lo, Hi: Cardinal; Op: TRegExpOpCode; T: Integer;
begin
  inherited Create;
  FProg := AProg;
  FCodeArr := Copy(AProg.Code);   { a copy: the peephole passes rewrite it }
  FProg.CharClasses := Copy(AProg.CharClasses);
  FUnicode := FProg.FullUnicode;
  FSlotCount := (FProg.CaptureCount + 1) * 2;
  SetLength(FSlots, FSlotCount);
  FEmpty := APattern = EMPTY_REGEX;
  Peephole;
  FCodeLen := Length(FCodeArr);
  if FCodeLen > 0 then FCode := @FCodeArr[0] else FCode := nil;
  SetLength(FClassBits, Length(FProg.CharClasses));
  for I := 0 to High(FProg.CharClasses) do
  begin
    FillChar(FClassBits[I], SizeOf(TClassBits), 0);
    for J := 0 to High(FProg.CharClasses[I].Ranges) do
    begin
      Lo := FProg.CharClasses[I].Ranges[J].Lo;
      Hi := FProg.CharClasses[I].Ranges[J].Hi;
      if Hi > $FF then FClassBits[I].HasHigh := True;
      while (Lo <= Hi) and (Lo <= $FF) do
      begin
        FClassBits[I].Bits[Lo shr 5] := FClassBits[I].Bits[Lo shr 5] or (UInt32(1) shl (Lo and 31));
        Inc(Lo);
      end;
    end;
  end;
  FAnchor := AnalyzeAnchor;
  SetLength(FSimpleLoop, FCodeLen + 1);
  SetLength(FSimpleTail, FCodeLen + 1);
  for I := 0 to FCodeLen - 3 do
    FSimpleLoop[I] := (TRegExpOpCode(FCode[I] and $FF) = RX_SPLIT) and
      (TRegExpOpCode(FCode[I + 1] and $FF) in [RX_CHAR, RX_CHAR_CLASS, RX_CHAR_CLASS_NEG, RX_ANY]) and
      (TRegExpOpCode(FCode[I + 2] and $FF) = RX_JUMP) and (Integer(FCode[I + 2] shr 8) = I);
  { per PC: does the rest of the program only save and accept (optionally after an unconditional $) }
  for I := 0 to FCodeLen - 1 do
  begin
    T := I;
    if (TRegExpOpCode(FCode[T] and $FF) = RX_ASSERT_END) and (Integer(FCode[T] shr 8) = 0) then Inc(T);
    while (T < FCodeLen) and (TRegExpOpCode(FCode[T] and $FF) = RX_SAVE) do Inc(T);
    Op := RX_FAIL;
    if T < FCodeLen then Op := TRegExpOpCode(FCode[T] and $FF);
    FSimpleTail[I] := (T < FCodeLen) and (Op = RX_MATCH) and
      not ((TRegExpOpCode(FCode[I] and $FF) = RX_ASSERT_END) and (Integer(FCode[I] shr 8) <> 0));
  end;
  SetLength(FSimpleLook, FCodeLen + 1);
  for I := 0 to FCodeLen - 3 do
    FSimpleLook[I] := (TRegExpOpCode(FCode[I] and $FF) in [RX_LOOKAHEAD, RX_LOOKBEHIND]) and
      (TRegExpOpCode(FCode[I + 1] and $FF) in [RX_CHAR, RX_CHAR_CLASS, RX_CHAR_CLASS_NEG, RX_ANY]) and
      (TRegExpOpCode(FCode[I + 2] and $FF) = RX_MATCH) and
      ((Integer(FCode[I] shr 8) and LOOK_TARGET_MASK) = I + 3);
  AnalyzeFirst;
  AnalyzePossessive;
  FCtx0 := Ctx(0);
  if GetEnvironmentVariable('G2_REDBG') <> '' then
  begin
    { the program after the peephole passes, and what the analyses found }
    J := 0;
    for I := 0 to FCodeLen - 1 do if FPossBody[I] >= 0 then Inc(J);
    Write(ErrOutput, 'regex /', UTF8Encode(APattern), '/ code ', Length(AProg.Code), '->', FCodeLen, ' anchor ', FAnchor,
      ' first ', FFirstOn, ' poss ', J, ' slots ', FSlotCount, ':');
    for I := 0 to FCodeLen - 1 do Write(ErrOutput, ' ', Ord(TRegExpOpCode(FCode[I] and $FF)), '/', Integer(FCode[I] shr 8));
    WriteLn(ErrOutput);
  end;
  FMatchIndex := -1;
  FMatchEnd := -1;
end;

destructor TRegexMatcher.Destroy;
var I: Integer;
begin
  for I := 0 to High(FCtx) do
    if FCtx[I] <> nil then Dispose(FCtx[I]);
  inherited;
end;

function TRegexMatcher.Ctx(Depth: Integer): PDepthCtx;
var I, Old: Integer;
begin
  if Depth >= Length(FCtx) then
  begin
    Old := Length(FCtx);
    SetLength(FCtx, Depth + 4);
    for I := Old to High(FCtx) do FCtx[I] := nil;
  end;
  Result := FCtx[Depth];
  if Result = nil then
  begin
    New(Result);
    Result^.MemoCount := 0;
    Result^.MemoGen := 1;
    Result^.DirtyCount := 0;
    SetLength(Result^.Stack, 256);
    SetLength(Result^.Rep, 8);
    FCtx[Depth] := Result;
  end;
end;

{ G2's peephole passes over the compiled program. They keep its results and change how many steps they take:
  R1 folds an alternation of single-character instructions ((a|b), (?:x|[0-9]|y)) into one class: one
     instruction and no backtrack frame instead of a split per alternative. Two alternatives that both match
     a character continue from the same state, so trying the second after the first cannot change a result.
  R2 removes a loop's empty-iteration guard (REPEAT_ENTER ... REPEAT_CHECK) when every path through the body
     consumes a character, so the check can never fail.
  R3 removes the per-iteration reset of a quantified group's capture, (x)* -> CLEAR g; SAVE 2g; ...; SAVE 2g+1,
     when the program never reads a capture while matching (no backreference, no capture guard): the start is
     set by the next instruction and every way out of the body that does not fail passes the end's SAVE, so
     at MATCH the slots are the same.
  Then the dead instructions are removed and every target is renumbered; a jump to the next live
  instruction goes too, unless it lands on a split (the VM's empty-loop exit looks at jumps to splits). }
procedure TRegexMatcher.Peephole;
var
  N, P, X, E, I, K, Depth, NewLen, T, Changes: Integer;
  Dead: array of Boolean;
  Targets: array of Integer;
  Map: array of Integer;
  Dist: array of Integer;
  NewCode: array of UInt32;
  Ranges: TRegExpCharRangeArray;
  Cls: TRegExpCharClass;
  Changed, Escapes: Boolean;

  function OpAt(I: Integer): TRegExpOpCode; inline; begin Result := TRegExpOpCode(FCodeArr[I] and $FF); end;
  function BxAt(I: Integer): Integer; inline; begin Result := Integer(FCodeArr[I] shr 8); end;
  function IsGuardWord(I: Integer): Boolean; inline;
  begin
    Result := (I > 0) and (OpAt(I - 1) = RX_CAPTURE_UNDEFINED_JUMP);
  end;
  { the PCs an instruction can continue at besides falling through }
  function TargetOf(I: Integer): Integer;
  begin
    case OpAt(I) of
      RX_JUMP, RX_SPLIT, RX_SPLIT_LAZY: Result := BxAt(I);
      RX_LOOKAHEAD, RX_LOOKBEHIND: Result := BxAt(I) and LOOK_TARGET_MASK;
    else Result := -1;
    end;
  end;
  procedure CountTargets;
  var J, T: Integer;
  begin
    for J := 0 to N do Targets[J] := 0;
    for J := 0 to N - 1 do
      if not Dead[J] then
      begin
        T := TargetOf(J);
        if (T >= 0) and (T <= N) then Inc(Targets[T]);
      end;
  end;
  function IsSingle(I: Integer): Boolean; inline;
  begin
    Result := (I < N) and not Dead[I] and (OpAt(I) in [RX_CHAR, RX_CHAR_CLASS]);
  end;
  procedure AddRangesOf(I: Integer);
  var J, L: Integer;
  begin
    if OpAt(I) = RX_CHAR then
    begin
      L := Length(Ranges);
      SetLength(Ranges, L + 1);
      Ranges[L].Lo := Cardinal(BxAt(I));
      Ranges[L].Hi := Cardinal(BxAt(I));
    end
    else
      for J := 0 to High(FProg.CharClasses[BxAt(I)].Ranges) do
      begin
        L := Length(Ranges);
        SetLength(Ranges, L + 1);
        Ranges[L] := FProg.CharClasses[BxAt(I)].Ranges[J];
      end;
  end;
  { sorted, merged ranges }
  procedure Normalize;
  var A, B: Integer; Tmp: TRegExpCharRange; Outp: TRegExpCharRangeArray;
  begin
    for A := 1 to High(Ranges) do
    begin
      Tmp := Ranges[A];
      B := A - 1;
      while (B >= 0) and (Ranges[B].Lo > Tmp.Lo) do begin Ranges[B + 1] := Ranges[B]; Dec(B); end;
      Ranges[B + 1] := Tmp;
    end;
    Outp := nil;
    for A := 0 to High(Ranges) do
      if (Length(Outp) > 0) and (Ranges[A].Lo <= Outp[High(Outp)].Hi + 1) then
      begin
        if Ranges[A].Hi > Outp[High(Outp)].Hi then Outp[High(Outp)].Hi := Ranges[A].Hi;
      end
      else
      begin
        SetLength(Outp, Length(Outp) + 1);
        Outp[High(Outp)] := Ranges[A];
      end;
    Ranges := Outp;
  end;
  { the least number of characters any path from From to To consumes; -1 when a path leaves [From, To] }
  function MinConsumed(From, To_: Integer): Integer;
  var J, Pass, D, Nx, Cost: Integer; Moved: Boolean;
    procedure Relax(Target, Value: Integer);
    begin
      while (Target >= From) and (Target < To_) and Dead[Target] do Inc(Target);   { dead code falls through }
      if (Target < From) or (Target > To_) then begin Escapes := True; Exit; end;
      if Value < Dist[Target] then begin Dist[Target] := Value; Moved := True; end;
    end;
  begin
    for J := From to To_ do Dist[J] := MaxInt;
    Dist[From] := 0;
    Escapes := False;
    for Pass := 0 to (To_ - From + 2) * 2 do
    begin
      Moved := False;
      for J := From to To_ - 1 do
      begin
        if Dead[J] or (Dist[J] = MaxInt) then Continue;
        D := Dist[J];
        case OpAt(J) of
          RX_CHAR, RX_CHAR_CLASS, RX_CHAR_CLASS_NEG, RX_ANY: Relax(J + 1, D + 1);
          RX_JUMP: Relax(BxAt(J), D);
          RX_SPLIT, RX_SPLIT_LAZY: begin Relax(J + 1, D); Relax(BxAt(J), D); end;
          RX_LOOKAHEAD, RX_LOOKBEHIND: Relax(BxAt(J) and LOOK_TARGET_MASK, D);
          RX_CAPTURE_UNDEFINED_JUMP: begin Relax(J + 2, D); if J + 1 < N then Relax(BxAt(J + 1), D); end;
          RX_FAIL: ;
          RX_MATCH: Escapes := True;
        else Relax(J + 1, D);   { zero-width, or may consume nothing (backreference, string set) }
        end;
        if Escapes then Exit(-1);
      end;
      if not Moved then Break;
    end;
    if Dist[To_] = MaxInt then Result := MaxInt else Result := Dist[To_];
  end;

begin
  if FEmpty then Exit;
  N := Length(FCodeArr);
  if N = 0 then Exit;
  SetLength(Dead, N + 1);
  SetLength(Targets, N + 1);
  SetLength(Dist, N + 1);
  Changes := 0;
  repeat
    Changed := False;
    { R1, innermost alternatives first }
    CountTargets;
    for P := N - 4 downto 0 do
    begin
      if Dead[P] or (OpAt(P) <> RX_SPLIT) then Continue;
      X := BxAt(P);
      if (X <> P + 3) or not IsSingle(P + 1) or Dead[P + 2] or (OpAt(P + 2) <> RX_JUMP) or not IsSingle(X) then Continue;
      if IsGuardWord(P + 2) or (X + 1 > N) then Continue;
      E := BxAt(P + 2);
      if not ((E = X + 1) or ((X + 1 < N) and not Dead[X + 1] and (OpAt(X + 1) = RX_JUMP) and (BxAt(X + 1) = E) and
         not IsGuardWord(X + 1))) then Continue;
      if (Targets[P + 1] <> 0) or (Targets[P + 2] <> 0) or (Targets[X] <> 1) then Continue;
      if (E <> X + 1) and (Targets[X + 1] <> 0) then Continue;
      Ranges := nil;
      AddRangesOf(P + 1);
      AddRangesOf(X);
      Normalize;
      Cls.Ranges := Ranges;
      Cls.PageFirstRange := nil;
      K := Length(FProg.CharClasses);
      if K > REGEXP_MAX_OPERAND then Continue;
      SetLength(FProg.CharClasses, K + 1);
      FProg.CharClasses[K] := Cls;
      FCodeArr[P] := UInt32(Ord(RX_CHAR_CLASS)) or (UInt32(K) shl 8);
      FCodeArr[P + 1] := UInt32(Ord(RX_JUMP)) or (UInt32(E) shl 8);
      Dead[P + 2] := True;
      Dead[X] := True;
      if E <> X + 1 then Dead[X + 1] := True;
      Changed := True;
      Inc(Changes);
      CountTargets;
    end;
    { R2 }
    CountTargets;
    for P := 0 to N - 1 do
    begin
      if Dead[P] or (OpAt(P) <> RX_REPEAT_ENTER) then Continue;
      Depth := 0;
      K := -1;
      for I := P + 1 to N - 1 do
      begin
        if Dead[I] then Continue;
        if OpAt(I) = RX_REPEAT_ENTER then Inc(Depth)
        else if OpAt(I) = RX_REPEAT_CHECK then
        begin
          if Depth = 0 then begin K := I; Break; end;
          Dec(Depth);
        end;
      end;
      if K < 0 then Continue;
      { nothing outside may jump into the body }
      Escapes := False;
      for I := 0 to N - 1 do
        if not Dead[I] and ((I < P) or (I > K)) then
        begin
          T := TargetOf(I);
          if (T > P) and (T <= K) then Escapes := True;
        end;
      if Escapes then Continue;
      T := MinConsumed(P + 1, K);
      if (T >= 1) and (T <> MaxInt) then
      begin
        Dead[P] := True;
        Dead[K] := True;
        Changed := True;
        Inc(Changes);
        CountTargets;
      end;
    end;
    { R3 }
    if not FProg.HasBackreferences then
    begin
      Escapes := False;
      for I := 0 to N - 1 do
        if not Dead[I] and (OpAt(I) in [RX_BACKREF, RX_CAPTURE_UNDEFINED_JUMP]) then Escapes := True;
      if not Escapes then
        for P := 0 to N - 2 do
        begin
          if Dead[P] or (OpAt(P) <> RX_CLEAR_CAPTURES) or ((BxAt(P) and CLEAR_CAPTURE_COUNT_MASK) <> 1) then Continue;
          K := BxAt(P) shr CLEAR_CAPTURE_COUNT_BITS;   { the group }
          X := P + 1;
          while (X < N) and Dead[X] do Inc(X);
          if (X >= N) or (OpAt(X) <> RX_SAVE) or (BxAt(X) <> 2 * K) then Continue;
          E := -1;
          for I := X + 1 to N - 1 do
            if not Dead[I] and (OpAt(I) = RX_SAVE) and (BxAt(I) = 2 * K + 1) then
            begin
              if E >= 0 then begin E := -2; Break; end;   { not a single end SAVE }
              E := I;
            end;
          if E < 0 then Continue;
          { the body (X, E] is entered only at X, and leaves only through E (or by failing) }
          Escapes := False;
          for I := 0 to N - 1 do
          begin
            if Dead[I] then Continue;
            T := TargetOf(I);
            if T < 0 then Continue;
            if (I > X) and (I < E) and ((T <= X) or (T > E)) then Escapes := True;
            if ((I <= X) or (I > E)) and (T > X) and (T <= E) then Escapes := True;
          end;
          for I := X + 1 to E - 1 do
            if not Dead[I] and (OpAt(I) = RX_MATCH) then Escapes := True;
          if Escapes then Continue;
          Dead[P] := True;
          Changed := True;
          Inc(Changes);
          CountTargets;
        end;
    end;
  until not Changed;
  if Changes = 0 then Exit;

  { compaction: dead instructions go, targets are renumbered (a dead target means the next live one); a jump
    whose target is the next live instruction goes too, unless that instruction is a split }
  SetLength(Map, N + 1);
  for I := 0 to N - 1 do
    if not Dead[I] and (OpAt(I) = RX_JUMP) and not IsGuardWord(I) then
    begin
      X := BxAt(I);
      while (X < N) and Dead[X] do Inc(X);
      T := I + 1;
      while (T < N) and Dead[T] do Inc(T);
      if (X = T) and ((X >= N) or (OpAt(X) <> RX_SPLIT)) then Dead[I] := True;
    end;
  K := 0;
  for I := 0 to N - 1 do
  begin
    Map[I] := K;
    if not Dead[I] then Inc(K);
  end;
  Map[N] := K;
  NewLen := Map[N];
  SetLength(NewCode, NewLen);
  for I := 0 to N - 1 do
  begin
    if Dead[I] then Continue;
    case OpAt(I) of
      RX_JUMP, RX_SPLIT, RX_SPLIT_LAZY:
        NewCode[Map[I]] := UInt32(Ord(OpAt(I))) or (UInt32(Map[BxAt(I)]) shl 8);
      RX_LOOKAHEAD, RX_LOOKBEHIND:
        NewCode[Map[I]] := UInt32(Ord(OpAt(I))) or
          (UInt32((BxAt(I) and not LOOK_TARGET_MASK) or Map[BxAt(I) and LOOK_TARGET_MASK]) shl 8);
    else NewCode[Map[I]] := FCodeArr[I];
    end;
  end;
  FCodeArr := NewCode;
end;

{ 1 when every path from the start asserts ^ (input start) before anything else, 2 when every path asserts
  ^ and some of them under the m flag (a line start), else 0 }
function TRegexMatcher.AnalyzeAnchor: Integer;
var Seen: array of Boolean;
  function Combine(A, B: Integer): Integer;
  begin
    if (A = 0) or (B = 0) then Result := 0
    else if (A = 2) or (B = 2) then Result := 2
    else Result := 1;
  end;
  function Walk(PC: Integer): Integer;
  var Op: TRegExpOpCode; Bx: Integer;
  begin
    while True do
    begin
      if (PC < 0) or (PC >= FCodeLen) then Exit(0);
      if Seen[PC] then Exit(1);   { already explored: its result is part of the combination }
      Seen[PC] := True;
      Op := TRegExpOpCode(FCode[PC] and $FF);
      Bx := Integer(FCode[PC] shr 8);
      case Op of
        RX_SAVE, RX_CLEAR_CAPTURES, RX_REPEAT_ENTER, RX_REPEAT_CHECK: Inc(PC);
        RX_JUMP: PC := Bx;
        RX_SPLIT, RX_SPLIT_LAZY: Exit(Combine(Walk(PC + 1), Walk(Bx)));
        RX_ASSERT_START: if Bx = 0 then Exit(1) else Exit(2);
      else Exit(0);
      end;
    end;
  end;
begin
  if FEmpty or (FCodeLen = 0) then Exit(0);
  SetLength(Seen, FCodeLen);
  Result := Walk(0);
end;

{ the set of code units a match can start with, from every path's first consuming instruction; off when a
  path can match without consuming (or starts with something this does not follow) }
procedure TRegexMatcher.AnalyzeFirst;
var Seen: array of Boolean; I: Integer;
  function Walk(PC: Integer): Boolean;
  var Op: TRegExpOpCode; Bx, K: Integer;
  begin
    while True do
    begin
      if (PC < 0) or (PC >= FCodeLen) then Exit(False);
      if Seen[PC] then Exit(True);
      Seen[PC] := True;
      Op := TRegExpOpCode(FCode[PC] and $FF);
      Bx := Integer(FCode[PC] shr 8);
      case Op of
        { zero-width: the first consumed code unit comes later }
        RX_SAVE, RX_CLEAR_CAPTURES, RX_REPEAT_ENTER, RX_REPEAT_CHECK, RX_ASSERT_START, RX_ASSERT_END, RX_ASSERT_WORD:
          Inc(PC);
        RX_LOOKAHEAD, RX_LOOKBEHIND: PC := Bx and LOOK_TARGET_MASK;
        RX_JUMP: PC := Bx;
        RX_SPLIT, RX_SPLIT_LAZY: Exit(Walk(PC + 1) and Walk(Bx));
        RX_CAPTURE_UNDEFINED_JUMP:
          if PC + 1 < FCodeLen then Exit(Walk(PC + 2) and Walk(Integer(FCode[PC + 1] shr 8))) else Exit(False);
        RX_CHAR:
          begin
            if Cardinal(Bx) <= $FF then FFirst[Bx shr 5] := FFirst[Bx shr 5] or (UInt32(1) shl (Bx and 31))
            else FFirstHigh := True;
            Exit(True);
          end;
        RX_CHAR_CLASS:
          begin
            for K := 0 to 7 do FFirst[K] := FFirst[K] or FClassBits[Bx].Bits[K];
            if FClassBits[Bx].HasHigh then FFirstHigh := True;
            Exit(True);
          end;
        RX_CHAR_CLASS_NEG:
          begin
            for K := 0 to 7 do FFirst[K] := FFirst[K] or not FClassBits[Bx].Bits[K];
            FFirstHigh := True;
            Exit(True);
          end;
        RX_FAIL: Exit(True);   { contributes no start }
      else Exit(False);        { MATCH (empty match), ANY, BACKREF, STRING_SET }
      end;
    end;
  end;
begin
  FFirstOn := False;
  FFirstHigh := False;
  FillChar(FFirst, SizeOf(FFirst), 0);
  if FEmpty or (FCodeLen = 0) then Exit;
  SetLength(Seen, FCodeLen);
  FFirstOn := Walk(0);
  if not FFirstOn and FProg.StartCheck.Enabled then
  begin
    { Goccia's compiler found a first-character set where this walk did not }
    FFirstOn := True;
    FFirstHigh := FProg.StartCheck.HasNonLatin1;
    for I := 0 to 7 do FFirst[I] := FProg.StartCheck.Latin1Bits[I];
    Exit;
  end;
  if FFirstOn and FFirstHigh then
  begin
    { a filter that passes every Latin-1 unit and everything above is no filter }
    FFirstOn := False;
    for I := 0 to 7 do
      if FFirst[I] <> $FFFFFFFF then FFirstOn := True;
  end;
  if not FFirstOn then
  begin
    FFirstHigh := False;
    FillChar(FFirst, SizeOf(FFirst), 0);
  end;
end;

{ the code units a path from PC must consume first; False when some path can match, or reach something this
  does not follow, before consuming }
function TRegexMatcher.FirstSetFrom(PC: Integer; var Bits: TClassBits): Boolean;
var Seen: array of Boolean;
  function Walk(PC: Integer): Boolean;
  var Op: TRegExpOpCode; Bx, K: Integer;
  begin
    while True do
    begin
      if (PC < 0) or (PC >= FCodeLen) then Exit(False);
      if Seen[PC] then Exit(True);
      Seen[PC] := True;
      Op := TRegExpOpCode(FCode[PC] and $FF);
      Bx := Integer(FCode[PC] shr 8);
      case Op of
        RX_SAVE, RX_CLEAR_CAPTURES, RX_REPEAT_ENTER, RX_REPEAT_CHECK, RX_ASSERT_START, RX_ASSERT_END, RX_ASSERT_WORD:
          Inc(PC);
        RX_LOOKAHEAD, RX_LOOKBEHIND: PC := Bx and LOOK_TARGET_MASK;
        RX_JUMP: PC := Bx;
        RX_SPLIT, RX_SPLIT_LAZY: Exit(Walk(PC + 1) and Walk(Bx));
        RX_CAPTURE_UNDEFINED_JUMP:
          if PC + 1 < FCodeLen then Exit(Walk(PC + 2) and Walk(Integer(FCode[PC + 1] shr 8))) else Exit(False);
        RX_CHAR:
          begin
            if Cardinal(Bx) <= $FF then Bits.Bits[Bx shr 5] := Bits.Bits[Bx shr 5] or (UInt32(1) shl (Bx and 31))
            else Bits.HasHigh := True;
            Exit(True);
          end;
        RX_CHAR_CLASS:
          begin
            for K := 0 to 7 do Bits.Bits[K] := Bits.Bits[K] or FClassBits[Bx].Bits[K];
            if FClassBits[Bx].HasHigh then Bits.HasHigh := True;
            Exit(True);
          end;
        RX_CHAR_CLASS_NEG, RX_ANY:
          begin
            for K := 0 to 7 do
              if Op = RX_ANY then Bits.Bits[K] := $FFFFFFFF
              else Bits.Bits[K] := Bits.Bits[K] or not FClassBits[Bx].Bits[K];
            Bits.HasHigh := True;
            Exit(True);
          end;
        RX_FAIL: Exit(True);
      else Exit(False);   { MATCH, BACKREF, STRING_SET }
      end;
    end;
  end;
begin
  SetLength(Seen, FCodeLen);
  Result := Walk(PC);
end;

{ Auto-possessive loops (PCRE's term). A greedy loop L: SPLIT exit; body; JUMP L whose body consumes exactly one
  character (one character instruction among saves and resets) never needs to give one back when no
  character its body accepts can start what follows the loop: after k < n iterations, the rest of the pattern
  would have to begin with a character the body consumed, and its first consuming instruction rejects it.
  So the split pushes no frame: it looks at the next character and enters the body or leaves. }
procedure TRegexMatcher.AnalyzePossessive;
var PC, X, I, Body, K: Integer; Op: TRegExpOpCode; BodyBits, Follow: TClassBits; Ok: Boolean;
begin
  SetLength(FPossBody, FCodeLen + 1);
  for PC := 0 to FCodeLen do FPossBody[PC] := -1;
  for PC := 0 to FCodeLen - 1 do
  begin
    if TRegExpOpCode(FCode[PC] and $FF) <> RX_SPLIT then Continue;
    X := Integer(FCode[PC] shr 8);
    if (X <= PC + 2) or (X > FCodeLen) then Continue;
    if (TRegExpOpCode(FCode[X - 1] and $FF) <> RX_JUMP) or (Integer(FCode[X - 1] shr 8) <> PC) then Continue;
    Body := -1;
    Ok := True;
    for I := PC + 1 to X - 2 do
    begin
      Op := TRegExpOpCode(FCode[I] and $FF);
      if Op in [RX_SAVE, RX_CLEAR_CAPTURES, RX_REPEAT_ENTER, RX_REPEAT_CHECK] then Continue;
      if (Op in [RX_CHAR, RX_CHAR_CLASS, RX_CHAR_CLASS_NEG, RX_ANY]) and (Body < 0) then begin Body := I; Continue; end;
      Ok := False;
      Break;
    end;
    if not Ok or (Body < 0) then Continue;
    FillChar(BodyBits, SizeOf(BodyBits), 0);
    if not FirstSetFrom(Body, BodyBits) then Continue;
    FillChar(Follow, SizeOf(Follow), 0);
    if not FirstSetFrom(X, Follow) then Continue;
    if BodyBits.HasHigh and Follow.HasHigh then Continue;
    Ok := True;
    for K := 0 to 7 do
      if (BodyBits.Bits[K] and Follow.Bits[K]) <> 0 then Ok := False;
    if Ok then FPossBody[PC] := Body;
  end;
end;

{ the first position at or after P where an attempt can succeed; FLen + 1 when there is none }
function TRegexMatcher.Candidate(P: Integer): Integer;
var U: Cardinal;
begin
  if FAnchor = 2 then
  begin
    while (P <= FLen) and (P > 0) and not IsLineTerminator(Ord(FIn[P - 1])) do Inc(P);
    Exit(P);
  end;
  if not FFirstOn then Exit(P);
  while P < FLen do
  begin
    U := Ord(FIn[P]);
    if U <= $FF then
    begin
      if (FFirst[U shr 5] and (UInt32(1) shl (U and 31))) <> 0 then Exit(P);
    end
    else if FFirstHigh then Exit(P);
    if FUnicode and IsHigh(U) and (P + 1 < FLen) and IsLow(Ord(FIn[P + 1])) then Inc(P, 2) else Inc(P);
  end;
  Result := FLen + 1;
end;

function TRegexMatcher.CaptureCount: Integer; begin Result := FProg.CaptureCount; end;
function TRegexMatcher.HasNamedGroups: Boolean; begin Result := Length(FProg.NamedGroups) > 0; end;
function TRegexMatcher.NamedGroups: TGocciaRegExpNamedGroups; begin Result := FProg.NamedGroups; end;

function TRegexMatcher.TryGetGroup(G: Integer; out St, En: Integer): Boolean;
begin
  if G = 0 then
  begin
    St := FMatchIndex;
    En := FMatchEnd;
  end
  else if FEmpty or (G * 2 + 1 >= FSlotCount) then
  begin
    St := -1;
    En := -1;
  end
  else
  begin
    St := FSlots[G * 2];
    En := FSlots[G * 2 + 1];
  end;
  Result := (St >= 0) and (En >= St) and (En <= FLen);
end;

{ Match attempts from StartPos at StartPC with Goccia's RunVM semantics. Scan: after a failed attempt, try
  the next candidate start position (top level only). Slots: SlotCount capture slots.
  One loop, no nested procedures: a failed instruction jumps to LFail, which records the failure in the memo
  and pops the next backtrack frame (LPop). The push sequence is written out at each site. }
function TRegexMatcher.Run(Depth: Integer; Slots: PInteger; StartPos, StartPC: Integer; EndPos: PInteger;
  Backward, Scan: Boolean): Boolean;
label LAttempt, LFail, LPop;
var
  C, NC: PDepthCtx;
  Code: PUInt32;
  CodeLen, Len, SlotCount, PC, InputPos, Bx, W, Top, Entries, RepeatDepth, Cap, RepCap, H, RD, J, K, Stride,
    ScanPos, RunStart, Iterations, BodyBx, RefStart, RefEnd, RefPos, ComparePos, RefW, InW, FirstIndex,
    FirstEnd, RangeEnd, SeqEnd, LookEnd: Integer;
  Inp: PWideChar;
  Uni, UseBits, NoMemo, Matched, Negated, BrICase, BrUni, RangeMatches, BeforeIsWord, AfterIsWord: Boolean;
  Instr: UInt32;
  Op, BodyOp: TRegExpOpCode;
  CP, RefCP, InputCP: Cardinal;
  StepCount, StepLimit: Int64;
  Stk, Rep: PInteger;
begin
  Result := False;
  if Depth = 0 then C := FCtx0 else C := Ctx(Depth);
  UseBits := FUseBits;
  Stride := FStride;
  NoMemo := FProg.HasBackreferences;
  Code := FCode;
  CodeLen := FCodeLen;
  Inp := FIn;
  Len := FLen;
  Uni := FUnicode;
  SlotCount := FSlotCount;
  StepLimit := FStepLimit;
  Stk := @C^.Stack[0];
  Cap := Length(C^.Stack);
  Rep := @C^.Rep[0];
  RepCap := Length(C^.Rep);
  if UseBits and (Length(C^.Bits) < ((CodeLen + 1) * Stride + 63) shr 6) then
  begin
    BitsReset(C);
    C^.Bits := nil;
    SetLength(C^.Bits, ((CodeLen + 1) * Stride + 63) shr 6 + 64);
    FillChar(C^.Bits[0], Length(C^.Bits) * SizeOf(UInt64), 0);
  end;
  ScanPos := StartPos;

LAttempt:
  if Scan then FillChar(Slots^, SlotCount * SizeOf(Integer), $FF);
  PC := StartPC;
  InputPos := ScanPos;
  Top := 0;
  Entries := 0;
  RepeatDepth := 0;
  StepCount := 0;
  if UseBits then BitsReset(C) else MemoReset(C);

  while True do
  begin
    if PC >= CodeLen then Exit(False);
    Inc(StepCount);
    if StepCount > StepLimit then begin FErr := 1; Exit(False); end;
    Instr := Code[PC];
    Op := TRegExpOpCode(Instr and $FF);
    Bx := Integer(Instr shr 8);
    case Op of
      RX_CHAR, RX_CHAR_CLASS, RX_CHAR_CLASS_NEG, RX_ANY:
        begin
          if Backward then
          begin
            if not ReadCPBefore(Inp, InputPos, Uni, CP, W) then goto LFail;
          end
          else if not ReadCP(Inp, Len, InputPos, Uni, CP, W) then goto LFail;
          case Op of
            RX_CHAR: Matched := CP = Cardinal(Bx);
            RX_CHAR_CLASS: Matched := InClass(FClassBits[Bx], FProg.CharClasses[Bx], CP);
            RX_CHAR_CLASS_NEG: Matched := not InClass(FClassBits[Bx], FProg.CharClasses[Bx], CP);
          else Matched := (Bx <> 0) or not IsLineTerminator(CP);
          end;
          if not Matched then goto LFail;
          if Backward then Dec(InputPos, W) else Inc(InputPos, W);
          Inc(PC);
        end;

      RX_SPLIT:
        begin
          if (FPossBody[PC] >= 0) and not Backward then
          begin
            { possessive: no frame; the next character decides between the body and the exit }
            K := FPossBody[PC];
            BodyOp := TRegExpOpCode(Code[K] and $FF);
            BodyBx := Integer(Code[K] shr 8);
            if FSimpleLoop[PC] then
            begin
              { the whole run at once }
              Iterations := 0;
              while ReadCP(Inp, Len, InputPos, Uni, CP, W) do
              begin
                case BodyOp of
                  RX_CHAR: Matched := CP = Cardinal(BodyBx);
                  RX_CHAR_CLASS: Matched := InClass(FClassBits[BodyBx], FProg.CharClasses[BodyBx], CP);
                  RX_CHAR_CLASS_NEG: Matched := not InClass(FClassBits[BodyBx], FProg.CharClasses[BodyBx], CP);
                else Matched := (BodyBx <> 0) or not IsLineTerminator(CP);
                end;
                if not Matched then Break;
                Inc(InputPos, W);
                Inc(Iterations);
              end;
              Inc(StepCount, 3 * Int64(Iterations));
              if StepCount > StepLimit then begin FErr := 1; Exit(False); end;
              PC := Bx;
              Continue;
            end;
            Matched := ReadCP(Inp, Len, InputPos, Uni, CP, W);
            if Matched then
              case BodyOp of
                RX_CHAR: Matched := CP = Cardinal(BodyBx);
                RX_CHAR_CLASS: Matched := InClass(FClassBits[BodyBx], FProg.CharClasses[BodyBx], CP);
                RX_CHAR_CLASS_NEG: Matched := not InClass(FClassBits[BodyBx], FProg.CharClasses[BodyBx], CP);
              else Matched := (BodyBx <> 0) or not IsLineTerminator(CP);
              end;
            if Matched then Inc(PC) else PC := Bx;
            Continue;
          end;
          if FSimpleLoop[PC] and not Backward and ((Len - InputPos) >= SIMPLE_GREEDY_LOOP_MIN_REMAINING) then
          begin
            { a loop whose body is one character instruction consumes its whole run in one step; one frame
              stands for every shorter count }
            BodyOp := TRegExpOpCode(Code[PC + 1] and $FF);
            BodyBx := Integer(Code[PC + 1] shr 8);
            RunStart := InputPos;
            Iterations := 0;
            while ReadCP(Inp, Len, InputPos, Uni, CP, W) do
            begin
              case BodyOp of
                RX_CHAR: Matched := CP = Cardinal(BodyBx);
                RX_CHAR_CLASS: Matched := InClass(FClassBits[BodyBx], FProg.CharClasses[BodyBx], CP);
                RX_CHAR_CLASS_NEG: Matched := not InClass(FClassBits[BodyBx], FProg.CharClasses[BodyBx], CP);
              else Matched := (BodyBx <> 0) or not IsLineTerminator(CP);
              end;
              if not Matched then Break;
              Inc(InputPos, W);
              Inc(Iterations);
            end;
            if not FSimpleTail[Bx] then
            begin
              Inc(StepCount, 3 * Int64(Iterations));
              if StepCount > StepLimit then begin FErr := 1; Exit(False); end;
              if InputPos > RunStart then
              begin
                if Entries >= BACKTRACK_CAP then begin FErr := 2; Exit(False); end;
                K := Top + SlotCount + RepeatDepth + 4;
                if K > Cap then begin GrowStack(C, K); Stk := @C^.Stack[0]; Cap := Length(C^.Stack); end;
                for J := 0 to SlotCount - 1 do Stk[Top + J] := Slots[J];
                Inc(Top, SlotCount);
                for J := 0 to RepeatDepth - 1 do Stk[Top + J] := Rep[J];
                Inc(Top, RepeatDepth);
                Stk[Top] := RepeatDepth;
                Stk[Top + 1] := RunStart;
                Stk[Top + 2] := InputPos - WidthBefore(Inp, InputPos, RunStart, Uni);
                Stk[Top + 3] := -Bx - 1;
                Inc(Top, 4);
                Inc(Entries);
              end;
            end;
            PC := Bx;
            if SeenAt(C, UseBits, Stride, PC, InputPos) then goto LPop;
            Continue;
          end;
          if not SeenAt(C, UseBits, Stride, Bx, InputPos) then
          begin
            if Entries >= BACKTRACK_CAP then begin FErr := 2; Exit(False); end;
            K := Top + SlotCount + RepeatDepth + 4;
            if K > Cap then begin GrowStack(C, K); Stk := @C^.Stack[0]; Cap := Length(C^.Stack); end;
            for J := 0 to SlotCount - 1 do Stk[Top + J] := Slots[J];
            Inc(Top, SlotCount);
            for J := 0 to RepeatDepth - 1 do Stk[Top + J] := Rep[J];
            Inc(Top, RepeatDepth);
            Stk[Top] := RepeatDepth;
            Stk[Top + 1] := 0;
            Stk[Top + 2] := InputPos;
            Stk[Top + 3] := Bx;
            Inc(Top, 4);
            Inc(Entries);
          end;
          Inc(PC);
        end;

      RX_SPLIT_LAZY:
        begin
          if not SeenAt(C, UseBits, Stride, PC + 1, InputPos) then
          begin
            if Entries >= BACKTRACK_CAP then begin FErr := 2; Exit(False); end;
            K := Top + SlotCount + RepeatDepth + 4;
            if K > Cap then begin GrowStack(C, K); Stk := @C^.Stack[0]; Cap := Length(C^.Stack); end;
            for J := 0 to SlotCount - 1 do Stk[Top + J] := Slots[J];
            Inc(Top, SlotCount);
            for J := 0 to RepeatDepth - 1 do Stk[Top + J] := Rep[J];
            Inc(Top, RepeatDepth);
            Stk[Top] := RepeatDepth;
            Stk[Top + 1] := 0;
            Stk[Top + 2] := InputPos;
            Stk[Top + 3] := PC + 1;
            Inc(Top, 4);
            Inc(Entries);
          end;
          PC := Bx;
        end;

      RX_JUMP:
        begin
          if (Bx >= 0) and (Bx < CodeLen) and (TRegExpOpCode(Code[Bx] and $FF) = RX_SPLIT) and (FPossBody[Bx] < 0) and (Top > 0) and
             (Stk[Top - 1] = Integer(Code[Bx] shr 8)) and (Stk[Top - 2] = InputPos) then
          begin
            { back at a loop's split with no progress: take its exit with the state the entry saved }
            PC := Integer(Code[Bx] shr 8);
            RD := Stk[Top - 4];
            for J := 0 to SlotCount - 1 do Slots[J] := Stk[Top - 4 - RD - SlotCount + J];
            RepeatDepth := RD;
            Top := Top - 4 - RD - SlotCount;
            Dec(Entries);
            Continue;
          end;
          PC := Bx;
        end;

      RX_SAVE:
        begin
          if Bx < SlotCount then Slots[Bx] := InputPos;
          Inc(PC);
        end;

      RX_CLEAR_CAPTURES:
        begin
          RefStart := Bx shr CLEAR_CAPTURE_COUNT_BITS;
          RefEnd := RefStart + (Bx and CLEAR_CAPTURE_COUNT_MASK) - 1;
          for RefPos := RefStart to RefEnd do
          begin
            if RefPos * 2 < SlotCount then Slots[RefPos * 2] := -1;
            if RefPos * 2 + 1 < SlotCount then Slots[RefPos * 2 + 1] := -1;
          end;
          Inc(PC);
        end;

      RX_REPEAT_ENTER:
        begin
          if RepeatDepth >= RepCap then begin GrowRep(C, RepeatDepth + 1); Rep := @C^.Rep[0]; RepCap := Length(C^.Rep); end;
          Rep[RepeatDepth] := InputPos;
          Inc(RepeatDepth);
          Inc(PC);
        end;

      RX_REPEAT_CHECK:
        begin
          if RepeatDepth <= 0 then begin FErr := 4; Exit(False); end;
          Dec(RepeatDepth);
          if Rep[RepeatDepth] = InputPos then goto LFail;
          Inc(PC);
        end;

      RX_CAPTURE_UNDEFINED_JUMP:
        begin
          if PC + 1 >= CodeLen then begin FErr := 4; Exit(False); end;
          RefStart := -1;
          RefEnd := -1;
          if Bx * 2 < SlotCount then RefStart := Slots[Bx * 2];
          if Bx * 2 + 1 < SlotCount then RefEnd := Slots[Bx * 2 + 1];
          if (RefStart < 0) or (RefEnd < 0) or (RefStart > RefEnd) then PC := Integer(Code[PC + 1] shr 8)
          else Inc(PC, 2);
        end;

      RX_STRING_SET:
        begin
          if Backward or (Bx < 0) or (Bx > High(FProg.StringSets)) then goto LFail;
          FirstIndex := -1;
          FirstEnd := -1;
          for K := 0 to High(FProg.StringSets[Bx].Strings) do
            if MatchSeq(Inp, Len, InputPos, Uni, FProg.StringSets[Bx].Strings[K], SeqEnd) then
            begin
              FirstIndex := K;
              FirstEnd := SeqEnd;
              Break;
            end;
          RangeMatches := False;
          RangeEnd := -1;
          K := FProg.StringSets[Bx].CharClassIndex;
          if ReadCP(Inp, Len, InputPos, Uni, CP, W) and (K >= 0) and (K <= High(FProg.CharClasses)) and
             InClass(FClassBits[K], FProg.CharClasses[K], CP) then
          begin
            RangeMatches := True;
            RangeEnd := InputPos + W;
          end;
          if FirstIndex < 0 then
          begin
            if not RangeMatches then goto LFail;
            InputPos := RangeEnd;
            Inc(PC);
            Continue;
          end;
          { the other ways to match, as frames, longest-preference order as Goccia's VM }
          for K := High(FProg.StringSets[Bx].Strings) + 1 downto FirstIndex + 1 do
          begin
            if K = High(FProg.StringSets[Bx].Strings) + 1 then
            begin
              if not RangeMatches then Continue;
              SeqEnd := RangeEnd;
            end
            else if not MatchSeq(Inp, Len, InputPos, Uni, FProg.StringSets[Bx].Strings[K], SeqEnd) then Continue;
            if SeenAt(C, UseBits, Stride, PC + 1, SeqEnd) then Continue;
            if Entries >= BACKTRACK_CAP then begin FErr := 2; Exit(False); end;
            H := Top + SlotCount + RepeatDepth + 4;
            if H > Cap then begin GrowStack(C, H); Stk := @C^.Stack[0]; Cap := Length(C^.Stack); end;
            for J := 0 to SlotCount - 1 do Stk[Top + J] := Slots[J];
            Inc(Top, SlotCount);
            for J := 0 to RepeatDepth - 1 do Stk[Top + J] := Rep[J];
            Inc(Top, RepeatDepth);
            Stk[Top] := RepeatDepth;
            Stk[Top + 1] := 0;
            Stk[Top + 2] := SeqEnd;
            Stk[Top + 3] := PC + 1;
            Inc(Top, 4);
            Inc(Entries);
          end;
          InputPos := FirstEnd;
          Inc(PC);
        end;

      RX_BACKREF:
        begin
          Negated := (Bx and BACKREF_STRICT_FLAG) <> 0;
          BrICase := (Bx and BACKREF_ICASE_FLAG) <> 0;
          BrUni := (Bx and BACKREF_UNICODE_FLAG) <> 0;
          K := Bx and BACKREF_INDEX_MASK;
          RefStart := -1;
          RefEnd := -1;
          if K * 2 < SlotCount then RefStart := Slots[K * 2];
          if K * 2 + 1 < SlotCount then RefEnd := Slots[K * 2 + 1];
          if (RefStart < 0) or (RefEnd < 0) or (RefStart > RefEnd) then
          begin
            if Negated then goto LFail;
            Inc(PC);
            Continue;
          end;
          ComparePos := InputPos;
          if Backward then
          begin
            ComparePos := InputPos - (RefEnd - RefStart);
            if ComparePos < 0 then goto LFail;
          end;
          RefPos := RefStart;
          Matched := True;
          while RefPos < RefEnd do
          begin
            if not ReadCP(Inp, Len, RefPos, BrUni, RefCP, RefW) or not ReadCP(Inp, Len, ComparePos, BrUni, InputCP, InW) then
            begin
              Matched := False;
              Break;
            end;
            if RefCP <> InputCP then
            begin
              if BrICase then
              begin
                RefCP := RegExpCanonicalizeCodePoint(RefCP, BrUni, True);
                InputCP := RegExpCanonicalizeCodePoint(InputCP, BrUni, True);
              end;
              if RefCP <> InputCP then
              begin
                Matched := False;
                Break;
              end;
            end;
            Inc(RefPos, RefW);
            Inc(ComparePos, InW);
          end;
          if not Matched then goto LFail;
          if Backward then InputPos := InputPos - (RefEnd - RefStart) else InputPos := ComparePos;
          Inc(PC);
        end;

      RX_ASSERT_START:
        begin
          if InputPos > 0 then
            if (Bx = 0) or not ReadCPBefore(Inp, InputPos, Uni, CP, W) or not IsLineTerminator(CP) then goto LFail;
          Inc(PC);
        end;

      RX_ASSERT_END:
        begin
          if Bx <> 0 then
          begin
            if ReadCP(Inp, Len, InputPos, Uni, CP, W) and not IsLineTerminator(CP) then goto LFail;
          end
          else if InputPos < Len then goto LFail;
          Inc(PC);
        end;

      RX_ASSERT_WORD:
        begin
          Negated := (Bx and WORD_ASSERT_NEGATED_FLAG) <> 0;
          BrICase := (Bx and WORD_ASSERT_ICASE_FLAG) <> 0;
          BeforeIsWord := ReadCPBefore(Inp, InputPos, Uni, CP, W) and IsWordChar(CP, Uni, BrICase);
          AfterIsWord := ReadCP(Inp, Len, InputPos, Uni, CP, W) and IsWordChar(CP, Uni, BrICase);
          if (Negated and (BeforeIsWord <> AfterIsWord)) or (not Negated and (BeforeIsWord = AfterIsWord)) then goto LFail;
          Inc(PC);
        end;

      RX_LOOKAHEAD, RX_LOOKBEHIND:
        begin
          Negated := (Bx and LOOK_NEGATED_FLAG) <> 0;
          LookEnd := Bx and LOOK_TARGET_MASK;
          if FSimpleLook[PC] then
          begin
            { (?=x), (?!\d), (?<=[a-z]): one character test, no captures, no recursion }
            BodyOp := TRegExpOpCode(Code[PC + 1] and $FF);
            BodyBx := Integer(Code[PC + 1] shr 8);
            if Op = RX_LOOKAHEAD then Matched := ReadCP(Inp, Len, InputPos, Uni, CP, W)
            else Matched := ReadCPBefore(Inp, InputPos, Uni, CP, W);
            if Matched then
              case BodyOp of
                RX_CHAR: Matched := CP = Cardinal(BodyBx);
                RX_CHAR_CLASS: Matched := InClass(FClassBits[BodyBx], FProg.CharClasses[BodyBx], CP);
                RX_CHAR_CLASS_NEG: Matched := not InClass(FClassBits[BodyBx], FProg.CharClasses[BodyBx], CP);
              else Matched := (BodyBx <> 0) or not IsLineTerminator(CP);
              end;
            if Negated = Matched then goto LFail;
            PC := LookEnd;
            Continue;
          end;
          NC := Ctx(Depth + 1);
          if Length(NC^.Slots) < SlotCount then SetLength(NC^.Slots, SlotCount);
          for J := 0 to SlotCount - 1 do NC^.Slots[J] := Slots[J];
          if (Depth >= NESTING_UNCHECKED_DEPTH) and NativeStackNearlyExhausted then begin FErr := 3; Exit(False); end;
          Matched := Run(Depth + 1, @NC^.Slots[0], InputPos, PC + 1, nil, Op = RX_LOOKBEHIND, False);
          if FErr <> 0 then Exit(False);
          if Negated = Matched then goto LFail;
          if not Negated then
            for J := 0 to SlotCount - 1 do Slots[J] := NC^.Slots[J];
          PC := LookEnd;
        end;

      RX_MATCH:
        begin
          if EndPos <> nil then EndPos^ := InputPos;
          Exit(True);
        end;

      RX_FAIL:
        goto LFail;
    else
      begin FErr := 4; Exit(False); end;
    end;
    Continue;

  LFail:
    if (Top > 0) and not NoMemo then
      if UseBits then BitsAdd(C, PC * Stride + InputPos) else MemoAdd(C, PC, InputPos);
  LPop:
    if Top = 0 then
    begin
      if not Scan then Exit(False);
      { the next start position }
      if ScanPos >= Len then Exit(False);
      if Uni and IsHigh(Ord(Inp[ScanPos])) and (ScanPos + 1 < Len) and IsLow(Ord(Inp[ScanPos + 1])) then Inc(ScanPos, 2)
      else Inc(ScanPos);
      ScanPos := Candidate(ScanPos);
      if ScanPos > Len then Exit(False);
      goto LAttempt;
    end;
    H := Top - 4;
    PC := Stk[H + 3];
    InputPos := Stk[H + 2];
    RD := Stk[H];
    RepeatDepth := RD;
    if RD > 0 then
    begin
      if RD > RepCap then begin GrowRep(C, RD); Rep := @C^.Rep[0]; RepCap := Length(C^.Rep); end;
      for J := 0 to RD - 1 do Rep[J] := Stk[H - RD + J];
    end;
    for J := 0 to SlotCount - 1 do Slots[J] := Stk[H - RD - SlotCount + J];
    if PC >= 0 then
    begin
      Top := H - RD - SlotCount;
      Dec(Entries);
    end
    else
    begin
      { a greedy run: the frame stays for the next shorter count, one code point back }
      PC := -PC - 1;
      if InputPos > Stk[H + 1] then Stk[H + 2] := InputPos - WidthBefore(Inp, InputPos, Stk[H + 1], Uni)
      else
      begin
        Top := H - RD - SlotCount;
        Dec(Entries);
      end;
    end;
    if SeenAt(C, UseBits, Stride, PC, InputPos) then goto LPop;
  end;
end;

function TRegexMatcher.Failed: Boolean;
begin
  Result := FErr <> 0;
end;

function TRegexMatcher.ErrorMessage: UnicodeString;
begin
  case FErr of
    1: Result := SStepLimitExceeded;
    2: Result := SBacktrackExceeded;
    3: Result := 'Maximum call stack size exceeded';
    4: Result := 'Invalid regular expression bytecode';
  else Result := '';
  end;
end;

function TRegexMatcher.Exec(Chars: PWideChar; Len, Start: Integer; RequireStart: Boolean): Boolean;
var P, I: Integer;
begin
  FErr := 0;
  FMatchIndex := -1;
  FMatchEnd := -1;
  FIn := Chars;
  FLen := Len;
  if Start > Len then Exit(False);
  if FEmpty then
  begin
    for I := 0 to FSlotCount - 1 do FSlots[I] := -1;
    FMatchIndex := Start;
    FMatchEnd := Start;
    Exit(True);
  end;
  FStride := Len + 1;
  FUseBits := Int64(FCodeLen + 1) * FStride <= 1 shl 24;   { at most 2 MB of bits per depth }
  FStepLimit := Int64(Len) * STEPS_PER_INPUT_UNIT;
  if FStepLimit < MIN_STEP_LIMIT then FStepLimit := MIN_STEP_LIMIT;
  P := Start;
  { a start inside a surrogate pair moves to the pair (Unicode mode) }
  if FUnicode and (P > 0) and (P < Len) and IsLow(Ord(Chars[P])) and IsHigh(Ord(Chars[P - 1])) then Dec(P);
  Result := False;
  if RequireStart or (FAnchor = 1) then
  begin
    { ^ (without m) holds only at the input start }
    if RequireStart or (P = 0) then
    begin
      FillChar(FSlots[0], FSlotCount * SizeOf(Integer), $FF);
      Result := Run(0, @FSlots[0], P, 0, nil, False, False);
    end;
  end
  else
  begin
    P := Candidate(P);
    if P <= Len then Result := Run(0, @FSlots[0], P, 0, nil, False, True);
  end;
  if FErr <> 0 then Result := False;
  if Result then
  begin
    FMatchIndex := FSlots[0];
    FMatchEnd := FSlots[1];
  end;
end;

end.
