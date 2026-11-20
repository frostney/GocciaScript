unit G2.Profile;
{ GocciaScript's runtime profile (--profile, docs/profiling.md) on G2: the thread's TGocciaProfiler (Goccia.Profiler)
  fed from G2's interpreter. Under profiling every instruction runs through Execute's counted loop (no fast loop, no
  compiled code), so the step hook sees each one: its opcode (and the pair it makes with the one before), and for a
  binary arithmetic or comparison instruction whether both operands are Numbers (the scalar fast path's hit or miss).

  Functions are timed at their frames. A frame's push reaches the push hook (a call, or a generator or async function
  resumed, which counts as a call too; a module resumed after an await continues its one evaluation); a frame ends
  when a later push reuses its slot or an instruction runs in a frame below it, so a throw that unwinds several
  frames needs no hook of its own. The host's code (the preludes, whose source names are <goccia:...>-style) is not
  profiled: its time and allocations are its caller's. Allocations are the objects made (G2.Core ObjectsMade),
  charged to the innermost profiled function at each push and end. }
{$mode delphiunicode}{$H+}
{$I G2.Checks.inc}
{$POINTERMATH ON}

interface

{ profiling for this thread's runs: Opcodes (opcode and pair counts, the scalar fast path) and Functions (calls,
  times, allocations), into the thread's TGocciaProfiler (which the host creates or adopts) }
procedure ProfileStart(Opcodes, Functions: Boolean);
{ the run ended: every function still open ends, and the hooks stop }
procedure ProfileStop;
{ the name of a G2 opcode, for the profiler's reports }
function G2OpcodeName(Op: Byte): string;

implementation

uses
  SysUtils, TypInfo, Goccia.Profiler, G2.Core, G2.Bytecode, G2.VM;

type
  TShadow = record
    Index: Integer;    { the frame's slot }
    Prof: Integer;     { its function profile, -1 for none (the host's code, a generator's creation) }
  end;

threadvar
  Prof: TGocciaProfiler;
  DoOps, DoFns: Boolean;
  Shadow: array of TShadow;
  ShadowTop: Integer;
  LastObjects: Int64;

function G2OpcodeName(Op: Byte): string;
begin
  if Op <= Ord(High(TOp)) then
  begin
    Result := GetEnumName(TypeInfo(TOp), Op);
    if Copy(Result, 1, 2) = 'op' then Delete(Result, 1, 2);
  end
  else
    Result := 'Op' + IntToStr(Op);
end;

{ the host's code: a prelude's (a source named like <goccia:console>) }
function HostSource(const Name: UnicodeString): Boolean;
begin
  Result := (Length(Name) > 1) and (Name[1] = '<') and (Pos(':', Name) > 0) and (Pos('>', Name) = Length(Name));
end;

function ProfileName(T: PFuncTemplate): UnicodeString;
begin
  if T^.TopLevel then Exit('<module>');
  if T^.Name <> nil then Result := StrToUnicode(T^.Name) else Result := '';
  if Result = '' then Result := '<anonymous>';
end;

{ T's function profile (registered on its first frame), -1 for the host's code }
function ProfileOf(T: PFuncTemplate): Integer;
var Line: Integer;
begin
  if T^.ProfIdx = 0 then
  begin
    if HostSource(T^.SourceName) then T^.ProfIdx := -1
    else
    begin
      Line := T^.DeclLine;
      if T^.TopLevel or (Line < 0) then Line := 0;
      T^.ProfIdx := Prof.RegisterTemplate(ProfileName(T), T^.SourceName, Line) + 1;
    end;
  end;
  if T^.ProfIdx < 0 then Result := -1 else Result := T^.ProfIdx - 1;
end;

{ the objects made since the last charge, to the innermost profiled function }
procedure ChargeObjects;
var N: Int64;
begin
  N := ObjectsMade - LastObjects;
  LastObjects := ObjectsMade;
  if N > 0 then Prof.RecordAllocations(N);
end;

{ the frames from slot Index up have ended }
procedure EndFrom(Index: Integer);
begin
  if (ShadowTop = 0) or (Shadow[ShadowTop - 1].Index < Index) then Exit;
  ChargeObjects;
  while (ShadowTop > 0) and (Shadow[ShadowTop - 1].Index >= Index) do
  begin
    Dec(ShadowTop);
    if Shadow[ShadowTop].Prof >= 0 then Prof.PopFunction(Shadow[ShadowTop].Prof);
  end;
end;

function StartsSuspended(T: PFuncTemplate): Boolean;
var I: Integer;
begin
  if T^.ProfSuspends = 0 then
  begin
    T^.ProfSuspends := 2;
    for I := 0 to T^.CodeLen - 1 do
      if TOp(T^.Code[I] and $FF) = opGenStart then
      begin
        T^.ProfSuspends := 1;
        Break;
      end;
  end;
  Result := T^.ProfSuspends = 1;
end;

procedure OnPush(Index: Integer; Resume: Boolean);
var T: PFuncTemplate; P: Integer; CountCall: Boolean;
begin
  if not DoFns then Exit;
  EndFrom(Index);
  ChargeObjects;
  T := Frames[Index].Tpl;
  P := -1;
  CountCall := True;
  if T^.TopLevel then
  begin
    { a module's evaluation is one call, however often an await suspends and resumes it }
    P := ProfileOf(T);
    CountCall := not Resume;
  end
  else if Resume then
  begin
    { a resumption counts as a call, but an async function's start: its call resumes it at its entry at once }
    P := ProfileOf(T);
    CountCall := Frames[Index].PC <> T^.EntryPC;
  end
  { a generator's creation runs only to its start (opGenStart): its first resumption is its first call }
  else if not StartsSuspended(T) then
    P := ProfileOf(T);
  if ShadowTop >= Length(Shadow) then SetLength(Shadow, ShadowTop * 2 + 64);
  Shadow[ShadowTop].Index := Index;
  Shadow[ShadowTop].Prof := P;
  Inc(ShadowTop);
  if P >= 0 then Prof.PushFunction(P, CountCall);
end;

procedure OnStep(FrP: Pointer; PC: Integer);
var Fr: PFrame; T: PFuncTemplate; I: QWord; Op: Byte; VA, VB: TValue;
begin
  Fr := PFrame(FrP);
  if DoFns then EndFrom(FrameTop);
  if not DoOps then Exit;
  T := Fr^.Tpl;
  if (T^.ProfIdx < 0) or ((T^.ProfIdx = 0) and HostSource(T^.SourceName)) then Exit;
  I := T^.Code[PC];
  Op := I and $FF;
  Prof.RecordOpcode(Op);
  case TOp(Op) of
    opAdd, opSub, opMul, opDiv, opMod, opPow, opBitAnd, opBitOr, opBitXor, opShl, opShr, opUShr,
    opEq, opNe, opSEq, opSNe, opLt, opLe, opGt, opGe:
      begin
        VA := VStack[Fr^.Base + Integer((I shr 24) and $FFFF)];
        VB := VStack[Fr^.Base + Integer(I shr 40)];
        if IsNumber(VA) and IsNumber(VB) then Prof.RecordScalarHit else Prof.RecordScalarMiss;
      end;
  end;
end;

procedure ProfileStart(Opcodes, Functions: Boolean);
begin
  Prof := TGocciaProfiler.Instance;
  if Prof = nil then Exit;
  DoOps := Opcodes;
  DoFns := Functions;
  ShadowTop := 0;
  LastObjects := ObjectsMade;
  if Functions then CountObjects := True;
  ProfilingAny := True;
  ProfileOn := True;
end;

procedure ProfileStop;
begin
  if not ProfileOn then Exit;
  if DoFns and (Prof <> nil) then EndFrom(0);
  ProfileOn := False;
  DoOps := False;
  DoFns := False;
  Prof := nil;
end;

initialization
  ProfPushHook := OnPush;
  ProfStepHook := OnStep;
  ProfilerOpcodeName := G2OpcodeName;
end.
