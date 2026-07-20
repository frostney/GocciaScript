unit Goccia.FFI.CallbackSlots;

{$I Goccia.inc}

interface

type
  {$IF defined(GOCCIA_CPU_64)}
  {$IFDEF FPC}
    {$PUSH}{$PACKRECORDS 8}
  {$ELSE}
    {$ALIGN 8}
  {$ENDIF}
  TGocciaFFICallbackMachineState = record
    GPR: array[0..7] of UInt64;        //   0
    FPR: array[0..7] of UInt64;        //  64
    StackData: Pointer;               // 128
    HiddenResult: Pointer;            // 136
    RetGPR: array[0..1] of UInt64;     // 144
    RetFPR: array[0..3] of UInt64;     // 160
  end;
  {$IFDEF FPC}
    {$POP}
  {$ENDIF}
  {$ELSE}
  {$IFDEF FPC}
    {$PUSH}{$PACKRECORDS 4}
  {$ELSE}
    {$ALIGN 4}
  {$ENDIF}
  TGocciaFFICallbackMachineState = record
    StackData: Pointer;               //  0
    RetGPR: array[0..1] of LongWord;  //  4
    RetFPR: UInt64;                    // 12
    ReturnFloatSize: LongWord;        // 20
  end;
  {$IFDEF FPC}
    {$POP}
  {$ELSE}
    {$ALIGN 8}
  {$ENDIF}
  {$ENDIF}

  TGocciaFFICallbackDispatchHook = procedure(const AContext: Pointer;
    var AState: TGocciaFFICallbackMachineState);

  TGocciaFFIHiddenResultLocation = (
    fhrNone,
    fhrGPR0,
    fhrDedicated,
    fhrStack0
  );

const
  MAX_FFI_CALLBACK_SLOTS = 64;

procedure SetFFICallbackDispatchHook(
  const AHook: TGocciaFFICallbackDispatchHook);
function AllocateFFICallbackSlot(const AContext: Pointer;
  const AHiddenResultLocation: TGocciaFFIHiddenResultLocation;
  const AHiddenResultSize: NativeUInt;
  const AReturnFloatSize: LongWord;
  out ACodePointer: Pointer): Integer;
procedure ReleaseFFICallbackSlot(const ASlot: Integer);
function ConsumeFFICallbackThreadViolation(const ASlot: Integer): Boolean;
function ConsumeFFICallbackThreadViolationsForCurrentThread: Boolean;

implementation

uses
  SysUtils,

  CriticalSections;

type
  TGocciaFFICallbackSlot = record
    Active: Boolean;
    OwnerThreadId: TThreadID;
    Context: Pointer;
    ForeignThreadViolation: Boolean;
    HiddenResultLocation: TGocciaFFIHiddenResultLocation;
    HiddenResultSize: NativeUInt;
    ReturnFloatSize: LongWord;
  end;

var
  GCallbackSlots: array[0..MAX_FFI_CALLBACK_SLOTS - 1] of TGocciaFFICallbackSlot;
  GCallbackSlotLock: TGocciaCriticalSection;
  GDispatchHook: TGocciaFFICallbackDispatchHook;

{$IFDEF FPC}
procedure FFICallbackDispatchPascal(const ASlot: NativeUInt;
  const AState: Pointer); cdecl; public name 'goccia_ffi_callback_dispatch';
{$ELSE}
procedure FFICallbackDispatchPascal(const ASlot: NativeUInt;
  const AState: Pointer); cdecl;
{$ENDIF}
var
  Context: Pointer;
  ForeignThread: Boolean;
  HiddenResultLocation: TGocciaFFIHiddenResultLocation;
  HiddenResultSize: NativeUInt;
  {$IF not (defined(GOCCIA_CPU_64))}
  ReturnFloatSize: LongWord;
  {$ENDIF}
  Hook: TGocciaFFICallbackDispatchHook;
  ResultPointer: Pointer;
begin
  if ASlot >= MAX_FFI_CALLBACK_SLOTS then Exit;
  Context := nil;
  ForeignThread := False;
  HiddenResultLocation := fhrNone;
  HiddenResultSize := 0;
  {$IF not (defined(GOCCIA_CPU_64))}
  ReturnFloatSize := 0;
  {$ENDIF}
  Hook := nil;
  CriticalSectionEnter(GCallbackSlotLock);
  try
    if not GCallbackSlots[ASlot].Active then Exit;
    if GCallbackSlots[ASlot].OwnerThreadId <> GetGocciaThreadId then
    begin
      GCallbackSlots[ASlot].ForeignThreadViolation := True;
      ForeignThread := True;
      HiddenResultLocation :=
        GCallbackSlots[ASlot].HiddenResultLocation;
      HiddenResultSize := GCallbackSlots[ASlot].HiddenResultSize;
      {$IF not (defined(GOCCIA_CPU_64))}
      ReturnFloatSize := GCallbackSlots[ASlot].ReturnFloatSize;
      {$ENDIF}
    end;
    if not ForeignThread then
    begin
      Context := GCallbackSlots[ASlot].Context;
      Hook := GDispatchHook;
    end;
  finally
    CriticalSectionLeave(GCallbackSlotLock);
  end;
  if ForeignThread then
  begin
    {$IF not (defined(GOCCIA_CPU_64))}
    TGocciaFFICallbackMachineState(AState^).ReturnFloatSize :=
      ReturnFloatSize;
    {$ENDIF}
    ResultPointer := nil;
    case HiddenResultLocation of
      {$IF defined(GOCCIA_CPU_64)}
      fhrGPR0:
      begin
        ResultPointer := Pointer(NativeUInt(
          TGocciaFFICallbackMachineState(AState^).GPR[0]));
        TGocciaFFICallbackMachineState(AState^).RetGPR[0] :=
          UInt64(NativeUInt(ResultPointer));
      end;
      fhrDedicated:
        ResultPointer :=
          TGocciaFFICallbackMachineState(AState^).HiddenResult;
      {$ELSE}
      fhrStack0:
      begin
        if Assigned(TGocciaFFICallbackMachineState(AState^).StackData) then
          Move(TGocciaFFICallbackMachineState(AState^).StackData^,
            ResultPointer, SizeOf(Pointer));
        TGocciaFFICallbackMachineState(AState^).RetGPR[0] :=
          LongWord(NativeUInt(ResultPointer));
      end;
      {$ENDIF}
    end;
    if Assigned(ResultPointer) and (HiddenResultSize > 0) then
      FillChar(ResultPointer^, HiddenResultSize, 0);
    Exit;
  end;
  if Assigned(Context) and Assigned(Hook) then
    Hook(Context, TGocciaFFICallbackMachineState(AState^));
end;

{$IF (defined(GOCCIA_CPU_AARCH64))}
procedure FFICallbackCommon; assembler; nostackframe;
  public name 'goccia_ffi_callback_common';
asm
  sub sp, sp, #224
  stp x19, x30, [sp]
  str x16, [sp, #16]
  add x19, sp, #32

  stp x0, x1, [x19, #0]
  stp x2, x3, [x19, #16]
  stp x4, x5, [x19, #32]
  stp x6, x7, [x19, #48]
  str d0, [x19, #64]
  str d1, [x19, #72]
  str d2, [x19, #80]
  str d3, [x19, #88]
  str d4, [x19, #96]
  str d5, [x19, #104]
  str d6, [x19, #112]
  str d7, [x19, #120]
  add x9, sp, #224
  str x9, [x19, #128]
  str x8, [x19, #136]
  stp xzr, xzr, [x19, #144]
  stp xzr, xzr, [x19, #160]
  stp xzr, xzr, [x19, #176]

  ldr x0, [sp, #16]
  mov x1, x19
  bl FFICallbackDispatchPascal

  ldp x0, x1, [x19, #144]
  ldr d0, [x19, #160]
  ldr d1, [x19, #168]
  ldr d2, [x19, #176]
  ldr d3, [x19, #184]
  ldp x19, x30, [sp]
  add sp, sp, #224
  ret
end;

procedure FFICallbackSlot0; assembler; nostackframe; asm mov x16, #0; b FFICallbackCommon end;
procedure FFICallbackSlot1; assembler; nostackframe; asm mov x16, #1; b FFICallbackCommon end;
procedure FFICallbackSlot2; assembler; nostackframe; asm mov x16, #2; b FFICallbackCommon end;
procedure FFICallbackSlot3; assembler; nostackframe; asm mov x16, #3; b FFICallbackCommon end;
procedure FFICallbackSlot4; assembler; nostackframe; asm mov x16, #4; b FFICallbackCommon end;
procedure FFICallbackSlot5; assembler; nostackframe; asm mov x16, #5; b FFICallbackCommon end;
procedure FFICallbackSlot6; assembler; nostackframe; asm mov x16, #6; b FFICallbackCommon end;
procedure FFICallbackSlot7; assembler; nostackframe; asm mov x16, #7; b FFICallbackCommon end;
procedure FFICallbackSlot8; assembler; nostackframe; asm mov x16, #8; b FFICallbackCommon end;
procedure FFICallbackSlot9; assembler; nostackframe; asm mov x16, #9; b FFICallbackCommon end;
procedure FFICallbackSlot10; assembler; nostackframe; asm mov x16, #10; b FFICallbackCommon end;
procedure FFICallbackSlot11; assembler; nostackframe; asm mov x16, #11; b FFICallbackCommon end;
procedure FFICallbackSlot12; assembler; nostackframe; asm mov x16, #12; b FFICallbackCommon end;
procedure FFICallbackSlot13; assembler; nostackframe; asm mov x16, #13; b FFICallbackCommon end;
procedure FFICallbackSlot14; assembler; nostackframe; asm mov x16, #14; b FFICallbackCommon end;
procedure FFICallbackSlot15; assembler; nostackframe; asm mov x16, #15; b FFICallbackCommon end;
procedure FFICallbackSlot16; assembler; nostackframe; asm mov x16, #16; b FFICallbackCommon end;
procedure FFICallbackSlot17; assembler; nostackframe; asm mov x16, #17; b FFICallbackCommon end;
procedure FFICallbackSlot18; assembler; nostackframe; asm mov x16, #18; b FFICallbackCommon end;
procedure FFICallbackSlot19; assembler; nostackframe; asm mov x16, #19; b FFICallbackCommon end;
procedure FFICallbackSlot20; assembler; nostackframe; asm mov x16, #20; b FFICallbackCommon end;
procedure FFICallbackSlot21; assembler; nostackframe; asm mov x16, #21; b FFICallbackCommon end;
procedure FFICallbackSlot22; assembler; nostackframe; asm mov x16, #22; b FFICallbackCommon end;
procedure FFICallbackSlot23; assembler; nostackframe; asm mov x16, #23; b FFICallbackCommon end;
procedure FFICallbackSlot24; assembler; nostackframe; asm mov x16, #24; b FFICallbackCommon end;
procedure FFICallbackSlot25; assembler; nostackframe; asm mov x16, #25; b FFICallbackCommon end;
procedure FFICallbackSlot26; assembler; nostackframe; asm mov x16, #26; b FFICallbackCommon end;
procedure FFICallbackSlot27; assembler; nostackframe; asm mov x16, #27; b FFICallbackCommon end;
procedure FFICallbackSlot28; assembler; nostackframe; asm mov x16, #28; b FFICallbackCommon end;
procedure FFICallbackSlot29; assembler; nostackframe; asm mov x16, #29; b FFICallbackCommon end;
procedure FFICallbackSlot30; assembler; nostackframe; asm mov x16, #30; b FFICallbackCommon end;
procedure FFICallbackSlot31; assembler; nostackframe; asm mov x16, #31; b FFICallbackCommon end;
procedure FFICallbackSlot32; assembler; nostackframe; asm mov x16, #32; b FFICallbackCommon end;
procedure FFICallbackSlot33; assembler; nostackframe; asm mov x16, #33; b FFICallbackCommon end;
procedure FFICallbackSlot34; assembler; nostackframe; asm mov x16, #34; b FFICallbackCommon end;
procedure FFICallbackSlot35; assembler; nostackframe; asm mov x16, #35; b FFICallbackCommon end;
procedure FFICallbackSlot36; assembler; nostackframe; asm mov x16, #36; b FFICallbackCommon end;
procedure FFICallbackSlot37; assembler; nostackframe; asm mov x16, #37; b FFICallbackCommon end;
procedure FFICallbackSlot38; assembler; nostackframe; asm mov x16, #38; b FFICallbackCommon end;
procedure FFICallbackSlot39; assembler; nostackframe; asm mov x16, #39; b FFICallbackCommon end;
procedure FFICallbackSlot40; assembler; nostackframe; asm mov x16, #40; b FFICallbackCommon end;
procedure FFICallbackSlot41; assembler; nostackframe; asm mov x16, #41; b FFICallbackCommon end;
procedure FFICallbackSlot42; assembler; nostackframe; asm mov x16, #42; b FFICallbackCommon end;
procedure FFICallbackSlot43; assembler; nostackframe; asm mov x16, #43; b FFICallbackCommon end;
procedure FFICallbackSlot44; assembler; nostackframe; asm mov x16, #44; b FFICallbackCommon end;
procedure FFICallbackSlot45; assembler; nostackframe; asm mov x16, #45; b FFICallbackCommon end;
procedure FFICallbackSlot46; assembler; nostackframe; asm mov x16, #46; b FFICallbackCommon end;
procedure FFICallbackSlot47; assembler; nostackframe; asm mov x16, #47; b FFICallbackCommon end;
procedure FFICallbackSlot48; assembler; nostackframe; asm mov x16, #48; b FFICallbackCommon end;
procedure FFICallbackSlot49; assembler; nostackframe; asm mov x16, #49; b FFICallbackCommon end;
procedure FFICallbackSlot50; assembler; nostackframe; asm mov x16, #50; b FFICallbackCommon end;
procedure FFICallbackSlot51; assembler; nostackframe; asm mov x16, #51; b FFICallbackCommon end;
procedure FFICallbackSlot52; assembler; nostackframe; asm mov x16, #52; b FFICallbackCommon end;
procedure FFICallbackSlot53; assembler; nostackframe; asm mov x16, #53; b FFICallbackCommon end;
procedure FFICallbackSlot54; assembler; nostackframe; asm mov x16, #54; b FFICallbackCommon end;
procedure FFICallbackSlot55; assembler; nostackframe; asm mov x16, #55; b FFICallbackCommon end;
procedure FFICallbackSlot56; assembler; nostackframe; asm mov x16, #56; b FFICallbackCommon end;
procedure FFICallbackSlot57; assembler; nostackframe; asm mov x16, #57; b FFICallbackCommon end;
procedure FFICallbackSlot58; assembler; nostackframe; asm mov x16, #58; b FFICallbackCommon end;
procedure FFICallbackSlot59; assembler; nostackframe; asm mov x16, #59; b FFICallbackCommon end;
procedure FFICallbackSlot60; assembler; nostackframe; asm mov x16, #60; b FFICallbackCommon end;
procedure FFICallbackSlot61; assembler; nostackframe; asm mov x16, #61; b FFICallbackCommon end;
procedure FFICallbackSlot62; assembler; nostackframe; asm mov x16, #62; b FFICallbackCommon end;
procedure FFICallbackSlot63; assembler; nostackframe; asm mov x16, #63; b FFICallbackCommon end;
{$ENDIF}

{$IF (defined(GOCCIA_CPU_X86_64))}
{$IFDEF FPC}
{$ENDIF}
{$IFDEF MSWINDOWS}
// The Win64 caller owns the first 32 stack bytes as shadow space. Keep that
// area available to the Pascal dispatch call and point StackData past the
// callback's return address and original shadow space.
procedure FFICallbackCommon; assembler;
{$IFDEF FPC}
  nostackframe;
  public name 'goccia_ffi_callback_common';
{$ENDIF}
asm
  push rbx
  sub rsp, 224
  lea rbx, [rsp + 32]

  mov [rbx], rcx
  mov [rbx + 8], rdx
  mov [rbx + 16], r8
  mov [rbx + 24], r9
  mov qword ptr [rbx + 32], 0
  mov qword ptr [rbx + 40], 0
  mov qword ptr [rbx + 48], 0
  mov qword ptr [rbx + 56], 0
  movq [rbx + 64], xmm0
  movq [rbx + 72], xmm1
  movq [rbx + 80], xmm2
  movq [rbx + 88], xmm3
  mov qword ptr [rbx + 96], 0
  mov qword ptr [rbx + 104], 0
  mov qword ptr [rbx + 112], 0
  mov qword ptr [rbx + 120], 0
  lea rax, [rsp + 272]
  mov [rbx + 128], rax
  mov qword ptr [rbx + 136], 0
  mov qword ptr [rbx + 144], 0
  mov qword ptr [rbx + 152], 0
  mov qword ptr [rbx + 160], 0
  mov qword ptr [rbx + 168], 0
  mov qword ptr [rbx + 176], 0
  mov qword ptr [rbx + 184], 0

  mov rcx, r10
  mov rdx, rbx
  call FFICallbackDispatchPascal

  mov rax, [rbx + 144]
  mov rdx, [rbx + 152]
  movq xmm0, [rbx + 160]
  movq xmm1, [rbx + 168]
  movq xmm2, [rbx + 176]
  movq xmm3, [rbx + 184]
  add rsp, 224
  pop rbx
  ret
end;
{$ELSE}
// SysV enters with the return address at RSP. The 208-byte local frame keeps
// the dispatch call aligned while leaving the original stack arguments at
// State.StackData.
procedure FFICallbackCommon; assembler;
{$IFDEF FPC}
  nostackframe;
  public name 'goccia_ffi_callback_common';
{$ENDIF}
asm
  push rbx
  sub rsp, 208
  mov rbx, rsp

  mov [rbx], rdi
  mov [rbx + 8], rsi
  mov [rbx + 16], rdx
  mov [rbx + 24], rcx
  mov [rbx + 32], r8
  mov [rbx + 40], r9
  mov qword ptr [rbx + 48], 0
  mov qword ptr [rbx + 56], 0
  movq [rbx + 64], xmm0
  movq [rbx + 72], xmm1
  movq [rbx + 80], xmm2
  movq [rbx + 88], xmm3
  movq [rbx + 96], xmm4
  movq [rbx + 104], xmm5
  movq [rbx + 112], xmm6
  movq [rbx + 120], xmm7
  lea rax, [rsp + 224]
  mov [rbx + 128], rax
  mov qword ptr [rbx + 136], 0
  mov qword ptr [rbx + 144], 0
  mov qword ptr [rbx + 152], 0
  mov qword ptr [rbx + 160], 0
  mov qword ptr [rbx + 168], 0
  mov qword ptr [rbx + 176], 0
  mov qword ptr [rbx + 184], 0

  mov rdi, r10
  mov rsi, rbx
  call FFICallbackDispatchPascal

  mov rax, [rbx + 144]
  mov rdx, [rbx + 152]
  movq xmm0, [rbx + 160]
  movq xmm1, [rbx + 168]
  movq xmm2, [rbx + 176]
  movq xmm3, [rbx + 184]
  add rsp, 208
  pop rbx
  ret
end;
{$ENDIF}

procedure FFICallbackSlot0; assembler; {$IFDEF FPC}nostackframe;{$ENDIF} asm mov r10, 0; jmp FFICallbackCommon end;
procedure FFICallbackSlot1; assembler; {$IFDEF FPC}nostackframe;{$ENDIF} asm mov r10, 1; jmp FFICallbackCommon end;
procedure FFICallbackSlot2; assembler; {$IFDEF FPC}nostackframe;{$ENDIF} asm mov r10, 2; jmp FFICallbackCommon end;
procedure FFICallbackSlot3; assembler; {$IFDEF FPC}nostackframe;{$ENDIF} asm mov r10, 3; jmp FFICallbackCommon end;
procedure FFICallbackSlot4; assembler; {$IFDEF FPC}nostackframe;{$ENDIF} asm mov r10, 4; jmp FFICallbackCommon end;
procedure FFICallbackSlot5; assembler; {$IFDEF FPC}nostackframe;{$ENDIF} asm mov r10, 5; jmp FFICallbackCommon end;
procedure FFICallbackSlot6; assembler; {$IFDEF FPC}nostackframe;{$ENDIF} asm mov r10, 6; jmp FFICallbackCommon end;
procedure FFICallbackSlot7; assembler; {$IFDEF FPC}nostackframe;{$ENDIF} asm mov r10, 7; jmp FFICallbackCommon end;
procedure FFICallbackSlot8; assembler; {$IFDEF FPC}nostackframe;{$ENDIF} asm mov r10, 8; jmp FFICallbackCommon end;
procedure FFICallbackSlot9; assembler; {$IFDEF FPC}nostackframe;{$ENDIF} asm mov r10, 9; jmp FFICallbackCommon end;
procedure FFICallbackSlot10; assembler; {$IFDEF FPC}nostackframe;{$ENDIF} asm mov r10, 10; jmp FFICallbackCommon end;
procedure FFICallbackSlot11; assembler; {$IFDEF FPC}nostackframe;{$ENDIF} asm mov r10, 11; jmp FFICallbackCommon end;
procedure FFICallbackSlot12; assembler; {$IFDEF FPC}nostackframe;{$ENDIF} asm mov r10, 12; jmp FFICallbackCommon end;
procedure FFICallbackSlot13; assembler; {$IFDEF FPC}nostackframe;{$ENDIF} asm mov r10, 13; jmp FFICallbackCommon end;
procedure FFICallbackSlot14; assembler; {$IFDEF FPC}nostackframe;{$ENDIF} asm mov r10, 14; jmp FFICallbackCommon end;
procedure FFICallbackSlot15; assembler; {$IFDEF FPC}nostackframe;{$ENDIF} asm mov r10, 15; jmp FFICallbackCommon end;
procedure FFICallbackSlot16; assembler; {$IFDEF FPC}nostackframe;{$ENDIF} asm mov r10, 16; jmp FFICallbackCommon end;
procedure FFICallbackSlot17; assembler; {$IFDEF FPC}nostackframe;{$ENDIF} asm mov r10, 17; jmp FFICallbackCommon end;
procedure FFICallbackSlot18; assembler; {$IFDEF FPC}nostackframe;{$ENDIF} asm mov r10, 18; jmp FFICallbackCommon end;
procedure FFICallbackSlot19; assembler; {$IFDEF FPC}nostackframe;{$ENDIF} asm mov r10, 19; jmp FFICallbackCommon end;
procedure FFICallbackSlot20; assembler; {$IFDEF FPC}nostackframe;{$ENDIF} asm mov r10, 20; jmp FFICallbackCommon end;
procedure FFICallbackSlot21; assembler; {$IFDEF FPC}nostackframe;{$ENDIF} asm mov r10, 21; jmp FFICallbackCommon end;
procedure FFICallbackSlot22; assembler; {$IFDEF FPC}nostackframe;{$ENDIF} asm mov r10, 22; jmp FFICallbackCommon end;
procedure FFICallbackSlot23; assembler; {$IFDEF FPC}nostackframe;{$ENDIF} asm mov r10, 23; jmp FFICallbackCommon end;
procedure FFICallbackSlot24; assembler; {$IFDEF FPC}nostackframe;{$ENDIF} asm mov r10, 24; jmp FFICallbackCommon end;
procedure FFICallbackSlot25; assembler; {$IFDEF FPC}nostackframe;{$ENDIF} asm mov r10, 25; jmp FFICallbackCommon end;
procedure FFICallbackSlot26; assembler; {$IFDEF FPC}nostackframe;{$ENDIF} asm mov r10, 26; jmp FFICallbackCommon end;
procedure FFICallbackSlot27; assembler; {$IFDEF FPC}nostackframe;{$ENDIF} asm mov r10, 27; jmp FFICallbackCommon end;
procedure FFICallbackSlot28; assembler; {$IFDEF FPC}nostackframe;{$ENDIF} asm mov r10, 28; jmp FFICallbackCommon end;
procedure FFICallbackSlot29; assembler; {$IFDEF FPC}nostackframe;{$ENDIF} asm mov r10, 29; jmp FFICallbackCommon end;
procedure FFICallbackSlot30; assembler; {$IFDEF FPC}nostackframe;{$ENDIF} asm mov r10, 30; jmp FFICallbackCommon end;
procedure FFICallbackSlot31; assembler; {$IFDEF FPC}nostackframe;{$ENDIF} asm mov r10, 31; jmp FFICallbackCommon end;
procedure FFICallbackSlot32; assembler; {$IFDEF FPC}nostackframe;{$ENDIF} asm mov r10, 32; jmp FFICallbackCommon end;
procedure FFICallbackSlot33; assembler; {$IFDEF FPC}nostackframe;{$ENDIF} asm mov r10, 33; jmp FFICallbackCommon end;
procedure FFICallbackSlot34; assembler; {$IFDEF FPC}nostackframe;{$ENDIF} asm mov r10, 34; jmp FFICallbackCommon end;
procedure FFICallbackSlot35; assembler; {$IFDEF FPC}nostackframe;{$ENDIF} asm mov r10, 35; jmp FFICallbackCommon end;
procedure FFICallbackSlot36; assembler; {$IFDEF FPC}nostackframe;{$ENDIF} asm mov r10, 36; jmp FFICallbackCommon end;
procedure FFICallbackSlot37; assembler; {$IFDEF FPC}nostackframe;{$ENDIF} asm mov r10, 37; jmp FFICallbackCommon end;
procedure FFICallbackSlot38; assembler; {$IFDEF FPC}nostackframe;{$ENDIF} asm mov r10, 38; jmp FFICallbackCommon end;
procedure FFICallbackSlot39; assembler; {$IFDEF FPC}nostackframe;{$ENDIF} asm mov r10, 39; jmp FFICallbackCommon end;
procedure FFICallbackSlot40; assembler; {$IFDEF FPC}nostackframe;{$ENDIF} asm mov r10, 40; jmp FFICallbackCommon end;
procedure FFICallbackSlot41; assembler; {$IFDEF FPC}nostackframe;{$ENDIF} asm mov r10, 41; jmp FFICallbackCommon end;
procedure FFICallbackSlot42; assembler; {$IFDEF FPC}nostackframe;{$ENDIF} asm mov r10, 42; jmp FFICallbackCommon end;
procedure FFICallbackSlot43; assembler; {$IFDEF FPC}nostackframe;{$ENDIF} asm mov r10, 43; jmp FFICallbackCommon end;
procedure FFICallbackSlot44; assembler; {$IFDEF FPC}nostackframe;{$ENDIF} asm mov r10, 44; jmp FFICallbackCommon end;
procedure FFICallbackSlot45; assembler; {$IFDEF FPC}nostackframe;{$ENDIF} asm mov r10, 45; jmp FFICallbackCommon end;
procedure FFICallbackSlot46; assembler; {$IFDEF FPC}nostackframe;{$ENDIF} asm mov r10, 46; jmp FFICallbackCommon end;
procedure FFICallbackSlot47; assembler; {$IFDEF FPC}nostackframe;{$ENDIF} asm mov r10, 47; jmp FFICallbackCommon end;
procedure FFICallbackSlot48; assembler; {$IFDEF FPC}nostackframe;{$ENDIF} asm mov r10, 48; jmp FFICallbackCommon end;
procedure FFICallbackSlot49; assembler; {$IFDEF FPC}nostackframe;{$ENDIF} asm mov r10, 49; jmp FFICallbackCommon end;
procedure FFICallbackSlot50; assembler; {$IFDEF FPC}nostackframe;{$ENDIF} asm mov r10, 50; jmp FFICallbackCommon end;
procedure FFICallbackSlot51; assembler; {$IFDEF FPC}nostackframe;{$ENDIF} asm mov r10, 51; jmp FFICallbackCommon end;
procedure FFICallbackSlot52; assembler; {$IFDEF FPC}nostackframe;{$ENDIF} asm mov r10, 52; jmp FFICallbackCommon end;
procedure FFICallbackSlot53; assembler; {$IFDEF FPC}nostackframe;{$ENDIF} asm mov r10, 53; jmp FFICallbackCommon end;
procedure FFICallbackSlot54; assembler; {$IFDEF FPC}nostackframe;{$ENDIF} asm mov r10, 54; jmp FFICallbackCommon end;
procedure FFICallbackSlot55; assembler; {$IFDEF FPC}nostackframe;{$ENDIF} asm mov r10, 55; jmp FFICallbackCommon end;
procedure FFICallbackSlot56; assembler; {$IFDEF FPC}nostackframe;{$ENDIF} asm mov r10, 56; jmp FFICallbackCommon end;
procedure FFICallbackSlot57; assembler; {$IFDEF FPC}nostackframe;{$ENDIF} asm mov r10, 57; jmp FFICallbackCommon end;
procedure FFICallbackSlot58; assembler; {$IFDEF FPC}nostackframe;{$ENDIF} asm mov r10, 58; jmp FFICallbackCommon end;
procedure FFICallbackSlot59; assembler; {$IFDEF FPC}nostackframe;{$ENDIF} asm mov r10, 59; jmp FFICallbackCommon end;
procedure FFICallbackSlot60; assembler; {$IFDEF FPC}nostackframe;{$ENDIF} asm mov r10, 60; jmp FFICallbackCommon end;
procedure FFICallbackSlot61; assembler; {$IFDEF FPC}nostackframe;{$ENDIF} asm mov r10, 61; jmp FFICallbackCommon end;
procedure FFICallbackSlot62; assembler; {$IFDEF FPC}nostackframe;{$ENDIF} asm mov r10, 62; jmp FFICallbackCommon end;
procedure FFICallbackSlot63; assembler; {$IFDEF FPC}nostackframe;{$ENDIF} asm mov r10, 63; jmp FFICallbackCommon end;
{$ENDIF}

{$IF (defined(GOCCIA_CPU_X86))}
{$IFDEF FPC}
{$ENDIF}
// Preserve the original callback stack separately from the aligned Pascal
// dispatch stack: all i386 cdecl arguments, including a hidden result pointer,
// remain addressable through State.StackData.
procedure FFICallbackCommon; assembler;
{$IFDEF FPC}
  nostackframe;
  public name 'goccia_ffi_callback_common';
{$ENDIF}
asm
  push ebx
  push esi
  push edi
  mov edi, esp
  mov esi, ecx
  and esp, -16
  sub esp, 24
  mov ebx, esp

  lea eax, [edi + 16]
  mov [ebx], eax
  mov dword ptr [ebx + 4], 0
  mov dword ptr [ebx + 8], 0
  mov dword ptr [ebx + 12], 0
  mov dword ptr [ebx + 16], 0
  mov dword ptr [ebx + 20], 0

  push ebx
  push esi
  call FFICallbackDispatchPascal
  add esp, 8

  mov eax, [ebx + 4]
  mov edx, [ebx + 8]
  cmp dword ptr [ebx + 20], 4
  je @@return_single
  cmp dword ptr [ebx + 20], 8
  je @@return_double
  jmp @@return_ready
@@return_single:
  fld dword ptr [ebx + 12]
  jmp @@return_ready
@@return_double:
  fld qword ptr [ebx + 12]
@@return_ready:
  mov esp, edi
  pop edi
  pop esi
  pop ebx
  ret
end;

procedure FFICallbackSlot0; assembler; {$IFDEF FPC}nostackframe;{$ENDIF} asm mov ecx, 0; jmp FFICallbackCommon end;
procedure FFICallbackSlot1; assembler; {$IFDEF FPC}nostackframe;{$ENDIF} asm mov ecx, 1; jmp FFICallbackCommon end;
procedure FFICallbackSlot2; assembler; {$IFDEF FPC}nostackframe;{$ENDIF} asm mov ecx, 2; jmp FFICallbackCommon end;
procedure FFICallbackSlot3; assembler; {$IFDEF FPC}nostackframe;{$ENDIF} asm mov ecx, 3; jmp FFICallbackCommon end;
procedure FFICallbackSlot4; assembler; {$IFDEF FPC}nostackframe;{$ENDIF} asm mov ecx, 4; jmp FFICallbackCommon end;
procedure FFICallbackSlot5; assembler; {$IFDEF FPC}nostackframe;{$ENDIF} asm mov ecx, 5; jmp FFICallbackCommon end;
procedure FFICallbackSlot6; assembler; {$IFDEF FPC}nostackframe;{$ENDIF} asm mov ecx, 6; jmp FFICallbackCommon end;
procedure FFICallbackSlot7; assembler; {$IFDEF FPC}nostackframe;{$ENDIF} asm mov ecx, 7; jmp FFICallbackCommon end;
procedure FFICallbackSlot8; assembler; {$IFDEF FPC}nostackframe;{$ENDIF} asm mov ecx, 8; jmp FFICallbackCommon end;
procedure FFICallbackSlot9; assembler; {$IFDEF FPC}nostackframe;{$ENDIF} asm mov ecx, 9; jmp FFICallbackCommon end;
procedure FFICallbackSlot10; assembler; {$IFDEF FPC}nostackframe;{$ENDIF} asm mov ecx, 10; jmp FFICallbackCommon end;
procedure FFICallbackSlot11; assembler; {$IFDEF FPC}nostackframe;{$ENDIF} asm mov ecx, 11; jmp FFICallbackCommon end;
procedure FFICallbackSlot12; assembler; {$IFDEF FPC}nostackframe;{$ENDIF} asm mov ecx, 12; jmp FFICallbackCommon end;
procedure FFICallbackSlot13; assembler; {$IFDEF FPC}nostackframe;{$ENDIF} asm mov ecx, 13; jmp FFICallbackCommon end;
procedure FFICallbackSlot14; assembler; {$IFDEF FPC}nostackframe;{$ENDIF} asm mov ecx, 14; jmp FFICallbackCommon end;
procedure FFICallbackSlot15; assembler; {$IFDEF FPC}nostackframe;{$ENDIF} asm mov ecx, 15; jmp FFICallbackCommon end;
procedure FFICallbackSlot16; assembler; {$IFDEF FPC}nostackframe;{$ENDIF} asm mov ecx, 16; jmp FFICallbackCommon end;
procedure FFICallbackSlot17; assembler; {$IFDEF FPC}nostackframe;{$ENDIF} asm mov ecx, 17; jmp FFICallbackCommon end;
procedure FFICallbackSlot18; assembler; {$IFDEF FPC}nostackframe;{$ENDIF} asm mov ecx, 18; jmp FFICallbackCommon end;
procedure FFICallbackSlot19; assembler; {$IFDEF FPC}nostackframe;{$ENDIF} asm mov ecx, 19; jmp FFICallbackCommon end;
procedure FFICallbackSlot20; assembler; {$IFDEF FPC}nostackframe;{$ENDIF} asm mov ecx, 20; jmp FFICallbackCommon end;
procedure FFICallbackSlot21; assembler; {$IFDEF FPC}nostackframe;{$ENDIF} asm mov ecx, 21; jmp FFICallbackCommon end;
procedure FFICallbackSlot22; assembler; {$IFDEF FPC}nostackframe;{$ENDIF} asm mov ecx, 22; jmp FFICallbackCommon end;
procedure FFICallbackSlot23; assembler; {$IFDEF FPC}nostackframe;{$ENDIF} asm mov ecx, 23; jmp FFICallbackCommon end;
procedure FFICallbackSlot24; assembler; {$IFDEF FPC}nostackframe;{$ENDIF} asm mov ecx, 24; jmp FFICallbackCommon end;
procedure FFICallbackSlot25; assembler; {$IFDEF FPC}nostackframe;{$ENDIF} asm mov ecx, 25; jmp FFICallbackCommon end;
procedure FFICallbackSlot26; assembler; {$IFDEF FPC}nostackframe;{$ENDIF} asm mov ecx, 26; jmp FFICallbackCommon end;
procedure FFICallbackSlot27; assembler; {$IFDEF FPC}nostackframe;{$ENDIF} asm mov ecx, 27; jmp FFICallbackCommon end;
procedure FFICallbackSlot28; assembler; {$IFDEF FPC}nostackframe;{$ENDIF} asm mov ecx, 28; jmp FFICallbackCommon end;
procedure FFICallbackSlot29; assembler; {$IFDEF FPC}nostackframe;{$ENDIF} asm mov ecx, 29; jmp FFICallbackCommon end;
procedure FFICallbackSlot30; assembler; {$IFDEF FPC}nostackframe;{$ENDIF} asm mov ecx, 30; jmp FFICallbackCommon end;
procedure FFICallbackSlot31; assembler; {$IFDEF FPC}nostackframe;{$ENDIF} asm mov ecx, 31; jmp FFICallbackCommon end;
procedure FFICallbackSlot32; assembler; {$IFDEF FPC}nostackframe;{$ENDIF} asm mov ecx, 32; jmp FFICallbackCommon end;
procedure FFICallbackSlot33; assembler; {$IFDEF FPC}nostackframe;{$ENDIF} asm mov ecx, 33; jmp FFICallbackCommon end;
procedure FFICallbackSlot34; assembler; {$IFDEF FPC}nostackframe;{$ENDIF} asm mov ecx, 34; jmp FFICallbackCommon end;
procedure FFICallbackSlot35; assembler; {$IFDEF FPC}nostackframe;{$ENDIF} asm mov ecx, 35; jmp FFICallbackCommon end;
procedure FFICallbackSlot36; assembler; {$IFDEF FPC}nostackframe;{$ENDIF} asm mov ecx, 36; jmp FFICallbackCommon end;
procedure FFICallbackSlot37; assembler; {$IFDEF FPC}nostackframe;{$ENDIF} asm mov ecx, 37; jmp FFICallbackCommon end;
procedure FFICallbackSlot38; assembler; {$IFDEF FPC}nostackframe;{$ENDIF} asm mov ecx, 38; jmp FFICallbackCommon end;
procedure FFICallbackSlot39; assembler; {$IFDEF FPC}nostackframe;{$ENDIF} asm mov ecx, 39; jmp FFICallbackCommon end;
procedure FFICallbackSlot40; assembler; {$IFDEF FPC}nostackframe;{$ENDIF} asm mov ecx, 40; jmp FFICallbackCommon end;
procedure FFICallbackSlot41; assembler; {$IFDEF FPC}nostackframe;{$ENDIF} asm mov ecx, 41; jmp FFICallbackCommon end;
procedure FFICallbackSlot42; assembler; {$IFDEF FPC}nostackframe;{$ENDIF} asm mov ecx, 42; jmp FFICallbackCommon end;
procedure FFICallbackSlot43; assembler; {$IFDEF FPC}nostackframe;{$ENDIF} asm mov ecx, 43; jmp FFICallbackCommon end;
procedure FFICallbackSlot44; assembler; {$IFDEF FPC}nostackframe;{$ENDIF} asm mov ecx, 44; jmp FFICallbackCommon end;
procedure FFICallbackSlot45; assembler; {$IFDEF FPC}nostackframe;{$ENDIF} asm mov ecx, 45; jmp FFICallbackCommon end;
procedure FFICallbackSlot46; assembler; {$IFDEF FPC}nostackframe;{$ENDIF} asm mov ecx, 46; jmp FFICallbackCommon end;
procedure FFICallbackSlot47; assembler; {$IFDEF FPC}nostackframe;{$ENDIF} asm mov ecx, 47; jmp FFICallbackCommon end;
procedure FFICallbackSlot48; assembler; {$IFDEF FPC}nostackframe;{$ENDIF} asm mov ecx, 48; jmp FFICallbackCommon end;
procedure FFICallbackSlot49; assembler; {$IFDEF FPC}nostackframe;{$ENDIF} asm mov ecx, 49; jmp FFICallbackCommon end;
procedure FFICallbackSlot50; assembler; {$IFDEF FPC}nostackframe;{$ENDIF} asm mov ecx, 50; jmp FFICallbackCommon end;
procedure FFICallbackSlot51; assembler; {$IFDEF FPC}nostackframe;{$ENDIF} asm mov ecx, 51; jmp FFICallbackCommon end;
procedure FFICallbackSlot52; assembler; {$IFDEF FPC}nostackframe;{$ENDIF} asm mov ecx, 52; jmp FFICallbackCommon end;
procedure FFICallbackSlot53; assembler; {$IFDEF FPC}nostackframe;{$ENDIF} asm mov ecx, 53; jmp FFICallbackCommon end;
procedure FFICallbackSlot54; assembler; {$IFDEF FPC}nostackframe;{$ENDIF} asm mov ecx, 54; jmp FFICallbackCommon end;
procedure FFICallbackSlot55; assembler; {$IFDEF FPC}nostackframe;{$ENDIF} asm mov ecx, 55; jmp FFICallbackCommon end;
procedure FFICallbackSlot56; assembler; {$IFDEF FPC}nostackframe;{$ENDIF} asm mov ecx, 56; jmp FFICallbackCommon end;
procedure FFICallbackSlot57; assembler; {$IFDEF FPC}nostackframe;{$ENDIF} asm mov ecx, 57; jmp FFICallbackCommon end;
procedure FFICallbackSlot58; assembler; {$IFDEF FPC}nostackframe;{$ENDIF} asm mov ecx, 58; jmp FFICallbackCommon end;
procedure FFICallbackSlot59; assembler; {$IFDEF FPC}nostackframe;{$ENDIF} asm mov ecx, 59; jmp FFICallbackCommon end;
procedure FFICallbackSlot60; assembler; {$IFDEF FPC}nostackframe;{$ENDIF} asm mov ecx, 60; jmp FFICallbackCommon end;
procedure FFICallbackSlot61; assembler; {$IFDEF FPC}nostackframe;{$ENDIF} asm mov ecx, 61; jmp FFICallbackCommon end;
procedure FFICallbackSlot62; assembler; {$IFDEF FPC}nostackframe;{$ENDIF} asm mov ecx, 62; jmp FFICallbackCommon end;
procedure FFICallbackSlot63; assembler; {$IFDEF FPC}nostackframe;{$ENDIF} asm mov ecx, 63; jmp FFICallbackCommon end;
{$ENDIF}

function CodePointerForSlot(const ASlot: Integer): Pointer;
begin
  {$IF defined(GOCCIA_CPU_FFI_SUPPORTED)}
  case ASlot of
    0: Result := @FFICallbackSlot0;
    1: Result := @FFICallbackSlot1;
    2: Result := @FFICallbackSlot2;
    3: Result := @FFICallbackSlot3;
    4: Result := @FFICallbackSlot4;
    5: Result := @FFICallbackSlot5;
    6: Result := @FFICallbackSlot6;
    7: Result := @FFICallbackSlot7;
    8: Result := @FFICallbackSlot8;
    9: Result := @FFICallbackSlot9;
    10: Result := @FFICallbackSlot10;
    11: Result := @FFICallbackSlot11;
    12: Result := @FFICallbackSlot12;
    13: Result := @FFICallbackSlot13;
    14: Result := @FFICallbackSlot14;
    15: Result := @FFICallbackSlot15;
    16: Result := @FFICallbackSlot16;
    17: Result := @FFICallbackSlot17;
    18: Result := @FFICallbackSlot18;
    19: Result := @FFICallbackSlot19;
    20: Result := @FFICallbackSlot20;
    21: Result := @FFICallbackSlot21;
    22: Result := @FFICallbackSlot22;
    23: Result := @FFICallbackSlot23;
    24: Result := @FFICallbackSlot24;
    25: Result := @FFICallbackSlot25;
    26: Result := @FFICallbackSlot26;
    27: Result := @FFICallbackSlot27;
    28: Result := @FFICallbackSlot28;
    29: Result := @FFICallbackSlot29;
    30: Result := @FFICallbackSlot30;
    31: Result := @FFICallbackSlot31;
    32: Result := @FFICallbackSlot32;
    33: Result := @FFICallbackSlot33;
    34: Result := @FFICallbackSlot34;
    35: Result := @FFICallbackSlot35;
    36: Result := @FFICallbackSlot36;
    37: Result := @FFICallbackSlot37;
    38: Result := @FFICallbackSlot38;
    39: Result := @FFICallbackSlot39;
    40: Result := @FFICallbackSlot40;
    41: Result := @FFICallbackSlot41;
    42: Result := @FFICallbackSlot42;
    43: Result := @FFICallbackSlot43;
    44: Result := @FFICallbackSlot44;
    45: Result := @FFICallbackSlot45;
    46: Result := @FFICallbackSlot46;
    47: Result := @FFICallbackSlot47;
    48: Result := @FFICallbackSlot48;
    49: Result := @FFICallbackSlot49;
    50: Result := @FFICallbackSlot50;
    51: Result := @FFICallbackSlot51;
    52: Result := @FFICallbackSlot52;
    53: Result := @FFICallbackSlot53;
    54: Result := @FFICallbackSlot54;
    55: Result := @FFICallbackSlot55;
    56: Result := @FFICallbackSlot56;
    57: Result := @FFICallbackSlot57;
    58: Result := @FFICallbackSlot58;
    59: Result := @FFICallbackSlot59;
    60: Result := @FFICallbackSlot60;
    61: Result := @FFICallbackSlot61;
    62: Result := @FFICallbackSlot62;
    63: Result := @FFICallbackSlot63;
  else
    Result := nil;
  end;
  {$ENDIF}
end;

procedure ValidateCallbackSlotTable;
var
  I, J: Integer;
  Code: Pointer;
begin
  {$IF defined(GOCCIA_CPU_FFI_SUPPORTED)}
  if Assigned(CodePointerForSlot(MAX_FFI_CALLBACK_SLOTS)) then
    raise EInvalidOpException.Create('FFI callback slot table exceeds limit');
  for I := 0 to MAX_FFI_CALLBACK_SLOTS - 1 do
  begin
    Code := CodePointerForSlot(I);
    if not Assigned(Code) then
      raise EInvalidOpException.Create('FFI callback slot table is incomplete');
    for J := 0 to I - 1 do
      if Code = CodePointerForSlot(J) then
        raise EInvalidOpException.Create(
          'FFI callback slot table contains duplicate entries');
  end;
  {$ENDIF}
end;

procedure SetFFICallbackDispatchHook(
  const AHook: TGocciaFFICallbackDispatchHook);
begin
  CriticalSectionEnter(GCallbackSlotLock);
  try
    GDispatchHook := AHook;
  finally
    CriticalSectionLeave(GCallbackSlotLock);
  end;
end;

function HiddenResultLocationSupported(
  const ALocation: TGocciaFFIHiddenResultLocation): Boolean;
begin
  {$IF (defined(GOCCIA_CPU_AARCH64))}
  Result := ALocation in [fhrNone, fhrDedicated];
  {$ELSE}
  {$IF (defined(GOCCIA_CPU_X86_64))}
  Result := ALocation in [fhrNone, fhrGPR0];
  {$ELSE}
  {$IF (defined(GOCCIA_CPU_X86))}
  Result := ALocation in [fhrNone, fhrStack0];
  {$ELSE}
  Result := ALocation = fhrNone;
  {$ENDIF}
  {$ENDIF}
  {$ENDIF}
end;

function AllocateFFICallbackSlot(const AContext: Pointer;
  const AHiddenResultLocation: TGocciaFFIHiddenResultLocation;
  const AHiddenResultSize: NativeUInt;
  const AReturnFloatSize: LongWord;
  out ACodePointer: Pointer): Integer;
begin
  ACodePointer := nil;
  if not HiddenResultLocationSupported(AHiddenResultLocation) then
    raise EArgumentException.Create(
      'FFI callback hidden result does not match this target');
  if ((AHiddenResultLocation = fhrNone) and
      (AHiddenResultSize <> 0)) or
     ((AHiddenResultLocation <> fhrNone) and
      (AHiddenResultSize = 0)) then
    raise EArgumentException.Create(
      'FFI callback hidden result metadata is inconsistent');
  if not (AReturnFloatSize in [0, 4, 8]) then
    raise EArgumentException.Create(
      'FFI callback float return size must be 0, 4, or 8');
  {$IF defined(GOCCIA_CPU_64)}
  if AReturnFloatSize <> 0 then
    raise EArgumentException.Create(
      'FFI callback float return metadata is only valid on i386');
  {$ENDIF}
  CriticalSectionEnter(GCallbackSlotLock);
  try
    for Result := 0 to MAX_FFI_CALLBACK_SLOTS - 1 do
      if not GCallbackSlots[Result].Active then
      begin
        GCallbackSlots[Result].Active := True;
        GCallbackSlots[Result].OwnerThreadId := GetGocciaThreadId;
        GCallbackSlots[Result].Context := AContext;
        GCallbackSlots[Result].ForeignThreadViolation := False;
        GCallbackSlots[Result].HiddenResultLocation :=
          AHiddenResultLocation;
        GCallbackSlots[Result].HiddenResultSize := AHiddenResultSize;
        GCallbackSlots[Result].ReturnFloatSize := AReturnFloatSize;
        ACodePointer := CodePointerForSlot(Result);
        if not Assigned(ACodePointer) then
        begin
          GCallbackSlots[Result].Active := False;
          raise EInvalidOpException.Create(
            'FFI callbacks are not implemented for this target');
        end;
        Exit;
      end;
  finally
    CriticalSectionLeave(GCallbackSlotLock);
  end;
  raise EOutOfMemory.Create('No FFI callback slots are available');
end;

procedure ReleaseFFICallbackSlot(const ASlot: Integer);
begin
  if (ASlot < 0) or (ASlot >= MAX_FFI_CALLBACK_SLOTS) then Exit;
  CriticalSectionEnter(GCallbackSlotLock);
  try
    GCallbackSlots[ASlot].Active := False;
    GCallbackSlots[ASlot].Context := nil;
    GCallbackSlots[ASlot].HiddenResultLocation := fhrNone;
    GCallbackSlots[ASlot].HiddenResultSize := 0;
    GCallbackSlots[ASlot].ReturnFloatSize := 0;
  finally
    CriticalSectionLeave(GCallbackSlotLock);
  end;
end;

function ConsumeFFICallbackThreadViolation(const ASlot: Integer): Boolean;
begin
  Result := False;
  if (ASlot < 0) or (ASlot >= MAX_FFI_CALLBACK_SLOTS) then Exit;
  CriticalSectionEnter(GCallbackSlotLock);
  try
    Result := GCallbackSlots[ASlot].ForeignThreadViolation;
    GCallbackSlots[ASlot].ForeignThreadViolation := False;
  finally
    CriticalSectionLeave(GCallbackSlotLock);
  end;
end;

function ConsumeFFICallbackThreadViolationsForCurrentThread: Boolean;
var
  I: Integer;
  OwnerThreadId: TThreadID;
begin
  Result := False;
  OwnerThreadId := GetGocciaThreadId;
  CriticalSectionEnter(GCallbackSlotLock);
  try
    for I := 0 to MAX_FFI_CALLBACK_SLOTS - 1 do
      if GCallbackSlots[I].Active and
         (GCallbackSlots[I].OwnerThreadId = OwnerThreadId) and
         GCallbackSlots[I].ForeignThreadViolation then
      begin
        GCallbackSlots[I].ForeignThreadViolation := False;
        Result := True;
      end;
  finally
    CriticalSectionLeave(GCallbackSlotLock);
  end;
end;

procedure ValidateCallbackStateLayout;
var
  State: TGocciaFFICallbackMachineState;
  Base: NativeUInt;
begin
  Base := NativeUInt(@State);
  {$IF defined(GOCCIA_CPU_64)}
  if (NativeUInt(@State.FPR[0]) - Base <> 64) or
     (NativeUInt(@State.StackData) - Base <> 128) or
     (NativeUInt(@State.HiddenResult) - Base <> 136) or
     (NativeUInt(@State.RetGPR[0]) - Base <> 144) or
     (NativeUInt(@State.RetFPR[0]) - Base <> 160) then
    raise EInvalidOpException.Create('FFI callback-state layout mismatch');
  {$ELSE}
  if (NativeUInt(@State.RetGPR[0]) - Base <> 4) or
     (NativeUInt(@State.RetFPR) - Base <> 12) or
     (NativeUInt(@State.ReturnFloatSize) - Base <> 20) then
    raise EInvalidOpException.Create('FFI callback-state layout mismatch');
  {$ENDIF}
end;

initialization
  CriticalSectionInit(GCallbackSlotLock);
  ValidateCallbackStateLayout;
  ValidateCallbackSlotTable;

finalization
  CriticalSectionDone(GCallbackSlotLock);

end.
