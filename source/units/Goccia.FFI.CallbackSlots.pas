unit Goccia.FFI.CallbackSlots;

{$I Goccia.inc}

interface

type
  {$IFDEF CPU64}
  {$PUSH}{$PACKRECORDS 8}
  TGocciaFFICallbackMachineState = record
    GPR: array[0..7] of QWord;        //   0
    FPR: array[0..7] of QWord;        //  64
    StackData: Pointer;               // 128
    HiddenResult: Pointer;            // 136
    RetGPR: array[0..1] of QWord;     // 144
    RetFPR: array[0..3] of QWord;     // 160
  end;
  {$POP}
  {$ELSE}
  {$PUSH}{$PACKRECORDS 4}
  TGocciaFFICallbackMachineState = record
    StackData: Pointer;               //  0
    RetGPR: array[0..1] of LongWord;  //  4
    RetFPR: QWord;                    // 12
    ReturnFloatSize: LongWord;        // 20
  end;
  {$POP}
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
  const AHiddenResultSize: PtrUInt;
  const AReturnFloatSize: LongWord;
  out ACodePointer: CodePointer): Integer;
procedure ReleaseFFICallbackSlot(const ASlot: Integer);
function ConsumeFFICallbackThreadViolation(const ASlot: Integer): Boolean;
function ConsumeFFICallbackThreadViolationsForCurrentThread: Boolean;

implementation

uses
  SysUtils;

type
  TGocciaFFICallbackSlot = record
    Active: Boolean;
    OwnerThreadId: TThreadID;
    Context: Pointer;
    ForeignThreadViolation: Boolean;
    HiddenResultLocation: TGocciaFFIHiddenResultLocation;
    HiddenResultSize: PtrUInt;
    ReturnFloatSize: LongWord;
  end;

var
  GCallbackSlots: array[0..MAX_FFI_CALLBACK_SLOTS - 1] of TGocciaFFICallbackSlot;
  GCallbackSlotLock: TRTLCriticalSection;
  GDispatchHook: TGocciaFFICallbackDispatchHook;

procedure FFICallbackDispatchPascal(const ASlot: PtrUInt;
  const AState: Pointer); cdecl; public name 'goccia_ffi_callback_dispatch';
var
  Context: Pointer;
  ForeignThread: Boolean;
  HiddenResultLocation: TGocciaFFIHiddenResultLocation;
  HiddenResultSize: PtrUInt;
  {$IFNDEF CPU64}
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
  {$IFNDEF CPU64}
  ReturnFloatSize := 0;
  {$ENDIF}
  Hook := nil;
  EnterCriticalSection(GCallbackSlotLock);
  try
    if not GCallbackSlots[ASlot].Active then Exit;
    if GCallbackSlots[ASlot].OwnerThreadId <> GetCurrentThreadId then
    begin
      GCallbackSlots[ASlot].ForeignThreadViolation := True;
      ForeignThread := True;
      HiddenResultLocation :=
        GCallbackSlots[ASlot].HiddenResultLocation;
      HiddenResultSize := GCallbackSlots[ASlot].HiddenResultSize;
      {$IFNDEF CPU64}
      ReturnFloatSize := GCallbackSlots[ASlot].ReturnFloatSize;
      {$ENDIF}
    end;
    if not ForeignThread then
    begin
      Context := GCallbackSlots[ASlot].Context;
      Hook := GDispatchHook;
    end;
  finally
    LeaveCriticalSection(GCallbackSlotLock);
  end;
  if ForeignThread then
  begin
    {$IFNDEF CPU64}
    TGocciaFFICallbackMachineState(AState^).ReturnFloatSize :=
      ReturnFloatSize;
    {$ENDIF}
    ResultPointer := nil;
    case HiddenResultLocation of
      {$IFDEF CPU64}
      fhrGPR0:
      begin
        ResultPointer := Pointer(PtrUInt(
          TGocciaFFICallbackMachineState(AState^).GPR[0]));
        TGocciaFFICallbackMachineState(AState^).RetGPR[0] :=
          QWord(PtrUInt(ResultPointer));
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
          LongWord(PtrUInt(ResultPointer));
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

{$IFDEF CPUAARCH64}
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

{$IFDEF CPUX86_64}
{$ASMMODE ATT}
{$IFDEF MSWINDOWS}
// The Win64 caller owns the first 32 stack bytes as shadow space. Keep that
// area available to the Pascal dispatch call and point StackData past the
// callback's return address and original shadow space.
procedure FFICallbackCommon; assembler; nostackframe;
  public name 'goccia_ffi_callback_common';
asm
  pushq %rbx
  subq $224, %rsp
  leaq 32(%rsp), %rbx

  movq %rcx, 0(%rbx)
  movq %rdx, 8(%rbx)
  movq %r8, 16(%rbx)
  movq %r9, 24(%rbx)
  movq $0, 32(%rbx)
  movq $0, 40(%rbx)
  movq $0, 48(%rbx)
  movq $0, 56(%rbx)
  movq %xmm0, 64(%rbx)
  movq %xmm1, 72(%rbx)
  movq %xmm2, 80(%rbx)
  movq %xmm3, 88(%rbx)
  movq $0, 96(%rbx)
  movq $0, 104(%rbx)
  movq $0, 112(%rbx)
  movq $0, 120(%rbx)
  leaq 272(%rsp), %rax
  movq %rax, 128(%rbx)
  movq $0, 136(%rbx)
  movq $0, 144(%rbx)
  movq $0, 152(%rbx)
  movq $0, 160(%rbx)
  movq $0, 168(%rbx)
  movq $0, 176(%rbx)
  movq $0, 184(%rbx)

  movq %r10, %rcx
  movq %rbx, %rdx
  call FFICallbackDispatchPascal

  movq 144(%rbx), %rax
  movq 152(%rbx), %rdx
  movq 160(%rbx), %xmm0
  movq 168(%rbx), %xmm1
  movq 176(%rbx), %xmm2
  movq 184(%rbx), %xmm3
  addq $224, %rsp
  popq %rbx
  ret
end;
{$ELSE}
// SysV enters with the return address at RSP. The 208-byte local frame keeps
// the dispatch call aligned while leaving the original stack arguments at
// State.StackData.
procedure FFICallbackCommon; assembler; nostackframe;
  public name 'goccia_ffi_callback_common';
asm
  pushq %rbx
  subq $208, %rsp
  movq %rsp, %rbx

  movq %rdi, 0(%rbx)
  movq %rsi, 8(%rbx)
  movq %rdx, 16(%rbx)
  movq %rcx, 24(%rbx)
  movq %r8, 32(%rbx)
  movq %r9, 40(%rbx)
  movq $0, 48(%rbx)
  movq $0, 56(%rbx)
  movq %xmm0, 64(%rbx)
  movq %xmm1, 72(%rbx)
  movq %xmm2, 80(%rbx)
  movq %xmm3, 88(%rbx)
  movq %xmm4, 96(%rbx)
  movq %xmm5, 104(%rbx)
  movq %xmm6, 112(%rbx)
  movq %xmm7, 120(%rbx)
  leaq 224(%rsp), %rax
  movq %rax, 128(%rbx)
  movq $0, 136(%rbx)
  movq $0, 144(%rbx)
  movq $0, 152(%rbx)
  movq $0, 160(%rbx)
  movq $0, 168(%rbx)
  movq $0, 176(%rbx)
  movq $0, 184(%rbx)

  movq %r10, %rdi
  movq %rbx, %rsi
  call FFICallbackDispatchPascal

  movq 144(%rbx), %rax
  movq 152(%rbx), %rdx
  movq 160(%rbx), %xmm0
  movq 168(%rbx), %xmm1
  movq 176(%rbx), %xmm2
  movq 184(%rbx), %xmm3
  addq $208, %rsp
  popq %rbx
  ret
end;
{$ENDIF}

procedure FFICallbackSlot0; assembler; nostackframe; asm movq $0, %r10; jmp FFICallbackCommon end;
procedure FFICallbackSlot1; assembler; nostackframe; asm movq $1, %r10; jmp FFICallbackCommon end;
procedure FFICallbackSlot2; assembler; nostackframe; asm movq $2, %r10; jmp FFICallbackCommon end;
procedure FFICallbackSlot3; assembler; nostackframe; asm movq $3, %r10; jmp FFICallbackCommon end;
procedure FFICallbackSlot4; assembler; nostackframe; asm movq $4, %r10; jmp FFICallbackCommon end;
procedure FFICallbackSlot5; assembler; nostackframe; asm movq $5, %r10; jmp FFICallbackCommon end;
procedure FFICallbackSlot6; assembler; nostackframe; asm movq $6, %r10; jmp FFICallbackCommon end;
procedure FFICallbackSlot7; assembler; nostackframe; asm movq $7, %r10; jmp FFICallbackCommon end;
procedure FFICallbackSlot8; assembler; nostackframe; asm movq $8, %r10; jmp FFICallbackCommon end;
procedure FFICallbackSlot9; assembler; nostackframe; asm movq $9, %r10; jmp FFICallbackCommon end;
procedure FFICallbackSlot10; assembler; nostackframe; asm movq $10, %r10; jmp FFICallbackCommon end;
procedure FFICallbackSlot11; assembler; nostackframe; asm movq $11, %r10; jmp FFICallbackCommon end;
procedure FFICallbackSlot12; assembler; nostackframe; asm movq $12, %r10; jmp FFICallbackCommon end;
procedure FFICallbackSlot13; assembler; nostackframe; asm movq $13, %r10; jmp FFICallbackCommon end;
procedure FFICallbackSlot14; assembler; nostackframe; asm movq $14, %r10; jmp FFICallbackCommon end;
procedure FFICallbackSlot15; assembler; nostackframe; asm movq $15, %r10; jmp FFICallbackCommon end;
procedure FFICallbackSlot16; assembler; nostackframe; asm movq $16, %r10; jmp FFICallbackCommon end;
procedure FFICallbackSlot17; assembler; nostackframe; asm movq $17, %r10; jmp FFICallbackCommon end;
procedure FFICallbackSlot18; assembler; nostackframe; asm movq $18, %r10; jmp FFICallbackCommon end;
procedure FFICallbackSlot19; assembler; nostackframe; asm movq $19, %r10; jmp FFICallbackCommon end;
procedure FFICallbackSlot20; assembler; nostackframe; asm movq $20, %r10; jmp FFICallbackCommon end;
procedure FFICallbackSlot21; assembler; nostackframe; asm movq $21, %r10; jmp FFICallbackCommon end;
procedure FFICallbackSlot22; assembler; nostackframe; asm movq $22, %r10; jmp FFICallbackCommon end;
procedure FFICallbackSlot23; assembler; nostackframe; asm movq $23, %r10; jmp FFICallbackCommon end;
procedure FFICallbackSlot24; assembler; nostackframe; asm movq $24, %r10; jmp FFICallbackCommon end;
procedure FFICallbackSlot25; assembler; nostackframe; asm movq $25, %r10; jmp FFICallbackCommon end;
procedure FFICallbackSlot26; assembler; nostackframe; asm movq $26, %r10; jmp FFICallbackCommon end;
procedure FFICallbackSlot27; assembler; nostackframe; asm movq $27, %r10; jmp FFICallbackCommon end;
procedure FFICallbackSlot28; assembler; nostackframe; asm movq $28, %r10; jmp FFICallbackCommon end;
procedure FFICallbackSlot29; assembler; nostackframe; asm movq $29, %r10; jmp FFICallbackCommon end;
procedure FFICallbackSlot30; assembler; nostackframe; asm movq $30, %r10; jmp FFICallbackCommon end;
procedure FFICallbackSlot31; assembler; nostackframe; asm movq $31, %r10; jmp FFICallbackCommon end;
procedure FFICallbackSlot32; assembler; nostackframe; asm movq $32, %r10; jmp FFICallbackCommon end;
procedure FFICallbackSlot33; assembler; nostackframe; asm movq $33, %r10; jmp FFICallbackCommon end;
procedure FFICallbackSlot34; assembler; nostackframe; asm movq $34, %r10; jmp FFICallbackCommon end;
procedure FFICallbackSlot35; assembler; nostackframe; asm movq $35, %r10; jmp FFICallbackCommon end;
procedure FFICallbackSlot36; assembler; nostackframe; asm movq $36, %r10; jmp FFICallbackCommon end;
procedure FFICallbackSlot37; assembler; nostackframe; asm movq $37, %r10; jmp FFICallbackCommon end;
procedure FFICallbackSlot38; assembler; nostackframe; asm movq $38, %r10; jmp FFICallbackCommon end;
procedure FFICallbackSlot39; assembler; nostackframe; asm movq $39, %r10; jmp FFICallbackCommon end;
procedure FFICallbackSlot40; assembler; nostackframe; asm movq $40, %r10; jmp FFICallbackCommon end;
procedure FFICallbackSlot41; assembler; nostackframe; asm movq $41, %r10; jmp FFICallbackCommon end;
procedure FFICallbackSlot42; assembler; nostackframe; asm movq $42, %r10; jmp FFICallbackCommon end;
procedure FFICallbackSlot43; assembler; nostackframe; asm movq $43, %r10; jmp FFICallbackCommon end;
procedure FFICallbackSlot44; assembler; nostackframe; asm movq $44, %r10; jmp FFICallbackCommon end;
procedure FFICallbackSlot45; assembler; nostackframe; asm movq $45, %r10; jmp FFICallbackCommon end;
procedure FFICallbackSlot46; assembler; nostackframe; asm movq $46, %r10; jmp FFICallbackCommon end;
procedure FFICallbackSlot47; assembler; nostackframe; asm movq $47, %r10; jmp FFICallbackCommon end;
procedure FFICallbackSlot48; assembler; nostackframe; asm movq $48, %r10; jmp FFICallbackCommon end;
procedure FFICallbackSlot49; assembler; nostackframe; asm movq $49, %r10; jmp FFICallbackCommon end;
procedure FFICallbackSlot50; assembler; nostackframe; asm movq $50, %r10; jmp FFICallbackCommon end;
procedure FFICallbackSlot51; assembler; nostackframe; asm movq $51, %r10; jmp FFICallbackCommon end;
procedure FFICallbackSlot52; assembler; nostackframe; asm movq $52, %r10; jmp FFICallbackCommon end;
procedure FFICallbackSlot53; assembler; nostackframe; asm movq $53, %r10; jmp FFICallbackCommon end;
procedure FFICallbackSlot54; assembler; nostackframe; asm movq $54, %r10; jmp FFICallbackCommon end;
procedure FFICallbackSlot55; assembler; nostackframe; asm movq $55, %r10; jmp FFICallbackCommon end;
procedure FFICallbackSlot56; assembler; nostackframe; asm movq $56, %r10; jmp FFICallbackCommon end;
procedure FFICallbackSlot57; assembler; nostackframe; asm movq $57, %r10; jmp FFICallbackCommon end;
procedure FFICallbackSlot58; assembler; nostackframe; asm movq $58, %r10; jmp FFICallbackCommon end;
procedure FFICallbackSlot59; assembler; nostackframe; asm movq $59, %r10; jmp FFICallbackCommon end;
procedure FFICallbackSlot60; assembler; nostackframe; asm movq $60, %r10; jmp FFICallbackCommon end;
procedure FFICallbackSlot61; assembler; nostackframe; asm movq $61, %r10; jmp FFICallbackCommon end;
procedure FFICallbackSlot62; assembler; nostackframe; asm movq $62, %r10; jmp FFICallbackCommon end;
procedure FFICallbackSlot63; assembler; nostackframe; asm movq $63, %r10; jmp FFICallbackCommon end;
{$ENDIF}

{$IFDEF CPUI386}
{$ASMMODE ATT}
// Preserve the original callback stack separately from the aligned Pascal
// dispatch stack: all i386 cdecl arguments, including a hidden result pointer,
// remain addressable through State.StackData.
procedure FFICallbackCommon; assembler; nostackframe;
  public name 'goccia_ffi_callback_common';
asm
  pushl %ebx
  pushl %esi
  pushl %edi
  movl %esp, %edi
  movl %ecx, %esi
  andl $-16, %esp
  subl $24, %esp
  movl %esp, %ebx

  leal 16(%edi), %eax
  movl %eax, 0(%ebx)
  movl $0, 4(%ebx)
  movl $0, 8(%ebx)
  movl $0, 12(%ebx)
  movl $0, 16(%ebx)
  movl $0, 20(%ebx)

  pushl %ebx
  pushl %esi
  call FFICallbackDispatchPascal
  addl $8, %esp

  movl 4(%ebx), %eax
  movl 8(%ebx), %edx
  cmpl $4, 20(%ebx)
  je .Lffi_callback_return_single
  cmpl $8, 20(%ebx)
  je .Lffi_callback_return_double
  jmp .Lffi_callback_return_ready
.Lffi_callback_return_single:
  flds 12(%ebx)
  jmp .Lffi_callback_return_ready
.Lffi_callback_return_double:
  fldl 12(%ebx)
.Lffi_callback_return_ready:
  movl %edi, %esp
  popl %edi
  popl %esi
  popl %ebx
  ret
end;

procedure FFICallbackSlot0; assembler; nostackframe; asm movl $0, %ecx; jmp FFICallbackCommon end;
procedure FFICallbackSlot1; assembler; nostackframe; asm movl $1, %ecx; jmp FFICallbackCommon end;
procedure FFICallbackSlot2; assembler; nostackframe; asm movl $2, %ecx; jmp FFICallbackCommon end;
procedure FFICallbackSlot3; assembler; nostackframe; asm movl $3, %ecx; jmp FFICallbackCommon end;
procedure FFICallbackSlot4; assembler; nostackframe; asm movl $4, %ecx; jmp FFICallbackCommon end;
procedure FFICallbackSlot5; assembler; nostackframe; asm movl $5, %ecx; jmp FFICallbackCommon end;
procedure FFICallbackSlot6; assembler; nostackframe; asm movl $6, %ecx; jmp FFICallbackCommon end;
procedure FFICallbackSlot7; assembler; nostackframe; asm movl $7, %ecx; jmp FFICallbackCommon end;
procedure FFICallbackSlot8; assembler; nostackframe; asm movl $8, %ecx; jmp FFICallbackCommon end;
procedure FFICallbackSlot9; assembler; nostackframe; asm movl $9, %ecx; jmp FFICallbackCommon end;
procedure FFICallbackSlot10; assembler; nostackframe; asm movl $10, %ecx; jmp FFICallbackCommon end;
procedure FFICallbackSlot11; assembler; nostackframe; asm movl $11, %ecx; jmp FFICallbackCommon end;
procedure FFICallbackSlot12; assembler; nostackframe; asm movl $12, %ecx; jmp FFICallbackCommon end;
procedure FFICallbackSlot13; assembler; nostackframe; asm movl $13, %ecx; jmp FFICallbackCommon end;
procedure FFICallbackSlot14; assembler; nostackframe; asm movl $14, %ecx; jmp FFICallbackCommon end;
procedure FFICallbackSlot15; assembler; nostackframe; asm movl $15, %ecx; jmp FFICallbackCommon end;
procedure FFICallbackSlot16; assembler; nostackframe; asm movl $16, %ecx; jmp FFICallbackCommon end;
procedure FFICallbackSlot17; assembler; nostackframe; asm movl $17, %ecx; jmp FFICallbackCommon end;
procedure FFICallbackSlot18; assembler; nostackframe; asm movl $18, %ecx; jmp FFICallbackCommon end;
procedure FFICallbackSlot19; assembler; nostackframe; asm movl $19, %ecx; jmp FFICallbackCommon end;
procedure FFICallbackSlot20; assembler; nostackframe; asm movl $20, %ecx; jmp FFICallbackCommon end;
procedure FFICallbackSlot21; assembler; nostackframe; asm movl $21, %ecx; jmp FFICallbackCommon end;
procedure FFICallbackSlot22; assembler; nostackframe; asm movl $22, %ecx; jmp FFICallbackCommon end;
procedure FFICallbackSlot23; assembler; nostackframe; asm movl $23, %ecx; jmp FFICallbackCommon end;
procedure FFICallbackSlot24; assembler; nostackframe; asm movl $24, %ecx; jmp FFICallbackCommon end;
procedure FFICallbackSlot25; assembler; nostackframe; asm movl $25, %ecx; jmp FFICallbackCommon end;
procedure FFICallbackSlot26; assembler; nostackframe; asm movl $26, %ecx; jmp FFICallbackCommon end;
procedure FFICallbackSlot27; assembler; nostackframe; asm movl $27, %ecx; jmp FFICallbackCommon end;
procedure FFICallbackSlot28; assembler; nostackframe; asm movl $28, %ecx; jmp FFICallbackCommon end;
procedure FFICallbackSlot29; assembler; nostackframe; asm movl $29, %ecx; jmp FFICallbackCommon end;
procedure FFICallbackSlot30; assembler; nostackframe; asm movl $30, %ecx; jmp FFICallbackCommon end;
procedure FFICallbackSlot31; assembler; nostackframe; asm movl $31, %ecx; jmp FFICallbackCommon end;
procedure FFICallbackSlot32; assembler; nostackframe; asm movl $32, %ecx; jmp FFICallbackCommon end;
procedure FFICallbackSlot33; assembler; nostackframe; asm movl $33, %ecx; jmp FFICallbackCommon end;
procedure FFICallbackSlot34; assembler; nostackframe; asm movl $34, %ecx; jmp FFICallbackCommon end;
procedure FFICallbackSlot35; assembler; nostackframe; asm movl $35, %ecx; jmp FFICallbackCommon end;
procedure FFICallbackSlot36; assembler; nostackframe; asm movl $36, %ecx; jmp FFICallbackCommon end;
procedure FFICallbackSlot37; assembler; nostackframe; asm movl $37, %ecx; jmp FFICallbackCommon end;
procedure FFICallbackSlot38; assembler; nostackframe; asm movl $38, %ecx; jmp FFICallbackCommon end;
procedure FFICallbackSlot39; assembler; nostackframe; asm movl $39, %ecx; jmp FFICallbackCommon end;
procedure FFICallbackSlot40; assembler; nostackframe; asm movl $40, %ecx; jmp FFICallbackCommon end;
procedure FFICallbackSlot41; assembler; nostackframe; asm movl $41, %ecx; jmp FFICallbackCommon end;
procedure FFICallbackSlot42; assembler; nostackframe; asm movl $42, %ecx; jmp FFICallbackCommon end;
procedure FFICallbackSlot43; assembler; nostackframe; asm movl $43, %ecx; jmp FFICallbackCommon end;
procedure FFICallbackSlot44; assembler; nostackframe; asm movl $44, %ecx; jmp FFICallbackCommon end;
procedure FFICallbackSlot45; assembler; nostackframe; asm movl $45, %ecx; jmp FFICallbackCommon end;
procedure FFICallbackSlot46; assembler; nostackframe; asm movl $46, %ecx; jmp FFICallbackCommon end;
procedure FFICallbackSlot47; assembler; nostackframe; asm movl $47, %ecx; jmp FFICallbackCommon end;
procedure FFICallbackSlot48; assembler; nostackframe; asm movl $48, %ecx; jmp FFICallbackCommon end;
procedure FFICallbackSlot49; assembler; nostackframe; asm movl $49, %ecx; jmp FFICallbackCommon end;
procedure FFICallbackSlot50; assembler; nostackframe; asm movl $50, %ecx; jmp FFICallbackCommon end;
procedure FFICallbackSlot51; assembler; nostackframe; asm movl $51, %ecx; jmp FFICallbackCommon end;
procedure FFICallbackSlot52; assembler; nostackframe; asm movl $52, %ecx; jmp FFICallbackCommon end;
procedure FFICallbackSlot53; assembler; nostackframe; asm movl $53, %ecx; jmp FFICallbackCommon end;
procedure FFICallbackSlot54; assembler; nostackframe; asm movl $54, %ecx; jmp FFICallbackCommon end;
procedure FFICallbackSlot55; assembler; nostackframe; asm movl $55, %ecx; jmp FFICallbackCommon end;
procedure FFICallbackSlot56; assembler; nostackframe; asm movl $56, %ecx; jmp FFICallbackCommon end;
procedure FFICallbackSlot57; assembler; nostackframe; asm movl $57, %ecx; jmp FFICallbackCommon end;
procedure FFICallbackSlot58; assembler; nostackframe; asm movl $58, %ecx; jmp FFICallbackCommon end;
procedure FFICallbackSlot59; assembler; nostackframe; asm movl $59, %ecx; jmp FFICallbackCommon end;
procedure FFICallbackSlot60; assembler; nostackframe; asm movl $60, %ecx; jmp FFICallbackCommon end;
procedure FFICallbackSlot61; assembler; nostackframe; asm movl $61, %ecx; jmp FFICallbackCommon end;
procedure FFICallbackSlot62; assembler; nostackframe; asm movl $62, %ecx; jmp FFICallbackCommon end;
procedure FFICallbackSlot63; assembler; nostackframe; asm movl $63, %ecx; jmp FFICallbackCommon end;
{$ENDIF}

function CodePointerForSlot(const ASlot: Integer): CodePointer;
begin
  {$IF defined(CPUAARCH64) or defined(CPUX86_64) or defined(CPUI386)}
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
  Code: CodePointer;
begin
  {$IF defined(CPUAARCH64) or defined(CPUX86_64) or defined(CPUI386)}
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
  EnterCriticalSection(GCallbackSlotLock);
  try
    GDispatchHook := AHook;
  finally
    LeaveCriticalSection(GCallbackSlotLock);
  end;
end;

function HiddenResultLocationSupported(
  const ALocation: TGocciaFFIHiddenResultLocation): Boolean;
begin
  {$IFDEF CPUAARCH64}
  Result := ALocation in [fhrNone, fhrDedicated];
  {$ELSE}
  {$IFDEF CPUX86_64}
  Result := ALocation in [fhrNone, fhrGPR0];
  {$ELSE}
  {$IFDEF CPUI386}
  Result := ALocation in [fhrNone, fhrStack0];
  {$ELSE}
  Result := ALocation = fhrNone;
  {$ENDIF}
  {$ENDIF}
  {$ENDIF}
end;

function AllocateFFICallbackSlot(const AContext: Pointer;
  const AHiddenResultLocation: TGocciaFFIHiddenResultLocation;
  const AHiddenResultSize: PtrUInt;
  const AReturnFloatSize: LongWord;
  out ACodePointer: CodePointer): Integer;
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
  {$IFDEF CPU64}
  if AReturnFloatSize <> 0 then
    raise EArgumentException.Create(
      'FFI callback float return metadata is only valid on i386');
  {$ENDIF}
  EnterCriticalSection(GCallbackSlotLock);
  try
    for Result := 0 to MAX_FFI_CALLBACK_SLOTS - 1 do
      if not GCallbackSlots[Result].Active then
      begin
        GCallbackSlots[Result].Active := True;
        GCallbackSlots[Result].OwnerThreadId := GetCurrentThreadId;
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
    LeaveCriticalSection(GCallbackSlotLock);
  end;
  raise EOutOfMemory.Create('No FFI callback slots are available');
end;

procedure ReleaseFFICallbackSlot(const ASlot: Integer);
begin
  if (ASlot < 0) or (ASlot >= MAX_FFI_CALLBACK_SLOTS) then Exit;
  EnterCriticalSection(GCallbackSlotLock);
  try
    GCallbackSlots[ASlot].Active := False;
    GCallbackSlots[ASlot].Context := nil;
    GCallbackSlots[ASlot].HiddenResultLocation := fhrNone;
    GCallbackSlots[ASlot].HiddenResultSize := 0;
    GCallbackSlots[ASlot].ReturnFloatSize := 0;
  finally
    LeaveCriticalSection(GCallbackSlotLock);
  end;
end;

function ConsumeFFICallbackThreadViolation(const ASlot: Integer): Boolean;
begin
  Result := False;
  if (ASlot < 0) or (ASlot >= MAX_FFI_CALLBACK_SLOTS) then Exit;
  EnterCriticalSection(GCallbackSlotLock);
  try
    Result := GCallbackSlots[ASlot].ForeignThreadViolation;
    GCallbackSlots[ASlot].ForeignThreadViolation := False;
  finally
    LeaveCriticalSection(GCallbackSlotLock);
  end;
end;

function ConsumeFFICallbackThreadViolationsForCurrentThread: Boolean;
var
  I: Integer;
  OwnerThreadId: TThreadID;
begin
  Result := False;
  OwnerThreadId := GetCurrentThreadId;
  EnterCriticalSection(GCallbackSlotLock);
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
    LeaveCriticalSection(GCallbackSlotLock);
  end;
end;

procedure ValidateCallbackStateLayout;
var
  State: TGocciaFFICallbackMachineState;
  Base: PtrUInt;
begin
  Base := PtrUInt(@State);
  {$IFDEF CPU64}
  if (PtrUInt(@State.FPR[0]) - Base <> 64) or
     (PtrUInt(@State.StackData) - Base <> 128) or
     (PtrUInt(@State.HiddenResult) - Base <> 136) or
     (PtrUInt(@State.RetGPR[0]) - Base <> 144) or
     (PtrUInt(@State.RetFPR[0]) - Base <> 160) then
    raise EInvalidOpException.Create('FFI callback-state layout mismatch');
  {$ELSE}
  if (PtrUInt(@State.RetGPR[0]) - Base <> 4) or
     (PtrUInt(@State.RetFPR) - Base <> 12) or
     (PtrUInt(@State.ReturnFloatSize) - Base <> 20) then
    raise EInvalidOpException.Create('FFI callback-state layout mismatch');
  {$ENDIF}
end;

initialization
  InitCriticalSection(GCallbackSlotLock);
  ValidateCallbackStateLayout;
  ValidateCallbackSlotTable;

finalization
  DoneCriticalSection(GCallbackSlotLock);

end.
