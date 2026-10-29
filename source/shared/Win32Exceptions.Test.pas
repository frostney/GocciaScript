program Win32Exceptions.Test;

{$I Shared.inc}

{$IF DEFINED(MSWINDOWS) AND DEFINED(CPU386)}
  {$DEFINE WIN32_SEH_PROBES}
{$ENDIF}

uses
  {$IFDEF WIN32_SEH_PROBES}
  Math,
  Signals,
  Windows,
  {$ENDIF}
  SysUtils;

type
  EProbe = class(Exception);
  EReplacement = class(Exception);
  ECheckFailed = class(Exception);
  ETracked = class(EProbe)
  private
    FDestroyed: PInteger;
  public
    constructor Create(const AMessage: string; const ADestroyed: PInteger);
    destructor Destroy; override;
  end;
  TSafeCallResult = function: LongInt; stdcall;

const
  CASES: array[0..9] of string = ('finally', 'except', 'typed-on', 'reraise',
    'cleanup-raises', 'except-raises', 'safecall', 'access-violation',
    'integer-divide', 'floating-point');
  UNEXPECTED_HRESULT = LongInt(-2147418113); // E_UNEXPECTED ($8000FFFF).
  THREAD_TIMEOUT_MS = 5000;
  THREAD_SUCCESS = 37;

constructor ETracked.Create(const AMessage: string; const ADestroyed: PInteger);
begin
  inherited Create(AMessage);
  FDestroyed := ADestroyed;
end;

destructor ETracked.Destroy;
begin
  Inc(FDestroyed^);
  inherited Destroy;
end;

procedure Check(const ACondition: Boolean; const AMessage: string);
begin
  if not ACondition then
    raise ECheckFailed.Create(AMessage);
end;

procedure TestFinally;
var
  Trace: string;
begin
  Trace := '';
  try
    Trace := 'body;';
  finally
    Trace := Trace + 'cleanup;';
  end;
  Check(Trace = 'body;cleanup;', 'normal finally order');
  Trace := '';
  try
    try
      raise EProbe.Create('original');
    finally
      Trace := Trace + 'cleanup;';
    end;
  except
    on E: EProbe do
    begin
      Check(E.Message = 'original', 'finally changed exception');
      Trace := Trace + 'caught;';
    end;
  end;
  Check(Trace = 'cleanup;caught;', 'finally order');
end;

procedure TestExcept;
var
  Caught: Boolean;
begin
  Caught := False;
  try
    raise EProbe.Create('bare');
  except
    Caught := True;
    Check(ExceptObject is EProbe, 'bare except class');
    Check(Exception(ExceptObject).Message = 'bare', 'bare except message');
  end;
  Check(Caught, 'bare except not reached');
end;

procedure TestTypedOn;
var
  Caught: Boolean;
begin
  Caught := False;
  try
    raise EProbe.Create('typed');
  except
    on E: EReplacement do
      Check(False, 'wrong typed handler');
    on E: EProbe do
    begin
      Caught := True;
      Check(E.Message = 'typed', 'typed message');
    end;
  end;
  Check(Caught, 'typed handler not reached');
end;

procedure TestReraise;
var
  Original: ETracked;
  Destroyed, Caught: Integer;
begin
  Destroyed := 0;
  Caught := 0;
  Original := ETracked.Create('reraised', @Destroyed);
  try
    try
      raise Original;
    except
      on E: ETracked do
      begin
        Check(E = Original, 'inner reraise identity');
        Inc(Caught);
        // FPC replaces this frame's handler with NullHandler before searching again.
        raise;
      end;
    end;
  except
    on E: ETracked do
    begin
      Check((E = Original) and (E.Message = 'reraised'), 'outer reraise identity');
      Check(Destroyed = 0, 'reraise freed live exception');
      Inc(Caught);
    end;
  end;
  Check((Caught = 2) and (Destroyed = 1), 'reraise lifetime');
end;

procedure TestReplacement(const AInFinally: Boolean);
var
  Trace: string;
  Destroyed: Integer;
begin
  Trace := '';
  Destroyed := 0;
  try
    if AInFinally then
      try
        raise EProbe.Create('original');
      finally
        // The finally frame becomes NullHandler before entering this cleanup.
        Trace := 'cleanup;';
        raise EReplacement.Create('replacement');
      end
    else
      try
        raise ETracked.Create('original', @Destroyed);
      except
        // Raising inside an active except body unwinds FPC's NestedHandler.
        on E: ETracked do
        begin
          Trace := 'cleanup;';
          raise EReplacement.Create('replacement');
        end;
      end;
  except
    on E: EReplacement do
    begin
      Check(E.Message = 'replacement', 'replacement message');
      Trace := Trace + 'replacement;';
    end;
  end;
  Check(Trace = 'cleanup;replacement;', 'replacement order');
  // The active except-object stack owns the nested-except case.
  if not AInFinally then
    Check(Destroyed = 1, 'nested exception not freed exactly once');
end;

procedure RaiseSafeCall; safecall;
begin
  raise EProbe.Create('safecall');
end;

procedure TestSafeCall;
var
  Invoke: TSafeCallResult;
begin
  // Call the ABI directly: a Pascal safecall caller would turn HRESULT back
  // into an exception and hide whether the callee returned the right result.
  Invoke := TSafeCallResult(@RaiseSafeCall);
  Check(Invoke() = UNEXPECTED_HRESULT, 'safecall HRESULT');
end;

{$IFDEF WIN32_SEH_PROBES}
// Actual processor faults, independent of constant folding and Pascal checks.
procedure AccessViolation; assembler;
asm
  xor eax, eax
  mov eax, [eax]
end;

procedure IntegerDivide; assembler;
asm
  mov eax, 1
  cdq
  xor ecx, ecx
  idiv ecx
end;

procedure FloatingPoint; assembler;
asm
  fld1
  fldz
  fdivp st(1), st
  fwait
  fstp st(0)
end;

procedure TestHardware(const AMode: string);
var
  Caught: Boolean;
  SavedMask: TFPUExceptionMask;
begin
  Caught := False;
  SavedMask := GetExceptionMask;
  try
    SetExceptionMask(SavedMask - [exZeroDivide]);
    try
      if AMode = 'access-violation' then
        AccessViolation
      else if AMode = 'integer-divide' then
        IntegerDivide
      else
        FloatingPoint;
    except
      on E: Exception do
      begin
        Caught := True;
        Check(((AMode = 'access-violation') and (E is EAccessViolation)) or
          ((AMode = 'integer-divide') and (E is EDivByZero)) or
          ((AMode = 'floating-point') and (E is EZeroDivide)), 'hardware exception class');
      end;
    end;
  finally
    SetExceptionMask(SavedMask);
  end;
  Check(Caught, 'hardware exception not raised');
end;

function ExceptionThread(AData: Pointer): PtrInt;
begin
  Result := 1;
  if AData = nil then
    // No Pascal except frame: BeginThread's main_wrapper must handle this.
    raise EProbe.Create('unhandled');
  try
    TestFinally;
    TestReraise;
    TestHardware('access-violation');
    Result := THREAD_SUCCESS;
  except
    // The joining thread requires the success value, not just termination.
    Result := 1;
  end;
end;

procedure TestThread(const AUnhandled: Boolean);
var
  ThreadHandle: TThreadID;
  ThreadResult: DWORD;
  Data: Pointer;
begin
  if AUnhandled then Data := nil else Data := @ThreadResult;
  ThreadHandle := BeginThread(@ExceptionThread, Data);
  Check(ThreadHandle <> 0, 'BeginThread failed');
  if WaitForSingleObject(ThreadHandle, THREAD_TIMEOUT_MS) <> WAIT_OBJECT_0 then
    // The child process owns this thread. Never leave a stuck worker running.
    Halt(1);
  Check(GetExitCodeThread(ThreadHandle, ThreadResult), 'thread result unavailable');
  Check(CloseHandle(ThreadHandle), 'thread handle not closed');
  Check(not AUnhandled, 'unhandled thread returned');
  Check(ThreadResult = THREAD_SUCCESS, 'thread exception behavior');
end;

// Removal has no public Pascal API. This exported finalizer in the pinned
// signals.o calls only remove_exception_handler, which is idempotent. Calling
// it in these isolated child modes tests the real RTL cleanup before exit.
procedure FinalizeSignals; external name 'FINALIZE$_$SIGNALS';

function ExceptionChain: Pointer; assembler; nostackframe;
asm
  mov eax, fs:[0]
end;

function UnexpectedSignal(const ASignal: LongInt): LongInt; cdecl;
begin
  Result := 0;
  Halt(1); // A restored Pascal handler must consume the subsequent fault.
end;

procedure TestSignalRestoration;
var
  SavedFrame: System.PEXCEPTION_FRAME;
  SavedChain, SavedEntry, InstalledEntry, InstalledChain: Pointer;
  Pass: Integer;
begin
  SavedFrame := System_exception_frame;
  Check(SavedFrame <> nil, 'missing executable exception frame');
  SavedChain := ExceptionChain;
  SavedEntry := SavedFrame^.Handler;
  for Pass := 1 to 2 do
  begin
    Signal(SIGSEGV, @UnexpectedSignal);
    InstalledEntry := SavedFrame^.Handler;
    InstalledChain := ExceptionChain;
    FinalizeSignals;
    Check((InstalledChain = SavedFrame) and (InstalledEntry <> SavedEntry),
      'signals frame not installed');
    Check((ExceptionChain = SavedChain) and (SavedFrame^.Handler = SavedEntry),
      'signals frame not restored');
    // Prove that restored state still dispatches and unwinds Pascal handlers.
    TestHardware('access-violation');
    TestReraise;
    FinalizeSignals;
    Check((ExceptionChain = SavedChain) and (SavedFrame^.Handler = SavedEntry),
      'signals removal not idempotent');
  end;
end;

type
  TExceptionFilter = function(AException: Windows.PEXCEPTION_POINTERS): LongInt; stdcall;

function SetExceptionFilter(const AFilter: TExceptionFilter): TExceptionFilter;
  stdcall; external 'kernel32.dll' name 'SetUnhandledExceptionFilter';

function PreviousFilter(AException: Windows.PEXCEPTION_POINTERS): LongInt; stdcall;
begin
  Result := EXCEPTION_CONTINUE_SEARCH;
end;

procedure TestSignalAPIRestoration;
var
  SavedFrame: System.PEXCEPTION_FRAME;
  SavedChain, SavedEntry: Pointer;
  OriginalFilter, InstalledFilter, RestoredFilter: TExceptionFilter;
begin
  SavedFrame := System_exception_frame;
  Check(SavedFrame <> nil, 'missing executable exception frame');
  SavedChain := ExceptionChain;
  SavedEntry := SavedFrame^.Handler;
  OriginalFilter := SetExceptionFilter(@PreviousFilter);
  // EXE startup always initializes this pointer. Explicitly reproduce the
  // nil-frame state to exercise the RTL's alternative registration branch.
  System_exception_frame := nil;
  Signal(SIGSEGV, @UnexpectedSignal);
  InstalledFilter := SetExceptionFilter(@PreviousFilter);
  SetExceptionFilter(InstalledFilter);
  FinalizeSignals;
  RestoredFilter := SetExceptionFilter(OriginalFilter);
  System_exception_frame := SavedFrame;
  Check(Assigned(InstalledFilter) and (@InstalledFilter <> @PreviousFilter),
    'signals API filter not installed');
  Check(@RestoredFilter = @PreviousFilter, 'signals API filter not restored');
  Check((ExceptionChain = SavedChain) and (SavedFrame^.Handler = SavedEntry),
    'signals API path changed SEH chain');
  TestHardware('access-violation');
end;

procedure Unhandled(const AObject: TObject; const AAddress: CodePointer;
  const AFrameCount: LongInt; const AFrames: PCodePointer);
begin
  // Preserve the RTL's default exit path, replacing only its address-bearing
  // diagnostic. Returning here must still make __FPC_DEFAULT_HANDLER exit 217.
  if (((ParamStr(1) = 'default-pascal') or (ParamStr(1) = 'thread-default')) and (AObject is EProbe) and
      (Exception(AObject).Message = 'unhandled')) or
     ((ParamStr(1) = 'default-hardware') and (AObject is EAccessViolation)) then
    WriteLn('PASS ', ParamStr(1))
  else
    WriteLn('FAIL default exception');
end;

function SignalCaught(const ASignal: LongInt): LongInt; cdecl;
begin
  Result := 0;
  if ((ParamStr(1) = 'signals-access') and (ASignal = SIGSEGV)) or
     ((ParamStr(1) = 'signals-divide') and (ASignal = SIGFPE)) then
  begin
    WriteLn('PASS ', ParamStr(1));
    Halt(0);
  end;
  Halt(1);
end;

procedure RunChild(const AMode: string);
begin
  if (AMode = 'default-pascal') or (AMode = 'default-hardware') or
    (AMode = 'thread-default') then
  begin
    ExceptProc := @Unhandled;
    if AMode = 'thread-default' then
      TestThread(True)
    else if AMode = 'default-pascal' then
      raise EProbe.Create('unhandled')
    else
      AccessViolation;
  end
  else if AMode = 'signals-access' then
  begin
    // signal() replaces the outermost SEH frame's handler. No surrounding
    // Pascal except frame may consume the hardware fault before it gets there.
    Signal(SIGSEGV, @SignalCaught);
    AccessViolation;
  end
  else if AMode = 'signals-divide' then
  begin
    Signal(SIGFPE, @SignalCaught);
    IntegerDivide;
  end
  else if (AMode = 'thread-caught') or (AMode = 'signals-restore') or
    (AMode = 'signals-api-restore') then
  begin
    if AMode = 'thread-caught' then TestThread(False)
    else if AMode = 'signals-restore' then TestSignalRestoration
    else TestSignalAPIRestoration;
    WriteLn('PASS ', AMode);
    Halt(0);
  end
  else
    Halt(2);
  Halt(1); // A child fault must never return normally.
end;
{$ENDIF}

procedure RunCase(const AMode: string);
begin
  if AMode = 'finally' then TestFinally
  else if AMode = 'except' then TestExcept
  else if AMode = 'typed-on' then TestTypedOn
  else if AMode = 'reraise' then TestReraise
  else if AMode = 'cleanup-raises' then TestReplacement(True)
  else if AMode = 'except-raises' then TestReplacement(False)
  else if AMode = 'safecall' then TestSafeCall
  else if (AMode = 'access-violation') or (AMode = 'integer-divide') or
    (AMode = 'floating-point') then
  begin
    {$IFDEF WIN32_SEH_PROBES}
    TestHardware(AMode);
    {$ELSE}
    WriteLn('SKIP ', AMode, ' (requires i386 Windows)');
    Exit;
    {$ENDIF}
  end
  else
    raise ECheckFailed.Create('unknown case');
  WriteLn('PASS ', AMode);
end;

var
  Mode: string;
begin
  {$IFDEF WIN32_SEH_PROBES}
  SetErrorMode(SEM_FAILCRITICALERRORS or SEM_NOGPFAULTERRORBOX);
  if (ParamCount = 1) and ((Copy(ParamStr(1), 1, 8) = 'default-') or
    (Copy(ParamStr(1), 1, 8) = 'signals-') or
    (Copy(ParamStr(1), 1, 7) = 'thread-')) then
    RunChild(ParamStr(1));
  {$ENDIF}
  try
    if ParamCount = 0 then
      for Mode in CASES do
        RunCase(Mode)
    else if ParamCount = 1 then
      RunCase(ParamStr(1))
    else
      raise ECheckFailed.Create('expected zero or one case argument');
  except
    on E: Exception do
    begin
      WriteLn('FAIL ', E.ClassName, ': ', E.Message);
      ExitCode := 1;
    end;
  end;
end.
