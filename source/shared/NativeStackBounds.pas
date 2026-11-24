unit NativeStackBounds;

{$I Shared.inc}

interface

// The lowest address of the calling thread's native stack and its size: the
// system's bounds where it gives them, else the runtime library's. False when
// neither is known.
function TryGetThreadStackBounds(out ALow, ASize: NativeUInt): Boolean;

implementation

uses
  {$IFDEF MSWINDOWS}Windows,{$ENDIF}
  {$IF DEFINED(LINUX) AND NOT DEFINED(LAKON)}BaseUnix, Syscall, UnixType,{$IFEND}
  SysUtils;

// The address of a local in a frame called from the caller's, which is where
// the native stack currently ends.
{$IFDEF FPC}{$OPTIMIZATION NOAUTOINLINE}{$ENDIF}
function NativeStackPosition: NativeUInt;
var
  Marker: Byte;
begin
  Result := NativeUInt(@Marker);
end;
{$IFDEF PRODUCTION}
  {$IFDEF FPC}
    {$OPTIMIZATION AUTOINLINE}
  {$ENDIF}
{$ENDIF}

// TryGetSystemStackBounds reports the lowest address of the calling thread's
// stack and its size, as the operating system reserved it.
{$IF DEFINED(LAKON)}
function TryGetSystemStackBounds(out ALow, ASize: NativeUInt): Boolean;
begin
  ALow := 0;
  ASize := 0;
  Result := False;
end;
{$ELSEIF DEFINED(MSWINDOWS)}
type
  TGetCurrentThreadStackLimits = procedure(out ALowLimit,
    AHighLimit: NativeUInt); stdcall;

function TryGetSystemStackBounds(out ALow, ASize: NativeUInt): Boolean;
var
  GetLimits: TGetCurrentThreadStackLimits;
  HighLimit: NativeUInt;
begin
  ALow := 0;
  ASize := 0;
  // Windows 8 and later. Looked up rather than imported, so the program
  // still starts where it is missing and falls back to the count.
  GetLimits := TGetCurrentThreadStackLimits(GetProcAddress(
    GetModuleHandleW(PWideChar(WideString('kernel32.dll'))),
    PAnsiChar('GetCurrentThreadStackLimits')));
  if not Assigned(GetLimits) then
    Exit(False);
  GetLimits(ALow, HighLimit);
  ASize := HighLimit - ALow;
  Result := HighLimit > ALow;
end;
{$ELSEIF DEFINED(DARWIN)}
function pthread_self: Pointer; cdecl; external 'c' name 'pthread_self';
function Pthread_get_stackaddr_np(AThread: Pointer): Pointer; cdecl;
  external 'c' name 'pthread_get_stackaddr_np';
function Pthread_get_stacksize_np(AThread: Pointer): NativeUInt; cdecl;
  external 'c' name 'pthread_get_stacksize_np';

function TryGetSystemStackBounds(out ALow, ASize: NativeUInt): Boolean;
var
  Thread: Pointer;
  StackTop: NativeUInt;
begin
  Thread := pthread_self;
  // The address is the top of the stack.
  StackTop := NativeUInt(Pthread_get_stackaddr_np(Thread));
  ASize := Pthread_get_stacksize_np(Thread);
  ALow := StackTop - ASize;
  Result := (ASize > 0) and (StackTop > ASize);
end;
{$ELSEIF DEFINED(LINUX)}
const
  // The kernel keeps a growing stack this far above the mapping below it
  // (stack_guard_gap, 256 pages by default).
  STACK_GUARD_GAP = 1024 * 1024;

// Parses the "start-end" address range at the start of a /proc/self/maps
// line.
function TryParseMapRange(const ALine: AnsiString;
  out AStart, AEnd: NativeUInt): Boolean;
var
  I, Digit: Integer;
  Value: NativeUInt;
  InEnd: Boolean;
begin
  AStart := 0;
  AEnd := 0;
  Value := 0;
  InEnd := False;
  for I := 1 to Length(ALine) do
  begin
    case ALine[I] of
      '0'..'9': Digit := Ord(ALine[I]) - Ord('0');
      'a'..'f': Digit := Ord(ALine[I]) - Ord('a') + 10;
      'A'..'F': Digit := Ord(ALine[I]) - Ord('A') + 10;
      '-':
        begin
          if InEnd then
            Exit(False);
          AStart := Value;
          Value := 0;
          InEnd := True;
          Continue;
        end;
    else
      Break;
    end;
    Value := Value * 16 + NativeUInt(Digit);
  end;
  AEnd := Value;
  Result := InEnd and (AEnd > AStart);
end;

// The main thread's id is the process id. Asked through the system call, so
// that it does not depend on the thread manager.
function IsMainThread: Boolean;
begin
  Result := Do_SysCall(syscall_nr_gettid) = TSysResult(FpGetPid);
end;

// glibc 2.34 and later define these in libc; earlier versions, such as the
// one the aarch64 build links against, only in libpthread. Naming libpthread
// links it in either case, as the cthreads unit does.
type
  PPthreadAttr = ^pthread_attr_t;

function pthread_self: pthread_t; cdecl; external 'pthread'
  name 'pthread_self';
function pthread_getattr_np(AThread: pthread_t;
  AAttr: PPthreadAttr): cint; cdecl; external 'pthread'
  name 'pthread_getattr_np';
function pthread_attr_getstack(AAttr: PPthreadAttr; AStackAddr: PPointer;
  AStackSize: psize_t): cint; cdecl; external 'pthread'
  name 'pthread_attr_getstack';
function Pthread_attr_destroy(AAttr: PPthreadAttr): cint; cdecl;
  external 'pthread' name 'pthread_attr_destroy';

// Asks the thread library for the stack it gave the calling thread. For a
// thread it created, glibc answers from its own record of the stack and
// leaves the guard pages out: the lowest address is the first one above the
// guard, and the size runs from there to the top, where the thread's static
// TLS and its descriptor take the first bytes.
function TryGetThreadLibraryStackBounds(out ALow, ASize: NativeUInt): Boolean;
var
  Attr: pthread_attr_t;
  StackAddr: Pointer;
  StackSize: size_t;
begin
  ALow := 0;
  ASize := 0;
  Result := False;
  if pthread_getattr_np(pthread_self, @Attr) <> 0 then
    Exit;
  try
    if (pthread_attr_getstack(@Attr, @StackAddr, @StackSize) = 0) and
       (StackSize > 0) then
    begin
      ALow := NativeUInt(StackAddr);
      ASize := NativeUInt(StackSize);
      Result := True;
    end;
  finally
    Pthread_attr_destroy(@Attr);
  end;
end;

// Another thread's bounds come from the thread library, which knows the
// stack it gave the thread, and not from /proc/self/maps alone: glibc 2.42
// and later install a thread's guard page inside its stack's mapping
// (MADV_GUARD_INSTALL, Linux 6.13 and later) instead of as a mapping of its
// own, and the kernel merges two thread stacks that lie next to each other
// into one mapping. /proc/self/maps then cannot tell a thread's own stack from
// a neighbour's: a 512 KiB stack mapped just below a 4 MiB one reads as
// 4.5 MiB. glibc before 2.34 keeps pthread_getattr_np in libpthread, so every
// Linux program that uses this unit links it. The thread library's bounds are
// still narrowed to the mapping that holds the running frame: for a stack the
// host supplied, glibc reports the whole buffer, including any pages the host
// protected at its bottom.
//
// The main thread's bounds, and another thread's where the thread library
// cannot give them or gives bounds that do not hold the running frame (code
// running on a stack of the host's own, such as a fiber), are read from the
// mapping that holds the running frame in /proc/self/maps. The main thread's
// stack grows down to RLIMIT_STACK below its top, but not into the guard gap
// above the mapping beneath it. The kernel marks it [stack]; Valgrind gives
// its program a main stack of its own, which is not marked and starts a few
// pages long.
function TryGetSystemStackBounds(out ALow, ASize: NativeUInt): Boolean;
var
  Maps: TextFile;
  Line: AnsiString;
  Position, MapStart, MapEnd, PreviousEnd, Size: NativeUInt;
  LibraryLow, LibrarySize, LibraryTop: NativeUInt;
  HasLibraryBounds: Boolean;
  Limits: TRLimit;
begin
  ALow := 0;
  ASize := 0;
  Result := False;
  Position := NativeStackPosition;
  HasLibraryBounds := not IsMainThread and
    TryGetThreadLibraryStackBounds(LibraryLow, LibrarySize) and
    (Position > LibraryLow) and (Position - LibraryLow < LibrarySize);
  AssignFile(Maps, '/proc/self/maps');
  {$I-}
  Reset(Maps);
  {$I+}
  if IOResult <> 0 then
  begin
    if HasLibraryBounds then
    begin
      ALow := LibraryLow;
      ASize := LibrarySize;
      Result := True;
    end;
    Exit;
  end;
  try
    PreviousEnd := 0;
    while not Eof(Maps) do
    begin
      ReadLn(Maps, Line);
      if not TryParseMapRange(Line, MapStart, MapEnd) then
        Continue;
      if (Position >= MapStart) and (Position < MapEnd) then
      begin
        if HasLibraryBounds then
        begin
          // Both hold the running frame, so they overlap.
          LibraryTop := LibraryLow + LibrarySize;
          if MapStart > LibraryLow then
            ALow := MapStart
          else
            ALow := LibraryLow;
          if MapEnd < LibraryTop then
            ASize := MapEnd - ALow
          else
            ASize := LibraryTop - ALow;
        end
        else if (Pos('[stack]', Line) > 0) or IsMainThread then
        begin
          if (FpGetRLimit(RLIMIT_STACK, @Limits) = 0) and
             (Limits.rlim_cur <> High(rlim_t)) then
            Size := NativeUInt(Limits.rlim_cur)
          else
            Size := StackLength;
          if MapEnd - PreviousEnd < Size + STACK_GUARD_GAP then
          begin
            if MapEnd - PreviousEnd <= STACK_GUARD_GAP then
              Exit;
            Size := MapEnd - PreviousEnd - STACK_GUARD_GAP;
          end;
          if Size > MapEnd then
            Exit;
          ALow := MapEnd - Size;
          ASize := Size;
        end
        else
        begin
          ALow := MapStart;
          ASize := MapEnd - MapStart;
        end;
        Exit(ASize > 0);
      end;
      PreviousEnd := MapEnd;
    end;
    if HasLibraryBounds then
    begin
      ALow := LibraryLow;
      ASize := LibrarySize;
      Result := True;
    end;
  finally
    CloseFile(Maps);
  end;
end;
{$ELSE}
function TryGetSystemStackBounds(out ALow, ASize: NativeUInt): Boolean;
begin
  ALow := 0;
  ASize := 0;
  Result := False;
end;
{$IFEND}

// The system's bounds where it gives them, else the RTL's. On Darwin the
// RTL's StackBottom and StackLength are not used at all: on x86_64-darwin they
// describe the 256 KiB the compiler defaults to instead of the 8 MiB main
// thread. Elsewhere the RTL's bottom is used when it is the higher one: a
// development build checks the stack against it ({$S+}) and stops with a fatal
// "Stack overflow" past it, and glibc can hand a thread a cached stack larger
// than the one the RTL was told about.
function TryGetThreadStackBounds(out ALow, ASize: NativeUInt): Boolean;
{$IF DEFINED(FPC) AND NOT DEFINED(DARWIN) AND NOT DEFINED(LAKON)}
var
  RTLLow, StackTop: NativeUInt;
{$IFEND}
begin
  Result := TryGetSystemStackBounds(ALow, ASize);
  {$IF DEFINED(FPC) AND NOT DEFINED(DARWIN) AND NOT DEFINED(LAKON)}
  RTLLow := NativeUInt(StackBottom);
  if (RTLLow = 0) or (StackLength = 0) then
    Exit;
  if not Result then
  begin
    ALow := RTLLow;
    ASize := StackLength;
    Result := True;
  end
  else if (RTLLow > ALow) and (RTLLow < ALow + ASize) then
  begin
    StackTop := ALow + ASize;
    ALow := RTLLow;
    ASize := StackTop - ALow;
  end;
  {$IFEND}
end;

end.
