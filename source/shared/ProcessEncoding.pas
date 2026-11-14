{ The process-wide string encoding.

  GocciaScript strings are UTF-16, and FPC 3.2's RTL is not Unicode: every
  AnsiString it takes or returns (TFileStream and FindFirst file names,
  TStringList items, text written with WriteLn) goes through the system code
  page. Under the usual UTF-8 locale on Linux and macOS that code page is
  UTF-8, so the round trip loses nothing. On Windows it is the ANSI code page
  (1252 on most systems), so a file name or an output line with a character
  outside it comes back with '?'.

  On Windows this unit makes UTF-8 the code page of those conversions and of
  the names the RTL returns from the file system, and the code page of
  standard output and standard error, so Windows reads, lists and writes the
  same strings and the same bytes as the other platforms when output is
  redirected to a file or a pipe; a GUI application's standard files keep the
  RTL's settings. When a standard handle is a console, ConsoleOutput writes
  its text to the console as UTF-16 with WriteConsoleW, so the console shows
  every character its font has, without a change to the console's code
  page. A unit that converts strings through the RTL lists this
  unit in its uses, so that it is initialized first.

  Standard output and standard error are thread variables in FPC, which the
  RTL opens again for each new thread with its defaults, so this applies to the
  main thread. The thread pool's workers capture their output for the main
  thread to write.

  On Linux and macOS this unit installs a string manager of its own that
  converts between UTF-16 and UTF-8 (or the locale's ASCII or Latin code page)
  directly, without cwstring's setlocale and iconv. }

unit ProcessEncoding;

{$I Shared.inc}

interface

implementation

{$IF DEFINED(MSWINDOWS)}
uses
  Windows,

  ConsoleOutput;

const
  { At most as many UTF-16 code units as UTF-8 bytes: a one-byte to
    three-byte character is one code unit, a four-byte one two. }
  CONSOLE_CHUNK_BYTES = 1024;

function IsConsoleHandle(const AHandle: THandle): Boolean;
var
  Mode: DWORD;
begin
  Result := GetConsoleMode(AHandle, Mode);
end;

function WriteConsoleCharacters(const AHandle: THandle;
  const ACharacters: PWideChar; const ACount: SizeInt): Boolean;
var
  Next: PWideChar;
  Left: SizeInt;
  Written: DWORD;
begin
  Next := ACharacters;
  Left := ACount;
  while Left > 0 do
  begin
    Written := 0;
    if not WriteConsoleW(AHandle, Next, Left, Written, nil) or
      (Written = 0) then
      Exit(False);
    Inc(Next, Written);
    Dec(Left, Written);
  end;
  Result := True;
end;

{ Converts UTF-8 to UTF-16 in a buffer on the stack, so that writing to the
  console allocates no memory, like the RTL's own write function: the runtime
  error message at exit can follow a failed allocation. A malformed byte
  becomes U+FFFD. }
function WriteConsoleUTF8(const AHandle: THandle; const AText: PAnsiChar;
  const ALength: SizeInt): Boolean;
var
  Characters: array[0..CONSOLE_CHUNK_BYTES - 1] of WideChar;
  Next: PAnsiChar;
  Left, Chunk, Count: SizeInt;
begin
  Next := AText;
  Left := ALength;
  while Left > 0 do
  begin
    Chunk := Left;
    if Chunk > CONSOLE_CHUNK_BYTES then
      Chunk := CompleteUTF8Length(Next, CONSOLE_CHUNK_BYTES);
    Count := MultiByteToWideChar(CP_UTF8, 0, Next, Chunk, @Characters[0],
      CONSOLE_CHUNK_BYTES);
    if (Count = 0) or
      not WriteConsoleCharacters(AHandle, @Characters[0], Count) then
      Exit(False);
    Inc(Next, Chunk);
    Dec(Left, Chunk);
  end;
  Result := True;
end;

initialization
  SetMultiByteConversionCodePage(CP_UTF8);
  SetMultiByteRTLFileSystemCodePage(CP_UTF8);
  // A GUI application's StdOut and StdErr show a message box, and its other
  // standard files are not open, so they keep the RTL's settings.
  if IsConsole then
  begin
    UseUnicodeOutput(Output, IsConsoleHandle, WriteConsoleUTF8);
    UseUnicodeOutput(StdOut, IsConsoleHandle, WriteConsoleUTF8);
    UseUnicodeOutput(ErrOutput, IsConsoleHandle, WriteConsoleUTF8);
    UseUnicodeOutput(StdErr, IsConsoleHandle, WriteConsoleUTF8);
  end;
{$ELSEIF DEFINED(FPC) AND DEFINED(UNIX)}
uses
  BaseUnix,

  unixcp,
  UnixType;

{ A UTF-16 string manager of the engine's own, in place of cwstring. cwstring
  converts through iconv and calls setlocale at start-up, which loads the
  locale's files and iconv's modules on every run (about 0.75 ms); the
  conversions the engine needs are to and from UTF-8 and a few single-byte
  code pages, done here directly. Any other code page (an explicit
  SetCodePage to one) goes through iconv, opened only for that conversion,
  as cwstring did for a code page other than the process's.

  The process code page follows the locale environment as cwstring's does,
  without setlocale: the first of LC_ALL, LC_CTYPE and LANG that is set names
  it. None set, a codeset of UTF-8, or one this unit does not know: UTF-8. "C"
  or "POSIX": ASCII. ISO-8859-1 and ISO-8859-15: those. A locale name without
  a codeset (en_US) is UTF-8 here, where cwstring would have taken the
  locale's legacy codeset.

  Malformed UTF-8 and lone surrogates become U+FFFD in UTF-8. In a single-byte
  code page, as iconv does, a character it does not have becomes '?' (a
  surrogate pair one '?') and a lone surrogate is dropped; iconv's
  transliterations (an em dash as "--") are not made. Case mapping and comparison through the
  RTL's string manager (UnicodeUpperCase, UnicodeCompareStr and their wide
  forms) are ASCII case mapping and code unit order: the engine maps case
  with its own Unicode data (TextSemantics), and nothing compares through the
  manager by locale. }

const
  CP_LATIN1 = 28591;
  CP_LATIN9 = 28605;
  REPLACEMENT_CHARACTER = $FFFD;

var
  GProcessCodePage: TSystemCodePage;

type
  TIconv = Pointer;

const
  ICONV_FAILED = TIconv(-1);
  UTF16_NATIVE = {$IFDEF ENDIAN_LITTLE}'UTF-16LE'{$ELSE}'UTF-16BE'{$ENDIF};

function iconv_open(ATo, AFrom: PAnsiChar): TIconv; cdecl; external 'c' name 'iconv_open';
function Iconv(ACd: TIconv; AIn: PPAnsiChar; AInLeft: psize_t; AOut: PPAnsiChar; AOutLeft: psize_t): size_t;
  cdecl; external 'c' name 'iconv';
function iconv_close(ACd: TIconv): cint; cdecl; external 'c' name 'iconv_close';

{ iconv's name for a code page ('' when unixcp does not know it) }
function IconvName(const ACodePage: TSystemCodePage): RawByteString;
var
  Index: Integer;
begin
  Index := GetCodepageData(ACodePage);
  if Index >= 0 then
    Result := UnixCpMap[Index].name
  else
    Result := '';
end;

{ ALength bytes from AFrom converted by iconv's converter ACd into ADest (ADestUnit bytes a unit: 1 a byte
  string, 2 UTF-16), an input unit it cannot convert as '?' in the output; False when iconv failed otherwise }
function IconvConvert(ACd: TIconv; AFrom: PAnsiChar; ALength: SizeInt; AFromUnit: Integer;
  var ABuffer: RawByteString; out AUsed: SizeInt; AReplacement: PAnsiChar; AReplacementSize: SizeInt): Boolean;
var
  InP, OutP: PAnsiChar;
  InLeft, OutLeft: size_t;
  Offset: SizeInt;
begin
  SetLength(ABuffer, ALength * 4 + 16);
  InP := AFrom;
  InLeft := ALength;
  OutP := PAnsiChar(Pointer(ABuffer));
  OutLeft := Length(ABuffer);
  while InLeft > 0 do
    if Iconv(ACd, @InP, @InLeft, @OutP, @OutLeft) = size_t(-1) then
    begin
      Offset := OutP - PAnsiChar(Pointer(ABuffer));
      if (fpgeterrno = ESysE2BIG) or (OutLeft < size_t(AReplacementSize)) then
      begin
        SetLength(ABuffer, Length(ABuffer) * 2);
        OutP := PAnsiChar(Pointer(ABuffer)) + Offset;
        OutLeft := Length(ABuffer) - Offset;
        Continue;
      end;
      if (fpgeterrno <> ESysEILSEQ) and (fpgeterrno <> ESysEINVAL) then
        Exit(False);
      { a unit it cannot convert: the replacement, then the next one }
      Move(AReplacement^, OutP^, AReplacementSize);
      Inc(OutP, AReplacementSize);
      Dec(OutLeft, AReplacementSize);
      Inc(InP, AFromUnit);
      if InLeft >= size_t(AFromUnit) then Dec(InLeft, AFromUnit) else InLeft := 0;
    end;
  AUsed := OutP - PAnsiChar(Pointer(ABuffer));
  Result := True;
end;

function IconvFromUTF16(ASource: PUnicodeChar; var ADest: RawByteString; ACodePage: TSystemCodePage;
  ALength: SizeInt): Boolean;
var
  Name: RawByteString;
  Cd: TIconv;
  Used: SizeInt;
  Question: AnsiChar;
begin
  Name := IconvName(ACodePage);
  if Name = '' then
    Exit(False);
  Cd := iconv_open(PAnsiChar(Name), UTF16_NATIVE);
  if Cd = ICONV_FAILED then
    Exit(False);
  Question := '?';
  Result := IconvConvert(Cd, PAnsiChar(ASource), ALength * 2, 2, ADest, Used, @Question, 1);
  iconv_close(Cd);
  if Result then
    SetLength(ADest, Used);
end;

function IconvToUTF16(ASource: PAnsiChar; ACodePage: TSystemCodePage; var ADest: UnicodeString;
  ALength: SizeInt): Boolean;
var
  Name, Buffer: RawByteString;
  Cd: TIconv;
  Used: SizeInt;
  Replacement: UnicodeChar;
begin
  Name := IconvName(ACodePage);
  if Name = '' then
    Exit(False);
  Cd := iconv_open(UTF16_NATIVE, PAnsiChar(Name));
  if Cd = ICONV_FAILED then
    Exit(False);
  Replacement := UnicodeChar($FFFD);
  Result := IconvConvert(Cd, ASource, ALength, 1, Buffer, Used, @Replacement, 2);
  iconv_close(Cd);
  if Result then
  begin
    SetLength(ADest, Used div 2);
    if Used > 0 then
      Move(Pointer(Buffer)^, Pointer(ADest)^, Used);
  end;
end;

{ The code page a locale value (en_GB.UTF-8, C, de_DE.ISO-8859-15@euro) names. }
function CodePageOfLocale(const ALocale: AnsiString): TSystemCodePage;
var
  Codeset: AnsiString;
  Dot, I: SizeInt;
  C: AnsiChar;
begin
  if (ALocale = 'C') or (ALocale = 'POSIX') then
    Exit(CP_ASCII);
  Dot := Pos('.', ALocale);
  if Dot = 0 then
    Exit(CP_UTF8);
  Codeset := '';
  for I := Dot + 1 to Length(ALocale) do
  begin
    C := ALocale[I];
    if C = '@' then
      Break;
    if (C = '-') or (C = '_') then
      Continue;
    if (C >= 'A') and (C <= 'Z') then
      C := AnsiChar(Ord(C) + 32);
    Codeset := Codeset + C;
  end;
  if (Codeset = 'iso88591') or (Codeset = 'latin1') then
    Result := CP_LATIN1
  else if (Codeset = 'iso885915') or (Codeset = 'latin9') then
    Result := CP_LATIN9
  else if (Codeset = 'ascii') or (Codeset = 'usascii') or (Codeset = 'ansix3.41968') then
    Result := CP_ASCII
  else
    Result := CP_UTF8;
end;

function DetectProcessCodePage: TSystemCodePage;
const
  NAMES: array[0..2] of PAnsiChar = ('LC_ALL', 'LC_CTYPE', 'LANG');
var
  I: Integer;
  Value: PAnsiChar;
begin
  for I := Low(NAMES) to High(NAMES) do
  begin
    Value := fpGetEnv(NAMES[I]);
    if (Value <> nil) and (Value^ <> #0) then
      Exit(CodePageOfLocale(AnsiString(Value)));
  end;
  Result := CP_UTF8;
end;

function ResolveCodePage(const ACodePage: TSystemCodePage): TSystemCodePage; inline;
begin
  if (ACodePage = CP_ACP) or (ACodePage = CP_OEMCP) or (ACodePage = CP_NONE) then
    Result := GProcessCodePage
  else
    Result := ACodePage;
end;

{ ISO-8859-15's eight characters that differ from ISO-8859-1: byte, character. }
const
  LATIN9_BYTES: array[0..7] of Byte = ($A4, $A6, $A8, $B4, $B8, $BC, $BD, $BE);
  LATIN9_CHARS: array[0..7] of Word = ($20AC, $0160, $0161, $017D, $017E, $0152, $0153, $0178);

function Latin9ByteOf(const ACharacter: Word): Integer;
var
  I: Integer;
begin
  for I := 0 to High(LATIN9_CHARS) do
    if LATIN9_CHARS[I] = ACharacter then
      Exit(LATIN9_BYTES[I]);
  if ACharacter <= $FF then
  begin
    for I := 0 to High(LATIN9_BYTES) do
      if LATIN9_BYTES[I] = ACharacter then
        Exit(-1);
    Exit(ACharacter);
  end;
  Result := -1;
end;

function Latin9CharOf(const AByte: Byte): Word;
var
  I: Integer;
begin
  for I := 0 to High(LATIN9_BYTES) do
    if LATIN9_BYTES[I] = AByte then
      Exit(LATIN9_CHARS[I]);
  Result := AByte;
end;

procedure UTF16ToUTF8(ASource: PUnicodeChar; var ADest: RawByteString; ALength: SizeInt);
var
  I, Size: SizeInt;
  C, D: Cardinal;
  P: PByte;
begin
  { the size first: one to three bytes a code unit, four for a surrogate pair }
  Size := 0;
  I := 0;
  while I < ALength do
  begin
    C := Ord(ASource[I]);
    if C < $80 then
      Inc(Size)
    else if C < $800 then
      Inc(Size, 2)
    else if (C >= $D800) and (C <= $DBFF) and (I + 1 < ALength) and
      (Ord(ASource[I + 1]) >= $DC00) and (Ord(ASource[I + 1]) <= $DFFF) then
    begin
      Inc(Size, 4);
      Inc(I);
    end
    else
      Inc(Size, 3);
    Inc(I);
  end;
  SetLength(ADest, Size);
  if Size = 0 then
    Exit;
  P := PByte(Pointer(ADest));
  I := 0;
  while I < ALength do
  begin
    C := Ord(ASource[I]);
    if C < $80 then
    begin
      P^ := C;
      Inc(P);
    end
    else if C < $800 then
    begin
      P[0] := $C0 or (C shr 6);
      P[1] := $80 or (C and $3F);
      Inc(P, 2);
    end
    else
    begin
      if (C >= $D800) and (C <= $DBFF) and (I + 1 < ALength) and
        (Ord(ASource[I + 1]) >= $DC00) and (Ord(ASource[I + 1]) <= $DFFF) then
      begin
        D := Ord(ASource[I + 1]);
        C := $10000 + ((C - $D800) shl 10) + (D - $DC00);
        P[0] := $F0 or (C shr 18);
        P[1] := $80 or ((C shr 12) and $3F);
        P[2] := $80 or ((C shr 6) and $3F);
        P[3] := $80 or (C and $3F);
        Inc(P, 4);
        Inc(I, 2);
        Continue;
      end;
      if (C >= $D800) and (C <= $DFFF) then
        C := REPLACEMENT_CHARACTER;
      P[0] := $E0 or (C shr 12);
      P[1] := $80 or ((C shr 6) and $3F);
      P[2] := $80 or (C and $3F);
      Inc(P, 3);
    end;
    Inc(I);
  end;
end;

procedure UTF8ToUTF16(ASource: PByte; var ADest: UnicodeString; ALength: SizeInt);
var
  I, J, N, Need: SizeInt;
  C, Min: Cardinal;
  B: Byte;
  Q: PUnicodeChar;
  Valid: Boolean;
begin
  { at most a code unit a byte: the string sized for that, then cut to what it holds }
  SetLength(ADest, ALength);
  if ALength = 0 then
    Exit;
  Q := PUnicodeChar(Pointer(ADest));
  N := 0;
  I := 0;
  while I < ALength do
  begin
    B := ASource[I];
    if B < $80 then
    begin
      Q[N] := UnicodeChar(B);
      Inc(N);
      Inc(I);
      Continue;
    end;
    if (B and $E0) = $C0 then
    begin
      Need := 1; C := B and $1F; Min := $80;
    end
    else if (B and $F0) = $E0 then
    begin
      Need := 2; C := B and $0F; Min := $800;
    end
    else if (B and $F8) = $F0 then
    begin
      Need := 3; C := B and $07; Min := $10000;
    end
    else
    begin
      { a continuation byte or a byte no sequence starts with }
      Q[N] := UnicodeChar(REPLACEMENT_CHARACTER);
      Inc(N);
      Inc(I);
      Continue;
    end;
    Valid := I + Need < ALength;
    J := 1;
    while Valid and (J <= Need) do
    begin
      if (ASource[I + J] and $C0) <> $80 then
        Valid := False
      else
        C := (C shl 6) or (ASource[I + J] and $3F);
      Inc(J);
    end;
    if not Valid or (C < Min) or (C > $10FFFF) or ((C >= $D800) and (C <= $DFFF)) then
    begin
      { a truncated, overlong or out-of-range sequence: one replacement for its first byte }
      Q[N] := UnicodeChar(REPLACEMENT_CHARACTER);
      Inc(N);
      Inc(I);
      Continue;
    end;
    if C >= $10000 then
    begin
      Dec(C, $10000);
      Q[N] := UnicodeChar($D800 + (C shr 10));
      Q[N + 1] := UnicodeChar($DC00 + (C and $3FF));
      Inc(N, 2);
    end
    else
    begin
      Q[N] := UnicodeChar(C);
      Inc(N);
    end;
    Inc(I, Need + 1);
  end;
  SetLength(ADest, N);
end;

procedure Unicode2AnsiMove(ASource: PUnicodeChar; var ADest: RawByteString;
  ACodePage: TSystemCodePage; ALength: SizeInt);
var
  CodePage: TSystemCodePage;
  I, N, B: SizeInt;
  P: PAnsiChar;
begin
  CodePage := ResolveCodePage(ACodePage);
  if CodePage = CP_UTF8 then
    UTF16ToUTF8(ASource, ADest, ALength)
  else if (CodePage <> CP_ASCII) and (CodePage <> CP_LATIN1) and (CodePage <> CP_LATIN9) and
    IconvFromUTF16(ASource, ADest, CodePage, ALength) then
    { iconv's }
  else
  begin
    { as iconv does: a surrogate pair is one character ('?'), a lone surrogate is dropped }
    SetLength(ADest, ALength);
    P := PAnsiChar(Pointer(ADest));
    N := 0;
    I := 0;
    while I < ALength do
    begin
      B := Ord(ASource[I]);
      Inc(I);
      if (B >= $D800) and (B <= $DFFF) then
      begin
        if (B <= $DBFF) and (I < ALength) and (Ord(ASource[I]) >= $DC00) and (Ord(ASource[I]) <= $DFFF) then
        begin
          Inc(I);
          P[N] := '?';
          Inc(N);
        end;
        Continue;
      end;
      if CodePage = CP_LATIN9 then
        B := Latin9ByteOf(B)
      else if (CodePage = CP_LATIN1) and (B <= $FF) then
        { as it is }
      else if B > $7F then
        B := -1;
      if B < 0 then
        P[N] := '?'
      else
        P[N] := AnsiChar(B);
      Inc(N);
    end;
    SetLength(ADest, N);
  end;
  if Pointer(ADest) <> nil then
    SetCodePage(ADest, CodePage, False);
end;

procedure Ansi2UnicodeMove(ASource: PAnsiChar; ACodePage: TSystemCodePage;
  var ADest: UnicodeString; ALength: SizeInt);
var
  CodePage: TSystemCodePage;
  I: SizeInt;
  B: Byte;
  Q: PUnicodeChar;
begin
  CodePage := ResolveCodePage(ACodePage);
  if CodePage = CP_UTF8 then
  begin
    UTF8ToUTF16(PByte(ASource), ADest, ALength);
    Exit;
  end;
  if (CodePage <> CP_ASCII) and (CodePage <> CP_LATIN1) and (CodePage <> CP_LATIN9) and
    IconvToUTF16(ASource, CodePage, ADest, ALength) then
    Exit;
  SetLength(ADest, ALength);
  if ALength = 0 then
    Exit;
  Q := PUnicodeChar(Pointer(ADest));
  for I := 0 to ALength - 1 do
  begin
    B := Byte(ASource[I]);
    if CodePage = CP_LATIN9 then
      Q[I] := UnicodeChar(Latin9CharOf(B))
    else if (CodePage = CP_LATIN1) or (B <= $7F) then
      Q[I] := UnicodeChar(B)
    else
      Q[I] := UnicodeChar(REPLACEMENT_CHARACTER);
  end;
end;

procedure Wide2AnsiMove(ASource: PWideChar; var ADest: RawByteString;
  ACodePage: TSystemCodePage; ALength: SizeInt);
begin
  Unicode2AnsiMove(PUnicodeChar(ASource), ADest, ACodePage, ALength);
end;

procedure Ansi2WideMove(ASource: PAnsiChar; ACodePage: TSystemCodePage;
  var ADest: WideString; ALength: SizeInt);
var
  U: UnicodeString;
begin
  Ansi2UnicodeMove(ASource, ACodePage, U, ALength);
  ADest := U;
end;

function AsciiCase(const AText: UnicodeString; const AUpper: Boolean): UnicodeString;
var
  I: SizeInt;
  C: UnicodeChar;
begin
  Result := AText;
  UniqueString(Result);
  for I := 1 to Length(Result) do
  begin
    C := Result[I];
    if AUpper and (C >= 'a') and (C <= 'z') then
      Result[I] := UnicodeChar(Ord(C) - 32)
    else if not AUpper and (C >= 'A') and (C <= 'Z') then
      Result[I] := UnicodeChar(Ord(C) + 32);
  end;
end;

function UpperUnicodeString(const AText: UnicodeString): UnicodeString;
begin
  Result := AsciiCase(AText, True);
end;

function LowerUnicodeString(const AText: UnicodeString): UnicodeString;
begin
  Result := AsciiCase(AText, False);
end;

function UpperWideString(const AText: WideString): WideString;
begin
  Result := AsciiCase(AText, True);
end;

function LowerWideString(const AText: WideString): WideString;
begin
  Result := AsciiCase(AText, False);
end;

{ code unit order; coIgnoreCase folds ASCII letters }
function CompareUnicodeString(const A, B: UnicodeString; AOptions: TCompareOptions): PtrInt;
var
  I, N: SizeInt;
  CA, CB: Word;
begin
  N := Length(A);
  if Length(B) < N then
    N := Length(B);
  for I := 1 to N do
  begin
    CA := Ord(A[I]);
    CB := Ord(B[I]);
    if coIgnoreCase in AOptions then
    begin
      if (CA >= Ord('A')) and (CA <= Ord('Z')) then Inc(CA, 32);
      if (CB >= Ord('A')) and (CB <= Ord('Z')) then Inc(CB, 32);
    end;
    if CA <> CB then
      Exit(PtrInt(CA) - PtrInt(CB));
  end;
  Result := Length(A) - Length(B);
end;

function CompareWideString(const A, B: WideString; AOptions: TCompareOptions): PtrInt;
begin
  Result := CompareUnicodeString(A, B, AOptions);
end;

function GetStandardCodePage(const AStandardCodePage: TStandardCodePageEnum): TSystemCodePage;
begin
  Result := GProcessCodePage;
end;

procedure SetStdIOCodePage(var T: Text);
begin
  case TextRec(T).Mode of
    fmInput, fmOutput:
      TextRec(T).CodePage := GProcessCodePage;
  end;
end;

procedure InstallStringManager;
var
  Manager: TUnicodeStringManager;
begin
  GProcessCodePage := DetectProcessCodePage;
  GetUnicodeStringManager(Manager);
  Manager.Wide2AnsiMoveProc := @Wide2AnsiMove;
  Manager.Ansi2WideMoveProc := @Ansi2WideMove;
  Manager.Unicode2AnsiMoveProc := @Unicode2AnsiMove;
  Manager.Ansi2UnicodeMoveProc := @Ansi2UnicodeMove;
  Manager.UpperWideStringProc := @UpperWideString;
  Manager.LowerWideStringProc := @LowerWideString;
  Manager.UpperUnicodeStringProc := @UpperUnicodeString;
  Manager.LowerUnicodeStringProc := @LowerUnicodeString;
  Manager.CompareWideStringProc := @CompareWideString;
  Manager.CompareUnicodeStringProc := @CompareUnicodeString;
  Manager.GetStandardCodePageProc := @GetStandardCodePage;
  SetUnicodeStringManager(Manager);
  DefaultSystemCodePage := GProcessCodePage;
  DefaultFileSystemCodePage := GProcessCodePage;
  DefaultRTLFileSystemCodePage := GProcessCodePage;
  SetStdIOCodePage(Input);
  SetStdIOCodePage(Output);
  SetStdIOCodePage(ErrOutput);
  SetStdIOCodePage(StdOut);
  SetStdIOCodePage(StdErr);
end;

initialization
  InstallStringManager;
{$ENDIF}

end.
