program goccia;
{$mode delphiunicode}{$H+}
{$I Goccia.Checks.inc}
{$MINFPCONSTPREC 64}
uses
  {$IFDEF UNIX}cthreads,{$ENDIF} Classes, SysUtils, Math,
  Goccia.Lexer, Goccia.Parser, Goccia.AST.Node, Goccia.Token,
  Goccia.Core, Goccia.Instructions, Goccia.Compiler, Goccia.VM, Goccia.Scan, Goccia.PreParse, Goccia.Lazy, Goccia.Builtins, Goccia.Builtins2, Goccia.RegExpV, Goccia.Eval, Goccia.Builtins3, Goccia.Promise, Goccia.Builtins4, Goccia.Atomics, Goccia.Agent, Goccia.Engine, Goccia.ObjModel, Goccia.ArrayProto, Goccia.IterHelpers, Goccia.TypedArrays, Goccia.Disposal, Goccia.Modules, Goccia.Builtins5, Goccia.Temporal, Goccia.JSON, Goccia.Date, Goccia.Intl, Goccia.HostAPI, Goccia.Weak, Goccia.Embed, Goccia.Test262, CLI.ConfigFile, CLI.Units, StrUtils,
  Goccia.Capabilities, Goccia.CLI.Permissions, Goccia.Coverage;

type
  TSampler = class(TThread)
    procedure Execute; override;
  end;

var
  SampleTpl: array of Pointer;
  SampleCount: array of Integer;
  SampleN, SampleTotal, SampleGC: Integer;

procedure TSampler.Execute;
var T: Pointer; I, F: Integer;
begin
  while not Terminated do
  begin
    Sleep(1);
    Inc(SampleTotal);
    F := FrameTop;
    if (F <= 0) or (F > Length(Frames)) then Continue;
    T := Frames[F - 1].Tpl;
    if T = nil then Continue;
    I := 0;
    while (I < SampleN) and (SampleTpl[I] <> T) do Inc(I);
    if I = SampleN then
    begin
      if SampleN >= Length(SampleTpl) then begin SetLength(SampleTpl, SampleN * 2 + 256); SetLength(SampleCount, SampleN * 2 + 256); end;
      SampleTpl[I] := T; SampleCount[I] := 0; Inc(SampleN);
    end;
    Inc(SampleCount[I]);
  end;
end;

{ S as a JSON string literal }
function JsonQuote(const S: UnicodeString): UnicodeString;
var I: Integer; C: WideChar;
begin
  Result := '"';
  for I := 1 to Length(S) do
  begin
    C := S[I];
    case C of
      '"': Result := Result + '\"';
      '\': Result := Result + '\\';
      #10: Result := Result + '\n';
      #13: Result := Result + '\r';
      #9: Result := Result + '\t';
    else
      if Ord(C) < 32 then Result := Result + '\u' + UnicodeString(IntToHex(Ord(C), 4)) else Result := Result + C;
    end;
  end;
  Result := Result + '"';
end;

{ a line that opens with an import declaration (not import( or import.) or an export declaration }
function HasModuleSyntax(const Src: UnicodeString): Boolean;
var I, N, J: Integer; AtLineStart: Boolean;
  function Word(const W: UnicodeString): Boolean;
  begin
    Result := (Copy(Src, J, Length(W)) = W) and ((J + Length(W) > N) or not (Src[J + Length(W)] in ['A'..'Z', 'a'..'z', '0'..'9', '_', '$']));
  end;
begin
  Result := False;
  N := Length(Src);
  AtLineStart := True;
  I := 1;
  while I <= N do
  begin
    if AtLineStart then
    begin
      J := I;
      while (J <= N) and ((Src[J] = ' ') or (Src[J] = #9)) do Inc(J);
      if J <= N then
      begin
        if Word('export') then Exit(True);
        if Word('import') then
        begin
          J := J + 6;
          while (J <= N) and ((Src[J] = ' ') or (Src[J] = #9)) do Inc(J);
          if (J <= N) and (Src[J] <> '(') and (Src[J] <> '.') then Exit(True);
        end;
      end;
    end;
    AtLineStart := (Src[I] = #10);
    Inc(I);
  end;
end;

{ --test: the file's suites registered (Err: the uncaught error while registering them, '' for none), run them;
  their result as one JSON line on stdout, a throw while registering or running a suite error }
procedure ReportTests(const Err: UnicodeString);
var Json, E: UnicodeString;
begin
  if Err <> '' then E := Err
  else if RunTestSuites(Json, E) then
  begin
    WriteLn('__GOCCIATEST__ ', UTF8Encode(Json));
    Halt(0);
  end;
  WriteLn('__GOCCIATEST__ ', UTF8Encode('{"suiteError":' + JsonQuote(E) + '}'));
  Halt(0);
end;

{ --test: the language profile and source type of the nearest goccia.json (CLI.ConfigFile, as the CLIs find
  it); without one, the recommended profile }
procedure LoadTestConfig(const FileName: string; out Prof: TLangProfile; out SourceType: string;
  out ConfigJson: UnicodeString; out Caps: TGocciaCapabilities);
var Path, V, SizeErr, Scope: string; Entries: TConfigEntryArray; I, J: Integer; Done: TStringList;
  Vals: UnicodeString; Bytes: Int64; Req: TGocciaConfigPermissionRequest; Cap: TGocciaCapability;
  function Flag(const Key: string): Boolean;
  var V: string;
  begin
    Result := FindConfigEntry(Entries, Key, V) and (V = 'true');
  end;
begin
  Prof := Default(TLangProfile);
  Prof.Restricted := True;
  SourceType := '';
  SetLength(Entries, 0);
  Path := DiscoverConfigFile(ExtractFilePath(ExpandFileName(FileName)), ['goccia'], ['.json']);
  if Path <> '' then Entries := ParseConfigFile(Path);
  { the config's permissions, as GocciaTestRunner -P accepts them for the run }
  Caps := TGocciaCapabilities.None;
  if Path <> '' then
  begin
    Req := ReadConfigPermissionRequest(Entries, Path);
    for Cap := Low(TGocciaCapability) to High(TGocciaCapability) do
    begin
      if Req.Allow[Cap].Unscoped then Caps := Caps.Allow(Cap);
      for Scope in Req.Allow[Cap].Scopes do Caps := Caps.Allow(Cap, Scope);
      if Req.Deny[Cap].Unscoped then Caps := Caps.Deny(Cap);
      for Scope in Req.Deny[Cap].Scopes do Caps := Caps.Deny(Cap, Scope);
    end;
  end;
  Prof.Asi := Flag('compat-asi');
  Prof.VarDecl := Flag('compat-var');
  Prof.FuncDecl := Flag('compat-function');
  Prof.ForLoop := Flag('compat-traditional-for-loop');
  Prof.WhileLoop := Flag('compat-while-loops');
  Prof.LooseEq := Flag('compat-loose-equality');
  Prof.NonStrict := Flag('compat-non-strict-mode');
  Prof.ArgumentsObject := Flag('compat-arguments-object');
  Prof.Labels := Flag('compat-label');
  Prof.ForIn := Flag('compat-for-in-loop');
  Prof.AnnexB := Flag('compat-annex-b');
  Prof.WarningUnsupported := Flag('warning-unsupported-features');
  Prof.FunctionCtor := Flag('unsafe-function-constructor');
  Prof.ShadowRealm := Flag('unsafe-shadowrealm');
  Prof.StrictTypes := Flag('strict-types');
  if not FindConfigEntry(Entries, 'source-type', SourceType) then SourceType := '';
  { --max-memory wins over the file }
  if (HeapCeiling <= 0) and FindConfigEntry(Entries, 'max-memory', V) and TryParseByteSize(V, Bytes, SizeErr) and
     (Bytes > 0) then
    HeapCeiling := Bytes;
  { every key as a JSON string (an array's elements as a list), the first entry for a key winning }
  ConfigJson := '{';
  Done := TStringList.Create;
  try
    for I := 0 to High(Entries) do
    begin
      if (Entries[I].Kind in [cvkUnsupported, cvkObject]) or (Done.IndexOf(Entries[I].Key) >= 0) then Continue;
      Done.Add(Entries[I].Key);
      if Length(ConfigJson) > 1 then ConfigJson := ConfigJson + ',';
      if Entries[I].InArray or (Entries[I].Kind = cvkEmptyArray) then
      begin
        Vals := '';
        for J := I to High(Entries) do
          if (Entries[J].Key = Entries[I].Key) and Entries[J].InArray then
          begin
            if Vals <> '' then Vals := Vals + ',';
            Vals := Vals + JsonQuote(UTF8Decode(Entries[J].Value));
          end;
        ConfigJson := ConfigJson + JsonQuote(UTF8Decode(Entries[I].Key)) + ':[' + Vals + ']';
      end
      else ConfigJson := ConfigJson + JsonQuote(UTF8Decode(Entries[I].Key)) + ':' + JsonQuote(UTF8Decode(Entries[I].Value));
    end;
  finally
    Done.Free;
  end;
  ConfigJson := ConfigJson + '}';
end;

{ --test FILE: GocciaTestRunner's run of one test file. The engine with the test framework API (prelude/testing.js)
  and the host's globals, the file under its goccia.json profile as a script or a module (source-type, .mjs, or a
  top-level import or export declaration), then its suites }
procedure RunTestFile(const FileName: string; const SrcText: UnicodeString);
var Prof: TLangProfile; SourceType: string; Err, ConfigJson: UnicodeString; Engine: TGocciaEngine; R: TGocciaRunResult;
  Caps: TGocciaCapabilities;
begin
  LoadTestConfig(FileName, Prof, SourceType, ConfigJson, Caps);
  Engine := TGocciaEngine.Create;   { lives until ReportTests ends the process }
  Engine.Capabilities := Caps;
  { GOCCIA_COVERAGE=1: the file runs under coverage (its results must not change) }
  if GetEnvironmentVariable('GOCCIA_COVERAGE') = '1' then
  begin
    TGocciaCoverageTracker.Initialize;
    TGocciaCoverageTracker.Instance.Enabled := True;
    Engine.Coverage := True;
  end;
  Engine.Profile := Prof;
  Engine.ConfigJson := ConfigJson;
  Engine.TestMode := True;
  { --max-memory, else the file's config (LoadTestConfig) }
  if HeapCeiling > 0 then Engine.MaxMemory := HeapCeiling;
  if SourceType = 'module' then Engine.SourceKind := gskModule
  else if SourceType = 'script' then Engine.SourceKind := gskScript;
  Err := '';
  if not (Engine.PrepareHost(Err) and InstallTestingAPI(Err)) then
  begin
    WriteLn('__GOCCIATEST__ setup error: ', UTF8Encode(Err));
    Halt(4);
  end;
  SetActiveProfile(Prof);
  HoistTestMocks(FileName, SrcText);
  R := Engine.Run(FileName, SrcText);
  if R.Ok then Err := '' else Err := R.ErrorMessage;
  ReportTests(Err);
end;

procedure ReportSamples;
var I, J, Best: Integer; T: PFuncTemplate; Nm: UnicodeString;
begin
  WriteLn(ErrOutput, 'samples ', SampleTotal, ' in minor GC ', SampleGC);
  for J := 1 to 30 do
  begin
    Best := -1;
    for I := 0 to SampleN - 1 do
      if (SampleCount[I] > 0) and ((Best < 0) or (SampleCount[I] > SampleCount[Best])) then Best := I;
    if Best < 0 then Break;
    T := PFuncTemplate(SampleTpl[Best]);
    if T^.Name <> nil then Nm := StrToUnicode(T^.Name) else Nm := '<anon>';
    if Length(T^.Lines) > 0 then Nm := Nm + ' line ' + UnicodeString(IntToStr(T^.Lines[0]));
    WriteLn(ErrOutput, SampleCount[Best]:7, '  ', UTF8Encode(Nm));
    SampleCount[Best] := 0;
  end;
end;

type
  TWatchdog = class(TThread)
    Secs: Integer;
    procedure Execute; override;
  end;

procedure TWatchdog.Execute;
var I, L: Integer; Nm: UnicodeString;
begin
  Sleep(Secs * 1000);
  WriteLn(ErrOutput, 'watchdog: ', FrameTop, ' frames');
  for I := FrameTop - 1 downto Max(0, FrameTop - 25) do
  begin
    L := -1;
    if (Frames[I].Tpl <> nil) and (Frames[I].PC > 0) and (Frames[I].PC - 1 < Length(Frames[I].Tpl^.Lines)) then
      L := Frames[I].Tpl^.Lines[Frames[I].PC - 1];
    Nm := '?';
    if (Frames[I].Tpl <> nil) and (Frames[I].Tpl^.Name <> nil) then Nm := StrToUnicode(Frames[I].Tpl^.Name);
    WriteLn(ErrOutput, '  ', UTF8Encode(Nm), ' line ', L);
  end;
  Flush(ErrOutput);
  Halt(9);
end;

{ UTF-8 to UTF-16 in one pass. Any malformed sequence (overlong, surrogate,
  above U+10FFFF, truncated) sends the whole input to UTF8Decode, so its
  replacement behaviour is unchanged. }
{ the leading ASCII bytes of Src (sixteen at a time, at most N) widened into Dst: how many (a multiple of 16) }
{$IF defined(CPUX86_64) and defined(UNIX)}
function WidenAscii(Src: PByte; Dst: PWideChar; N: PtrInt): PtrInt; assembler; nostackframe;
asm
  xorl %eax, %eax
  pxor %xmm2, %xmm2
.Lwide:
  leaq 16(%rax), %rcx
  cmpq %rdx, %rcx
  ja .Lwdone
  movdqu (%rdi,%rax), %xmm0
  pmovmskb %xmm0, %ecx
  testl %ecx, %ecx
  jnz .Lwdone
  movdqa %xmm0, %xmm1
  punpcklbw %xmm2, %xmm0
  punpckhbw %xmm2, %xmm1
  movdqu %xmm0, (%rsi,%rax,2)
  movdqu %xmm1, 16(%rsi,%rax,2)
  addq $16, %rax
  jmp .Lwide
.Lwdone:
end;
{$ELSE}
function WidenAscii(Src: PByte; Dst: PWideChar; N: PtrInt): PtrInt;
begin
  Result := 0;
end;
{$ENDIF}

{$PUSH}{$OVERFLOWCHECKS OFF}{$RANGECHECKS OFF}
function DecodeSource(const Raw: RawByteString): UnicodeString;
var I, N, J, K: Integer; P: PByte; D: PWideChar; B, C: Cardinal; Q: QWord;
begin
  N := Length(Raw);
  SetLength(Result, N);
  if N = 0 then Exit;
  P := PByte(Raw); D := PWideChar(Result);
  I := 0; J := 0;
  while I < N do
  begin
    K := WidenAscii(@P[I], @D[J], N - I);
    Inc(I, K); Inc(J, K);
    { eight ASCII bytes at a time }
    while (I + 8 <= N) and (PQWord(@P[I])^ and QWord($8080808080808080) = 0) do
    begin
      Q := PQWord(@P[I])^;
      D[J] := WideChar(Q and $FF); D[J + 1] := WideChar((Q shr 8) and $FF);
      D[J + 2] := WideChar((Q shr 16) and $FF); D[J + 3] := WideChar((Q shr 24) and $FF);
      D[J + 4] := WideChar((Q shr 32) and $FF); D[J + 5] := WideChar((Q shr 40) and $FF);
      D[J + 6] := WideChar((Q shr 48) and $FF); D[J + 7] := WideChar(Q shr 56);
      Inc(I, 8); Inc(J, 8);
    end;
    if I >= N then Break;
    B := P[I];
    if B < $80 then begin D[J] := WideChar(B); Inc(I); Inc(J); Continue; end;
    if (B >= $C2) and (B <= $DF) and (I + 1 < N) and (P[I + 1] and $C0 = $80) then
    begin
      D[J] := WideChar(((B and $1F) shl 6) or (P[I + 1] and $3F)); Inc(I, 2); Inc(J);
    end
    else if (B >= $E0) and (B <= $EF) and (I + 2 < N) and (P[I + 1] and $C0 = $80) and (P[I + 2] and $C0 = $80) then
    begin
      C := ((B and $0F) shl 12) or ((P[I + 1] and $3F) shl 6) or (P[I + 2] and $3F);
      if (C < $800) or ((C >= $D800) and (C <= $DFFF)) then Exit(UTF8Decode(Raw));
      D[J] := WideChar(C); Inc(I, 3); Inc(J);
    end
    else if (B >= $F0) and (B <= $F4) and (I + 3 < N) and (P[I + 1] and $C0 = $80) and (P[I + 2] and $C0 = $80) and
      (P[I + 3] and $C0 = $80) then
    begin
      C := ((B and $07) shl 18) or ((P[I + 1] and $3F) shl 12) or ((P[I + 2] and $3F) shl 6) or (P[I + 3] and $3F);
      if (C < $10000) or (C > $10FFFF) then Exit(UTF8Decode(Raw));
      Dec(C, $10000);
      D[J] := WideChar($D800 + (C shr 10)); D[J + 1] := WideChar($DC00 + (C and $3FF)); Inc(I, 4); Inc(J, 2);
    end
    else
      Exit(UTF8Decode(Raw));
  end;
  SetLength(Result, J);
end;
{$POP}

{ GOCCIA_BCHASH: a digest of everything the compiler emits, to compare builds.
  Constants are hashed by their upper 16 bits (number or tag), since a heap
  address differs between runs. }
{ GOCCIA_PHASES: what the compiled program holds }
var StatTpl, StatCode, StatConsts, StatLines, StatICs, StatChildren: Int64;
procedure TemplateStats(T: PFuncTemplate);
var I: Integer;
begin
  Inc(StatTpl); Inc(StatCode, Length(T^.Code)); Inc(StatConsts, Length(T^.Consts)); Inc(StatLines, Length(T^.Lines));
  Inc(StatICs, Length(T^.ICs)); Inc(StatChildren, Length(T^.Children));
  for I := 0 to High(T^.Children) do TemplateStats(T^.Children[I]);
  if Assigned(T^.FieldInit) then TemplateStats(T^.FieldInit);
end;

{$PUSH}{$OVERFLOWCHECKS OFF}{$RANGECHECKS OFF}
function HashTemplate(T: PFuncTemplate; H: QWord): QWord;
var I: Integer;
  procedure Mix(V: QWord);
  begin
    H := (H xor V) * 1099511628211;
  end;
begin
  Mix(T^.CodeLen); Mix(T^.EntryPC); Mix(T^.NumParams); Mix(T^.MaxRegs); Mix(QWord(T^.RestReg)); Mix(QWord(T^.ArgsReg));
  Mix(QWord(T^.HomeReg)); Mix(QWord(T^.NewTargetReg)); Mix(Ord(T^.IsArrow) + 2 * Ord(T^.IsClassCtor) + 4 * Ord(T^.IsDerived) +
    8 * Ord(T^.IsMethod) + 16 * Ord(T^.IsGenerator) + 32 * Ord(T^.IsAsync) + 64 * Ord(T^.IsStrict));
  Mix(T^.Length); Mix(T^.ExpectedProps); Mix(Length(T^.ICs)); Mix(Length(T^.Upvals));
  for I := 0 to T^.CodeLen - 1 do Mix(T^.Code[I]);
  for I := 0 to T^.CodeLen - 1 do if I < Length(T^.Lines) then Mix(T^.Lines[I]);   { slack after CodeLen is not part of the program }
  Mix(Length(T^.Consts));
  for I := 0 to High(T^.Consts) do Mix(QWord(T^.Consts[I]) shr 48);
  for I := 0 to High(T^.Upvals) do begin Mix(Ord(T^.Upvals[I].FromParentLocal)); Mix(T^.Upvals[I].Index); end;
  Mix(Length(T^.Children));
  for I := 0 to High(T^.Children) do H := HashTemplate(T^.Children[I], H);
  if Assigned(T^.FieldInit) then H := HashTemplate(T^.FieldInit, H);
  Result := H;
end;
{$POP}

{ ---------------- test262 host (Goccia.Test262, GocciaScript grading contract 4) ----------------
  goccia --test262 [--strict] [--module] [--harness FILE]... TEST
  What the run printed, then its outcome as the last line: #$1E 'GocciaT262 ' and a JSON object with kind (ok, parse,
  throw, harness, compile, crash), name, message and file (Goccia.Test262's TGocciaT262Outcome). }
procedure RunTest262Host;
var I: Integer; Strict, IsModule: Boolean; Harness: array of string; TestFile: string; O: TGocciaT262Outcome;
begin
  Strict := False; IsModule := False; TestFile := '';
  I := 2;
  while I <= ParamCount do
  begin
    if ParamStr(I) = '--strict' then Strict := True
    else if ParamStr(I) = '--module' then IsModule := True
    else if (ParamStr(I) = '--harness') and (I < ParamCount) then
    begin
      Inc(I);
      SetLength(Harness, Length(Harness) + 1);
      Harness[High(Harness)] := ParamStr(I);
    end
    else TestFile := ParamStr(I);
    Inc(I);
  end;
  InitTest262Engine(GetEnvironmentVariable('GOCCIA_CANNOT_BLOCK') <> '');
  O := RunTest262Case(Harness, TestFile, Strict, IsModule);
  Write(UTF8Encode(Test262PrintText));
  WriteLn(#$1E'GocciaT262 {"kind":', UTF8Encode(JsonQuote(O.Kind)), ',"name":', UTF8Encode(JsonQuote(O.Name)),
    ',"message":', UTF8Encode(JsonQuote(O.Message)), ',"file":', UTF8Encode(JsonQuote(O.FileName)), '}');
  Flush(Output);
end;

var
  FS: TFileStream; Raw: RawByteString; SrcText: UnicodeString; PP: TPreParser; LazySrc: TLazySource; L: TGocciaLexer; P: TGocciaParser; Prog: TGocciaProgram; T: PFuncTemplate;
  Opts: TGocciaParserOptions; Ok, TestMode: Boolean; MaxMem, TimeoutMs, MaxInstr, MaxStack: Int64; SizeErr: string; S: TValue; FileName: string; I, NTokens: Integer; T0, T1, T2, T3, T4, H0, H1: QWord;
begin
  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);
  if ParamStr(1) = '--test262' then begin RunTest262Host; Halt(0); end;
  FileName := '';
  TestMode := False;
  TimeoutMs := 0; MaxInstr := 0; MaxStack := 0;
  for I := 1 to ParamCount do
    if ParamStr(I) = '--test' then TestMode := True
    else if ParamStr(I) = '--gc-stress' then GCStress := True
    else if ParamStr(I) = '--gc-verify' then GCVerify := True
    else if ParamStr(I) = '--trace-throw' then ThrowTrace := True
    else if StartsStr('--max-memory=', ParamStr(I)) then
    begin
      if not TryParseByteSize(Copy(ParamStr(I), Length('--max-memory=') + 1, MaxInt), MaxMem, SizeErr) or (MaxMem <= 0) then
      begin
        WriteLn(ErrOutput, 'Invalid value for --max-memory: ', ParamStr(I), ' (', SizeErr, ')');
        Halt(2);
      end;
      HeapCeiling := MaxMem;
    end
    else if StartsStr('--timeout=', ParamStr(I)) then
    begin
      if not TryParseDurationMilliseconds(Copy(ParamStr(I), Length('--timeout=') + 1, MaxInt), TimeoutMs, SizeErr) or
        (TimeoutMs < 0) then
      begin
        WriteLn(ErrOutput, 'Invalid value for --timeout: ', ParamStr(I), ' (', SizeErr, ')');
        Halt(2);
      end;
    end
    else if StartsStr('--max-instructions=', ParamStr(I)) then
    begin
      if not TryStrToInt64(Copy(ParamStr(I), Length('--max-instructions=') + 1, MaxInt), MaxInstr) or (MaxInstr < 0) then
      begin
        WriteLn(ErrOutput, 'Invalid value for --max-instructions: ', ParamStr(I));
        Halt(2);
      end;
    end
    else if StartsStr('--max-stack=', ParamStr(I)) then
    begin
      if not TryStrToInt64(Copy(ParamStr(I), Length('--max-stack=') + 1, MaxInt), MaxStack) or (MaxStack < 0) or
        (MaxStack > High(Integer)) then
      begin
        WriteLn(ErrOutput, 'Invalid value for --max-stack: ', ParamStr(I));
        Halt(2);
      end;
    end
    else FileName := ParamStr(I);
  if FileName = '' then
  begin
    WriteLn(ErrOutput, 'Usage: goccia [options] <file>');
    Halt(2);
  end;
  { as GocciaRunner reports it, not as an unhandled EFOpenError }
  if not FileExists(FileName) then
  begin
    WriteLn(ErrOutput, 'Error: Path not found: ', FileName);
    Halt(1);
  end;
  T0 := GetTickCount64;
  { the file's bytes, decoded once (a string list would decode, split and join the lines again) }
  try
    FS := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  except
    on E: EFOpenError do
    begin
      WriteLn(ErrOutput, 'Error: ', E.Message);
      Halt(1);
    end;
  end;
  try
    SetLength(Raw, FS.Size);
    if FS.Size > 0 then FS.ReadBuffer(Raw[1], FS.Size);
  finally
    FS.Free;
  end;
  if (Length(Raw) >= 3) and (Raw[1] = #$EF) and (Raw[2] = #$BB) and (Raw[3] = #$BF) then Delete(Raw, 1, 3);
  SrcText := DecodeSource(Raw);
  Raw := '';
  T1 := GetTickCount64;
  { GOCCIA_NURSERY_KB: GC stress for a test file's engine too (scripts/engine/gcstress-tests.py) }
  if GetEnvironmentVariable('GOCCIA_NURSERY_KB') <> '' then
    EngineNurseryBytes := StrToInt(GetEnvironmentVariable('GOCCIA_NURSERY_KB')) * 1024;
  if TestMode then RunTestFile(FileName, SrcText);
  { the functions the pre-parse finds eligible are compiled on their first call (Goccia.Lazy); GOCCIA_LAZY=0 compiles
    everything now }
  LazySrc := nil;
  if GetEnvironmentVariable('GOCCIA_LAZY') <> '0' then
  begin
    PP := TPreParser.Create(PWideChar(SrcText), Length(SrcText), pgScript, False);
    try
      try
        PP.ParseProgram;
        LazySrc := BuildLazySource(SrcText, FileName, PP);
      except
        on EScanError do LazySrc := nil;
      end;
    finally
      PP.Free;
    end;
    if GetEnvironmentVariable('GOCCIA_PHASES') <> '' then
      WriteLn(ErrOutput, 'pre-parse ', GetTickCount64 - T1, ' ms');
  end;
  L := TGocciaLexer.Create(SrcText, FileName);
  if LazySrc <> nil then
  begin
    LazySrc.Coords := L.SourceCoordinates;
    LazySrc.CoordsRef := LazySrc.Coords;
  end;
  L.HTMLComments := True;   { B.1.1: a Script }
  L.StandardIdentifiers := True;
  L.MeasureScanTime := GetEnvironmentVariable('GOCCIA_PHASES') <> '';
  P := TGocciaParser.CreateFromLexer(L, FileName, nil);   { source lines: built only if an error needs them }
  Opts := P.Options;
  Opts.AutomaticSemicolonInsertion := True;
  Opts.VarDeclarationsEnabled := True;
  Opts.FunctionDeclarationsEnabled := True;
  Opts.TraditionalForLoopsEnabled := True;
  Opts.WhileLoopsEnabled := True;
  Opts.LooseEqualityEnabled := True;
  Opts.NonStrictModeEnabled := True;
  Opts.LabelStatementsEnabled := True;
  Opts.ForInLoopsEnabled := True;
  P.ApplyOptions(Opts);
  if LazySrc <> nil then
  begin
    P.SetFunctionBodySkipper(@LazyBodySkipper, LazySrc);
    P.SetEarlyErrorsChecked(True);
  end;
  T2 := GetTickCount64;
  try
    Prog := P.Parse;
  except
    on E: Exception do begin WriteLn('parse error: ', E.Message); Halt(2); end;
  end;
  { the AST holds no token (but spans point at the lexer's coordinates): free the tokens before compiling, whose
    allocations then reuse their memory. typescript: peak 181 -> 156 MB parse-only, 208 -> 171 MB end to end, +1.5% instructions.
    GOCCIA_KEEPTOKENS keeps them. }
  NTokens := L.Tokens.Count;
  H0 := GetFPCHeapStatus.CurrHeapUsed;
  { only the tokens: the lexer owns the source coordinates every AST span points at }
  if GetEnvironmentVariable('GOCCIA_KEEPTOKENS') = '' then L.Tokens.Clear;
  H1 := GetFPCHeapStatus.CurrHeapUsed;
  if GetEnvironmentVariable('GOCCIA_NURSERY_KB') <> '' then HeapInit(StrToInt(GetEnvironmentVariable('GOCCIA_NURSERY_KB')) * 1024)
  else if GetEnvironmentVariable('GOCCIA_NURSERY_MB') <> '' then HeapInit(StrToInt(GetEnvironmentVariable('GOCCIA_NURSERY_MB')) * 1024 * 1024)
  else HeapInit(8 * 1024 * 1024);
  VMInit;
  RootCheck := GetEnvironmentVariable('GOCCIA_ROOTCHECK') <> '';
  if GetEnvironmentVariable('GOCCIA_SAMPLE') <> '' then
    with TSampler.Create(False) do FreeOnTerminate := True;
  if GetEnvironmentVariable('GOCCIA_WATCHDOG') <> '' then
    with TWatchdog.Create(True) do begin Secs := StrToInt(GetEnvironmentVariable('GOCCIA_WATCHDOG')); FreeOnTerminate := True; Start; end;
  InitIntrinsics;
  { GOCCIA_HOST: the hidden GocciaHost object (GocciaHost.gc() collects now) for scripts/engine/regress, as test262 runs have it }
  if GetEnvironmentVariable('GOCCIA_HOST') <> '' then InstallHostHooks;
  { test262 hosts: $262agent (Goccia.Agent), and a main agent that may not block (flag CanBlockIsFalse) }
  if GetEnvironmentVariable('GOCCIA_AGENT') <> '' then InitAgentHost;
  AgentCannotSuspend := GetEnvironmentVariable('GOCCIA_CANNOT_BLOCK') <> '';
  T3 := GetTickCount64;
  if GetEnvironmentVariable('GOCCIA_PHASES') <> '' then
    WriteLn(ErrOutput, 'after parse: tokens ', NTokens, ' (', SizeOf(TGocciaTokenRec), ' bytes each, ', (H0 - H1) div 1024,
      ' KB freed) heap used ', GetFPCHeapStatus.CurrHeapUsed div 1024, ' KB');
  if GetEnvironmentVariable('GOCCIA_ASTSTATS') <> '' then
    WriteLn(ErrOutput, Prog.NodeArena.ClassStats(30));
  if GetEnvironmentVariable('GOCCIA_MEMSPLIT') <> '' then
  begin
    H0 := GetFPCHeapStatus.CurrHeapUsed + (H0 - H1);
    H1 := GetFPCHeapStatus.CurrHeapUsed;
    Prog.Free;
    WriteLn(ErrOutput, 'tokens ', (H0 - H1) div 1024, ' KB; AST ', (H1 - GetFPCHeapStatus.CurrHeapUsed) div 1024, ' KB; left ',
      GetFPCHeapStatus.CurrHeapUsed div 1024, ' KB');
    Halt(0);
  end;
  try
    CurLazySource := LazySrc;
    T := CompileProgram(Prog, FileName, False, 1);
    CurLazySource := nil;
  except
    on E: Exception do begin WriteLn('compile error: ', E.Message); Halt(3); end;
  end;
  T4 := GetTickCount64;
  { The templates hold nothing of the AST. Its nodes' chunks go back to the heap without the destructors, which
    for typescript cost 264 M instructions (+9% of parsing and compiling); what nodes own outside the chunks
    stays. The program object is never freed after this. GOCCIA_FREEAST destroys the tree properly instead,
    GOCCIA_KEEPAST keeps it. }
  if GetEnvironmentVariable('GOCCIA_FREEAST') <> '' then Prog.Free
  else if GetEnvironmentVariable('GOCCIA_KEEPAST') = '' then Prog.NodeArena.AbandonMemory;
  if GetEnvironmentVariable('GOCCIA_BCHASH') <> '' then
  begin
    WriteLn(ErrOutput, 'bytecode hash ', IntToHex(HashTemplate(T, 1469598103934665603), 16));
    Halt(0);
  end;
  if GetEnvironmentVariable('GOCCIA_PHASES') <> '' then
  begin
    WriteLn(ErrOutput, 'after compile: heap used ', GetFPCHeapStatus.CurrHeapUsed div 1024, ' KB');
    TemplateStats(T);
    WriteLn(ErrOutput, 'templates ', StatTpl, '; code ', StatCode * 8 div 1024, ' KB (capacity); consts ', StatConsts * 8 div 1024,
      ' KB; lines ', StatLines * 4 div 1024, ' KB; ICs ', StatICs, ' x ', SizeOf(TPropIC), ' B = ', StatICs * SizeOf(TPropIC) div 1024, ' KB');
  end;
  SetMaxFrames(MaxStack);
  LimitsArm(TimeoutMs, MaxInstr);
  Ok := RunScript(T);
  if Ok then Ok := RunMicrotasks;
  if Ok then Ok := RunHostLoop;   { Atomics.waitAsync }
  { FinalizationRegistry cleanup jobs, each followed by the microtasks it queued }
  while Ok and WeakCleanupPending do
  begin
    ClearKeptObjects;
    Ok := RunWeakCleanup and RunMicrotasks;
  end;
  if GetEnvironmentVariable('GOCCIA_OPSTATS') <> '' then
  begin
    for I := 0 to 255 do if OpCounts[I] > 0 then WriteLn(ErrOutput, 'op ', I, ' ', OpCounts[I]);
    for I := 0 to 65535 do if PairCounts[I] > 0 then WriteLn(ErrOutput, 'pair ', I shr 8, ' ', I and 255, ' ', PairCounts[I]);
  end;
  if GetEnvironmentVariable('GOCCIA_ICSTATS') <> '' then
    WriteLn(ErrOutput, 'get ICs: hit entry0 ', ICHit0, ' hit other ', ICHitOther, ' misses ', ICMissN, ' (filled ', ICMissFill,
      ') holder mismatches ', ICMissDepthFail, ' why: dict-recv ', ICWhy[0], ' index ', ICWhy[1], ' absent ', ICWhy[2],
      ' accessor ', ICWhy[3], ' virtual ', ICWhy[4], ' dict/deep-proto ', ICWhy[5], ' proxy ', ICWhy[6], ' fn-proto ', ICWhy[7]);
  if GetEnvironmentVariable('GOCCIA_PHASES') <> '' then
    WriteLn(ErrOutput, 'phases ms: load ', T1 - T0, ' lexer+parser setup ', T2 - T1, ' parse ', T3 - T2, ' compile ', T4 - T3,
      ' run ', GetTickCount64 - T4, ' (lexer scanning inside parse: ', L.ScanTimeNanoseconds div 1000000,
      '; lazily compiled: ', LazyCompileCount, ' functions in ', LazyCompileNs div 1000000, ' ms: setup ',
      LazySetupNs div 1000000, ' parse ', LazyParseNs div 1000000, ', ', LazyTokens, ' tokens, ', LazyChars,
      ' chars)');
  if GetEnvironmentVariable('GOCCIA_SAMPLE') <> '' then ReportSamples;
  { a spent limit, as GocciaRunner reports it }
  if AbortKind = 1 then
  begin
    WriteLn('Fatal error: file timed out after ', TimeoutMs, 'ms');
    Halt(1);
  end;
  if AbortKind = 2 then
  begin
    WriteLn('Fatal error: Execution exceeded instruction limit of ', MaxInstr);
    Halt(1);
  end;
  if AbortKind = 3 then
  begin
    WriteLn('Fatal error: ', UTF8Encode(MemoryLimitMessage));
    Halt(1);
  end;
  LimitsDisarm;
  if not Ok then
  begin
    if ToStringV(Pending, S) and IsString(S) then WriteLn('Uncaught ', UTF8Encode(StrToUnicode(StrOf(S))))
    else WriteLn('Uncaught exception');
    Halt(1);
  end;
  if GetEnvironmentVariable('GOCCIA_GCSTATS') <> '' then
    WriteLn(ErrOutput, 'gc: minor ', GCMinorCount, ' major ', GCMajorCount, ' promoted ', GCPromotedBytes div 1024, ' KB old ', GCOldBytes div 1024, ' KB');
  if GetEnvironmentVariable('GOCCIA_GCSTATS') <> '' then
    with GetFPCHeapStatus do
      WriteLn(ErrOutput, 'heap: maxused ', MaxHeapUsed div 1024, ' KB maxsize ', MaxHeapSize div 1024, ' KB cur ', CurrHeapUsed div 1024, ' KB');
end.
