program g2;
{$mode delphiunicode}{$H+}
{$MINFPCONSTPREC 64}
uses
  cthreads, Classes, SysUtils, Math,
  Goccia.Lexer, Goccia.Parser, Goccia.AST.Node, Goccia.Token,
  G2.Core, G2.Bytecode, G2.Compiler, G2.VM, G2.Scan, G2.PreParse, G2.Lazy, G2.Builtins, G2.Builtins2, G2.RegExpV, G2.Eval, G2.Builtins3, G2.Promise, G2.Builtins4, G2.Atomics, G2.Agent, G2.Engine, G2.ObjModel, G2.ArrayProto, G2.IterHelpers, G2.TypedArrays, G2.Disposal, G2.Modules, G2.Builtins5, G2.Temporal, G2.JSON, G2.Date, G2.Intl;

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
{$PUSH}{$OVERFLOWCHECKS OFF}{$RANGECHECKS OFF}
function DecodeSource(const Raw: RawByteString): UnicodeString;
var I, N, J: Integer; P: PByte; D: PWideChar; B, C: Cardinal; Q: QWord;
begin
  N := Length(Raw);
  SetLength(Result, N);
  if N = 0 then Exit;
  P := PByte(Raw); D := PWideChar(Result);
  I := 0; J := 0;
  while I < N do
  begin
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

{ G2_BCHASH: a digest of everything the compiler emits, to compare builds.
  Constants are hashed by their upper 16 bits (number or tag), since a heap
  address differs between runs. }
{ G2_PHASES: what the compiled program holds }
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

{ ---------------- test262 host (GocciaScript grading contract 4) ----------------
  g2 --test262 [--strict] [--module] [--harness FILE]... TEST
  Each harness file runs as its own non-strict global Script in one realm, then the test file, prefixed with
  '"use strict";' and a newline in a strict run. The last line printed is the outcome:
  #$1E 'G2T262 ' and a JSON object: kind (ok, parse, throw, harness, compile, unsupported), name, message, file.
  The escaped value's name is its constructor's name, read from data properties only (no getter runs). }

function JsonStr(const S: UnicodeString): string;
var I: Integer; C: WideChar; R: UnicodeString;
begin
  R := '"';
  for I := 1 to Length(S) do
  begin
    C := S[I];
    case C of
      '"': R := R + '\"';
      '\': R := R + '\\';
      #10: R := R + '\n';
      #13: R := R + '\r';
      #9: R := R + '\t';
    else
      if Ord(C) < 32 then R := R + '\u' + UnicodeString(IntToHex(Ord(C), 4)) else R := R + C;
    end;
  end;
  Result := UTF8Encode(R + '"');
end;

procedure Outcome(const Kind, Name, Msg, F: UnicodeString);
begin
  WriteLn(#$1E'G2T262 {"kind":', JsonStr(Kind), ',"name":', JsonStr(Name), ',"message":', JsonStr(Copy(Msg, 1, 400)),
    ',"file":', JsonStr(F), '}');
  Flush(Output);
  Halt(0);
end;

{ Key along O's prototype chain as a data property; False at an accessor, a proxy or the end of the chain }
function DataProp(O: TValue; Key: PStr; var V: TValue): Boolean;
var P: PObj; S: Integer;
begin
  while IsObject(O) do
  begin
    P := ObjOf(O);
    if P^.OKind = okProxy then Exit(False);
    S := ShapeLookup(P^.Shape, Key);
    if S >= 0 then
    begin
      if (P^.Shape^.PFlags[S] and pfAccessor) <> 0 then Exit(False);
      V := GetSlot(P, S);
      Exit(True);
    end;
    O := ProtoOf(P);
  end;
  Result := False;
end;

procedure ThrownOutcomeKind(E: TValue; const Kind: UnicodeString);
var C, N, M, S: TValue; Name, Msg: UnicodeString;
begin
  Name := ''; Msg := '';
  if IsObject(E) then
  begin
    if DataProp(E, AtomConstructor, C) and IsObject(C) then
    begin
      { G2 keeps a function's own name outside its shape (read through GetV, no getter involved) }
      if IsCallable(C) and (ShapeLookup(ObjOf(C)^.Shape, AtomName) < 0) then
      begin
        if GetV(C, AtomName, N) and IsString(N) then Name := StrToUnicode(StrOf(N));
        Pending := VUndefined;
      end
      else if DataProp(C, AtomName, N) and IsString(N) then Name := StrToUnicode(StrOf(N));
    end;
    if DataProp(E, AtomMessage, M) and IsString(M) then Msg := StrToUnicode(StrOf(M));
  end
  else
  begin
    Pending := VUndefined;
    if ToStringV(E, S) and IsString(S) then Msg := StrToUnicode(StrOf(S));
  end;
  Outcome(Kind, Name, Msg, '');
end;

procedure ThrownOutcome(E: TValue);
begin
  ThrownOutcomeKind(E, 'throw');
end;

function ReadSourceFile(const Path: string; var Src: UnicodeString): Boolean;
var FS: TFileStream; Raw: RawByteString;
begin
  Result := False;
  try
    FS := TFileStream.Create(Path, fmOpenRead or fmShareDenyWrite);
    try
      SetLength(Raw, FS.Size);
      if FS.Size > 0 then FS.ReadBuffer(Raw[1], FS.Size);
    finally
      FS.Free;
    end;
  except
    Exit;
  end;
  if (Length(Raw) >= 3) and (Raw[1] = #$EF) and (Raw[2] = #$BB) and (Raw[3] = #$BF) then Delete(Raw, 1, 3);
  Src := DecodeSource(Raw);
  Result := True;
end;

{ G2Host.detachArrayBuffer(buffer): $262.detachArrayBuffer (25.1.3.5 DetachArrayBuffer) }
function Host_HTMLDDA(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
begin
  Res := VNull;
  Result := True;
end;

function Host_Detach(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
begin
  if Argc < 1 then Exit(DetachArrayBufferV(VUndefined));
  Result := DetachArrayBufferV(Args[1]);
  Res := VNull;
end;

{ G2Host.evalScript(source): $262.evalScript, a global Script run in this realm }
function Host_EvalScript(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var T: PFuncTemplate; Err: UnicodeString; Clo, S: TValue;
begin
  if Argc >= 1 then S := Args[1] else S := VUndefined;
  if not ToStringV(S, S) then Exit(False);
  { the Script's completion value is the result (ScriptEvaluation step 13) }
  if not ParseAndCompile(StrToUnicode(StrOf(S)), '<evalScript>', True, T, Err, 1) then
  begin
    if CompileFailKind = 1 then Exit(ThrowError(ParseErrorName, Err));
    Exit(ThrowError('SyntaxError', 'G2 compile: ' + Err));
  end;
  if not MakeScriptClosure(T, Clo) then Exit(False);
  Result := CallValue(Clo, GlobalObj, nil, 0, Res);
end;

function Host_GC(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
begin
  { a full collection, as the allocator runs one: the nursery first }
  CollectMinor;
  CollectMajor;
  Res := VUndefined;
  Result := True;
end;

var
  { the $262.js harness file, which createRealm runs in each new realm }
  HostHarnessSrc: UnicodeString;

function Host_CreateRealm(Args: PValue; Argc: Integer; var Res: TValue): Boolean; forward;

{ G2Host.pumpAsyncWaits(): a timer tick of the host's event loop settles the Atomics.waitAsync waits that are due }
function Host_PumpAsyncWaits(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
begin
  PumpAsyncWaits;
  Res := VUndefined;
  Result := True;
end;

{ the hidden G2Host object $262.js reads, in the running realm }
procedure InstallHostHooks;
var H: TValue;
begin
  H := MkPtr(NewPlainObject(ObjectProto, 8));
  DefineOwn(GlobalObj, AtomOf('G2Host'), H, pfHidden);
  { H is re-read after each allocation: it may move }
  GetV(GlobalObj, AtomOf('G2Host'), H);
  DefineNative(H, 'evalScript', Host_EvalScript, 1);
  GetV(GlobalObj, AtomOf('G2Host'), H);
  DefineNative(H, 'gc', Host_GC, 0);
  GetV(GlobalObj, AtomOf('G2Host'), H);
  DefineNative(H, 'detachArrayBuffer', Host_Detach, 1);
  GetV(GlobalObj, AtomOf('G2Host'), H);
  DefineNative(H, 'createRealm', Host_CreateRealm, 0);
  GetV(GlobalObj, AtomOf('G2Host'), H);
  DefineNative(H, 'pumpAsyncWaits', Host_PumpAsyncWaits, 0);
  { B.3.6 $262.IsHTMLDDA: a callable object that returns null and emulates undefined }
  GetV(GlobalObj, AtomOf('G2Host'), H);
  DefineNative(H, 'IsHTMLDDA', Host_HTMLDDA, 0);
  GetV(GlobalObj, AtomOf('G2Host'), H);
  GetV(H, AtomOf('IsHTMLDDA'), H);
  PCell(PtrOf(H))^.Flags := PCell(PtrOf(H))^.Flags or cfEmulatesUndefined;
  ObjOf(H)^.OFlags := ObjOf(H)^.OFlags and not 32;   { not a constructor }  { $262.AbstractModuleSource: the realm's %AbstractModuleSource% }
  PushRoot(AbstractModuleSourceIntrinsic);
  GetV(GlobalObj, AtomOf('G2Host'), H);
  DefineOwn(H, AtomOf('AbstractModuleSource'), RootAt(TempTop - 1), pfHidden);
  PopRoots(1);
end;

{ G2Host.createRealm(): $262.createRealm, the $262 object of a new realm }
function Host_CreateRealm(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var R, Old: Integer; T: PFuncTemplate; Err: UnicodeString; Clo: TValue;
begin
  if HostHarnessSrc = '' then Exit(ThrowTypeError('createRealm needs the $262.js harness'));
  if not CreateRealm(R) then Exit(False);
  Old := CurRealm;
  SwitchRealm(R);
  InstallHostHooks;
  Result := ParseAndCompile(HostHarnessSrc, '$262.js', False, T, Err, 1);
  if not Result then ThrowError('SyntaxError', Err)
  else Result := MakeScriptClosure(T, Clo) and CallValue(Clo, GlobalObj, nil, 0, Res) and
    GetV(GlobalObj, AtomOf('$262'), Res);
  SwitchRealm(Old);
end;

procedure RunTest262Host;
var I: Integer; Strict, IsModule: Boolean; Harness: array of string; TestFile: string; Src, Err: UnicodeString;
  T: PFuncTemplate; H, Agent, LogFn: TValue; Phase: Integer;
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
  { G2_NURSERY_KB and G2_GC_VERIFY: the GC stress settings of scripts/g2/gcstress.sh for test262 runs }
  GCVerify := GetEnvironmentVariable('G2_GC_VERIFY') <> '';
  if GetEnvironmentVariable('G2_NURSERY_KB') <> '' then EngineInit(StrToInt(GetEnvironmentVariable('G2_NURSERY_KB')) * 1024)
  else EngineInit;
  AgentCannotSuspend := GetEnvironmentVariable('G2_CANNOT_BLOCK') <> '';
  InitAgentHost;
  { the host object $262.js reads: the agent API moves into it, so $262 stays the only harness global }
  InstallHostHooks;
  if GetV(GlobalObj, AtomOf('$262agent'), Agent) then
  begin
    GetV(GlobalObj, AtomOf('G2Host'), H);
    DefineOwn(H, AtomOf('agent'), Agent, pfDefault);
    DeleteProperty(ObjOf(GlobalObj), AtomOf('$262agent'));
  end;
  { print, which doneprintHandle.js reports through }
  if GetV(GlobalObj, AtomOf('console'), LogFn) and GetV(LogFn, AtomOf('log'), LogFn) then
    DefineOwn(GlobalObj, AtomOf('print'), LogFn, pfHidden);
  { a Pascal exception in the engine (an engine bug) grades the run as a failure, as upstream's runner does;
    only a signal the runtime cannot turn into an exception kills the host (wrapper-infra) }
  try
  for I := 0 to High(Harness) do
  begin
    if not ReadSourceFile(Harness[I], Src) then Outcome('harness', '', 'cannot read', UnicodeString(Harness[I]));
    if ExtractFileName(Harness[I]) = '$262.js' then HostHarnessSrc := Src;
    if not ParseAndCompile(Src, UnicodeString(Harness[I]), False, T, Err, 1) then
      Outcome('harness', '', Err, UnicodeString(Harness[I]));
    if not (RunScript(T) and RunMicrotasks) then
    begin
      Err := '';
      if IsObject(Pending) and DataProp(Pending, AtomMessage, H) and IsString(H) then Err := StrToUnicode(StrOf(H));
      Outcome('harness', '', Err, UnicodeString(Harness[I]));
    end;
  end;
  if IsModule then
  begin
    { a module test: parse and link errors are early (resolution) errors, the rest are evaluation errors }
    if not RunModuleFile(UnicodeString(TestFile), Phase) then
    begin
      if Phase = 0 then ThrownOutcomeKind(Pending, 'parse');
      ThrownOutcome(Pending);
    end;
    if not (RunMicrotasks and RunHostLoop) then ThrownOutcome(Pending);
    Outcome('ok', '', '', '');
  end;
  if not ReadSourceFile(TestFile, Src) then Outcome('harness', '', 'cannot read test', UnicodeString(TestFile));
  if Strict then Src := '"use strict";'#10 + Src;
  if not ParseAndCompile(Src, UnicodeString(TestFile), False, T, Err, 1) then
  begin
    if CompileFailKind = 1 then Outcome('parse', ParseErrorName, Err, '');
    Outcome('compile', '', Err, '');
  end;
  if not (RunScript(T) and RunMicrotasks and RunHostLoop) then ThrownOutcome(Pending);
  except
    on E: Exception do Outcome('crash', UnicodeString(E.ClassName), UnicodeString(E.Message), '');
  end;
  Outcome('ok', '', '', '');
end;

var
  FS: TFileStream; Raw: RawByteString; SrcText: UnicodeString; PP: TPreParser; LazySrc: TLazySource; L: TGocciaLexer; P: TGocciaParser; Prog: TGocciaProgram; T: PFuncTemplate;
  Opts: TGocciaParserOptions; Ok: Boolean; S: TValue; FileName: string; I, NTokens: Integer; T0, T1, T2, T3, T4, H0, H1: QWord;
begin
  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);
  if ParamStr(1) = '--test262' then begin RunTest262Host; Halt(0); end;
  FileName := '';
  for I := 1 to ParamCount do
    if ParamStr(I) = '--gc-stress' then GCStress := True
    else if ParamStr(I) = '--gc-verify' then GCVerify := True
    else if ParamStr(I) = '--trace-throw' then ThrowTrace := True
    else FileName := ParamStr(I);
  T0 := GetTickCount64;
  { the file's bytes, decoded once (a string list would decode, split and join the lines again) }
  FS := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
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
  { the functions the pre-parse finds eligible are compiled on their first call (G2.Lazy); G2_LAZY=0 compiles
    everything now }
  LazySrc := nil;
  if GetEnvironmentVariable('G2_LAZY') <> '0' then
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
    if GetEnvironmentVariable('G2_PHASES') <> '' then
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
  L.MeasureScanTime := GetEnvironmentVariable('G2_PHASES') <> '';
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
    G2_KEEPTOKENS keeps them. }
  NTokens := L.Tokens.Count;
  H0 := GetFPCHeapStatus.CurrHeapUsed;
  { only the tokens: the lexer owns the source coordinates every AST span points at }
  if GetEnvironmentVariable('G2_KEEPTOKENS') = '' then L.Tokens.Clear;
  H1 := GetFPCHeapStatus.CurrHeapUsed;
  if GetEnvironmentVariable('G2_NURSERY_KB') <> '' then HeapInit(StrToInt(GetEnvironmentVariable('G2_NURSERY_KB')) * 1024)
  else if GetEnvironmentVariable('G2_NURSERY_MB') <> '' then HeapInit(StrToInt(GetEnvironmentVariable('G2_NURSERY_MB')) * 1024 * 1024)
  else HeapInit(8 * 1024 * 1024);
  VMInit;
  RootCheck := GetEnvironmentVariable('G2_ROOTCHECK') <> '';
  if GetEnvironmentVariable('G2_SAMPLE') <> '' then
    with TSampler.Create(False) do FreeOnTerminate := True;
  if GetEnvironmentVariable('G2_WATCHDOG') <> '' then
    with TWatchdog.Create(True) do begin Secs := StrToInt(GetEnvironmentVariable('G2_WATCHDOG')); FreeOnTerminate := True; Start; end;
  InitIntrinsics;
  { test262 hosts: $262agent (G2.Agent), and a main agent that may not block (flag CanBlockIsFalse) }
  if GetEnvironmentVariable('G2_AGENT') <> '' then InitAgentHost;
  AgentCannotSuspend := GetEnvironmentVariable('G2_CANNOT_BLOCK') <> '';
  T3 := GetTickCount64;
  if GetEnvironmentVariable('G2_PHASES') <> '' then
    WriteLn(ErrOutput, 'after parse: tokens ', NTokens, ' (', SizeOf(TGocciaTokenRec), ' bytes each, ', (H0 - H1) div 1024,
      ' KB freed) heap used ', GetFPCHeapStatus.CurrHeapUsed div 1024, ' KB');
  if GetEnvironmentVariable('G2_ASTSTATS') <> '' then
    WriteLn(ErrOutput, Prog.NodeArena.ClassStats(30));
  if GetEnvironmentVariable('G2_MEMSPLIT') <> '' then
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
    stays. The program object is never freed after this. G2_FREEAST destroys the tree properly instead,
    G2_KEEPAST keeps it. }
  if GetEnvironmentVariable('G2_FREEAST') <> '' then Prog.Free
  else if GetEnvironmentVariable('G2_KEEPAST') = '' then Prog.NodeArena.AbandonMemory;
  if GetEnvironmentVariable('G2_BCHASH') <> '' then
  begin
    WriteLn(ErrOutput, 'bytecode hash ', IntToHex(HashTemplate(T, 1469598103934665603), 16));
    Halt(0);
  end;
  if GetEnvironmentVariable('G2_PHASES') <> '' then
  begin
    WriteLn(ErrOutput, 'after compile: heap used ', GetFPCHeapStatus.CurrHeapUsed div 1024, ' KB');
    TemplateStats(T);
    WriteLn(ErrOutput, 'templates ', StatTpl, '; code ', StatCode * 8 div 1024, ' KB (capacity); consts ', StatConsts * 8 div 1024,
      ' KB; lines ', StatLines * 4 div 1024, ' KB; ICs ', StatICs, ' x ', SizeOf(TPropIC), ' B = ', StatICs * SizeOf(TPropIC) div 1024, ' KB');
  end;
  Ok := RunScript(T);
  if Ok then Ok := RunMicrotasks;
  if Ok then Ok := RunHostLoop;   { Atomics.waitAsync }
  if GetEnvironmentVariable('G2_OPSTATS') <> '' then
  begin
    for I := 0 to 255 do if OpCounts[I] > 0 then WriteLn(ErrOutput, 'op ', I, ' ', OpCounts[I]);
    for I := 0 to 65535 do if PairCounts[I] > 0 then WriteLn(ErrOutput, 'pair ', I shr 8, ' ', I and 255, ' ', PairCounts[I]);
  end;
  if GetEnvironmentVariable('G2_ICSTATS') <> '' then
    WriteLn(ErrOutput, 'get ICs: hit entry0 ', ICHit0, ' hit other ', ICHitOther, ' misses ', ICMissN, ' (filled ', ICMissFill,
      ') holder mismatches ', ICMissDepthFail, ' why: dict-recv ', ICWhy[0], ' index ', ICWhy[1], ' absent ', ICWhy[2],
      ' accessor ', ICWhy[3], ' virtual ', ICWhy[4], ' dict/deep-proto ', ICWhy[5], ' proxy ', ICWhy[6], ' fn-proto ', ICWhy[7]);
  if GetEnvironmentVariable('G2_PHASES') <> '' then
    WriteLn(ErrOutput, 'phases ms: load ', T1 - T0, ' lexer+parser setup ', T2 - T1, ' parse ', T3 - T2, ' compile ', T4 - T3,
      ' run ', GetTickCount64 - T4, ' (lexer scanning inside parse: ', L.ScanTimeNanoseconds div 1000000,
      '; lazily compiled: ', LazyCompileCount, ' functions in ', LazyCompileNs div 1000000, ' ms: setup ',
      LazySetupNs div 1000000, ' parse ', LazyParseNs div 1000000, ', ', LazyTokens, ' tokens, ', LazyChars,
      ' chars)');
  if GetEnvironmentVariable('G2_SAMPLE') <> '' then ReportSamples;
  if not Ok then
  begin
    if ToStringV(Pending, S) and IsString(S) then WriteLn('Uncaught ', UTF8Encode(StrToUnicode(StrOf(S))))
    else WriteLn('Uncaught exception');
    Halt(1);
  end;
  if GetEnvironmentVariable('G2_GCSTATS') <> '' then
    WriteLn(ErrOutput, 'gc: minor ', GCMinorCount, ' major ', GCMajorCount, ' promoted ', GCPromotedBytes div 1024, ' KB old ', GCOldBytes div 1024, ' KB');
  if GetEnvironmentVariable('G2_GCSTATS') <> '' then
    with GetFPCHeapStatus do
      WriteLn(ErrOutput, 'heap: maxused ', MaxHeapUsed div 1024, ' KB maxsize ', MaxHeapSize div 1024, ' KB cur ', CurrHeapUsed div 1024, ' KB');
end.
