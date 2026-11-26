unit Goccia.Artifact;
{ Bytecode files (.gbc): a compiled Script or Module written to bytes and read back.

  Unlike the prelude snapshot (Goccia.Snapshot), a bytecode file comes from disk and is untrusted input
  (docs/bytecode-vm.md, Binary loading is a validation boundary): the reader checks every length against what
  is left, every count against a limit and every tag against its known values, and rejects trailing bytes; the
  verifier (VerifyTemplate) then checks the code before anything of it runs.

  Layout, little-endian:
    header   magic 'GBC'#0, u16 format version (200; the old engine's files are version 100), the runtime tag
             'goccia-engine', u64 engine fingerprint (the instruction set: a build whose opcodes differ rejects
             the file), source path (its extension gives the module's logical address), the compile profile
             (Goccia.Bytecode.Profile's wire fields, then the engine's unrestricted flag), u8 kind (0 Script,
             1 Module)
    sources  the source texts the templates' Function.prototype.toString ranges point into (usually one)
    module   a Module's record (TModuleInfo); a Script's import text (ScriptImportSource, an explicit Script's
             imports run as a module first)
    template the top-level template and, nested, every function in it
  Strings are UTF-16 code units, a u32 length first (-1: none). Constants carry an explicit kind (a double,
  an int32, undefined, null, a boolean, a string or a BigInt's decimal text): never raw value bits, which
  could forge a pointer or a hole. A tagged template's site number is stored relative to the file's first
  and given a fresh number at load. Inline caches are stored as their keys only. }
{$mode delphiunicode}{$H+}
{$I Goccia.Checks.inc}
{$MINFPCONSTPREC 64}
{$POINTERMATH ON}

interface

uses
  SysUtils, Classes, Goccia.Core, Goccia.Instructions, Goccia.Compiler, Goccia.Bytecode.Profile;

const
  GOCCIA_GBC_VERSION = 200;
  GOCCIA_GBC_RUNTIME_TAG = 'goccia-engine';
  { as the old engine's reader (Goccia.Bytecode.Binary) }
  GOCCIA_GBC_MAX_FILE_BYTES = 64 * 1024 * 1024;
  GOCCIA_GBC_MAX_DEPTH = 256;

type
  EGocciaArtifact = class(Exception);

  TGocciaArtifact = record
    SourcePath: UnicodeString;
    Profile: TGocciaCompileProfile;
    { compiled by an unrestricted engine (every language form on: the conformance host) }
    Unrestricted: Boolean;
    Top: PFuncTemplate;
    { a Module's record (nil for a Script); the caller owns it }
    Info: TModuleInfo;
    { a Script's imports, run as a module before it (Goccia.Eval ScriptImportSource) }
    ScriptImports: UnicodeString;
  end;

{ the bytes of a compiled Script (A.Info nil) or Module; raises EGocciaArtifact for a template the format cannot
  hold (a lazy function not compiled yet, a constant of another kind, a filled inline cache) }
function SaveArtifact(const A: TGocciaArtifact): TBytes;
{ reads and verifies Size bytes at Data (borrowed for the call only): False with Err set when they are not a
  valid bytecode file for this engine. The templates are registered with this thread's engine (they live as
  long as it); their ModuleId is ModuleId and their SourceName RuntimeName (the module's logical address). }
function LoadArtifact(Data: PByte; Size: Int64; const RuntimeName: UnicodeString; ModuleId: Integer;
  out A: TGocciaArtifact; var Err: UnicodeString): Boolean;
{ whether the bytes start as a bytecode file of any engine (the magic) }
function LooksLikeArtifact(Data: PByte; Size: Int64): Boolean;
{ the source path a bytecode file of this engine records (its header only, checked as LoadArtifact checks it):
  False with Err set when the bytes are not such a file }
function ArtifactSourcePath(Data: PByte; Size: Int64; out Path: UnicodeString; var Err: UnicodeString): Boolean;
{ the engine fingerprint stored in the header }
function EngineFingerprint: QWord;
{ the profile code compiled now has (ModuleSource: the experimental JS module source flag), as a bytecode file
  records it }
function CurrentArtifactProfile(Module, ModuleSource: Boolean; out Unrestricted: Boolean): TGocciaCompileProfile;

implementation

uses
  TypInfo, BigInteger, Goccia.BigIntV, Goccia.VM, Goccia.SourceSpan;

const
  MAGIC: array[0..3] of Byte = (Ord('G'), Ord('B'), Ord('C'), 0);
  { constant kinds }
  CK_DOUBLE = 0;
  CK_INT = 1;
  CK_SPECIAL = 2;   { undefined, null, false, true }
  CK_STRING = 3;
  CK_BIGINT = 4;
  { template flags }
  TF_ARROW = 1 shl 0;
  TF_CLASS_CTOR = 1 shl 1;
  TF_DERIVED = 1 shl 2;
  TF_METHOD = 1 shl 3;
  TF_GENERATOR = 1 shl 4;
  TF_ASYNC = 1 shl 5;
  TF_STRICT = 1 shl 6;
  TF_SIMPLE_PARAMS = 1 shl 7;
  TF_MAPPED_ARGS = 1 shl 8;
  TF_TOP_LEVEL = 1 shl 9;
  TF_KNOWN = (1 shl 10) - 1;

var
  Fingerprint: QWord = 0;

function EngineFingerprint: QWord;
var Op: TOp; S: string; I: Integer; H: QWord;
begin
  if Fingerprint <> 0 then Exit(Fingerprint);
  {$PUSH}{$OVERFLOWCHECKS OFF}{$RANGECHECKS OFF}
  H := 1469598103934665603 xor GOCCIA_GBC_VERSION;
  for Op := Low(TOp) to High(TOp) do
  begin
    S := GetEnumName(TypeInfo(TOp), Ord(Op)) + ';';
    for I := 1 to Length(S) do H := (H xor Ord(S[I])) * 1099511628211;
  end;
  H := (H xor QWord(INTRINSIC_COUNT)) * 1099511628211;
  {$POP}
  if H = 0 then H := 1;
  Fingerprint := H;
  Result := H;
end;

function LooksLikeArtifact(Data: PByte; Size: Int64): Boolean;
begin
  Result := (Size >= 4) and (Data[0] = MAGIC[0]) and (Data[1] = MAGIC[1]) and (Data[2] = MAGIC[2]) and
    (Data[3] = MAGIC[3]);
end;

{ ---------------- writing ---------------- }

type
  TWriter = class
  private
    S: TMemoryStream;
    Sources: array of UnicodeString;
    SiteBase: Integer;
    procedure Template(T, Parent: PFuncTemplate; Depth: Integer);
    function SourceIndex(const Src: UnicodeString): Integer;
    procedure CollectSources(T: PFuncTemplate; Depth: Integer);
    procedure SiteRange(T: PFuncTemplate; var Lo, Hi: Integer);
  public
    constructor Create;
    destructor Destroy; override;
    procedure U8(V: Byte);
    procedure U16(V: Word);
    procedure I32(V: Integer);
    procedure U64(V: QWord);
    procedure Text(const U: UnicodeString);
    procedure Str(P: PStr);
  end;

constructor TWriter.Create;
begin
  S := TMemoryStream.Create;
end;

destructor TWriter.Destroy;
begin
  S.Free;
  inherited;
end;

procedure TWriter.U8(V: Byte); begin S.WriteBuffer(V, 1); end;
procedure TWriter.U16(V: Word); begin S.WriteBuffer(V, 2); end;
procedure TWriter.I32(V: Integer); begin S.WriteBuffer(V, 4); end;
procedure TWriter.U64(V: QWord); begin S.WriteBuffer(V, 8); end;

procedure TWriter.Text(const U: UnicodeString);
begin
  I32(Length(U));
  if Length(U) > 0 then S.WriteBuffer(U[1], Length(U) * 2);
end;

{ an atom; nil as -1, and the engine's internal keys the compiler names (UsingKey 0..2) as -2 - their index }
procedure TWriter.Str(P: PStr);
var K: Integer;
begin
  if P = nil then begin I32(-1); Exit; end;
  if P^.Hdr.Extra <> 0 then
  begin
    for K := 0 to 2 do
      if P = UsingKey(K) then begin I32(-2 - K); Exit; end;
    raise EGocciaArtifact.Create('a key no bytecode file can name (a symbol or private name)');
  end;
  Text(StrToUnicode(P));
end;

function TWriter.SourceIndex(const Src: UnicodeString): Integer;
var I: Integer;
begin
  { the same string, shared by the templates of one compilation: compare the pointer first }
  for I := 0 to High(Sources) do
    if (Pointer(Sources[I]) = Pointer(Src)) or (Sources[I] = Src) then Exit(I);
  Result := Length(Sources);
  SetLength(Sources, Result + 1);
  Sources[Result] := Src;
end;

procedure TWriter.CollectSources(T: PFuncTemplate; Depth: Integer);
var I, Start, Len: Integer; Src: string;
begin
  if Depth > GOCCIA_GBC_MAX_DEPTH then raise EGocciaArtifact.CreateFmt('functions nested more than %d deep', [GOCCIA_GBC_MAX_DEPTH]);
  if T^.SourceRange.GetRange(Src, Start, Len) then SourceIndex(Src);
  for I := 0 to High(T^.Children) do CollectSources(T^.Children[I], Depth + 1);
end;

procedure TWriter.SiteRange(T: PFuncTemplate; var Lo, Hi: Integer);
var I, C: Integer;
begin
  for I := 0 to T^.CodeLen - 1 do
    if InsOp(T^.Code[I]) = opTemplateObj then
    begin
      C := InsCU(T^.Code[I]);
      if C < Lo then Lo := C;
      if C > Hi then Hi := C;
    end;
  for I := 0 to High(T^.Children) do SiteRange(T^.Children[I], Lo, Hi);
end;

procedure TWriter.Template(T, Parent: PFuncTemplate; Depth: Integer);
var I, K, Flags, Start, Len: Integer; V: TValue; Ins_: QWord; Src: string; IC: PPropIC;
begin
  if T^.Lazy <> nil then raise EGocciaArtifact.Create('a function not compiled yet (lazy)');
  Str(T^.Name);
  I32(T^.NumParams); I32(T^.MaxRegs);
  I32(T^.RestReg); I32(T^.ArgsReg); I32(T^.HomeReg); I32(T^.NewTargetReg); I32(T^.AsyncReg);
  I32(T^.Length);
  Flags := Ord(T^.IsArrow) * TF_ARROW or Ord(T^.IsClassCtor) * TF_CLASS_CTOR or Ord(T^.IsDerived) * TF_DERIVED or
    Ord(T^.IsMethod) * TF_METHOD or Ord(T^.IsGenerator) * TF_GENERATOR or Ord(T^.IsAsync) * TF_ASYNC or
    Ord(T^.IsStrict) * TF_STRICT or Ord(T^.SimpleParams) * TF_SIMPLE_PARAMS or Ord(T^.MappedArgs) * TF_MAPPED_ARGS or
    Ord(T^.TopLevel) * TF_TOP_LEVEL;
  I32(Flags);
  I32(T^.DeclLine); I32(T^.DeclCol);
  I32(Length(T^.Upvals));
  for I := 0 to High(T^.Upvals) do
  begin
    U8(Ord(T^.Upvals[I].FromParentLocal));
    I32(T^.Upvals[I].Index);
  end;
  { code: a tagged template's site relative to the file's first }
  I32(T^.CodeLen); I32(T^.EntryPC);
  for I := 0 to T^.CodeLen - 1 do
  begin
    Ins_ := T^.Code[I];
    if InsOp(Ins_) = opTemplateObj then
      Ins_ := Ins(opTemplateObj, InsA(Ins_), InsB(Ins_), InsCU(Ins_) - SiteBase);
    U64(Ins_);
  end;
  { a line and a column for each instruction (0 unknown); the arrays may be longer (spare capacity) }
  for I := 0 to T^.CodeLen - 1 do
    if I <= High(T^.Lines) then I32(T^.Lines[I]) else I32(0);
  for I := 0 to T^.CodeLen - 1 do
    if I <= High(T^.Cols) then I32(T^.Cols[I]) else I32(0);
  I32(Length(T^.CallSites));
  for I := 0 to High(T^.CallSites) do
  begin
    I32(T^.CallSites[I].PC);
    Str(T^.CallSites[I].Text);
  end;
  I32(Length(T^.Consts));
  for I := 0 to High(T^.Consts) do
  begin
    V := T^.Consts[I];
    if IsDouble(V) then begin U8(CK_DOUBLE); U64(V); end
    else if IsInt(V) then begin U8(CK_INT); I32(Int32(UInt32(V))); end
    else if (V = VUndefined) or (V = VNull) or (V = VFalse) or (V = VTrue) then
    begin
      U8(CK_SPECIAL);
      U8(Byte(V and $FF));
    end
    else if IsString(V) then begin U8(CK_STRING); Str(PStr(PtrOf(V))); end
    else if IsBigInt(V) then begin U8(CK_BIGINT); Text(BigToString(V)); end
    else raise EGocciaArtifact.Create('a constant of a kind a bytecode file cannot hold');
  end;
  I32(Length(T^.ICs));
  for I := 0 to High(T^.ICs) do
  begin
    IC := @T^.ICs[I];
    { a fresh inline cache holds its key only }
    if (IC^.Shape <> nil) or (IC^.NewShape <> nil) or (IC^.PolyCount <> 0) or (IC^.Depth <> 0) or (IC^.Slot <> -1) then
      raise EGocciaArtifact.Create('an inline cache filled before the file was written');
    Str(IC^.Key);
  end;
  { global declarations (a Script's top level) }
  U8(T^.GdiKind);
  I32(Length(T^.GdiLex));
  for I := 0 to High(T^.GdiLex) do Str(T^.GdiLex[I]);
  I32(Length(T^.GdiLexConst));
  for I := 0 to High(T^.GdiLexConst) do U8(Ord(T^.GdiLexConst[I]));
  I32(Length(T^.GdiVars));
  for I := 0 to High(T^.GdiVars) do Str(T^.GdiVars[I]);
  I32(Length(T^.GdiFuncs));
  for I := 0 to High(T^.GdiFuncs) do Str(T^.GdiFuncs[I]);
  { Function.prototype.toString: a range of one of the sources, or text of its own }
  if T^.SourceRange.GetRange(Src, Start, Len) then
  begin
    U8(1);
    I32(SourceIndex(Src));
    I32(Start);
    I32(Len);
  end
  else
  begin
    U8(0);
    Text(T^.SourceRange.Get);
  end;
  I32(Length(T^.Children));
  for I := 0 to High(T^.Children) do Template(T^.Children[I], T, Depth + 1);
  { a class's field initializer is a function of the constructor's parent too (Goccia.Compiler makes its closure
    there): its index among the parent's children, -1 for none }
  K := -1;
  if T^.FieldInit <> nil then
  begin
    if Parent <> nil then
      for I := 0 to High(Parent^.Children) do
        if Parent^.Children[I] = T^.FieldInit then begin K := I; Break; end;
    if K < 0 then raise EGocciaArtifact.Create('a field initializer that is not a function of its class''s scope');
  end;
  I32(K);
end;

procedure WriteModuleInfo(W: TWriter; Info: TModuleInfo);
  procedure Entries(const E: array of TModEntry);
  var I: Integer;
  begin
    W.I32(Length(E));
    for I := 0 to High(E) do
    begin
      W.Text(E[I].ExportName);
      W.Text(E[I].LocalName);
      W.Text(E[I].Request);
      W.Text(E[I].ImportName);
      W.I32(E[I].Slot);
    end;
  end;
var I: Integer;
begin
  W.I32(Length(Info.Requests));
  for I := 0 to High(Info.Requests) do
  begin
    W.Text(Info.Requests[I]);
    if I <= High(Info.RequestLines) then W.I32(Info.RequestLines[I]) else W.I32(0);
    if I <= High(Info.RequestColumns) then W.I32(Info.RequestColumns[I]) else W.I32(0);
  end;
  Entries(Info.Imports);
  Entries(Info.LocalExports);
  Entries(Info.IndirectExports);
  W.I32(Length(Info.StarExports));
  for I := 0 to High(Info.StarExports) do W.Text(Info.StarExports[I]);
  W.I32(Length(Info.EvalOrder));
  for I := 0 to High(Info.EvalOrder) do
  begin
    W.Text(Info.EvalOrder[I].Req);
    W.U8(Ord(Info.EvalOrder[I].Eager));
  end;
  W.U8(Ord(Info.HasTLA));
  W.I32(Info.SlotCount);
end;

function SaveArtifact(const A: TGocciaArtifact): TBytes;
var W: TWriter; I, Lo, Hi: Integer; Body: TWriter;
begin
  W := TWriter.Create;
  Body := TWriter.Create;
  try
    Body.CollectSources(A.Top, 1);
    Lo := MaxInt; Hi := -1;
    Body.SiteRange(A.Top, Lo, Hi);
    if Hi < 0 then Lo := 0;
    Body.SiteBase := Lo;
    Body.Template(A.Top, nil, 1);
    W.S.WriteBuffer(MAGIC, 4);
    W.U16(GOCCIA_GBC_VERSION);
    W.Text(GOCCIA_GBC_RUNTIME_TAG);
    W.U64(EngineFingerprint);
    W.Text(A.SourcePath);
    W.I32(Integer(A.Profile.CompatibilityMask));
    W.I32(Integer(A.Profile.PreprocessorMask));
    W.U8(Ord(A.Profile.StrictTypes));
    W.U8(Ord(A.Profile.WarningUnsupportedFeatures));
    W.U8(Ord(A.Profile.SourceType));
    W.U8(Ord(A.Unrestricted));
    W.I32(Length(Body.Sources));
    for I := 0 to High(Body.Sources) do W.Text(Body.Sources[I]);
    { the number of tagged template sites (fresh numbers at load) }
    if Hi < 0 then W.I32(0) else W.I32(Hi - Lo + 1);
    if A.Info <> nil then
    begin
      W.U8(1);
      WriteModuleInfo(W, A.Info);
    end
    else
    begin
      W.U8(0);
      W.Text(A.ScriptImports);
    end;
    W.S.CopyFrom(Body.S, 0);
    SetLength(Result, W.S.Size);
    if W.S.Size > 0 then Move(W.S.Memory^, Result[0], W.S.Size);
  finally
    Body.Free;
    W.Free;
  end;
end;

{ ---------------- reading ---------------- }

type
  TReader = record
    P, Limit: PByte;
    procedure Need(N: Int64);
    function U8: Byte;
    function U16: Word;
    function I32: Integer;
    function U64: QWord;
    { a count of items at least MinBytes each, checked against what is left }
    function Count(MinBytes: Integer): Integer;
    function Text: UnicodeString;
    function Str: PStr;
  end;

procedure Fail(const Msg: string);
begin
  raise EGocciaArtifact.Create(Msg);
end;

procedure TReader.Need(N: Int64);
begin
  if (N < 0) or (N > Limit - P) then Fail('truncated bytecode file');
end;

function TReader.U8: Byte; begin Need(1); Result := P^; Inc(P); end;
function TReader.U16: Word; begin Need(2); Move(P^, Result, 2); Inc(P, 2); end;
function TReader.I32: Integer; begin Need(4); Move(P^, Result, 4); Inc(P, 4); end;
function TReader.U64: QWord; begin Need(8); Move(P^, Result, 8); Inc(P, 8); end;

function TReader.Count(MinBytes: Integer): Integer;
begin
  Result := I32;
  if Result < 0 then Fail('negative count in bytecode file');
  Need(Int64(Result) * MinBytes);
end;

function TReader.Text: UnicodeString;
var N: Integer;
begin
  N := I32;
  if N <= 0 then
  begin
    if N < -1 then Fail('invalid string length in bytecode file');
    Exit('');
  end;
  Need(Int64(N) * 2);
  SetLength(Result, N);
  Move(P^, Result[1], N * 2);
  Inc(P, N * 2);
end;

function TReader.Str: PStr;
var N: Integer;
begin
  N := I32;
  if N = -1 then Exit(nil);
  if (N <= -2) and (N >= -4) then Exit(UsingKey(-2 - N));
  if N < 0 then Fail('invalid string length in bytecode file');
  Need(Int64(N) * 2);
  Result := AtomOfChars(PWideChar(P), N);
  Inc(P, N * 2);
end;

type
  TLoadState = record
    Sources: array of UnicodeString;
    SiteBase, SiteCount: Integer;
    RuntimeName: UnicodeString;
    ModuleId: Integer;
  end;

function ValidBigIntText(const S: UnicodeString): Boolean;
var I, First: Integer;
begin
  First := 1;
  if (Length(S) > 0) and (S[1] = '-') then First := 2;
  Result := (Length(S) >= First) and (Length(S) <= 1 shl 20);
  if not Result then Exit;
  for I := First to Length(S) do
    if (S[I] < '0') or (S[I] > '9') then Exit(False);
end;

{ FieldInitIdx: the template's field initializer as an index among its parent's children (-1 none), which the
  parent resolves }
function ReadTemplate(var R: TReader; var St: TLoadState; Depth: Integer; out FieldInitIdx: Integer): PFuncTemplate;
var T: PFuncTemplate; I, N, Flags, SrcIdx, Start, Len, Site: Integer; Ins_, Bits: QWord; Kind, B: Byte;
  BigText: UnicodeString; P: PStr; FI: array of Integer;
begin
  if Depth > GOCCIA_GBC_MAX_DEPTH then Fail(Format('functions nested more than %d deep', [GOCCIA_GBC_MAX_DEPTH]));
  New(T);
  { every field starts zeroed: the runtime caches (new's, the JIT's, the profiler's) start empty }
  FillChar(T^, SizeOf(TFuncTemplate), 0);
  RegisterTemplate(T);
  Result := T;
  T^.Name := R.Str;
  T^.NumParams := R.I32; T^.MaxRegs := R.I32;
  T^.RestReg := R.I32; T^.ArgsReg := R.I32; T^.HomeReg := R.I32; T^.NewTargetReg := R.I32; T^.AsyncReg := R.I32;
  { ExpectedProps is a run-time sizing cache (opRet raises it): it starts at 0 }
  T^.Length := R.I32;
  Flags := R.I32;
  if Flags and not TF_KNOWN <> 0 then Fail('unknown function flags in bytecode file');
  T^.IsArrow := Flags and TF_ARROW <> 0;
  T^.IsClassCtor := Flags and TF_CLASS_CTOR <> 0;
  T^.IsDerived := Flags and TF_DERIVED <> 0;
  T^.IsMethod := Flags and TF_METHOD <> 0;
  T^.IsGenerator := Flags and TF_GENERATOR <> 0;
  T^.IsAsync := Flags and TF_ASYNC <> 0;
  T^.IsStrict := Flags and TF_STRICT <> 0;
  T^.SimpleParams := Flags and TF_SIMPLE_PARAMS <> 0;
  T^.MappedArgs := Flags and TF_MAPPED_ARGS <> 0;
  T^.TopLevel := Flags and TF_TOP_LEVEL <> 0;
  T^.DeclLine := R.I32; T^.DeclCol := R.I32;
  SetLength(T^.Upvals, R.Count(5));
  for I := 0 to High(T^.Upvals) do
  begin
    B := R.U8;
    if B > 1 then Fail('invalid upvalue descriptor in bytecode file');
    T^.Upvals[I].FromParentLocal := B = 1;
    T^.Upvals[I].Index := R.I32;
  end;
  T^.CodeLen := R.Count(8);
  T^.EntryPC := R.I32;
  SetLength(T^.Code, T^.CodeLen);
  for I := 0 to T^.CodeLen - 1 do
  begin
    Ins_ := R.U64;
    if (Ins_ and $FF) > Ord(High(TOp)) then Fail(Format('unknown opcode %d in bytecode file', [Ins_ and $FF]));
    if InsOp(Ins_) = opTemplateObj then
    begin
      Site := InsCU(Ins_);
      if Site >= St.SiteCount then Fail('tagged template site out of range in bytecode file');
      Ins_ := Ins(opTemplateObj, InsA(Ins_), InsB(Ins_), St.SiteBase + Site);
    end;
    T^.Code[I] := Ins_;
  end;
  R.Need(Int64(T^.CodeLen) * 8);
  SetLength(T^.Lines, T^.CodeLen);
  for I := 0 to T^.CodeLen - 1 do T^.Lines[I] := R.I32;
  SetLength(T^.Cols, T^.CodeLen);
  for I := 0 to T^.CodeLen - 1 do T^.Cols[I] := R.I32;
  SetLength(T^.CallSites, R.Count(8));
  for I := 0 to High(T^.CallSites) do
  begin
    T^.CallSites[I].PC := R.I32;
    T^.CallSites[I].Text := R.Str;
  end;
  SetLength(T^.Consts, R.Count(2));
  for I := 0 to High(T^.Consts) do
  begin
    Kind := R.U8;
    case Kind of
      CK_DOUBLE:
        begin
          Bits := R.U64;
          if not IsDouble(Bits) then Fail('invalid number constant in bytecode file');
          T^.Consts[I] := Bits;
        end;
      CK_INT: T^.Consts[I] := MkInt(R.I32);
      CK_SPECIAL:
        begin
          B := R.U8;
          if B > 3 then Fail('invalid constant in bytecode file');
          T^.Consts[I] := TAG_SPECIAL or B;
        end;
      CK_STRING:
        begin
          T^.Consts[I] := VUndefined;
          P := R.Str;
          if P = nil then Fail('invalid string constant in bytecode file');
          T^.Consts[I] := MkPtr(P);
        end;
      CK_BIGINT:
        begin
          BigText := R.Text;
          if not ValidBigIntText(BigText) then Fail('invalid BigInt constant in bytecode file');
          T^.Consts[I] := BigToValue(TBigInteger.FromDecimalString(string(BigText)), True);
        end;
    else
      Fail('unknown constant kind in bytecode file');
    end;
  end;
  N := R.Count(4);
  SetLength(T^.ICs, N);
  for I := 0 to N - 1 do
  begin
    FillChar(T^.ICs[I], SizeOf(TPropIC), 0);
    T^.ICs[I].Key := R.Str;
    T^.ICs[I].Slot := -1;
  end;
  T^.GdiKind := R.U8;
  if T^.GdiKind > 2 then Fail('invalid global declaration kind in bytecode file');
  SetLength(T^.GdiLex, R.Count(4));
  for I := 0 to High(T^.GdiLex) do T^.GdiLex[I] := R.Str;
  SetLength(T^.GdiLexConst, R.Count(1));
  for I := 0 to High(T^.GdiLexConst) do T^.GdiLexConst[I] := R.U8 <> 0;
  SetLength(T^.GdiVars, R.Count(4));
  for I := 0 to High(T^.GdiVars) do T^.GdiVars[I] := R.Str;
  SetLength(T^.GdiFuncs, R.Count(4));
  for I := 0 to High(T^.GdiFuncs) do T^.GdiFuncs[I] := R.Str;
  case R.U8 of
    0: T^.SourceRange.SetText(R.Text);
    1:
      begin
        SrcIdx := R.I32; Start := R.I32; Len := R.I32;
        if (SrcIdx < 0) or (SrcIdx > High(St.Sources)) or (Start < 0) or (Len < 0) or
           (Int64(Start) + Len > Length(St.Sources[SrcIdx])) then
          Fail('invalid source range in bytecode file');
        if Len > 0 then T^.SourceRange.SetRange(St.Sources[SrcIdx], Start, Len);
      end;
  else
    Fail('invalid source text tag in bytecode file');
  end;
  T^.SourceName := St.RuntimeName;
  T^.ModuleId := St.ModuleId;
  SetLength(T^.Children, R.Count(4));
  SetLength(FI, Length(T^.Children));
  for I := 0 to High(T^.Children) do T^.Children[I] := nil;
  for I := 0 to High(T^.Children) do T^.Children[I] := ReadTemplate(R, St, Depth + 1, FI[I]);
  { the children's field initializers: functions of this template too }
  for I := 0 to High(T^.Children) do
    if FI[I] >= 0 then
    begin
      if (FI[I] > High(T^.Children)) or (FI[I] = I) then Fail('invalid field initializer in bytecode file');
      T^.Children[I]^.FieldInit := T^.Children[FI[I]];
    end;
  FieldInitIdx := R.I32;
  if FieldInitIdx < -1 then Fail('invalid field initializer in bytecode file');
  if (FieldInitIdx >= 0) and (Depth = 1) then Fail('field initializer on a top level in bytecode file');
  { the cached parts: constant and IC bases, the call and capture flags }
  FinishTemplate(T);
end;

function ReadModuleInfo(var R: TReader): TModuleInfo;
  procedure Entries(var E: array of TModEntry);
  var I: Integer;
  begin
    for I := 0 to High(E) do
    begin
      E[I].ExportName := R.Text;
      E[I].LocalName := R.Text;
      E[I].Request := R.Text;
      E[I].ImportName := R.Text;
      E[I].Slot := R.I32;
    end;
  end;
var I, N: Integer; B: Byte;
begin
  Result := TModuleInfo.Create;
  try
    N := R.Count(12);
    SetLength(Result.Requests, N);
    SetLength(Result.RequestLines, N);
    SetLength(Result.RequestColumns, N);
    for I := 0 to N - 1 do
    begin
      Result.Requests[I] := R.Text;
      Result.RequestLines[I] := R.I32;
      Result.RequestColumns[I] := R.I32;
    end;
    SetLength(Result.Imports, R.Count(20));
    Entries(Result.Imports);
    SetLength(Result.LocalExports, R.Count(20));
    Entries(Result.LocalExports);
    SetLength(Result.IndirectExports, R.Count(20));
    Entries(Result.IndirectExports);
    SetLength(Result.StarExports, R.Count(4));
    for I := 0 to High(Result.StarExports) do Result.StarExports[I] := R.Text;
    SetLength(Result.EvalOrder, R.Count(5));
    for I := 0 to High(Result.EvalOrder) do
    begin
      Result.EvalOrder[I].Req := R.Text;
      B := R.U8;
      if B > 1 then Fail('invalid module request phase in bytecode file');
      Result.EvalOrder[I].Eager := B = 1;
    end;
    B := R.U8;
    if B > 1 then Fail('invalid module flag in bytecode file');
    Result.HasTLA := B = 1;
    Result.SlotCount := R.I32;
  except
    Result.Free;
    raise;
  end;
end;

{ ---------------- verifying ---------------- }

const
  { registers an instruction can name (A and B are 16-bit) }
  MAX_REGS = $FFFF;

procedure Reject(T: PFuncTemplate; PC: Integer; const Msg: string);
var N: UnicodeString;
begin
  if (T <> nil) and (T^.Name <> nil) then N := StrToUnicode(T^.Name) else N := '<anonymous>';
  if PC >= 0 then
    raise EGocciaArtifact.CreateFmt('invalid bytecode in %s at instruction %d: %s', [string(N), PC, Msg])
  else
    raise EGocciaArtifact.CreateFmt('invalid bytecode in %s: %s', [string(N), Msg]);
end;

{ the template-level invariants: register layout, tables, upvalues against the parent }
procedure VerifyTemplateShape(T, Parent: PFuncTemplate; IsTop: Boolean);
  procedure OptReg(R: Integer; const What: string);
  begin
    if (R < -1) or (R >= T^.MaxRegs) then Reject(T, -1, Format('%s register %d outside 0..%d', [What, R, T^.MaxRegs - 1]));
  end;
var I: Integer;
begin
  if (T^.MaxRegs < 0) or (T^.MaxRegs > MAX_REGS) then Reject(T, -1, Format('register count %d', [T^.MaxRegs]));
  if (T^.NumParams < 0) or (T^.NumParams > T^.MaxRegs) then Reject(T, -1, Format('parameter count %d', [T^.NumParams]));
  OptReg(T^.RestReg, 'rest');
  OptReg(T^.ArgsReg, 'arguments');
  OptReg(T^.HomeReg, 'home object');
  OptReg(T^.NewTargetReg, 'new.target');
  OptReg(T^.AsyncReg, 'promise');
  if T^.Length < 0 then Reject(T, -1, 'negative length');
  if T^.CodeLen < 1 then Reject(T, -1, 'no code');
  if (T^.EntryPC < 0) or (T^.EntryPC >= T^.CodeLen) then Reject(T, -1, Format('entry %d outside the code', [T^.EntryPC]));
  for I := 0 to High(T^.CallSites) do
    if (T^.CallSites[I].PC < 0) or (T^.CallSites[I].PC >= T^.CodeLen) or
       ((I > 0) and (T^.CallSites[I].PC <= T^.CallSites[I - 1].PC)) then
      Reject(T, -1, 'call site table out of order or outside the code');
  if Length(T^.GdiLexConst) <> Length(T^.GdiLex) then Reject(T, -1, 'global lexical table lengths differ');
  if (T^.GdiKind <> 0) and not (IsTop and T^.TopLevel) then Reject(T, -1, 'global declarations outside a top level');
  if IsTop then
  begin
    if not T^.TopLevel then Reject(T, -1, 'top level not marked so');
    if Length(T^.Upvals) <> 0 then Reject(T, -1, 'top level with upvalues');
  end
  else
  begin
    if T^.TopLevel then Reject(T, -1, 'nested function marked as a top level');
    for I := 0 to High(T^.Upvals) do
      if T^.Upvals[I].FromParentLocal then
      begin
        if (T^.Upvals[I].Index < 0) or (T^.Upvals[I].Index >= Parent^.MaxRegs) then
          Reject(T, -1, Format('upvalue %d names register %d of its parent', [I, T^.Upvals[I].Index]));
      end
      else if (T^.Upvals[I].Index < 0) or (T^.Upvals[I].Index > High(Parent^.Upvals)) then
        Reject(T, -1, Format('upvalue %d names upvalue %d of its parent', [I, T^.Upvals[I].Index]));
  end;
end;

procedure VerifyCode(T: PFuncTemplate); forward;

procedure VerifyTree(T, Parent: PFuncTemplate; IsTop: Boolean);
var I: Integer;
begin
  VerifyTemplateShape(T, Parent, IsTop);
  VerifyCode(T);
  { a field initializer is one of the parent's children (the reader made it so): verified as that }
  for I := 0 to High(T^.Children) do VerifyTree(T^.Children[I], T, False);
end;

procedure VerifyModuleInfo(const A: TGocciaArtifact);
var I: Integer;
  procedure Slot(S: Integer);
  begin
    if (S < -1) or (S >= A.Info.SlotCount) then Reject(A.Top, -1, Format('module cell %d outside 0..%d', [S, A.Info.SlotCount - 1]));
  end;
begin
  if A.Info = nil then Exit;
  if (A.Info.SlotCount < 0) or (A.Info.SlotCount > MAX_REGS) then Reject(A.Top, -1, 'module cell count');
  for I := 0 to High(A.Info.Imports) do Slot(A.Info.Imports[I].Slot);
  for I := 0 to High(A.Info.LocalExports) do Slot(A.Info.LocalExports[I].Slot);
  for I := 0 to High(A.Info.IndirectExports) do Slot(A.Info.IndirectExports[I].Slot);
end;

var
  ScrambleSeed: Int64 = -1;   { GOCCIA_ARTIFACT_SCRAMBLE: -1 not read yet, 0 off }

{ GOCCIA_ARTIFACT_SCRAMBLE=<seed> (a test of the checks, never for real files): before verifying, about one
  instruction in eight of every template gets its A, B or C replaced by another register of the template, so the
  code passes the verifier while registers hold kinds the instructions do not expect }
procedure Scramble(T: PFuncTemplate);
var I, Field, V: Integer; Ins_: QWord; Op: TOp;
  function Rnd(N: Integer): Integer;
  begin
    {$PUSH}{$OVERFLOWCHECKS OFF}{$RANGECHECKS OFF}
    ScrambleSeed := ScrambleSeed * 6364136223846793005 + 1442695040888963407;
    {$POP}
    Result := Integer((QWord(ScrambleSeed) shr 33) mod QWord(N));
  end;
begin
  if T^.MaxRegs > 0 then
    for I := 0 to T^.CodeLen - 1 do
      if Rnd(8) = 0 then
      begin
        Ins_ := T^.Code[I];
        Op := InsOp(Ins_);
        if Op in [opJmp, opJT, opJF, opJNullish, opJNotNullish, opJUndef, opJNotUndef, opJNLt, opJNLe, opJNGt, opJNGe,
          opJNSEq, opJSEq, opIterNext, opIterUnpack, opAsyncIterReturn, opTry, opTemplateObj] then Continue;
        V := Rnd(T^.MaxRegs);
        Field := Rnd(3);
        case Field of
          0: T^.Code[I] := Ins(Op, V, InsB(Ins_), InsCU(Ins_));
          1: T^.Code[I] := Ins(Op, InsA(Ins_), V, InsCU(Ins_));
        else T^.Code[I] := Ins(Op, InsA(Ins_), InsB(Ins_), V);
        end;
      end;
  for I := 0 to High(T^.Children) do Scramble(T^.Children[I]);
end;

procedure VerifyArtifact(const A: TGocciaArtifact);
begin
  if ScrambleSeed < 0 then ScrambleSeed := StrToInt64Def(GetEnvironmentVariable('GOCCIA_ARTIFACT_SCRAMBLE'), 0);
  if ScrambleSeed > 0 then Scramble(A.Top);
  VerifyModuleInfo(A);
  VerifyTree(A.Top, nil, True);
end;

{$I Goccia.ArtifactVerify.inc}

function CurrentArtifactProfile(Module, ModuleSource: Boolean; out Unrestricted: Boolean): TGocciaCompileProfile;
var Pr: TLangProfile; M: UInt32;
begin
  Pr := ActiveProfile;
  Unrestricted := not Pr.Restricted;
  M := 0;
  if Pr.Restricted then
  begin
    if Pr.Asi then M := M or BYTECODE_COMPAT_ASI;
    if Pr.VarDecl then M := M or BYTECODE_COMPAT_VAR;
    if Pr.FuncDecl then M := M or BYTECODE_COMPAT_FUNCTION;
    if Pr.ForLoop then M := M or BYTECODE_COMPAT_TRADITIONAL_FOR;
    if Pr.WhileLoop then M := M or BYTECODE_COMPAT_WHILE;
    if Pr.LooseEq then M := M or BYTECODE_COMPAT_LOOSE_EQUALITY;
    if Pr.NonStrict then M := M or BYTECODE_COMPAT_NON_STRICT;
    if Pr.ArgumentsObject then M := M or BYTECODE_COMPAT_ARGUMENTS;
    if Pr.Labels then M := M or BYTECODE_COMPAT_LABEL;
    if Pr.ForIn then M := M or BYTECODE_COMPAT_FOR_IN;
    if ModuleSource then M := M or BYTECODE_COMPAT_MODULE_SOURCE;
    Result.PreprocessorMask := BYTECODE_PREPROCESSOR_JSX;
    Result.WarningUnsupportedFeatures := Pr.WarningUnsupported;
    Result.StrictTypes := Pr.StrictTypes;
  end
  else
  begin
    Result.PreprocessorMask := 0;
    Result.WarningUnsupportedFeatures := False;
    Result.StrictTypes := False;
  end;
  Result.CompatibilityMask := M;
  if Module then Result.SourceType := bstModule else Result.SourceType := bstScript;
end;

{ the header through the source path }
procedure ReadHead(var R: TReader; Data: PByte; Size: Int64; out SourcePath: UnicodeString);
var Version: Word; Tag: UnicodeString;
begin
  if (Size < 0) or (Size > GOCCIA_GBC_MAX_FILE_BYTES) then
    Fail(Format('bytecode file exceeds the %d byte limit', [GOCCIA_GBC_MAX_FILE_BYTES]));
  if not LooksLikeArtifact(Data, Size) then Fail('not a GocciaScript bytecode file');
  R.P := Data + 4;
  R.Limit := Data + Size;
  Version := R.U16;
  if Version <> GOCCIA_GBC_VERSION then
    Fail(Format('bytecode format version %d is not supported (this engine reads version %d); recompile the ' +
      'source with this GocciaScript', [Version, GOCCIA_GBC_VERSION]));
  Tag := R.Text;
  if Tag <> GOCCIA_GBC_RUNTIME_TAG then Fail('bytecode file for another runtime: ' + string(Tag));
  if R.U64 <> EngineFingerprint then
    Fail('bytecode file compiled by a different build of GocciaScript; recompile the source');
  SourcePath := R.Text;
end;

function ArtifactSourcePath(Data: PByte; Size: Int64; out Path: UnicodeString; var Err: UnicodeString): Boolean;
var R: TReader;
begin
  Path := '';
  try
    ReadHead(R, Data, Size, Path);
    Result := True;
  except
    on E: Exception do
    begin
      Err := UnicodeString(E.Message);
      Result := False;
    end;
  end;
end;

function LoadArtifact(Data: PByte; Size: Int64; const RuntimeName: UnicodeString; ModuleId: Integer;
  out A: TGocciaArtifact; var Err: UnicodeString): Boolean;
var R: TReader; St: TLoadState; I: Integer; B: Byte;
begin
  Result := False;
  A := Default(TGocciaArtifact);
  try
    ReadHead(R, Data, Size, A.SourcePath);
    A.Profile.CompatibilityMask := UInt32(R.I32);
    A.Profile.PreprocessorMask := UInt32(R.I32);
    B := R.U8; if B > 1 then Fail('invalid profile flag in bytecode file');
    A.Profile.StrictTypes := B = 1;
    B := R.U8; if B > 1 then Fail('invalid profile flag in bytecode file');
    A.Profile.WarningUnsupportedFeatures := B = 1;
    B := R.U8; if B > Ord(High(TGocciaBytecodeSourceType)) then Fail('invalid source type in bytecode file');
    A.Profile.SourceType := TGocciaBytecodeSourceType(B);
    B := R.U8; if B > 1 then Fail('invalid profile flag in bytecode file');
    A.Unrestricted := B = 1;
    A.Profile.Validate;
    SetLength(St.Sources, R.Count(4));
    for I := 0 to High(St.Sources) do St.Sources[I] := R.Text;
    St.SiteCount := R.I32;
    if (St.SiteCount < 0) or (St.SiteCount > $7FFFFF) then Fail('invalid tagged template count in bytecode file');
    { each site is an instruction of the code that follows }
    R.Need(Int64(St.SiteCount) * 8);
    St.SiteBase := ReserveTemplateSites(St.SiteCount);
    if St.SiteBase < 0 then Fail('too many tagged templates');
    St.RuntimeName := RuntimeName;
    St.ModuleId := ModuleId;
    case R.U8 of
      0: A.ScriptImports := R.Text;
      1: A.Info := ReadModuleInfo(R);
    else
      Fail('invalid bytecode file kind');
    end;
    if (A.Info <> nil) <> (A.Profile.SourceType = bstModule) then Fail('bytecode file kind does not match its goal');
    A.Top := ReadTemplate(R, St, 1, I);
    if R.P <> R.Limit then Fail('trailing data in bytecode file');
    VerifyArtifact(A);
    Result := True;
  except
    on E: Exception do
    begin
      Err := UnicodeString(E.Message);
      FreeAndNil(A.Info);
      A.Top := nil;
    end;
  end;
end;

end.
