unit G2.HostAPI;
{ The CLI host on G2: what GocciaRunner and GocciaTestRunner give a script beyond the language. The API surfaces
  are JS preludes (prelude/host.js, prelude/testing.js) called with the natives they need from here; this unit
  installs them on the thread's realm, keeps what the host reads back, and reports a test file's results.

  The conformance host (g2 --test262) installs none of this: its realm keeps print, eval and the Annex B
  globals the CLIs leave out. }
{$mode delphiunicode}{$H+}
{$I G2.Checks.inc}
{$MINFPCONSTPREC 64}

interface

uses
  G2.Core, Goccia.ModuleResolver;

{ the host's globals on this thread's realm (after EngineInit): the const Goccia, and the conformance host's
  extras removed; TestMode lists the test runner's globals in Goccia.runtimeGlobals }
function InstallHostGlobals(ShadowRealm, TestMode: Boolean; const ConfigJson: UnicodeString;
  var Err: UnicodeString; ExposeTimers: Boolean = True): Boolean;
{ the heap limit when none is given: half the physical memory, at most 8 GB (700 MB on a 32-bit build) }
function DefaultHeapCeiling: Int64;

type
  { a console line with the method that wrote it (log, warn, error, ...), in place of writing it to stdout }
  TConsoleSink = procedure(const Method, Line: UnicodeString) of object;
{ this thread's console sink (nil: lines go to stdout) }
procedure SetConsoleSink(Sink: TConsoleSink);
{ the resolver of this thread's imports: a host's, configured with its import map, aliases and node_modules grant
  (Goccia.Modules.Resolver, not owned); nil for the plain one, which resolves relative and absolute paths only }
procedure SetHostResolver(R: TModuleResolver);
{ the jobs the script queued, then the timers in due order on the virtual clock, each followed by the jobs it
  queued, until none is left (False: an uncaught exception, in Pending) }
function RunEventLoop: Boolean;
{ the test framework API (describe, test, expect, ...) as globals of this thread's realm }
function InstallTestingAPI(var Err: UnicodeString): Boolean;
{ the test file's vi.mock directives (Goccia.Vitest.MockHoist): each mock's module, served at the address its
  specifier resolves to ahead of the file there }
procedure HoistTestMocks(const FileName: string; const Source: UnicodeString);
{ runs the suites the test file registered (after InstallTestingAPI) and the jobs they queue; Json their result
  (totalTests, passed, failed, skipped, assertions, failedTests), or False and Err the error that stopped
  them }
function RunTestSuites(var Json, Err: UnicodeString): Boolean;

implementation

uses
  SysUtils, Classes, Math, Linux, UnixType, G2.Bytecode, G2.VM, G2.Typed, G2.Modules, G2.Snapshot, G2.Promise, G2.Atomics, G2.Eval, G2.Weak,
  Goccia.Spec.Data, Goccia.Version, Goccia.Platform, Goccia.URL.Parser, Goccia.SourcePipeline, Goccia.Vitest.MockHoist,
  Goccia.AST.Report, Goccia.FileExtensions, TextSemantics, G2.HostSemver, Goccia.DataTree,
  Goccia.YAML, Goccia.TOML, Goccia.URI;

{$I G2.Testing.inc}
{$I G2.HostPrelude.inc}
{$I G2.UrlPrelude.inc}
{$I G2.WebPrelude.inc}
{$I G2.ModulesPrelude.inc}
{$I G2.ConsolePrelude.inc}
{$I G2.FetchPrelude.inc}
{$I G2.Json5Prelude.inc}
{$I G2.DelimitedPrelude.inc}
{$I G2.JsonlPrelude.inc}
{$I G2.SemverPrelude.inc}
{$I G2.AsyncHooksPrelude.inc}
{$I G2.EventsPrelude.inc}
{$I G2.TimersPrelude.inc}
{$I G2.DataTreePrelude.inc}

type
  { G2.Modules's resolution as the hoister's resolver }
  TMockResolver = class
    function Resolve(const ASpecifier, AReferrer: string; out AAddress: string): Boolean;
  end;

function TMockResolver.Resolve(const ASpecifier, AReferrer: string; out AAddress: string): Boolean;
begin
  AAddress := string(ResolveModuleSpecifier(UnicodeString(ASpecifier), UnicodeString(AReferrer)));
  Result := AAddress <> '';
end;

threadvar
  { the vi.mock modules: address, then source }
  MockModules: TStringList;
  SharedV, LoadHostModuleV, HasHostModuleV: TValue;

  FireNextV: TValue;
  EventLoopRootedGen: Integer;
  TestApiV: TValue;
  TestApiRootedGen: Integer;
  ReportJson, ReportErr: UnicodeString;
  ReportDone: Boolean;

function JsonStr(const S: UnicodeString): UnicodeString;
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

function GroupsJson(const Groups: TGocciaFeatureGroups): UnicodeString;
var I, J: Integer;
begin
  Result := '[';
  for I := 0 to High(Groups) do
  begin
    if I > 0 then Result := Result + ',';
    Result := Result + '{"key":' + JsonStr(UnicodeString(Groups[I].Key)) + ',"entries":[';
    for J := 0 to High(Groups[I].Entries) do
    begin
      if J > 0 then Result := Result + ',';
      Result := Result + '{"name":' + JsonStr(UTF8Decode(Groups[I].Entries[J].Name)) + ',"link":' +
        JsonStr(UTF8Decode(Groups[I].Entries[J].Link)) + '}';
    end;
    Result := Result + ']}';
  end;
  Result := Result + ']';
end;

{ the build facts host.js reads: Goccia.version, commit, build, spec and proposal }
function InfoJson: UnicodeString;
begin
  Result := '{"version":' + JsonStr(UnicodeString(GetVersion)) + ',"commit":' + JsonStr(UnicodeString(GetCommit)) +
    ',"date":' + JsonStr(UnicodeString(GetBuildDate)) + ',"os":' + JsonStr(UnicodeString(GetBuildOS)) +
    ',"arch":' + JsonStr(UnicodeString(GetBuildArch)) + ',"gcStress":' + LowerCase(BoolToStr(IsGCStressBuild, True)) +
    ',"spec":' + GroupsJson(SpecFeatureGroups) + ',"proposal":' + GroupsJson(ProposalFeatureGroups) + '}';
end;

{ Goccia.gc(): a full collection, the nursery first }
function Host_GC(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
begin
  CollectMinor;
  CollectMajor;
  Res := VUndefined;
  Result := True;
end;

{ Goccia.gc.bytesAllocated: the old generation and the nursery's used part }
function Host_GCBytes(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
begin
  Res := MkNumber(GCOldBytes + (NurseryTop - NurseryStart));
  Result := True;
end;

{ physical memory from /proc/meminfo, 0 when it cannot be read }
function PhysicalMemory: Int64;
var L: TStringList; I: Integer; S: string;
begin
  Result := 0;
  if not FileExists('/proc/meminfo') then Exit;
  L := TStringList.Create;
  try
    try
      L.LoadFromFile('/proc/meminfo');
    except
      Exit;
    end;
    for I := 0 to L.Count - 1 do
      if Copy(L[I], 1, 9) = 'MemTotal:' then
      begin
        S := Trim(Copy(L[I], 10, MaxInt));
        if Pos(' ', S) > 0 then S := Copy(S, 1, Pos(' ', S) - 1);
        Result := StrToInt64Def(S, 0) * 1024;
        Exit;
      end;
  finally
    L.Free;
  end;
end;

{ Goccia.gc.suggestedMaxBytes, and the ceiling without --max-memory or goccia.json max-memory: half of physical
  memory, at most 8 GB (700 MB on a 32-bit build) (docs/built-ins.md) }
function DefaultHeapCeiling: Int64;
const Cap = {$IFDEF CPU64}Int64(8) * 1024 * 1024 * 1024{$ELSE}Int64(700) * 1024 * 1024{$ENDIF};
begin
  Result := PhysicalMemory div 2;
  if (Result <= 0) or (Result > Cap) then Result := Cap;
end;

function Host_GCSuggestedMaxBytes(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
begin
  Res := MkNumber(DefaultHeapCeiling);
  Result := True;
end;

{ Goccia.gc.maxBytes: the ceiling in force (HeapCeiling) }
function Host_GCMaxBytes(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
begin
  if HeapCeiling <= 0 then HeapCeiling := DefaultHeapCeiling;
  Res := MkNumber(HeapCeiling);
  Result := True;
end;

{ ---- URL (Goccia.URL.Parser) ---- }

function ArgStr(Args: PValue; Argc, I: Integer): UnicodeString;
begin
  if (I <= Argc) and IsString(Args[I]) then Result := StrToUnicode(StrOf(Args[I])) else Result := '';
end;

function U8(const S: UnicodeString): RawByteString;
begin
  Result := UTF8Encode(S);
end;

function FromU8(const S: RawByteString): TValue;
begin
  Result := StrFromUnicode(UTF8Decode(S));
end;

{ the array at the temp root Slot, with V pushed (V rooted by the caller or a primitive) }
procedure PushAt(Slot: Integer; V: TValue);
begin
  PushRoot(V);
  ArrayPush(ObjOf(RootAt(Slot)), RootAt(TempTop - 1));
  PopRoots(1);
end;

{ the URL record as url.js reads it }
function RecordValue(const R: TGocciaURLRecord): TValue;
var Slot: Integer;
begin
  PushRoot(MkPtr(NewArray(0, 12)));
  Slot := TempTop - 1;
  PushAt(Slot, FromU8(R.Scheme));
  PushAt(Slot, FromU8(R.Username));
  PushAt(Slot, FromU8(R.Password));
  PushAt(Slot, FromU8(R.Host));
  PushAt(Slot, MkBool(R.HasNullHost));
  PushAt(Slot, MkInt(R.Port));
  PushAt(Slot, FromU8(SerializePath(R)));
  PushAt(Slot, MkBool(R.HasOpaquePath));
  PushAt(Slot, FromU8(R.Query));
  PushAt(Slot, MkBool(R.HasNullQuery));
  PushAt(Slot, FromU8(R.Fragment));
  PushAt(Slot, MkBool(R.HasNullFragment));
  Result := RootAt(Slot);
  PopRoots(1);
end;

{ urlParse(input[, base]): the record, null when input does not parse, -1 when base does not }
function Host_URLParse(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var R, B: TGocciaURLRecord;
begin
  Result := True;
  if Argc >= 2 then
  begin
    B := ParseURL(U8(ArgStr(Args, Argc, 2)));
    if not B.IsValid then begin Res := MkInt(-1); Exit; end;
    R := ParseURLWithBase(U8(ArgStr(Args, Argc, 1)), B);
  end
  else R := ParseURL(U8(ArgStr(Args, Argc, 1)));
  if R.IsValid then Res := RecordValue(R) else Res := VNull;
end;

{ urlOrigin(scheme, host, hasNullHost, port) }
function Host_URLOrigin(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var R: TGocciaURLRecord;
begin
  R := Default(TGocciaURLRecord);
  R.Scheme := U8(ArgStr(Args, Argc, 1));
  R.Host := U8(ArgStr(Args, Argc, 2));
  R.HasNullHost := (Argc >= 3) and (Args[3] = VTrue);
  if Argc >= 4 then R.Port := Round(PrimitiveToNumber(Args[4])) else R.Port := URL_NULL_PORT;
  Res := FromU8(SerializeOrigin(R));
  Result := True;
end;

{ urlEncode(kind, input, special): kind 0 userinfo, 1 query, 2 fragment, 3 path }
function Host_URLEncode(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var S: RawByteString;
begin
  S := U8(ArgStr(Args, Argc, 2));
  case Round(PrimitiveToNumber(Args[1])) of
    0: S := PercentEncodeForUserinfo(S);
    1: S := PercentEncodeForQuery(S, (Argc >= 3) and (Args[3] = VTrue));
    2: S := PercentEncodeForFragment(S);
  else
    S := PercentEncodeForPath(S);
  end;
  Res := FromU8(S);
  Result := True;
end;

function Host_URLIsSpecial(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
begin
  Res := MkBool(IsSpecialScheme(U8(ArgStr(Args, Argc, 1))));
  Result := True;
end;

function Host_URLDefaultPort(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
begin
  Res := MkInt(DefaultPortForScheme(U8(ArgStr(Args, Argc, 1))));
  Result := True;
end;

{ formParse(input): names and values interleaved }
function Host_FormParse(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var Names, Values: TArray<string>; I, Slot: Integer;
begin
  ParseFormEncoded(U8(ArgStr(Args, Argc, 1)), Names, Values);
  PushRoot(MkPtr(NewArray(0, 2 * Length(Names))));
  Slot := TempTop - 1;
  for I := 0 to High(Names) do
  begin
    PushAt(Slot, FromU8(Names[I]));
    PushAt(Slot, FromU8(Values[I]));
  end;
  Res := RootAt(Slot);
  PopRoots(1);
  Result := True;
end;

function Host_FormSerialize(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
begin
  Res := FromU8(SerializeFormEncoded(U8(ArgStr(Args, Argc, 1)), U8(ArgStr(Args, Argc, 2))));
  Result := True;
end;

{ plainCloneable(v): an ordinary object structuredClone copies property by property (a plain object, an array or
  an unmapped arguments object), not an exotic or platform object it cannot serialize }
function Host_PlainCloneable(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
begin
  Res := MkBool((Argc >= 1) and IsObject(Args[1]) and (ObjOf(Args[1])^.OKind in [okPlain, okArray, okArgumentsOrd]));
  Result := True;
end;

{ viewInfo(view): [length-tracking, out of bounds] of a typed array or a DataView (25.3.1.3 IsViewOutOfBounds:
  the DataView getters answer +0 for one out of bounds, so its clone asks here) }
function Host_ViewInfo(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var O, B: PObj; Tracking, OOB: Boolean; BL: Int64;
begin
  Tracking := False; OOB := False;
  if (Argc >= 1) and IsObject(Args[1]) then
  begin
    O := ObjOf(Args[1]);
    if O^.OKind = okTypedArray then
    begin
      Tracking := O^.Aux2 = MkInt(-1);
      OOB := TAOutOfBounds(O);
    end
    else if O^.OKind = okDataView then
    begin
      Tracking := O^.Aux2 = MkInt(-1);
      B := ObjOf(O^.Aux1);
      if BufferDetached(B) then OOB := True
      else
      begin
        BL := BufferLen(B);
        if Tracking then OOB := O^.Pad > BL else OOB := O^.Pad + IntOf(O^.Aux2) > BL;
      end;
    end;
  end;
  PushRoot(MkPtr(NewArray(0, 2)));
  PushAt(TempTop - 1, MkBool(Tracking));
  PushAt(TempTop - 1, MkBool(OOB));
  Res := RootAt(TempTop - 1);
  PopRoots(1);
  Result := True;
end;

{ isBytecode(f): f is a compiled JS function (a prelude's or a script's), not a native one }
function Host_IsBytecode(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
begin
  Res := MkBool((Argc >= 1) and IsObject(Args[1]) and (ObjOf(Args[1])^.OKind = okFunction));
  Result := True;
end;

{ writeLine(text): text and a line break on standard output }
threadvar
  ConsoleSink: TConsoleSink;

procedure SetConsoleSink(Sink: TConsoleSink);
begin
  ConsoleSink := Sink;
end;

{ writeLine(line, method) }
function Host_WriteLine(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var Sink: TConsoleSink;
begin
  Sink := ConsoleSink;
  if Assigned(Sink) then Sink(ArgStr(Args, Argc, 2), ArgStr(Args, Argc, 1))
  else WriteLn(UTF8Encode(ArgStr(Args, Argc, 1)));
  Res := VUndefined;
  Result := True;
end;

{ hrNow(): a monotonic time in milliseconds (an event's timeStamp) }
function Host_HrNow(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
begin
  Res := MkNumber(GetTickCount64);
  Result := True;
end;

{ G2.Modules's host hook: shared.loadHostModule(Key) }
function HostModule(const Key: UnicodeString; var Exp: TValue): Integer;
var A: array[0..0] of TValue; S: TValue; I: Integer;
begin
  if Assigned(MockModules) then
  begin
    I := MockModules.IndexOfName(string(Key));
    if I >= 0 then
    begin
      Exp := StrFromUnicode(UnicodeString(MockModules.ValueFromIndex[I]));
      Exit(2);
    end;
  end;
  if not IsObject(LoadHostModuleV) then Exit(0);
  A[0] := StrFromUnicode(Key);
  if not CallValue(LoadHostModuleV, SharedV, @A[0], 1, Exp) then Exit(-1);
  if Exp = VUndefined then Result := 0
  else if IsString(Exp) then Result := 2
  else Result := 1;
  S := VUndefined;
end;

{ G2.Modules's resolution hook: the host's own modules by their keys, a mock by its address, anything else by the
  CLIs' resolver (Goccia.ModuleResolver) }
var
  HostResolver: TModuleResolver;
  { a host's resolver, in place of HostResolver }
  ProvidedResolver: TModuleResolver;
  ImportSpecifiers: TStringList;   { module key = the first specifier that resolved to it }

procedure SetHostResolver(R: TModuleResolver);
begin
  ProvidedResolver := R;
end;

function HostResolveSpec(const Spec, Referrer: UnicodeString; var Resolved: UnicodeString): Integer;
var A: array[0..0] of TValue; R: TValue; Ref: string; Resolver: TModuleResolver;
begin
  if IsObject(HasHostModuleV) then
  begin
    A[0] := StrFromUnicode(Spec);
    if CallValue(HasHostModuleV, SharedV, @A[0], 1, R) and (R = VTrue) then
    begin
      Resolved := Spec;
      Exit(1);
    end;
    Pending := VUndefined;
  end;
  Resolver := ProvidedResolver;
  if Resolver = nil then
  begin
    if HostResolver = nil then
    begin
      HostResolver := TModuleResolver.Create;
      HostResolver.SetExtensions(ModuleImportExtensions);
    end;
    Resolver := HostResolver;
  end;
  if (Referrer = '') or (Referrer[1] = '<') then Ref := '' else Ref := ExpandFileName(UTF8Encode(Referrer));
  try
    Resolved := UTF8Decode(Resolver.Resolve(UTF8Encode(Spec), Ref));
    { the first specifier a module is imported by names it in its load errors (specifierOf) }
    if ImportSpecifiers = nil then ImportSpecifiers := TStringList.Create;
    if ImportSpecifiers.IndexOfName(UTF8Encode(Resolved)) < 0 then
      ImportSpecifiers.Add(UTF8Encode(Resolved) + '=' + UTF8Encode(Spec));
    Result := 1;
  except
    on E: Exception do
    begin
      Resolved := UTF8Decode(E.Message);
      Result := -1;
    end;
  end;
end;

{ readText(path): a file's text, decoded as UTF-8 without a BOM; undefined when it cannot be read }
function Host_ReadText(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var F: TFileStream; B: RawByteString; Path: string;
begin
  Res := VUndefined;
  Result := True;
  Path := UTF8Encode(ArgStr(Args, Argc, 1));
  if (Path = '') or not FileExists(Path) then Exit;
  try
    F := TFileStream.Create(Path, fmOpenRead or fmShareDenyNone);
    try
      SetLength(B, F.Size);
      if F.Size > 0 then F.ReadBuffer(B[1], F.Size);
    finally
      F.Free;
    end;
  except
    Exit;
  end;
  if (Length(B) >= 3) and (B[1] = #$EF) and (B[2] = #$BB) and (B[3] = #$BF) then Delete(B, 1, 3);
  SetCodePage(B, CP_UTF8, False);
  Res := StrFromUnicode(UTF8Decode(B));
end;

{ fileSize(path): a file's size in bytes, -1 when it cannot be read }
function Host_FileSize(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var SR: TSearchRec; Path: string;
begin
  Res := MkInt(-1);
  Result := True;
  Path := UTF8Encode(ArgStr(Args, Argc, 1));
  if FindFirst(Path, faAnyFile, SR) = 0 then
  begin
    Res := MkNumber(SR.Size);
    FindClose(SR);
  end;
end;

{ setClock(epochMs, monoMs): the mocked clocks (NaN: the real one) }
function Host_SetClock(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var E, M: Double;
begin
  if Argc >= 1 then E := PrimitiveToNumber(Args[1]) else E := NaN;
  if Argc >= 2 then M := PrimitiveToNumber(Args[2]) else M := NaN;
  SetClockOverride(E, M);
  Res := VUndefined;
  Result := True;
end;

{ realNow(): the real epoch time in milliseconds, whatever is mocked }
function Host_RealNow(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var TS: TTimeSpec;
begin
  clock_gettime(CLOCK_REALTIME, @TS);
  Res := MkNumber(TS.tv_sec * 1000.0 + TS.tv_nsec div 1000000);
  Result := True;
end;

{ asyncGet() and asyncSet(v): the async context (G2.Promise) }
function Host_AsyncGet(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
begin
  Res := GetAsyncContext;
  Result := True;
end;

function Host_AsyncSet(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
begin
  if Argc >= 1 then SetAsyncContext(Args[1]) else SetAsyncContext(VUndefined);
  Res := VUndefined;
  Result := True;
end;

{ astParse(source, jsx, module, tokens, expressions, fileName): goccia:ast's parse as JSON text (Goccia.AST.Report);
  a source that does not parse throws its SyntaxError }
function Host_ASTParse(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var Json: string;
  function Flag(I: Integer): Boolean;
  begin
    Result := (Argc >= I) and (Args[I] = VTrue);
  end;
begin
  try
    Json := ASTReportJSON(U8(ArgStr(Args, Argc, 1)), ActivePipelineOptions(False), Flag(2), Flag(3), Flag(4), Flag(5),
      U8(ArgStr(Args, Argc, 6)));
  except
    on E: EGocciaASTSyntaxError do Exit(ThrowError('SyntaxError', UTF8Decode(E.Message)));
  end;
  Res := FromU8(Json);
  Result := True;
end;

{ dataParse(kind, text): a parse as Goccia.DataTree's JSON (DataTreeToJSON), which datatree.js rebuilds: kind 0 YAML
  (YAML.parse), 1 the YAML documents (YAML.parseDocuments), 2 TOML. Text that does not parse throws a SyntaxError,
  nesting past the parsers' depth limit a RangeError, as the CLIs' modules do. }
function Host_DataParse(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var Tree: TGocciaDataTree; YAML: TGocciaYAMLParser; TOML: TGocciaTOMLParser; Text, Json: UnicodeString;
begin
  Text := ArgStr(Args, Argc, 2);
  Tree := nil;
  try
    try
      case Round(PrimitiveToNumber(Args[1])) of
        0, 1:
          begin
            YAML := TGocciaYAMLParser.Create;
            try
              if Round(PrimitiveToNumber(Args[1])) = 0 then Tree := YAML.Parse(Text) else Tree := YAML.ParseDocuments(Text);
            finally
              YAML.Free;
            end;
          end;
      else
        TOML := TGocciaTOMLParser.Create;
        try
          Tree := TOML.Parse(Text);
        finally
          TOML.Free;
        end;
      end;
      Json := DataTreeToJSON(Tree.Root);
    finally
      Tree.Free;
    end;
  except
    on E: EGocciaYAMLParseError do Exit(ThrowError('SyntaxError', UTF8Decode(E.Message)));
    on E: EGocciaTOMLParseError do Exit(ThrowError('SyntaxError', UTF8Decode(E.Message)));
    on E: EGocciaDataDepthError do Exit(ThrowError('RangeError', UTF8Decode(E.Message)));
    on E: EGocciaDataTypeError do Exit(ThrowError('TypeError', UTF8Decode(E.Message)));
    { any other parser failure is still the text's: never let a Pascal exception into the VM }
    on E: Exception do Exit(ThrowError('SyntaxError', UTF8Decode(E.Message)));
  end;
  Res := StrFromUnicode(Json);
  Result := True;
end;

{ RFC 8089: a host path as a file: URL (absolute, percent-encoded), as GocciaScript's import.meta.url has it }
function FileURLOf(const Path: string): string;
var Abs: string;
begin
  Abs := ExpandFileName(Path);
  {$IFDEF MSWINDOWS}
  Abs := StringReplace(Abs, '\', '/', [rfReplaceAll]);
  if (Length(Abs) >= 2) and (Abs[1] = '/') and (Abs[2] = '/') then Exit(FILE_URL_SCHEME + PercentEncodePath(Abs));
  Result := FILE_URL_SCHEME + '///' + PercentEncodePath(Abs);
  {$ELSE}
  Result := FILE_URL_SCHEME + '//' + PercentEncodePath(Abs);
  {$ENDIF}
end;

{ fileURL(path): the file: URL of a host path }
function Host_FileURL(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
begin
  Res := StrFromUnicode(UTF8Decode(FileURLOf(UTF8Encode(ArgStr(Args, Argc, 1)))));
  Result := True;
end;

{ metaResolve(modulePath, argc, specifier): import.meta.resolve(specifier) of the module at modulePath, as
  GocciaScript resolves it: an absolute specifier as it is, any other against the module's directory, as a file:
  URL (the file need not exist) }
function Host_MetaResolve(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var Spec, Path: string; S: TValue;
begin
  if (Argc < 2) or (PrimitiveToNumber(Args[2]) < 1) then
    Exit(ThrowTypeError('import.meta.resolve requires a specifier argument'));
  if (Argc < 3) or not ToStringV(Args[3], S) then Exit(False);
  Spec := UTF8Encode(StrToUnicode(StrOf(S)));
  if (Spec <> '') and ((Spec[1] = '/') or (Spec[1] = '\') or ((Length(Spec) >= 2) and (Spec[2] = ':'))) then
    Path := ExpandFileName(Spec)
  else
    Path := ExpandFileName(IncludeTrailingPathDelimiter(ExtractFilePath(ExpandFileName(
      UTF8Encode(ArgStr(Args, Argc, 1))))) + Spec);
  Res := StrFromUnicode(UTF8Decode(FileURLOf(Path)));
  Result := True;
end;

{ G2.Modules's import.meta hook: shared.initImportMeta(meta, path) (modules.js) }
function HostInitImportMeta(Meta: TValue; const Path: UnicodeString): Boolean;
var A: array[0..1] of TValue; F, R: TValue;
begin
  Result := True;
  if not IsObject(SharedV) then Exit;
  PushRoot(Meta);
  A[1] := StrFromUnicode(Path);
  PushRoot(A[1]);
  if not GetV(SharedV, AtomOf('initImportMeta'), F) or not IsCallable(F) then begin PopRoots(2); Exit; end;
  A[0] := RootAt(TempTop - 2);
  A[1] := RootAt(TempTop - 1);
  Result := CallValue(F, SharedV, @A[0], 2, R);
  PopRoots(2);
end;

{ specifierOf(key): the specifier the module at key was first imported by (key itself when none was recorded) }
function Host_SpecifierOf(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var Key: string; I: Integer;
begin
  Key := UTF8Encode(ArgStr(Args, Argc, 1));
  I := -1;
  if ImportSpecifiers <> nil then I := ImportSpecifiers.IndexOfName(Key);
  if I >= 0 then Res := StrFromUnicode(UTF8Decode(ImportSpecifiers.ValueFromIndex[I]))
  else Res := StrFromUnicode(UTF8Decode(Key));
  Result := True;
end;

{ G2.Modules's JSON module exports hook: shared.jsonExports(value) (modules.js) }
function HostJSONExports(V: TValue; var Exp: TValue): Boolean;
var F: TValue; A: array[0..0] of TValue;
begin
  Exp := VUndefined;
  Result := True;
  if not IsObject(SharedV) then Exit;
  PushRoot(V);
  if not GetV(SharedV, AtomOf('jsonExports'), F) or not IsCallable(F) then begin PopRoots(1); Exit; end;
  A[0] := RootAt(TempTop - 1);
  Result := CallValue(F, SharedV, @A[0], 1, Exp);
  PopRoots(1);
end;

{ vitestSource(): the vitest compatibility shim's module source (Goccia.VitestShim.inc) }
function VitestShimSource: UnicodeString;
const
  LB = #10;
begin
  {$I Goccia.VitestShim.inc}
end;

function Host_VitestSource(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
begin
  Res := StrFromUnicode(VitestShimSource);
  Result := True;
end;

{ ---- installing the preludes ---- }

{ calls the prelude's function value (Source's completion) with the Argc temp roots from Base; Res its result }
function CallPrelude(const Name, Source: UnicodeString; Base, Argc: Integer; var Res: TValue;
  var Err: UnicodeString): Boolean;
var T: PFuncTemplate; Clo, Fn, S: TValue; A: array[0..15] of TValue; I: Integer;
begin
  Result := False;
  if not CompilePrelude(Name, Source, T, Err) then Exit;
  if MakeScriptClosure(T, Clo) and CallValue(Clo, VUndefined, nil, 0, Fn) then
  begin
    PushRoot(Fn);
    for I := 0 to Argc - 1 do A[I] := RootAt(Base + I);
    Result := CallValue(RootAt(TempTop - 1), VUndefined, @A[0], Argc, Res);
    PopRoots(1);
  end;
  if not Result then
  begin
    if ToStringV(Pending, S) and IsString(S) then Err := StrToUnicode(StrOf(S)) else Err := Name + ' failed';
    Pending := VUndefined;
  end;
end;

{ the natives object the host preludes share }
function HostNatives: TValue;
begin
  PushRoot(MkPtr(NewPlainObject(ObjectProto, 16)));
  DefineNative(RootAt(TempTop - 1), 'gc', Host_GC, 0);
  DefineNative(RootAt(TempTop - 1), 'gcBytes', Host_GCBytes, 0);
  DefineNative(RootAt(TempTop - 1), 'gcMaxBytes', Host_GCMaxBytes, 0);
  DefineNative(RootAt(TempTop - 1), 'gcSuggestedMaxBytes', Host_GCSuggestedMaxBytes, 0);
  DefineNative(RootAt(TempTop - 1), 'urlParse', Host_URLParse, 2);
  DefineNative(RootAt(TempTop - 1), 'urlOrigin', Host_URLOrigin, 4);
  DefineNative(RootAt(TempTop - 1), 'urlEncode', Host_URLEncode, 3);
  DefineNative(RootAt(TempTop - 1), 'isSpecial', Host_URLIsSpecial, 1);
  DefineNative(RootAt(TempTop - 1), 'defaultPort', Host_URLDefaultPort, 1);
  DefineNative(RootAt(TempTop - 1), 'formParse', Host_FormParse, 1);
  DefineNative(RootAt(TempTop - 1), 'formSerialize', Host_FormSerialize, 2);
  DefineNative(RootAt(TempTop - 1), 'plainCloneable', Host_PlainCloneable, 1);
  DefineNative(RootAt(TempTop - 1), 'viewInfo', Host_ViewInfo, 1);
  DefineNative(RootAt(TempTop - 1), 'isBytecode', Host_IsBytecode, 1);
  DefineNative(RootAt(TempTop - 1), 'hrNow', Host_HrNow, 0);
  DefineNative(RootAt(TempTop - 1), 'writeLine', Host_WriteLine, 1);
  DefineNative(RootAt(TempTop - 1), 'semver', Host_Semver, 6);
  DefineNative(RootAt(TempTop - 1), 'setClock', Host_SetClock, 2);
  DefineNative(RootAt(TempTop - 1), 'realNow', Host_RealNow, 0);
  DefineNative(RootAt(TempTop - 1), 'asyncGet', Host_AsyncGet, 0);
  DefineNative(RootAt(TempTop - 1), 'asyncSet', Host_AsyncSet, 1);
  DefineNative(RootAt(TempTop - 1), 'vitestSource', Host_VitestSource, 0);
  DefineNative(RootAt(TempTop - 1), 'astParse', Host_ASTParse, 6);
  DefineNative(RootAt(TempTop - 1), 'dataParse', Host_DataParse, 2);
  DefineNative(RootAt(TempTop - 1), 'fileURL', Host_FileURL, 1);
  DefineNative(RootAt(TempTop - 1), 'specifierOf', Host_SpecifierOf, 1);
  DefineNative(RootAt(TempTop - 1), 'metaResolve', Host_MetaResolve, 3);
  DefineNative(RootAt(TempTop - 1), 'readText', Host_ReadText, 1);
  DefineNative(RootAt(TempTop - 1), 'fileSize', Host_FileSize, 1);
  Result := RootAt(TempTop - 1);
  PopRoots(1);
end;

function InstallHostGlobals(ShadowRealm, TestMode: Boolean; const ConfigJson: UnicodeString;
  var Err: UnicodeString; ExposeTimers: Boolean): Boolean;
var Goccia, R: TValue; Base: Integer; T: PFuncTemplate;
begin
  if HeapCeiling <= 0 then HeapCeiling := DefaultHeapCeiling;
  if FirstInEngine(EventLoopRootedGen) then
  begin
    AddRoot(@FireNextV); AddRoot(@SharedV); AddRoot(@LoadHostModuleV); AddRoot(@HasHostModuleV);
  end;
  FireNextV := VUndefined;
  SharedV := VUndefined;
  LoadHostModuleV := VUndefined;
  HasHostModuleV := VUndefined;
  Base := TempTop;
  PushRoot(GlobalObj);
  PushRoot(HostNatives);
  { the preludes' shared internals, runtimeGlobals the names they install }
  PushRoot(MkPtr(NewPlainObject(ObjectProto, 8)));
  PushRoot(MkPtr(NewArray(0, 16)));
  DefineOwn(RootAt(Base + 2), AtomOf('runtimeGlobals'), RootAt(TempTop - 1), pfHidden);
  PopRoots(1);
  PushRoot(StrFromUnicode(InfoJson));
  PushRoot(MkBool(TestMode));
  PushRoot(MkBool(ShadowRealm));
  PushRoot(StrFromUnicode(ConfigJson));
  { (G, n, shared), then host.js's (G, n, shared, infoJson, testMode, shadowRealm, configJson), in the order the
    CLIs' runtime extensions install (Goccia.runtimeGlobals lists their names in it) }
  Result := CallPrelude('<goccia:modules>', ModulesSrc, Base, 3, R, Err) and
    CallPrelude('<goccia:console>', ConsoleSrc, Base, 3, R, Err) and
    CallPrelude('<goccia:url>', UrlSrc, Base, 3, R, Err) and CallPrelude('<goccia:web>', WebSrc, Base, 3, R, Err) and
    CallPrelude('<goccia:events>', EventsSrc, Base, 3, R, Err) and
    CallPrelude('<goccia:fetch>', FetchSrc, Base, 3, R, Err) and
    CallPrelude('<goccia:json5>', Json5Src, Base, 3, R, Err) and
    CallPrelude('<goccia:csv>', DelimitedSrc, Base, 3, R, Err) and
    CallPrelude('<goccia:jsonl>', JsonlSrc, Base, 3, R, Err) and
    CallPrelude('<goccia:yaml>', DataTreeSrc, Base, 3, R, Err) and
    CallPrelude('<goccia:semver>', SemverSrc, Base, 3, R, Err) and
    CallPrelude('<goccia:host>', HostSrc, Base, 7, Goccia, Err);
  if Result then
  begin
    PushRoot(Goccia);
    { the timer globals and goccia:timers, or only the queue (ExposeTimers False) }
    if not ExposeTimers then DefineOwn(RootAt(Base + 2), AtomOf('exposeTimers'), VFalse, pfDefault);
    Result := CallPrelude('<goccia:timers>', TimersSrc, Base, 3, R, Err) and
      CallPrelude('<node:async_hooks>', AsyncHooksSrc, Base, 3, R, Err);
    Goccia := RootAt(TempTop - 1);
    { shared.timers.fireNext, for the event loop }
    if Result and GetV(RootAt(Base + 2), AtomOf('timers'), R) and IsObject(R) then
      Result := GetV(R, AtomOf('fireNext'), FireNextV);
    SharedV := RootAt(Base + 2);
    if Result then Result := GetV(SharedV, AtomOf('loadHostModule'), LoadHostModuleV) and
      GetV(SharedV, AtomOf('hasHostModule'), HasHostModuleV);
    if Result then
    begin
      SetHostModuleHook(@HostModule);
      ImportMetaInitHook := @HostInitImportMeta;
      JSONExportsHook := @HostJSONExports;
      SetHostResolveHook(@HostResolveSpec);
    end;
  end;
  PopRoots(TempTop - Base);
  if not Result then Exit;
  { Goccia: a const binding of the global scope, mirrored on the global object as the CLIs mirror theirs }
  DefineOwn(GlobalObj, AtomOf('Goccia'), Goccia, pfHidden);
  Result := ParseAndCompile('const Goccia = globalThis.Goccia;', '<goccia:host>', False, T, Err, 1) and RunScript(T);
  if not Result and (Err = '') then Err := 'Goccia binding failed';
end;

function InstallTestingAPI(var Err: UnicodeString): Boolean;
var Base: Integer;
begin
  if FirstInEngine(TestApiRootedGen) then AddRoot(@TestApiV);
  Base := TempTop;
  PushRoot(GlobalObj);
  PushRoot(SharedV);
  Result := CallPrelude('<goccia:test>', TestingSrc, Base, 2, TestApiV, Err);
  PopRoots(2);
end;

{ report's done(json, error) }
function Test_Done(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var S: TValue;
begin
  ReportDone := True;
  if (Argc >= 1) and IsString(Args[1]) then ReportJson := StrToUnicode(StrOf(Args[1]))
  else if (Argc >= 2) and ToStringV(Args[2], S) and IsString(S) then ReportErr := StrToUnicode(StrOf(S))
  else ReportErr := 'the test run failed';
  Res := VUndefined;
  Result := True;
end;

procedure HoistTestMocks(const FileName: string; const Source: UnicodeString);
var Lines: TStringList; R: TMockResolver; Mocks: TGocciaMockModules; I: Integer;
begin
  if (Pos('vi.mock', Source) = 0) and (Pos('vi.unmock', Source) = 0) and (Pos('vitest.mock', Source) = 0) and
     (Pos('vitest.unmock', Source) = 0) then Exit;
  Lines := CreateFileTextLines(NormalizeNewlinesToLF(UTF8Encode(Source)));
  R := TMockResolver.Create;
  try
    Mocks := CollectModuleMocks(Lines, ExpandFileName(FileName), [ppJSX], R.Resolve);
  finally
    R.Free;
    Lines.Free;
  end;
  if Length(Mocks) = 0 then Exit;
  if not Assigned(MockModules) then
  begin
    MockModules := TStringList.Create;
    MockModules.NameValueSeparator := #1;
  end;
  for I := 0 to High(Mocks) do MockModules.Add(Mocks[I].Address + #1 + Mocks[I].Source);
end;

function RunEventLoopIn(Intervals: Boolean): Boolean;
var R: TValue; A: array[0..0] of TValue;
begin
  while True do
  begin
    if not (RunMicrotasks and RunHostLoop) then Exit(False);
    { a FinalizationRegistry's cleanup: one job (one dead cell) at a time, the microtasks it queues before the
      next (9.13 HostEnqueueFinalizationRegistryCleanupJob) }
    if WeakCleanupPending then
    begin
      ClearKeptObjects;
      if not RunWeakCleanup then Exit(False);
      Continue;
    end;
    if not IsObject(FireNextV) or ReportDone then Exit(True);
    ClearKeptObjects;
    A[0] := MkBool(Intervals);
    if not CallValue(FireNextV, VUndefined, @A[0], 1, R) then Exit(False);
    if R <> VTrue then Exit(True);
  end;
end;

function RunEventLoop: Boolean;
begin
  Result := RunEventLoopIn(False);
end;

function RunTestSuites(var Json, Err: UnicodeString): Boolean;
var A: array[0..0] of TValue; Report, R, S: TValue;
begin
  ReportJson := ''; ReportErr := ''; ReportDone := False;
  if not GetV(TestApiV, AtomOf('report'), Report) then Exit(False);
  PushRoot(NewNativeFunction(Test_Done, 'done', 2));
  A[0] := RootAt(TempTop - 1);
  Result := CallValue(Report, TestApiV, @A[0], 1, R) and RunEventLoopIn(True);
  PopRoots(1);
  if not Result then
  begin
    if ToStringV(Pending, S) and IsString(S) then Err := StrToUnicode(StrOf(S)) else Err := 'uncaught exception';
    Pending := VUndefined;
    Exit;
  end;
  if not ReportDone then Exit(False);
  Json := ReportJson;
  Err := ReportErr;
  Result := ReportErr = '';
end;

finalization
  FreeAndNil(HostResolver);
  FreeAndNil(ImportSpecifiers);
end.
