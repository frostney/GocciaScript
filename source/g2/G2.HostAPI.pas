unit G2.HostAPI;
{ The CLI host on G2: what GocciaRunner and GocciaTestRunner give a script beyond the language. The API surfaces
  are JS preludes (prelude/host.js, prelude/testing.js) called with the natives they need from here; this unit
  installs them on the thread's realm, keeps what the host reads back, and reports a test file's results.

  The conformance host (g2 --test262) installs none of this: its realm keeps print, eval and the Annex B
  globals the CLIs leave out. }
{$mode delphiunicode}{$H+}
{$MINFPCONSTPREC 64}

interface

uses
  G2.Core;

{ the host's globals on this thread's realm (after EngineInit): the const Goccia, and the conformance host's
  extras removed; TestMode lists the test runner's globals in Goccia.runtimeGlobals }
function InstallHostGlobals(ShadowRealm, TestMode: Boolean; var Err: UnicodeString): Boolean;
{ the test framework API (describe, test, expect, ...) as globals of this thread's realm }
function InstallTestingAPI(var Err: UnicodeString): Boolean;
{ runs the suites the test file registered (after InstallTestingAPI) and the jobs they queue; Json their result
  (totalTests, passed, failed, skipped, assertions, failedTests), or False and Err the error that stopped
  them }
function RunTestSuites(var Json, Err: UnicodeString): Boolean;

implementation

uses
  SysUtils, Classes, G2.Bytecode, G2.VM, G2.Typed, G2.Snapshot, G2.Promise, G2.Atomics, G2.Eval,
  Goccia.Spec.Data, Goccia.Version, Goccia.Platform, Goccia.URL.Parser;

{$I G2.Testing.inc}
{$I G2.HostPrelude.inc}
{$I G2.UrlPrelude.inc}
{$I G2.WebPrelude.inc}

threadvar
  TestApiV: TValue;
  TestApiRooted: Boolean;
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

{ Goccia.gc.maxBytes and suggestedMaxBytes: half of physical memory, at most 8 GB (docs/built-ins.md) }
function Host_GCMaxBytes(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
const Cap = Int64(8) * 1024 * 1024 * 1024;
var M: Int64;
begin
  M := PhysicalMemory div 2;
  if (M <= 0) or (M > Cap) then M := Cap;
  Res := MkNumber(M);
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

{ viewInfo(view): [length-tracking, out of bounds] of a typed array (a DataView: [length-tracking, false]) }
function Host_ViewInfo(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var O: PObj; Tracking, OOB: Boolean;
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
    else if O^.OKind = okDataView then Tracking := O^.Aux2 = MkInt(-1);
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
  Result := RootAt(TempTop - 1);
  PopRoots(1);
end;

function InstallHostGlobals(ShadowRealm, TestMode: Boolean; var Err: UnicodeString): Boolean;
var Goccia, R: TValue; Base: Integer; T: PFuncTemplate;
begin
  Base := TempTop;
  PushRoot(GlobalObj);
  PushRoot(HostNatives);
  PushRoot(StrFromUnicode(InfoJson));
  PushRoot(MkBool(TestMode));
  PushRoot(MkBool(ShadowRealm));
  { (G, n) }
  Result := CallPrelude('<goccia:url>', UrlSrc, Base, 2, R, Err) and CallPrelude('<goccia:web>', WebSrc, Base, 2, R, Err);
  { (G, n, infoJson, testMode, shadowRealm) }
  if Result then Result := CallPrelude('<goccia:host>', HostSrc, Base, 5, Goccia, Err);
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
  if not TestApiRooted then begin AddRoot(@TestApiV); TestApiRooted := True; end;
  Base := TempTop;
  PushRoot(GlobalObj);
  Result := CallPrelude('<goccia:test>', TestingSrc, Base, 1, TestApiV, Err);
  PopRoots(1);
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

function RunTestSuites(var Json, Err: UnicodeString): Boolean;
var A: array[0..0] of TValue; Report, R, S: TValue;
begin
  ReportJson := ''; ReportErr := ''; ReportDone := False;
  if not GetV(TestApiV, AtomOf('report'), Report) then Exit(False);
  PushRoot(NewNativeFunction(Test_Done, 'done', 2));
  A[0] := RootAt(TempTop - 1);
  Result := CallValue(Report, TestApiV, @A[0], 1, R) and RunMicrotasks and RunHostLoop;
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

end.
