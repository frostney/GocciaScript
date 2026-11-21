unit G2.Modules;
{ 16.2.1 Modules: Source Text Module Records loaded from files, linked and evaluated; module namespace objects
  (10.4.6, G2.VM's okModuleNS); import() and import.meta.

  A module compiles (goal 3, G2.Compiler) to a generator over its cell table: calling it is InitializeEnvironment
  (its function declarations made, its exported bindings published as cells), resuming it is ExecuteModule.
  Imports are the exporters' cells in the importer's table. Linking runs every module's start first, then fills
  the import slots, so cycles see each other's functions. A module with top-level await is an async generator;
  its evaluation waits for it (a simplification of 16.2.1.5.3's asynchronous ordering). }
{$mode delphiunicode}{$H+}
{$I G2.Checks.inc}
{$MINFPCONSTPREC 64}

interface

uses
  G2.Core;

procedure InitModules;
{ 28.1.1 %AbstractModuleSource% of the running realm (made when asked: no module source exists yet) }
function AbstractModuleSourceIntrinsic: TValue;
{ loads, links and evaluates the module at Path (the host's entry point); False with the error pending, Phase
  0 when it is a parse or link (resolution) error, 1 an evaluation error }
function RunModuleFile(const Path: UnicodeString; var Phase: Integer): Boolean;
{ a module a host evaluates before its entry (a globals module, a module manifest): loaded, linked and evaluated
  (its top-level await settled through the microtasks), its namespace in NS; the jobs it leaves are the entry's }
function EvaluateModuleNamespace(const Path: UnicodeString; var NS: TValue): Boolean;
{ whether the entry RunModuleFile ran is still evaluating: its top-level await waits on what nothing settled }
function EntryEvaluationPending: Boolean;
{ the error a host's resolution refused a request with (its hook returning -2): the import throws it }
procedure SetResolveError(V: TValue);
{ whether the module at Path (an import's referrer) was loaded for the host }
function HostOwnedReferrer(const Path: UnicodeString): Boolean;
{ whether a module record of this thread is keyed Key (it was loaded, by the host or the script) }
function ModuleRecordExists(const Key: UnicodeString): Boolean;
{ an explicit Script entry's import declarations and re-exports (GocciaScript accepts them, docs/language.md, Entry
  source type), Source a module of only them (G2.Eval ScriptImportSource), keyed EntryPath + ScriptImportsSuffix,
  whose requests resolve as the entry's own would. Start loads and links it as one graph (a link error evaluates no
  module), then evaluates it, its microtasks run: Evaluation its promise, which the host settles through its event
  loop when a top-level await waits on more. Finish, once it fulfilled, binds each name it imports as a const global
  lexical binding on the binding it imports, for the Script to run against; False with the error pending. }
const ScriptImportsSuffix = '#script-imports';
function StartScriptImports(const EntryPath, Source: UnicodeString; var Evaluation: TValue): Boolean;
function FinishScriptImports(const EntryPath: UnicodeString; Evaluation: TValue): Boolean;

type
  { a host's modules: for a request key (a specifier with a scheme, as goccia:csv, kept as written; otherwise a
    resolved file path), 1 with Exp an array of the module's export names and values, interleaved (a
    synthetic module, evaluated at once), 2 with Exp the module's JavaScript source text, 0 when the host
    has no such module (a file is then read as JavaScript), -1 with an error pending }
  THostModuleHook = function(const Key: UnicodeString; var Exp: TValue): Integer;

  { a host's resolution of a specifier imported by Referrer: 1 with Resolved the request key (a host module's key or
    an absolute path), 0 to resolve it as G2 does, -1 with Resolved the message of the error it fails with }
  THostResolveHook = function(const Spec, Referrer: UnicodeString; var Resolved: UnicodeString): Integer;

{ this thread's host module hook (nil: none) }
procedure SetHostModuleHook(Hook: THostModuleHook);
{ 16.2.1.10 HostGetImportMetaProperties / HostFinalizeImportMeta: the host fills a module's import.meta (made
  with a null prototype) once, given the module's path; nil leaves it empty }
var ImportMetaInitHook: function(Meta: TValue; const Path: UnicodeString): Boolean = nil;
{ the host's exports of a JSON module (type: 'json') from its value, names and values interleaved (GocciaScript
  exports the top-level object's properties besides default); nil: default only, as ECMA-262 has it }
var JSONExportsHook: function(V: TValue; var Exp: TValue): Boolean = nil;
{ this thread's host resolution hook (nil: none) }
procedure SetHostResolveHook(Hook: THostResolveHook);
{ Spec as an import in Referrer resolves it: a host module's key, or an absolute path }
function ResolveModuleSpecifier(const Spec, Referrer: UnicodeString): UnicodeString;
{ the source of the module at Path, as the host has it (a module entry read from stdin, or a section of a file):
  loading Path takes it instead of reading the file. An empty Path clears it. }
procedure SetProvidedSource(const Path, Source: UnicodeString);
{ GocciaScript's experimental JavaScript module source objects (--experimental-js-module-source): a source text
  module's source phase import gives one }
procedure SetJSModuleSources(On: Boolean);
{ this thread's module records so far: a mark ModuleLoadedSince counts from }
function ModuleCount: Integer;
{ whether this thread's engine loaded a module from Path after Mark records (a host shows a code frame only of a
  module the script's run loaded, never of one the host evaluated before it, nor of a path a thrown value's stack
  names) }
function ModuleLoadedSince(const Path: UnicodeString; Mark: Integer): Boolean;
{ the entry's first module record (ModuleCount when the run starts): the records before it are the host's, and a
  module they import later is too (TModRec.HostOwned) }
procedure SetEntryModuleMark(Mark: Integer);
{ the provided entry's completion value (a host's --print): Wanted compiles it to keep the value, which
  EntryCompletion returns once it ran (undefined before) }
procedure SetEntryCompletionWanted(Wanted: Boolean);
{ the import declaration a static import failed to load at (the innermost importer's), for a host's diagnostic;
  False when no import failed since ClearLoadFailure }
procedure ClearLoadFailure;
function LoadFailure(out Path: UnicodeString; out Line, Column: Integer): Boolean;
{ the static import whose specifier the host is resolving now (while a module links): the importing module's path
  and the declaration's line and column; False otherwise }
function ImportSiteNow(out Path: UnicodeString; out Line, Column: Integer): Boolean;
function EntryCompletion: TValue;
type
  { a host's file system for this thread's engine: the bytes of the file at Path, False when there is none (a
    sandbox's virtual file system) }
  TFileReadHook = function(const Path: UnicodeString; var B: RawByteString): Boolean;
{ this thread's file reader (nil: the host's files) }
procedure SetFileReadHook(Hook: TFileReadHook);
{ the bytes of the file at Path through this thread's file reader; False when there is none }
function ReadFileBytes(const Path: UnicodeString; var B: RawByteString): Boolean;

implementation

uses
  SysUtils, Classes, Generics.Collections, G2.Bytecode, G2.VM, G2.Compiler, G2.Eval, G2.Promise;

const
  { the test262 host's module with a source phase representation (INTERPRETING.md: "<module source>") }
  HostSourceSpecifier = '<module source>';

type
  TModStatus = (msNew, msLinking, msLinked, msEvaluating, msEvaluatingAsync, msEvaluated);
  TModRec = class
    Id: Integer;
    Path: UnicodeString;
    Info: TModuleInfo;
    Tpl: PFuncTemplate;
    Status: TModStatus;
    Errored: Boolean;   { with Status msEvaluated: [[EvaluationError]] (rError); with msNew: a load error }
    Deps: array of TModRec;
    { 16.2.1.6 Cyclic Module Record fields of the evaluation }
    DFSAnc, PendingAsync: Integer;
    CycleRoot: TModRec;
    AsyncOrder: Int64;   { [[AsyncEvaluationOrder]]: 0 unset, -1 done }
    AsyncParents: array of TModRec;
    { loaded for the host: before the entry's run (EntryModuleMark), or imported by such a module, transitively }
    HostOwned: Boolean;
    destructor Destroy; override;
  end;
  TResKind = (rkNull, rkAmbiguous, rkBinding, rkNamespace, rkDeferNS, rkSource);
  TResolution = record
    Kind: TResKind;
    Module: TModRec;
    Slot: Integer;
  end;

const
  { the values of a module kept in ModRoots at Id * RS + k }
  RS = 7; rTable = 0; rGen = 1; rNS = 2; rMeta = 3; rError = 4; rDNS = 5; rCap = 6;

threadvar
  Mods: TList<TModRec>;
  { (SetEntryModuleMark) and, while a host-owned module's import loads, True: the records it makes are the host's }
  EntryModuleMark: Integer;
  HostLoading: Boolean;
  AsyncEvalCount: Int64;   { IncrementModuleAsyncEvaluationCount }
  ModRoots: TValue;     { a rooted array: RS slots per module }
  PendingImports: TValue;   { a rooted array of [promise, referrer, specifier] per import() in flight }
  { per import() in flight, the frames of its call (StackFramesNow): its failure, found in a job, is located there }
  ImportStacks: array of UnicodeString;
  AMSCtor: TValue;        { the realm's %AbstractModuleSource% (made when first asked for) }
  HostModSource: TValue;  { the realm's source object of the host's source module }
  { GocciaScript's experimental JavaScript module source objects (--experimental-js-module-source): each source
    text module's, by its Id, and the prototype they share (ModuleSource.prototype, inheriting from
    %AbstractModuleSource%.prototype); JSModSourceKey marks them }
  JSModSources: TValue;
  JSModSourceProto: TValue;
  JSModSourceKey: PStr;
  JSModuleSourcesOn: Boolean;
  { the evaluation promise of the entry RunModuleFile ran last }
  EntryEvalPromise: TValue;
  ResolveErrorV: TValue;

destructor TModRec.Destroy;
begin
  Info.Free;
  inherited;
end;


{ a module request as its import wrote it: the specifier without the type an import attribute added (#1 type) }
function RequestSpecifier(const Request: UnicodeString): UnicodeString;
var P: Integer;
begin
  P := Pos(#1, Request);
  if P > 0 then Result := Copy(Request, 1, P - 1) else Result := Request;
end;

{ GocciaScript's SyntaxError for an import of a name its module does not export }
function MissingExportMessage(const Request, Name: UnicodeString): UnicodeString;
begin
  Result := 'Module "' + RequestSpecifier(Request) + '" has no export named "' + Name + '"';
end;

function GetRoot(M: TModRec; K: Integer): TValue; inline;
begin
  Result := ObjOf(ModRoots)^.Elements^.Data[M.Id * RS + K];
end;

procedure SetRoot(M: TModRec; K: Integer; V: TValue);
begin
  while ObjOf(ModRoots)^.Elements^.Len <= M.Id * RS + K do
  begin
    PushRoot(V);
    ArrayPush(ObjOf(ModRoots), VUndefined);
    V := RootAt(TempTop - 1);
    PopRoots(1);
  end;
  ValueArrayStore(ObjOf(ModRoots)^.Elements, M.Id * RS + K, V);
end;

{ ---- loading ---- }

threadvar
  HostModules: THostModuleHook;
  HostResolve: THostResolveHook;
  FileRead: TFileReadHook;

procedure SetFileReadHook(Hook: TFileReadHook);
begin
  FileRead := Hook;
end;

function ReadFileBytes(const Path: UnicodeString; var B: RawByteString): Boolean;
var F: TFileStream;
begin
  B := '';
  if Assigned(FileRead) then Exit(FileRead(Path, B));
  Result := False;
  if (Path = '') or not FileExists(string(Path)) then Exit;
  try
    F := TFileStream.Create(string(Path), fmOpenRead or fmShareDenyNone);
    try
      SetLength(B, F.Size);
      if F.Size > 0 then F.ReadBuffer(B[1], F.Size);
    finally
      F.Free;
    end;
  except
    on EStreamError do
    begin
      B := '';
      Exit;
    end;
  end;
  Result := True;
end;

procedure SetHostModuleHook(Hook: THostModuleHook);
begin
  HostModules := Hook;
end;

procedure SetHostResolveHook(Hook: THostResolveHook);
begin
  HostResolve := Hook;
end;

{ a host module's key, not a path: a specifier with a URL-like scheme of two or more letters (goccia:, node:) or a
  bare one (vitest), which is neither absolute nor relative }
function IsSchemeSpecifier(const Spec: UnicodeString): Boolean;
var I, C: Integer;
begin
  if (Spec = '') or (Spec[1] = '/') or (Copy(Spec, 1, 2) = './') or (Copy(Spec, 1, 3) = '../') or (Spec = '.') or
     (Spec = '..') then Exit(False);
  C := Pos(':', Spec);
  if C = 0 then Exit(Pos('/', Spec) = 0);
  if C < 3 then Exit(False);
  for I := 1 to C - 1 do
    if not CharInSet(Spec[I], ['a'..'z', 'A'..'Z', '0'..'9', '+', '-', '.']) then Exit(False);
  Result := True;
end;

{ a resolution failure travels as its key: #0 and the message (LoadModule throws it) }
function ResolvePath(const Spec, Referrer: UnicodeString): UnicodeString;
var Base: UnicodeString;
begin
  if Assigned(HostResolve) then
    case HostResolve(Spec, Referrer, Result) of
      1: Exit;
      -1: Exit(#0 + Result);
      { the error the host made (ResolveError), not a message }
      -2: Exit(#0#1);
    end;
  if Assigned(HostModules) and IsSchemeSpecifier(Spec) then Exit(Spec);
  if (Spec <> '') and (Spec[1] = '/') then Exit(UnicodeString(ExpandFileName(string(Spec))));
  if (Referrer = '') or (Referrer[1] = '<') then Base := UnicodeString(GetCurrentDir) + '/'
  else Base := UnicodeString(ExtractFilePath(ExpandFileName(string(Referrer))));
  Result := UnicodeString(ExpandFileName(string(Base + Spec)));
end;

function ResolveModuleSpecifier(const Spec, Referrer: UnicodeString): UnicodeString;
begin
  Result := ResolvePath(Spec, Referrer);
end;

threadvar
  ProvidedPath, ProvidedSrc: UnicodeString;
  LoadFailPath: UnicodeString;
  LoadFailLine, LoadFailColumn: Integer;
  { ImportSiteNow }
  SitePath: UnicodeString;
  SiteLine, SiteColumn: Integer;

  CompletionWanted: Boolean;
  Completion: TValue;
  CompletionRootedGen: Integer;

procedure ClearLoadFailure;
begin
  LoadFailPath := '';
  LoadFailLine := 0;
  LoadFailColumn := 0;
end;

function LoadFailure(out Path: UnicodeString; out Line, Column: Integer): Boolean;
begin
  Path := LoadFailPath;
  Line := LoadFailLine;
  Column := LoadFailColumn;
  Result := Path <> '';
end;

function ImportSiteNow(out Path: UnicodeString; out Line, Column: Integer): Boolean;
begin
  Path := SitePath;
  Line := SiteLine;
  Column := SiteColumn;
  Result := Path <> '';
end;

procedure SetEntryCompletionWanted(Wanted: Boolean);
begin
  if FirstInEngine(CompletionRootedGen) then AddRoot(@Completion);
  Completion := VUndefined;
  CompletionWanted := Wanted;
end;

function EntryCompletion: TValue;
begin
  if SameEngine(CompletionRootedGen) then Result := Completion else Result := VUndefined;
end;

{ M is the provided entry, compiled to keep its completion value }
function IsCompletionEntry(M: TModRec): Boolean;
var P: UnicodeString;
begin
  P := ProvidedPath;
  Result := CompletionWanted and (P <> '') and (M.Path = P);
end;

procedure SetProvidedSource(const Path, Source: UnicodeString);
begin
  ProvidedPath := Path;
  ProvidedSrc := Source;
end;

function ReadSource(const Path: UnicodeString; var Src: UnicodeString): Boolean;
var B: RawByteString; P: UnicodeString;
begin
  P := ProvidedPath;
  if (P <> '') and (P = Path) then
  begin
    Src := ProvidedSrc;
    if (Src <> '') and (Src[1] = #$FEFF) then Delete(Src, 1, 1);
    Exit(True);
  end;
  Result := ReadFileBytes(Path, B);
  if not Result then Exit;
  SetCodePage(B, CP_UTF8, False);
  Src := UnicodeString(UTF8Decode(B));
  if (Src <> '') and (Src[1] = #$FEFF) then Delete(Src, 1, 1);
end;

{ a request key (ModuleRequestKey) resolved against its referrer: the resolved path, then its type suffix }
function ResolveRequest(const Req, Referrer: UnicodeString): UnicodeString;
var P: Integer;
begin
  if Req = HostSourceSpecifier then Exit(Req);
  P := Pos(#1, Req);
  if P = 0 then Exit(ResolvePath(Req, Referrer));
  Result := ResolvePath(Copy(Req, 1, P - 1), Referrer) + Copy(Req, P, MaxInt);
end;

function ReadBytes(const Path: UnicodeString; var B: RawByteString): Boolean;
begin
  Result := ReadFileBytes(Path, B);
end;

function FindModule(const Path: UnicodeString): TModRec;
var M: TModRec;
begin
  for M in Mods do
    if M.Path = Path then Exit(M);
  Result := nil;
end;

function ClosedCell(V: TValue): TValue; forward;
function HasModuleSource(M: TModRec): Boolean; forward;
function ModuleSourceOf(M: TModRec): TValue; forward;
function MakeHostModule(const Key: UnicodeString; Exp: TValue; var M: TModRec): Boolean; forward;

{ a module of a non-JavaScript type, a Synthetic Module Record with the one export default, evaluated at once:
  'json' (ParseJSONModule), 'text' (the import text proposal: the source decoded as UTF-8) and 'bytes' (the
  import bytes proposal: a Uint8Array over an immutable ArrayBuffer) }
function LoadTypedModule(const Key: UnicodeString; var M: TModRec): Boolean;
var P, K: Integer; Path, AType, Src: UnicodeString; B: RawByteString; V, Tbl: TValue; Virtual: Boolean;
begin
  P := Pos(#1, Key);
  Path := Copy(Key, 1, P - 1);
  AType := Copy(Key, P + 1, MaxInt);
  if (AType <> 'json') and (AType <> 'text') and (AType <> 'bytes') then
    Exit(ThrowTypeError('Unsupported module type ''' + AType + ''''));
  { a host's virtual module (an address, not a path) reinterprets the source it stores }
  Virtual := False;
  if Assigned(HostModules) and (Path <> '') and (Path[1] <> '/') then
  begin
    K := HostModules(Path, V);
    if K = -1 then Exit(False);
    if K = 2 then
    begin
      Virtual := True;
      Src := StrToUnicode(StrOf(V));
      B := UTF8Encode(Src);
    end;
  end;
  if AType = 'bytes' then
  begin
    if not Virtual and not ReadBytes(Path, B) then Exit(ThrowTypeError('Cannot find module ''' + Path + ''''));
    if not BytesHook(B, V) then Exit(False);
  end
  else
  begin
    if not Virtual and not ReadSource(Path, Src) then Exit(ThrowTypeError('Cannot find module ''' + Path + ''''));
    if AType = 'text' then V := StrFromUnicode(Src)
    else if not JSONTextHook(Src, V) then Exit(False);
  end;
  if (AType = 'json') and Assigned(JSONExportsHook) then
  begin
    if not JSONExportsHook(V, Tbl) then Exit(False);
    if IsObject(Tbl) then Exit(MakeHostModule(Key, Tbl, M));
  end;
  PushRoot(V);
  M := TModRec.Create;
  M.Path := Key;
  M.Id := Mods.Count;
  M.HostOwned := HostLoading;
  Mods.Add(M);
  SetRoot(M, rError, VUndefined);
  M.Info := TModuleInfo.Create;
  SetLength(M.Info.LocalExports, 1);
  M.Info.LocalExports[0].ExportName := 'default';
  M.Info.LocalExports[0].Slot := 0;
  M.Info.SlotCount := 1;
  Tbl := MkPtr(NewArray(0, 1));
  SetRoot(M, rTable, Tbl);
  ArrayPush(ObjOf(GetRoot(M, rTable)), ClosedCell(RootAt(TempTop - 1)));
  PopRoots(1);
  M.Status := msEvaluated;
  Result := True;
end;

{ a synthetic module (16.2.1.8) of the host's: Exp its names and values, interleaved }
function MakeHostModule(const Key: UnicodeString; Exp: TValue; var M: TModRec): Boolean;
var I, N: Integer; V, Tbl: TValue; E: PValueArray;
begin
  if not IsObject(Exp) or (ObjOf(Exp)^.OKind <> okArray) then
    Exit(ThrowTypeError('host module ''' + Key + ''': exports are not a list'));
  PushRoot(Exp);
  N := ObjOf(Exp)^.Elements^.Len div 2;
  M := TModRec.Create;
  M.Path := Key;
  M.Id := Mods.Count;
  M.HostOwned := HostLoading;
  Mods.Add(M);
  SetRoot(M, rError, VUndefined);
  M.Info := TModuleInfo.Create;
  SetLength(M.Info.LocalExports, N);
  M.Info.SlotCount := N;
  Tbl := MkPtr(NewArray(0, N));
  SetRoot(M, rTable, Tbl);
  for I := 0 to N - 1 do
  begin
    E := ObjOf(RootAt(TempTop - 1))^.Elements;
    if not IsString(E^.Data[2 * I]) then begin PopRoots(1); Exit(ThrowTypeError('host module ''' + Key + ''': an export name is not a string')); end;
    M.Info.LocalExports[I].ExportName := StrToUnicode(StrOf(E^.Data[2 * I]));
    M.Info.LocalExports[I].Slot := I;
    V := ClosedCell(E^.Data[2 * I + 1]);
    ArrayPush(ObjOf(GetRoot(M, rTable)), V);
  end;
  PopRoots(1);
  M.Status := msEvaluated;
  Result := True;
end;

{ 16.2.1.5.1 LoadRequestedModules (synchronously, from files): the module at Path and everything it requests }
function LoadModule(const Path: UnicodeString; var M: TModRec): Boolean;
var Src, Err, ErrName, Resolved: UnicodeString; I: Integer; D: TModRec; V: TValue; Compiled: Boolean;
begin
  if (Length(Path) >= 2) and (Path[1] = #0) and (Path[2] = #1) then
  begin
    { a resolution the host refused with an error of its own }
    Pending := ResolveErrorV;
    ResolveErrorV := VUndefined;
    Exit(False);
  end;
  if (Path <> '') and (Path[1] = #0) then
  begin
    { a failed resolution (ResolvePath); a typed request carries its type after #1 }
    if Pos(#1, Path) > 0 then Exit(ThrowError('Error', Copy(Path, 2, Pos(#1, Path) - 2)));
    Exit(ThrowError('Error', Copy(Path, 2, MaxInt)));
  end;
  M := FindModule(Path);
  if Assigned(M) then
  begin
    if M.Errored and (M.Status = msNew) then begin Pending := GetRoot(M, rError); Exit(False); end;
    Exit(True);
  end;
  if Pos(#1, Path) > 0 then Exit(LoadTypedModule(Path, M));
  if Path = HostSourceSpecifier then
  begin
    { a module record without exports or evaluation, only a source phase representation }
    M := TModRec.Create;
    M.Path := Path;
    M.Id := Mods.Count;
    M.HostOwned := HostLoading;
    Mods.Add(M);
    SetRoot(M, rError, VUndefined);
    M.Info := TModuleInfo.Create;
    M.Info.SlotCount := 0;
    SetRoot(M, rTable, MkPtr(NewArray(0, 0)));
    M.Status := msEvaluated;
    Exit(True);
  end;
  Src := '';
  I := 0;
  if Assigned(HostModules) then I := HostModules(Path, V);
  case I of
    1: Exit(MakeHostModule(Path, V, M));
    2: Src := StrToUnicode(StrOf(V));
    -1: Exit(False);
  end;
  if (I <> 2) and not ReadSource(Path, Src) then Exit(ThrowTypeError('Cannot find module ''' + Path + ''''));
  M := TModRec.Create;
  M.Path := Path;
  M.Id := Mods.Count;
  M.HostOwned := HostLoading;
  Mods.Add(M);
  SetRoot(M, rError, VUndefined);
  ModuleReturnLast := IsCompletionEntry(M);
  try
    Compiled := ParseAndCompileModule(Src, Path, M.Id + 1, M.Tpl, M.Info, Err, ErrName);
  finally
    ModuleReturnLast := False;
  end;
  if not Compiled then
  begin
    M.Errored := True;
    ThrowError(ErrName, Err);
    SetRoot(M, rError, Pending);
    Exit(False);
  end;
  SetLength(M.Deps, Length(M.Info.Requests));
  for I := 0 to High(M.Info.Requests) do
  begin
    { the declaration is where a host's resolution decision is made (ImportSiteNow) }
    SitePath := Path;
    SiteLine := 0;
    SiteColumn := 0;
    if I <= High(M.Info.RequestLines) then
    begin
      SiteLine := M.Info.RequestLines[I];
      SiteColumn := M.Info.RequestColumns[I];
    end;
    try
      Resolved := ResolveRequest(M.Info.Requests[I], Path);
    finally
      SitePath := '';
    end;
    if not LoadModule(Resolved, D) then
    begin
      if (LoadFailPath = '') and (I <= High(M.Info.RequestLines)) and (M.Info.RequestLines[I] > 0) then
      begin
        LoadFailPath := Path;
        LoadFailLine := M.Info.RequestLines[I];
        LoadFailColumn := M.Info.RequestColumns[I];
      end;
      Exit(False);
    end;
    M.Deps[I] := D;
  end;
  Result := True;
end;

function DepOf(M: TModRec; const Req: UnicodeString): TModRec;
var I: Integer;
begin
  for I := 0 to High(M.Info.Requests) do
    if M.Info.Requests[I] = Req then Exit(M.Deps[I]);
  Result := nil;
end;

{ ---- 16.2.1.7.2.2 ResolveExport / .1 GetExportedNames ---- }

type
  TResolveSet = array of record M: TModRec; N: UnicodeString; end;

function ResolveExport(M: TModRec; const Name: UnicodeString; var RS_: TResolveSet): TResolution;
var I: Integer; R: TResolution; Star: TResolution; D: TModRec;
begin
  Result.Kind := rkNull; Result.Module := nil; Result.Slot := -1;
  for I := 0 to High(RS_) do
    if (RS_[I].M = M) and (RS_[I].N = Name) then Exit;   { a circular import request }
  SetLength(RS_, Length(RS_) + 1);
  RS_[High(RS_)].M := M; RS_[High(RS_)].N := Name;
  for I := 0 to High(M.Info.LocalExports) do
    if M.Info.LocalExports[I].ExportName = Name then
    begin
      Result.Kind := rkBinding; Result.Module := M; Result.Slot := M.Info.LocalExports[I].Slot;
      Exit;
    end;
  for I := 0 to High(M.Info.IndirectExports) do
    if M.Info.IndirectExports[I].ExportName = Name then
    begin
      D := DepOf(M, M.Info.IndirectExports[I].Request);
      if M.Info.IndirectExports[I].ImportName = ModNamespace then
      begin
        Result.Kind := rkNamespace; Result.Module := D;
        Exit;
      end;
      if M.Info.IndirectExports[I].ImportName = ModDeferNamespace then
      begin
        Result.Kind := rkDeferNS; Result.Module := D;
        Exit;
      end;
      { a re-exported source-phase import: the imported module's source object (Source Phase Imports) }
      if M.Info.IndirectExports[I].ImportName = ModSource then
      begin
        Result.Kind := rkSource; Result.Module := D;
        Exit;
      end;
      Exit(ResolveExport(D, M.Info.IndirectExports[I].ImportName, RS_));
    end;
  if Name = 'default' then Exit;
  Star.Kind := rkNull;
  for I := 0 to High(M.Info.StarExports) do
  begin
    R := ResolveExport(DepOf(M, M.Info.StarExports[I]), Name, RS_);
    if R.Kind = rkAmbiguous then Exit(R);
    if R.Kind = rkNull then Continue;
    if Star.Kind = rkNull then Star := R
    else if (R.Module <> Star.Module) or (R.Kind <> Star.Kind) or (R.Slot <> Star.Slot) then
    begin
      Result.Kind := rkAmbiguous;
      Exit;
    end;
  end;
  Result := Star;
end;

function Resolve(M: TModRec; const Name: UnicodeString): TResolution;
var RS_: TResolveSet;
begin
  SetLength(RS_, 0);
  Result := ResolveExport(M, Name, RS_);
end;

procedure ExportedNames(M: TModRec; Seen: TList<TModRec>; Names: TStringList);
var I, J: Integer; S: TStringList;
begin
  if Seen.IndexOf(M) >= 0 then Exit;
  Seen.Add(M);
  for I := 0 to High(M.Info.LocalExports) do Names.Add(M.Info.LocalExports[I].ExportName);
  for I := 0 to High(M.Info.IndirectExports) do Names.Add(M.Info.IndirectExports[I].ExportName);
  for I := 0 to High(M.Info.StarExports) do
  begin
    S := TStringList.Create;
    S.UseLocale := False; S.CaseSensitive := True;
    try
      ExportedNames(DepOf(M, M.Info.StarExports[I]), Seen, S);
      for J := 0 to S.Count - 1 do
        if (S[J] <> 'default') and (Names.IndexOf(S[J]) < 0) then Names.Add(S[J]);
    finally
      S.Free;
    end;
  end;
end;

function CodeUnitOrder(L: TStringList; A, B: Integer): Integer;
begin
  if L[A] < L[B] then Result := -1 else if L[A] > L[B] then Result := 1 else Result := 0;
end;

function ClosedCell(V: TValue): TValue;
var U: PUpval;
begin
  PushRoot(V);
  U := NewUpval;
  U^.StackIndex := -1;
  U^.Value := RootAt(TempTop - 1);
  PopRoots(1);
  Result := MkPtr(U);
end;

function CellOf(const R: TResolution): TValue; forward;

{ 16.2.1.10 GetModuleNamespace / 10.4.6.12 ModuleNamespaceCreate }
function GetNamespace(M: TModRec): TValue;
var Names: TStringList; Seen: TList<TModRec>; I: Integer; R: TResolution; O: PObj; Arr: PObj; Base: Integer;
begin
  Result := GetRoot(M, rNS);
  if IsObject(Result) then Exit;
  Names := TStringList.Create; Names.UseLocale := False; Names.CaseSensitive := True;
  Seen := TList<TModRec>.Create;
  Base := TempTop;
  try
    ExportedNames(M, Seen, Names);
    O := NewObject(RootShapeFor(VNull), 0, okModuleNS);
    PushRoot(MkPtr(O));
    DefineOwn(RootAt(Base), SymKey(SymToStringTagW), StrFromUnicode('Module'), 0);
    PushRoot(MkPtr(NewArray(0, 8)));
    ObjOf(RootAt(Base))^.Aux1 := RootAt(Base + 1);
    ObjOf(RootAt(Base))^.OFlags := ObjOf(RootAt(Base))^.OFlags or 8;   { not extensible }
    ObjOf(RootAt(Base))^.Shape := ShapeFork(ObjOf(RootAt(Base))^.Shape);   { its own shape: no inline cache shares it }
    SetRoot(M, rNS, RootAt(Base));
    { the unambiguous names, in code unit order (TStringList.Sort compares with the locale: sorted by hand) }
    Names.CustomSort(CodeUnitOrder);
    for I := 0 to Names.Count - 1 do
    begin
      R := Resolve(M, Names[I]);
      if not (R.Kind in [rkBinding, rkNamespace, rkDeferNS, rkSource]) then Continue;
      ArrayPush(ObjOf(RootAt(Base + 1)), MkPtr(AtomOf(Names[I])));
      ArrayPush(ObjOf(RootAt(Base + 1)), CellOf(R));
    end;
    Result := RootAt(Base);
  finally
    PopRoots(TempTop - Base);
    Names.Free;
    Seen.Free;
  end;
end;

{ GetModuleNamespace(module, defer): the deferred namespace, a namespace object (OFlags 16, its module's Id in
  Aux2) whose exports leave out "then", with @@toStringTag "Deferred Module" }
function GetDeferredNamespace(M: TModRec): TValue;
var Base, I: Integer; O: PObj; E: PValueArray;
begin
  Result := GetRoot(M, rDNS);
  if IsObject(Result) then Exit;
  Base := TempTop;
  PushRoot(GetNamespace(M));
  O := NewObject(RootShapeFor(VNull), 0, okModuleNS);
  PushRoot(MkPtr(O));
  DefineOwn(RootAt(Base + 1), SymKey(SymToStringTagW), StrFromUnicode('Deferred Module'), 0);
  PushRoot(MkPtr(NewArray(0, 8)));
  E := ObjOf(ObjOf(RootAt(Base))^.Aux1)^.Elements;
  I := 0;
  while I < Integer(E^.Len) div 2 do
  begin
    if StrToUnicode(PStr(PtrOf(E^.Data[I * 2]))) <> 'then' then
    begin
      ArrayPush(ObjOf(RootAt(Base + 2)), E^.Data[I * 2]);
      E := ObjOf(ObjOf(RootAt(Base))^.Aux1)^.Elements;
      ArrayPush(ObjOf(RootAt(Base + 2)), E^.Data[I * 2 + 1]);
      E := ObjOf(ObjOf(RootAt(Base))^.Aux1)^.Elements;
    end;
    Inc(I);
  end;
  O := ObjOf(RootAt(Base + 1));
  O^.Aux1 := RootAt(Base + 2);
  O^.Aux2 := MkInt(M.Id);
  O^.OFlags := O^.OFlags or 8 or 16;
  O^.Shape := ShapeFork(O^.Shape);
  SetRoot(M, rDNS, RootAt(Base + 1));
  Result := RootAt(Base + 1);
  PopRoots(3);
end;

function CellOf(const R: TResolution): TValue;
begin
  if R.Kind = rkSource then Exit(ClosedCell(ModuleSourceOf(R.Module)));
  if R.Kind = rkNamespace then Exit(ClosedCell(GetNamespace(R.Module)));
  if R.Kind = rkDeferNS then Exit(ClosedCell(GetDeferredNamespace(R.Module)));
  Result := ObjOf(GetRoot(R.Module, rTable))^.Elements^.Data[R.Slot];
end;

{ ---- 16.2.1.5.2 Link ---- }

procedure CollectGraph(M: TModRec; L: TList<TModRec>);
var D: TModRec;
begin
  if (M.Status <> msNew) or (L.IndexOf(M) >= 0) then Exit;
  M.Status := msLinking;
  L.Add(M);
  for D in M.Deps do CollectGraph(D, L);
end;

function LinkFail(L: TList<TModRec>): Boolean;
var M: TModRec;
begin
  for M in L do M.Status := msNew;
  Result := False;
end;

function NoSourceFail(L: TList<TModRec>): Boolean;
begin
  ThrowError('SyntaxError', 'A source text module has no source phase representation');
  Result := LinkFail(L);
end;

function Link(Root: TModRec): Boolean;
var L: TList<TModRec>; M: TModRec; I, J: Integer; Clo, Gen, Tbl: TValue; R: TResolution; D: TModRec; U: PUpval;
begin
  L := TList<TModRec>.Create;
  try
    CollectGraph(Root, L);
    { every module's InitializeEnvironment start: its table, then its function run to its first suspension }
    for M in L do
    begin
      Tbl := MkPtr(NewArray(M.Info.SlotCount, M.Info.SlotCount));
      SetRoot(M, rTable, Tbl);
      if not MakeScriptClosure(M.Tpl, Clo) then Exit(LinkFail(L));
      PushRoot(Clo);
      Tbl := GetRoot(M, rTable);
      if not CallValue(RootAt(TempTop - 1), VUndefined, @Tbl, 1, Gen) then begin PopRoots(1); Exit(LinkFail(L)); end;
      PopRoots(1);
      SetRoot(M, rGen, Gen);
    end;
    { the namespace imports first: a re-export of one (export of the ns binding) is a local export whose cell is the import's
      table slot, which a named import or a namespace elsewhere in the graph may take. Their cells are made
      before any namespace is (a namespace holds the cells of its exports), then given the namespaces }
    for M in L do
      for I := 0 to High(M.Info.Imports) do
        if (M.Info.Imports[I].ImportName = ModNamespace) or (M.Info.Imports[I].ImportName = ModDeferNamespace) then
        begin
          Tbl := ClosedCell(VHole);
          ValueArrayStore(ObjOf(GetRoot(M, rTable))^.Elements, M.Info.Imports[I].Slot, Tbl);
        end;
    for M in L do
      for I := 0 to High(M.Info.Imports) do
        if (M.Info.Imports[I].ImportName = ModNamespace) or (M.Info.Imports[I].ImportName = ModDeferNamespace) then
        begin
          if M.Info.Imports[I].ImportName = ModNamespace then Clo := GetNamespace(DepOf(M, M.Info.Imports[I].Request))
          else Clo := GetDeferredNamespace(DepOf(M, M.Info.Imports[I].Request));
          U := PUpval(PtrOf(ObjOf(GetRoot(M, rTable))^.Elements^.Data[M.Info.Imports[I].Slot]));
          U^.Value := Clo;
          WriteBarrier(@U^.Hdr, Clo);
        end;
    { the imports and indirect exports resolved }
    for M in L do
    begin
      for I := 0 to High(M.Info.IndirectExports) do
      begin
        if (M.Info.IndirectExports[I].ImportName = ModNamespace) or
          (M.Info.IndirectExports[I].ImportName = ModDeferNamespace) then Continue;
        if M.Info.IndirectExports[I].ImportName = ModSource then
        begin
          if not HasModuleSource(DepOf(M, M.Info.IndirectExports[I].Request)) then Exit(NoSourceFail(L));
          Continue;
        end;
        R := Resolve(DepOf(M, M.Info.IndirectExports[I].Request), M.Info.IndirectExports[I].ImportName);
        if not (R.Kind in [rkBinding, rkNamespace, rkDeferNS, rkSource]) then
        begin
          ThrowError('SyntaxError', MissingExportMessage(M.Info.IndirectExports[I].Request,
            M.Info.IndirectExports[I].ImportName));
          Exit(LinkFail(L));
        end;
      end;
      for I := 0 to High(M.Info.Imports) do
      begin
        D := DepOf(M, M.Info.Imports[I].Request);
        if (M.Info.Imports[I].ImportName = ModDeferNamespace) or (M.Info.Imports[I].ImportName = ModNamespace) then
          Continue;
        if M.Info.Imports[I].ImportName = ModSource then
        begin
          { GetModuleSource of the module: a SyntaxError when it has no source representation }
          if not HasModuleSource(D) then Exit(NoSourceFail(L));
          R.Kind := rkSource; R.Module := D; R.Slot := -1;
        end
        else
        begin
          R := Resolve(D, M.Info.Imports[I].ImportName);
          if (R.Kind = rkSource) and not HasModuleSource(R.Module) then Exit(NoSourceFail(L));
          if not (R.Kind in [rkBinding, rkNamespace, rkDeferNS, rkSource]) then
          begin
            if R.Kind = rkAmbiguous then
              ThrowError('SyntaxError', MissingExportMessage(M.Info.Imports[I].Request, M.Info.Imports[I].ImportName) +
                ' (its star exports provide conflicting bindings for it)')
            else
              ThrowError('SyntaxError', MissingExportMessage(M.Info.Imports[I].Request, M.Info.Imports[I].ImportName));
            Exit(LinkFail(L));
          end;
        end;
        J := M.Info.Imports[I].Slot;
        Tbl := CellOf(R);
        ValueArrayStore(ObjOf(GetRoot(M, rTable))^.Elements, J, Tbl);
      end;
    end;
    for M in L do M.Status := msLinked;
    Result := True;
  finally
    L.Free;
  end;
end;

{ ---- 16.2.1.5.3 Evaluate (depth first; a cycle's member already evaluating is skipped) ---- }

{ ---- 16.2.1.6.1.3 Evaluate, with the import defer proposal's changes (GatherAsynchronousTransitiveDependencies,
  ReadyForSyncExecution, EvaluateModuleSync) ---- }

function IsCyclic(M: TModRec): Boolean; inline;
begin
  Result := M.Tpl <> nil;   { a typed module (LoadTypedModule) is a Synthetic Module Record }
end;

function HasCap(M: TModRec): Boolean; inline;
begin
  Result := IsObject(GetRoot(M, rCap));
end;

procedure SetEvalError(M: TModRec; E: TValue);
begin
  M.Errored := True;
  SetRoot(M, rError, E);
end;

{ IsModuleSCCEvaluated }
function SCCEvaluated(M: TModRec): Boolean;
begin
  if M.CycleRoot <> nil then Exit(M.CycleRoot.Status = msEvaluated);
  Result := M.Status = msEvaluated;
end;

{ GatherAsynchronousTransitiveDependencies: the modules with top-level await a deferred module's evaluation will
  need, gathered depth first and stopping at each }
procedure GatherAsyncDeps(M: TModRec; Seen, Res: TList<TModRec>);
var I: Integer;
begin
  if Seen.IndexOf(M) >= 0 then Exit;
  Seen.Add(M);
  if not IsCyclic(M) then Exit;
  if (M.Status = msEvaluating) or SCCEvaluated(M) then Exit;
  if M.Info.HasTLA then
  begin
    if Res.IndexOf(M) < 0 then Res.Add(M);
    Exit;
  end;
  for I := 0 to High(M.Deps) do GatherAsyncDeps(M.Deps[I], Seen, Res);
end;

function AsyncDepsOf(M: TModRec): TList<TModRec>;
var Seen: TList<TModRec>;
begin
  Result := TList<TModRec>.Create;
  Seen := TList<TModRec>.Create;
  try
    GatherAsyncDeps(M, Seen, Result);
  finally
    Seen.Free;
  end;
end;

{ ReadyForSyncExecution }
function ReadyForSync(M: TModRec; Seen: TList<TModRec>): Boolean;
var I: Integer;
begin
  Result := True;
  if not IsCyclic(M) then Exit;
  if Seen.IndexOf(M) >= 0 then Exit;
  Seen.Add(M);
  if SCCEvaluated(M) then Exit;
  if M.Status in [msEvaluating, msEvaluatingAsync] then Exit(False);
  if M.Info.HasTLA then Exit(False);
  for I := 0 to High(M.Deps) do
    if not ReadyForSync(M.Deps[I], Seen) then Exit(False);
end;

{ ExecuteModule of a module without top-level await: its function run to the end }
function ExecuteSync(M: TModRec): Boolean;
var V: TValue; Done: Boolean;
begin
  Result := GeneratorResume(GetRoot(M, rGen), VUndefined, 0, V, Done);
  if Result and Done and IsCompletionEntry(M) then Completion := V;
end;

procedure ExecuteAsyncModule(M: TModRec); forward;

{ AsyncModuleExecutionRejected }
procedure AsyncRejected(M: TModRec; E: TValue);
var I: Integer;
begin
  if M.Status = msEvaluated then Exit;
  PushRoot(E);
  SetEvalError(M, RootAt(TempTop - 1));
  M.Status := msEvaluated;
  M.AsyncOrder := -1;
  if HasCap(M) then RejectPendingPromise(GetRoot(M, rCap), RootAt(TempTop - 1));
  for I := 0 to High(M.AsyncParents) do AsyncRejected(M.AsyncParents[I], RootAt(TempTop - 1));
  PopRoots(1);
end;

{ GatherAvailableAncestors }
procedure GatherAncestors(M: TModRec; L: TList<TModRec>);
var I: Integer; P, Root: TModRec;
begin
  for I := 0 to High(M.AsyncParents) do
  begin
    P := M.AsyncParents[I];
    Root := P.CycleRoot;
    if Root = nil then Root := P;
    if (L.IndexOf(P) >= 0) or Root.Errored then Continue;
    Dec(P.PendingAsync);
    if P.PendingAsync = 0 then
    begin
      L.Add(P);
      if not P.Info.HasTLA then GatherAncestors(P, L);
    end;
  end;
end;

{ AsyncModuleExecutionFulfilled }
procedure AsyncFulfilled(M: TModRec);
var L: TList<TModRec>; I, J: Integer; P: TModRec; E: TValue;
begin
  if M.Status = msEvaluated then Exit;
  M.AsyncOrder := -1;
  M.Status := msEvaluated;
  if HasCap(M) then ResolvePendingPromise(GetRoot(M, rCap), VUndefined);
  L := TList<TModRec>.Create;
  try
    GatherAncestors(M, L);
    { sorted by [[AsyncEvaluationOrder]] }
    for I := 1 to L.Count - 1 do
    begin
      P := L[I];
      J := I - 1;
      while (J >= 0) and (L[J].AsyncOrder > P.AsyncOrder) do
      begin
        L[J + 1] := L[J];
        Dec(J);
      end;
      L[J + 1] := P;
    end;
    for I := 0 to L.Count - 1 do
    begin
      P := L[I];
      if P.Status = msEvaluated then Continue;
      if P.Info.HasTLA then ExecuteAsyncModule(P)
      else if not ExecuteSync(P) then
      begin
        E := Pending;
        Pending := VUndefined;
        AsyncRejected(P, E);
      end
      else
      begin
        P.AsyncOrder := -1;
        P.Status := msEvaluated;
        if HasCap(P) then ResolvePendingPromise(GetRoot(P, rCap), VUndefined);
      end;
    end;
  finally
    L.Free;
  end;
end;

function AsyncFulfilledFn(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var V: TValue;
begin
  { the module function's last step, an iterator result: its value is the completion value of an entry compiled
    to keep it }
  if IsCompletionEntry(Mods[NativeData]) and (Argc >= 1) and IsObject(Args[1]) and GetV(Args[1], AtomValue, V) then
    Completion := V;
  Pending := VUndefined;
  AsyncFulfilled(Mods[NativeData]);
  Res := VUndefined;
  Result := True;
end;

function AsyncRejectedFn(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var E: TValue;
begin
  if Argc >= 1 then E := Args[1] else E := VUndefined;
  AsyncRejected(Mods[NativeData], E);
  Res := VUndefined;
  Result := True;
end;

{ ExecuteAsyncModule: the body runs to its first await; the promise its function returns settles when it ends }
procedure ExecuteAsyncModule(M: TModRec);
var NextF, P, F: TValue;
begin
  if not GetV(GetRoot(M, rGen), AtomNext, NextF) or not CallValue(NextF, GetRoot(M, rGen), nil, 0, P) then
  begin
    P := Pending;
    Pending := VUndefined;
    PushRoot(P);
    P := NewPendingPromise;
    RejectPendingPromise(P, RootAt(TempTop - 1));
    PopRoots(1);
  end;
  PushRoot(P);
  PushRoot(NewNativeFunctionData(AsyncFulfilledFn, '', 0, M.Id));
  F := NewNativeFunctionData(AsyncRejectedFn, '', 1, M.Id);
  PushRoot(F);
  PromiseThenNative(RootAt(TempTop - 3), RootAt(TempTop - 2), RootAt(TempTop - 1), VUndefined);
  PopRoots(3);
end;

{ IncrementModuleAsyncEvaluationCount }
function NextAsyncOrder: Int64; noinline;
begin
  Inc(AsyncEvalCount);
  Result := AsyncEvalCount;
end;

procedure AddAsyncParent(D, M: TModRec);
begin
  SetLength(D.AsyncParents, Length(D.AsyncParents) + 1);
  D.AsyncParents[High(D.AsyncParents)] := M;
end;

{ InnerModuleEvaluation }
function InnerEval(M: TModRec; Stack: TList<TModRec>; var Index: Integer): Boolean;
var L, G: TList<TModRec>; I, J, ModIndex: Integer; D: TModRec; Done: Boolean;
begin
  Result := True;
  if not IsCyclic(M) then Exit;
  if M.Status in [msEvaluatingAsync, msEvaluated] then
  begin
    if M.Errored then begin Pending := GetRoot(M, rError); Exit(False); end;
    Exit;
  end;
  if M.Status = msEvaluating then Exit;
  M.Status := msEvaluating;
  ModIndex := Index;
  M.DFSAnc := Index;
  M.PendingAsync := 0;
  Inc(Index);
  L := TList<TModRec>.Create;
  try
    for I := 0 to High(M.Info.EvalOrder) do
    begin
      D := DepOf(M, M.Info.EvalOrder[I].Req);
      if not M.Info.EvalOrder[I].Eager then
      begin
        G := AsyncDepsOf(D);
        try
          for J := 0 to G.Count - 1 do
            if L.IndexOf(G[J]) < 0 then L.Add(G[J]);
        finally
          G.Free;
        end;
      end
      else if L.IndexOf(D) < 0 then L.Add(D);
    end;
    Stack.Add(M);
    for I := 0 to L.Count - 1 do
    begin
      D := L[I];
      if not InnerEval(D, Stack, Index) then Exit(False);
      if not IsCyclic(D) then Continue;
      if D.Status = msEvaluating then
      begin
        if D.DFSAnc < M.DFSAnc then M.DFSAnc := D.DFSAnc;
      end
      else
      begin
        if D.CycleRoot <> nil then D := D.CycleRoot;
        if D.Errored then begin Pending := GetRoot(D, rError); Exit(False); end;
      end;
      if D.AsyncOrder > 0 then
      begin
        Inc(M.PendingAsync);
        AddAsyncParent(D, M);
      end;
    end;
  finally
    L.Free;
  end;
  if (M.PendingAsync > 0) or M.Info.HasTLA then
  begin
    M.AsyncOrder := NextAsyncOrder;
    if M.PendingAsync = 0 then ExecuteAsyncModule(M);
  end
  else if not ExecuteSync(M) then Exit(False);
  if M.DFSAnc = ModIndex then
    repeat
      D := Stack[Stack.Count - 1];
      Stack.Delete(Stack.Count - 1);
      if D.AsyncOrder > 0 then D.Status := msEvaluatingAsync else D.Status := msEvaluated;
      Done := D = M;
      D.CycleRoot := M;
    until Done;
end;

{ Evaluate(): the promise of the module's evaluation (its cycle root's) }
function EvaluateModule(M: TModRec): TValue;
var Stack: TList<TModRec>; Index, I: Integer;
begin
  if (M.Status in [msEvaluatingAsync, msEvaluated]) and (M.CycleRoot <> nil) then M := M.CycleRoot;
  if HasCap(M) then Exit(GetRoot(M, rCap));
  SetRoot(M, rCap, NewPendingPromise);
  Stack := TList<TModRec>.Create;
  try
    Index := 0;
    if not InnerEval(M, Stack, Index) then
    begin
      PushRoot(Pending);
      Pending := VUndefined;
      for I := 0 to Stack.Count - 1 do
      begin
        Stack[I].Status := msEvaluated;
        SetEvalError(Stack[I], RootAt(TempTop - 1));
      end;
      RejectPendingPromise(GetRoot(M, rCap), RootAt(TempTop - 1));
      PopRoots(1);
    end
    else if M.Status = msEvaluated then ResolvePendingPromise(GetRoot(M, rCap), VUndefined);
  finally
    Stack.Free;
  end;
  Result := GetRoot(M, rCap);
end;

{ NSDeferHook: EnsureDeferredNamespaceEvaluation, EvaluateModuleSync }
function DeferEval(NS: TValue): Boolean;
var M: TModRec; Seen: TList<TModRec>; Ready: Boolean; P, R: TValue;
begin
  M := Mods[IntOf(ObjOf(NS)^.Aux2)];
  Seen := TList<TModRec>.Create;
  try
    Ready := ReadyForSync(M, Seen);
  finally
    Seen.Free;
  end;
  if not Ready then
    Exit(ThrowTypeError('A deferred module cannot be evaluated while it or a dependency is evaluating or has top-level await'));
  P := EvaluateModule(M);
  case PromiseStatus(P, R) of
    1: Result := True;
    2: begin Pending := R; Result := False; end;
  else
    Result := ThrowTypeError('A deferred module''s evaluation did not complete synchronously');
  end;
end;

function LoadAndLink(const Path: UnicodeString; var M: TModRec): Boolean;
begin
  Result := LoadModule(Path, M);
  if Result and (M.Status = msNew) then Result := Link(M);
end;

{ the host's entry point: the module loaded, linked and evaluated, its evaluation's jobs run until it settles }
function RunModuleFile(const Path: UnicodeString; var Phase: Integer): Boolean;
var M: TModRec; P, R: TValue; Entry: UnicodeString;
begin
  Phase := 0;
  { a provided entry has no file for a resolver to find: its path is its key }
  Entry := ProvidedPath;
  if (Entry = '') or (Entry <> Path) then Entry := ResolvePath(Path, '');
  EntryEvalPromise := VUndefined;
  if not LoadAndLink(Entry, M) then Exit(False);
  Phase := 1;
  PushRoot(EvaluateModule(M));
  EntryEvalPromise := RootAt(TempTop - 1);
  if PromiseStatus(RootAt(TempTop - 1), R) = 0 then
    if not RunMicrotasks then begin PopRoots(1); Exit(False); end;
  P := RootAt(TempTop - 1);
  PopRoots(1);
  if PromiseStatus(P, R) = 2 then begin Pending := R; Exit(False); end;
  Result := True;
end;

function ModuleRecordExists(const Key: UnicodeString): Boolean;
begin
  Result := (Mods <> nil) and (FindModule(Key) <> nil);
end;

{ G2.VM's ModSlotNameHook: the import or local export of the module whose cell table is Table at Slot }
function ModSlotName(Table: TValue; Slot: Integer): UnicodeString;
var M: TModRec; I: Integer;
begin
  Result := '';
  if Mods = nil then Exit;
  for M in Mods do
    if (M.Info <> nil) and (GetRoot(M, rTable) = Table) then
    begin
      for I := 0 to High(M.Info.Imports) do
        if (M.Info.Imports[I].Slot = Slot) and (M.Info.Imports[I].LocalName <> '') then
          Exit(M.Info.Imports[I].LocalName);
      for I := 0 to High(M.Info.LocalExports) do
        if M.Info.LocalExports[I].Slot = Slot then Exit(M.Info.LocalExports[I].LocalName);
      Exit;
    end;
end;

function StartScriptImports(const EntryPath, Source: UnicodeString; var Evaluation: TValue): Boolean;
var M: TModRec; R: TValue; Key, SavedPath, SavedSrc: UnicodeString; Ok: Boolean;
begin
  Evaluation := VUndefined;
  Key := EntryPath + ScriptImportsSuffix;
  SavedPath := ProvidedPath;
  SavedSrc := ProvidedSrc;
  SetProvidedSource(Key, Source);
  try
    Ok := LoadAndLink(Key, M);
  finally
    SetProvidedSource(SavedPath, SavedSrc);
  end;
  if not Ok then Exit(False);
  PushRoot(EvaluateModule(M));
  if PromiseStatus(RootAt(TempTop - 1), R) = 0 then
    if not RunMicrotasks then begin PopRoots(1); Exit(False); end;
  Evaluation := RootAt(TempTop - 1);
  PopRoots(1);
  Result := True;
end;

function FinishScriptImports(const EntryPath: UnicodeString; Evaluation: TValue): Boolean;
var M: TModRec; R: TValue; I: Integer;
begin
  case PromiseStatus(Evaluation, R) of
    1: ;
    2: begin Pending := R; Exit(False); end;
  else
    Exit(ThrowTypeError('await: Promise did not settle'));
  end;
  M := FindModule(EntryPath + ScriptImportsSuffix);
  if (M = nil) or (M.Info = nil) then Exit(ThrowTypeError('The Script entry''s imports did not load'));
  for I := 0 to High(M.Info.Imports) do
    if M.Info.Imports[I].LocalName <> '' then
      if not BindGlobalImport(AtomOf(M.Info.Imports[I].LocalName),
        ObjOf(GetRoot(M, rTable))^.Elements^.Data[M.Info.Imports[I].Slot]) then Exit(False);
  Result := True;
end;

procedure SetResolveError(V: TValue);
begin
  ResolveErrorV := V;
end;

function EntryEvaluationPending: Boolean;
var R: TValue;
begin
  Result := IsObject(EntryEvalPromise) and (PromiseStatus(EntryEvalPromise, R) = 0);
end;

function EvaluateModuleNamespace(const Path: UnicodeString; var NS: TValue): Boolean;
var M: TModRec; P, R: TValue;
begin
  if not LoadAndLink(ResolvePath(Path, ''), M) then Exit(False);
  PushRoot(EvaluateModule(M));
  if PromiseStatus(RootAt(TempTop - 1), R) = 0 then
    if not RunMicrotasks then begin PopRoots(1); Exit(False); end;
  P := RootAt(TempTop - 1);
  PopRoots(1);
  case PromiseStatus(P, R) of
    1: ;
    2: begin Pending := R; Exit(False); end;
  else
    Exit(ThrowTypeError('The module ' + Path + ' did not finish evaluating'));
  end;
  NS := GetNamespace(M);
  Result := True;
end;

{ ---- 13.3.10 import() ---- }

{ a dynamic import's promise settled (once): PendingImports holds [promise, module id, deps left] per import }
procedure SettleImport(I: Integer; Ok: Boolean; V: TValue);
var P, R: TValue;
begin
  P := ObjOf(PendingImports)^.Elements^.Data[I * 3];
  if not IsObject(P) or (PromiseStatus(P, R) <> 0) then Exit;
  ValueArrayStore(ObjOf(PendingImports)^.Elements, I * 3, VUndefined);
  if Ok then ResolvePendingPromise(P, V) else RejectPendingPromise(P, V);
end;

{ fulfilledClosure: the namespace (deferred for import.defer()); Data = the import's index, bit 23 defer }
procedure ImportFulfilled(Data: Integer);
var I: Integer; M: TModRec; NS: TValue;
begin
  I := Data and $3FFFFF;
  M := Mods[IntOf(ObjOf(PendingImports)^.Elements^.Data[I * 3 + 1])];
  if (Data and $800000) <> 0 then NS := GetDeferredNamespace(M) else NS := GetNamespace(M);
  SettleImport(I, True, NS);
end;

function ImportFulfilledFn(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
begin
  ImportFulfilled(NativeData);
  Res := VUndefined;
  Result := True;
end;

{ import.defer(): one of the asynchronous dependencies evaluated; the namespace once all are }
function ImportDepFulfilledFn(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var I, N: Integer;
begin
  I := NativeData and $3FFFFF;
  N := IntOf(ObjOf(PendingImports)^.Elements^.Data[I * 3 + 2]) - 1;
  ValueArrayStore(ObjOf(PendingImports)^.Elements, I * 3 + 2, MkInt(N));
  if N = 0 then ImportFulfilled(NativeData);
  Res := VUndefined;
  Result := True;
end;

function ImportRejectedFn(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var E: TValue;
begin
  if Argc >= 1 then E := Args[1] else E := VUndefined;
  SettleImport(NativeData and $3FFFFF, False, E);
  Res := VUndefined;
  Result := True;
end;

procedure ThenImport(P: TValue; OnF: TNativeFn; Data: Integer);
var F: TValue;
begin
  PushRoot(P);
  PushRoot(NewNativeFunctionData(OnF, '', 0, Data));
  F := NewNativeFunctionData(ImportRejectedFn, '', 1, Data);
  PushRoot(F);
  PromiseThenNative(RootAt(TempTop - 3), RootAt(TempTop - 2), RootAt(TempTop - 1), VUndefined);
  PopRoots(3);
end;

{ the job that loads and links a dynamic import's module, then evaluates it (ContinueDynamicImport); Data = its
  index in PendingImports, bit 23 set for import.defer() }
function ImportJob(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var I, J, Data: Integer; M: TModRec; Ref, Spec, Text: UnicodeString; G: TList<TModRec>; R, St: TValue;
  Loaded: Boolean;
begin
  Res := VUndefined;
  Result := True;
  { NativeData read once: the loading and linking below run other natives }
  Data := NativeData;
  I := Data and $3FFFFF;
  Ref := StrToUnicode(StrOf(ObjOf(PendingImports)^.Elements^.Data[I * 3 + 1]));
  Spec := StrToUnicode(StrOf(ObjOf(PendingImports)^.Elements^.Data[I * 3 + 2]));
  { a host module's import loads modules for the host }
  HostLoading := HostOwnedReferrer(Ref);
  try
    ImportComputed := (Data and $400000) <> 0;
    Loaded := LoadAndLink(ResolveRequest(Spec, Ref), M);
  finally
    HostLoading := False;
  end;
  if not Loaded then
  begin
    R := Pending;
    Pending := VUndefined;
    { an error made here has no frames of its own: it takes the import() call's }
    if IsObject(R) and (I <= High(ImportStacks)) and (ImportStacks[I] <> '') then
    begin
      PushRoot(R);
      if not GetV(RootAt(TempTop - 1), AtomStack, St) or not IsString(St) or
         (Pos(#10'    at ', StrToUnicode(StrOf(St))) = 0) then
      begin
        Pending := VUndefined;
        if IsString(St) then Text := StrToUnicode(StrOf(St)) else Text := 'Error';
        SetEngineErrorStack(RootAt(TempTop - 1), Text + ImportStacks[I]);
      end;
      R := RootAt(TempTop - 1);
      PopRoots(1);
    end;
    SettleImport(I, False, R);
    Exit;
  end;
  ValueArrayStore(ObjOf(PendingImports)^.Elements, I * 3 + 1, MkInt(M.Id));
  if (Data and $800000) = 0 then
  begin
    ThenImport(EvaluateModule(M), ImportFulfilledFn, Data);
    Exit;
  end;
  { import.defer(): only the asynchronous modules of its graph evaluated now }
  G := AsyncDepsOf(M);
  try
    if G.Count = 0 then begin ImportFulfilled(Data); Exit; end;
    ValueArrayStore(ObjOf(PendingImports)^.Elements, I * 3 + 2, MkInt(G.Count));
    for J := 0 to G.Count - 1 do ThenImport(EvaluateModule(G[J]), ImportDepFulfilledFn, Data);
  finally
    G.Free;
  end;
end;

{ 13.3.10.1 EvaluateImportCall from ToString(specifier) on: errors reject the promise }
function DoImport(const Referrer: UnicodeString; ModuleId: Integer; Spec, Opts: TValue; var Res: TValue): Boolean;
var P, S, W, Keys, V, F: TValue; I, N, Idx, Ph: Integer; Ref, AType, Key: UnicodeString;
  function Reject: Boolean;
  begin
    V := Pending;
    Pending := VUndefined;
    RejectPendingPromise(RootAt(TempTop - 1), V);
    Res := RootAt(TempTop - 1);
    PopRoots(1);
    Result := True;
  end;
begin
  { the phase read before ToString(specifier) can run code with an import() of its own }
  Ph := ImportPhase;
  ImportPhase := 0;
  PushRoot(Opts);
  PushRoot(Spec);
  P := NewPendingPromise;
  Spec := RootAt(TempTop - 1); Opts := RootAt(TempTop - 2);
  PopRoots(2);
  PushRoot(P);
  PushRoot(Opts);
  if not ToStringV(Spec, S) then begin PopRoots(1); Exit(Reject); end;
  Opts := RootAt(TempTop - 1);
  PopRoots(1);
  PushRoot(S);
  AType := '';
  if Opts <> VUndefined then
  begin
    if not IsObject(Opts) then begin ThrowTypeError('The second argument of import() must be an object'); PopRoots(1); Exit(Reject); end;
    if not GetV(Opts, AtomOf('with'), W) then begin PopRoots(1); Exit(Reject); end;
    if W <> VUndefined then
    begin
      if not IsObject(W) then begin ThrowTypeError('The ''with'' option of import() must be an object'); PopRoots(1); Exit(Reject); end;
      PushRoot(W);
      Keys := OwnKeys(ObjOf(W), True);
      if OwnKeysFailed then begin OwnKeysFailed := False; PopRoots(2); Exit(Reject); end;
      PushRoot(Keys);
      N := ArrayLen(ObjOf(Keys));
      for I := 0 to N - 1 do
      begin
        if not GetV(RootAt(TempTop - 2), StrOf(ObjOf(RootAt(TempTop - 1))^.Elements^.Data[I]), V) then begin PopRoots(3); Exit(Reject); end;
        if not IsString(V) then begin ThrowTypeError('import() attribute values must be strings'); PopRoots(3); Exit(Reject); end;
        Key := StrToUnicode(StrOf(ObjOf(RootAt(TempTop - 1))^.Elements^.Data[I]));
        if Key = 'type' then AType := StrToUnicode(StrOf(V));
      end;
      { AllImportAttributesSupported: the host supports only type }
      for I := 0 to N - 1 do
        if StrToUnicode(StrOf(ObjOf(RootAt(TempTop - 1))^.Elements^.Data[I])) <> 'type' then
        begin
          ThrowError('SyntaxError', 'Unsupported import attribute ''' +
            StrToUnicode(StrOf(ObjOf(RootAt(TempTop - 1))^.Elements^.Data[I])) + '''');
          PopRoots(3);
          Exit(Reject);
        end;
      PopRoots(2);
    end;
  end;
  { import.source(): the host's source module gives its source object; GetModuleSource of a Source Text Module
    Record throws (it has no source representation) }
  if (Ph = 1) and (StrToUnicode(StrOf(RootAt(TempTop - 1))) = HostSourceSpecifier) then
  begin
    PopRoots(1);
    ResolvePendingPromise(RootAt(TempTop - 1), ModuleSourceOf(nil));
    Res := RootAt(TempTop - 1);
    PopRoots(1);
    Exit(True);
  end;
  { an import attribute type the host has no module for: a TypeError, whatever the phase }
  if (AType <> '') and (AType <> 'json') and (AType <> 'text') and (AType <> 'bytes') then
  begin
    ThrowTypeError('Unsupported module type ''' + AType + '''');
    PopRoots(1);
    Exit(Reject);
  end;
  if Ph = 1 then
  begin
    ThrowError('SyntaxError', 'Module source is not available for "' + StrToUnicode(StrOf(RootAt(TempTop - 1))) + '"');
    PopRoots(1);
    Exit(Reject);
  end;
  { the loading runs as a job: import() never evaluates synchronously }
  if AType <> '' then
  begin
    S := StrFromUnicode(ModuleRequestKey(StrToUnicode(StrOf(RootAt(TempTop - 1))), AType));
    PopRoots(1);
    PushRoot(S);
  end;
  Ref := Referrer;
  Idx := ArrayLen(ObjOf(PendingImports)) div 3;
  if Length(ImportStacks) <= Idx then SetLength(ImportStacks, Idx + 16);
  ImportStacks[Idx] := StackFramesNow;
  if Ph = 2 then Idx := Idx or $800000;
  { a specifier computed at run time, for the host's read policy (G2.VM ImportComputed) }
  if ImportComputed then Idx := Idx or $400000;
  ArrayPush(ObjOf(PendingImports), RootAt(TempTop - 2));
  ArrayPush(ObjOf(PendingImports), StrFromUnicode(Ref));
  ArrayPush(ObjOf(PendingImports), RootAt(TempTop - 1));
  F := NewNativeFunctionData(ImportJob, 'import', 0, Idx);
  EnqueueCallback(F);
  PopRoots(1);
  Res := RootAt(TempTop - 1);
  PopRoots(1);
  Result := True;
end;

{ 13.3.12.1 import.meta: an object with a null prototype, made once per module }
function DoImportMeta(ModuleId: Integer; var Res: TValue): Boolean;
var M: TModRec;
begin
  if (ModuleId < 1) or (ModuleId > Mods.Count) then Exit(ThrowError('SyntaxError', 'import.meta outside a module'));
  M := Mods[ModuleId - 1];
  Res := GetRoot(M, rMeta);
  if not IsObject(Res) then
  begin
    Res := MkPtr(NewPlainObject(VNull, 4));
    SetRoot(M, rMeta, Res);
    if Assigned(ImportMetaInitHook) then
    begin
      if not ImportMetaInitHook(Res, M.Path) then Exit(False);
      Res := GetRoot(M, rMeta);
    end;
  end;
  Result := True;
end;

{ 28.1.1.1 AbstractModuleSource(): always a TypeError (it is abstract) }
function AMS_Ctor(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
begin
  Result := ThrowTypeError('AbstractModuleSource is an abstract class and cannot be constructed');
end;

{ 28.1.3.1 get %AbstractModuleSource%.prototype [ @@toStringTag ]: the [[ModuleSourceClassName]] of a module
  source, else undefined (G2 has no module source objects) }
function AMS_ToStringTag(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var Mark: TValue;
begin
  Mark := VUndefined;
  if IsObject(Args[0]) and (JSModSourceKey <> nil) and (ObjOf(Args[0])^.OKind <> okProxy) then
    if not GetV(Args[0], JSModSourceKey, Mark) then Exit(False);
  if (IsObject(HostModSource) and (Args[0] = HostModSource)) or (Mark = VTrue) then
    Res := StrFromUnicode('ModuleSource')
  else Res := VUndefined;
  Result := True;
end;

function AbstractModuleSourceIntrinsic: TValue;
var Base: Integer;
begin
  if IsObject(AMSCtor) then Exit(AMSCtor);
  Base := TempTop;
  PushRoot(NewNativeFunction(AMS_Ctor, 'AbstractModuleSource', 0));
  ObjOf(RootAt(Base))^.OFlags := ObjOf(RootAt(Base))^.OFlags or 32;   { a constructor }
  PushRoot(MkPtr(NewPlainObject(ObjectProto, 4)));
  DefineOwn(RootAt(Base), AtomPrototype, RootAt(Base + 1), 0);
  DefineOwn(RootAt(Base + 1), AtomConstructor, RootAt(Base), pfHidden);
  DefineNativeGetterKey(RootAt(Base + 1), SymKey(SymToStringTagW), 'get [Symbol.toStringTag]', AMS_ToStringTag);
  AMSCtor := RootAt(Base);
  Result := RootAt(Base);
  PopRoots(2);
end;

{ the realm's source object of the host's source module: an object whose prototype inherits from
  %AbstractModuleSource%.prototype (a WebAssembly.Module, say, in a host that has those) }
function ModuleSourceOf(M: TModRec): TValue;
var C, P: TValue; E: PValueArray;
begin
  if (M <> nil) and (M.Path <> HostSourceSpecifier) then
  begin
    { a source text module's own source object, made once }
    E := ObjOf(JSModSources)^.Elements;
    if (E <> nil) and (E^.Len > M.Id) and IsObject(E^.Data[M.Id]) then Exit(E^.Data[M.Id]);
    if not IsObject(JSModSourceProto) then
    begin
      C := AbstractModuleSourceIntrinsic;
      PushRoot(C);
      GetV(RootAt(TempTop - 1), AtomPrototype, P);
      JSModSourceProto := MkPtr(NewPlainObject(P, 4));
      PopRoots(1);
    end;
    PushRoot(MkPtr(NewPlainObject(JSModSourceProto, 2)));
    DefineOwn(RootAt(TempTop - 1), JSModSourceKey, VTrue, pfHidden);
    while (ObjOf(JSModSources)^.Elements = nil) or (ObjOf(JSModSources)^.Elements^.Len <= M.Id) do
      ArrayPush(ObjOf(JSModSources), VUndefined);
    ValueArrayStore(ObjOf(JSModSources)^.Elements, M.Id, RootAt(TempTop - 1));
    Result := RootAt(TempTop - 1);
    PopRoots(1);
    Exit;
  end;
  if IsObject(HostModSource) then Exit(HostModSource);
  C := AbstractModuleSourceIntrinsic;
  PushRoot(C);
  GetV(RootAt(TempTop - 1), AtomPrototype, P);
  PushRoot(MkPtr(NewPlainObject(P, 4)));
  HostModSource := MkPtr(NewPlainObject(RootAt(TempTop - 1), 4));
  PopRoots(2);
  Result := HostModSource;
end;

function ModuleCount: Integer;
begin
  if Mods = nil then Result := 0 else Result := Mods.Count;
end;

function ModuleLoadedSince(const Path: UnicodeString; Mark: Integer): Boolean;
var M: TModRec;
begin
  Result := False;
  if Mods = nil then Exit;
  M := FindModule(Path);
  Result := (M <> nil) and (M.Id >= Mark) and not M.HostOwned;
end;

procedure SetEntryModuleMark(Mark: Integer);
begin
  EntryModuleMark := Mark;
end;

{ whether the module at Path (an import's referrer) was loaded for the host }
function HostOwnedReferrer(const Path: UnicodeString): Boolean;
var M: TModRec;
begin
  Result := False;
  if Mods = nil then Exit;
  M := FindModule(Path);
  Result := (M <> nil) and ((M.Id < EntryModuleMark) or M.HostOwned);
end;

function HasModuleSource(M: TModRec): Boolean;
begin
  { the host's source module, and with experimental JavaScript module sources every source text module }
  Result := Assigned(M) and ((M.Path = HostSourceSpecifier) or (JSModuleSourcesOn and (M.Tpl <> nil)));
end;

procedure SetJSModuleSources(On: Boolean);
begin
  JSModuleSourcesOn := On;
end;

procedure InitModules;
begin
  NSDeferHook := @DeferEval;
  AddRoot(@AMSCtor); AddRoot(@HostModSource); AddRoot(@JSModSourceProto);
  RealmVals([@AMSCtor, @HostModSource, @JSModSourceProto]);
  JSModSourceKey := NewKeyAtom('%moduleSource', kfInternal);
  { the module map and the pending dynamic imports belong to the agent's first realm }
  if RealmInit then Exit;
  Mods := TList<TModRec>.Create;
  AddRoot(@ModRoots);
  AddRoot(@EntryEvalPromise);
  EntryEvalPromise := VUndefined;
  AddRoot(@ResolveErrorV);
  ResolveErrorV := VUndefined;
  AddRoot(@JSModSources);
  JSModSources := MkPtr(NewArray(0, 0));
  AddRoot(@PendingImports);
  ModRoots := MkPtr(NewArray(0, 64));
  PendingImports := MkPtr(NewArray(0, 16));
  ImportHook := @DoImport;
  ImportMetaHook := @DoImportMeta;
  ModSlotNameHook := @ModSlotName;
end;

end.
