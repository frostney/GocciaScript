unit Goccia.Sandbox;
{ GocciaRunner's sandbox mode (ADR 0122): the host side of prelude/sandbox.js, whose fs, goccia and
  goccia:search modules work on this thread's sandbox context (Goccia.Sandbox.Context). The natives here do the
  file system, glob, search, shell and nested-run work; the prelude validates the arguments, defers the callback
  and promise forms, and shapes the results as GocciaRunner's sandbox extension
  (Goccia.RuntimeExtensions.Sandbox) does.

  While a sandbox is set, the thread's engine reads its modules and data files from the virtual file system
  (Goccia.Modules SetFileReadHook). A nested run's completion value crosses to the calling engine, which runs on
  another thread, as text (EncodeSandboxValue, DecodeSandboxValue): primitives, arrays and own data properties,
  cycles cut, as GocciaRunner's sandbox host copies it. }
{$mode delphiunicode}{$H+}
{$I Goccia.Checks.inc}
{$MINFPCONSTPREC 64}
{$POINTERMATH ON}

interface

uses
  Goccia.Sandbox.Context, Goccia.Core;

{ this thread's sandbox (nil: host mode); set before the thread's engine prepares its host }
procedure SetSandbox(Ctx: TGocciaSandboxContext);
function CurrentSandbox: TGocciaSandboxContext;
function SandboxActive: Boolean;
{ while OnHost, the thread reads the host's files instead of the sandbox's (the command line's own --modules
  manifests, which the host reads before the run) }
procedure SandboxHostReads(OnHost: Boolean);
{ the sb* natives on the host's natives object N }
procedure DefineSandboxNatives(N: TValue);
{ a run's completion value as text another engine decodes; False with an exception pending when reading it threw
  (a getter) }
function EncodeSandboxValue(V: TValue; var Text: UnicodeString): Boolean;
{ the value Text encodes, made in this thread's engine }
function DecodeSandboxValue(const Text: UnicodeString): TValue;
{ whether the run ended at a ceiling the sandbox host set (runScript nesting depth, the child's inherited quota):
  the natives end the run with HostAbort; taken }
function TakeSandboxCeiling: Boolean;
{ An embedder's native ends the run at a ceiling of the host's own (its native memory budget, say), past every handler,
  as the sandbox's own natives do: False with the abort pending, which the native returns; TGocciaSandboxHost reports
  the run as sfkResourceLimit with Msg. }
function SandboxCeiling(const Msg: UnicodeString): Boolean;

implementation

uses
  SysUtils, Math, SandboxVirtualFileSystem, SandboxGlob, SandboxFuzzy, SandboxShell, Goccia.Base64,
  Goccia.NativeLimits, Goccia.RegExp.VM, Goccia.Sandbox.FileSystemErrors, Goccia.Sandbox.Search, Goccia.VM, Goccia.Typed,
  Goccia.ObjModel,
  Goccia.Modules, Goccia.EngineFault;

const
  { the largest integer a Number holds exactly }
  MAX_SAFE_INTEGER = 9007199254740991;
  { cycles are cut; a value nested deeper than this is copied as undefined, as a cycle is }
  MAX_ENCODE_DEPTH = 10000;

threadvar
  Box: TGocciaSandboxContext;
  CeilingHit: Boolean;

{ ---- the virtual file system as the thread's files ---- }

function ReadVirtualFile(const Path: UnicodeString; var B: RawByteString): Boolean;
var Bytes: TBytes;
begin
  B := '';
  Result := False;
  if Box = nil then Exit;
  try
    if not Box.Fs.IsFile(Path) then Exit;
    Bytes := Box.Fs.SnapshotReadAllBytes(Path);
  except
    on E: ESandboxFsError do Exit;
  end;
  SetLength(B, Length(Bytes));
  if Length(Bytes) > 0 then Move(Bytes[0], B[1], Length(Bytes));
  Result := True;
end;

{ when a virtual file was last modified (a module reload's clock: under --deterministic every time is the epoch, so
  nothing reloads) }
function VirtualFileTime(const Path: UnicodeString; out Modified: TDateTime): Boolean;
begin
  Modified := 0;
  Result := False;
  if Box = nil then Exit;
  try
    if not Box.Fs.IsFile(Path) then Exit;
    Modified := Box.Fs.Stat(Path).ModifiedAt;
    Result := True;
  except
    on E: ESandboxFsError do Exit;
  end;
end;

{ the search's native working memory, charged to the heap's ceiling as a SharedArrayBuffer's block is }
function ChargeNativeBytes(const ABytes: Int64; const AReserve: Boolean): Boolean;
begin
  Result := True;
  if not AReserve then
  begin
    HeapExternalAdd(-ABytes);
    Exit;
  end;
  if not HeapReserve(ABytes) then Exit(False);
  HeapExternalAdd(ABytes);
end;

{ native work (a search's) charged to the run as instructions, and its limits and a cancellation checked }
function ChargeNativeWork(const AUnits: QWord): Boolean;
begin
  Result := PollInstructionUnits(AUnits);
end;

procedure SetSandbox(Ctx: TGocciaSandboxContext);
begin
  Box := Ctx;
  CeilingHit := False;
  if Ctx <> nil then
  begin
    SetFileReadHook(ReadVirtualFile);
    SetFileTimeHook(VirtualFileTime);
    NativeBytesHook := ChargeNativeBytes;
    NativeWorkHook := ChargeNativeWork;
  end
  else
  begin
    SetFileReadHook(nil);
    SetFileTimeHook(nil);
    NativeBytesHook := nil;
    NativeWorkHook := nil;
  end;
end;

procedure SandboxHostReads(OnHost: Boolean);
begin
  if Box = nil then Exit;
  if OnHost then
  begin
    SetFileReadHook(nil);
    SetFileTimeHook(nil);
  end
  else
  begin
    SetFileReadHook(ReadVirtualFile);
    SetFileTimeHook(VirtualFileTime);
  end;
end;

function CurrentSandbox: TGocciaSandboxContext;
begin
  Result := Box;
end;

function SandboxActive: Boolean;
begin
  Result := Box <> nil;
end;

function TakeSandboxCeiling: Boolean;
begin
  Result := CeilingHit;
  CeilingHit := False;
end;

{ ---- arguments and results ---- }

function ValArg(Args: PValue; Argc, I: Integer): TValue;
begin
  if I <= Argc then Result := Args[I] else Result := VUndefined;
end;

function StrArg(Args: PValue; Argc, I: Integer): UnicodeString;
begin
  if (I <= Argc) and IsString(Args[I]) then Result := StrToUnicode(StrOf(Args[I])) else Result := '';
end;

function BoolArg(Args: PValue; Argc, I: Integer): Boolean;
begin
  Result := (I <= Argc) and (Args[I] = VTrue);
end;

function NumArg(Args: PValue; Argc, I: Integer): Double;
begin
  if (I <= Argc) and IsNumber(Args[I]) then Result := NumOf(Args[I]) else Result := 0;
end;

{ the strings of an array the prelude made (dense, strings only) }
function StrsArg(Args: PValue; Argc, I: Integer): TStringArray;
var A: TValue; E: PValueArray; K, N: Integer;
begin
  Result := nil;
  A := ValArg(Args, Argc, I);
  if not IsObject(A) or (ObjOf(A)^.OKind <> okArray) then Exit;
  E := ObjOf(A)^.Elements;
  if E = nil then Exit;
  N := E^.Len;
  SetLength(Result, N);
  for K := 0 to N - 1 do
    if IsString(E^.Data[K]) then Result[K] := StrToUnicode(StrOf(E^.Data[K])) else Result[K] := '';
end;

function NoSandbox: Boolean;
begin
  Result := ThrowTypeError('the sandbox file system is not available');
end;

{ a new array at the top of the root stack }
procedure PushArray(Cap: Integer);
begin
  PushRoot(MkPtr(NewArray(0, Cap)));
end;

{ V appended to the array at root Base }
procedure Append(Base: Integer; V: TValue);
var L: Int64;
begin
  PushRoot(V);
  L := 0;
  LengthOfArrayLike(RootAt(Base), L);
  DefineElemRaw(RootAt(Base), L, RootAt(TempTop - 1));
  PopRoots(1);
end;

procedure AppendStr(Base: Integer; const S: UnicodeString);
begin
  Append(Base, StrFromUnicode(S));
end;

{ Name = V on the object at root Base }
procedure SetProp(Base: Integer; const Name: UnicodeString; V: TValue);
var K: PStr;
begin
  PushRoot(V);
  K := AtomOf(Name);
  DefineOwn(RootAt(Base), K, RootAt(TempTop - 1), pfDefault);
  PopRoots(1);
end;

{ Node's error for a failed operation (Goccia.Sandbox.FileSystemErrors): code, errno, path, syscall and dest }
function ThrowFsError(E: ESandboxFsError; const Syscall, Path, Dest: UnicodeString; HasDest: Boolean): Boolean;
var D: TSandboxFileSystemErrorDefinition; Base: Integer;
begin
  D := SandboxFileSystemErrorDefinition(E);
  Base := TempTop;
  PushRoot(MakeError(ErrorProto, SandboxFileSystemErrorMessage(D, Syscall, Path, Dest, HasDest)));
  SetProp(Base, 'code', StrFromUnicode(D.Code));
  SetProp(Base, 'errno', MkNumber(D.Errno));
  SetProp(Base, 'path', StrFromUnicode(Path));
  SetProp(Base, 'syscall', StrFromUnicode(Syscall));
  if HasDest then SetProp(Base, 'dest', StrFromUnicode(Dest));
  Pending := RootAt(Base);
  PopRoots(1);
  Result := False;
end;

{ an exception a native met that no case above names: its message as an Error }
function ThrowOther(E: Exception): Boolean;
begin
  { a search's work was refused by the run's poll: its error is pending (an abort that ends the run, or a catchable
    memory RangeError), and nothing else is thrown }
  if E is ENativeWorkStopped then Exit(False);
  Result := ThrowError('Error', E.Message);
end;

{ the run ends at a host-set ceiling, past every handler }
function Ceiling(const Msg: UnicodeString): Boolean;
begin
  CeilingHit := True;
  Result := HostAbort(Msg);
end;

function SandboxCeiling(const Msg: UnicodeString): Boolean;
begin
  Result := Ceiling(Msg);
end;

{ an ArrayBuffer holding Bytes (the memory limit refuses one too large, with a RangeError) }
function BufferOf(const Bytes: TBytes; var Res: TValue): Boolean;
var B: PObj; Proto: TValue;
begin
  if not HeapReserve(Length(Bytes)) then
    Exit(ThrowMemoryLimit);
  if GetV(GlobalObj, AtomOf('ArrayBuffer'), Proto) and GetV(Proto, AtomPrototype, Proto) then
  else
  begin
    Pending := VUndefined;
    Proto := ObjectProto;
  end;
  B := NewArrayBuffer(Length(Bytes), Proto);
  if Length(Bytes) > 0 then Move(Bytes[0], BufferData(B)^, Length(Bytes));
  Res := MkPtr(B);
  Result := True;
end;

{ data as the bytes a write stores: a string as UTF-8, a TypedArray's or DataView's viewed bytes (an out-of-bounds or
  detached TypedArray none; such a DataView throws), and with AllowBuffer an ArrayBuffer whole (sbBytes's copy; a
  script's ArrayBuffer is refused, as Node refuses it) }
function DataBytes(V: TValue; const Method: UnicodeString; AllowBuffer: Boolean; var Bytes: TBytes): Boolean;
var O, B: PObj; Len, Off, BL: Int64; P: PByte; S: UTF8String;
begin
  Bytes := nil;
  Result := True;
  if IsString(V) then
  begin
    S := UTF8Encode(StrToUnicode(StrOf(V)));
    SetLength(Bytes, Length(S));
    if Length(S) > 0 then Move(S[1], Bytes[0], Length(S));
    Exit;
  end;
  if IsObject(V) then
  begin
    O := ObjOf(V);
    if (O^.OKind = okArrayBuffer) and AllowBuffer then
    begin
      Len := BufferLen(O);
      SetLength(Bytes, Len);
      if Len > 0 then Move(BufferData(O)^, Bytes[0], Len);
      Exit;
    end;
    if O^.OKind = okTypedArray then
    begin
      Len := Int64(TALength(O)) * TAElemSize[TAKind(O)];
      SetLength(Bytes, Len);
      if Len > 0 then Move(TAData(O)^, Bytes[0], Len);
      Exit;
    end;
    if O^.OKind = okDataView then
    begin
      B := ObjOf(O^.Aux1);
      Off := O^.Pad;
      if BufferDetached(B) then
        Exit(ThrowTypeError('DataView cannot be used with a detached or out-of-bounds ArrayBuffer'));
      BL := BufferLen(B);
      if O^.Aux2 = MkInt(-1) then Len := BL - Off else Len := IntOf(O^.Aux2);
      if (Off > BL) or (Off + Len > BL) then
        Exit(ThrowTypeError('DataView cannot be used with a detached or out-of-bounds ArrayBuffer'));
      SetLength(Bytes, Len);
      P := BufferData(B) + Off;
      if Len > 0 then Move(P^, Bytes[0], Len);
      Exit;
    end;
  end;
  Result := ThrowTypeError(Method + ' requires string, TypedArray or DataView data');
end;

{ ---- the file system ---- }

{ sbRead(path, text): the file's text, or its bytes as an ArrayBuffer }
function SB_Read(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var Path: UnicodeString; Bytes: TBytes; Text: UnicodeString;
begin
  if Box = nil then Exit(NoSandbox);
  Path := StrArg(Args, Argc, 1);
  try
    if BoolArg(Args, Argc, 2) then
    begin
      Text := Box.Fs.ReadAllText(Path);
      if not HeapReserve(Int64(Length(Text)) * 2) then
        Exit(ThrowMemoryLimit);
      Res := StrFromUnicode(Text);
      Exit(True);
    end;
    Bytes := Box.Fs.ReadAllBytes(Path);
  except
    on E: ESandboxFsError do Exit(ThrowFsError(E, 'readFile', Path, '', False));
    on E: Exception do
    begin
      if IsEngineFault(E) then raise;
      Exit(ThrowOther(E));
    end;
  end;
  Result := BufferOf(Bytes, Res);
end;

{ sbBytes(method, data): a copy of the bytes data holds, as a string or an ArrayBuffer, made when the call is }
function SB_Bytes(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var Bytes: TBytes;
begin
  if IsString(ValArg(Args, Argc, 2)) then
  begin
    Res := ValArg(Args, Argc, 2);
    Exit(True);
  end;
  if not DataBytes(ValArg(Args, Argc, 2), StrArg(Args, Argc, 1), False, Bytes) then Exit(False);
  Result := BufferOf(Bytes, Res);
end;

{ sbWrite(path, data, append): data a string or an ArrayBuffer (sbBytes) }
function SB_Write(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var Path, Op: UnicodeString; Data, Existing, Combined: TBytes; Append_: Boolean;
begin
  if Box = nil then Exit(NoSandbox);
  Path := StrArg(Args, Argc, 1);
  Append_ := BoolArg(Args, Argc, 3);
  if Append_ then Op := 'appendFile' else Op := 'writeFile';
  if not DataBytes(ValArg(Args, Argc, 2), 'fs.' + Op, True, Data) then Exit(False);
  Res := VUndefined;
  try
    if Append_ then
    begin
      { opening for append refuses a read-only path before the content is copied, and creates a missing file; an
        append does not read, so the access time stays }
      Box.Fs.Open(Path, omAppend).Free;
      Existing := Box.Fs.SnapshotReadAllBytes(Path);
      SetLength(Combined, Length(Existing) + Length(Data));
      if Length(Existing) > 0 then Move(Existing[0], Combined[0], Length(Existing));
      if Length(Data) > 0 then Move(Data[0], Combined[Length(Existing)], Length(Data));
      Box.Fs.WriteAllBytes(Path, Combined);
    end
    else
      Box.Fs.WriteAllBytes(Path, Data);
  except
    on E: ESandboxFsError do Exit(ThrowFsError(E, Op, Path, '', False));
    on E: Exception do
    begin
      if IsEngineFault(E) then raise;
      Exit(ThrowOther(E));
    end;
  end;
  Result := True;
end;

{ sbMkdir(path, recursive): the first directory a recursive call made, else undefined }
function SB_Mkdir(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var Path, First: UnicodeString; Recursive: Boolean;
begin
  if Box = nil then Exit(NoSandbox);
  Path := StrArg(Args, Argc, 1);
  Recursive := BoolArg(Args, Argc, 2);
  try
    First := Box.Fs.MakeDirectoryWithResult(Path, Recursive);
  except
    on E: ESandboxFsError do Exit(ThrowFsError(E, 'mkdir', Path, '', False));
    on E: Exception do
    begin
      if IsEngineFault(E) then raise;
      Exit(ThrowOther(E));
    end;
  end;
  if Recursive and (First <> '') then Res := StrFromUnicode(First) else Res := VUndefined;
  Result := True;
end;

{ sbReaddir(path): the entries' names }
function SB_Readdir(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var Path: UnicodeString; Entries: TSandboxFsStatArray; I, Base: Integer;
begin
  if Box = nil then Exit(NoSandbox);
  Path := StrArg(Args, Argc, 1);
  try
    Entries := Box.Fs.List(Path);
  except
    on E: ESandboxFsError do Exit(ThrowFsError(E, 'readdir', Path, '', False));
    on E: Exception do
    begin
      if IsEngineFault(E) then raise;
      Exit(ThrowOther(E));
    end;
  end;
  Base := TempTop;
  PushArray(Length(Entries));
  for I := 0 to High(Entries) do AppendStr(Base, Entries[I].Name);
  Res := RootAt(Base);
  PopRoots(1);
  Result := True;
end;

{ sbStat(path): [path, name, type, size, atimeMs, mtimeMs, ctimeMs, birthtimeMs, isFile, isDirectory] }
function SB_Stat(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var Path: UnicodeString; S: TSandboxFsStat; Base: Integer;
begin
  if Box = nil then Exit(NoSandbox);
  Path := StrArg(Args, Argc, 1);
  try
    S := Box.Fs.Stat(Path);
  except
    on E: ESandboxFsError do Exit(ThrowFsError(E, 'stat', Path, '', False));
    on E: Exception do
    begin
      if IsEngineFault(E) then raise;
      Exit(ThrowOther(E));
    end;
  end;
  Base := TempTop;
  PushArray(10);
  AppendStr(Base, S.Path);
  AppendStr(Base, S.Name);
  AppendStr(Base, SandboxFsKindName(S.Kind));
  Append(Base, MkNumber(S.Size));
  Append(Base, MkNumber(SandboxDateTimeToUnixMilliseconds(S.AccessedAt)));
  Append(Base, MkNumber(SandboxDateTimeToUnixMilliseconds(S.ModifiedAt)));
  Append(Base, MkNumber(SandboxDateTimeToUnixMilliseconds(S.ChangedAt)));
  Append(Base, MkNumber(SandboxDateTimeToUnixMilliseconds(S.BirthTime)));
  Append(Base, MkBool(S.Kind = nkFile));
  Append(Base, MkBool(S.Kind = nkDirectory));
  Res := RootAt(Base);
  PopRoots(1);
  Result := True;
end;

function SB_Exists(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var Path: UnicodeString; B: Boolean;
begin
  if Box = nil then Exit(NoSandbox);
  Path := StrArg(Args, Argc, 1);
  try
    B := Box.Fs.Exists(Path);
  except
    on E: ESandboxFsError do Exit(ThrowFsError(E, 'exists', Path, '', False));
    on E: Exception do
    begin
      if IsEngineFault(E) then raise;
      Exit(ThrowOther(E));
    end;
  end;
  Res := MkBool(B);
  Result := True;
end;

function SB_Rm(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var Path: UnicodeString;
begin
  if Box = nil then Exit(NoSandbox);
  Path := StrArg(Args, Argc, 1);
  try
    Box.Fs.DeletePath(Path, BoolArg(Args, Argc, 2));
  except
    on E: ESandboxFsError do Exit(ThrowFsError(E, 'rm', Path, '', False));
    on E: Exception do
    begin
      if IsEngineFault(E) then raise;
      Exit(ThrowOther(E));
    end;
  end;
  Res := VUndefined;
  Result := True;
end;

{ sbMove(source, destination, copy): a rename, or a file's copy }
function SB_Move(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var Path, Dest: UnicodeString; Copy_: Boolean;
begin
  if Box = nil then Exit(NoSandbox);
  Path := StrArg(Args, Argc, 1);
  Dest := StrArg(Args, Argc, 2);
  Copy_ := BoolArg(Args, Argc, 3);
  try
    if Copy_ then Box.Fs.CopyPath(Path, Dest, False) else Box.Fs.MovePath(Path, Dest);
  except
    on E: ESandboxFsError do
      if Copy_ then Exit(ThrowFsError(E, 'copyFile', Path, Dest, True))
      else Exit(ThrowFsError(E, 'rename', Path, Dest, True));
    on E: Exception do
    begin
      if IsEngineFault(E) then raise;
      Exit(ThrowOther(E));
    end;
  end;
  Res := VUndefined;
  Result := True;
end;

function SB_Cwd(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
begin
  if Box = nil then Exit(NoSandbox);
  Res := StrFromUnicode(Box.Shell.WorkingDirectory);
  Result := True;
end;

{ ---- glob and search ---- }

type
  TProgress = class
    procedure Step;
  end;

{ a step of a glob's or a search's walk: a file or a pattern, charged and polled as native work }
procedure TProgress.Step;
begin
  CheckNativeWork;
end;

var
  Progress: TProgress;

{ the patterns compiled; a refused one is the script's TypeError "<method>: <reason>" }
function CompileGlobs(const Patterns: TStringArray; const Method: UnicodeString;
  var Compiled: TSandboxGlobPatternArray): Boolean;
begin
  Result := True;
  try
    Compiled := CompileSandboxGlobs(Patterns, Progress.Step);
  except
    on E: ESandboxGlobError do Result := ThrowTypeError(Method + ': ' + E.Message);
    on E: Exception do
    begin
      if IsEngineFault(E) then raise;
      Result := ThrowOther(E);
    end;
  end;
end;

{ sbGlob(method, patterns, excludes, cwd, run): the patterns checked; with run, the matching paths }
function SB_Glob(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var Method, Cwd: UnicodeString; Patterns, Excludes: TSandboxGlobPatternArray; Paths: TStringArray;
  I, Base: Integer;
begin
  if Box = nil then Exit(NoSandbox);
  Method := StrArg(Args, Argc, 1);
  Cwd := StrArg(Args, Argc, 4);
  if not CompileGlobs(StrsArg(Args, Argc, 2), Method, Patterns) or
     not CompileGlobs(StrsArg(Args, Argc, 3), Method, Excludes) then Exit(False);
  Res := VUndefined;
  if not BoolArg(Args, Argc, 5) then Exit(True);
  try
    Paths := GlobSandbox(Box.Fs, Patterns, Excludes, Box.Fs.Normalize(Cwd, Box.Shell.WorkingDirectory),
      Progress.Step);
  except
    on E: ESandboxFsError do Exit(ThrowFsError(E, 'glob', Cwd, '', False));
    on E: Exception do
    begin
      if IsEngineFault(E) then raise;
      Exit(ThrowOther(E));
    end;
  end;
  Base := TempTop;
  PushArray(Length(Paths));
  for I := 0 to High(Paths) do AppendStr(Base, Paths[I]);
  Res := RootAt(Base);
  PopRoots(1);
  Result := True;
end;

{ the files a search looks at (cwd, hasGlob, globs, excludes from argument First on) }
function SearchFiles(Args: PValue; Argc, First: Integer; const Method: UnicodeString;
  var Files: TSandboxSearchFileArray): Boolean;
var Cwd: UnicodeString; Globs, Excludes: TSandboxGlobPatternArray;
begin
  Cwd := StrArg(Args, Argc, First);
  if not CompileGlobs(StrsArg(Args, Argc, First + 2), Method, Globs) or
     not CompileGlobs(StrsArg(Args, Argc, First + 3), Method, Excludes) then Exit(False);
  Result := True;
  try
    Files := SandboxSearchFiles(Box.Fs, Box.Fs.Normalize(Cwd, Box.Shell.WorkingDirectory),
      BoolArg(Args, Argc, First + 1), Globs, Excludes, Progress.Step);
  except
    on E: ESandboxFsError do Result := ThrowFsError(E, 'search', Cwd, '', False);
    on E: Exception do
    begin
      if IsEngineFault(E) then raise;
      Result := ThrowOther(E);
    end;
  end;
end;

{ sbGrep(pattern, regex, ignoreCase, maxResults, maxBytes, context, cwd, hasGlob, globs, excludes):
  [truncated, [path, line, column, text, before, after]...] }
function SB_Grep(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var Opt: TGocciaSandboxGrepOptions; Files: TSandboxSearchFileArray; Found: TGocciaSandboxGrepResult;
  I, J, K, Base, M, L: Integer;
begin
  if Box = nil then Exit(NoSandbox);
  Opt := Default(TGocciaSandboxGrepOptions);
  Opt.Pattern := StrArg(Args, Argc, 1);
  Opt.Regex := BoolArg(Args, Argc, 2);
  Opt.IgnoreCase := BoolArg(Args, Argc, 3);
  Opt.MaxResults := Integer(Trunc(NumArg(Args, Argc, 4)));
  Opt.MaxBytes := Trunc(NumArg(Args, Argc, 5));
  Opt.Context := Integer(Trunc(NumArg(Args, Argc, 6)));
  if not SearchFiles(Args, Argc, 7, 'grep', Files) then Exit(False);
  try
    Found := SandboxGrep(Box.Fs, Files, Opt);
  except
    on E: ENativeMemoryLimit do Exit(ThrowMemoryLimit(E.Message));
    on E: ESandboxGrepPatternError do Exit(ThrowError('SyntaxError', E.Message));
    on E: ERegExpStackExhausted do Exit(ThrowError('RangeError', E.Message));
    on E: ERegExpRuntimeError do Exit(ThrowError('Error', E.Message));
    on E: Exception do
    begin
      if IsEngineFault(E) then raise;
      Exit(ThrowOther(E));
    end;
  end;
  Base := TempTop;
  PushArray(Length(Found.Matches) + 1);
  Append(Base, MkBool(Found.Truncated));
  for I := 0 to High(Found.Matches) do
  begin
    M := TempTop;
    PushArray(6);
    AppendStr(M, Found.Matches[I].Path);
    Append(M, MkNumber(Found.Matches[I].Line));
    Append(M, MkNumber(Found.Matches[I].Column));
    AppendStr(M, Found.Matches[I].Text);
    for K := 0 to 1 do
    begin
      L := TempTop;
      PushArray(0);
      if K = 0 then
        for J := 0 to High(Found.Matches[I].Before) do AppendStr(L, Found.Matches[I].Before[J])
      else
        for J := 0 to High(Found.Matches[I].After) do AppendStr(L, Found.Matches[I].After[J]);
      Append(M, RootAt(L));
      PopRoots(1);
    end;
    Append(Base, RootAt(M));
    PopRoots(1);
  end;
  Res := RootAt(Base);
  PopRoots(1);
  { the matches' text is script values now }
  ReleaseNativeBytes(Found.ReservedBytes);
  Result := True;
end;

{ sbFuzzy(query, limit, cwd, hasGlob, globs, excludes): [truncated, [path, score, positions]...] }
function SB_Fuzzy(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var Query: UnicodeString; Limit: Int64; Files: TSandboxSearchFileArray; Found: TSandboxFuzzyMatchArray;
  Match: TSandboxFuzzyMatch; Count, I, K, Base, M, P: Integer;
begin
  if Box = nil then Exit(NoSandbox);
  Query := FoldSandboxFuzzyQuery(StrArg(Args, Argc, 1));
  Limit := Trunc(NumArg(Args, Argc, 2));
  if not SearchFiles(Args, Argc, 3, 'fuzzyFind', Files) then Exit(False);
  SetLength(Found, Length(Files));
  Count := 0;
  for I := 0 to High(Files) do
    if TrySandboxFuzzyMatch(Files[I].Path, Query, Match) then
    begin
      Found[Count] := Match;
      Inc(Count);
    end;
  SetLength(Found, Count);
  SortSandboxFuzzyMatches(Found);
  if Count > Limit then Count := Integer(Limit);
  Base := TempTop;
  PushArray(Count + 1);
  Append(Base, MkBool(Length(Found) > Limit));
  for I := 0 to Count - 1 do
  begin
    M := TempTop;
    PushArray(3);
    AppendStr(M, Found[I].Path);
    Append(M, MkNumber(Found[I].Score));
    P := TempTop;
    PushArray(Length(Found[I].Positions));
    for K := 0 to High(Found[I].Positions) do Append(P, MkNumber(Found[I].Positions[K]));
    Append(M, RootAt(P));
    PopRoots(1);
    Append(Base, RootAt(M));
    PopRoots(1);
  end;
  Res := RootAt(Base);
  PopRoots(1);
  Result := True;
end;

{ ---- the shell and nested runs ---- }

{ a nested run the sandbox host refused before it began: a ceiling ends the calling run }
function NestedFailure(E: Exception): Boolean;
begin
  if (E is EGocciaSandboxNestingLimitExceeded) or (E is ESandboxFsQuotaExceeded) then Exit(Ceiling(E.Message));
  if E is ESandboxFsError then Exit(ThrowError('Error', E.Message));
  { the host's own failure (a capability audit it could not deliver) ends the run too }
  Result := HostAbort(E.Message);
end;

{ sbShell(commandLine): [exitCode, stdout, stderr] }
function SB_Shell(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var R: TExecResult; Base: Integer;
begin
  if Box = nil then Exit(NoSandbox);
  try
    R := Box.Shell.Exec(StrArg(Args, Argc, 1));
  except
    on E: Exception do
    begin
      if IsEngineFault(E) then raise;
      Exit(NestedFailure(E));
    end;
  end;
  Base := TempTop;
  PushArray(3);
  Append(Base, MkNumber(R.ExitCode));
  AppendStr(Base, R.Output);
  AppendStr(Base, R.ErrorOutput);
  Res := RootAt(Base);
  PopRoots(1);
  Result := True;
end;

function IsDirectoryTargetPath(const Path: UnicodeString): Boolean;
var P: UnicodeString;
begin
  P := NormalizeSandboxPathSeparators(Path);
  Result := (P = '/') or ((P <> '') and (P[Length(P)] = '/'));
end;

{ sbRunScript(path, isolated, includeDiff, diffFormat, copies): copies [kind, a, b]... (0: a parent path a to b,
  '' for the same path; 1: a file b's text at a; 2: base64 b's bytes at a). [ok, exitCode, stdout, stderr,
  result, error, diff, failureKind] }
function SB_RunScript(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var Opt: TGocciaSandboxRunOptions; R: TGocciaSandboxRunResult; Copies: TStringArray; C: TGocciaSandboxCopySpec;
  I, Base: Integer; Kind, A, B: UnicodeString; Bytes: TBytes;
begin
  if Box = nil then Exit(NoSandbox);
  if not Assigned(Box.RunScriptCallback) then Exit(ThrowTypeError('runScript is not configured'));
  Opt := DefaultSandboxRunOptions;
  Opt.Isolated := BoolArg(Args, Argc, 2);
  if BoolArg(Args, Argc, 3) then
  begin
    Opt.IncludeDiff := True;
    Opt.Isolated := True;
    if StrArg(Args, Argc, 4) <> '' then Opt.DiffFormat := StrArg(Args, Argc, 4);
  end;
  Copies := StrsArg(Args, Argc, 5);
  try
    I := 0;
    while I + 2 <= High(Copies) do
    begin
      Kind := Copies[I];
      A := Copies[I + 1];
      B := Copies[I + 2];
      Inc(I, 3);
      C := Default(TGocciaSandboxCopySpec);
      if Kind = '0' then
      begin
        C.Kind := sckParentPath;
        C.FromPath := Box.Fs.Normalize(A, Box.Shell.WorkingDirectory);
        C.ToDirectory := IsDirectoryTargetPath(B);
        if B = '' then C.ToPath := C.FromPath else C.ToPath := Box.Fs.Normalize(B, '/');
      end
      else
      begin
        C.Path := Box.Fs.Normalize(A, '/');
        if Kind = '2' then
        begin
          C.Kind := sckBytes;
          if not TryDecodeBase64Standard(B, Bytes) then Exit(ThrowError('Error', 'Invalid base64 data'));
          C.Bytes := Bytes;
        end
        else
        begin
          C.Kind := sckText;
          C.Text := B;
        end;
      end;
      SetLength(Opt.Copies, Length(Opt.Copies) + 1);
      Opt.Copies[High(Opt.Copies)] := C;
      Opt.Isolated := True;
    end;
    R := Box.RunScriptCallback(Box, Box.Fs.Normalize(StrArg(Args, Argc, 1), Box.Shell.WorkingDirectory), Opt);
  except
    on E: Exception do
    begin
      if IsEngineFault(E) then raise;
      Exit(NestedFailure(E));
    end;
  end;
  Base := TempTop;
  PushArray(8);
  Append(Base, MkBool(R.Ok));
  Append(Base, MkNumber(R.ExitCode));
  AppendStr(Base, R.Output);
  AppendStr(Base, R.ErrorOutput);
  if R.HasResultEncoded then Append(Base, DecodeSandboxValue(R.ResultEncoded)) else Append(Base, VNull);
  if R.ErrorMessage <> '' then AppendStr(Base, R.ErrorMessage) else Append(Base, VNull);
  if R.DiffRequested then AppendStr(Base, R.Diff) else Append(Base, VNull);
  AppendStr(Base, SandboxFailureKindName(R.FailureKind));
  Res := RootAt(Base);
  PopRoots(1);
  Result := True;
end;

procedure DefineSandboxNatives(N: TValue);
begin
  PushRoot(N);
  DefineNative(RootAt(TempTop - 1), 'sbRead', SB_Read, 2);
  DefineNative(RootAt(TempTop - 1), 'sbBytes', SB_Bytes, 2);
  DefineNative(RootAt(TempTop - 1), 'sbWrite', SB_Write, 3);
  DefineNative(RootAt(TempTop - 1), 'sbMkdir', SB_Mkdir, 2);
  DefineNative(RootAt(TempTop - 1), 'sbReaddir', SB_Readdir, 1);
  DefineNative(RootAt(TempTop - 1), 'sbStat', SB_Stat, 1);
  DefineNative(RootAt(TempTop - 1), 'sbExists', SB_Exists, 1);
  DefineNative(RootAt(TempTop - 1), 'sbRm', SB_Rm, 2);
  DefineNative(RootAt(TempTop - 1), 'sbMove', SB_Move, 3);
  DefineNative(RootAt(TempTop - 1), 'sbCwd', SB_Cwd, 0);
  DefineNative(RootAt(TempTop - 1), 'sbGlob', SB_Glob, 5);
  DefineNative(RootAt(TempTop - 1), 'sbGrep', SB_Grep, 10);
  DefineNative(RootAt(TempTop - 1), 'sbFuzzy', SB_Fuzzy, 6);
  DefineNative(RootAt(TempTop - 1), 'sbShell', SB_Shell, 1);
  DefineNative(RootAt(TempTop - 1), 'sbRunScript', SB_RunScript, 5);
  PopRoots(1);
end;

{ ---- a run's value across engines ----
  u undefined, n null, t true, f false, d<16 hex digits> a number's bits, s<length>:<code units> a string,
  a<count>: the elements, o<count>: each own string key (an s) and its value }

function EncodeInto(V: TValue; Depth: Integer; SeenBase: Integer; var Text: UnicodeString): Boolean;
var D: Double; Q: QWord; Keys, E: TValue; I, N, K, Self_: Integer; L: Int64; O: PObj; Parts: UnicodeString;
begin
  Result := True;
  if V = VUndefined then begin Text := Text + 'u'; Exit; end;
  if V = VNull then begin Text := Text + 'n'; Exit; end;
  if V = VTrue then begin Text := Text + 't'; Exit; end;
  if V = VFalse then begin Text := Text + 'f'; Exit; end;
  if IsNumber(V) then
  begin
    D := NumOf(V);
    Move(D, Q, 8);
    Text := Text + 'd' + UnicodeString(IntToHex(Q, 16));
    Exit;
  end;
  if IsString(V) then
  begin
    Parts := StrToUnicode(StrOf(V));
    Text := Text + 's' + UnicodeString(IntToStr(Length(Parts))) + ':' + Parts;
    Exit;
  end;
  if not IsObject(V) or (Depth > MAX_ENCODE_DEPTH) then begin Text := Text + 'u'; Exit; end;
  { a cycle is cut: an object already on the path copies as undefined }
  for I := SeenBase to TempTop - 1 do
    if RootAt(I) = V then begin Text := Text + 'u'; Exit; end;
  PushRoot(V);
  Self_ := TempTop - 1;
  try
    O := ObjOf(V);
    if O^.OKind = okArray then
    begin
      L := 0;
      if not LengthOfArrayLike(RootAt(Self_), L) then Exit(False);
      Text := Text + 'a' + UnicodeString(IntToStr(L)) + ':';
      for K := 0 to Integer(L) - 1 do
      begin
        if not GetElemV(RootAt(Self_), MkInt(K), E) then Exit(False);
        if not EncodeInto(E, Depth + 1, SeenBase, Text) then Exit(False);
      end;
      Exit;
    end;
    if not OwnPropertyKeysV(RootAt(Self_), Keys) then Exit(False);
    PushRoot(Keys);
    N := 0;
    L := 0;
    LengthOfArrayLike(RootAt(Self_ + 1), L);
    for K := 0 to Integer(L) - 1 do
      if IsString(ObjOf(RootAt(Self_ + 1))^.Elements^.Data[K]) then Inc(N);
    Text := Text + 'o' + UnicodeString(IntToStr(N)) + ':';
    for K := 0 to Integer(L) - 1 do
    begin
      E := ObjOf(RootAt(Self_ + 1))^.Elements^.Data[K];
      if not IsString(E) then Continue;
      Parts := StrToUnicode(StrOf(E));
      Text := Text + 's' + UnicodeString(IntToStr(Length(Parts))) + ':' + Parts;
      if not GetV(RootAt(Self_), AtomOf(Parts), E) then Exit(False);
      if not EncodeInto(E, Depth + 1, SeenBase, Text) then Exit(False);
    end;
  finally
    PopRoots(TempTop - Self_);
  end;
end;

function EncodeSandboxValue(V: TValue; var Text: UnicodeString): Boolean;
var Base: Integer;
begin
  Text := '';
  Base := TempTop;
  Result := EncodeInto(V, 0, Base, Text);
  PopRoots(TempTop - Base);
end;

{ the value at Text[P] on, P past it }
function DecodeAt(const Text: UnicodeString; var P: Integer): TValue;
var C: WideChar; Q: QWord; D: Double; N, K, KL, Base, Colon: Integer; Key: UnicodeString; KeyAtom: PStr;

  function Count: Integer;
  begin
    Colon := P;
    while (Colon <= Length(Text)) and (Text[Colon] <> ':') do Inc(Colon);
    Result := StrToIntDef(string(Copy(Text, P, Colon - P)), 0);
    P := Colon + 1;
  end;

begin
  Result := VUndefined;
  if P > Length(Text) then Exit;
  C := Text[P];
  Inc(P);
  case C of
    'n': Result := VNull;
    't': Result := VTrue;
    'f': Result := VFalse;
    'd':
      begin
        Q := StrToQWord('$' + string(Copy(Text, P, 16)));
        Inc(P, 16);
        Move(Q, D, 8);
        Result := MkNumber(D);
      end;
    's':
      begin
        N := Count;
        Result := StrFromUnicode(Copy(Text, P, N));
        Inc(P, N);
      end;
    'a':
      begin
        N := Count;
        Base := TempTop;
        PushRoot(MkPtr(NewArray(0, N)));
        for K := 0 to N - 1 do
        begin
          PushRoot(DecodeAt(Text, P));
          { an own element, whatever Array.prototype holds }
          DefineElemRaw(RootAt(Base), K, RootAt(TempTop - 1));
          PopRoots(1);
        end;
        Result := RootAt(Base);
        PopRoots(1);
      end;
    'o':
      begin
        N := Count;
        Base := TempTop;
        PushRoot(MkPtr(NewPlainObject(ObjectProto, N)));
        for K := 0 to N - 1 do
        begin
          Inc(P);   { the key's 's' }
          KL := Count;
          Key := Copy(Text, P, KL);
          Inc(P, KL);
          PushRoot(DecodeAt(Text, P));
          { the keys are distinct own keys of the copied object }
          KeyAtom := AtomOf(Key);
          DefineOwn(RootAt(Base), KeyAtom, RootAt(TempTop - 1), pfDefault);
          PopRoots(1);
        end;
        Result := RootAt(Base);
        PopRoots(1);
      end;
  end;
end;

function DecodeSandboxValue(const Text: UnicodeString): TValue;
var P: Integer;
begin
  P := 1;
  Result := DecodeAt(Text, P);
end;

initialization
  Progress := TProgress.Create;

finalization
  Progress.Free;

end.
