unit HostOutputFiles;

{$I Shared.inc}

{ Output files a config file names, opened so that they stay where the config
  was allowed to put them.

  A config may only write inside its own directory, and the CLI checks that
  when it reads the config. The write itself can come much later — coverage,
  profiles, and a runner's report are written when the run ends — and in
  between, anything that can write to the project (the script itself, given
  a write grant, or another process) can swap a directory on the way for a
  symbolic link to somewhere else. Checking again just before the write only
  narrows that window.

  So a confined output is registered with its route from the config's
  directory, and that directory's identity (device and inode) is pinned. The
  write then opens the directory, confirms it is still the same one, and walks
  the route one component at a time relative to the open directory with
  O_NOFOLLOW, creating the file the same way: a symbolic link anywhere on the
  route, the leaf included, refuses the write rather than being followed, and
  no path is ever resolved again from the top. A leaf that is not a regular
  file with a single link is refused too, before it is truncated.

  Outputs nobody registered (a path given on the command line) open as they
  always have. Windows and the Lakon/WASI lane re-check the path just before
  the write instead: the parent must still resolve to the directory it did
  when the config was read, and the leaf must not be a link. }

interface

uses
  Classes,
  SysUtils;

type
  EHostOutputRefused = class(Exception);

{ Registers APath (as the option holds it) as an output a config confined to
  ACanonicalRoot, the config's directory. ACanonicalPath is APath with its
  existing directories resolved, as the containment check saw it; it must lie
  within ACanonicalRoot. Writes to APath, and to a file directly inside APath
  when it names a directory, then go through the pinned route. Register before
  any worker thread writes; registration is not synchronized. }
procedure RegisterConfinedHostOutput(const APath, ACanonicalPath,
  ACanonicalRoot: string);
{ Forgets every registration. For tests. }
procedure ClearConfinedHostOutputs;
{ True when a write to APath goes through a registration. }
function IsConfinedHostOutput(const APath: string): Boolean;

{ A new, empty file at APath for writing, replacing what was there. A
  confined output is opened through its pinned route and raises
  EHostOutputRefused when the route no longer leads where it did; any other
  path is created as TFileStream(fmCreate) would. The caller frees the
  stream. }
function CreateHostOutputStream(const APath: string): TStream;
{ AStrings.SaveToFile(APath), through CreateHostOutputStream. }
procedure SaveStringsToHostFile(const AStrings: TStrings; const APath: string);
{ AText as UTF-8 (lone surrogates replaced), through CreateHostOutputStream. }
procedure WriteHostOutputText(const APath, AText: string);

implementation

uses
  {$IF DEFINED(UNIX) AND NOT DEFINED(LAKON)}BaseUnix,{$IFEND}
  FileUtils,
  TextEncoding;

type
  TConfinedOutput = record
    Path: string;
    { APath's route below Root, components separated by '/'; '' when the
      output is the root directory itself. }
    Route: string;
    CanonicalPath: string;
    Root: string;
    RootKnown: Boolean;
    RootDevice: QWord;
    RootInode: QWord;
  end;

  { A stream over a descriptor this unit opened, closed with the stream. }
  THostOutputStream = class(THandleStream)
  public
    destructor Destroy; override;
  end;

var
  ConfinedOutputs: array of TConfinedOutput;

destructor THostOutputStream.Destroy;
begin
  FileClose(Handle);
  inherited Destroy;
end;

function SameHostPath(const ALeft, ARight: string): Boolean;
begin
  {$IF DEFINED(DARWIN) OR DEFINED(MSWINDOWS)}
  Result := SameText(ALeft, ARight);
  {$ELSE}
  Result := ALeft = ARight;
  {$IFEND}
end;

function StripTrailingDelimiter(const APath: string): string;
begin
  Result := APath;
  while (Length(Result) > 1) and (Result[Length(Result)] = PathDelim) do
    Delete(Result, Length(Result), 1);
end;

{$IF DEFINED(UNIX) AND NOT DEFINED(LAKON)}
{ POSIX openat(2); variadic in C, so declared varargs for the mode. }
function HostOpenAt(ADirectory: cint; APath: PAnsiChar; AFlags: cint): cint;
  cdecl; varargs; external 'c' name 'openat';
{$IFDEF LINUX}
function HostErrnoLocation: pcint; cdecl; external 'c' name '__errno_location';
{$ELSE}
function HostErrnoLocation: pcint; cdecl; external 'c' name '__error';
{$ENDIF}

function SplitRoute(const ARoute: string): TStringArray;
var
  Start, I: Integer;
begin
  Result := nil;
  Start := 1;
  for I := 1 to Length(ARoute) + 1 do
    if (I > Length(ARoute)) or (ARoute[I] = '/') then
    begin
      SetLength(Result, Length(Result) + 1);
      Result[High(Result)] := Copy(ARoute, Start, I - Start);
      Start := I + 1;
    end;
end;

function HostErrorText: string;
begin
  Result := SysErrorMessage(HostErrnoLocation^);
end;

function DirectoryIdentity(const APath: string; out ADevice,
  AInode: QWord): Boolean;
var
  Info: Stat;
  PathBytes: TBytes;
  ErrorOffset: Integer;
begin
  Result := TryEncodeUTF8NullTerminated(APath, PathBytes, ErrorOffset) and
    (fpStat(PAnsiChar(@PathBytes[0]), Info) = 0) and fpS_ISDIR(Info.st_mode);
  if Result then
  begin
    ADevice := QWord(Info.st_dev);
    AInode := QWord(Info.st_ino);
  end;
end;
{$IFEND}

procedure RegisterConfinedHostOutput(const APath, ACanonicalPath,
  ACanonicalRoot: string);
var
  Entry: TConfinedOutput;
  Root, Canonical: string;
begin
  Root := StripTrailingDelimiter(ACanonicalRoot);
  Canonical := StripTrailingDelimiter(ACanonicalPath);
  Entry := Default(TConfinedOutput);
  Entry.Path := StripTrailingDelimiter(ExpandFileName(APath));
  Entry.CanonicalPath := Canonical;
  Entry.Root := Root;
  if SameHostPath(Canonical, Root) then
    Entry.Route := ''
  else if (Length(Canonical) > Length(Root)) and
     SameHostPath(Copy(Canonical, 1, Length(Root)), Root) and
     ((Root[Length(Root)] = PathDelim) or
      (Canonical[Length(Root) + 1] = PathDelim)) then
  begin
    Entry.Route := Copy(Canonical, Length(Root) + 1, MaxInt);
    while (Entry.Route <> '') and (Entry.Route[1] = PathDelim) do
      Delete(Entry.Route, 1, 1);
    Entry.Route := StringReplace(Entry.Route, PathDelim, '/', [rfReplaceAll]);
  end
  else
    raise EHostOutputRefused.CreateFmt('%s is not inside %s',
      [ACanonicalPath, ACanonicalRoot]);
  {$IF DEFINED(UNIX) AND NOT DEFINED(LAKON)}
  Entry.RootKnown := DirectoryIdentity(Root, Entry.RootDevice,
    Entry.RootInode);
  {$IFEND}
  SetLength(ConfinedOutputs, Length(ConfinedOutputs) + 1);
  ConfinedOutputs[High(ConfinedOutputs)] := Entry;
end;

procedure ClearConfinedHostOutputs;
begin
  ConfinedOutputs := nil;
end;

{ The registration a write to APath goes through, with the route and the
  canonical path of the file itself. }
function FindConfinedOutput(const APath: string; out AEntry: TConfinedOutput;
  out ARoute, ACanonicalPath: string): Boolean;
var
  Expanded, Name: string;
  I: Integer;
begin
  Result := False;
  if Length(ConfinedOutputs) = 0 then
    Exit;
  Expanded := StripTrailingDelimiter(ExpandFileName(APath));
  for I := 0 to High(ConfinedOutputs) do
  begin
    AEntry := ConfinedOutputs[I];
    if SameHostPath(Expanded, AEntry.Path) then
    begin
      ARoute := AEntry.Route;
      ACanonicalPath := AEntry.CanonicalPath;
      Exit(True);
    end;
    { A file directly inside a registered directory (a directory output). }
    if SameHostPath(ExtractFileDir(Expanded), AEntry.Path) then
    begin
      Name := ExtractFileName(Expanded);
      if AEntry.Route = '' then
        ARoute := Name
      else
        ARoute := AEntry.Route + '/' + Name;
      ACanonicalPath := IncludeTrailingPathDelimiter(AEntry.CanonicalPath) +
        Name;
      Exit(True);
    end;
  end;
end;

function IsConfinedHostOutput(const APath: string): Boolean;
var
  Entry: TConfinedOutput;
  Route, Canonical: string;
begin
  Result := FindConfinedOutput(APath, Entry, Route, Canonical);
end;

procedure Refuse(const APath, AReason: string);
begin
  raise EHostOutputRefused.CreateFmt('Refusing to write %s: %s; a config ' +
    'may only write inside its own directory', [APath, AReason]);
end;

{$IF DEFINED(UNIX) AND NOT DEFINED(LAKON)}
function OpenConfinedOutput(const APath: string; const AEntry: TConfinedOutput;
  const ARoute: string): TStream;
const
  NEW_FILE_MODE = &666;
var
  Parts: TStringArray;
  PathBytes: TBytes;
  ErrorOffset, I: Integer;
  Directory, Next: cint;
  Info: Stat;
  Walked: string;
begin
  if ARoute = '' then
    Refuse(APath, 'it names the config''s directory itself');
  if not TryEncodeUTF8NullTerminated(AEntry.Root, PathBytes, ErrorOffset) then
    Refuse(APath, 'the path cannot be encoded for the host');
  { The root is opened by name, which follows links, and then held to the
    identity it had when the config was read. }
  Directory := fpOpen(PAnsiChar(@PathBytes[0]), O_RDONLY or O_NONBLOCK);
  if Directory < 0 then
    Refuse(APath, AEntry.Root + ' cannot be opened: ' +
      SysErrorMessage(fpgeterrno));
  try
    if (fpFStat(Directory, Info) <> 0) or not fpS_ISDIR(Info.st_mode) or
       not AEntry.RootKnown or (QWord(Info.st_dev) <> AEntry.RootDevice) or
       (QWord(Info.st_ino) <> AEntry.RootInode) then
      Refuse(APath, AEntry.Root + ' is no longer the directory it was when ' +
        'the config was read');
    Parts := SplitRoute(ARoute);
    Walked := AEntry.Root;
    { Each directory on the route, relative to the one before, never
      through a link. }
    for I := 0 to High(Parts) - 1 do
    begin
      Walked := IncludeTrailingPathDelimiter(Walked) + Parts[I];
      if not TryEncodeUTF8NullTerminated(Parts[I], PathBytes, ErrorOffset) then
        Refuse(APath, 'the path cannot be encoded for the host');
      Next := HostOpenAt(Directory, PAnsiChar(@PathBytes[0]),
        O_RDONLY or O_NOFOLLOW or O_NONBLOCK);
      if Next < 0 then
      begin
        { Nothing creates a missing directory for an output, as before. }
        if HostErrnoLocation^ = ESysENOENT then
          raise EHostOutputRefused.CreateFmt('Cannot write %s: the ' +
            'directory %s does not exist', [APath, Walked]);
        Refuse(APath, Walked + ' is a symbolic link or cannot be opened (' +
          HostErrorText + ')');
      end;
      fpClose(Directory);
      Directory := Next;
      if (fpFStat(Directory, Info) <> 0) or not fpS_ISDIR(Info.st_mode) then
        Refuse(APath, Walked + ' is not a directory');
    end;
    if not TryEncodeUTF8NullTerminated(Parts[High(Parts)], PathBytes,
         ErrorOffset) then
      Refuse(APath, 'the path cannot be encoded for the host');
    { Not truncated yet: what is there has to be a plain file first. }
    Next := HostOpenAt(Directory, PAnsiChar(@PathBytes[0]),
      O_WRONLY or O_CREAT or O_NOFOLLOW or O_NONBLOCK, cint(NEW_FILE_MODE));
    if Next < 0 then
      Refuse(APath, 'it is a symbolic link or cannot be created (' +
        HostErrorText + ')');
  finally
    fpClose(Directory);
  end;
  if (fpFStat(Next, Info) <> 0) or not fpS_ISREG(Info.st_mode) or
     (Info.st_nlink <> 1) or (fpFTruncate(Next, 0) <> 0) then
  begin
    fpClose(Next);
    Refuse(APath, 'it is not a plain file with a single link');
  end;
  Result := THostOutputStream.Create(THandle(Next));
end;
{$ELSE}
function OpenConfinedOutput(const APath: string; const AEntry: TConfinedOutput;
  const ARoute, ACanonicalPath: string): TStream;
var
  Parent: string;
begin
  if ARoute = '' then
    Refuse(APath, 'it names the config''s directory itself');
  if HostPathIsSymlink(APath) then
    Refuse(APath, 'it is a symbolic link');
  if not DirectoryExists(ExtractFileDir(ExpandFileName(APath))) then
    raise EHostOutputRefused.CreateFmt('Cannot write %s: the directory %s ' +
      'does not exist', [APath, ExtractFileDir(ExpandFileName(APath))]);
  Parent := CanonicalHostPath(ExtractFileDir(ExpandFileName(APath)));
  if (Parent <> '') and not SameHostPath(StripTrailingDelimiter(Parent),
     StripTrailingDelimiter(ExtractFileDir(ACanonicalPath))) then
    Refuse(APath, 'its directory no longer resolves to ' +
      ExtractFileDir(ACanonicalPath));
  Result := TFileStream.Create(APath, fmCreate);
end;
{$IFEND}

function CreateHostOutputStream(const APath: string): TStream;
var
  Entry: TConfinedOutput;
  Route, Canonical: string;
begin
  if not FindConfinedOutput(APath, Entry, Route, Canonical) then
    Exit(TFileStream.Create(APath, fmCreate));
  {$IF DEFINED(UNIX) AND NOT DEFINED(LAKON)}
  Result := OpenConfinedOutput(APath, Entry, Route);
  {$ELSE}
  Result := OpenConfinedOutput(APath, Entry, Route, Canonical);
  {$IFEND}
end;

procedure SaveStringsToHostFile(const AStrings: TStrings; const APath: string);
var
  Stream: TStream;
begin
  Stream := CreateHostOutputStream(APath);
  try
    AStrings.SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure WriteHostOutputText(const APath, AText: string);
var
  Bytes: TBytes;
  Stream: TStream;
begin
  Bytes := EncodeUTF8WithReplacement(AText);
  Stream := CreateHostOutputStream(APath);
  try
    if Length(Bytes) > 0 then
      Stream.WriteBuffer(Bytes[0], Length(Bytes));
  finally
    Stream.Free;
  end;
end;

end.
