unit FileUtils;

{$I Shared.inc}

interface

uses
  {$IFDEF UNIX}BaseUnix,{$ENDIF}
  Classes,
  SysUtils;

function FindAllFiles(const ADirectory: string; const AFileExtension: string): TStringList; overload;
function FindAllFiles(const ADirectory: string; const AFileExtensions: array of string): TStringList; overload;
function ExpandHostFileName(const APath: string): string;
function HostDirectoryExists(const APath: string): Boolean;
function HostFileExists(const APath: string): Boolean;

{ True when APath itself is a symbolic link (UNIX) or a reparse
  point / junction (Windows). Does not follow the link. }
function HostPathIsSymlink(const APath: string): Boolean;

{ Read an entire file as strict UTF-8 source text. No BOM stripping or
  newline normalization is performed. Invalid UTF-8 raises EConvertError. }
function ReadUTF8FileText(const APath: string): string;
procedure WriteUTF8FileText(const APath, AText: string);

{ Read an entire file as raw bytes, preserving every byte exactly
  (NUL bytes, non-UTF-8 sequences, and original newlines). }
function ReadFileBytes(const APath: string): TBytes;

implementation

uses
  TextEncoding;

function ExpandHostFileName(const APath: string): string;
begin
  Result := ExpandFileName(APath);
end;

function HostDirectoryExists(const APath: string): Boolean;
begin
  Result := DirectoryExists(APath);
end;

function HostFileExists(const APath: string): Boolean;
begin
  Result := FileExists(APath);
end;

function HostPathIsSymlink(const APath: string): Boolean;
{$IFDEF UNIX}
var
  Info: Stat;
  ErrorOffset: Integer;
  PathBytes: TBytes;
begin
  if not TryEncodeUTF8NullTerminated(APath, PathBytes, ErrorOffset) then
    Exit(False);
  Result := (fpLStat(PAnsiChar(@PathBytes[0]), Info) = 0) and
    fpS_ISLNK(Info.st_mode);
end;
{$ELSE}
var
  Attr: LongInt;
begin
  Attr := FileGetAttr(APath);
  Result := (Attr <> -1) and ((Attr and faSymLink) <> 0);
end;
{$ENDIF}

function MatchesExtension(const AName: string; const AExtensions: array of string): Boolean;
var
  Ext: string;
  I: Integer;
begin
  Ext := ExtractFileExt(AName);
  for I := Low(AExtensions) to High(AExtensions) do
    if Ext = AExtensions[I] then
      Exit(True);
  Result := False;
end;

function FindAllFiles(const ADirectory: string; const AFileExtensions: array of string): TStringList;
var
  SearchRec: TSearchRec;
  Files: TStringList;
  SubdirFiles: TStringList;
  Dir: string;
begin
  Files := TStringList.Create;
  Dir := ExcludeTrailingPathDelimiter(ADirectory);

  if FindFirst(Dir + PathDelim + '*', faAnyFile, SearchRec) = 0 then
  begin
    repeat
      if (SearchRec.Attr and faDirectory) = faDirectory then
      begin
        if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
        begin
          SubdirFiles := FindAllFiles(Dir + PathDelim + SearchRec.Name, AFileExtensions);
          try
            Files.AddStrings(SubdirFiles);
          finally
            SubdirFiles.Free;
          end;
        end;
      end;

      if MatchesExtension(SearchRec.Name, AFileExtensions) then
        Files.Add(Dir + PathDelim + SearchRec.Name);
    until FindNext(SearchRec) <> 0;
  end;
  FindClose(SearchRec);
  Files.Sort;
  Result := Files;
end;

function FindAllFiles(const ADirectory: string; const AFileExtension: string): TStringList;
var
  // A named array rather than the bracket-constructor argument:
  // context-typed constructor arguments refuse OVERLOAD resolution
  // under Lakon (its documented minimal-overload boundary), and the
  // explicit form is identical native code.
  Extensions: array[0..0] of string;
begin
  Extensions[0] := AFileExtension;
  Result := FindAllFiles(ADirectory, Extensions);
end;

{$IFDEF LAKON}

// The Lakon/WASI file lane ignores share flags on its single-process lane.

function ReadFileBytes(const APath: string): TBytes;
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(APath, fmOpenRead);
  try
    SetLength(Result, Stream.Size);
    if Length(Result) > 0 then
      Stream.ReadBuffer(Result[0], Length(Result));
  finally
    Stream.Free;
  end;
end;

function ReadUTF8FileText(const APath: string): string;
var
  Bytes: TBytes;
  ErrorOffset: Integer;
begin
  Bytes := ReadFileBytes(APath);
  if not TryDecodeUTF8(Bytes, Result, ErrorOffset) then
    raise EConvertError.CreateFmt('Invalid UTF-8 at byte %d in file "%s"',
      [ErrorOffset, APath]);
end;

{$ELSE}

function ReadUTF8FileText(const APath: string): string;
var
  Bytes: TBytes;
  ErrorOffset: Integer;
begin
  Bytes := ReadFileBytes(APath);
  if not TryDecodeUTF8(Bytes, Result, ErrorOffset) then
    raise EConvertError.CreateFmt('Invalid UTF-8 at byte %d in file "%s"',
      [ErrorOffset, APath]);
end;

function ReadFileBytes(const APath: string): TBytes;
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(APath, fmOpenRead or fmShareDenyWrite);
  try
    SetLength(Result, Stream.Size);
    if Length(Result) > 0 then
      Stream.ReadBuffer(Result[0], Length(Result));
  finally
    Stream.Free;
  end;
end;

{$ENDIF}

procedure WriteUTF8FileText(const APath, AText: string);
var
  Bytes: TBytes;
  ErrorOffset: Integer;
  Stream: TFileStream;
begin
  if not TryEncodeUTF8(AText, Bytes, ErrorOffset) then
    raise EConvertError.CreateFmt(
      'Cannot encode lone UTF-16 surrogate at code-unit %d in file "%s"',
      [ErrorOffset, APath]);
  Stream := TFileStream.Create(APath, fmCreate);
  try
    if Length(Bytes) > 0 then
      Stream.WriteBuffer(Bytes[0], Length(Bytes));
  finally
    Stream.Free;
  end;
end;

end.
