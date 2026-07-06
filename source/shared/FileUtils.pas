unit FileUtils;

{$I Shared.inc}

interface

uses
  {$IFDEF UNIX}BaseUnix,{$ENDIF}
  Classes,
  SysUtils;

function FindAllFiles(const ADirectory: string; const AFileExtension: string): TStringList; overload;
function FindAllFiles(const ADirectory: string; const AFileExtensions: array of string): TStringList; overload;
function ExpandUTF8FileName(const APath: string): string;
function UTF8DirectoryExists(const APath: string): Boolean;
function UTF8FileExists(const APath: string): Boolean;

{ True when APath itself is a symbolic link (UNIX) or a reparse
  point / junction (Windows). Does not follow the link. }
function HostPathIsSymlink(const APath: string): Boolean;

{ Read an entire file as raw bytes and tag the result as UTF-8.
  No BOM stripping or newline normalization is performed. }
function ReadUTF8FileText(const APath: string): UTF8String;

{ Read an entire file as raw bytes, preserving every byte exactly
  (NUL bytes, non-UTF-8 sequences, and original newlines). }
function ReadFileBytes(const APath: string): TBytes;

implementation

function UTF8PathToUnicodeString(const APath: string): UnicodeString;
var
  Bytes: RawByteString;
begin
  Bytes := RawByteString(APath);
  SetCodePage(Bytes, CP_UTF8, False);
  Result := UTF8Decode(UTF8String(Bytes));
end;

function UnicodeStringToUTF8Path(const APath: UnicodeString): string;
var
  Bytes: RawByteString;
begin
  Bytes := RawByteString(UTF8Encode(APath));
  SetCodePage(Bytes, CP_UTF8, False);
  Result := string(Bytes);
end;

function ExpandUTF8FileName(const APath: string): string;
begin
  Result := UnicodeStringToUTF8Path(ExpandFileName(
    UTF8PathToUnicodeString(APath)));
end;

function UTF8DirectoryExists(const APath: string): Boolean;
begin
  Result := DirectoryExists(UTF8PathToUnicodeString(APath));
end;

function UTF8FileExists(const APath: string): Boolean;
begin
  Result := FileExists(UTF8PathToUnicodeString(APath));
end;

function HostPathIsSymlink(const APath: string): Boolean;
{$IFDEF UNIX}
var
  Info: Stat;
begin
  Result := (fpLStat(APath, Info) = 0) and fpS_ISLNK(Info.st_mode);
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

// The Lakon/WASI file lane is real since rung 5 (read-only) and
// rung 6 (the write lane): the readers mirror the native shapes
// over a TBytes buffer. Lakon's strings ride bytes one per code
// unit, so the byte-to-text conversion is a plain widening copy
// (UTF8String aliases string there); share flags stay ignored on
// the single-process lane.

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

function ReadUTF8FileText(const APath: string): UTF8String;
var
  Bytes: TBytes;
  Text: string;
  Index: Integer;
begin
  Bytes := ReadFileBytes(APath);
  SetLength(Text, Length(Bytes));
  for Index := 1 to Length(Bytes) do
    Text[Index] := Chr(Bytes[Index - 1]);
  Result := Text;
end;

{$ELSE}

function ReadUTF8FileText(const APath: string): UTF8String;
var
  SourceText: RawByteString;
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(APath, fmOpenRead or fmShareDenyWrite);
  try
    SetLength(SourceText, Stream.Size);
    if Length(SourceText) > 0 then
      Stream.ReadBuffer(Pointer(SourceText)^, Length(SourceText));
  finally
    Stream.Free;
  end;

  SetCodePage(SourceText, CP_UTF8, False);
  Result := UTF8String(SourceText);
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

end.
