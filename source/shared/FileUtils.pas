unit FileUtils;

{$I Shared.inc}

interface

uses
  Classes,
  SysUtils;

function FindAllFiles(const ADirectory: string; const AFileExtension: string): TStringList; overload;
function FindAllFiles(const ADirectory: string; const AFileExtensions: array of string): TStringList; overload;
function ExpandUTF8FileName(const APath: string): string;
function UTF8DirectoryExists(const APath: string): Boolean;
function UTF8FileExists(const APath: string): Boolean;

{ Read an entire file as raw bytes and tag the result as UTF-8.
  No BOM stripping or newline normalization is performed. }
function ReadUTF8FileText(const APath: string): UTF8String;

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
begin
  Result := FindAllFiles(ADirectory, [AFileExtension]);
end;

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

end.
