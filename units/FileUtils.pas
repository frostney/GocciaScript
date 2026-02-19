unit FileUtils;

{$I Goccia.inc}

interface

uses
  Classes,
  SysUtils;

const
  DefaultScriptExtensions: array[0..3] of string = ('.js', '.jsx', '.ts', '.tsx');

function FindAllFiles(const ADirectory: string): TStringList; overload;
function FindAllFiles(const ADirectory: string; const AFileExtension: string): TStringList; overload;
function FindAllFiles(const ADirectory: string; const AFileExtensions: array of string): TStringList; overload;

implementation

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
begin
  Files := TStringList.Create;

  if FindFirst(ADirectory + '/*', faAnyFile, SearchRec) = 0 then
  begin
    repeat
      if (SearchRec.Attr and faDirectory) = faDirectory then
      begin
        if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
        begin
          SubdirFiles := FindAllFiles(ADirectory + '/' + SearchRec.Name, AFileExtensions);
          try
            Files.AddStrings(SubdirFiles);
          finally
            SubdirFiles.Free;
          end;
        end;
      end;

      if MatchesExtension(SearchRec.Name, AFileExtensions) then
        Files.Add(ADirectory + '/' + SearchRec.Name);
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

function FindAllFiles(const ADirectory: string): TStringList;
begin
  Result := FindAllFiles(ADirectory, DefaultScriptExtensions);
end;

end.
