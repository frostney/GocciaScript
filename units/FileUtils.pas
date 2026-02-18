unit FileUtils;

{$I Goccia.inc}

interface

uses
  Classes,
  SysUtils;

function FindAllFiles(const ADirectory: string; const AFileExtension: string): TStringList;

implementation

function FindAllFiles(const ADirectory: string; const AFileExtension: string): TStringList;
var
  SearchRec: TSearchRec;
  Files: TStringList;
  SubdirFiles: TStringList;
begin
  Files := TStringList.Create;

  if FindFirst(ADirectory + '/*', faAnyFile, SearchRec) = 0 then
  begin
    // If the file is a directory, add all the files in the directory
    repeat
      if (SearchRec.Attr and faDirectory) = faDirectory then
      begin
        if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
        begin
          SubdirFiles := FindAllFiles(ADirectory + '/' + SearchRec.Name, AFileExtension);
          try
            Files.AddStrings(SubdirFiles);
          finally
            SubdirFiles.Free; // Free the recursive result to prevent memory leak
          end;
        end;
      end;

      // If the file is a .js file, add it to the list
      if ExtractFileExt(SearchRec.Name) = AFileExtension then
      begin
        Files.Add(ADirectory + '/' + SearchRec.Name);
      end;
    until FindNext(SearchRec) <> 0;
  end;
  FindClose(SearchRec);
  Files.Sort;
  Result := Files;
end;

end.
