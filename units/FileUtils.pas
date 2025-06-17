unit FileUtils;

{$I Goccia.inc}

interface

uses
  Classes, SysUtils;

function FindAllFiles(const Directory: string; const FileExtension: string): TStringList;

implementation

function FindAllFiles(const Directory: string; const FileExtension: string): TStringList;
var
  SearchRec: TSearchRec;
  Files: TStringList;
  SubdirFiles: TStringList;
begin
  Files := TStringList.Create;

  if FindFirst(Directory + '/*', faAnyFile, SearchRec) = 0 then
  begin
    // If the file is a directory, add all the files in the directory
    repeat
      if (SearchRec.Attr and faDirectory) = faDirectory then
      begin
        if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
        begin
          SubdirFiles := FindAllFiles(Directory + '/' + SearchRec.Name, FileExtension);
          try
            Files.AddStrings(SubdirFiles);
          finally
            SubdirFiles.Free; // Free the recursive result to prevent memory leak
          end;
        end;
      end;

      // If the file is a .js file, add it to the list
      if ExtractFileExt(SearchRec.Name) = FileExtension then
      begin
        Files.Add(Directory + '/' + SearchRec.Name);
      end;
    until FindNext(SearchRec) <> 0;
  end;
  FindClose(SearchRec);
  Result := Files;
end;

end.
