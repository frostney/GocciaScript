unit Goccia.TextFiles;

{$I Goccia.inc}

interface

function ReadUTF8FileText(const APath: string): UTF8String;

implementation

uses
  FileUtils;

function ReadUTF8FileText(const APath: string): UTF8String;
begin
  Result := FileUtils.ReadUTF8FileText(APath);
end;

end.
