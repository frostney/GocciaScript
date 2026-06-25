unit Goccia.TextFiles;

{$I Goccia.inc}

interface

uses
  SysUtils;

function ReadUTF8FileText(const APath: string): UTF8String;
function ReadFileBytes(const APath: string): TBytes;

implementation

uses
  FileUtils;

function ReadUTF8FileText(const APath: string): UTF8String;
begin
  Result := FileUtils.ReadUTF8FileText(APath);
end;

function ReadFileBytes(const APath: string): TBytes;
begin
  Result := FileUtils.ReadFileBytes(APath);
end;

end.
