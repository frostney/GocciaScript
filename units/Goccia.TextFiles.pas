unit Goccia.TextFiles;

{$I Goccia.inc}

interface

function ReadUTF8FileText(const APath: string): string;

implementation

uses
  Classes,
  SysUtils;

function ReadUTF8FileText(const APath: string): string;
var
  SourceText: UTF8String;
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

  Result := SourceText;
end;

end.
