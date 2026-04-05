unit Goccia.TextFiles;

{$I Goccia.inc}

interface

uses
  Classes;

function RetagUTF8Text(const ABytes: RawByteString): string;
function CreateUTF8StringList(const AText: string): TStringList;
function ReadUTF8FileText(const APath: string): UTF8String;

implementation

uses
  SysUtils;

function RetagUTF8Text(const ABytes: RawByteString): string;
var
  Bytes: RawByteString;
begin
  Bytes := ABytes;
  SetCodePage(Bytes, CP_UTF8, False);
  Result := string(Bytes);
end;

function CreateUTF8StringList(const AText: string): TStringList;
var
  LineStart: Integer;
  TextIndex: Integer;
begin
  Result := TStringList.Create;
  LineStart := 1;
  TextIndex := 1;

  while TextIndex <= Length(AText) do
  begin
    if AText[TextIndex] = #13 then
    begin
      Result.Add(Copy(AText, LineStart, TextIndex - LineStart));
      if (TextIndex < Length(AText)) and (AText[TextIndex + 1] = #10) then
        Inc(TextIndex);
      LineStart := TextIndex + 1;
    end
    else if AText[TextIndex] = #10 then
    begin
      Result.Add(Copy(AText, LineStart, TextIndex - LineStart));
      LineStart := TextIndex + 1;
    end;
    Inc(TextIndex);
  end;

  if LineStart <= Length(AText) then
    Result.Add(Copy(AText, LineStart, Length(AText) - LineStart + 1))
  else if (Length(AText) > 0) and ((AText[Length(AText)] = #10) or
    (AText[Length(AText)] = #13)) then
    Result.Add('');
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

  Result := UTF8String(RetagUTF8Text(SourceText));
end;

initialization
  {$IFDEF MSWINDOWS}
  DefaultSystemCodePage := CP_UTF8;
  {$ENDIF}

end.
