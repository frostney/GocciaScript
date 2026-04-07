unit Goccia.TextFiles;

{$I Goccia.inc}

interface

uses
  Classes;

function RetagUTF8Text(const ABytes: RawByteString): string;
function CreateUTF8StringList(const AText: string): TStringList;
function CreateUTF8FileTextLines(const AText: UTF8String): TStringList;
function NormalizeNewlinesToLF(const AText: string): string;
function NormalizeUTF8NewlinesToLF(const AText: UTF8String): UTF8String;
function ReadUTF8FileText(const APath: string): UTF8String;
function ReadUTF8FileLines(const APath: string): TStringList;
function StringListToLFText(const ALines: TStrings): string;

implementation

uses
  SysUtils,

  StringBuffer;

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

function CreateUTF8FileTextLines(const AText: UTF8String): TStringList;
var
  LineText: UTF8String;
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
      LineText := Copy(AText, LineStart, TextIndex - LineStart);
      Result.Add(string(LineText));
      if (TextIndex < Length(AText)) and (AText[TextIndex + 1] = #10) then
        Inc(TextIndex);
      LineStart := TextIndex + 1;
    end
    else if AText[TextIndex] = #10 then
    begin
      LineText := Copy(AText, LineStart, TextIndex - LineStart);
      Result.Add(string(LineText));
      LineStart := TextIndex + 1;
    end;
    Inc(TextIndex);
  end;

  if LineStart <= Length(AText) then
  begin
    LineText := Copy(AText, LineStart, Length(AText) - LineStart + 1);
    Result.Add(string(LineText));
  end
  else if (Length(AText) > 0) and ((AText[Length(AText)] = #10) or
    (AText[Length(AText)] = #13)) then
    Result.Add('');
end;

function NormalizeNewlinesToLF(const AText: string): string;
begin
  Result := StringReplace(AText, #13#10, #10, [rfReplaceAll]);
  Result := StringReplace(Result, #13, #10, [rfReplaceAll]);
end;

function NormalizeRawNewlinesToLF(const AText: RawByteString): RawByteString;
begin
  Result := StringReplace(AText, #13#10, #10, [rfReplaceAll]);
  Result := StringReplace(Result, #13, #10, [rfReplaceAll]);
end;

function NormalizeUTF8NewlinesToLF(const AText: UTF8String): UTF8String;
var
  NormalizedBytes: RawByteString;
begin
  NormalizedBytes := NormalizeRawNewlinesToLF(RawByteString(AText));
  SetCodePage(NormalizedBytes, CP_UTF8, False);
  Result := UTF8String(NormalizedBytes);
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

function ReadUTF8FileLines(const APath: string): TStringList;
begin
  Result := CreateUTF8FileTextLines(ReadUTF8FileText(APath));
end;

function StringListToLFText(const ALines: TStrings): string;
var
  Buffer: TStringBuffer;
  I: Integer;
begin
  if not Assigned(ALines) then
    Exit('');

  Buffer := TStringBuffer.Create;
  for I := 0 to ALines.Count - 1 do
  begin
    if I > 0 then
      Buffer.AppendChar(#10);
    Buffer.Append(ALines[I]);
  end;

  Result := Buffer.ToString;
end;

initialization
  {$IFDEF MSWINDOWS}
  DefaultSystemCodePage := CP_UTF8;
  {$ENDIF}

end.
